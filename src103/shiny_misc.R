completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# Fn to calculate child share of parent for the target measure and remove redundant parent (middle child)
calculate_child_portion <- function(df, 
                                    target_measure = "Discounted $ Share Total DYA",
                                    child_share_threshold = .95,
                                    remove = TRUE,
                                    make_owner_id = FALSE) {
  
  # Flag grouping (level) variables
  attr(df, "grouping_variables") <- grouping_vars(df)
  
  # Keep a copy of the rest of the matrix to merge back with the calculated columns
  tbl_df <- df[!names(df) %in% attr(df, "grouping_variables")] %>% select(-`Embedding Level`, -is_primary, -owner_ids)
  
  # Create Grouping and Primary ID if not created outside
  ### Comment out in actual server.R
  if (make_owner_id == TRUE) {
    df <- mark_owner_ids2(df, 1)
    
    tbl_tmp <- df %>% group_by(owner_ids) %>%
      mutate_(is_primary = lazyeval::interp(~ifelse(var1, rank(desc(abs(as.numeric(var1))), ties.method = "first") == 1, F),
                                            var1 = as.name(target_measure)))
    df$is_primary <- tbl_tmp$is_primary
  }
  ### Comment out end
  
  # Create Concatenate Level
  df <- unite_(df, "level", attr(df, "grouping_variables"), sep = ",", remove = F) %>%
    mutate(level = str_replace_all(level, ",NA",""))
  
  # Flag grouping (level) variables
  # TODO: Doesn't always pick up the grouping variables
  attr(df, "grouping_variables") <- grouping_vars(df)
  gdf <- df[attr(df, "grouping_variables")]
  
  df <- df %>% select_("level", "is_primary", "owner_ids", 
                       lazyeval::interp(~var1, var1 = as.name("Embedding Level")), 
                       lazyeval::interp(~var2, var2 = as.name(target_measure))) %>%
    mutate(row_id = 1: nrow(.))
  
  df <- inner_join(gdf, df, by = "level")
  
  # Convert all the / separator to ,
  df$level <- str_replace_all(df$level, " / ", ",")
  
  # Remove the current level (level minus 1)
  df <- separate(df, level, "level_minus_1", sep=",\\s*(?=[^,]+$)", remove = FALSE)
  
  # Create a vector to store the parent_id
  parent_id_list <- 0
  # Find matching parent id by matching the current level minus 1
  for (i in 1: nrow(df)) {
    parent_id <- max(which(str_detect(df$level_minus_1[i], df$level)), na.rm = T)
    if (is.infinite(parent_id)) { parent_id <- i } # If is infinite, then it is the parent
    parent_id_list[i] <- parent_id
  }
  df$parent_id <- parent_id_list
  
  # Find matching parent values on the target measure
  sdf <- select_(df, "row_id", lazyeval::interp(~var1, var1 = as.name("Embedding Level")), 
                 lazyeval::interp(~var2, var2 = as.name(target_measure))) %>% 
    rename_("parent_value" =  lazyeval::interp(~var1, var1 = as.name(target_measure)),
            "parent_level" = lazyeval::interp(~var2, var2 = as.name("Embedding Level")))
  
  df <- inner_join(df, sdf, by = c("parent_id" = "row_id")) %>%
    rename_("current_value" =  lazyeval::interp(~var1, var1 = as.name(target_measure)))
  
  # Calculate child portion for those with immediate parent only
  df <- mutate(df,
               parent_value = ifelse(parent_id == row_id, NA, parent_value),
               child_portion = current_value / parent_value,
               immediate_parent = ifelse(`Embedding Level` - parent_level == 1, "Yes", "No"))
  
  # Flag parents that are not the top level or is the winner path for removal
  df <- mutate(df, 
               immediate_child = lead(immediate_parent),
               child_portion_value = ifelse(immediate_child == "Yes", lead(child_portion), NA),
               remove_parent = ifelse(child_portion_value >= child_share_threshold & is_primary == FALSE, "Yes", "No"),
               remove_parent = ifelse(is.na(remove_parent), "No", remove_parent),
               remove_parent = ifelse(`Embedding Level` == 0, "No", remove_parent))
  
  # Append back with the other metrics
  df <- bind_cols(df, tbl_df)
  
  # Remove redundant row
  # Remove level and level_minus_1 variable used to identify the parent and child rows
  if (remove == TRUE) {
    df <- filter(df, remove_parent == "No") %>%
      select(-level, -level_minus_1, -current_value, -row_id, -parent_id, -parent_level, -parent_value, -child_portion, 
             -immediate_parent, -immediate_child, -remove_parent, -child_portion_value)
  }
  
  # Label the grouping variables again for next steps
  attr(df, "grouping_variables") <- grouping_vars(df)
  df
}
#rounding off dataframes
round_df <- function(df, digits = 0, min_threshold = 10) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  for (i in 1:length(nums)){
    if (length(grep("IYA",names(nums)[i],ignore.case = TRUE))>0 & nums[i]==TRUE){
      df[,names(nums)[i]] <- round(df[,names(nums)[i]], digits = 0)
    }else if (length(grep("Velocity",names(nums)[i],ignore.case = TRUE))>0 & nums[i]==TRUE){
      df[,names(nums)[i]] <- round(df[,names(nums)[i]], digits = 1)
    }else if (length(grep("Shr",names(nums)[i],ignore.case = TRUE))>0 & nums[i]==TRUE){
      df[,names(nums)[i]] <- round(df[,names(nums)[i]], digits = 1)
    }else if (length(grep("Share",names(nums)[i],ignore.case = TRUE))>0 & nums[i]==TRUE){
      df[,names(nums)[i]] <- round(df[,names(nums)[i]], digits = 1)
    }else if(nums[i]==TRUE){
      # Check min of column to determine the number of digits to show
      # If it is more than the min_threshod, suppress decimal place
      min <- min(abs(df[,names(nums)[i]]), na.rm = T)
      if (min < min_threshold) {
        df[,names(nums)[i]] <- round(df[,names(nums)[i]], digits = 0)
      } else {
        df[,names(nums)[i]] <- round(df[,names(nums)[i]], digits = digits)
      }
    }
  }
  (df)
}
round_df_drv <- function(df, digits = 2) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  for (i in 1:length(nums)){
    if(nums[i]==TRUE){
      df[,names(nums)[i]] <- round(df[,names(nums)[i]], digits = digits)
    }
  }
  (df)
}

#get grouping or product/market level values from frame
grouping_vars<-function(df){
  cols<-colnames(df)
  for (i in 1:length(cols)){
    if (is.numeric(df[,cols[i]]))
    {
      break
    }
  }
  return (cols[1:(i-1)])
}

#for datatable callback
fnDrawCallback = JS("function (oSettings, json) {
                    $('.spark:not(:has(canvas))').sparkline('html', {
                    type: 'bar',
                    highlightColor: 'orange'
                    });
                    }")

#to clear zeros in trend lines as present
clearbegzero<-function(df,sparvar, roundto = 2){
  for (i in 1:nrow(df)){
    str<-df[,sparvar][i]
    if (!is.null(str) & !is.na(str)){
      str<-strsplit(str,",")[[1]]
      str <- as.numeric(str) # Convert to numeric
      str <- round(str, roundto) # Round to specific digits
      for (j in 1:length(str)){
        if (str[j]!=0){
          break
        }
      }
      df[,sparvar][i]<-paste(str[(j+1):length(str)],collapse=",")
    }
    
  }
  return(df)
}

# Main Market/Product Table
dataastable <- function(market,input,list_markets,hoverover_df,values,market_level_total,total_market,headers_mapping,whichtab){
  req(input$sharebase)
  req(input$prod)
  req(input$Martyp)
  markind = 0
  nsep=0
  if (!is.null(values$tottabtype)){
    if (length(grep("TOP123",market))>0){
      if (values$tottabtype=="tab1"){
        table_1<-list_markets[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[values$mtp]][[input$currentperiod]][[input$refperiod]]
      }else{
        table_1<-list_markets[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[market_level_total]][[total_market]][[input$currentperiod]][[input$refperiod]]
      }
      
    }else{
      idx<-which(market==names(list_markets[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]]))
      table_1 <- list_markets[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[idx]][[input$currentperiod]][[input$refperiod]]
    }
  }else{
    if (length(grep("TOP123",market))>0){
      table_1<-list_markets[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[market_level_total]][[total_market]][[input$currentperiod]][[input$refperiod]]
    }else{
      # browser()
      idx<-which(market==names(list_markets[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]]))
      table_1 <- list_markets[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[idx]][[input$currentperiod]][[input$refperiod]]
    }
  }
  aaa <<- table_1
  
  idxkey=grep("key market",colnames(table_1),ignore.case = T)
  if (length(idxkey)>0){
    table_1<-table_1[,c(1:3,idxkey,setdiff(4:ncol(table_1),idxkey))]
  }
  
  if (length(grep("Value",colnames(table_1)))>0){
    colidx=grep("Value",colnames(table_1))
    colnames(table_1)[colidx]<-gsub("Value","value",colnames(table_1)[colidx])
  }
  
  
  if ("table_id" %in% colnames(table_1)){
    if (whichtab>1){
      nsep=nrow(table_1[table_1$table_id==1,])
    }
    table_1<-table_1[table_1$table_id==whichtab,]
    table_1$table_id<-NULL
  }
  if (length(grep('distance', colnames(table_1))) > 0) {
    markind=1 
  }
  if ("Total No ratings" %in% colnames(table_1)){
    colnames(table_1)[grep("Total No ratings",colnames(table_1),ignore.case=T)]="Total # Reviews"
  }
  a = colnames(table_1)
  table_1[,1]<-gsub("\\\\","",table_1[,1])
  # Which columns are we filtering on? - the ones with duplicate colnames
  # Which columns do we keep? - the ones that have an important value
  discard = c()
  columns <-  grep(" I", colnames(table_1), ignore.case = TRUE)-1
  # Which columns do we keep? - the ones that have an important value
  for(i in 1:length(columns)){
    mx = max(table_1[,columns[i]+1][is.finite(table_1[,columns[i]+1])], na.rm = TRUE)
    mn = min(table_1[,columns[i]+1][is.finite(table_1[,columns[i]+1])], na.rm = TRUE)
    if((mx < 102 &  mn > 98) | (mx==0 & mn==0) | (is.infinite(mx) & is.infinite(mn))){
      discard = c(discard,colnames(table_1)[columns[i]], colnames(table_1)[columns[i]+1])}
  }
  orig<-table_1
  if (length(columns)>0 & nrow(table_1)>0 & !is.null(input$ic)){
    if (input$ic==TRUE){
      for(i in 1:length(columns)){
        for (j in 1:nrow(table_1)){
          table_1[j,columns[i]+1]<-(-1)*((table_1[j,columns[i]]/table_1[j,columns[i]+1])*100-table_1[j,columns[i]])
        }
      }
      colnames(table_1)[columns+1]<-gsub(" I"," C",colnames(table_1)[columns+1])
      discard<-gsub(" I"," C",discard)
    }
    
  }
  if ("Average rating" %in% discard){
    discard<-c(discard,"Total # Reviews","rnr_distance","Reviews")
  }
  # Create Tree Trigger column  
  
  table_1$tree <- ifelse(table_1$is_root, paste0("<img src = 'tree_icon.png', class = 'icon-popup'>",
                                                 "<span title = '", table_1$`Market/Product Attributes`, "'></span>"), "")
  table_1$Trend <- paste0("<i class='fa fa-line-chart' style = 'color: green;'></i>","<span title = '", paste0(nsep+(1:nrow(table_1))), "'></span>")
  table_1$original_order<-NULL
  table_1_pre <<- table_1
  if (markind == 1) {
    distidx <- grep('distance', colnames(table_1))
    matchidx <- grep('match', colnames(table_1))
    for (i in 1:length(distidx)){
      dist_vals <- table_1[, distidx[i]]
      match_vals <- table_1[, matchidx[i]]
      #dct_match <- which(is.nan(dist_vals))
      aeq_match <- which(dist_vals > 0)
      eq_match <- which(dist_vals == 0 | is.nan(dist_vals))
      
      #dist_vals[dct_match] <- paste0("<img src = 'dict.png', class = 'icon-rnr', title = '", match_vals[dct_match], "'>")
      dist_vals[aeq_match] <- paste0("<img src = 'approx_equal.png', class = 'icon-rnr', height='24', width='36', title = '", match_vals[aeq_match], "'>")
      dist_vals[eq_match] <- paste0("<img src = 'equal.png', class = 'icon-rnr', height='24', width='36', title = '", match_vals[eq_match], "'>")
      table_1[,distidx[i]] <- dist_vals 
      # table_1[,distidx[i]] <- ifelse(table_1[,distidx[i]] > 0, paste0("<i class='fa fa-lock' style = 'color: green;'></i>","<span title = '", "A", "'></span>"),
      #                                ifelse(table_1[,distidx[i]] < 0,paste0("<i class='fa fa-lock' style = 'color: red;'></i>","<span title = '", "B", "'></span>"),
      #                                       ifelse(table_1[,distidx[i]] == 0,paste0("<i class='fa fa-lock' style = 'color: yellow;'></i>","<span title = '", "C", "'></span>"),"")))
    }
    table_1$Reviews <- ifelse(!is.na(table_1[, matchidx[1]]), paste0("<i class='fa fa-eye' style = 'color: green;'></i>","<span title = '", paste0(nsep+1:nrow(table_1)), "'></span>"),"")
  }
  breakatchar<-60
  for (rowcount in 1:nrow(table_1)){
    nstr<-nchar(table_1$`Market/Product Attributes`[rowcount])
    if (nstr>breakatchar){
      mstr<-table_1$`Market/Product Attributes`[rowcount]
      table_1$`Market/Product Attributes`[rowcount]<-paste0(substr(mstr,1,breakatchar),"<br>",substr(mstr,breakatchar+1,nstr))
    }
  }
  
  indent<-NULL
  for (i in 1:nrow(table_1)){
    if (length(grep("/",table_1[i,1]))==0){
      indent<-c(indent,0)
    # }else if (length(grep(as.character(table_1[i-1,1]),as.character(table_1[i,1])))>0){
    #   indent<-c(indent,indent[length(indent)]+1)
    }else{
      indent<-c(indent,indent[length(indent)])
    }
  }
  table_1$indent<-indent*7
  # Style the Market/Product Attributes to make starting objects and winners stand out
  if ("cup_type" %in% colnames(table_1)){
    if (!("Competition" %in% colnames(table_1))){
      table_1$Competition<-""
    }
    table_1 %<>% mutate(product_org = `Market/Product Attributes`,
                        `Market/Product Attributes` =
                          ifelse(table_1$is_root, paste0("<strong><span class = 'showcomp' title = '", Competition,"'>", `Market/Product Attributes`,"</span></strong>"),
                                 ifelse(is.na(`cup_type`) & !is_root, paste0("<div style = 'padding-left:",indent,"px;'><span>", `Market/Product Attributes`,"</span></div>"),
                                       ifelse(`cup_type` == -1, paste0("<div style = 'padding-left:",indent,"px;'><img src='Image-1.PNG' style = 'width:36px;height:24px;padding-right: 10px'/><span>", `Market/Product Attributes`, 
                                                                      "</span></div>"),
                                              ifelse(`cup_type` == -2,paste0("<div style = 'padding-left:",indent,"px;'><img src='Image-2.PNG' style = 'width:36px;height:24px;padding-right: 10px'/><span><em>", `Market/Product Attributes`, 
                                                                            "</em></span></div>"),
                                                     ifelse(`cup_type` == -3,paste0("<div style = 'padding-left:",indent,"px;'><img src='Image-3.PNG' style = 'width:36px;height:24px;padding-right: 10px'/><span><em>", `Market/Product Attributes`, 
                                                                                   "</em></span></div>"),
                                                            ifelse(`cup_type` == -4,paste0("<div style = 'padding-left:",indent,"px;'><img src='Image-4.PNG' style = 'width:36px;height:24px;padding-right: 10px'/><span><em>", `Market/Product Attributes`, 
                                                                                          "</em></span></div>"),
                                                                   ifelse(`cup_type` == -5,paste0("<div style = 'padding-left:",indent,"px;'><img src='Image-5.PNG' style = 'width:36px;height:24px;padding-right: 10px'/><span><em>", `Market/Product Attributes`, 
                                                                                                 "</em></span></div>"),
                                                                          ifelse(`cup_type` == 1,paste0("<div style = 'padding-left:",indent,"px;'><img src='Image1.PNG' style = 'width:36px;height:24px;padding-right: 10px'/><span><em>", `Market/Product Attributes`, 
                                                                                                        "</em></span></div>"),
                                                                                 ifelse(`cup_type` == 2,paste0("<div style = 'padding-left:",indent,"px;'><img src='Image2.PNG' style = 'width:36px;height:24px;padding-right: 10px'/><span><em>", `Market/Product Attributes`, 
                                                                                                               "</em></span></div>"),
                                                                                        ifelse(`cup_type` == 3,paste0("<div style = 'padding-left:",indent,"px;'><img src='Image3.PNG' style = 'width:36px;height:24px;padding-right: 10px'/><span><em>", `Market/Product Attributes`, 
                                                                                                                      "</em></span></div>"),
                                                                                               ifelse(`cup_type` == 4,paste0("<div style = 'padding-left:",indent,"px;'><img src='Image4.PNG' style = 'width:36px;height:24px;padding-right: 10px'/><span><em>", `Market/Product Attributes`, 
                                                                                                                             "</em></span></div>"),
                                                                                                      paste0("<div style = 'padding-left:",indent,"px;'><img src='Image5.PNG' style = 'width:36px;height:24px;padding-right: 10px'/><span><em>", `Market/Product Attributes`, 
                                                                                                             "</em></span></div>")))))))))))))
    
    table_1$cup_type<-NULL
    table_1$indent<-NULL
    table_1$is_root<-NULL
    table_1$Competition<-NULL
  }else{
    table_1 %<>% mutate(product_org = `Market/Product Attributes`,
                        `Market/Product Attributes` =
                          ifelse(table_1$is_root, paste0("<strong><span class = 'showcomp' title = '", Competition,"'>", `Market/Product Attributes`, 
                                                         "</span></strong>"),
                                 ifelse(table_1$is_root, paste0("<i title = 'This is the element of the portfolio best explaining the change above. You can verify by exploring the portfolio tree' class='fa fa-trophy' style = 'color: 	#FFD700; padding-right: 10px'></i><span title = '",paste("this is Winning Subpath",table_1[,3]),"'>", `Market/Product Attributes`, 
                                                                "</span></div>"),
                                        paste0("<div style = 'padding-left:25px;'><i class='fa fa-trophy' style = 'color: #C0C0C0; padding-right: 10px'></i><span title = '",paste("this is child item",table_1[,3]),"'><em>", `Market/Product Attributes`, 
                                               "</em></span></div>"))))
    table_1$indent<-NULL
    table_1$is_root<-NULL
    table_1$Competition<-NULL
  }
  
  quote_names <- function(vec) {
    new_names <- paste("`", vec, "`", sep = "")
    return(new_names)
  }
  val_share_name <- colnames(table_1)[2]
  val_share_diff_name <- colnames(table_1)[3]
  
  top_vars <- c("tree", "Market/Product Attributes","Trend", val_share_name, val_share_diff_name)
  
  
  cols<-intersect(as.character(hoverover_df$driver_short_names),colnames(table_1))
  colsd <-match(cols,colnames(table_1))
  colsi <-colsd+1
  metric<-trimws(gsub(colnames(table_1)[which(colnames(table_1) %in% as.character(hoverover_df$driver_short_names),arr.ind = T)][1],"",
                      colnames(table_1)[which(colnames(table_1) %in% as.character(hoverover_df$driver_short_names),arr.ind = T)+1][1]))
  
  values$drvs<-cols
  drvs<-colnames(table_1)[c(rbind(colsd,colsi))]
  if (markind==1){
    distidx<-grep('distance',colnames(table_1))
    rcols<-c(colnames(table_1)[distidx],"Total # Reviews","Reviews")
    drvs1<-drvs[!grepl("review|rating",drvs,ignore.case = TRUE)]
    drvs2<-setdiff(drvs,drvs1)
    all_others <- setdiff(colnames(table_1), c(top_vars,rcols,drvs))
    all_vars <- c(top_vars,drvs1,rcols,drvs2,all_others)
  }else{
    all_others <- setdiff(colnames(table_1), top_vars)
    all_vars<- c(top_vars,all_others)
  }
  table_1 <- table_1[,all_vars]
  if (values$mtpf==T){
    if ("mtypec" %in% names(values)){
      if (values$mtypec==TRUE){
        newname=paste0(input$Martyp,"/Products")
        eval(parse(text=paste0("table_1 <- rename(table_1, '",newname,"' = `Market/Product Attributes`)")))
      }else{
        table_1 <- rename(table_1, 'Product' = `Market/Product Attributes`)
      }
    }else{
      table_1 <- rename(table_1, 'Product' = `Market/Product Attributes`)
    }
    
  }else{
    table_1 <- rename(table_1, 'Product' = `Market/Product Attributes`)
  }
  
  
  cols<-which(colnames(table_1) %in% as.character(hoverover_df$driver_short_names),arr.ind = T)
  # Create span tooltips
  # Check which drivers is present
  drvs_ls <- colnames(table_1)[cols]
  
  # Create lookup dataframe for tooltips
  # Update as needed
  drvs_lkup <- hoverover_df
  
  # Filter lookup table to drivers that are present
  drvs_lkup <- filter(drvs_lkup, driver_short_names %in% drvs_ls)
  cols<-intersect(as.character(hoverover_df$driver_short_names),colnames(table_1))
  colsd <-match(cols,colnames(table_1))
  colsi <-colsd+1

  drvnames<-NULL
  for (i in 1:length(colsd)){
    drvnames<-c(drvnames,as.character(drvs_lkup$driver_short_names[i]),paste0(as.character(drvs_lkup$driver_short_names[i])," ",metric))
  }
  if (markind==1){
    distidx<-grep('distance',colnames(table_1))
    all_others <- c(drvs1,colnames(table_1)[distidx],drvs2,"Total # Reviews","Reviews","product_org")
    table_1<-table_1[,c(colnames(table_1)[1:min(colsd)-1],all_others)]
    if (any(str_detect(names(table_1), "DPP|DP12W|DP3W"))) {
      value_s_diff <- paste0("Value Share Difference Previous Period = Value Share Current Period – Value Share Previous Period")
    } else if (any(str_detect(names(table_1), "DYA"))) {
      value_s_diff <- paste0("Value Share Difference Year Ago = Value Share Current Period – Value Share Same Period Year Ago")
    }
    colndrvs_title <- c("", 
                        " Market/Product Attributes", 
                        " Potential Competitors to P&G",
                        "",
                        value_s_diff,
                        "Value Share = Product Sales / Total Share Base Sales")
    no_title <- c("tree", "Trend")
    cols<-which(colnames(table_1) %in% as.character(hoverover_df$driver_short_names) ,arr.ind = T)
    for (i in 1:nrow(drvs_lkup)){
      if (i == 1 & drvs_lkup$driver_type[i]!="MEDIA"){
        colspn1 <- paste0("th(colspan = 2, '", drvs_lkup[i, 1],"', span('", drvs_lkup[i, 2], "'))")
      }else if (i>1 & length(grep("rating",drvs_lkup$driver_short_names[i],ignore.case = T))==0 & drvs_lkup$driver_type[i]!="MEDIA"){
        colspn1 <- c(colspn1, paste0("th(colspan = 2, '", drvs_lkup[i, 1], "', span('", drvs_lkup[i, 2], "'))"))
      }else if (drvs_lkup$driver_type[i]!="MEDIA"){
        colspn2 <- paste0("th(colspan = 2, '", drvs_lkup[i, 1],"', span('", drvs_lkup[i, 2], "'))")
      }
      # else if (drvs_lkup$driver_type[i]=="MEDIA"){
      #   if (!exists("colspn3")){
      #     colspn3<-""
      #   }
      #   colspn3 <- c(colspn3,paste0("th(colspan = 2, '", drvs_lkup[i, 1],"', span('", drvs_lkup[i, 2], "'))"))
      # }
    }
    colspn1 <- paste(colspn1, collapse = ",")
    colndrvs <- colnames(table_1)[1:(cols[1]-1)]
    # Get parameters for tooltips
    select_category <- input$sharebase
    select_market <- gsub("'","",market)
    select_refperiod <- input$refperiod
    select_currentperiod <- input$currentperiod
    select_time_num <- str_extract(select_currentperiod, "[0-9]+")
    for (i in 1:length(colndrvs)) {
      if (colndrvs[i] == "tree") {colndrvs[i] <- ""} # Blank out tree colname
      if (i == 1){
        if (colndrvs[i] %in% no_title) { # Do not add hover for popup column
          rowspn1 <- paste0("th(rowspan = 2, '",colndrvs[i],"')")
        } else {
          rowspn1 <- paste0("th(rowspan = 2, '",colndrvs[i],"', span('", colndrvs_title[i], "'))")
        }
      } else {
        if (colndrvs[i] %in% no_title) { # Do not add hover for popup column
          rowspn1 <- c(rowspn1,paste0("th(rowspan = 2, '",colndrvs[i],"')"))
        } else {
          rowspn1 <- c(rowspn1,paste0("th(rowspan = 2, '",colndrvs[i],"', span('", colndrvs_title[i], "'))"))
        }
      }
    }
    rowspnD1<-""
    rowspnD2<-""
    table_1<<-table_1
    if (length(grep('distance',colnames(table_1)))>0){
      distidx<-grep('distance',colnames(table_1))
      colndrvs_title <- colnames(table_1)[distidx]
      colndrvs<-colndrvs_title 
      no_title <- c(colnames(table_1)[distidx])
      
      for (i in 1:length(colndrvs)) {
        if (i == 1){
          if (colndrvs[i] %in% no_title) { # Do not add hover for popup column
            rowspnD1 <- paste0("th(rowspan = 2, '","","')")
          } else {
            rowspnD1 <- paste0("th(rowspan = 2, '",colndrvs[i],"', span('", colndrvs_title[i], "'))")
          }
        } else {
          if (colndrvs[i] %in% no_title) { # Do not add hover for popup column
            rowspnD1<- c(rowspnD1,paste0("th(rowspan = 2, '","","')"))
          } else {
            rowspnD1 <- c(rowspnD1,paste0("th(rowspan = 2, '",colndrvs[i],"', span('", colndrvs_title[i], "'))"))
          }
        }
      }
    }
    if (length(grep('distance',colnames(table_1)))>0){
      colndrvs_title <- c("Total # Reviews","Reviews")
      colndrvs<-colndrvs_title 
      no_title <- c("Reviews")
      
      for (i in 1:length(colndrvs)) {
        if (i == 1){
          if (colndrvs[i] %in% no_title) { # Do not add hover for popup column
            rowspnD2 <- paste0("th(rowspan = 2, '","","')")
          } else {
            rowspnD2 <- paste0("th(rowspan = 2, '",colndrvs[i],"', span('", colndrvs_title[i], "'))")
          }
        } else {
          if (colndrvs[i] %in% no_title) { # Do not add hover for popup column
            rowspnD2 <- c(rowspnD2,paste0("th(rowspan = 2, '","","')"))
          } else {
            rowspnD2 <- c(rowspnD2,paste0("th(rowspan = 2, '",colndrvs[i],"', span('", colndrvs_title[i], "'))"))
          }
        }
      }
    }
    rowspnD1<-paste(rowspnD1,collapse=",")
    rowspnD2<-paste(rowspnD2,collapse=",")
    eval(parse(text=paste0("custom_header <- htmltools::withTags(
                           table(class = 'display',thead(tr(th(rowspan = 2, ''),",
                           paste(c(rowspn1,colspn1,rowspnD1,colspn2,rowspnD2),collapse=","),"),
                           tr(lapply(rep(c('Value', paste0('",metric,"', span('",headers_mapping$hoverover[headers_mapping$diff_abreviation==gsub("I","D",metric)][1],"'))),",length(drvs_ls),"), th)))))")))
    
    
    
    
    
  }else{
    a<-c(colnames(table_1)[1:min(colsd)-1],drvnames,colnames(table_1)[(max(colsi)+1):ncol(table_1)])
    table_1<-table_1[,a]
    for (i in 1:nrow(drvs_lkup)){
      if (i == 1){
        colspn <- paste0("th(colspan = 2, '", drvs_lkup[i, 1],"', span('", drvs_lkup[i, 2], "'))")
      }else{
        colspn <- c(colspn, paste0("th(colspan = 2, '", drvs_lkup[i, 1], "', span('", drvs_lkup[i, 2], "'))"))
      }
    }
    
    colspn <- paste(colspn, collapse = ",")
    colndrvs <- setdiff(setdiff(colnames(table_1),drvnames),
                        c("is_primary", "owner_ids", "product_org"))
    # Get parameters for tooltips
    select_category <- input$sharebase
    select_market <- gsub("'","",market)
    select_refperiod <- input$refperiod
    select_currentperiod <- input$currentperiod
    select_time_num <- str_extract(select_currentperiod, "[0-9]+")
    
    # Create tooltip for first level headers
    
    # Determine if it is DMA or DYA
    if (any(str_detect(names(table_1), "DPP|DP12W|DP3W"))) {
      value_s_diff <- paste0("Value Share Difference Previous Period = Value Share Current Period – Value Share Previous Period")
    } else if (any(str_detect(names(table_1), "DYA"))) {
      value_s_diff <- paste0("Value Share Difference Year Ago = Value Share Current Period – Value Share Same Period Year Ago")
    }
    
    colndrvs_title <- c("", 
                        "Market/Product Attributes", 
                        "",
                        "Value Share = Product Sales / Total Share Base Sales",
                        value_s_diff)
    no_title <- c("tree", "Trend","")
    
    for (i in 1:length(colndrvs)) {
      if (colndrvs[i] == "tree") {colndrvs[i] <- ""} # Blank out tree colname
      if (i == 1){
        if (colndrvs[i] %in% no_title) { # Do not add hover for popup column
          rowspn <- paste0("th(rowspan = 2, '",colndrvs[i],"')")
        } else {
          rowspn <- paste0("th(rowspan = 2, '",colndrvs[i],"', span('", colndrvs_title[i], "'))")
        }
      } else {
        if (colndrvs[i] %in% no_title) { # Do not add hover for popup column
          rowspn <- c(rowspn,paste0("th(rowspan = 2, '",colndrvs[i],"')"))
        } else {
          rowspn <- c(rowspn,paste0("th(rowspan = 2, '",colndrvs[i],"', span('", colndrvs_title[i], "'))"))
        }
      }
    }
    
    rowspn<-paste(rowspn,collapse=",")
    rowspn_A <<- rowspn
    colspn_A <<- colspn
    
    eval(parse(text=paste0("custom_header <- htmltools::withTags(
                         table(class = 'display',thead(tr(th(rowspan = 2, ''),",
                           paste(c(rowspn,colspn),collapse=","),"),
                         tr(lapply(rep(c('Value', paste0('",metric,"', span('",headers_mapping$hoverover[headers_mapping$diff_abreviation==gsub("I","D",metric)][1],"'))),",length(drvs_ls),"), th)))))")))
  }
  
  
  
  
  
  # table_1$`Market/Product Attributes` <- paste0("<span title = '",paste("this is sample text",table_1[,2]),"'>", table_1$`Market/Product Attributes`," <i class='fa fa-bell' style = 'color: red;'></i></span>")
  # Replace the span substitute < and >
  custom_header <- str_replace_all(custom_header, "&lt;", "<")
  custom_header <- str_replace_all(custom_header, "&gt;", ">")
  #createreturn values
  retlist<-list()
  retlist$discard<-discard
  retlist$tab<-table_1
  retlist$chead<-custom_header
  retlist$orig<-orig
  return(retlist)
}


#get best path
getbestpath<-function(best_paths,values,market_level_total,total_market,input,headers_mapping,deftree){
  tree_name <- values$tree_name
  tree_name<-trimws(strsplit(tree_name,"/")[[1]][1])
  select_refperiod <- input$refperiod
  select_currentperiod <- input$currentperiod
  market<-gsub(paste0("/ ",input$sharebase),"",gsub("TOP123","",values$mkt),fixed=T)
  diffmet<-headers_mapping$diff_abreviation[headers_mapping$current_period==input$currentperiod & headers_mapping$reference_period==input$refperiod][1]
  tree_select_box <- selectizeInput("show_whichtree", label = NULL, choices = c("Simplified Tree", "Full Tree"), width = "120px")
  if (deftree==1){
    path_select_box <- selectizeInput("show_whichpath", label = NULL, choices = c("Most Informative Path", "Predetermind Path"), width = "120px")
  }else{
    path_select_box <- selectizeInput("show_whichpath", label = NULL, choices = c("Most Informative Path"), width = "120px")
  }
  
  select_category <- input$sharebase
  str = paste0("<h4 style = 'color: green;'>Share Tree for <span style = 'color: #3182bd;'><strong>", tree_name, 
               "</span></strong> within <span style = 'color: #3182bd;'><strong> ", select_category, 
               "</span></strong> at <span style = 'color: #3182bd;'><strong> ", market, "</strong></span>")
  show_tree = paste0("Showing Product <span style = 'color: #3182bd;'><strong>", select_currentperiod, " ", select_refperiod, "</strong></span>", tree_select_box,path_select_box, "</h4>")
  
  legendstr<-paste0("<span style = 'color: #3182bd;'>Size of the ball represents share; Number in () is share change; (+/- ",input$metrictype ,"Change",diffmet,").</span>")
  HTML(paste0(str, show_tree,legendstr, '<button type="button" class="btn btn-default round-button" 
              data-dismiss="modal" style = "float: right;">Close</button>'))
}

#win loss tables
wltable<-function(top_tables,input,type,market_level_total,total_market,headers_mapping){
  bmwltab <- tryCatch({
    if (type=="win"){
      if (input$Martyp==market_level_total){
        stop("no other market")
      }
      table_wl <- top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[input$currentperiod]][[input$refperiod]]$winners
      plstr="Gain"
      if (is.null(table_wl)){
        stop("No Winners Data")
      }else if (nrow(table_wl)>1){
        if (input$metrictype=="Value"){
          table_wl <- table_wl[order(-table_wl[,6]),]
          # table_wl <- table_wl[table_wl[,4] >= input$winthreshold,]
          # table_wl<-head(table_wl,as.numeric(input$winnum))
          val_share_diff_front_end_name<-gsub(strsplit(colnames(table_wl)[4]," ")[[1]][length(strsplit(colnames(table_wl)[4]," ")[[1]])],headers_mapping$diff_abreviation[headers_mapping$current_period==input$currentperiod & headers_mapping$reference_period==input$refperiod][1],colnames(table_wl)[4])
          colnames(table_wl)[4]<-val_share_diff_front_end_name
        }else{
          table_wl <- table_wl[order(-table_wl[,6]),]
          table_wl <- table_wl[table_wl[,3] >= input$winthreshold,]
          table_wl<-head(table_wl,as.numeric(input$winnum))
          val_share_diff_front_end_name<-gsub(strsplit(colnames(table_wl)[3]," ")[[1]][length(strsplit(colnames(table_wl)[3]," ")[[1]])],headers_mapping$diff_abreviation[headers_mapping$current_period==input$currentperiod & headers_mapping$reference_period==input$refperiod][1],colnames(table_wl)[3])
          colnames(table_wl)[3]<-val_share_diff_front_end_name
        }
        colnames(table_wl)[1]<-"Market"
        eval(parse(text=paste0("table_wl <- mutate(table_wl,`",colnames(table_wl)[2],"` = paste0('', format(round(`",colnames(table_wl)[2],"`, 1), nsmall = 1)),`",
                               val_share_diff_front_end_name,"` = ifelse(`",val_share_diff_front_end_name,"` >= 0, paste0('+', format(round(`",val_share_diff_front_end_name,"`, 1), nsmall = 1)), format(round(`",val_share_diff_front_end_name,"`, 1), nsmall = 1)))")))
        
      }else if (nrow(table_wl)==1){
        val_share_diff_front_end_name<-gsub(strsplit(colnames(table_wl)[3]," ")[[1]][length(strsplit(colnames(table_wl)[3]," ")[[1]])],headers_mapping$diff_abreviation[headers_mapping$current_period==input$currentperiod & headers_mapping$reference_period==input$refperiod][1],colnames(table_wl)[3])
        table_wl <- table_wl[table_wl[,3] >= input$winthreshold,]
        table_wl <- head(table_wl,as.numeric(input$winnum))
        colnames(table_wl)[1]<-"Market"
        eval(parse(text=paste0("table_wl <- mutate(table_wl,`",colnames(table_wl)[2],"` = paste0('', format(round(`",colnames(table_wl)[2],"`, 1), nsmall = 1)),`",
                               val_share_diff_front_end_name,"` = ifelse(`",val_share_diff_front_end_name,"` >= 0, paste0('+', format(round(`",val_share_diff_front_end_name,"`, 1), nsmall = 1)), format(round(`",val_share_diff_front_end_name,"`, 1), nsmall = 1)))")))
      }
      
      
    }else if (type=="loss"){
      if (input$Martyp==market_level_total){
        stop("no other market")
      }
      # browser()
      table_wl <- top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[input$currentperiod]][[input$refperiod]]$losers 
      plstr="Loss"
      # 
      if (is.null(table_wl)){
        stop("No Losers Data")
      }else if (nrow(table_wl)>1){
        if (input$metrictype=="Value"){
          table_wl <- table_wl[order(table_wl[,6]),]
          # table_wl <- table_wl[table_wl[,4] <= input$lossthreshold,]
          # table_wl<-head(table_wl,as.numeric(input$lossnum))
          val_share_diff_front_end_name<-gsub(strsplit(colnames(table_wl)[4]," ")[[1]][length(strsplit(colnames(table_wl)[4]," ")[[1]])],headers_mapping$diff_abreviation[headers_mapping$current_period==input$currentperiod & headers_mapping$reference_period==input$refperiod][1],colnames(table_wl)[4])
          colnames(table_wl)[4]<-val_share_diff_front_end_name
        }else{
          table_wl <- table_wl[order(table_wl[,6]),]
          table_wl <- table_wl[table_wl[,3] <= input$lossthreshold,]
          table_wl<-head(table_wl,as.numeric(input$lossnum))
          val_share_diff_front_end_name<-gsub(strsplit(colnames(table_wl)[3]," ")[[1]][length(strsplit(colnames(table_wl)[3]," ")[[1]])],headers_mapping$diff_abreviation[headers_mapping$current_period==input$currentperiod & headers_mapping$reference_period==input$refperiod][1],colnames(table_wl)[3])
          colnames(table_wl)[3]<-val_share_diff_front_end_name
        }
        colnames(table_wl)[1]<-"Market"
        eval(parse(text=paste0("table_wl <- mutate(table_wl,`",colnames(table_wl)[2],"` = paste0(\"\", format(round(`",colnames(table_wl)[2],"`, 1), nsmall = 1)),`",
                               val_share_diff_front_end_name,"` = ifelse(`",val_share_diff_front_end_name,"` <= 0, paste0(\"\", format(round(`",val_share_diff_front_end_name,"`, 1), nsmall = 1)), format(round(`",val_share_diff_front_end_name,"`, 1), nsmall = 1)))")))
      }else if (nrow(table_wl)==1){
        table_wl <- table_wl[table_wl[,3] <= input$lossthreshold,]
        table_wl<-head(table_wl,as.numeric(input$lossnum))
        val_share_diff_front_end_name<-gsub(strsplit(colnames(table_wl)[3]," ")[[1]][length(strsplit(colnames(table_wl)[3]," ")[[1]])],headers_mapping$diff_abreviation[headers_mapping$current_period==input$currentperiod & headers_mapping$reference_period==input$refperiod][1],colnames(table_wl)[3])
        colnames(table_wl)[1]<-"Market"
        eval(parse(text=paste0("table_wl <- mutate(table_wl,`",colnames(table_wl)[2],"` = paste0(\"\", format(round(`",colnames(table_wl)[2],"`, 1), nsmall = 1)),`",
                               val_share_diff_front_end_name,"` = ifelse(`",val_share_diff_front_end_name,"` <= 0, paste0(\"\", format(round(`",val_share_diff_front_end_name,"`, 1), nsmall = 1)), format(round(`",val_share_diff_front_end_name,"`, 1), nsmall = 1)))")))
      }
      
      
    }else{
      
      table_wl<-top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[market_level_total]][[input$currentperiod]][[input$refperiod]]$winners

      plstr=colnames(table_wl)[length(colnames(table_wl))-2]
      if (nrow(table_wl)==0){
        table_wl<-top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[market_level_total]][[input$currentperiod]][[input$refperiod]]$losers
        plstr="Loss"
      }
      if (ncol(table_wl)<=4){
        val_share_diff_front_end_name<- colnames(table_wl)[2]
        colnames(table_wl)[3]<-"Value"
        # eval(parse(text=paste0("table_wl <- mutate(table_wl,`",colnames(table_wl)[2],"` = paste0(\"\", format(round(`",colnames(table_wl)[2],"`, 1), nsmall = 1)),`",
        #                        val_share_diff_front_end_name,"` = ifelse(`",val_share_diff_front_end_name,"` >= 0, paste0(\"+\", format(round(`",val_share_diff_front_end_name,"`, 1), nsmall = 1)), format(round(`",val_share_diff_front_end_name,"`, 1), nsmall = 1)))")))
      }else if (ncol(table_wl)<=6){
        if (!is.na(gsub(strsplit(colnames(table_wl)[4]," ")[[1]][length(strsplit(colnames(table_wl)[4]," ")[[1]])],headers_mapping$diff_abreviation[headers_mapping$current_period==input$currentperiod & headers_mapping$reference_period==input$refperiod][1],colnames(table_wl)[4]))){
          val_share_diff_front_end_name<- gsub(strsplit(colnames(table_wl)[4]," ")[[1]][length(strsplit(colnames(table_wl)[4]," ")[[1]])],headers_mapping$diff_abreviation[headers_mapping$current_period==input$currentperiod & headers_mapping$reference_period==input$refperiod][1],colnames(table_wl)[4])
        }else{
          val_share_diff_front_end_name<- colnames(table_wl)[4]
        }
        colnames(table_wl)[4]<-val_share_diff_front_end_name
        colnames(table_wl)[1]<-"Market"
        eval(parse(text=paste0("table_wl <- mutate(table_wl,`",colnames(table_wl)[2],"` = paste0(\"\", format(round(`",colnames(table_wl)[2],"`, 1), nsmall = 1)),`",
                               val_share_diff_front_end_name,"` = ifelse(`",val_share_diff_front_end_name,"` >= 0, paste0(\"+\", format(round(`",val_share_diff_front_end_name,"`, 1), nsmall = 1)), format(round(`",val_share_diff_front_end_name,"`, 1), nsmall = 1)))")))
      }else{
        if (!is.na(gsub(strsplit(colnames(table_wl)[3]," ")[[1]][length(strsplit(colnames(table_wl)[3]," ")[[1]])],headers_mapping$diff_abreviation[headers_mapping$current_period==input$currentperiod & headers_mapping$reference_period==input$refperiod][1],colnames(table_wl)[3]))){
          val_share_diff_front_end_name<- gsub(strsplit(colnames(table_wl)[3]," ")[[1]][length(strsplit(colnames(table_wl)[3]," ")[[1]])],headers_mapping$diff_abreviation[headers_mapping$current_period==input$currentperiod & headers_mapping$reference_period==input$refperiod][1],colnames(table_wl)[3])
        }else{
          val_share_diff_front_end_name<- colnames(table_wl)[3]
        }
        colnames(table_wl)[3]<-val_share_diff_front_end_name
        colnames(table_wl)[1]<-"Market"
        colnames(table_wl)[7]<-paste0(input$sharebase," share of ", total_market)
        eval(parse(text=paste0("table_wl <- mutate(table_wl,`",colnames(table_wl)[2],"` = paste0(\"\", format(round(`",colnames(table_wl)[2],"`, 1), nsmall = 1)),`",
                               val_share_diff_front_end_name,"` = ifelse(`",val_share_diff_front_end_name,"` >= 0, paste0(\"+\", format(round(`",val_share_diff_front_end_name,"`, 1), nsmall = 1)), format(round(`",val_share_diff_front_end_name,"`, 1), nsmall = 1)))")))
      }
    }
      
      
      
    if (nrow(table_wl)>0){
      table_wl[,1]<-paste0("<div style = 'padding-left:0px;'><span><b><font color='blue'>", table_wl[,1], "</font></b></span></div>")
    }
    
    # Create span tooltips
    # Get parameters for tooltips
    select_category <- input$prod
    select_refperiod <- input$refperiod
    select_currentperiod <- input$currentperiod
    select_time_num <- str_extract(select_currentperiod, "[0-9]+")
    # Determine if it is DMA or DYA
    if (any(str_detect(names(table_wl), "DPP|DP12W|DP3W"))) {
      value_s_diff <- paste0(colnames(table_wl)[2],"Difference Previous Period = ",colnames(table_wl)[2]," Current Period – ",colnames(table_wl)[2]," Previous Period")  
      market_size_d <- paste0('Market Size DPP = Market Size Difference Previous Period')
      pl <- paste0(plstr, " = (",colnames(table_wl)[2]," DPP/100) * Market Size")
    } else if (any(str_detect(names(table_wl), "DYA"))) {
      value_s_diff <- paste0(colnames(table_wl)[2]," Difference Year Ago = ",colnames(table_wl)[2]," Current Period – ",colnames(table_wl)[2]," Same Period Year Ago")  
      market_size_d <- paste0('Market Size DYA = Market Size Difference Year Ago')
      pl <-paste0(plstr, " = (",colnames(table_wl)[2]," DYA/100) * Market Size")
    }
    colwltab<-colnames(table_wl)
    diffld<-strsplit(val_share_diff_front_end_name," ")[[1]][length(strsplit(val_share_diff_front_end_name," ")[[1]])]
    markdfld<-paste("Market Size ",diffld)
    if (type=="total"){
      if (input$metrictype=="Value"){
        custom_header = htmltools::withTags(table(
          class = 'display',
          thead(
            tr(
              th(rowspan = 1, 'Market'),
              th(rowspan = 1, colnames(table_wl)[2], span(paste0(colnames(table_wl)[2],"= Product Sales / Total Share Base Sales"))),
              th(rowspan = 1, colnames(table_wl)[3], span("")),
              th(rowspan = 1, val_share_diff_front_end_name, span(value_s_diff)),
              th(rowspan = 1, gsub("'Mkt/Shr-Base'","Market",colnames(table_wl)[5]), span(market_size_d)),
              th(rowspan = 1, gsub("'Mkt/Shr-Base'","Market",colnames(table_wl)[6]), span(pl))
            )
          )
        )
        )
      }else{
        colnames(table_wl)[8]<-gsub(" D"," I",paste0(colnames(table_wl)[7],": ",diffld))
        custom_header = htmltools::withTags(table(
          class = 'display',
          thead(
            tr(
              th(rowspan = 1, 'Market'),
              th(rowspan = 1, colnames(table_wl)[2], span(paste0(colnames(table_wl)[2],"= Product Sales / Total Share Base Sales"))),
              th(rowspan = 1, val_share_diff_front_end_name, span(value_s_diff)),
              th(rowspan = 1, gsub("'Mkt/Shr-Base'","Market",colnames(table_wl)[4]), span(paste0('Total Sales of that Share Base within the Market'))),
              th(rowspan = 1, gsub("'Mkt/Shr-Base'","Market",colnames(table_wl)[5]), span(market_size_d)),
              th(rowspan = 1, plstr, span(pl)),
              th(rowspan = 1, colnames(table_wl)[7], span(colnames(table_wl)[7])),
              th(rowspan = 1, colnames(table_wl)[8], span(colnames(table_wl)[8]))
            )
          )
        ))
      }
    }else{
      if (input$metrictype=="Value"){
        custom_header = htmltools::withTags(table(
          class = 'display',
          thead(
            tr(
              th(rowspan = 1, 'Market'),
              th(rowspan = 1, colnames(table_wl)[2], span(paste0(colnames(table_wl)[2],"= Product Sales / Total Share Base Sales"))),
              th(rowspan = 1, colnames(table_wl)[3], span("")),
              th(rowspan = 1, val_share_diff_front_end_name, span(value_s_diff)),
              th(rowspan = 1, gsub("'Mkt/Shr-Base'","Market",colnames(table_wl)[5]), span(market_size_d)),
              th(rowspan = 1, gsub("'Mkt/Shr-Base'","Market",colnames(table_wl)[6]), span(pl))
            )
          )
        )
        )
      }else{
        custom_header = htmltools::withTags(table(
          class = 'display',
          thead(
            tr(
              th(rowspan = 1, 'Market'),
              th(rowspan = 1, colnames(table_wl)[2], span(paste0(colnames(table_wl)[2],"= Product Sales / Total Share Base Sales"))),
              th(rowspan = 1, val_share_diff_front_end_name, span(value_s_diff)),
              th(rowspan = 1, 'Market Size', span(paste0('Total Sales of that Share Base within the Market'))),
              th(rowspan = 1, markdfld, span(market_size_d)),
              th(rowspan = 1, plstr, span(pl))
            )
          )
        )
        )
      }
    }
    
    
    # Replace the span substitute < and >
    custom_header <- str_replace_all(custom_header, "&lt;", "<")
    custom_header <- str_replace_all(custom_header, "&gt;", ">")
    if (type=="total"){
      table_wl[1,1]<-gsub(paste0(" / ",input$sharebase),"",table_wl[1,1],fixed=T)
      if (ncol(table_wl)>4){
        format_datatable(table_wl, tabdesc = paste0("Losing_", input$Brands), digits = 1,
                         dmax = "Dec16", container = custom_header,  highlight_col = TRUE,fixwidth=F,widthauto=F,xscroll=T) %>%
          formatStyle('Market', cursor = 'pointer') %>%
          formatStyle(val_share_diff_front_end_name, textAlign = 'right') %>%
          formatStyle(colnames(table_wl)[2], textAlign = 'right')
      }else{
        format_datatable(table_wl, tabdesc = paste0("Losing_", input$Brands), digits = 1,
                         dmax = "Dec16", container = custom_header,  highlight_col = TRUE,fixwidth=F,widthauto=F,xscroll=T) %>%
          formatStyle(val_share_diff_front_end_name, textAlign = 'right') %>%
          formatStyle(colnames(table_wl)[2], textAlign = 'right')
      }
      
    }else{
      if (ncol(table_wl)>4){
        format_datatable(table_wl, tabdesc = paste0("Losing_", input$Brands), digits = 1,
                         dmax = "Dec16", container = custom_header,  highlight_col = TRUE) %>%
          formatStyle('Market', cursor = 'pointer') %>%
          formatStyle(val_share_diff_front_end_name, textAlign = 'right') %>%
          formatStyle(colnames(table_wl)[2], textAlign = 'right')
      }else{
        format_datatable(table_wl, tabdesc = paste0("Losing_", input$Brands), digits = 1,
                         dmax = "Dec16", container = custom_header,  highlight_col = TRUE) %>%
          formatStyle(val_share_diff_front_end_name, textAlign = 'right') %>%
          formatStyle(colnames(table_wl)[2], textAlign = 'right')
      }
      
    }

  # }, warning = function(war) {
  #   table_blank <- DT::datatable(data.frame(Alert = paste0("No markets meet this threshold")),option=list( dom = 't'),rownames=F,colnames="")
  }, error = function(err) {
    table_blank <- DT::datatable(data.frame(Alert = paste0("No markets meet this threshold")),option=list( dom = 't'),rownames=F,colnames="")
  }, finally = {
    table_blank <- DT::datatable(data.frame(Alert = paste0("No markets meet this threshold")),option=list( dom = 't'),rownames=F,colnames="")
  }) # END tryCatch
  bmwltab
}
#get list of best path trees
bestpathlist<-function(ex_search_tables,input,values,market_level_total,total_market){
  tree_name <- values$tree_name
  tree_name<-trimws(strsplit(tree_name,"/")[[1]][1])
  market<-values$mkt
  if (!is.null(values$tottabtype)){
    if (length(grep("TOP123",market))>0){
      market<-gsub("TOP123","",values$mkt)
      if (values$tottabtype=="tab1"){
        bestpath<-ex_search_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[values$mtp]][[input$currentperiod]][[input$refperiod]][[tree_name]]
      }else{
        bestpath<-ex_search_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[market_level_total]][[total_market]][[input$currentperiod]][[input$refperiod]][[tree_name]]
      }
    }else{
      bestpath<-ex_search_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[market]][[input$currentperiod]][[input$refperiod]][[tree_name]]
    }
  }else{
    bestpath<-ex_search_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[market]][[input$currentperiod]][[input$refperiod]][[tree_name]]
  }
  bestpath <<- bestpath
  if (ncol(bestpath)>4){
    bestpath <- bestpath[,c(2,3,4)]
  }
  
  if (ncol(bestpath)==4){
    bestpath[,4]<-as.numeric(bestpath[,4])
    bestpath[,4]<-round(bestpath[,4],1)
  }
  
  
  DT::datatable(bestpath,
                class = "compact", rownames = FALSE, selection = 'none',
                extensions = 'Scroller', 
                options = list(dom = "t", ordering = FALSE, class = "compact", pageLength = -1,scrollX=TRUE,
                               deferRender = TRUE, scrollY = 600, scroller = TRUE)) 
}
#get main tab
getushctab1<-function(market,input,list_markets,hoverover_df,values,market_level_total,total_market,multiplication_factor,headers_mapping,whichtab){
  out <- tryCatch(
    {
      retlist<-dataastable(market,input,list_markets,hoverover_df,values,market_level_total,total_market,headers_mapping,whichtab)
      table_1<-retlist$tab
      orig<-retlist$orig
      stopifnot(!is.null(table_1))
      # Temp code to multiply it to 100
      if(!exists("multiplication_factor")) multiplication_factor <- 100
      table_1[str_detect(names(table_1), "IPP|IMA")] <- table_1[str_detect(names(table_1), "IPP|IMA")] * multiplication_factor
      main_search <- table_1[,colnames(table_1)[2]][1] %>% read_html() %>% html_text()
      table_1 <- mutate(table_1, main_search = as.numeric(str_detect(product_org, main_search)))
      table_1 <- mutate(table_1, 
                        one = 1, 
                        row_number = cumsum(one),
                        mainrow = ifelse(tree != "" & row_number == 1, 1, 0),
                        mainrow = ifelse(tree != "" & row_number != 1, 3, mainrow),
                        mainrow = ifelse(mainrow != 1 & main_search == 1, 2, mainrow)) %>%
        select(-one, -row_number, -main_search, -product_org)
      bbb<<-table_1
      title1<<-market[1]
      if (!is.null(input$ic)){
        if (input$ic==F){
          cols <- grep(" I", colnames(table_1), ignore.case = TRUE)
          retlist$discard<-match(retlist$discard,colnames(table_1))
          table_1 %>% dataframe_to_datatable_3(toggle_structure = T, table_width = '100%', digits = 1,container = retlist$chead, scrollX=T,fcols=T,fcolnl=6,fhead=F,show_button = T,autow=F,widthmat='250px',colswidth=c(2),hde=retlist$discard)  %>%
            formatStyle(colnames(table_1)[cols],color = styleInterval(c(98,102), c('#3288bd', 'black', '#fdae61'))) %>%
            formatStyle("mainrow", target = "row", backgroundColor = styleInterval(c(0, 1, 2), c('white', '#deebf7', '#edf4fa', '#f9f9f9')))
        }else{
          cols <- grep(" C", colnames(table_1), ignore.case = TRUE)
          retlist$discard<-match(retlist$discard,colnames(table_1))
          lim1=sapply(1:length(cols),function(i)  ifelse(retlist$tab[cols[i]-1]/(retlist$tab[cols[i]-1]-retlist$tab[cols[i]])<0.98,"#3288bd",ifelse(retlist$tab[cols[i]-1]/(retlist$tab[cols[i]-1]-retlist$tab[cols[i]])>1.02,"#fdae61","#000000")))
          lim1[is.na(lim1)]<-"#000000"
          createcol <- function(table_1,i,j,lim1,li,lj) {
            table_1[i,j]<-ifelse(is.na(table_1[i,j]) | is.nan(table_1[i,j]) | is.infinite(table_1[i,j]) | table_1[i,j]=="" | table_1[i,j]=="NaN"
                                 ,paste0('<a href="" target="_blank" style="color:',lim1[li,lj],';float: right;"></a>')
                                 ,paste0('<a href="" target="_blank" style="color:',lim1[li,lj],';float: right;">', format(round(as.numeric(table_1[i,j]), 1), nsmall=0, big.mark=","), '</a>'))
            
            return(table_1)
          }
          for (cntr in 1:ncol(lim1)){
            for (cntl in 1:nrow(lim1)){
              table_1<- createcol(table_1,cntl,cols[cntr],lim1,cntl,cntr)
            }
          }
          table_1 %>% dataframe_to_datatable_3(toggle_structure = T, table_width = '100%', digits = 1,container = retlist$chead, scrollX=T,fcols=T,fcolnl=6,fhead=F,show_button = T,autow=F,widthmat=NULL,colswidth=NULL,hde=retlist$discard)  %>%
            # formatStyle(colnames(table_1)[cols], color = lim1) %>%
            # formatStyle(colnames(table_1)[cols],color = styleInterval(c(98,102), c('#3288bd', 'black', '#fdae61'))) %>%
            formatStyle("mainrow", target = "row", backgroundColor = styleInterval(c(0, 1, 2), c('white', '#deebf7', '#edf4fa', '#f9f9f9')))
        }
      }else{
        cols <- grep(" I", colnames(table_1), ignore.case = TRUE)
        retlist$discard<-match(retlist$discard,colnames(table_1))
        table_1 %>% dataframe_to_datatable_3(toggle_structure = T, table_width = '100%', digits = 1,container = retlist$chead, scrollX=T,fcols=T,fcolnl=6,fhead=F,show_button = T,autow=F,widthmat=NULL,colswidth=NULL,hde=retlist$discard)  %>%
          formatStyle(colnames(table_1)[cols],color = styleInterval(c(98,102), c('#3288bd', 'black', '#fdae61'))) %>%
          formatStyle("mainrow", target = "row", backgroundColor = styleInterval(c(0, 1, 2), c('white', '#deebf7', '#edf4fa', '#f9f9f9')))
      }
      
      
      
    # }, warning = function(war) {
    #   nodata <- data.frame(No = paste0(input$prod," did not change significantly in ", gsub("TOP123","",market),". Choose another market"))
    #   datatable(nodata, colnames = NULL, rownames = FALSE,  options = list(dom = 't'))
    }, error=function(cond) {
      nodata <- data.frame(No = paste0(input$prod," did not change significantly in ", gsub("TOP123","",market),". Choose another market"))
      datatable(nodata, colnames = NULL, rownames = FALSE,  options = list(dom = 't'))
    },finally={
      nodata <- data.frame(No = paste0(input$prod," did not change significantly in ", gsub("TOP123","",market),". Choose another market"))
      datatable(nodata, colnames = NULL, rownames = FALSE,  options = list(dom = 't'))
    }
  )
  out
}

#get winning title
getwintitle<-function(top_tables,input,minimal_treshold,headers_mapping){
  # Get Parameters for titles
  out <- tryCatch(
    {
      table_bmwin <- top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[input$currentperiod]][[input$refperiod]]$winners
      select_refperiod <-headers_mapping$blue_text_ref_per[headers_mapping$reference_period==input$refperiod][1]
      select_currentperiod <- headers_mapping$blue_text_cur_per[headers_mapping$current_period==input$currentperiod][1] 
      select_currentperiod_num <- "1" # TODO: to parameterize later
      select_sharebase <- input$sharebase
      select_prod <- input$prod
      select_threshold <- minimal_treshold
      select_market <- input$Martyp
      if (input$metrictype!="Value"){
        title_txt <- HTML(paste0('<h5>Top <em class = "tooltip-emphasize" title = "option available in next version">', div(style="display:inline-block", uiOutput("winnum")), '</em> <em class = "tooltip-emphasize">', select_market, '</em> for ',
                                 '<em class = "tooltip-emphasize">', select_prod,'</em> within <em class = "tooltip-emphasize">',select_sharebase, '</em> where <em class = "tooltip-emphasize">',
                                 select_currentperiod, '</em> ',colnames(table_bmwin)[2],' diff vs. <em class = "tooltip-emphasize">', select_refperiod, '</em> > <em class = "tooltip-emphasize" title = "option available in next version"> ', div(style="display:inline-block;", uiOutput("winthreshold")), '</em> ppt </h5>'))
      }else{
        title_txt <- HTML(paste0('<em class = "tooltip-emphasize">Winning', select_market, '</em> for ',
                                 '<em class = "tooltip-emphasize">', select_prod,'</em> within <em class = "tooltip-emphasize">',select_sharebase,'</em> for <em class = "tooltip-emphasize">',select_currentperiod))
      }
      
      
    },
    error=function(cond) {
      title_txt <- HTML(paste0('<h5>No markets meet this threshold</h5>'))
    },
    warning=function(cond) {
      title_txt <- HTML(paste0('<h5>No markets meet this threshold</h5>'))
    },
    finally={
    }
  )
}
#get winning threshold
getwinthresh<-function(input,top_tables,minimal_treshold){
  out <- tryCatch(
    {
      table_bmwin <- top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[input$currentperiod]][[input$refperiod]]$winners
      if (!is.null(table_bmwin)){
        if (input$metrictype!="Value"){
          max_threshold <- round(max(table_bmwin[,6], na.rm = T), 1)
          min_threshold <- round(min(table_bmwin[,6], na.rm = T), 1)
          table_bmwin <- table_bmwin[table_bmwin[,6] >= minimal_treshold, ]
        }else{
          max_threshold <- round(max(table_bmwin[,3], na.rm = T), 1)
          min_threshold <- round(min(table_bmwin[,3], na.rm = T), 1)
          table_bmwin <- table_bmwin[table_bmwin[,3] >= minimal_treshold, ]
        }
        if (nrow(table_bmwin)>0) {
          numericInput("winthreshold", "", width = "45px", value = minimal_treshold, min = min_threshold-0.1, max = max_threshold+0.1, step = 0.1)
        }else{
          numericInput("winthreshold", "", width = "45px", value = minimal_treshold, min = minimal_treshold-0.1, max = minimal_treshold+0.1, step = 0.1)
        }
      }else{
        numericInput("winthreshold", "", width = "45px", value = minimal_treshold, min = minimal_treshold-0.1, max = minimal_treshold+0.1, step = 0.1)
      }
    },
    error=function(cond) {
      numericInput("winthreshold", "", width = "45px", value = minimal_treshold, min = minimal_treshold-0.1, max = minimal_treshold+0.1, step = 0.1)
    },
    warning=function(cond) {
      numericInput("winthreshold", "", width = "45px", value = minimal_treshold, min = minimal_treshold-0.1, max = minimal_treshold+0.1, step = 0.1)
    },
    finally={
    }
  )
  
}

#get losing title
getlosstitle<-function(top_tables,input,minimal_treshold,headers_mapping){
  # Get Parameters for titles
  # Get Parameters for titles
  out <- tryCatch(
    {
      table_bmloss <- top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[input$currentperiod]][[input$refperiod]]$losers
      select_refperiod <-headers_mapping$blue_text_ref_per[headers_mapping$reference_period==input$refperiod][1]
      select_currentperiod <- headers_mapping$blue_text_cur_per[headers_mapping$current_period==input$currentperiod][1] 
      select_currentperiod_num <- "1" # TODO: to parameterize later
      select_sharebase <- input$sharebase
      select_prod <- input$prod
      select_threshold <- minimal_treshold
      select_market <- input$Martyp
      if (input$metrictype!="Value"){
        title_txt <- HTML(paste0('<h5>Bottom <em class = "tooltip-emphasize" title = "option available in next version">', div(style="display:inline-block", uiOutput("lossnum")), '</em> <em class = "tooltip-emphasize">', select_market, '</em> for ',
                                 '<em class = "tooltip-emphasize">', select_prod,'</em> within <em class = "tooltip-emphasize">',select_sharebase, '</em> where <em class = "tooltip-emphasize">',
                                 select_currentperiod, '</em>', colnames(table_bmloss)[2],' diff vs. <em class = "tooltip-emphasize">', select_refperiod, '</em> < <em class = "tooltip-emphasize" title = "option available in next version"> ', div(style="display:inline-block;", uiOutput("lossthreshold")), '</em> ppt </h5>'))
      }else{
        title_txt <- HTML(paste0('<em class = "tooltip-emphasize">Losing ', select_market, '</em> for ', 
                                 '<em class = "tooltip-emphasize">', select_prod,'</em> within <em class = "tooltip-emphasize">',select_sharebase,'</em> for <em class = "tooltip-emphasize">',select_currentperiod))
      }

      
    },
    error=function(cond) {
      title_txt <- HTML(paste0('<h5>No markets meet this threshold</h5>'))
    },
    warning=function(cond) {
      title_txt <- HTML(paste0('<h5>No markets meet this threshold</h5>'))
    },
    finally={
    }
  )
}
#get losing threshold
getlossthresh<-function(input,top_tables,minimal_treshold){
  out <- tryCatch(
    {
      table_bmloss <- top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[input$currentperiod]][[input$refperiod]]$losers
      if (!is.null(table_bmloss)) {
        if (input$metrictype=="Value"){
          min_threshold <- round(min(table_bmloss[,6], na.rm = T), 1)
          max_threshold <- round(max(table_bmloss[,6], na.rm = T), 1)
          table_bmloss <- table_bmloss[table_bmloss[,6] <= -minimal_treshold, ]
        }else{
          min_threshold <- round(min(table_bmloss[,3], na.rm = T), 1)
          max_threshold <- round(max(table_bmloss[,3], na.rm = T), 1)
          table_bmloss <- table_bmloss[table_bmloss[,3] <= -minimal_treshold, ]
        }
        
        if (nrow(table_bmloss)>0) {
          numericInput("lossthreshold", "", width = "45px", value = -minimal_treshold, min = -min_threshold-0.1, max = max_threshold+0.1, step = 0.1)
        }else{
          numericInput("lossthreshold", "", width = "45px", value = -minimal_treshold, min = -minimal_treshold, max = minimal_treshold, step = 0.1)
        }
      }else{
        numericInput("lossthreshold", "", width = "45px", value = -minimal_treshold, min = -minimal_treshold, max = minimal_treshold, step = 0.1)
      }
    },
    error=function(cond) {
      numericInput("lossthreshold", "", width = "45px", value = -minimal_treshold, min = -minimal_treshold, max = minimal_treshold, step = 0.1)
    },
    warning=function(cond) {
      numericInput("lossthreshold", "", width = "45px", value = -minimal_treshold, min = -minimal_treshold, max = minimal_treshold, step = 0.1)
    },
    finally={
    }
  )
  
}
#market alerts appendix
mktalrtsappendix<-function(input,minimal_treshold){
  result = tryCatch({
    select_currentperiod <- input$currentperiod
    select_threshold <- minimal_treshold
    if (input$refperiod == "Previous Period") {
      select_refperiod <- "DPP"
    } else if (input$refperiod == "Previous Year") {
      select_refperiod <- "DYA"
    } else {
      select_refperiod <- "DPP"
    }
    appstr<-paste0("<b>Bar Strength Explanation</b><br>
                   <table style='width:30%;font-family:arial, sans-serif;border-collapse:collapse;'>
                   <tr style='border:1px solid #dddddd;text-align:left;padding:8px;'>
                   <th>Icons</th>
                   <th>Significance</th> 
                   </tr>
                   <tr style='border:1px solid #dddddd;text-align:left;padding:8px;'>
                   <td><img src='Image5.PNG' height='42' width='42'><span> or </span><img src='Image-5.PNG' height='42' width='42'></td>
                   <td>Green indicates share growth and Red indicates share loss with the bars <br>represent the strength of the growth/loss</td> 
                   </tr>
                   </table>
                   <br><br><br><br>
                   <b>Why were these products chosen? </b><br>
                   The algorithm looks for a small number of actionable products to explain the total share change. Products are chosen to explains the trends at the highest level possible (eg Sub-Brand,Tier,Form); 
                   if there are no trends the actionable product will be the lowest level (eg sku). 
                   <br>
                   If a product has parentheses () at the end this gives additional, redundant, information. For example, if Dollar General has Tide/Liquid (16oz) than the issue is with Tide/Liquid, but user should note that there is only one size (16oz) for this product.<br><br>
                   * Likely competitors are chosen when the products are similar and the Value Share & some drivers (eg TDP, Price) are moving in opposite directions. Competitors also need to have a minimum change of 
                   +/- 0.5 pts Past 1 month DPP.<br> Small markets (<.05% of total) and markets without P&G sales are not included by default.")
    # str1 <- paste0("<p>* Likely competitors are chosen when the products are similar and the Value Share & some drivers (eg TDP, Price) are moving in opposite directions. Competitors also need to have a minimum change of ",
    #                "+/- ", select_threshold, " pts ", select_currentperiod, " ", select_refperiod, ".</p>",
    #                "<p>Driver Index: <span style = 'color: #3288bd';'> Below 98 </span>& <span style = 'color: #fdae61;'> Above 102</span></p>",
    #                "<h4>Key</h4>",
    #                "<p><i class='fa fa-trophy' style = 'color:	#FFD700; padding-right: 10px'></i> - Items on the first “branch” of the share tree that explain at least 40%. </p>",
    #                "<p><i class='fa fa-trophy' style = 'color: #C0C0C0; padding-right: 10px'></i> - Items on the first “branch” that explain >20%. If there is no gold winner then multiple silver cups (20-40%) are allowed. </p>",
    #                "<p><i class='fa fa-trophy' style = 'color: #CD7F32; padding-right: 10px'></i> - Items on the first “branch” that explain >15%. </p>",
    #                "<p><i class='fa fa-exclamation-circle' style = 'color: #FF0000; padding-right: 10px'></i> - When we have items in the opposite direction as the trend, that sum to least 30% of the magnitude of the total, we call this out as “cannibalization”. Cannibalization items use the same logic as the cups, but all items are marked with the same icon. </p>",
    #                "The number and depth of cups is a balance. We try to choose a small number of low level (actionable) items that explain most of the change. A lower level is chosen if its’ value is at least 2/3rds of its’ parent and it is still at least 50% of the initial item.")
    HTML(appstr)
  }, warning = function(w) {
    HTML("Process Text")
  }, error = function(e) {
    HTML("Process Text")
  }, finally = {
  })
  
}


#mktalrts title
mktlerts_title<-function(urlmkt,input,list_markets,values,drivers_time_series,market_level_total,total_market,hoverover_df){
  out <- tryCatch(
    {
      if(is.null(urlmkt)){
        market<-values$mkt
      }else{
        market<-urlmkt
      }
      market<-values$mkt
      select_category <- input$sharebase
      select_brand <- input$prod
      select_market <- market
      select_currentperiod <- input$currentperiod
      #custom js function
      if (is.null(select_market)) {
        title_txt <- h3(class = "select_mkt_msg", "From any of the three tables click on a market, to see brand details")
      } else{
        select_market <- gsub("TOP123","",gsub(paste0("/ ",input$sharebase),"",market,fixed=T))
        drvstr<-paste(values$drvs,collapse="\",\"")
        if ("ic" %in% names(values)){
          if (values$ic==TRUE){
            val="checked"
          }else{
            val=""
          }
        }else{
          val=""
        }
        if (values$ddchanged==FALSE){
          if ("mtypec" %in% names(values)){
            if (values$mtypec==TRUE){
              valc="checked"
            }else{
              valc=""
            }
          }else{
            valc=""
          }
        }else{
          valc=""
        }
        
        if (!is.null(values$tottabtype)){
          if (length(grep("TOP123",market))>0){
            market<-gsub("TOP123","",values$mkt)
            if (values$tottabtype=="tab1"){
              comptab<-rtb_competition[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[values$mtp]][[input$currentperiod]][[input$refperiod]]
            }else{
              comptab<-rtb_competition[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[market_level_total]][[total_market]][[input$currentperiod]][[input$refperiod]]
            }
          }else{
            comptab<-rtb_competition[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[market]][[input$currentperiod]][[input$refperiod]]
          }
        }else{
          comptab<-rtb_competition[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[market]][[input$currentperiod]][[input$refperiod]]
        }
        
        n=nrow(comptab)
        if (is.null(n)){
          n=0
        }
        jstext = paste0("function defaultActions(firstUse) {
                        if(firstUse === true) {
                        $('.dt-button-collection a').hide();
                        $('.dt-button-collection a').filter(function() { return $(this).text().indexOf('Value') === 0; }).next().show();
                        var arr =[\"",drvstr,"\"];
                        var colArr = [];
                        console.log('Strings from DRVSTR: ', arr);
                        $('.dt-button-collection a').each(function () {colArr.push($(this).text())});
                        console.log('Values from Table columns: ', colArr);
                        var i = 0;
                        var len = $('.dt-button-collection a').filter(function() { return $(this).text().indexOf('Value') === 0; }).length;
                        $('.dt-button-collection a').filter(function() { return $(this).text().indexOf('Value') === 0; }).each(function () {
                          if(i<len) {
                            $(this).next().text(arr[i]);
                            $(this).next().bind(\"click\", function(){
                            $(this).prev().trigger(\"click\");
                          });
                          if(i === len-1 && arr[i] == \"Average rating\") {
                            $(this).next().bind(\"click\", function(){
                              $(this).prev().prev().trigger(\"click\");
                              $(this).next().trigger(\"click\");
                              $(this).next().next().trigger(\"click\");
                            });
                          }
                        }
                        i++;
                        });
                        }
                        
                        }","function modifyColumnVisibility () {
                        var firstUse = true;
                        $('a:contains(\"Measure Selector\")').click(function(){
                        setTimeout(function(){
                        defaultActions(firstUse);
                        firstUse = false;
                        }, 1);
                        });
                        }")
        # ictoggle<-switchInput("ic",onLabel = "Show Index",offLabel = "Show Change",value = val,onStatus = "primary", offStatus = "info")
        if(values$mtpf==T){
          if (!is.null(n) & n>0){
            title_txt <- HTML(paste0('<script>',jstext,'</script><h5><em class = "tooltip-emphasize">',
                                     select_brand, '</em> and Key Competitors* within ',
                                     '<em class = "tooltip-emphasize">', select_category,
                                     '</em> at ', '<em class = "tooltip-emphasize">', select_market, '</em> for',
                                     '<em class = "tooltip-emphasize">', select_currentperiod, ' </em> ',
                                     '<script>modifyColumnVisibility();</script>
                                     <button id = "show_comp_trend" type = "button" class="btn btn-default action-button round-button">Competitor Trend</button>
                                     <button id = "show_comp_table" type = "button" class="btn btn-default action-button round-button">Competitor Ranking</button>
                                     <input type="checkbox" ',val,' id="ic" data-onstyle="primary" data-offstyle="info">
                                     <script>
                                     $(function() {
                                     $("#ic").bootstrapToggle({
                                     on: "Show Index",
                                     off: "Show Change"
                                     });
                                     })
                                     </script>
                                     <input type="checkbox" ',valc,' id="mtypec" data-onstyle="primary" data-offstyle="info">
                                     <script>
                                     $(function() {
                                     $("#mtypec").bootstrapToggle({
                                     on: "Show Key Products",
                                     off: "Show Key ',input$Martyp,'/Products",
                                     width: "15%"
                                     });
                                     })
                                     </script>
                                     <a id="downloadData" class="btn btn-default shiny-download-link " href="" target="_blank" download>
                                     <i class="fa fa-download"></i>
                                     Download
                                     </a></h5>'))
          }else{
            title_txt <- HTML(paste0('<script>',jstext,'</script><h5><em class = "tooltip-emphasize">',
                                     select_brand, '</em> and Key Competitors* within ',
                                     '<em class = "tooltip-emphasize">', select_category,
                                     '</em> at ', '<em class = "tooltip-emphasize">', select_market, '</em> for',
                                     '<em class = "tooltip-emphasize">', select_currentperiod, ' </em> ',
                                     '<script>modifyColumnVisibility();</script>
                                     <button id = "show_comp_trend" type = "button" class="btn btn-default action-button round-button">Competitor Trend</button>
                                     <input type="checkbox" ',val,' id="ic" data-onstyle="primary" data-offstyle="info">
                                     <script>
                                     $(function() {
                                     $("#ic").bootstrapToggle({
                                     on: "Show Index",
                                     off: "Show Change"
                                     });
                                     })
                                     </script>
                                     <input type="checkbox" ',valc,' id="mtypec" data-onstyle="primary" data-offstyle="info">
                                     <script>
                                     $(function() {
                                     $("#mtypec").bootstrapToggle({
                                     on: "Show Key Products",
                                     off: "Show Key ',input$Martyp,'/Products",
                                     width: "15%"
                                     });
                                     })
                                     </script>
                                     <a id="downloadData" class="btn btn-default shiny-download-link " href="" target="_blank" download>
                                     <i class="fa fa-download"></i>
                                     Download
                                     </a></h5>'))
          }
          
        }else{
          if (!is.null(n) & n>0){
            title_txt <- HTML(paste0('<script>',jstext,'</script><h5><em class = "tooltip-emphasize">',
                                     select_brand, '</em> and Key Competitors* within ',
                                     '<em class = "tooltip-emphasize">', select_category,
                                     '</em> at ', '<em class = "tooltip-emphasize">', select_market, '</em> for',
                                     '<em class = "tooltip-emphasize">', select_currentperiod, ' </em> ',
                                     '<script>modifyColumnVisibility();</script>
                                     <button id = "show_comp_trend" type = "button" class="btn btn-default action-button round-button">Competitor Trend</button>
                                     <button id = "show_comp_table" type = "button" class="btn btn-default action-button round-button">Competitor Ranking</button>
                                     <input type="checkbox" ',val,' id="ic" data-onstyle="primary" data-offstyle="info">
                                     <script>
                                     $(function() {
                                     $("#ic").bootstrapToggle({
                                     on: "Show Index",
                                     off: "Show Change"
                                     });
                                     })
                                     </script>
                                     <a id="downloadData" class="btn btn-default shiny-download-link " href="" target="_blank" download>
                                     <i class="fa fa-download"></i>
                                     Download
                                     </a></h5>'))
          }else{
            title_txt <- HTML(paste0('<script>',jstext,'</script><h5><em class = "tooltip-emphasize">',
                                     select_brand, '</em> and Key Competitors* within ',
                                     '<em class = "tooltip-emphasize">', select_category,
                                     '</em> at ', '<em class = "tooltip-emphasize">', select_market, '</em> for',
                                     '<em class = "tooltip-emphasize">', select_currentperiod, ' </em> ',
                                     '<script>modifyColumnVisibility();</script>
                                   <button id = "show_comp_trend" type = "button" class="btn btn-default action-button round-button">Competitor Trend</button>
                                   <input type="checkbox" ',val,' id="ic" data-onstyle="primary" data-offstyle="info">
                                   <script>
                                   $(function() {
                                   $("#ic").bootstrapToggle({
                                   on: "Show Index",
                                   off: "Show Change"
                                   });
                                   })
                                   </script>
                                   <a id="downloadData" class="btn btn-default shiny-download-link " href="" target="_blank" download>
                                   <i class="fa fa-download"></i>
                                   Download
                                   </a></h5>'))
          }
          
        }
        
        
        
      }
    },
    error=function(cond) {
      title_txt <- h3(class = "select_mkt_msg", "From any of the three tables click on a market, to see brand details")
    },
    warning=function(cond) {
      title_txt <- h3(class = "select_mkt_msg", "From any of the three tables click on a market, to see brand details")
    },
    finally={
    })
}
mktlerts_title2<-function(urlmkt,input,list_markets,values,drivers_time_series,market_level_total,total_market,hoverover_df,big_competitors_treshold){
  out <- tryCatch(
    {
      if(is.null(urlmkt)){
        market<-values$mkt
      }else{
        market<-urlmkt
      }
      market<-values$mkt
      select_category <- input$sharebase
      select_brand <- input$prod
      select_market <- market
      select_currentperiod <- input$currentperiod
      #custom js function
      if (is.null(select_market)) {
        title_txt <- h3(class = "select_mkt_msg", "From any of the three tables click on a market, to see brand details")
      } else{
        select_market <- gsub("TOP123","",gsub(paste0("/ ",input$sharebase),"",market))
        drvstr<-paste(values$drvs,collapse="\",\"")
        jstext = paste0("function defaultActions(firstUse) {
                        if(firstUse === true) {
                        $('.dt-button-collection a').hide();
                        $('.dt-button-collection a').filter(function() { return $(this).text().indexOf('Value') === 0; }).next().show();
                        var arr =[\"",drvstr,"\"];
                        var colArr = [];
                        console.log('Strings from DRVSTR: ', arr);
                        $('.dt-button-collection a').each(function () {colArr.push($(this).text())});
                        console.log('Values from Table columns: ', colArr);
                        var i = 0;
                        var len = $('.dt-button-collection a').filter(function() { return $(this).text().indexOf('Value') === 0; }).length;
                        $('.dt-button-collection a').filter(function() { return $(this).text().indexOf('Value') === 0; }).each(function () {
                        if(i<len) {
                        $(this).next().text(arr[i]);
                        $(this).next().bind(\"click\", function(){
                        $(this).prev().trigger(\"click\");
                        });
                        if(i === len-1 && arr[i] == \"Average rating\") {
                        $(this).next().bind(\"click\", function(){
                        $(this).prev().prev().trigger(\"click\");
                        $(this).next().trigger(\"click\");
                        $(this).next().next().trigger(\"click\");
                        });
                        }
                        }
                        i++;
                        });
                        }
                        
                        }","function modifyColumnVisibility () {
                        var firstUse = true;
                        $('a:contains(\"Measure Selector\")').click(function(){
                        setTimeout(function(){
                        defaultActions(firstUse);
                        firstUse = false;
                        }, 1);
                        });
                        }")
        title_txt <- HTML(paste0('<script>',jstext,'</script><h5><em class = "tooltip-emphasize">Competitors with share greater than <b></em>',big_competitors_treshold * 100,
                                 '<em class = "tooltip-emphasize"></b> pts and growing in the same direction as the P&G</em> ',
                                 '<script>modifyColumnVisibility();</script>
                                 <a id="downloadData" class="btn btn-default shiny-download-link " href="" target="_blank" download>
                                 <i class="fa fa-download"></i>
                                 Download
                                 </a></h5>'))
                        }
                        },
    error=function(cond) {
      title_txt <- h3(class = "select_mkt_msg", "From any of the three tables click on a market, to see brand details")
    },
    warning=function(cond) {
      title_txt <- h3(class = "select_mkt_msg", "From any of the three tables click on a market, to see brand details")
    },
    finally={
    }
)}
#select input to control number of rown in winner and losers tables
winnersnum<-function(top_tables, input, winthreshold, minimal_treshold) {
  out <- tryCatch(
    {
      table_bmwin <- top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[input$currentperiod]][[input$refperiod]]$winners
      if (input$metrictype!="Value"){
        if(nrow(table_bmwin)>1){
          table_bmwin <- table_bmwin[order(-table_bmwin[,6]),]
        }
        table_bmwin <- table_bmwin[table_bmwin[,3] >= winthreshold, ]
      }else{
        if(nrow(table_bmwin)>1){
          table_bmwin <- table_bmwin[order(-table_bmwin[,6]),]
        }
        table_bmwin <- table_bmwin[table_bmwin[,4] >= winthreshold, ]
      }
      if (nrow(table_bmwin)>0) {
        selectInput("winnum","",choices=seq(1, nrow(table_bmwin), 1), multiple=FALSE,width="55px",selected=ifelse(nrow(table_bmwin) > 0, ifelse(nrow(table_bmwin)>5,5,nrow(table_bmwin)), 0))
      } else {
        selectInput("winnum","",choices=seq(0, 0, 0), multiple=FALSE,width="55px",selected=0)
      }
    },
    error=function(cond) {
      selectInput("winnum","",choices=seq(0, 0, 0), multiple=FALSE,width="55px",selected=0)
    },
    warning=function(cond) {
      selectInput("winnum","",choices=seq(0, 0, 0), multiple=FALSE,width="55px",selected=0)
    },
    finally={
    }
  )
  
}
losersnum<-function(top_tables, input, lossthreshold, minimal_treshold){
  out <- tryCatch(
    {
      table_bmloss <- top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[input$currentperiod]][[input$refperiod]]$losers
      if (input$metrictype!="Value"){
        if(nrow(table_bmloss)>1){
          table_bmloss <- table_bmloss[order(table_bmloss[,6]),]
        }
        table_bmloss <- table_bmloss[table_bmloss[,3] <= lossthreshold, ]
      }else{
        if(nrow(table_bmloss)>1){
          table_bmloss <- table_bmloss[order(table_bmloss[,6]),]
        }
        table_bmloss <- table_bmloss[table_bmloss[,4] <= lossthreshold, ]
      }
      
      if (nrow(table_bmloss)>0) {
        selectInput("lossnum","",choices=seq(1, nrow(table_bmloss), 1), multiple=FALSE,width="55px",selected=ifelse(nrow(table_bmloss) > 0, ifelse(nrow(table_bmloss)>5,5,nrow(table_bmloss)), 0))
      } else {
        selectInput("lossnum","",choices=seq(0, 0, 0), multiple=FALSE,width="55px",selected=0)
      }
    },
    error=function(cond) {
      selectInput("lossnum","",choices=seq(0, 0, 0), multiple=FALSE,width="55px",selected=0)
    },
    warning=function(cond) {
      selectInput("lossnum","",choices=seq(0, 0, 0), multiple=FALSE,width="55px",selected=0)
    },
    finally={
    }
  )
  
}

getcomptab<-function(rtb_competition,input,values,market_level_total,total_market){
  out <- tryCatch(
    {
      market<-values$mkt
      if (!is.null(values$tottabtype)){
        if (length(grep("TOP123",market))>0){
          market<-gsub("TOP123","",values$mkt)
          if (values$tottabtype=="tab1"){
            comptab<-rtb_competition[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[values$mtp]][[input$currentperiod]][[input$refperiod]]
          }else{
            comptab<-rtb_competition[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[market_level_total]][[total_market]][[input$currentperiod]][[input$refperiod]]
          }
        }else{
          comptab<-rtb_competition[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[market]][[input$currentperiod]][[input$refperiod]]
        }
      }else{
        comptab<-rtb_competition[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[market]][[input$currentperiod]][[input$refperiod]]
      }
      for (i in 2:ncol(comptab)){
        comptab[,i]<-as.numeric(comptab[,i])
      }
      comptab<-round_df(comptab,2)
      comptab$mainrow<-c(0,rep(1,nrow(comptab)-1))
      format_datatable(comptab, tabdesc ="Competitor_Tab", digits = 1,
                       dmax = "Dec16",highlight_col = TRUE,global_hide_columns_regex = "mainrow" ,
                       global_show_columns_regex="nosuchcolumn") %>%
        formatStyle('mainrow', target = "row", backgroundColor = styleInterval(c(0.25,0.5), c('#d3d3d3', '#ffffff','#ffffff')))
    },
    error=function(cond) {
      table_blank <- data.frame(header = paste0("Not Applicable"))
      shiny::validate(need(nrow(table_blank) == 0, "There is only one possible competitor so no ranking information is available"))
    },
    warning=function(cond) {
      table_blank <- data.frame(header = paste0("Not Applicable"))
      shiny::validate(need(nrow(table_blank) == 0, "There is only one possible competitor so no ranking information is available"))
    },
    finally={
    }
  )
  
}

#competitoen appendix
comappendix<-function(input,minimal_treshold){
  result = tryCatch({
    str1 <- paste0('<b>Background:</b> To decide which competitor(s) are the most likely explanation of our share change SAMANTA scores all competitors with the following 5 criteria. Competitor(s) with the highest total scores are the most likely explanation. <i>Please note this is directional guidance, it does not replace the switching studies.</i><br>
                     <b>1. Share change:</b> must be opposite direction and similar magnitude to ours. <br>
                     <b>2. Product attributes:</b> Competitor(s) attributes must be "similar" to those that drove our share change. <br>
                     <b>3. Drivers:</b> Key drivers of competitor’s share change <i>(like price, distribution, promo, etc.)</i> must be opposite and "similar" in magnitude to our drivers.<br>
                     <b>4. Price tier:</b> must be similar to ours.<br>
                     <b>5. Historical correlation:</b> Over the long run competitor share must be strongly correlated to our share.<br>')
    HTML(paste(str1, sep = '<br/>'))
  }, warning = function(w) {
    HTML("Process Text")
  }, error = function(e) {
    HTML("Process Text")
  }, finally = {
  })
  
}
# Function to call in place of dropdownMenu
dropdownMenuCustom <- function (..., type = c("messages", "notifications", "tasks"), 
                                badgeStatus = "primary", icon = NULL, .list = NULL,dropmsg="",dropheader="") 
{
  type <- match.arg(type)
  if (!is.null(badgeStatus)) shinydashboard:::validateStatus(badgeStatus)
  items <- c(list(...), .list)
  lapply(items, shinydashboard:::tagAssert, type = "li")
  dropdownClass <- paste0("dropdown ", type, "-menu")
  if (is.null(icon)) {
    icon <- switch(type, messages = shiny::icon("info-circle"), 
                   notifications = shiny::icon("file"), tasks = shiny::icon("tasks"))
  }
  numItems <- length(items)
  if (is.null(badgeStatus)) {
    badge <- NULL
  }
  else {
    badge <- span(class = paste0("label label-", badgeStatus), 
                  numItems)
  }
  tags$li(
    class = dropdownClass, 
    a(
      href = "#", 
      class = "dropdown-toggle", 
      `data-toggle` = "dropdown", 
      icon, 
      badge
    ), 
    
    tags$ul(
      class = "dropdown-menu", 
      tags$li(
        class = "header", 
        dropheader
      ), 
      tags$div(HTML(dropmsg)),
      tags$li(
        tags$ul(class = "menu", items)
      )
    )
  )
}



###########################custom sort######################################
###########################custom sort######################################
sortcustom<-function(a){
  if (length(grep("\\.",a))==length(a) & !is.null(a) & length(a)>0){
    pos=gregexpr(pattern ='\\.',a)
    indx<-list()
    for(i in 1:length(a)){
      indx[[i]]<-substr(a[i],1,pos[[i]][1]-1)
    }
    indx<-unlist(as.numeric(indx))
    a<-a[order(indx)]
  }else if(!is.null(a) & sum(grepl("\\d",a))==0 & length(a)>0){
    a=sort(a)
  }else if(!is.null(a) & sum(grepl("\\d",a))<length(a) & length(a)>0){
    part1<-sort(a[!grepl("\\d",a)])
    part2<-a[grepl("\\d",a)]
    pos=gregexpr(pattern ='[[:digit:]]+',part2)
    indx<-list()
    for(i in 1:length(pos)){
      indx[[i]]<-substr(part2[i],pos[[i]][1],attr(pos[[i]],"match.length"))
    }
    indx<-unlist(as.numeric(indx))
    part2<-part2[order(indx)]
    a<-c(part1,part2)
  }
  return(a)
}

sortcustom_nested_list_names <- function(lst, rm_names) {
  if (length(names(lst))>1){
    nms <- setdiff(sortcustom(names(lst)), rm_names)
  }else{
    nms <- sortcustom(names(lst))
  }
  if (names(lst[[nms[1]]])[1] != c('winners')) {
    out <- sortcustom_nested_list_names(lst[[nms[1]]], rm_names)
  } else {
    out <- c()
  }
  
  
  
  c(list(nms), out)
}

escapeSingleQuotes <- function(vector) {
  gsub("'", "\\\\'", vector)
}

from_named_nested_list <- function(lst, lvls) {
  eval(parse(text = str_c("lst", str_c("[['", escapeSingleQuotes(lvls) ,"']]", collapse = ""))))
}

########

#TODO: this code rounds up dates to the last date of a month. remove it when it's no longer needed 
ceiling_dates_to_month <- function(dates){
  dates %>% ceiling_date('month', change_on_boundary = T) %>% `-`(days(1))
}


versupdate<-function(){
  "Version F23052018_01"
}