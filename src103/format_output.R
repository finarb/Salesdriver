graphs_css <- "<style>
.jqstooltip{
box-sizing: content-box;
}
.tooltipwrapper {
font-size: smaller;
margin: auto;
display: inline-block;
}
.divwrapper{
width: 200px;
text-align: center; 
}
</style>"


rmarkdown_title <- function(title, date = NULL) {
  if (!is.null(date))
    title %<>% paste0(., ": ", fmt_month(date))
  
  cat("\n")
  cat("---", "\n")
  cat("title:", sprintf('\"%s\"', title), "\n")
  cat("---", "\n")
}

fmt_month <- function(date) {
  if (is.character(date)) {
    date %<>% parse_date
  }
  format(date, "%b '%y")
}

mark_owner_ids2 <- function(tbl, depth = 2) { #use_zero_level_in_family = F
  
  grouping_variables <- attr(tbl, "grouping_variables") %>% .[1:depth]
  group_vars<- attr(tbl, "grouping_variables")
  if (length(group_vars)>0){
    for(i in 1:length(group_vars)){
      tbl[,group_vars[i]]<-trimws(tbl[,group_vars[i]])
    }
  }
  #lev <- ifelse(use_zero_level_in_family, 0, 1)
  #grouping_variable <- tbl %>% apply(2, nunique ) %>% .[. > lev] %>% names %>% head(1)
  
  tbl$owner_ids <- tbl %>% group_indices_(.dots = grouping_variables)
  tbl
}
mark_owner_ids <- function(tbl) { #use_zero_level_in_family = F
  na_nunique <- function(x) unique(x) %>% setdiff(NA) %>% length
  
  grouping_variables <- attr(tbl, "grouping_variables")
  primary_grouping_variable <- tbl[grouping_variables] %>% apply(2, na_nunique ) %>% .[. > 1] %>% names %>% head(1)

  #lev <- ifelse(use_zero_level_in_family, 0, 1)
  #grouping_variable <- tbl %>% apply(2, nunique ) %>% .[. > lev] %>% names %>% head(1)
  
  tbl$owner_ids <- tbl %>% group_indices_(.dots = primary_grouping_variable)
  tbl
}

mark_primary_rows <- function(tbl, target_varname)
{
  is_abs_maximum <- function(x) {
    abs(x) == max(abs(x))
  }
  is_secondary_maximum <- function(x, is_max, perc = .65) { 
    x_max = x[is_max] %>% unique; 
    if ( length(unique(sign(x_max))) > 1)
      return (rep(FALSE, length(x)))
    
    candidates = (x[sign(x_max) != sign(x)]) %>% .[abs(.) >= perc*abs(x_max)]
    if (length(candidates) == 0)
      return (rep(FALSE, length(x)))
    
    x == candidates[which.max(abs(candidates))] 
  }
  
  na_nunique <- function(x) unique(x) %>% setdiff(NA) %>% length
  
  grouping_variables <- attr(tbl, "grouping_variables")
  primary_grouping_variable <- tbl[grouping_variables] %>% apply(2, na_nunique ) %>% .[. > 1] %>% names %>% head(1)
  
  tbl_tmp <- tbl %T>% { .$Discounted_Metric <- .[[target_varname]] } %>% 
    group_by_(.dots = primary_grouping_variable) %>% 
    mutate( is_abs_max = is_abs_maximum(Discounted_Metric),
            is_sec_max = is_secondary_maximum(Discounted_Metric, is_abs_max),
            is_primary = ifelse(is_abs_max | is_sec_max, T, F)) %>%
    select(-is_abs_max, -is_sec_max)
  tbl$is_primary <- tbl_tmp$is_primary
  tbl
}


bubble_up_zero_level <- function(tbl) {
  zero_level_df <- tbl[tbl$family_levels == 0, , drop =F]
  tbl <- tbl[tbl$family_levels > 0, , drop = F]
  insert_idx <- c()
  for(i in 1:nrow(zero_level_df)){
    cur_row <- zero_level_df[i, , drop = F]
    
    for(j in 1:nrow(tbl)) {
      if(stri_trim(tbl[[j,2]]) == stri_trim(cur_row[[2]])) {
        insert_idx <- j
        tbl <- rbind(tbl[0:(insert_idx-1), , drop = F], cur_row, tbl[insert_idx:nrow(tbl), ,drop = F])
        break
      } else if(j == nrow(tbl)) {
        tbl <- rbind(tbl, cur_row)
      }
    }
  }
  tbl
}

bubble_up_zero_level_on_top <- function(tbl) {
  zero_level_df <- tbl[tbl$family_levels == 0 & is.na(tbl[,3]), , drop = F]
  tbl <- tbl[!(tbl$family_levels == 0) | !is.na(tbl[,3]), , drop = F]
  tbl <- rbind(zero_level_df, tbl)
  tbl
}

filter_colback <- function(is_primary_col, owners_id_col, filter_col, row_names = F) {
  if(row_names) {
    pointer_col <- 1
  } else {
    pointer_col <- 0
  }
  js_code <- JS(paste0("
                       table.column(",pointer_col,").nodes().to$().css({cursor: 'pointer'});
                       table.on('click', 'td.details-control', function() {
                       var td = $(this); 
                       row = table.row(td.closest('tr'));
                       cur_row_data = row.data();


                       table.rows().every( function ( rowIdx, tableLoop, rowLoop ) {
                       var data = this.data();
                       if(data[",owners_id_col,"] == cur_row_data[",owners_id_col,"] && data[",is_primary_col,"]=='FALSE') {
                       if(data[",filter_col,"] == 'HIDE') {
                         data[",filter_col,"] = 'SHOW';
                         data[",pointer_col,"] = '&CircleMinus;'
                       } else {
                        data[",filter_col,"] = 'HIDE';
                       
                       }
                       }

                       if(data[",owners_id_col,"] == cur_row_data[",owners_id_col,"] && data[",is_primary_col,"]=='TRUE') {
                       if(data[",pointer_col,"] == '&CircleMinus;') {
                         data[",pointer_col,"] = '&oplus;';
                       } else if(data[",pointer_col,"] == '&oplus;') {
                        data[",pointer_col,"] = '&CircleMinus;';
                       
                       }
                       }

                       this.data(data);
                       });
                       table.draw()
                       });
                       table.search('SHOW').draw();
                       
                       ")
  )
return(js_code)
}


PrettyName <- function(var.name, sep = "_") {
  var.name %<>% gsub("_", " ", .)
  var.name
}

merge_kpi_tables <- function(path)
{
  table <- NULL
  cur_path <- path
  
  grouping_variables <- c()
  info_text <- ""
  
  repeat {
    # append the table if it's non-empty
    cur_table <- cur_path$kpi_table
    
    if (!is.null(cur_table) && nrow(cur_table))
    {
      cur_info_text <- attr(cur_table, "info_text")
      if (!grepl(cur_info_text, info_text, fixed = TRUE))
        info_text %<>% paste(., "\n\n", cur_info_text )
      
      grouping_variables %<>% c(attr(cur_table, "grouping_variables")) %>% unique
      
      if (is.null(table)) {
        table <- cur_table
        
      } else {
        #cur_attr <- intersection_attributes(table, cur_table)
        attr_names <- diff_attr_names(table, cur_table)
        table %<>% bind_rows_tidy( cur_table )
        table %<>% delete_attributes(attr_names)
      }
    }
    
    # move on to the embedded table if one is available, or finish otherwise
    if (is.null(cur_path$kpi_tables)) {
      break;
    } else {
      cur_path <- cur_path$kpi_tables
    }
  }
  
  attr(table, "info_text") <- info_text
  attr(table, "grouping_variables") <- grouping_variables
  
  # create sorting variables, keep NAs up by introducing dummy sorting variables in addition to the proper ones.
  sorting_variables <- grouping_variables %>% rep(., each = 2)
  sorting_variables[seq(1, length(sorting_variables), by = 2)] %<>% paste("!is.na(", ., ")")
  
  table %<>% arrange_(.dots = sorting_variables)
  list(kpi_table = table)
}

detect_repetition <- function(tbl, col_indices)
{
  detect_repetition <- function(value) {
    repetition <- value == lag(value)
    repetition[is.na(repetition)] <- FALSE
    repetition
  }
  
  repetition_tbl <- tbl[col_indices]
  repetition <- rep(T, nrow(tbl))
  for (idx in col_indices) {
    repetition <- repetition & detect_repetition(tbl[[idx]])
    repetition_tbl[[idx]] <- repetition
  }
  repetition_tbl
}

detect_structure <- function(tbl, cnames, use_zero_level_in_family = F)
{
  parent_level <- function(potential_parent, potential_child) {
    non_na_parent <- potential_parent %>% is.na(.) %>% cummax() %>% not()
    non_na_child <- potential_child %>% is.na(.) %>% cummax() %>% not()
    if (sum(non_na_parent) >= sum(non_na_child) )
      return (0)
    
    if ( all(potential_parent[non_na_parent] == potential_child[non_na_parent]) )
      return (sum(non_na_parent))
    else
      return (0)
  }
  
  n_columns <- length(cnames)
  ###
  #parent_level(tbl[2, cnames], tbl[3, cnames]) 
  ###
  
  parent_levels <- sapply(2:nrow(tbl), function(idx_row_child) {
      parent_levels <- sapply(1:(idx_row_child-1), function(idx_row_parent) { 
        parent_level(tbl[idx_row_parent, cnames], tbl[idx_row_child, cnames]) 
        })  
      max(parent_levels)
  })
  embedding_tbl <- sapply(c(0, parent_levels), function(level) { c(rep(T, level), rep(F, n_columns-level)) })
  if (is.vector(embedding_tbl)) {
    embedding_tbl %<>% as.data.frame
  } else {
    embedding_tbl %<>% t
  }
  colnames(embedding_tbl) <- cnames
  owner_ids <- 1:nrow(tbl)
  family_levels <- apply(embedding_tbl, 1, sum)
  min_family_level <- ifelse(use_zero_level_in_family, 0, 1)
  
  is_primary <- family_levels <= min_family_level
  for(i in 1:length(family_levels)){
    if(family_levels[i] <= min_family_level | (i>1 && family_levels[i] < family_levels[i-1])) {
      cur_owner <- owner_ids[i]
      next
    }
    owner_ids[i] <- cur_owner
  }
  return(list(embedding_tbl = embedding_tbl, owner_ids = owner_ids, is_primary = is_primary, family_levels = family_levels))
}

treat_as_integer <- function(values) {
  is_integer_anyway <- all( is.na(values) ) || all( values == round(values) )
  has_large_sd <- (length(values) > 2 && sd(values, na.rm = T) > 2) %>% ifelse(is.na(.), FALSE, .)
  is_large <- ( all(abs(values) > 50) ) #&& mean(abs(values)) > 10^3 )
  is_integer_anyway || has_large_sd || (is_large) #length(values) == 1 && 
}











# format numbers in a datatables row
formatNumber <- function(dt_tbl, col_idx, values, digits)
{
  values <- values %>% .[!is.na(.)]
  if (!length(values))
    return (dt_tbl)
  
  if ( is.numeric(values) ) {
    if ( treat_as_integer(values) ) {
        dt_tbl %<>% formatCurrency(col_idx, currency = "", digits = 0)
    } else {
        dt_tbl %<>% formatCurrency(col_idx, currency = "", digits = digits)
    }
  }
  dt_tbl
}

# format numbers for datatables
formatNumber <- function(dt_tbl, col_idx, values, digits, use_treat_as_integer = T)
{
  values <- values %>% .[!is.na(.)]
  if (!length(values))
    return (dt_tbl)
  
  if ( is.numeric(values) ) {
    if ( use_treat_as_integer & treat_as_integer(values) ) {
      dt_tbl %<>% formatCurrency(col_idx, currency = "", digits = 0)
    } else {
      dt_tbl %<>% formatCurrency(col_idx, currency = "", digits = digits)
    }
  }
  dt_tbl
}

add_brand_and_original_order <- function(tbl, target_column = "Market/Product Attributes") {
  root_levels <- tbl %>% filter(`Embedding Level` == 0)
  tbl$BRAND <- NA
  tbl$original_order <- 1:nrow(tbl)
  
  for(i in 1:nrow(root_levels)) {
    cur_brand <- root_levels[[target_column]][i]
    idx_brand <- grepl(cur_brand, tbl[[target_column]])
    tbl$BRAND[idx_brand] <- cur_brand
  }
  tbl
}

#' A function that takes a table for a particular market that contains brands which gained and lost most in that market and returns a new table 
#' which is subsetted and sorted based on a target brand and its likely competitors specified in a competition column
#'
#' @param tbl - A table for a particular market that contains brands which gained and lost most in that market
#' @param target_brand - A target brand based on which the new table will be sorted 
#' @param competition_col - optional
#' @param target_column - optional
#' @param preprocess - optional
#'
#' @return
#' @export
#'
#' @examples
sort_subset_tbl_brand <- function(tbl, target_brand, competition_col = "Competition", target_column = "Market/Product Attributes", preprocess = T) {
  if(preprocess) tbl %<>% add_brand_and_original_order(target_column = target_column)
  
  tbl_target_brand <- tbl %>% filter(BRAND == target_brand)
  if(!nrow(tbl_target_brand)) stop("The target brand ", target_brand, " doesn't appear in this Market.")
  competitor_brands <- tbl_target_brand$Competition[1] %>% str_split(pattern = ", ") %>% .[[1]]
  
  for(comp_brand in competitor_brands) {
    tbl_competitor_brand <- tbl %>% filter(BRAND == comp_brand)
    tbl_target_brand %<>% plyr::rbind.fill(tbl_competitor_brand)
  }
  
  attributes_all <- attributes(tbl)
  attributes_res <- attributes(tbl_target_brand)
  attributes_all[names(attributes_res)] <- attributes_res
  attributes(tbl_target_brand) <- attributes_all
  return(tbl_target_brand)
}


dataframe_to_datatable <- function(tbl, 
                                   digits, 
                                   graph_types, 
                                   dt_options, 
                                   sort_target = NULL, 
                                   mark_structure = FALSE, 
                                   table_width = '80%',
                                   use_zero_level_in_family = T,
                                   bubble_up_zero_level = F,
                                   show_cumulative = F, 
                                   cum_tresh = 100,
                                   tabdesc="No_Desc",
                                   dmax=format(now(),"%B %Y"),
                                   global_hide_columns_regex = c("Alert","Embedding","Discounted","Category_Cum","PG_Cum","PG.*./Stat","Category.*./Stat","PG_Sales_IYA","Category_Sales_IYA","Acceleration","Missing","Appeared","3m","Driver","Share","Growth","Opp"),
                                   global_show_columns_regex = c("Value Share Total","Category Dollar Share Trend","PG Dollar Share Trend","Trend","Value_Volume Share Diff","Total CYA"),
                                   container = NULL,
                                   show_button,
                                   highlight_col = FALSE,widthauto=TRUE,xscroll=TRUE
                          )
{
  
  global_hide_columns_regex<-global_hide_columns_regex
  global_show_columns_regex<-global_show_columns_regex
  if (length(sort_target) > 1 && is.vector(sort_target)) {
    sort_order <- order(sort_target, decreasing = T)
    tbl %<>% .[sort_order,]
  }
  
  grouping_variables <- attr(tbl, "grouping_variables")
  callback_fun <- JS("return table;")
  # escape <- F
  # TODO Make a separate function for toggling functionallity and add that below the code for column filter
  if (mark_structure) {
    stopifnot(!is.null(grouping_variables))
    structure_results <- detect_structure(tbl, grouping_variables, use_zero_level_in_family = use_zero_level_in_family)
    # embedding_tbl <- detect_structure(tbl, grouping_variables)
    embedding_tbl <- structure_results$embedding_tbl
    for (var_name in grouping_variables) {
      var <- tbl[[var_name]]
      tbl[[var_name]] %<>% ifelse(embedding_tbl[,var_name], paste0(., " "), .)
    }
  }
  
  # NOTE/TODO: This might be a better way to do specify the filters https://datatables.net/examples/plug-ins/range_filtering.html
  column_filter <- attr(tbl, "column_filter")
  if (!is.null(column_filter))
  {
      cnames_filtered_content <- column_filter$target %>% paste(., "Content")

      tbl %<>% df_insert_column(values = .[[ column_filter$target ]] %>% format_number(digits = digits),
                                    cname = cnames_filtered_content, 
                                    left_cname = column_filter$target)
      tbl[[ column_filter$target ]] <- tbl[[ column_filter$filter ]] %>% as.factor

      idx_filtered_target  <- which(colnames(tbl) %in% column_filter$target) - 1
      idx_filtered_content <- which(colnames(tbl) %in% cnames_filtered_content) - 1
      idx_filter <- which(colnames(tbl) %in% column_filter$filter) - 1
      
      code_merge_filter_columns <- paste0("$('td:eq(", idx_filtered_target, ")', row).html( data[", idx_filtered_content, "] );")
      
      sl <- lapply(1:(idx_filtered_target), function(x) NULL)
      sl %<>% c(.,list(list(search = paste0('["', column_filter$default_value,'"]'))))
      sl %<>% c(.,lapply((idx_filtered_target+1):ncol(tbl), function(x) NULL))
      dt_options$searchCols <- sl
        
      option_filter <- "top"
      dt_options$columnDefs %<>% c(., list( list(visible = FALSE, targets = c(idx_filtered_content, idx_filter) ),
                                            list(orderable = FALSE, targets = idx_filtered_target) )) 
      

  } else {
      option_filter <- "none"
      # TODO Uncouple toggling from greying structure
      if(mark_structure)
      {
        # if there is no owners id ...
        atts <- attributes(tbl)
        if(!"owner_ids" %in% colnames(tbl)) 
            tbl$owner_ids <- structure_results$owner_ids
        
        # if there is no primary id but there is also a sort variable ...
        has_primary_id <- "is_primary" %in% colnames(tbl)
        if(!has_primary_id & !is.null(atts$sort_variable))
        {
          attribute_to_analyze <- atts$sort_variable
          tbl_tmp <- tbl %>% group_by(owner_ids) %>% mutate_(is_primary = lazyeval::interp(~rank(desc(abs(var))) == 1, var = as.name(attribute_to_analyze)))
          tbl$is_primary <- tbl_tmp$is_primary
          
        } else if(!has_primary_id) {
          tbl$is_primary <- structure_results$is_primary
        }
        
        # rewrite with column names with is_primary
        atts <- attributes(tbl)
        tbl$original_order <- 1:nrow(tbl)
        tbl %<>% group_by(owner_ids) %>% mutate(within_group_order = 1:n(), group_size = n(), all_primary = all(as.logical(is_primary)))
        tbl$Tog <- ifelse(tbl$group_size == 1 | tbl$all_primary, " ", '&oplus;')
        tbl$all_primary <- NULL
        tbl$group_size <- NULL
        
        # TODO: Make sorting functionallity generic and modularize into a function
        # if there is a particular variable on which to sort and rank the inter family members as primary
        if(!is.null(atts$sort_variable))
        {
          attribute_to_analyze <- atts$sort_variable
          if("owner_ids" %in% colnames(tbl)) 
          {
            tbl %<>% group_by(owner_ids) %>% mutate_(group_max = lazyeval::interp(~na_max(abs(var)), var = as.name(attribute_to_analyze)))
            
          } else {
            tbl %<>% mutate_(group_max = lazyeval::interp(~na_max(abs(var)), var = as.name(attribute_to_analyze)))
          }
          tbl %<>% ungroup() %>% arrange(desc(group_max), within_group_order)
          tbl$group_max <- NULL
          # tbl %<>% bubble_up_zero_level()
        }
       
        tbl$within_group_order <- NULL

        # If we need to bubble up zero levels on top
        if(bubble_up_zero_level) {
          tbl$family_levels <- structure_results$family_levels[tbl$original_order]
          tbl %<>% bubble_up_zero_level_on_top()
          tbl$family_levels <- NULL
        } 
        
        # TODO: Move the computation of variables such as this into the alerting framework instead of computing them here in the formatting functions!
        if(!is.null(atts$sort_variable) & show_cumulative) 
        {
          attribute_to_analyze <- atts$sort_variable
          total_sum <- tbl %>% select_(as.name(attribute_to_analyze)) %>% unlist() %>% abs() %>% na_sum()
          cumulative_sum <- tbl %>% select_(as.name(attribute_to_analyze))  %>% unlist() %>% abs() %>% cumsum() /total_sum * 100
          tbl$Cum_Info <- round(cumulative_sum)
        }
        
        # In case rows were reordered
        embedding_tbl <- embedding_tbl[tbl$original_order,, drop=FALSE]
        tbl$original_order <- NULL
        
        # return the original attributes
        missing_atts <- names(atts) %>% setdiff(names(attributes(tbl)))
        attributes(tbl) <- c(attributes(tbl), atts[missing_atts])
        tbl$filter_col <- ifelse(tbl$is_primary, "SHOW", "HIDE")
        tbl$is_primary %<>% as.character()
        
        
        is_primary_idx <- which(colnames(tbl) %in% "is_primary") - 1 
        filter_col_idx <- which(colnames(tbl) %in% "filter_col") - 1
        owner_ids_idx <- which(colnames(tbl) %in% "owner_ids") - 1
        
        if(show_cumulative) {
          Cum_Info_idx <- which(colnames(tbl) %in% "Cum_Info") - 1
          option_filter <- "top"
          closest_tresh <- tbl$Cum_Info[tbl$Cum_Info > cum_tresh] %>% .[1]
          if(is.na(closest_tresh)) closest_tresh <- cum_tresh
          sl <- lapply(1:(Cum_Info_idx), function(x) NULL)
          sl %<>% c(.,list(list(search = paste0('0 ... ', closest_tresh,''))))
          sl %<>% c(.,lapply((Cum_Info_idx+1):ncol(tbl), function(x) NULL))
          dt_options$searchCols <- sl
        }
        
        
        callback_fun <- filter_colback(is_primary_idx, owner_ids_idx, filter_col_idx)
        # escape <- -1
        dt_options$columnDefs %<>% c(., list( list(visible = FALSE, targets = c(is_primary_idx, filter_col_idx, owner_ids_idx) )))
      }
  }
  # use the global settings for hiding columns if any are present
  if (exists("global_hide_columns_regex") & exists("global_show_columns_regex")) {
    global_hide_columns_idx <- sapply(global_hide_columns_regex, function(pattern) grep(pattern, colnames(tbl)) ) %>% unlist %>% unique
    global_show_columns_idx <- sapply(global_show_columns_regex, function(pattern) grep(pattern, colnames(tbl)) ) %>% unlist %>% unique
    global_hide_columns_idx<-setdiff(global_hide_columns_idx,global_show_columns_idx)
    dt_options$columnDefs %<>% c(., list(list(visible = FALSE, targets = global_hide_columns_idx - 1 )))
  }else if (exists("global_hide_columns_regex")){
    global_hide_columns_idx <- sapply(global_hide_columns_regex, function(pattern) grep(pattern, colnames(tbl)) ) %>% unlist %>% unique
    dt_options$columnDefs %<>% c(., list(list(visible = FALSE, targets = global_hide_columns_idx - 1 )))
  }
  
  # Use normal header if container arg is NULL
  if (class(container) == "NULL") {
    container = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          lapply(colnames(tbl), th)
        )
      )
    ))}
  
  if (show_button == FALSE) {
    dom <- "rtip"
  } else {
    dom <- "Brtip"
  }
  
  if (highlight_col == TRUE) {
    dt_options$columnDefs %<>% c(., list(list(targets = c(0), class = "tbl-highlight")))
  }

  dt_tbl <- DT::datatable(tbl, filter = option_filter,
                          class = "compact stripe row-border order-column hover",
                          colnames = colnames(tbl),
                          extensions = c('FixedHeader','Buttons'), 
                          rownames = FALSE, 
                          escape = FALSE,
                          width = table_width,
                          options = c(list(
                            dom = dom,
                            buttons = 
                              list('colvis', list(
                                extend = 'collection',
                                buttons = list(list(extend='csv',
                                                    filename =  paste0(tabdesc,dmax)),
                                               list(extend='excel',
                                                    filename =  paste0(tabdesc,dmax)),
                                               list(extend='pdf',
                                                    filename=   paste0(tabdesc,dmax))),
                                text = 'Download'
                              ))),scrollX = xscroll,autoWidth  = widthauto,dt_options),
                          callback = callback_fun,
                          container = container,
                          selection = 'single'
  )
  
  # if (mark_structure) {
  #     for (var_name in grouping_variables) {
  #         values_repeated <- tbl[[var_name]][ embedding_tbl[,var_name] ] %>% unique
  #         values_first <- tbl[[var_name]][ !embedding_tbl[,var_name] ] %>% unique
  #         mapping_values <- c(values_first, values_repeated)
  #         mapping_color <- c(rep("black", length(values_first)), rep("lightgrey", length(values_repeated)))
  #         
  #         var_idx <- which(colnames(tbl) == var_name)
  #         dt_tbl %<>% formatStyle(var_idx, color = styleEqual(levels = mapping_values, values = mapping_color ))
  #       }
  # }

  for (i in colnames(tbl) %>% setdiff(grouping_variables) ) {
      dt_tbl %<>% formatNumber(col_idx = i, values = tbl[[i]], digits = digits)
  }
  
  if (!is.null(column_filter)) {
      if (!is.null(column_filter$colors)) {
        idx_non_grouping <- which(!colnames(tbl) %in% grouping_variables)
        dt_tbl %<>% formatStyle(columns = idx_non_grouping, valueColumns = "Status", color = styleEqual(levels = names(column_filter$colors), values = column_filter$colors ))
      }
    
      backup <- dt_tbl$x$options$rowCallback
      dt_tbl$x$options$rowCallback <- backup %>% gsub(") \\{", paste(") {", code_merge_filter_columns), .)
  }
  
  if (!is.null(graph_types)) { 
      dt_tbl$dependencies %<>% append(., htmlwidgets:::getDependency("sparkline"))
  }
  
  attr(dt_tbl, "table") <- tbl
  
  dt_tbl
}
dataframe_to_datatable_3 <- function(tbl, 
                                     digits = 1, 
                                     dt_options = NULL, 
                                     toggle_structure = FALSE,
                                     table_width = '100%',
                                     show_cumulative = F, 
                                     cum_tresh = 100,
                                     use_treat_as_integer = F,
                                     show_all_rows = TRUE,
                                     container = NULL,
                                     show_button = TRUE,
                                     tabdesc="No_Desc",
                                     dmax=format(now(),"%B %Y"),
                                     scrollX = F,fcols=F,fhead=F,fcolnl=0,fcolnr=0,autow=T,widthmat=NULL,colswidth=NULL,hde=NULL)
{
  atts <- attributes(tbl)
  
  if(show_all_rows)
  {
    dt_options$dom <- 'tip'
    dt_options$info <- FALSE
    dt_options$paging <- FALSE
    dt_options$pageLength <- nrow(tbl)
    
  }
  
  if(toggle_structure) {
    tbl_copy <- tbl
    tbl_copy <- cbind('Tog' = '&oplus;', tbl_copy)
    attributes(tbl_copy) <- attributes(tbl)
    attr(tbl_copy, "names") <- c('Tog', attr(tbl, "names"))
    tbl <- tbl_copy
    dt_options$columnDefs %<>% c(., list(list(orderable = FALSE, className = 'details-control', targets = 0)))
  }
  
  graph_types <- attr(tbl, "graph_types")
  if (!is.null(graph_types)) { # TODO: Only one type of sparkline is implemented so far, so the graph_type attribute isn't really being used yet.
    graph_column_idx <- which( colnames(tbl) %in% names(graph_types) )
    graph_opts <- sparklines_code(graph_column_idx - 1)
    dt_options$fnDrawCallback <- graph_opts$fnDrawCallback
    dt_options$columnDefs  %<>% c(., graph_opts$columnDefs)
  }
  
  callback_fun <- JS("return table;")
  option_filter <- "none"
  
  # This implements the logic for the toggling, in particular the part where the column called filter_col is generated with the "SHOW" and "HIDE" values
  # Then it calls the filter_colback JS function that implements the actual toggling. 
  if(toggle_structure) {
    # stopifnot("owner_ids" %in% colnames(tbl) & "is_primary" %in% colnames(tbl))  
    # # 
    # tbl %<>% group_by(owner_ids) %>% mutate(within_group_order = 1:n(), group_size = n(), all_primary = all(is_primary))
    
    # This is needed so that there is no toggle sign in grops of size one or in groups where all items are primary
    tbl$Tog <-  " "
    
    tbl$all_primary <- NULL
    tbl$group_size <- NULL
    tbl$within_group_order <- NULL
    # 
    # tbl$filter_col <- ifelse(tbl$is_primary, "SHOW", "HIDE")
    # tbl$is_primary %<>% as.character()
    # is_primary_idx <- which(colnames(tbl) %in% "is_primary") - 1 
    # filter_col_idx <- which(colnames(tbl) %in% "filter_col") - 1
    # owner_ids_idx  <- which(colnames(tbl) %in% "owner_ids") - 1
    mainrow_idx  <- which(colnames(tbl) %in% "mainrow") - 1
    # competition_idx <- which(colnames(tbl) %in% "Competition") - 1 
    # callback_fun   <- filter_colback(is_primary_idx, owner_ids_idx, filter_col_idx)
    # this hides the columns that are involved in the toggling functionality
    # 0 to hide to Tog column in the table
    if (!is.null(hde)){
      dt_options$columnDefs %<>% c(., list(list(visible = FALSE, targets = c(0, hde,mainrow_idx))))
    }else{
      dt_options$columnDefs %<>% c(., list(list(visible = FALSE, targets = c(0,mainrow_idx))))
    }
    
  }  

  # TODO: Make sorting functionallity generic and modularize into a function
  # if there is a particular variable on which to sort and rank the inter family members as primary
  if(!is.null(atts$sort_variable))
  {
    attribute_to_analyze <- atts$sort_variable
    if("owner_ids" %in% colnames(tbl)) 
    {
      tbl %<>% group_by(owner_ids) %>% mutate(within_group_order = 1:n())
      tbl %<>% group_by(owner_ids) %>% mutate_(group_max = lazyeval::interp(~na_max(abs(var)), var = as.name(attribute_to_analyze)))
      tbl %<>% ungroup() %>% arrange(desc(group_max), within_group_order)
    } else {
      tbl %<>% mutate_(group_max = lazyeval::interp(~na_max(abs(var)), var = as.name(attribute_to_analyze)))
      tbl %<>% ungroup() %>% arrange(desc(group_max))
    }
    tbl$group_max <- NULL
  }
  
  tbl$within_group_order <- NULL
  
  # return the original attributes
  missing_atts <- names(atts) %>% setdiff(names(attributes(tbl)))
  attributes(tbl) <- c(attributes(tbl), atts[missing_atts])
  
  # TODO: Move the computation of variables such as this into the alerting framework instead of computing them here in the formatting functions!
  if(!is.null(atts$sort_variable) & show_cumulative) 
  {
    attribute_to_analyze <- atts$sort_variable
    total_sum <- tbl %>% ungroup() %>% select_(as.name(attribute_to_analyze)) %>% unlist() %>% abs() %>% na_sum()
    cumulative_sum <- tbl %>% ungroup() %>% select_(as.name(attribute_to_analyze))  %>% unlist() %>% abs() %T>% {.[is.na(.)] <- 0} %>% cumsum() /total_sum * 100
    tbl$`Cum Impact` <- round(cumulative_sum)
  }
  
  if(show_cumulative) {
    Cum_Info_idx <- which(colnames(tbl) %in% "Cum Impact") - 1
    option_filter <- "top"
    closest_tresh <- tbl$`Cum Impact`[tbl$`Cum Impact` > cum_tresh] %>% .[1]
    if(is.na(closest_tresh)) closest_tresh <- cum_tresh
    sl <- lapply(1:(Cum_Info_idx), function(x) NULL)
    sl %<>% c(.,list(list(search = paste0('0 ... ', closest_tresh,''))))
    attr(tbl,"Cum_Info_Threshold") <- closest_tresh
    sl %<>% c(.,lapply((Cum_Info_idx+1):ncol(tbl), function(x) NULL))
    dt_options$searchCols <- sl
    
    dt_options$columnDefs %<>% c(., list( list(width = '30px', targets = Cum_Info_idx )))
  }
  
  # use the global settings for hiding columns if any are present
  if (exists("global_hide_columns_regex")) {
    global_hide_columns_idx <- sapply(global_hide_columns_regex, function(pattern) grep(pattern, colnames(tbl)) ) %>% unlist %>% unique
    dt_options$columnDefs %<>% c(., list( list(visible = FALSE, targets = global_hide_columns_idx - 1 )))
  }
  
  # Use normal header if container arg is NULL
  if (class(container) == "NULL") {
    container = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          lapply(colnames(tbl), th)
        )
      )
    ))}
  extend=NULL
  if (show_button == FALSE) {
    dt_options$dom <- "rtip"
  } else {
    extend<-c(extend,"Buttons")
    dt_options$dom <- "Brtip"
    dt_options$buttons = list('colvis'
                              # list(
                              #   extend = 'collection',
                              #   buttons = list(list(extend='csv',
                              #                       filename =  paste0(tabdesc,dmax)),
                              #                  list(extend='excel',
                              #                       filename =  paste0(tabdesc,dmax)),
                              #                  list(extend='pdf',
                              #                       filename=   paste0(tabdesc,dmax))),
                              #   text = 'Download'
                              #   )
                          )
  }
  

  if (scrollX == TRUE) {
    dt_options$scrollX <- TRUE
    dt_options$sScrollX = "10%"
  }
  
  if (fcols == TRUE) {
    extend<-c(extend,"FixedColumns")
    dt_options$fixedColumns = list(leftColumns = fcolnl, rightColumns = fcolnr)
  }

  if (fhead == TRUE) {
    extend<-c(extend,"FixedHeader")
    dt_options$fixedHeader=TRUE
  }
  dt_options$bSort<-FALSE

  global_hide_columns_idx <- grep(paste(c("Discounted","Alerts","owner","primary"),collapse="|"),colnames(tbl))
  dt_options$columnDefs %<>% c(., list(list(visible = FALSE, targets = global_hide_columns_idx )))
  if (!is.null(widthmat)){
    dt_options$columnDefs %<>% c(., list(list(width = widthmat, targets = colswidth)))
  }
  if (is.null(extend)){
    extend=list()
  }
  dt_options$autoWidth = autow
  dt_tbl <- DT::datatable(tbl, 
                          filter = option_filter,
                          colnames = colnames(tbl),
                          extensions = extend, 
                          rownames = FALSE, 
                          selection = list(mode="single", target="cell"),
                          escape = F, 
                          width = table_width,
                          options = dt_options,
                          callback = callback_fun,
                          #style = 'bootstrap',
                          #class = "table",
                          class = "compact stripe row-border order-column hover",
                          container = container)
  
  numeric_cols <- sapply(tbl, is.numeric) %>% which()
  ipp_cols <- str_detect(names(tbl), " I") %>% which()
  valshr_cols <- str_detect(names(tbl), "Val Share") %>% which()
  tdp_cols <- str_detect(names(tbl), "TDP") %>% which()
  numeric_cols <- setdiff(numeric_cols, ipp_cols)
  numeric_cols <- setdiff(numeric_cols, valshr_cols)
  numeric_cols <- setdiff(numeric_cols, tdp_cols)
  for (i in numeric_cols) {
    dt_tbl %<>% formatNumber(col_idx = i, values = tbl[[i]], digits = digits, use_treat_as_integer = use_treat_as_integer)
  }

  for (i in ipp_cols) {
    dt_tbl %<>% formatNumber(col_idx = i, values = tbl[[i]], digits = 1, use_treat_as_integer = use_treat_as_integer)
  }

  for (i in valshr_cols) {
    dt_tbl %<>% formatNumber(col_idx = i, values = tbl[[i]], digits = 1, use_treat_as_integer = use_treat_as_integer)
  }

  for (i in tdp_cols) {
    dt_tbl %<>% formatNumber(col_idx = i, values = tbl[[i]], digits = 0, use_treat_as_integer = use_treat_as_integer)
  }

  if (!is.null(graph_types)) {
    dt_tbl$dependencies %<>% append(., htmlwidgets:::getDependency("sparkline"))
  }

  attr(dt_tbl, "table") <- tbl
  dt_tbl<<-dt_tbl
  dt_tbl
}

counter <- function(start) {
  next_number <- function() {
    ret <- start
    start <<- start + 1
    ret
  }
  list( next_number = next_number )
}



format_datatable <- function(tbl, 
                             digits = 1, 
                             show_all_rows = TRUE, 
                             split_gains_losses = FALSE, 
                             sort_by_target = FALSE, 
                             mark_structure = FALSE, 
                             table_width = '100%',
                             use_zero_level_in_family = FALSE,
                             bubble_up_zero_level = F,
                             show_cumulative = F, 
                             cum_tresh = 100,tabdesc,dmax,
                             global_hide_columns_regex = c("Discounted $ Share Total DYA", "Sales IYA (MUSD)", "Alert","Embedding","Discounted","Category_Cum","PG_Cum","PG.*./Stat","Category.*./Stat","PG_Sales_IYA","Category_Sales_IYA","Acceleration","Missing","Appeared","3m","Driver","Share","Growth","Opp"),
                             global_show_columns_regex = c("Value Share Total","Category Dollar Share Trend","PG Dollar Share Trend","Trend","Value_Volume Share Diff","Total CYA"),
                             container = NULL,
                             highlight_col = FALSE,show_button = FALSE,fixwidth=FALSE,widthauto=TRUE,xscroll=TRUE)
{
  dt_options <- list( columnDefs = list(list(width = '300px', targets = c("Drivers")),
                                        list(width = '300px', targets = c("Alerts")) ))
  if (fixwidth==TRUE){
    dt_options <- list( columnDefs = list(list(width = '50px', targets = "_all")))
  }
  
  global.hide.check <<- global_hide_columns_regex
  global.show.check <<- global_show_columns_regex
  
  if(mark_structure) {
    tbl_copy <- tbl
    tbl_copy <- cbind('Tog' = '&oplus;', tbl_copy)
    attributes(tbl_copy) <- attributes(tbl)
    attr(tbl_copy, "names") <- c('Tog', attr(tbl, "names"))
    tbl <- tbl_copy
    dt_options$columnDefs %<>% c(., list(list(orderable = FALSE, className = 'details-control', targets = 0)))
  }
  
  if (show_all_rows)
  {
    dt_options$dom <- 'tip'
    dt_options$info <- FALSE
    dt_options$paging <- FALSE
    dt_options$pageLength <- nrow(tbl)
  }
  
  graph_types <- attr(tbl, "graph_types")
  if (!is.null(graph_types)) { # TODO: Only one type of sparkline is implemented so far, so the graph_type attribute isn't really being used yet.
    # cat( graphs_css )
    graph_column_idx <- which( colnames(tbl) %in% names(graph_types) )
    graph_opts <- sparklines_code(graph_column_idx - 1)
    dt_options$fnDrawCallback <- graph_opts$fnDrawCallback
    dt_options$columnDefs  %<>% c(., graph_opts$columnDefs)
  }
  
  target_variable <- attr(tbl, "target_variable")
  n_target_variable <- length(target_variable)
  
  if (split_gains_losses)
  {
    computed_variables <- attr(tbl, "computed_variables")
    n_computed_variables <- length(computed_variables)
    stopifnot( n_target_variable <= 1)
    stopifnot( n_target_variable == 1 || n_computed_variables == 1 )
    
    if (n_target_variable == 0) {
      target_variable <- computed_variables
    }
    
    tbl_gains <- tbl %>% .[ .[[target_variable]] > 0, ] 
    tbl_losses <- tbl %>% .[ .[[target_variable]] < 0, ] 
    
    if (sort_by_target) {
      list( gains  = dataframe_to_datatable(tbl_gains, 
                                            digits, 
                                            graph_types, 
                                            dt_options, 
                                            mark_structure = mark_structure, 
                                            table_width = table_width, 
                                            sort = tbl_gains[[target_variable]], 
                                            use_zero_level_in_family = use_zero_level_in_family,
                                            bubble_up_zero_level = bubble_up_zero_level,
                                            show_cumulative = show_cumulative, 
                                            cum_tresh = cum_tresh,tabdesc,dmax,
                                            global_hide_columns_regex = global_hide_columns_regex,
                                            global_show_columns_regex = global_show_columns_regex,
                                            container = container,
                                            highlight_col = highlight_col,show_button=show_button,widthauto,xscroll),
            
            losses = dataframe_to_datatable(tbl_losses, 
                                            digits, 
                                            graph_types, 
                                            dt_options, 
                                            mark_structure = mark_structure, 
                                            table_width = table_width, 
                                            sort = -tbl_losses[[target_variable]], 
                                            use_zero_level_in_family = use_zero_level_in_family, 
                                            bubble_up_zero_level = bubble_up_zero_level,
                                            show_cumulative = show_cumulative, 
                                            cum_tresh = cum_tresh,tabdesc,dmax,
                                            global_hide_columns_regex = global_hide_columns_regex,
                                            global_show_columns_regex = global_show_columns_regex,
                                            container = container,
                                            highlight_col = highlight_col,show_button=show_button,widthauto,xscroll)
            )
    } else {
      list( gains  = dataframe_to_datatable(tbl_gains, 
                                            digits, 
                                            graph_types, 
                                            dt_options, 
                                            mark_structure = mark_structure, 
                                            table_width = table_width, 
                                            use_zero_level_in_family = use_zero_level_in_family, 
                                            bubble_up_zero_level = bubble_up_zero_level,
                                            show_cumulative = show_cumulative, 
                                            cum_tresh = cum_tresh,tabdesc,dmax,
                                            global_hide_columns_regex = global_hide_columns_regex,
                                            global_show_columns_regex = global_show_columns_regex,
                                            container = container,
                                            highlight_col = highlight_col,show_button=show_button,widthauto,xscroll),
            losses = dataframe_to_datatable(tbl_losses, 
                                            digits, 
                                            graph_types, 
                                            dt_options, 
                                            mark_structure = mark_structure, 
                                            table_width = table_width, 
                                            use_zero_level_in_family = use_zero_level_in_family,
                                            bubble_up_zero_level = bubble_up_zero_level,
                                            show_cumulative = show_cumulative, 
                                            cum_tresh = cum_tresh,tabdesc,dmax,
                                            global_hide_columns_regex = global_hide_columns_regex,
                                            global_show_columns_regex = global_show_columns_regex,
                                            container = container,
                                            highlight_col = highlight_col,show_button=show_button,widthauto,xscroll) 
            )
    }
    
  } else {
    
    if (sort_by_target && n_target_variable == 1) {
      return (dataframe_to_datatable(tbl, 
                                     digits, 
                                     graph_types, 
                                     dt_options, 
                                     mark_structure = mark_structure, 
                                     table_width = table_width, 
                                     sort = tbl_gains[[target_variable]], 
                                     use_zero_level_in_family = use_zero_level_in_family,
                                     bubble_up_zero_level = bubble_up_zero_level,
                                     show_cumulative = show_cumulative, 
                                     cum_tresh = cum_tresh,tabdesc,dmax,
                                     global_hide_columns_regex = global_hide_columns_regex,
                                     global_show_columns_regex = global_show_columns_regex,
                                     container = container,
                                     highlight_col = highlight_col,show_button=show_button,widthauto,xscroll)
              )
      
    } else {
      return (dataframe_to_datatable(tbl, 
                                     digits, 
                                     graph_types, 
                                     dt_options, 
                                     mark_structure = mark_structure, 
                                     table_width = table_width, 
                                     use_zero_level_in_family = use_zero_level_in_family,
                                     bubble_up_zero_level = bubble_up_zero_level,
                                     show_cumulative = show_cumulative, 
                                     cum_tresh = cum_tresh,tabdesc,dmax,
                                     global_hide_columns_regex = global_hide_columns_regex,
                                     global_show_columns_regex = global_show_columns_regex,
                                     container = container,
                                     highlight_col = highlight_col,show_button=show_button,widthauto,xscroll)
              )
    }
  }
}

format_datatables <- function(path, 
                              digits = 1, 
                              show_all_rows = TRUE, 
                              skip_empty_tables = FALSE, 
                              split_gains_losses = FALSE, 
                              sort = FALSE, 
                              mark_structure = FALSE, 
                              table_width = '100%', 
                              use_zero_level_in_family = TRUE,
                              bubble_up_zero_level = F,
                              show_cumulative = F, 
                              cum_tresh = 100,
                              global_hide_columns_regex = c("Discounted $ Share Total DYA", "Sales IYA (MUSD)", "Alert","Embedding","Discounted","Category_Cum","PG_Cum","PG.*./Stat","Category.*./Stat","PG_Sales_IYA","Category_Sales_IYA","Acceleration","Missing","Appeared","3m","Driver","Share","Growth","Opp"),
                              global_show_columns_regex = c("Value Share Total","Category Dollar Share Trend","PG Dollar Share Trend","Trend","Value_Volume Share Diff","Total CYA")
)
{
  # format all results tables as data tables
  out_datatables <- list()
  section_info <- list()
  section_titles <- c()
  cur_path <- path
  
  repeat {
    # format and append the table if it's non-empty
    cur_table <- cur_path$kpi_table
    
    if (!is.null(cur_table) && (nrow(cur_table) || !skip_empty_tables ))
    {
      if (nrow(cur_table)) {
        out_datatable <- cur_table %>% format_datatable(digits = digits, 
                                                        show_all_rows = show_all_rows, 
                                                        split_gains_losses = split_gains_losses, 
                                                        sort = sort, 
                                                        mark_structure = mark_structure, 
                                                        table_width = table_width,
                                                        use_zero_level_in_family = use_zero_level_in_family,
                                                        bubble_up_zero_level = bubble_up_zero_level,
                                                        show_cumulative = show_cumulative, 
                                                        cum_tresh = cum_tresh,tabdesc,dmax,
                                                        global_hide_columns_regex = global_hide_columns_regex,
                                                        global_show_columns_regex = global_show_columns_regex)
        out_datatables <- c(out_datatables, list(out_datatable) )
        
      } else {
        out_datatables <- c(out_datatables, list("\n\n*No above-threshold findings.*\n\n") )
      }
      
      info <- attr(cur_table, "info_text")
      section_info <- c(section_info, list(info))
      
      grouping_vars <- attr(cur_table, "grouping_variables")
      section_title <- paste(mixed_case(grouping_vars), collapse=", ") %>% paste("By", .)
      section_titles %<>% c(section_title)
      
    }
    
    # move on to the embedded table if one is available, or finish otherwise
    if (is.null(cur_path$kpi_tables)) {
      break;
    } else {
      cur_path <- cur_path$kpi_tables
    }
  }
  
  list(datatables = out_datatables, titles = section_titles, info = section_info)
}

detect_redundant_rows_tbl <- function(tbl_high, tbl_low, redundancy_threshold = .99)
{
  is_redundant <- rep(F, nrow(tbl_high))
  grouping_variables <- attr(tbl_high, "grouping_variables")
  target_variable <- attr(tbl_high, "target_variable")
  
  # don't filter if no target variable was specified (this means that this table is not part of the exhaustive search alert)
  if (!length(target_variable) | nrow(tbl_high)==0)
    return (is_redundant)
  
  for (i in 1:nrow(tbl_high)) {
    cur_high <- tbl_high[i,]
    cur_low <- extract_subset(tbl_low, cur_high[grouping_variables], allow_empty_subset = T)
    var_high <- cur_high[[target_variable]]
    var_low <- cur_low[[target_variable]]
    # TODO check all bad cases that might happen here
    if(!is.null(var_low) && !is.na(var_high) && !is.na(var_low) && var_high == 0) {
      is_redundant[i] <- T
    } else {  
      is_redundant[i] <- any(var_low/var_high > redundancy_threshold, na.rm =T)
    }
  }
  is_redundant
}

detect_redundant_rows_tbls <- function(tbl, path, redundancy_threshold = .95) 
{
  is_redundant <- rep(F, nrow(tbl))
  while (TRUE) {
    tbl_low <- path$kpi_table
    if (is.null(tbl_low))
      break;
    is_redundant <- is_redundant | detect_redundant_rows_tbl(tbl, tbl_low, redundancy_threshold = redundancy_threshold)
    path <- path$kpi_tables
  }
  is_redundant
}

filter_redundant_rows_on_path <- function(path, redundancy_threshold = .95)
{
  if (is.null(path))
    return (NULL)
  
  is_redundant <- detect_redundant_rows_tbls(path$kpi_table, path$kpi_tables)
  path$kpi_table %<>% .[!is_redundant,]
  path$kpi_tables %<>% filter_redundant_rows_on_path(.)
  path
}

rep_paste <- function(x, n) rep(x, n) %>% paste0(collapse = "")



# initialize closure from which we'll receive chunk numbers
current_chunk_number <- counter(start = 1)
  

format_chunks <- function(output, title_level, chunk_idx_offset = current_chunk_number$next_number())
{
  output_var <- deparse(substitute(output))
  chunk_indices <- 1:length(output$datatables) - 1 + chunk_idx_offset
  
  chunk_titles <- paste0("\n", rep_paste("#", title_level), output$titles)
  chunk_info <- paste0("cat(", output_var, "$info[[", 1:length(out$info), "]])")
  
  chunk <- function(chunk_code, idx = "") paste0("```{r out_chunk", chunk_indices, idx, ", results='asis', echo=FALSE}", "\n", chunk_code, "\n", "```\n\n")
  
  output_is_string <- sapply(output$datatables, function(x) is.character(x) ) 
  output_is_split <- sapply(output$datatables, function(x) is.list(x) && all(names(x) %in% c("gains", "losses"))) 
  
  content <- paste0(output_var, "$datatables[[", 1:length(output$datatables), "]]")
  chunk_content <- 
    ifelse(output_is_string, 
           chunk( paste("cat(", content, ")") ),
           ifelse(output_is_split, 
                  paste0(chunk(chunk_info),
                         rep_paste("#", title_level+1), " Gains \n ", 
                         chunk(paste0(content, "$gains"), idx = 'a'), "\n", 
                         rep_paste("#", title_level+1), " Losses \n ", 
                         chunk(paste0(content, "$losses"), idx = 'b'), "\n"), 
                  chunk(paste(chunk_info, "\n", content)) )) 
  
  paste(chunk_titles, chunk_content, sep = " \n ")
}

exhaustive_search_path_quality <- function(path, root_node_name) {
  # extract search path
  search_path <- attr(path, "search_path")
  search_path <- tree_add_parent_node(node_name = root_node_name, path = search_path, mark_orphans = F)
  
  # determine the path quality
  path_quality <- tree_traverse_aggregations(search_path, root_node_name = root_node_name )
  path_quality %<>% mutate(avg_quality = quality_sum/n_tables)
  path_quality %<>% select(-n_tables, -quality_sum)
  path_quality$avg_quality <- path_quality$avg_quality / max(path_quality$avg_quality)
  path_quality %<>% arrange(desc(avg_quality))
  
  # return
  path_quality
}
# New formatting fucntions ------------------------------------------------

format_datatable2 <- function(tbl, 
                              digits = 1, 
                              show_all_rows = TRUE, 
                              split_gains_losses = FALSE, 
                              sort_by_target = FALSE, 
                              mark_structure = FALSE,
                              toggle_structure = FALSE,
                              table_width = '150%',
                              use_zero_level_in_family = TRUE,
                              bubble_up_zero_level = F,
                              show_cumulative = F, 
                              cum_tresh = 100,
                              use_treat_as_integer = T,
                              dt_options = NULL)
{
  target_variable <- attr(tbl, "target_variable")
  n_target_variable <- length(target_variable)
  
  if (split_gains_losses)
  {
    computed_variables <- attr(tbl, "computed_variables")
    n_computed_variables <- length(computed_variables)
    stopifnot( n_target_variable <= 1)
    stopifnot( n_target_variable == 1 || n_computed_variables == 1 )
    
    if (n_target_variable == 0) {
      target_variable <- computed_variables
    }
    
    tbl_gains <- tbl %>% .[ .[[target_variable]] > 0, ] 
    tbl_losses <- tbl %>% .[ .[[target_variable]] < 0, ] 
    
    if (sort_by_target) {
      list( gains  = dataframe_to_datatable2(tbl_gains, 
                                             digits, 
                                             graph_types, 
                                             dt_options, 
                                             mark_structure = mark_structure,
                                             toggle_structure = toggle_structure,
                                             table_width = table_width, 
                                             sort = tbl_gains[[target_variable]], 
                                             use_zero_level_in_family = use_zero_level_in_family,
                                             bubble_up_zero_level = bubble_up_zero_level,
                                             show_cumulative = show_cumulative, 
                                             cum_tresh = cum_tresh,
                                             use_treat_as_integer = use_treat_as_integer),
            
            losses = dataframe_to_datatable2(tbl_losses, 
                                             digits, 
                                             graph_types, 
                                             dt_options, 
                                             mark_structure = mark_structure,
                                             toggle_structure = toggle_structure,
                                             table_width = table_width, 
                                             sort = -tbl_losses[[target_variable]], 
                                             use_zero_level_in_family = use_zero_level_in_family, 
                                             bubble_up_zero_level = bubble_up_zero_level,
                                             show_cumulative = show_cumulative, 
                                             cum_tresh = cum_tresh,
                                             use_treat_as_integer = use_treat_as_integer)
      )
    } else {
      list( gains  = dataframe_to_datatable2(tbl_gains, 
                                             digits, 
                                             graph_types, 
                                             dt_options, 
                                             mark_structure = mark_structure,
                                             toggle_structure = toggle_structure,
                                             table_width = table_width, 
                                             use_zero_level_in_family = use_zero_level_in_family, 
                                             bubble_up_zero_level = bubble_up_zero_level,
                                             show_cumulative = show_cumulative, 
                                             cum_tresh = cum_tresh,
                                             use_treat_as_integer = use_treat_as_integer),
            losses = dataframe_to_datatable2(tbl_losses, 
                                             digits, 
                                             graph_types, 
                                             dt_options, 
                                             mark_structure = mark_structure,
                                             toggle_structure = toggle_structure,
                                             table_width = table_width, 
                                             use_zero_level_in_family = use_zero_level_in_family,
                                             bubble_up_zero_level = bubble_up_zero_level,
                                             show_cumulative = show_cumulative, 
                                             cum_tresh = cum_tresh,
                                             use_treat_as_integer = use_treat_as_integer) 
      )
    }
    
  } else {
    
    if (sort_by_target && n_target_variable == 1) {
      return (dataframe_to_datatable2(tbl, 
                                      digits, 
                                      graph_types, 
                                      dt_options, 
                                      mark_structure = mark_structure,
                                      toggle_structure = toggle_structure,
                                      table_width = table_width, 
                                      sort = tbl_gains[[target_variable]], 
                                      use_zero_level_in_family = use_zero_level_in_family,
                                      bubble_up_zero_level = bubble_up_zero_level,
                                      show_cumulative = show_cumulative, 
                                      cum_tresh = cum_tresh,
                                      use_treat_as_integer = use_treat_as_integer)
      )
      
    } else {
      return (dataframe_to_datatable2(tbl, 
                                      digits, 
                                      graph_types, 
                                      dt_options, 
                                      mark_structure = mark_structure,
                                      toggle_structure = toggle_structure,
                                      table_width = table_width, 
                                      use_zero_level_in_family = use_zero_level_in_family,
                                      bubble_up_zero_level = bubble_up_zero_level,
                                      show_cumulative = show_cumulative, 
                                      cum_tresh = cum_tresh,
                                      use_treat_as_integer = use_treat_as_integer)
      )
    }
  }
}



dataframe_to_datatable2 <- function(tbl, 
                                    digits = 1, 
                                    dt_options = NULL, 
                                    toggle_structure = FALSE,
                                    table_width = '100%',
                                    show_cumulative = F, 
                                    cum_tresh = 100,
                                    use_treat_as_integer = T,
                                    show_all_rows = TRUE)
{
  atts <- attributes(tbl)
  
  if(show_all_rows)
  {
    dt_options$dom <- 'tip'
    dt_options$info <- FALSE
    dt_options$paging <- FALSE
    dt_options$pageLength <- nrow(tbl)
  }
  
  if(toggle_structure) {
    tbl_copy <- tbl
    tbl_copy <- cbind('Tog' = '&oplus;', tbl_copy)
    attributes(tbl_copy) <- attributes(tbl)
    attr(tbl_copy, "names") <- c('Tog', attr(tbl, "names"))
    tbl <- tbl_copy
    dt_options$columnDefs %<>% c(., list(list(orderable = FALSE, className = 'details-control', targets = 0)))
  }
  
  graph_types <- attr(tbl, "graph_types")
  if (!is.null(graph_types)) { # TODO: Only one type of sparkline is implemented so far, so the graph_type attribute isn't really being used yet.
    graph_column_idx <- which( colnames(tbl) %in% names(graph_types) )
    graph_opts <- sparklines_code(graph_column_idx - 1)
    dt_options$fnDrawCallback <- graph_opts$fnDrawCallback
    dt_options$columnDefs  %<>% c(., graph_opts$columnDefs)
  }
  
  callback_fun <- JS("return table;")
  option_filter <- "none"
  
  # This implements the logic for the toggling, in particular the part where the column called filter_col is generated with the "SHOW" and "HIDE" values
  # Then it calls the filter_colback JS function that implements the actual toggling. 
  if(toggle_structure) {
    stopifnot("owner_ids" %in% colnames(tbl) & "is_primary" %in% colnames(tbl))  
    # 
    tbl %<>% group_by(owner_ids) %>% mutate(within_group_order = 1:n(), group_size = n(), all_primary = all(is_primary))
    
    # This is needed so that there is no toggle sign in grops of size one or in groups where all items are primary
    tbl$Tog <- ifelse(tbl$group_size == 1 | tbl$all_primary, " ", '&oplus;')
    
    tbl$all_primary <- NULL
    tbl$group_size <- NULL
    tbl$within_group_order <- NULL
    
    tbl$filter_col <- ifelse(tbl$is_primary, "SHOW", "HIDE")
    tbl$is_primary %<>% as.character()
    is_primary_idx <- which(colnames(tbl) %in% "is_primary") - 1 
    filter_col_idx <- which(colnames(tbl) %in% "filter_col") - 1
    owner_ids_idx  <- which(colnames(tbl) %in% "owner_ids") - 1
    callback_fun   <- filter_colback(is_primary_idx, owner_ids_idx, filter_col_idx)
    # this hides the columns that are involved in the toggling functionality
    dt_options$columnDefs %<>% c(., list(list(visible = FALSE, targets = c(is_primary_idx, filter_col_idx, owner_ids_idx))))
  }    
  # TODO: Make sorting functionallity generic and modularize into a function
  # if there is a particular variable on which to sort and rank the inter family members as primary
  if(!is.null(atts$sort_variable))
  {
    attribute_to_analyze <- atts$sort_variable
    if("owner_ids" %in% colnames(tbl)) 
    {
      tbl %<>% group_by(owner_ids) %>% mutate(within_group_order = 1:n())
      tbl %<>% group_by(owner_ids) %>% mutate_(group_max = lazyeval::interp(~na_max(abs(var)), var = as.name(attribute_to_analyze)))
      tbl %<>% ungroup() %>% arrange(desc(group_max), within_group_order)
    } else {
      tbl %<>% mutate_(group_max = lazyeval::interp(~na_max(abs(var)), var = as.name(attribute_to_analyze)))
      tbl %<>% ungroup() %>% arrange(desc(group_max))
    }
    tbl$group_max <- NULL
  }
  
  tbl$within_group_order <- NULL
  
  # return the original attributes
  missing_atts <- names(atts) %>% setdiff(names(attributes(tbl)))
  attributes(tbl) <- c(attributes(tbl), atts[missing_atts])
  
  # TODO: Move the computation of variables such as this into the alerting framework instead of computing them here in the formatting functions!
  if(!is.null(atts$sort_variable) & show_cumulative) 
  {
    attribute_to_analyze <- atts$sort_variable
    total_sum <- tbl %>% ungroup() %>% select_(as.name(attribute_to_analyze)) %>% unlist() %>% abs() %>% na_sum()
    cumulative_sum <- tbl %>% ungroup() %>% select_(as.name(attribute_to_analyze))  %>% unlist() %>% abs() %T>% {.[is.na(.)] <- 0} %>% cumsum() /total_sum * 100
    tbl$`Cum Impact` <- round(cumulative_sum)
  }
  
  if(show_cumulative) {
    Cum_Info_idx <- which(colnames(tbl) %in% "Cum Impact") - 1
    option_filter <- "top"
    closest_tresh <- tbl$`Cum Impact`[tbl$`Cum Impact` > cum_tresh] %>% .[1]
    if(is.na(closest_tresh)) closest_tresh <- cum_tresh
    sl <- lapply(1:(Cum_Info_idx), function(x) NULL)
    sl %<>% c(.,list(list(search = paste0('0 ... ', closest_tresh,''))))
    attr(tbl,"Cum_Info_Threshold") <- closest_tresh
    sl %<>% c(.,lapply((Cum_Info_idx+1):ncol(tbl), function(x) NULL))
    dt_options$searchCols <- sl
    
    dt_options$columnDefs %<>% c(., list( list(width = '30px', targets = Cum_Info_idx )))
  }
  
  # use the global settings for hiding columns if any are present
  if (exists("global_hide_columns_regex")) {
    global_hide_columns_idx <- sapply(global_hide_columns_regex, function(pattern) grep(pattern, colnames(tbl)) ) %>% unlist %>% unique
    dt_options$columnDefs %<>% c(., list( list(visible = FALSE, targets = global_hide_columns_idx - 1 )))
  }
  
  dt_tbl <- DT::datatable(tbl, 
                          filter = option_filter,
                          colnames = colnames(tbl),
                          extensions = 'FixedHeader', 
                          rownames = FALSE, 
                          escape = F,
                          width = table_width,
                          options = dt_options,
                          callback = callback_fun,
                          class = "compact stripe row-border order-column hover")
  
  numeric_cols <- sapply(tbl, is.numeric) %>% which()
  for (i in numeric_cols) {
    dt_tbl %<>% formatNumber(col_idx = i, values = tbl[[i]], digits = digits, use_treat_as_integer = use_treat_as_integer)
  }
  
  if (!is.null(graph_types)) { 
    dt_tbl$dependencies %<>% append(., htmlwidgets:::getDependency("sparkline"))
  }
  
  attr(dt_tbl, "table") <- tbl
  
  dt_tbl
}