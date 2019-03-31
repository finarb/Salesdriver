#get list structure for collapsible tree....functions vary on number of features needed
makeList<-function(x){
  if(nrow(x)>0 & ncol(x)>2){
    listSplit<-split(x[-1],x[1],drop=T)
    lapply(names(listSplit),function(y){list(name=y,children=makeList(listSplit[[y]]))})
  }else{
    lapply(seq(nrow(x[1])),function(y){list(name=x[,1][y],value=x[,2][y])})
  }
}
makeList1<-function(x){
  if(ncol(x)>1){
    listSplit<-split(x[-1],x[1],drop=T)
    lapply(names(listSplit),function(y){list(name=y,size=4.5,children=makeList1(listSplit[[y]]))})
  }else{
    lapply(seq(nrow(x[1])),function(y){list(name=x[,1][y],size=4.5)})
  }
}
makeList2<-function(x,y){
  if(ncol(x)>1){
    listSplitx<-split(x[-1],x[1],drop=T)
    listSplity<-split(y[-1],y[1],drop=T)
    z<-cbind(names(listSplitx),names(listSplitx))
    lapply(seq(nrow(z)),function(y){list(name=z[,1][y],size=2,level=z[,2][y],children=makeList2(listSplitx[[z[,1][y]]],listSplity[[z[,2][y]]]))})
  }else{
    lapply(seq(nrow(x[1])),function(z){list(name=x[,1][z],size=2,level=y[,1][z])})
  }
}
makeList3<-function(x,y,z){
  if(ncol(x)>1){
    listSplitx<-split(x[-1],x[1],drop=T)
    listSplity<-split(y[-1],y[1],drop=T)
    listSplitz<-split(z[-1],z[1],drop=T)
    k<-cbind(names(listSplitx),names(listSplitx),names(listSplitx))
    lapply(seq(nrow(k)),function(y){list(name=k[,1][y],size=2,level=k[,2][y],icon=k[,3][y],children=makeList3(listSplitx[[k[,1][y]]],listSplity[[k[,2][y]]],listSplitz[[k[,3][y]]]))})
  }else{
    lapply(seq(nrow(x[1])),function(l){list(name=x[,1][l],size=2,level=y[,1][l],icon=z[,1][l])})
  }
}
makeList4<-function(x,y,z,w){
  if(ncol(x)>1){
    listSplitx<-split(x[-1],x[1],drop=T)
    listSplity<-split(y[-1],y[1],drop=T)
    listSplitz<-split(z[-1],z[1],drop=T)
    listSplitw<-split(w[-1],w[1],drop=T)
    k<-cbind(names(listSplitx),names(listSplitx),names(listSplitx),names(listSplitx))
    lapply(seq(nrow(k)),function(y){list(name=k[,1][y],size=2,level=k[,2][y],icon=k[,3][y],textsize=10, children=makeList4(listSplitx[[k[,1][y]]],listSplity[[k[,2][y]]],listSplitz[[k[,3][y]]],listSplitw[[k[,4][y]]]))})
  }else{
    lapply(seq(nrow(x[1])),function(l){list(name=x[,1][l],size=2,level=y[,1][l],icon=z[,1][l],textsize=10)})
  }
}

#collapsible tree
getcolltree<-function(treedata,values,input,proxy1,market_level_total,total_market,best_paths,treetab=1,coltab){
  visgraph <- tryCatch({
    tree_name <- gsub("\\\\","",values$tree_name)
    tree_name<-trimws(strsplit(tree_name,"/")[[1]][1])
    market<-values$mkt
    if (!is.null(values$tottabtype)){
      if (length(grep("TOP123",market))>0){
        market<-gsub("TOP123","",values$mkt)
        if (values$tottabtype=="tab1"){
          df <- round_df(treedata[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[values$mtp]][[input$currentperiod]][[input$refperiod]][[tree_name]],digits=2,min_threshold=0)
        }else{
          df <- round_df(treedata[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[market_level_total]][[total_market]][[input$currentperiod]][[input$refperiod]][[tree_name]],digits=2,min_threshold=0)
        }
      }else{
        df <- round_df(treedata[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[market]][[input$currentperiod]][[input$refperiod]][[tree_name]],digits=2,min_threshold=0)
      }
    }else{
      df <- round_df(treedata[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[market]][[input$currentperiod]][[input$refperiod]][[tree_name]],digits=2,min_threshold=0)
    }

    #mek nodes in a sequence
    seqdf<-as.data.frame(cbind("seqid"=seq(1,nrow(df),1),"nodeid"=df$node_id,"parentid"=df$parent_node_id))
    seqdf<-sqldf("select a.*, b.seqid as parentmap from seqdf as a left join seqdf as b on a.parentid=b.nodeid")
    df$node_id<-seqdf$seqid
    df$parent_node_id<-seqdf$parentmap
    df$attribute_value<-gsub("\\\\","",df$attribute_value)
    if (is.null(coltab)){
      levs<-unique(df$attribute_name)
      if (treetab==1){
        if (input$treelegCheck==TRUE){
          colorslev=c("#ffe119","#0082c8","#f58231","#911eb4","#46f0f0","#d2f53c","#fabebe","#008080","#e6beff","#aa6e28","#fffac8","#800000","#aaffc3","#808000","#ffd8b1","#000080","#808080","#FF00FF","#808000","#008080")[1:length(levs)]
        }else{
          colorslev=c("#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de")[1:length(levs)]
        }
      }
      if (treetab==2){
        if (input$treelegCheck2==TRUE){
          colorslev=c("#ffe119","#0082c8","#f58231","#911eb4","#46f0f0","#d2f53c","#fabebe","#008080","#e6beff","#aa6e28","#fffac8","#800000","#aaffc3","#808000","#ffd8b1","#000080","#808080","#FF00FF","#808000","#008080")[1:length(levs)]
        }else{
          colorslev=c("#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de","#b0c4de")[1:length(levs)]
        }
      }
      
      #colorslev=randomColor(count = length(levs))
      colslevels<-as.data.frame(cbind(levs,colorslev))
    }else{
      levs<-as.character(coltab$attributes_with_colors)
      if (treetab==1){
        if (input$treelegCheck==TRUE){
          colorslev=as.character(coltab$colorslev)
        }else{
          colorslev=rep("#b0c4de",length(levs))
        }
      }
      if (treetab==2){
        if (input$treelegCheck2==TRUE){
          colorslev=as.character(coltab$colorslev)
        }else{
          colorslev=rep("#b0c4de",length(levs))
        }
      }
      colslevels<-as.data.frame(cbind(levs,colorslev))
    }
    
    
    #normalise size
    kpiname<-df$target_kpi_name[1]
    colnames(df)[4]<-"node_name"
    colnames(df)[5]<-"level"
    df$level=df$level+1
    kpiidx=grep(" shr",colnames(df),ignore.case = T)
    colnames(df)[kpiidx[length(kpiidx)]]<-"KPI"
    rootval<-max(df$KPI,na.rm=T)
    df$node_name<-paste0(df$node_name,": (",round(df$target_kpi_value,2),")[",round(df$KPI,2),"]{",df$cup_type,"__",df$attribute_name,"__",df$is_open_node,"}")
    df<-sqldf("select a.*,b.attribute_name as parent_attribute,b.node_name as parent_name, b.target_kpi_value as parent_target,b.KPI as parent_target_val_share,b.cup_type as parent_best_level from df as a left join df as b on a.parent_node_id=b.node_id")
    df$parent_target<-NULL
    #map the edges
    df$parent_name<-as.character(df$parent_name)
    df$node_name<-as.character(df$node_name)
    df$from<-df$parent_name
    df$to<-df$node_name
    edges<-as.data.frame(unique(na.omit(cbind(df$parent_node_id,df$node_id,df$cup_type))))
    colnames(edges)<-c("from","to","bestedge")
    edges$color<-ifelse(edges$bestedge==1,"red","lightsteelblue")
    edges<-edges[order(edges$from),]
    
    # levlist<-list()
    # levlist[[1]]<-cbind(1,1)
    # cntr=2
    # while (length(unlist(levlist))<(2*nrow(df))){
    #   print(length(unlist(levlist)))
    #   levlist[[cntr]]<-cbind(unique(edges$to[edges$from %in% levlist[[cntr-1]][,1]]),rep(cntr,length(unique(edges$to[edges$from %in% levlist[[cntr-1]][,1]]))))
    #   cntr=cntr+1
    # }
    # library(plyr)
    # levdf <- ldply(levlist, data.frame)
    # colnames(levdf)<-c("id","level")
    # df<-merge(df,levdf,by.x="node_id",by.y="id")
    icondf<-df
    df<-df[,c("parent_name","node_name","level","node_id")]
    df$node_name<-as.character(df$node_name)
    df$parent_id<-icondf$parent_node_id
    dfkeep <<- df
    bestpathlist<-NULL
    for (i in max(df$level):1){
      if (i==max(df$level)){
        series<-df[df$level==i,c("node_id","node_name")]
        graphdf<-data.frame(series)
        eval(parse(text=paste0("colnames(graphdf)[2]<-'level",i,"'")))
        eval(parse(text=paste0("graphdf$level",i,"<-as.character(graphdf$level",i,")")))
      }else{
        eval(parse(text=paste0("graphdf<-merge(graphdf,df[df$level==",i+1,",],by.x=c('node_id',","'level",i+1,"'),","by.y=c('node_id','node_name'),all.y=T)")))
        graphdf$level<-NULL
        graphdf$node_id<-graphdf$parent_id
        graphdf$parent_id<-NULL
        cols<-colnames(graphdf)
        idx<-grep("parent_name",cols)
        eval(parse(text=paste0("cols[",idx,"]<-'level",i,"'")))
        colnames(graphdf)<-cols
      }
      bestpathlist<-c(bestpathlist,paste0("Level",i))
    }
    
    findValues <- function(df){
      #dframe <- as.data.frame(rep(1,length(which(df$parent_name == df$node_name[1]))))
      dframe <- as.data.frame(df$node_name[2:(length(which(df$parent_name == df$node_name[1]))+1)])
      colnames(dframe) <- "name"
      dframe$name <- as.character(dframe$name)
      # Go through each node, finding the names of the children
      for(i in 1:10){
        round = i
        level <- paste0("level", round)
        for(j in 1:dim(dframe)[1]){
          dframe[j,level] <- paste(df$node_name[which(df$parent_name %in% strsplit(dframe[j,round], ";")[[1]])], collapse = ";")
        }
      }
      # Trim off anything extra
      dframe <- dframe[ ,which(sapply(dframe, function(x) all(x == "")) == FALSE)]
      # Find the column with the maximum numbers of leaves
      dd <- dim(dframe)[2]
      for(i in 1:dd){
        dframe[,(i+dd)] <- 1
        for(j in 1:40){
          # Count which nodes don't have a sub tree
          count <-  length(which(!(strsplit(dframe[j,i], ";")[[1]] %in% dfkeep$parent_name)))
          if(i == dd){count = 0}
          dframe[j,(i+dd)] <- max(length(which(df$parent_name %in% strsplit(dframe[j,i], ";")[[1]])),1)
          dframe[j,(i+dd)] <- dframe[j,(i+dd)] + count
        }
      }
      dframe <- dframe[is.na(dframe$level1) == FALSE,]
      HT <- max(colSums(dframe[,(dd+1):dim(dframe)[2]]))
      WD <- dd + 1
      maxChar <- max(nchar(df$node_name)+2)
      keepValues <- c(HT,WD, maxChar)
    }
    dfkeep <- df
    
    graphdf$node_id<-NULL
    #order columns
    x<-gsub("level","",colnames(graphdf))
    temporder<-as.data.frame(cbind(names=colnames(graphdf),idx=x),stringsAsFactors=FALSE)
    temporder$idx<-as.numeric(temporder$idx)
    temporder<-temporder[order(temporder$idx),]
    #format df for consecutive levels
    graphdf<-unique(graphdf[,temporder$names])
    graphdf[is.na(graphdf)] <- "No Sub Tree"
    graphdf<-data.frame(graphdf,stringsAsFactors = FALSE)
    root<-paste0(market,":",graphdf[1,1])
    if (ncol(graphdf)>2){
      graphdf<-graphdf[,2:ncol(graphdf)]
    }else{
      graphdf<-graphdf
    }
    
    jsonOut<-toJSON(list(name=root,size=2,level="lightsteelblue",icon="",textsize=10,children=makeList4(graphdf,graphdf,graphdf,graphdf)))
    jsonOut2 <- str_replace_all(jsonOut, "[\r\n]" , "")
    jsonOut2<- str_replace_all(jsonOut, "children" , "_children")
    str<-strsplit(jsonOut2,"\"")[[1]]
    str[grep("level",str)+2]<-"lightsteelblue"
    str[grep("icon",str)+2]<-""
    levid<-0
    treestr<<-str
    for (i in 1:length(str)){
      n1=str_count(str[i], "\\[")
      n2=str_count(str[i], "\\(")
      n3=str_count(str[i], "\\{")
      if (n1==1 & n2>=1 & n3==1){
        print(str[10])
        l1<-gregexpr(pattern ='\\{',str[i])[[1]][1]
        l2<-gregexpr(pattern ='\\}',str[i])[[1]][1]
        colr<-strsplit(substr(str[i],l1+1,l2-1),"__")[[1]][2]
        levnum=as.numeric(strsplit(substr(str[i],l1+1,l2-1),"__")[[1]][1])
        isopen=as.character(strsplit(substr(str[i],l1+1,l2-1),"__")[[1]][3])
        str[i]=trimws(substr(str[i],1,l1-1))
        if (str[i+4]=="level"){
          str[i+6]<-as.character(colslevels$colorslev[colslevels$levs==colr][1])
        }
        
        if (isopen=="TRUE" & (i+14)<=length(str) & str[i+14]=="_children"){
          str[i+14]="children"
        }
        if (levnum!=0 & (i+10)<=length(str) & levid>0){
          eval(parse(text=paste0("str[i+10]='Image",levnum,".PNG'")))
        }
        if (levid==0){
          levid=1
        }
        n1=str_count(str[i], "\\[")
        n2=str_count(str[i], "\\]")
        if (n1==1 & n2==1){
          if(str[i+2]=="size"){
            l1<-gregexpr(pattern ='\\[',str[i])[[1]][1]
            l2<-gregexpr(pattern ='\\]',str[i])[[1]][1]
            str[i+3]=gsub("2",gsub("-","",substr(str[i],l1+1,l2-1)),str[i+3])
          }
        }
        l1<-gregexpr(pattern ='\\[',str[i])[[1]][1]
        l2<-gregexpr(pattern ='\\]',str[i])[[1]][1]
        vs<-substr(str[i],l1+1,l2-1)
        n2=str_count(str[i], "\\(")
        if (n2==1){
          l1<-gregexpr(pattern ='\\(',str[i])[[1]][1]
          l2<-gregexpr(pattern ='\\)',str[i])[[1]][1]
        }else if(n2==2){
          l1<-gregexpr(pattern ='\\(',str[i])[[1]][2]
          l2<-gregexpr(pattern ='\\)',str[i])[[1]][2]
        }
        diff<-as.numeric(substr(str[i],l1+1,l2-1))
        vsc<-gsub("-","",substr(str[i],l1+1,l2-1))
        signs<-ifelse(diff>0,"+","-")
        #str[i]<-paste(substr(str[i],1,l1),vs,"/",signs,vsc,")")
        str[i]<-paste(substr(str[i],1,l1),signs,vsc,")")
      }
    }
    jsonOut2<-paste(str,collapse="\"")
    proxy1 %>% selectCells(NULL)
    keepValues <- c(nrow(graphdf),ncol(graphdf)+2,max(unlist(sapply(df$node_name,nchar))))#findValues(df)
    bestpathlist<-paste(sort(bestpathlist),collapse=",")
    retlist<-list()
    retlist$coltab<-colslevels
    retlist$plot<-fCTR(jsonOut2,width="800px", HT = keepValues[1], WD = keepValues[2],maxChar=keepValues[3],color1="lightsteelblue",color2="white",color3="lightsteelblue",color4="lightsteelblue",minimum_distance = 21,top_bar = NULL, maxsize=rootval/1000)
    retlist
  }, error = function(err) {
    type <- c("NULL")
    name <- c("No Selected Path")
    size <- c(rep(3840,1))
    data <- data.frame(type, name, size)
    jsonOut<-toJSON(list(name="No Selected Path",children=makeList(data)))
    jsonOut2 <- str_replace_all(jsonOut, "[\r\n]" , "")
    retlist<-list()
    retlist$plot<-fCTR(jsonOut2,width="1200px",height="600px")
    retlist$coltab<-as.data.frame("No Data Available")
    retlist
  }, finally = {
  }) # END tryCatch
  visgraph
}

#get network tree
nettree<-function(cristmas_tree_tables_part,cristmas_tree_tables_full,values,input,market_level_total,total_market){
  visgraph <- tryCatch({
    #get respective table: this logic only works for table names in format XXXX/AAAA and tries to find the AAAA table
    tree_name <- input$ushctab1_cell_clicked$value %>% read_html() %>% html_node('span') %>% html_text()
    tree_name<-trimws(strsplit(tree_name,"/")[[1]][1])
    market<-values$mkt
    if (input$show_whichtree=="Simplified Tree"){
      if (length(grep("TOP123",market))>0){
        df <- round_df(cristmas_tree_tables_part[[input$category]][[input$Brands]][[market_level_total]][[total_market]][[tree_name]]$ct_table,digits=2,min_threshold=0)
      }else{
        df <- round_df(cristmas_tree_tables_part[[input$category]][[input$Brands]][[input$Martyp]][[market]][[tree_name]]$ct_table,digits=2,min_threshold=0)
      }
    }else{
      if (length(grep("TOP123",market))>0){
        df <- round_df(cristmas_tree_tables_full[[input$category]][[input$Brands]][[market_level_total]][[total_market]][[tree_name]]$ct_table,digits=2,min_threshold=0)
      }else{
        df <- round_df(cristmas_tree_tables_full[[input$category]][[input$Brands]][[input$Martyp]][[market]][[tree_name]]$ct_table,digits=2,min_threshold=0)
      }
    }
    #get list of nodes
    nodelist<-unique(na.omit(c(as.character(df$parent_name),as.character(df$node_name))))
    nodelist<-as.data.frame(nodelist)
    colnames(nodelist)<-"Nodes"
    nodelist$id<-row.names(nodelist)
    nodelist$Nodes<-as.character(nodelist$Nodes)
    nodelist$id<-as.numeric(nodelist$id)
    #map the edges
    df$parent_name<-as.character(df$parent_name)
    df$node_name<-as.character(df$node_name)
    df<-merge(df,nodelist,by.x="parent_name",by.y="Nodes",all=T)
    colnames(df)[ncol(df)]<-"from"
    df<-merge(df,nodelist,by.x="node_name",by.y="Nodes",all=T)
    colnames(df)[ncol(df)]<-"to"
    #create edges
    edges<-as.data.frame(unique(na.omit(cbind(df$from,df$to,df$target_value))))
    colnames(edges)<-c("from","to","title")
    edges$label<-edges$title
    #create nodes
    nodes<-as.data.frame(unique(na.omit(cbind(df$to,df$node_name))))
    colnames(nodes)<-c("id","label")
    nodes$id<-as.numeric(as.character(nodes$id))
    nodes$label<-as.character(nodes$label)
    nodes$shape<-"dot"
    nodes$size<-15
    nodes$color<-"lightsteelblue"
    #find best path and highlight....logic is based on is primary
    path<-unique(df$node_name[df$is_on_best_level>0])
    nodes$shape<-ifelse(nodes$label %in% path,"star",nodes$shape)
    nodes$size<-ifelse(nodes$label %in% path,25,nodes$size)
    nodes$color<-ifelse(nodes$label %in% path,"red",nodes$color)
    #crete tree level
    nodes<-nodes[order(nodes$id),]
    edges<-edges[order(edges$from),]

    levlist<-list()
    levlist[[1]]<-cbind(1,1)
    cntr=2
    while (!(max(nodes$id) %in% unlist(levlist))){
      levlist[[cntr]]<-cbind(unique(edges$to[edges$from %in% levlist[[cntr-1]][,1]]),rep(cntr,length(unique(edges$to[edges$from %in% levlist[[cntr-1]][,1]]))))
      cntr=cntr+1
    }
    levdf <- ldply(levlist, data.frame)
    colnames(levdf)<-c("id","level")
    nodes<-merge(nodes,levdf,by="id")
    nodes$level.x<-NULL
    colnames(nodes)[ncol(nodes)]<-"level"
    #create graph
    visNetwork(nodes, edges,width="300%",height="600%") %>%
      visEdges(arrows = "to") %>%
      visNodes(font = list(size = 25)) %>%
      visOptions(highlightNearest = list(enabled = T, degree = 2, hover = T), nodesIdSelection = TRUE,clickToUse = T)%>%
      visInteraction(dragNodes = TRUE)%>%
      visPhysics(stabilization = T)%>%
      visInteraction(navigationButtons = TRUE)
  }, warning = function(war) {
  }, error = function(err) {
    nodes <- data.frame(id = 1,label="No Path Selected")
    edges <- data.frame(from = c(1), to = c(2))
    visNetwork(nodes, edges) %>% visNodes(shape = "square", borderWidth = 3,font = list(size = 50))
  }, finally = {
  }) # END tryCatch
  visgraph
}

#COMPRESSED TREE
comptree<-function(cristmas_tree_tables_part,cristmas_tree_tables_full,values,input,market_level_total,total_market){
  visgraph <- tryCatch({
    tree_name <- input$ushctab1_cell_clicked$value %>% read_html() %>% html_node('span') %>% html_text()
    tree_name<-trimws(strsplit(tree_name,"/")[[1]][1])
    market<-values$mkt
    if (input$show_whichtree=="Simplified Tree"){
      if (length(grep("TOP123",market))>0){
        df <- round_df(cristmas_tree_tables_part[[input$category]][[input$Brands]][[market_level_total]][[total_market]][[tree_name]]$ct_table,digits=2,min_threshold=0)
      }else{
        df <- round_df(cristmas_tree_tables_part[[input$category]][[input$Brands]][[input$Martyp]][[market]][[tree_name]]$ct_table,digits=2,min_threshold=0)
      }
    }else{
      if (length(grep("TOP123",market))>0){
        df <- round_df(cristmas_tree_tables_full[[input$category]][[input$Brands]][[market_level_total]][[total_market]][[tree_name]]$ct_table,digits=2,min_threshold=0)
      }else{
        df <- round_df(cristmas_tree_tables_full[[input$category]][[input$Brands]][[input$Martyp]][[market]][[tree_name]]$ct_table,digits=2,min_threshold=0)
      }
    }
    #get list of nodes
    nodelist<-unique(na.omit(c(as.character(df$parent_name),as.character(df$node_name))))
    nodelist<-as.data.frame(nodelist)
    colnames(nodelist)<-"Nodes"
    nodelist$id<-row.names(nodelist)
    nodelist$Nodes<-as.character(nodelist$Nodes)
    nodelist$id<-as.numeric(nodelist$id)
    #map the edges
    df$parent_name<-as.character(df$parent_name)
    df$node_name<-as.character(df$node_name)
    df<-merge(df,nodelist,by.x="parent_name",by.y="Nodes",all=T)
    colnames(df)[ncol(df)]<-"from"
    df<-merge(df,nodelist,by.x="node_name",by.y="Nodes",all=T)
    colnames(df)[ncol(df)]<-"to"
    #create edges
    edges<-as.data.frame(unique(na.omit(cbind(df$from,df$to,df$target_value))))
    colnames(edges)<-c("from","to","title")
    edges$label<-edges$title
    #create nodes
    nodes<-as.data.frame(unique(na.omit(cbind(df$to,df$node_name))))
    colnames(nodes)<-c("id","label")
    nodes$id<-as.numeric(as.character(nodes$id))
    nodes$label<-as.character(nodes$label)
    nodes$shape<-"dot"
    nodes$size<-15
    nodes$color<-"lightsteelblue"
    #best path
    path<-unique(df$node_name[df$is_on_best_level>0])
    nodes$shape<-ifelse(nodes$label %in% path,"star",nodes$shape)
    nodes$size<-ifelse(nodes$label %in% path,25,nodes$size)
    nodes$color<-ifelse(nodes$label %in% path,"red",nodes$color)
    #crete tree level
    nodes<-nodes[order(nodes$id),]
    edges<-edges[order(edges$from),]

    levlist<-list()
    levlist[[1]]<-cbind(1,1)
    cntr=2
    while (!(max(nodes$id) %in% unlist(levlist))){
      levlist[[cntr]]<-cbind(unique(edges$to[edges$from %in% levlist[[cntr-1]][,1]]),rep(cntr,length(unique(edges$to[edges$from %in% levlist[[cntr-1]][,1]]))))
      cntr=cntr+1
    }
    levdf <- ldply(levlist, data.frame)
    colnames(levdf)<-c("id","level")
    nodes<-merge(nodes,levdf,by="id")
    nodes$level.x<-NULL
    colnames(nodes)[ncol(nodes)]<-"level"
    #create graph
    visNetwork(nodes, edges,width="300%",height="600%") %>%
      visEdges(arrows = "to") %>%
      visNodes(font = list(size = 25)) %>%
      visOptions(highlightNearest = list(enabled = T, degree = 2, hover = T), nodesIdSelection = TRUE,clickToUse = T)%>%
      visInteraction(dragNodes = TRUE)%>%
      visPhysics(stabilization = T)%>%
      visInteraction(navigationButtons = TRUE)%>%
      visHierarchicalLayout(direction = "LR",edgeMinimization=FALSE, levelSeparation = 500)
  }, warning = function(war) {
  }, error = function(err) {
    nodes <- data.frame(id = 1,label="No Path Selected")
    edges <- data.frame(from = c(1), to = c(2))
    visNetwork(nodes, edges) %>% visNodes(shape = "square", borderWidth = 3,font = list(size = 50))
  }, finally = {

  }) # END tryCatch
  visgraph
}
#get full tree
fulltree<-function(cristmas_tree_tables_part,cristmas_tree_tables_full,values,input,market_level_total,total_market){
  visgraph <- tryCatch({
    viewlev<-as.numeric(input$treelevel)
    #get respective table: this logic only works for table names in format XXXX/AAAA and tries to find the AAAA table
    tree_name <- input$ushctab1_cell_clicked$value %>% read_html() %>% html_node('span') %>% html_text()
    tree_name<-trimws(strsplit(tree_name,"/")[[1]][1])
    market<-values$mkt
    if (input$show_whichtree=="Simplified Tree"){
      if (length(grep("TOP123",market))>0){
        df <- round_df(cristmas_tree_tables_part[[input$category]][[input$Brands]][[market_level_total]][[total_market]][[tree_name]]$ct_table,digits=2,min_threshold=0)
      }else{
        df <- round_df(cristmas_tree_tables_part[[input$category]][[input$Brands]][[input$Martyp]][[market]][[tree_name]]$ct_table,digits=2,min_threshold=0)
      }
    }else{
      if (length(grep("TOP123",market))>0){
        df <- round_df(cristmas_tree_tables_full[[input$category]][[input$Brands]][[market_level_total]][[total_market]][[tree_name]]$ct_table,digits=2,min_threshold=0)
      }else{
        df <- round_df(cristmas_tree_tables_full[[input$category]][[input$Brands]][[input$Martyp]][[market]][[tree_name]]$ct_table,digits=2,min_threshold=0)
      }
    }
    #get list of nodes
    nodelist<-cbind(as.numeric(df$node_id),as.character(df$node_name))
    nodelist<-as.data.frame(nodelist)
    colnames(nodelist)<-c("id","Nodes")
    nodelist$id<-as.numeric(as.character(nodelist$id))
    nodelist$Nodes<-as.character(nodelist$Nodes)
    #map the edges
    df$parent_name<-as.character(df$parent_name)
    df$node_name<-as.character(df$node_name)
    df<-merge(df,nodelist,by.x="parent_id",by.y="id",all=T)
    colnames(df)[ncol(df)]<-"from"
    df<-merge(df,nodelist,by.x="node_id",by.y="id",all=T)
    colnames(df)[ncol(df)]<-"to"
    #create edges
    edges<-as.data.frame(unique(na.omit(cbind(df$parent_id,df$node_id,df$is_on_best_level))))
    colnames(edges)<-c("from","to","bestedge")
    edges$color<-ifelse(edges$bestedge==1,"red","lightsteelblue")
    #create nodes
    nodes<-as.data.frame(na.omit(cbind(df$node_id,df$node_name,df$target_value,df$is_on_best_level)))
    colnames(nodes)<-c("id","label","title","bestlev")
    nodes$id<-as.numeric(as.character(nodes$id))
    nodes$label<-as.character(paste0(nodes$label,":",nodes$title))
    nodes$shape<-"dot"
    nodes$size<-15
    nodes$color<-"lightsteelblue"
    #find best path and highlight....logic is based on is primary
    nodes$shape<-ifelse(nodes$bestlev==1,"star",nodes$shape)
    nodes$size<-ifelse(nodes$bestlev==1,25,nodes$size)
    nodes$color<-ifelse(nodes$bestlev==1,"red",nodes$color)
    #crete tree level
    nodes<-nodes[order(nodes$id),]
    edges<-edges[order(edges$from),]

    levlist<-list()
    levlist[[1]]<-cbind(1,1)
    cntr=2
    while (!(max(nodes$id) %in% unlist(levlist))){
      levlist[[cntr]]<-cbind(unique(edges$to[edges$from %in% levlist[[cntr-1]][,1]]),rep(cntr,length(unique(edges$to[edges$from %in% levlist[[cntr-1]][,1]]))))
      cntr=cntr+1
    }
    levdf <- ldply(levlist, data.frame)
    colnames(levdf)<-c("id","level")
    nodes<-merge(nodes,levdf,by="id")
    nodes$level.x<-NULL
    colnames(nodes)[ncol(nodes)]<-"level"
    #remove levels not required
    innodes<-nodes$id[nodes$level<=viewlev]
    nodes<-nodes[nodes$level<=viewlev,]
    edges<-edges[edges$to %in% innodes,]

    #create graph
    visNetwork(nodes, edges,width="400%",height="400%") %>%
      visEdges(arrows = "to") %>%
      visNodes(font = list(size = 25)) %>%
      visOptions(highlightNearest = list(enabled = T, degree = 2, hover = T), nodesIdSelection = TRUE,clickToUse = T)%>%
      visInteraction(dragNodes = TRUE)%>%
      visPhysics(stabilization = T)%>%
      visInteraction(navigationButtons = TRUE)%>%
      visHierarchicalLayout(direction = "LR",edgeMinimization=FALSE, levelSeparation = 800)
  }, warning = function(war) {
  }, error = function(err) {
    nodes <- data.frame(id = 1,label="No Path Selected")
    edges <- data.frame(from = c(1), to = c(2))
    visNetwork(nodes, edges) %>% visNodes(shape = "square", borderWidth = 3,font = list(size = 50))
  }, finally = {
  }) # END tryCatch
  visgraph
}

#get competitor trend
getcomptrend<-function(list_markets,brand_competitors_time_series,zone_mapping_list,values,input,market_level_total,total_market,time_granularity){
  # browser()
  input_list <- list(
    metrictype = input$metrictype,
    sharebasetype = input$sharebasetype,
    sharebase = input$sharebase,
    prodhier = input$prodhier,
    prod = input$prod,
    Martyp = input$Martyp,
    market = values$mkt,
    currentperiod = input$currentperiod,
    refperiod = input$refperiod
  )
  if (!is.null(values$tottabtype)) {
    if (length(grep("TOP123", input_list$market)) > 0) {
      if (values$tottabtype == "tab1") {
        input_list$market <- values$mtp
      } else{
        input_list$Martyp <- market_level_total
        input_list$market <- total_market
      }
    }
  }
  df <- from_named_nested_list(brand_competitors_time_series, unlist(input_list))
  list_markets_df <- from_named_nested_list(list_markets, unlist(input_list))
  namesl <- list_markets_df$`Market/Product Attributes`
  if ('table_id' %in% colnames(list_markets_df)){
    table_ids <- list_markets_df$table_id
  }else{
    table_ids <- matrix(1,1,nrow(list_markets_df))
  }
  vislist<-list_markets_df[is.na(list_markets_df$cup_type) & ( list_markets_df$table_id == 1),]$`Market/Product Attributes`
  
  df<-as.data.frame(df)
  colnames(df)[2:ncol(df)]<-namesl
  if (time_granularity %in% c("MONTHLY", "BIMONTHLY")){
    formatstr <- "%b %Y"
  } else if (time_granularity=="WEEKLY"){
    formatstr <- "%d %b %Y"
  }

  df<-round_df_drv(df,2)
  chartdf <- melt(df, id.vars = c("Date"))
  hc <- highchart()
  dlev <- colnames(df)[2:ncol(df)]
  # stopifnot(length(dlev) == length(table_ids))
  for(i in 1:length(dlev))
  {
    if (dlev[i] %in% vislist){
      vis<-TRUE
    }else{
      vis<-FALSE
    }
    hc <- hc_add_series(hc, data = chartdf[chartdf$variable==dlev[i],]$value, name = dlev[i],type = "line",visible=vis)
  }
  if (as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][5])>0){
    hc<-hc_xAxis(hc,categories = format(df$Date, format=formatstr),plotBands = list(
      list(label = list(text = "Current Period",rotation= 90,textAlign= 'left'),
           color = "#9DC3E3",
           from = length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][2]),
           to = length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][1])),
      list(label = list(text = "Reference Period",rotation= 90,textAlign= 'left'),
           color = "#9DC3E3",
           width = 2,
           from = (length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][4])),
           to=(length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][3])))))
  }else{
    hc<-hc_xAxis(hc,categories = format(df$Date, format=formatstr),plotLines = list(
      list(label = list(text = "Current Period",rotation= 90,textAlign= 'left'),
           color = "#9DC3E3",
           width=2,
           value = length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][2])),
      list(label = list(text = "Reference Period",rotation= 90,textAlign= 'left'),
           color = "#9DC3E3",
           width = 2,
           value = (length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][4])))))
  }
  hc <- hc_yAxis(hc,title=list(text = input$metrictype),max=max(chartdf$value))
  hc <- hc_chart(hc,zoomType='x',panKey="shift",panning=TRUE)
  hc <- hc_legend(hc,
                  enabled = TRUE,
                  align = 'right',
                  verticalAlign = 'top',
                  layout = 'horizontal',
                  floating = FALSE)
  hc <- hc_tooltip(hc,borderWidth=0, followPointer=TRUE, followTouchMove=TRUE, shared = FALSE,
                   pointFormat = "y: {point.y}")
  hc <- hc_exporting(hc,enabled=TRUE,
                     url = "https://export.highcharts.com",
                     fallbackToExportServer=FALSE,
                     buttons=list(
                       contextButton = list(
                         align = 'left',
                         verticalAlign = 'top'
                       )
                     )
  )
  hc <- hc_plotOptions(hc,column = list(stacking = "normal", pointPadding = 0, groupPadding = 0, borderWidth = 0))
  column(width = 12, hc)
  #hc %>% hc_add_theme(hc_theme_sandsignika())
  hc
}



#get tree levels
getlev<-function(df){
  levlist <- tryCatch({
    #get list of nodes
    nodelist<-cbind(as.numeric(df$node_id),as.character(df$node_name))
    nodelist<-as.data.frame(nodelist)
    colnames(nodelist)<-c("id","Nodes")
    nodelist$id<-as.numeric(as.character(nodelist$id))
    nodelist$Nodes<-as.character(nodelist$Nodes)
    #map the edges
    df$parent_name<-as.character(df$parent_name)
    df$node_name<-as.character(df$node_name)
    df<-merge(df,nodelist,by.x="parent_id",by.y="id",all=T)
    colnames(df)[ncol(df)]<-"from"
    df<-merge(df,nodelist,by.x="node_id",by.y="id",all=T)
    colnames(df)[ncol(df)]<-"to"
    #create edges
    edges<-as.data.frame(unique(na.omit(cbind(df$parent_id,df$node_id,df$is_on_best_level))))
    colnames(edges)<-c("from","to","bestedge")
    edges$color<-ifelse(edges$bestedge==1,"red","lightsteelblue")
    #create nodes
    nodes<-as.data.frame(na.omit(cbind(df$node_id,df$node_name,df$target_value,df$is_on_best_level)))
    colnames(nodes)<-c("id","label","title","bestlev")
    nodes$id<-as.numeric(as.character(nodes$id))
    nodes$label<-as.character(paste0(nodes$label,":",nodes$title))
    nodes$shape<-"dot"
    nodes$size<-15
    nodes$color<-"lightsteelblue"
    #find best path and highlight....logic is based on is primary
    nodes$shape<-ifelse(nodes$bestlev==1,"star",nodes$shape)
    nodes$size<-ifelse(nodes$bestlev==1,25,nodes$size)
    nodes$color<-ifelse(nodes$bestlev==1,"red",nodes$color)
    #crete tree level
    nodes<-nodes[order(nodes$id),]
    edges<-edges[order(edges$from),]

    levlist<-list()
    levlist[[1]]<-cbind(1,1)
    cntr=2
    while (!(max(nodes$id) %in% unlist(levlist))){
      levlist[[cntr]]<-cbind(unique(edges$to[edges$from %in% levlist[[cntr-1]][,1]]),rep(cntr,length(unique(edges$to[edges$from %in% levlist[[cntr-1]][,1]]))))
      cntr=cntr+1
    }
    levdf <- ldply(levlist, data.frame)
    colnames(levdf)<-c("id","level")
    seq(1,max(levdf$level),1)

  }, warning = function(war) {

  }, error = function(err) {
    seq(0,0,0)
  }, finally = {

  }) # END tryCatch
}

#driver and share trends
gethc_allinone <- function(clist,llist,list_markets,values,input,market_level_total,total_market,drivers_time_series,time_granularity,hoverover_df,zone_mapping_list) {
  #get the chart title
  tree_name=strsplit(values$tree_name,";")[[1]][1]
  market<-values$mkt
  if (!is.null(values$tottabtype)){
    if (length(grep("TOP123",market))>0){
      market<-gsub("TOP123","",values$mkt)
      if (values$tottabtype=="tab1"){
        table_1<-list_markets[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[values$mtp]][[input$currentperiod]][[input$refperiod]]
      }else{
        table_1<-list_markets[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[market_level_total]][[total_market]][[input$currentperiod]][[input$refperiod]]
      }
    }else{
      idx<-which(values$mkt==names(list_markets[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]]))
      table_1 <- list_markets[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[idx]][[input$currentperiod]][[input$refperiod]]
    }
  }else{
    idx<-which(values$mkt==names(list_markets[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]]))
    table_1 <- list_markets[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[idx]][[input$currentperiod]][[input$refperiod]]
  }
  rowval<-table_1$`Market/Product Attributes`[as.numeric(tree_name)]
  drvi<-which(colnames(table_1) %in% as.character(hoverover_df$driver_short_names),arr.ind = T)+1
  drvn<-colnames(table_1)[which(colnames(table_1) %in% as.character(hoverover_df$driver_short_names),arr.ind = T)]
  table_1<-table_1[,drvi]
  colnames(table_1)<-drvn
  rowfull<-table_1[as.numeric(tree_name),]
  drvval<-rowfull

  drvval<-t(drvval)
  drvval<-as.data.frame(drvval)
  drvval$drvs<-rownames(drvval)
  test<<-drvval
  drvval<-drvval[drvval[,1]>102 | drvval[,1]<98,]
  showdrv<-drvval$drvs
  title_text = paste0("Share and Driver Trends for ", rowval," at ",values$mkt)
  # function to return the chart in a column div
  idx <- as.numeric(values$tree_name)
  market<-values$mkt
  if (length(grep("TOP123",market))>0){
    df<-drivers_time_series[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[market_level_total]][[total_market]][[input$currentperiod]][[input$refperiod]][[idx]]
  }else{
    df<-drivers_time_series[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[market]][[input$currentperiod]][[input$refperiod]][[idx]]
  }
  df<-round_df_drv(df,2)
  counter=1
  chartlist<-list()
  ylablist<-list()
  ylabcount=1
  hoverover_df$driver_short_names<-as.character((hoverover_df$driver_short_names))
  hoverover_df$driver_hoverover_labels<-as.character((hoverover_df$driver_hoverover_labels))
  hoverover_df$driver_type<-as.character((hoverover_df$driver_type))
  hoverover_df$driver_scale<-as.character((hoverover_df$driver_scale))
  clist<-unique(hoverover_df[,3:4])
  llist<-clist$driver_scale
  clist<-clist$driver_type
  for (i in 1:(length(clist)+1)){
    if (i==1){
      cols<-c(1,2)
      if (length(cols)>1){
        chartlist[[counter]]<-data.frame(df[,cols])
        colnames(chartlist[[counter]])<-colnames(df)[cols]
        counter=counter+1
        ylablist[[ylabcount]]<-"Value"
        ylabcount<-ylabcount+1
      }
    }else{
      cols<-c(1,which(colnames(df) %in% hoverover_df$driver_short_names[hoverover_df$driver_type==clist[i-1]]))
      if (length(cols)>1){
        chartlist[[counter]]<-data.frame(df[,cols])
        colnames(chartlist[[counter]])<-colnames(df)[cols]
        counter=counter+1
        ylablist[[ylabcount]]<-llist[i-1]
        ylabcount<-ylabcount+1
      }
    }
  }
  llist<-unlist(ylablist)
  hc <- highchart(width="100%",height=1200)
  #hc<-hc_title(hc,text=title_text)
  for (i in 1:length(chartlist)){
    cdf<-chartlist[[i]]
    chartdf <- melt(cdf, id.vars = c("Date"))
    for (j in 2:ncol(cdf)){
      if ((colnames(cdf)[j] %in% showdrv) | i==1){
        visl=TRUE
      }else{
        visl=FALSE
      }
      hc<-hc_add_series(hc,data=chartdf[chartdf$variable==colnames(cdf)[j],]$value,yAxis=i-1,name = colnames(cdf)[j],type = "line",visible=visl)
    }
  }

  #create the multiple y axis
  pp<-"Reference Period:"
  yaxis<-create_yaxis(naxis = length(chartlist),heights = rep(1,length(chartlist)))
  ylabels=llist
  yaxislabside=rep(FALSE,length(chartlist))
  newaxis <- purrr::pmap(list(yaxis, ylabels,yaxislabside), function(x, y, z){ x$title <- list(text = y); x$opposite=z; return(x);})
  hc <- hc_yAxis_multiples(hc,newaxis)
  hc$x$hc_opts$yAxis<-hc$x$hc_opts$yAxis[[1]]
  if (time_granularity=="WEEKLY"){
    formatstr="%d %b %Y"
  }else{
    formatstr="%b %Y"
  }
  if (as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][5])>0){
    hc<-hc_xAxis(hc,categories = format(df$Date, format=formatstr),plotBands = list(
      list(label = list(text = "Current Period",rotation= 90,textAlign= 'left'),
           color = "#9DC3E3",
           from = length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][2]),
           to = length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][1])),
      list(label = list(text = pp,rotation= 90,textAlign= 'left'),
           color = "#9DC3E3",
           width = 2,
           from = (length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][4])),
           to=(length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][3])))))
  }else{
    hc<-hc_xAxis(hc,categories = format(df$Date, format=formatstr),plotLines = list(
      list(label = list(text = "Current Period",rotation= 90,textAlign= 'left'),
           color = "#9DC3E3",
           width=2,
           value = length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][2])),
      list(label = list(text = pp,rotation= 90,textAlign= 'left'),
           color = "#9DC3E3",
           width = 2,
           value = (length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][4])))))
  }

  hc<-hc_chart(hc,zoomType = "xy",panKey="shift",panning=TRUE)
  hc<-  hc_legend(hc,enabled = TRUE,align = 'right',
                  verticalAlign = 'top',
                  layout = 'vertical',x=0,y=100,
                  floating = FALSE)
  hc <- hc_tooltip(hc,borderWidth=0, followPointer=TRUE, followTouchMove=TRUE, shared = FALSE,
                   pointFormat = "y: {point.y}")
  hc<-hc_scrollbar(hc)
  hc <- hc_plotOptions(hc,column = list(stacking = "normal", pointPadding = 0, groupPadding = 0, borderWidth = 0))
  column(width = 12, hc)#<-hc %>% hc_add_theme(hc_theme_sandsignika()))
  hc
}

gethc_onebyone <- function(clist,llist,list_markets,values,input,market_level_total,total_market,drivers_time_series,time_granularity,hoverover_df,zone_mapping_list,height,width,ratings=NULL) {
  #get the chart title
  tree_name=strsplit(values$tree_name,";")[[1]][1]
  market<-values$mkt
  if (!is.null(values$tottabtype)){
    if (length(grep("TOP123",market))>0){
      market<-gsub("TOP123","",values$mkt)
      if (values$tottabtype=="tab1"){
        table_1<-list_markets[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[values$mtp]][[input$currentperiod]][[input$refperiod]]
      }else{
        table_1<-list_markets[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[market_level_total]][[total_market]][[input$currentperiod]][[input$refperiod]]
      }
    }else{
      idx<-which(values$mkt==names(list_markets[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]]))
      table_1 <- list_markets[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[idx]][[input$currentperiod]][[input$refperiod]]
    }
  }else{
    idx<-which(values$mkt==names(list_markets[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]]))
    table_1 <- list_markets[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[idx]][[input$currentperiod]][[input$refperiod]]
  }
  metric<-colnames(table_1)[2]
  rowval<-table_1$`Market/Product Attributes`[as.numeric(tree_name)]
  drvi<-which(colnames(table_1) %in% as.character(hoverover_df$driver_short_names),arr.ind = T)+1
  drvn<-colnames(table_1)[which(colnames(table_1) %in% as.character(hoverover_df$driver_short_names),arr.ind = T)]
  table_1<-table_1[,drvi]
  colnames(table_1)<-drvn
  rowfull<-table_1[as.numeric(tree_name),]
  drvval<-rowfull
  
  drvval<-t(drvval)
  drvval<-as.data.frame(drvval)
  drvval$drvs<-rownames(drvval)
  test<<-drvval
  drvval<-drvval[drvval[,1]>102 | drvval[,1]<98,]
  showdrv<-drvval$drvs
  title_text = paste0("Share and Driver Trends for ", rowval," at ",values$mkt)
  # function to return the chart in a column div
  idx <- as.numeric(values$tree_name)
  market<-values$mkt
  if (length(grep("TOP123",market))>0){
    df<-drivers_time_series[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[market_level_total]][[total_market]][[input$currentperiod]][[input$refperiod]][[idx]]
  }else{
    df<-drivers_time_series[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[market]][[input$currentperiod]][[input$refperiod]][[idx]]
  }
  df<-round_df_drv(df,2)
  counter=1
  chartlist<-list()
  ylablist<-list()
  charts<-list()
  ylabcount=1
  hoverover_df$driver_short_names<-as.character((hoverover_df$driver_short_names))
  hoverover_df$driver_hoverover_labels<-as.character((hoverover_df$driver_hoverover_labels))
  hoverover_df$driver_type<-as.character((hoverover_df$driver_type))
  hoverover_df$driver_scale<-as.character((hoverover_df$driver_scale))
  clist<-unique(hoverover_df[,3:4])
  llist<-clist$driver_scale
  clist<-clist$driver_type
  for (i in 1:(length(clist)+1)){
    if (i==1){
      cols<-c(1,grep(metric,colnames(df),ignore.case = T))
      if (length(cols)>1){
        chartlist[[counter]]<-data.frame(df[,cols])
        colnames(chartlist[[counter]])<-colnames(df)[cols]
        counter=counter+1
        ylablist[[ylabcount]]<-"Value"
        ylabcount<-ylabcount+1
      }
    }else{
      cols<-c(1,which(colnames(df) %in% hoverover_df$driver_short_names[hoverover_df$driver_type==clist[i-1]]))
      if (length(cols)>1){
        chartlist[[counter]]<-data.frame(df[,cols])
        colnames(chartlist[[counter]])<-colnames(df)[cols]
        counter=counter+1
        ylablist[[ylabcount]]<-llist[i-1]
        ylabcount<-ylabcount+1
      }
    }
  }
  llist<-unlist(ylablist)
  if (!is.null(ratings)){
    if (nrow(ratings)>0){
      ncharts<-length(chartlist)+1
    }else{
      ncharts<-length(chartlist)
    }
  }else{
    ncharts<-length(chartlist)
  }
  #hc<-hc_title(hc,text=title_text)
  for (i in 1:length(chartlist)){
    cdf<-chartlist[[i]]
    chartdf <- melt(cdf, id.vars = c("Date"))
    if (i %% 2==0){
      bkcolor="#ffffff"
    }else{
      bkcolor="#ffffff"
    }
    if (i<ncharts){
      hc <- highchart(width=0.9*width,height=0.75*height/ncharts)
      hc <- hc_chart(hc,marginLeft=70,marginRight=240,marginBottom=15,backgroundColor=bkcolor)
    }else{
      hc <- highchart(width=0.9*width,height=0.75*height/ncharts+60)
      hc <- hc_chart(hc,marginLeft=70,marginRight=240,marginBottom=50,backgroundColor=bkcolor)
    }
    for (j in 2:ncol(cdf)){
      if ((colnames(cdf)[j] %in% showdrv) | i==1){
        visl=TRUE
      }else{
        visl=FALSE
      }
      hc<-hc_add_series(hc,data=chartdf[chartdf$variable==colnames(cdf)[j],]$value,yAxis=0,name = colnames(cdf)[j],type = "line",visible=visl)
    }
    if (i==1){
      cp<-"Current Period"
      pp<-"Reference Period"
    }else{
      cp<-""
      pp<-""
    }
    
    hc$x$hc_opts$yAxis<-hc$x$hc_opts$yAxis[[1]]
    if (time_granularity=="WEEKLY"){
      formatstr="%d%b%y"
    }else{
      formatstr="%b%y"
    }
    lenx=paste(rep(" ",nchar(format(df$Date, format=formatstr)[1])),collapse="")
    if (i<ncharts){
      if (as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][5])>0){
        hc<-hc_xAxis(hc,categories = rep(lenx,length(df$Date)),tickInterval=1,labels=list(enabled=FALSE),plotBands = list(
          list(label = list(text = cp,rotation= 90,textAlign= 'left'),
               color = "#9DC3E3",
               from = length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][2]),
               to = length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][1])),
          list(label = list(text = pp,rotation= 90,textAlign= 'left'),
               color = "#9DC3E3",
               width = 2,
               from = (length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][4])),
               to=(length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][3])))))
      }else{
        hc<-hc_xAxis(hc,categories = rep(lenx,length(df$Date)),tickInterval=1,labels=list(enabled=FALSE),plotLines = list(
          list(label = list(text = cp,rotation= 90,textAlign= 'left'),
               color = "#9DC3E3",
               width=2,
               value = length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][2])),
          list(label = list(text = pp,rotation= 90,textAlign= 'left'),
               color = "#9DC3E3",
               width = 2,
               value = (length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][4])))))
      }
    }else{
      if (as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][5])>0){
        hc<-hc_xAxis(hc,categories = format(df$Date, format=formatstr),tickInterval=1,labels=list(align='left',y=40),plotBands = list(
          list(label = list(text = cp,rotation= 90,textAlign= 'left'),
               color = "#9DC3E3",
               from = length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][2]),
               to = length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][1])),
          list(label = list(text = pp,rotation= 90,textAlign= 'left'),
               color = "#9DC3E3",
               width = 2,
               from = (length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][4])),
               to=(length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][3])))))
      }else{
        hc<-hc_xAxis(hc,categories = format(df$Date, format=formatstr),tickInterval=1,labels=list(align='left',y=40),plotLines = list(
          list(label = list(text =cp,rotation= 90,textAlign= 'left'),
               color = "#9DC3E3",
               width=2,
               value = length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][2])),
          list(label = list(text = pp,rotation= 90,textAlign= 'left'),
               color = "#9DC3E3",
               width = 2,
               value = (length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][4])))))
      }
    }
    
    
    hc<-hc_chart(hc,zoomType = "xy",panKey="shift",panning=TRUE)
    hc<-  hc_legend(hc,enabled = TRUE,align = 'left',width=160,padding=0,
                    verticalAlign = 'right',
                    layout = 'vertical',x=0.8*width,y=0,floating=TRUE)
    hc <- hc_tooltip(hc,borderWidth=0, followPointer=TRUE, followTouchMove=TRUE, shared = T,table=T,
                     headerFormat= '<small>{point.key}</small><table>',
                     pointFormat = "<br>{series.name}: <b>{point.y}</b>")
    hc <-hc_scrollbar(hc)
    hc <- hc_plotOptions(hc,column = list(stacking = "normal", pointPadding = 0, groupPadding = 0, borderWidth = 0))
    charts[[i]]<-column(width = 12, hc)
  }
  if (!is.null(ratings) && nrow(ratings) > 0) {
    dfr <- ratings
    # df[,2]<-NULL
    # rnr_tbls<-ratings
    # eval(parse(text=paste0("dfr<-rnr_tbls[rnr_tbls$",input$prodhier,"==input$prod,c(1,6,7,8,9,10,11,14)]")))
    # 
    # dfr<-merge(df,dfr,by.x="Date",by.y="DATE",all.x=T,all.y=F)
    # dfr<-subset(dfr,select= -c(which(colnames(dfr) %in% hoverover_df$driver_short_names)))
    # dfr<-dfr[!is.na(dfr$Date),]
    # colnames(dfr)[2:7]<-paste0("Rating_",colnames(dfr)[2:7])
    # dfr=dfr %>% 
    #   group_by(Date) %>% 
    #   summarise(Rating_0 = sum(Rating_0,na.rm=T) , Rating_1 = sum(Rating_1,na.rm=T),Rating_2 = sum(Rating_2,na.rm=T),Rating_3 = sum(Rating_3,na.rm=T),Rating_4 = sum(Rating_4,na.rm=T),Rating_5 = sum(Rating_5,na.rm=T),AVERAGE_RATING=mean(AVERAGE_RATING,na.rm=T))
    # dfr[is.na(dfr)]<-0
    math_dates <- match(format(df$Date,"%m-%Y"), format(dfr$Date,"%m-%Y"))
    dfr$Date[na.omit(math_dates)] <- df$Date[!is.na(math_dates)]
    extra_dates <- df$Date %>% union(dfr$Date) %>% setdiff(df$Date) %>% as.Date(origin = "1970-01-01") 
    xax <- df$Date
    dfr <- merge(dfr[!dfr$Date %in% extra_dates, ], data.frame(Date = xax), all.y = TRUE)
    
    dfr$Date<-NULL
    lineser<-dfr[,ncol(dfr)]
    dfr[,ncol(dfr)]<-NULL
    dfr <- gather(dfr)
    dfr <- dfr %>%
      # we change the key to name to have the label in the legend
      group_by(name = key) %>%  
      # the data in this case is simple, is just .$value column
      do(data = .$value) 
    series <- list_parse(dfr)
    lineser<-as.matrix(lineser)
    if (as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][5])>0){
      hc <- highchart(width=0.90*width,height=0.75*height/ncharts+60)%>% 
        hc_chart(marginLeft=70,marginRight=240,marginBottom=50,backgroundColor=bkcolor,zoomType = "xy",panKey="shift",panning=TRUE)%>% 
        hc_chart(type = "column") %>%
        hc_yAxis_multiples(
          list(lineWidth = 3,title=list(text="Ratings")),
          list(showLastLabel = FALSE,opposite=TRUE,title=list(text="Avg Rating"),labels=list(x=-5))
        ) %>% 
        hc_xAxis(categories = format(df$Date, format=formatstr),tickInterval=1,labels=list(align='left',y=40),plotBands = list(
          list(label = list(text = cp,rotation= 90,textAlign= 'left'),
               color = "#9DC3E3",
               from = length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][2]),
               to = length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][1])),
          list(label = list(text = pp,rotation= 90,textAlign= 'left'),
               color = "#9DC3E3",
               width = 2,
               from = (length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][4])),
               to=(length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][3])))))%>%
        # hc_xAxis(categories = format(df$Date, format=formatstr),tickInterval=2,labels=list(align='left'))%>%
        hc_plotOptions(column = list(
          dataLabels = list(enabled = F),
          stacking = "normal",
          enableMouseTracking = T)
        ) %>% 
        hc_add_series_list(series) %>%
        hc_add_series(lineser, type = "line",name = "Avg Rating", yAxis = 1,
                      tooltip = list(pointFormat = "Avg Rating : {point.y:.2f}"), shared = FALSE) %>%
        hc_legend(enabled = TRUE,align = 'left',
                  verticalAlign = 'right',
                  layout = 'vertical',x=0.8*width,y=0,floating=TRUE) %>%
        hc_tooltip(shared = T,table=T)#pointFormat = "Selected # Reviews : {point.y}<br>Total # Reviews : {point.total}")
    }else{
      hc <- highchart(width=0.90*width,height=0.75*height/ncharts+60)%>% 
        hc_chart(marginLeft=70,marginRight=240,marginBottom=50,backgroundColor=bkcolor,zoomType = "xy",panKey="shift",panning=TRUE)%>% 
        hc_chart(type = "column") %>%
        hc_yAxis_multiples(
          list(lineWidth = 3,title=list(text="Ratings")),
          list(showLastLabel = FALSE, opposite = TRUE, title=list(text="Avg Rating"),labels=list(x=-5))
        ) %>% 
        hc_xAxis(categories = format(df$Date, format=formatstr),tickInterval=1,labels=list(align='left',y=40),plotLines = list(
          list(label = list(text =cp,rotation= 90,textAlign= 'left'),
               color = "#9DC3E3",
               width=2,
               value = length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][2])),
          list(label = list(text = pp,rotation= 90,textAlign= 'left'),
               color = "#9DC3E3",
               width = 2,
               value = (length(df$Date)-as.numeric(zone_mapping_list[[input$currentperiod]][[input$refperiod]][4])))))%>%
        # hc_xAxis(categories = format(df$Date, format=formatstr),tickInterval=1,labels=list(align='left'))%>%
        hc_plotOptions(column = list(
          dataLabels = list(enabled = F),
          stacking = "normal",
          enableMouseTracking = T)
        ) %>% 
        hc_add_series_list(series) %>%
        hc_add_series(lineser, type = "line",name = "Avg Rating", yAxis = 1,
                      tooltip = list(pointFormat = "Avg Rating : {point.y:.2f}"), shared = FALSE) %>%
        hc_legend(enabled = TRUE,align = 'left',
                  verticalAlign = 'right',
                  layout = 'vertical',x=0.8*width,y=0,floating=TRUE) %>%
        hc_tooltip(shared = T,table=T)
    }
    
    charts[[length(charts)+1]]<-column(width = 12, hc)
    
  }
  
  return(charts)
}
#getnvd3chart for driver and share trends
getnvd3 <- function(clist,llist,list_markets,values,input,market_level_total,total_market,drivers_time_series,period_lag) {
  #get the chart title
  tree_name=strsplit(values$tree_name,";")[[1]][1]
  if (values$mkt=="TOTAL"){
    table_1<-list_markets[[input$category]][[input$Brands]][[market_level_total]][[total_market]]
  }else{
    idx<-which(values$mkt==names(list_markets[[input$category]][[input$Brands]][[input$Martyp]]))
    table_1 <- list_markets[[input$category]][[input$Brands]][[input$Martyp]][[idx]]
  }
  rowval<-table_1$`Market/Product Attributes`[as.numeric(tree_name)]
  rowfull<-table_1[as.numeric(tree_name),6:(ncol(table_1)-6)]
  idcolso<-seq(1,ncol(rowfull),2)
  idcolse<-seq(2,ncol(rowfull),2)
  drvval<-rowfull[1,idcolse]
  colnames(drvval)<-colnames(rowfull)[idcolso]
  drvval<-t(drvval)
  drvval<-drvval[drvval[,1]>102 | drvval[,1]<98,]
  showdrv<-names(drvval)
  title_text = paste0("Share and Driver Trends for ", rowval," at ",values$mkt)
  # function to return the chart in a column div
  idx <- as.numeric(values$tree_name)
  market<-values$mkt
  if (length(grep("TOP123",market))>0){
    df<-drivers_time_series[[input$category]][[input$Brands]][[market_level_total]][[total_market]][[idx]]
  }else{
    df<-drivers_time_series[[input$category]][[input$Brands]][[input$Martyp]][[market]][[idx]]
  }
  df<-round_df_drv(df,2)
  counter=1
  chartlist<-list()
  for (i in 1:length(clist)){
    cols<-c(1,grep(clist[i],colnames(df),ignore.case=TRUE,fixed=F))
    if (length(cols)>1){
      chartlist[[counter]]<-data.frame(df[,cols])
      colnames(chartlist[[counter]])<-colnames(df)[cols]
      counter=counter+1
    }
  }

  chart_out_list<-lapply(1:length(chartlist),function(i){

    cdf<-chartlist[[i]]
    chartdf <- melt(cdf, id.vars = c("Date"))
    nPlot(value ~ Date, data = chartdf , group="variable", type = 'lineChart')
    renderChart2({
      plot<-nPlot(value ~ Date, data = chartdf , group="variable", type = 'lineChart')

      plot$yAxis(axisLabel = "Population", width = 62)
      plot$xAxis(axisLabel = "Year")
      plot$set(width=1600, height=800)

      plot$save("ac.html")
      plot
    })
  })
  # nvd3list<-list()
  # for (i in 1:length(chartlist)){
  #   cdf<-chartlist[[i]]
  #   chartdf <- melt(cdf, id.vars = c("Date"))
  #   for (j in 2:ncol(cdf)){
  #     if (colnames(cdf)[j] %in% showdrv){
  #       visl=TRUE
  #     }else{
  #       visl=FALSE
  #     }
  #     #eval(parse(text=paste0("chartdf",i,"<<-chartdf")))
  #     nvd3list[[i]] <-rCharts::renderChart2({
  #       nPlot(value ~ Date, data = chartdf , group="variable", type = 'lineChart')
  #     })
  #   }
  # }
  chart_out_list

}

# #generate plots in loop
# # Call renderPlot for each one. Plots are only actually generated when they
# # are visible on the web page.
# for (i in 1:length(lcount)) {
#   # Need local so that each item gets its own number. Without it, the value
#   # of i in the renderPlot() will be the same across all instances, because
#   # of when the expression is evaluated.
#   local({
#     my_i <- i
#     plotname <- paste0("Plot", my_i)
#     eval(parse(text=paste0("chartdf<-chartdf",my_i)))
#     output[[plotname]] <- renderChart2({
#       nPlot(value ~ Date, data = chartdf , group="variable", type = 'lineChart')
#     })
#   })
# }



getlevlist<-function(cristmas_tree_tables_full,input,values){
  #get respective table: this logic only works for table names in format XXXX/AAAA and tries to find the AAAA table
  tree_name <- input$ushctab1_cell_clicked$value %>% read_html() %>% html_node('span') %>% html_text()
  tree_name<-trimws(strsplit(tree_name,"/")[[1]][1])
  market<-values$mkt
  df <- cristmas_tree_tables_full[[input$category]][[input$Brands]][[input$Martyp]][[market]][[tree_name]]$ct_table
  levlist<-getlev(df)
  selectInput("treelevel","Choose Level",choices=levlist,multiple=FALSE,selected=max(levlist))
}
#get drv share title
getdrvsharetitle<-function(list_markets,input,values, market_level_total, total_market){
  tree_name=strsplit(values$tree_name,";")[[1]][1]
  market<-values$mkt
  if (length(grep("TOP123",market))>0){
    market<-gsub(paste0("/ ",input$sharebase),"",gsub("TOP123","",market),fixed=T)
    table_1<-list_markets[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[market_level_total]][[total_market]][[input$currentperiod]][[input$refperiod]]
  }else{
    idx<-which(values$mkt==names(list_markets[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]]))
    table_1 <- list_markets[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[idx]][[input$currentperiod]][[input$refperiod]]
  }
  rowval<-table_1$`Market/Product Attributes`[as.numeric(tree_name)]
  if (nchar(rowval)>20){
    rowval<-paste0(rowval,"<br>")
  }
  title_text = title = HTML(paste0("<h3 id = 'comp_modal' style = 'color: green; margin-right: 120px;'>Share and Driver <span style = 'color: #3182bd;'><strong>", 
                                   paste0("</strong></span> Trends for <span style = 'color: #3182bd;'><strong>", rowval, "</span></strong> in <span style = 'color: #3182bd;'><strong>", market), "</strong></span> within <span style = 'color: #3182bd;'><strong>",input$sharebase,"</strong></span></h3>",
                                   "<button type=\"button\" class=\"btn btn-default round-button\"data-dismiss=\"modal\" style = \"float: right; margin-top: -60px;\">Close</button></h3>"))
  title_text
}
