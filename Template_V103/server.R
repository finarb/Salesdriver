#start shiny server functiona
shinyServer(function(input, output,session) {
  #load codes
  shinyjs::hide("onload")
  #create datatable proxies
  proxy1 = dataTableProxy('ushctab1')
  proxy1a = dataTableProxy('ushctab2')
  proxy2 = dataTableProxy('bmwin')
  proxy3 = dataTableProxy('bmloss')
  proxy4 = dataTableProxy('totalmarkdt')
  error(logger, 'An Error Message')
  #get reactive values
  values <- reactiveValues(url_not_used = TRUE,appload=TRUE, mt = names(top_tables))
  values$drvs<-hoverover_df$driver_short_names
  values$ddchanged=TRUE
  starttime<-Sys.time()
  shinyjs::hide("divtab1summ")
  
  if (market_to_path==T){
    values$mtpf=T
    values$mtp="MARKET_TO_PATH"
  }else{
    values$mtpf=F
  }
  #########################################################################
  ###############update top dropdown menu##################################
  #########################################################################
  output$Notifications1 <- renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    msgs <- ""
    status<-""
    
    notificationData<-data.frame(message=msgs,status="success")
    nots <- apply(notificationData,1,function(row){
      notificationItem(text=row[["message"]], status=row[["status"]],icon=shiny::icon("navicon",lib="font-awesome"))
    })
    dropmsg<-"Tips for better viewing within Samanta<br><ul><li>Always use Chrome.</li><li>Press F11 to go full screen on your browser.</li><li>You can zoom in and out by holding the ctrl key and turning the mouse wheel.  Alternatively press ctrl + or ctrl – to zoom</li><li>Click on the three bars at the top to close the alert menu</li></ul>"
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenuCustom(type = "notifications", .list = nots,dropmsg=dropmsg,dropheader="General Information",icon=shiny::icon("question"))
  })
  output$Notifications2 <- renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    
    filelink<-row.names(file.info(list.files(path = "./www/", pattern = "xlsx")))                                                   
    
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    if (length(filelink)!=0){
      msgs <- "Please Download File for Definitions"
      status<-"success"
      notificationData<-data.frame(message=msgs,status="success",link=filelink)
      nots <- apply(notificationData,1,function(row){
        notificationItem(text=row[["message"]], status=row[["status"]],href=row[["link"]],icon=shiny::icon("download",lib="font-awesome"))
      })
      dropdownMenuCustom(type = "notifications", .list = nots,dropheader="Download Helper Files",icon=shiny::icon("download"))
    }
    
  })
  output$Messages1 <- renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    msgs <- ""
    maillink<-"mailto:hay.jd@pg.com"
    
    messagesData<-data.frame(message=msgs,status="success",link=maillink)
    messages <- apply(messagesData,1,function(row){
      messageItem(from="SEND TO APP OWNER",message=row[["message"]],href=row[["link"]],icon=shiny::icon("envelope",lib="font-awesome"))
    })
    dropmsg<-"<ul><li>Questions</li><li>Bugs</li><li>Suggestions</li><li>Changes to the pull-down choice</li><li>To add or remove product or market data</li><li>To create a new custom product attribute</li></ul>"
    
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenuCustom(type = "messages", .list = messages,dropmsg=dropmsg,dropheader="Send Feedback",icon=shiny::icon("envelope"))
  })
  
  #########################################################################
  ###############update select inputs######################################
  #########################################################################
  # updateSelectizeInput(session,"metrictype",choices=names(top_tables))
  # updateSelectizeInput(session,"sharebasetype",choices=names(top_tables[[1]]))
  # updateSelectizeInput(session,"sharebase",choices=names(top_tables[[1]][[1]]))
  # updateSelectizeInput(session,"prodhier",choices=names(top_tables[[1]][[1]][[1]]))
  # updateSelectizeInput(session,"prod",choices=names(top_tables[[1]][[1]][[1]][[1]]))
  # updateSelectizeInput(session,"Martyp",choices=setdiff(names(top_tables[[1]][[1]][[1]][[1]][[1]]),market_level_total))
  # updateSelectizeInput(session,"currentperiod",choices=names(top_tables[[1]][[1]][[1]][[1]][[1]][[1]]))
  # updateSelectizeInput(session,"refperiod",choices=names(top_tables[[1]][[1]][[1]][[1]][[1]][[1]][[1]]))
  #########################################################################
  ######################server finctions###################################
  #########################################################################
  
  ###### Start with collapsed sidebar panel #######
  shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  
  ##### date update fields and browser alert ############
  output$lastupdate <- renderUI({
    if (str_detect(input$whichbrowser, "Chrome|chrome")) {
    } else {
      showModal(modalDialog(
        HTML("For the best experience please use Chrome"),
        size = "s",
        footer = NULL,
        easyClose = TRUE
      ))
    }
  })
  output$backendupdate <- renderUI({
    if (time_granularity=="WEEKLY"){
      str1 <- paste0("Data through <b>",format(date_desc,"%B %d %Y"),"<b>")
    }else{
      str1 <- paste0("Data through <b>",format(date_desc,"%B %Y"),"<b>")
    }
    
    HTML(paste(str1,";<br>",versupdate(),";<br> R code version ", code_version,"<br>"))
  })
  ##### Header Alert Title ##########
  output$header_alert_title <- renderUI({
    title_txt <- str_to_title(gsub("Flower","",str_replace_all(row.names(filelist), "_|.RData", " "),ignore.case = T))
    title_txt <- str_replace_all(title_txt, "Us", "US")
    title_txt <- str_replace_all(title_txt, "Uk", "UK")
    paste(title_txt, "Alerts")
  })
  ##### total table Title ##########
  output$totalstitle<-renderUI({
    HTML(paste0('<h5><em class = "tooltip-emphasize">',market_level_total, '</em> for <em class = "tooltip-emphasize">',input$prod,'</em> within <em class = "tooltip-emphasize">',input$sharebase, '</em> for <em class = "tooltip-emphasize">', input$currentperiod,"</em>*</h5>"))
  })
  #####Dynamic Table Titles############
  output$winning_title <- renderUI({
    getwintitle(top_tables,input,minimal_treshold,headers_mapping)
  })
  
  output$losing_title <- renderUI({
    getlosstitle(top_tables,input,minimal_treshold,headers_mapping)
  })
  ##############select input to control number of rown in winner and losers tables###################
  output$winnum <- renderUI({
    winnersnum(top_tables, input, input$winthreshold, minimal_treshold)
  })
  output$lossnum <- renderUI({
    losersnum(top_tables, input, input$lossthreshold, minimal_treshold)
  })
  #####################################threshold for dpp##################################
  #select DPP threshold to control number of rown in winner and losers tables
  output$winthreshold <- renderUI({
    getwinthresh(input,top_tables,minimal_treshold)
  })
  
  output$lossthreshold <- renderUI({
    getlossthresh(input,top_tables,minimal_treshold)
  })
  #main table text
  output$mktalrts_title <- renderUI({
    # Get Parameters for titles
    delay(5000,1+1)
    mktlerts_title(NULL,input,list_markets,values,drivers_time_series,market_level_total,total_market,hoverover_df)
  })
  #V1 Appendix
  output$mktalrts_appendix <- renderUI({
    # TODO: to paramaterize the threshold
    mktalrtsappendix(input,minimal_treshold)
  })
  output$mktalrts_title2 <- renderUI({
    # Get Parameters for titles
    delay(5000,1+1)
    mktlerts_title2(NULL,input,list_markets,values,drivers_time_series,market_level_total,total_market,hoverover_df,big_competitors_treshold)
  })
  #Big competitors core table
  output$ushctab2_box_ui <- renderUI({
    req(big_competitors_treshold)
    box(class = "no-border",  id = 'big_competitors_box',
        title = "Big Competitors with share change in same direction as P&G",
        fluidRow(class = "input-panel", column(10, htmlOutput("mktalrts_title2"))),
        status = "primary", collapsible = TRUE, collapsed = TRUE, solidHeader = FALSE, width = 12,
        fluidRow(column(12 ,DT::dataTableOutput('ushctab2')))
    )
  })
  #toggle response
  observeEvent(input$ic,{
    values$ic<-input$ic
  })
  ############################################ushctab click####################################
  ############################################ushctab click####################################
  #observe event on main ushctab click and showing xmas tree and rapaint main tab so next click works
  observeEvent(input$ushctab1_cell_clicked, {
    # Only trigger if click on the market/product/attribute col
    req(input$ushctab1_cell_clicked$col == 0 & values$core1$x$data$tree[input$ushctab1_cell_clicked$row] != "")
    
    x<-strsplit(values$core1$x$data$tree[input$ushctab1_cell_clicked$row],"=")[[1]][length(strsplit(values$core1$x$data$tree[input$ushctab1_cell_clicked$row],"=")[[1]])]
    x<-strsplit(x,">")[[1]][1]
    values$tree_name<-substr(trimws(x),2,nchar(trimws(x))-1)
    tree_name=values$tree_name
    # Create modal
    showModal(modalDialog(
      htmlOutput("bestpathhtml"),
      # Use grVizOutput to render the tree in Shiny
      fluidPage(
        fluidRow(
          column(12,
                 div(id = "treeHolder", style = 'height:750px;overflow-y:auto;overflow-x: scroll', fCTROutput("colltree"))
          )
        ),
        fluidRow(
          column(10,
                 #h4("Other Paths explored by SAMANTA Algorithms:"),
                 checkboxInput("bestpathCheck", "Other Paths Explored by SAMANTA Algorithms:"),
                 conditionalPanel(
                   condition = "input.bestpathCheck == true",
                   dataTableOutput("bestpath_list"))
          ),
          column(2,
                 checkboxInput("treelegCheck", "Color mapping"),
                 conditionalPanel(
                   condition = "input.treelegCheck == true",
                   div(dataTableOutput("treeleg_list"))
                 )
                 
          )
        ),
        checkboxInput("vwappendix1", "View Appendix"),
        uiOutput("shrtreeappdx1")
      ),
      size = "l",
      footer = NULL,
      easyClose = TRUE
    ))
    #repaint main tab so next click works
    output$ushctab1<-DT::renderDataTable({
      dtable<-getushctab1(values$mkt,input,list_markets,hoverover_df,values,market_level_total,total_market,multiplication_factor,headers_mapping,1)
      values$core1<-dtable
      dtable
    },server=T)
    output$mktalrts_title <- renderUI({
      # Get Parameters for titles
      delay(5000,1+1)
      mktlerts_title(NULL,input,list_markets,values,drivers_time_series,market_level_total,total_market,hoverover_df)
    })
  })
  observeEvent(input$ushctab2_cell_clicked, {
    # Only trigger if click on the market/product/attribute col
    req(input$ushctab2_cell_clicked$col == 0 & values$core2$x$data$tree[input$ushctab2_cell_clicked$row] != "")
    x<-strsplit(values$core2$x$data$tree[input$ushctab2_cell_clicked$row],"=")[[1]][length(strsplit(values$core2$x$data$tree[input$ushctab2_cell_clicked$row],"=")[[1]])]
    x<-strsplit(x,">")[[1]][1]
    values$tree_name<-substr(trimws(x),2,nchar(trimws(x))-1)
    tree_name=values$tree_name
    # Create modal
    showModal(modalDialog(
      htmlOutput("bestpathhtml2"),
      # Use grVizOutput to render the tree in Shiny
      fluidPage(
        fluidRow(
          column(12,
                 div(id = "treeHolder2", style = 'height:750px;overflow-y:scroll;overflow-x: scroll', fCTROutput("colltree2"))
          )
        ),
        fluidRow(
          column(10,
                 #h4("Other Paths explored by SAMANTA Algorithms:"),
                 checkboxInput("bestpathCheck2", "Other Paths Explored by SAMANTA Algorithms:"),
                 conditionalPanel(
                   condition = "input.bestpathCheck2 == true",
                   dataTableOutput("bestpath_list2"))
          ),
          column(2,
                 #h4("Other Paths explored by SAMANTA Algorithms:"),
                 checkboxInput("treelegCheck2", "Color mapping"),
                 conditionalPanel(
                   condition = "input.treelegCheck2 == true",
                   dataTableOutput("treeleg_list2"))
          )
        ),
        checkboxInput("vwappendix2", "View Appendix"),
        uiOutput("shrtreeappdx2")
      ),
      size = "l",
      footer = NULL,
      easyClose = TRUE
    ))
    #repaint main tab so next click works
    output$ushctab2<-DT::renderDataTable({
      dtable<-getushctab1(values$mkt,input,list_markets,hoverover_df,values,market_level_total,total_market,multiplication_factor,headers_mapping,2)
      values$core2<-dtable
      dtable
      },server=T)
    output$mktalrts_title <- renderUI({
      # Get Parameters for titles
      delay(5000,1+1)
      mktlerts_title(NULL,input,list_markets,values,drivers_time_series,market_level_total,total_market,hoverover_df)
    })
  })
  output$shrtreeappdx1<-renderUI({
    if (input$vwappendix1==TRUE){
      HTML("<b>How is the share tree created?  </b><br>The share tree is created by picking the product attribute (eg Size, Form, etc) with the maximum share change for each value. <br>
                   <br><br><br>                   
                   <table style='width:100%;font-family:arial, sans-serif;border-collapse:collapse;'>
                   <tr style='border:1px solid #dddddd;text-align:left;padding:8px;'>
                   <th>Tree Image</th>
                   <th>Explanation</th> 
                   </tr>
                   <tr style='border:1px solid #dddddd;text-align:left;padding:8px;'>
                   <td><img src='sharetree.jpg' height='300' width='450'>></td>
                   <td>EG: Within a product level (eg Brand) each item might have different product drivers. In this example H&S is driven by Origin (eg Local), Rejoice is driven by Anti-Dandruff, <br>and H&S Supreme is driven by bottle size (750ML). </td> 
                   </tr>
                   </table>")
    }
  })
  output$shrtreeappdx2<-renderUI({
    if (input$vwappendix1==TRUE){
      HTML("<b>How is the share tree created?  </b><br>The share tree is created by picking the product attribute (eg Size, Form, etc) with the maximum share change for each value. <br>
                   <br><br><br>                   
                   <table style='width:100%;font-family:arial, sans-serif;border-collapse:collapse;'>
                   <tr style='border:1px solid #dddddd;text-align:left;padding:8px;'>
                   <th>Tree Image</th>
                   <th>Explanation</th> 
                   </tr>
                   <tr style='border:1px solid #dddddd;text-align:left;padding:8px;'>
                   <td><img src='sharetree.jpg' height='300' width='450'>></td>
                   <td>EG: Within a product level (eg Brand) each item might have different product drivers. In this example H&S is driven by Origin (eg Local), Rejoice is driven by Anti-Dandruff, <br>and H&S Supreme is driven by bottle size (750ML). </td> 
                   </tr>
                   </table>")
    }
  })
  output$treeleg_list<-DT::renderDataTable({
    coltab<-values$coltab
    coltab$levs<-as.character.factor(coltab$levs)
    coltab$colorslev<-as.character.factor(coltab$colorslev)
    colnames(coltab)<-c("AttributeName","Hexcode")
    coltab$color<-""
    datatable(coltab,rownames = FALSE,options = list(columnDefs = list(list(visible=FALSE, targets=c(1,2))),dom = 'tp')) %>% formatStyle(
      "AttributeName","Hexcode",
      backgroundColor = styleEqual(c(coltab$Hexcode), c(coltab$Hexcode))
    )
    
  })
  output$treeleg_list2<-DT::renderDataTable({
    coltab<-values$coltab
    coltab$levs<-as.character.factor(coltab$levs)
    coltab$colorslev<-as.character.factor(coltab$colorslev)
    colnames(coltab)<-c("AttributeName","Hexcode")
    coltab$color<-""
    datatable(coltab,rownames = FALSE,options = list(columnDefs = list(list(visible=FALSE, targets=c(1,2))),dom = 'tp')) %>% formatStyle(
      "AttributeName","Hexcode",
      backgroundColor = styleEqual(c(coltab$Hexcode), c(coltab$Hexcode))
    )
    
  })
  #output server function  generate collapsible tree
  output$colltree<-arenderCTR({
    visgraph <- tryCatch({
      if (input$show_whichtree=="Simplified Tree" & input$show_whichpath=="Most Informative Path"){
        getcolltree(cristmas_tree_tables_part,values,input,proxy1,market_level_total,total_market,best_paths,1,attribute_color_map)
      }else if (input$show_whichtree=="Full Tree" & input$show_whichpath=="Most Informative Path"){
        getcolltree(cristmas_tree_tables_full,values,input,proxy1,market_level_total,total_market,best_paths,1,attribute_color_map)
      }else if (input$show_whichtree=="Simplified Tree" & input$show_whichpath=="Predetermind Path"){
        getcolltree(cristmas_tree_tables_part_default,values,input,proxy1,market_level_total,total_market,default_paths,1,attribute_color_map)
      }else if (input$show_whichtree=="Full Tree" & input$show_whichpath=="Predetermind Path"){
        getcolltree(cristmas_tree_tables_full_default,values,input,proxy1,market_level_total,total_market,default_paths,1,attribute_color_map)
      }
    }, error = function(err) {
      error(logger, 'err')
      type <- c("NULL")
      name <- c("No Selected Path")
      size <- c(rep(3840,1))
      data <- data.frame(type, name, size)
      jsonOut<-toJSON(list(name="No Selected Path",children=makeList(data)))
      jsonOut2 <- str_replace_all(jsonOut, "[\r\n]" , "")
      CTR(jsonOut2,width="1200px",height="600px")
    }, finally = {
    }) # END tryCatch
    values$treeplot<-visgraph$plot
    values$coltab<-visgraph$coltab
    values$treeplot
    
    
  })
  output$colltree2<-frenderCTR({
    visgraph <- tryCatch({
      if (input$show_whichtree=="Simplified Tree" & input$show_whichpath=="Most Informative Path"){
        getcolltree(cristmas_tree_tables_part,values,input,proxy1,market_level_total,total_market,best_paths,2,attribute_color_map)
      }else if (input$show_whichtree=="Full Tree" & input$show_whichpath=="Most Informative Path"){
        getcolltree(cristmas_tree_tables_full,values,input,proxy1,market_level_total,total_market,best_paths,2,attribute_color_map)
      }else if (input$show_whichtree=="Simplified Tree" & input$show_whichpath=="Predetermind Path"){
        getcolltree(cristmas_tree_tables_part_default,values,input,proxy1,market_level_total,total_market,default_paths,2,attribute_color_map)
      }else if (input$show_whichtree=="Full Tree" & input$show_whichpath=="Predetermind Path"){
        getcolltree(cristmas_tree_tables_full_default,values,input,proxy1,market_level_total,total_market,default_paths,2,attribute_color_map)
      }
    }, error = function(err) {
      error(logger, 'err')
      type <- c("NULL")
      name <- c("No Selected Path")
      size <- c(rep(3840,1))
      data <- data.frame(type, name, size)
      jsonOut<-toJSON(list(name="No Selected Path",children=makeList(data)))
      jsonOut2 <- str_replace_all(jsonOut, "[\r\n]" , "")
      CTR(jsonOut2,width="1200px",height="600px")
    }, finally = {
    }) # END tryCatch
    values$treeplot<-visgraph$plot
    values$coltab<-visgraph$coltab
    values$treeplot
    
    
  })
  #output server function  Create Best Path List
  output$bestpath_list <- renderDataTable({
    bestpathlist(ex_search_tables,input,values,market_level_total,total_market)
  })
  output$bestpath_list2 <- renderDataTable({
    bestpathlist(ex_search_tables,input,values,market_level_total,total_market)
  })
  #output server function display best path in the share tree
  output$bestpathhtml<-renderUI({
    bestpath <- tryCatch({
      if (exists("cristmas_tree_tables_full_default")){
        if (length(cristmas_tree_tables_full_default)>0){
          getbestpath(best_paths,values,market_level_total,total_market,input,headers_mapping,1)
        }else{
          getbestpath(best_paths,values,market_level_total,total_market,input,headers_mapping,0)
        }
      }else{
        getbestpath(best_paths,values,market_level_total,total_market,input,headers_mapping,0)
      }
    }, error = function(err) {
      error(logger, 'err')
    }, finally = {
    }) # END tryCatch
    bestpath
    
    
  })
  output$bestpathhtml2<-renderUI({
    bestpath <- tryCatch({
      if (exists("cristmas_tree_tables_full_default")){
        if (length(cristmas_tree_tables_full_default)>0){
          getbestpath(best_paths,values,market_level_total,total_market,input,headers_mapping,1)
        }else{
          getbestpath(best_paths,values,market_level_total,total_market,input,headers_mapping,0)
        }
      }else{
        getbestpath(best_paths,values,market_level_total,total_market,input,headers_mapping,0)
      }
    }, error = function(err) {
      error(logger, 'err')
    }, finally = {
    }) # END tryCatch
    bestpath
    
  })
  #observe event on main ushctab click and showing driver and value share trends and rapaint main tab so next click works
  observeEvent(input$ushctab1_cell_clicked, {
    # Only trigger if click on the market/product/attribute col
    req(input$ushctab1_cell_clicked$col == 2 & values$core1$x$data$Trend[input$ushctab1_cell_clicked$row] != "")
    values$tree_name <- values$core1$x$data$Trend[input$ushctab1_cell_clicked$row] %>% read_html() %>% html_node('span') %>% html_attr("title")
    # Obtain tree name to display the right tree
    # Create modal
    showModal(modalDialog(
      htmlOutput("drvsharetitle"),
      # Use grVizOutput to render the tree in Shiny
      # HTML('<button type="button" class="btn btn-default round-button"
      #      data-dismiss="modal" style = "float: right;margin-bottom: -35px;\">Close</button>'),
      fluidPage(
        fluidRow(
          column(12, div(style = 'overflow-x: scroll',htmlOutput("hcontainer"))),
          HTML("<br><br>")
          # column(12, div(style = 'overflow-x: scroll', highchartOutput("hcontainer",width = "100%", height = input$GetScreenHeight * .75))) # get screen height from the jscode in ui.R
        )
      ),
      size = "l",
      footer = NULL,
      easyClose = TRUE
    ))
    #repaint main tab so next click works
    output$ushctab1<-DT::renderDataTable({
      dtable<-getushctab1(values$mkt,input,list_markets,hoverover_df,values,market_level_total,total_market,multiplication_factor,headers_mapping,1)
      values$core1<-dtable
      dtable
    },server=T)
    output$mktalrts_title <- renderUI({
      # Get Parameters for titles
      delay(5000,1+1)
      mktlerts_title(NULL,input,list_markets,values,drivers_time_series,market_level_total,total_market,hoverover_df)
    })
  })
  observeEvent(input$ushctab2_cell_clicked, {
    # Only trigger if click on the market/product/attribute col
    req(input$ushctab2_cell_clicked$col == 2 & values$core2$x$data$Trend[input$ushctab2_cell_clicked$row] != "")
    values$tree_name <- values$core2$x$data$Trend[input$ushctab2_cell_clicked$row] %>% read_html() %>% html_node('span') %>% html_attr("title")
    # Obtain tree name to display the right tree
    # Create modal
    showModal(modalDialog(
      htmlOutput("drvsharetitle2"),
      # Use grVizOutput to render the tree in Shiny
      # HTML('<button type="button" class="btn btn-default round-button"
      #      data-dismiss="modal" style = "float: right;margin-bottom: -35px;\">Close</button>'),
      fluidPage(
        fluidRow(
          column(12, div(style = 'overflow-x: scroll',htmlOutput("hcontainer2")))
          # column(12, div(style = 'overflow-x: scroll', highchartOutput("hcontainer",width = "100%", height = input$GetScreenHeight * .75))) # get screen height from the jscode in ui.R
        )
      ),
      size = "l",
      footer = NULL,
      easyClose = TRUE
    ))
    #repaint main tab so next click works
    output$ushctab2<-DT::renderDataTable({
      dtable<-getushctab1(values$mkt,input,list_markets,hoverover_df,values,market_level_total,total_market,multiplication_factor,headers_mapping,2)
      values$core2<-dtable
      dtable
    },server=T)
    output$mktalrts_title <- renderUI({
      # Get Parameters for titles
      delay(5000,1+1)
      mktlerts_title(NULL,input,list_markets,values,drivers_time_series,market_level_total,total_market,hoverover_df)
    })
  })
  #output server function  create driver and share plots
  # output$hcontainer <- renderHighchart({
  #   gethc_allinone(clist,llist,list_markets,values,input,market_level_total,total_market,drivers_time_series,time_granularity,hoverover_df,zone_mapping_list)
  # })
  output$hcontainer <- renderUI({
    result = tryCatch({
      req(rnr_data_prepared())
      charts <- gethc_onebyone(clist,llist,list_markets,values,input,market_level_total,total_market,drivers_time_series,time_granularity,hoverover_df,zone_mapping_list,height = input$GetScreenHeight,width=input$GetScreenWidth,ratings = rnr_data_prepared())
      do.call(tagList, charts)
    }, error = function(e) {
      error(logger, 'e')
      charts <- gethc_onebyone(clist,llist,list_markets,values,input,market_level_total,total_market,drivers_time_series,time_granularity,hoverover_df,zone_mapping_list,height = input$GetScreenHeight,width=input$GetScreenWidth,ratings = NULL)
      do.call(tagList, charts)
    }, finally = {
    })
    result
  })
  output$hcontainer2 <- renderUI({
    result = tryCatch({
      req(rnr_data_prepared())
      charts <- gethc_onebyone(clist,llist,list_markets,values,input,market_level_total,total_market,drivers_time_series,time_granularity,hoverover_df,zone_mapping_list,height = input$GetScreenHeight,width=input$GetScreenWidth,ratings = rnr_data_prepared())
      do.call(tagList, charts)
    }, error = function(e) {
      charts <- gethc_onebyone(clist,llist,list_markets,values,input,market_level_total,total_market,drivers_time_series,time_granularity,hoverover_df,zone_mapping_list,height = input$GetScreenHeight,width=input$GetScreenWidth,ratings = NULL)
      do.call(tagList, charts)
    }, finally = {
    })
    result
  })
  #display title for driver chart
  output$drvsharetitle<-renderUI({
    getdrvsharetitle(list_markets,input,values, market_level_total, total_market)
  })
  output$drvsharetitle2<-renderUI({
    getdrvsharetitle(list_markets,input,values, market_level_total, total_market)
  })
  
  
  
  
  ############################################ratings click####################################
  #observe event on main ushctab click and showing ratings popup
  observeEvent(input$ushctab1_cell_clicked, {
    # Only trigger if click on the market/product/attribute col
    req(input$ushctab1_cell_clicked$col == (8+2*nrow(hoverover_df)) & input$ushctab1_cell_clicked$value != "")
    x<-strsplit(input$ushctab1_cell_clicked$value,"=")[[1]][length(strsplit(input$ushctab1_cell_clicked$value,"=")[[1]])]
    x<-strsplit(x,">")[[1]][1]
    values$tree_name<-substr(trimws(x),2,nchar(trimws(x))-1)
    tree_name=values$tree_name
    # Create modal
    showModal(modalDialog(
      htmlOutput("reviewhtml"),
      # Use grVizOutput to render the tree in Shiny
      fluidPage(
        fluidRow(
          column(12,
                 highchartOutput("Reviewchart")
          ),
          
          column(12,
                 HTML("<br><br><br><br>"),
                 box(class = "pad-side no-border",
                     title = "Periodic Analysis of Reviews", status = "primary", collapsed = FALSE, solidHeader = TRUE, width = 12,
                     fluidRow(column(6, uiOutput("ratmon"))),
                     # column(6, radioButtons('revlang', 'Review language:' , choices = list(Local = FALSE, English = TRUE), inline = TRUE))),
                     fluidRow(
                       column(9, DT::dataTableOutput("revtxt")),
                       column(3,plotOutput("revcloud"))
                     )
                 )
          ))
      ),
      size = "l",
      footer = NULL,
      easyClose = TRUE
    ))
    #repaint main tab so next click works
    
    output$ushctab1<-DT::renderDataTable({
      dtable<-getushctab1(values$mkt,input,list_markets,hoverover_df,values,market_level_total,total_market,multiplication_factor,headers_mapping,1)
      values$core1<-dtable
      dtable
    },server=T)
    output$mktalrts_title <- renderUI({
      # Get Parameters for titles
      delay(5000,1+1)
      mktlerts_title(input,list_markets,values,drivers_time_series,market_level_total,total_market,hoverover_df)
    })
    
  })
  
  current_total_table <- eventReactive(values$mkt, {
    market <- values$mkt
    req(length(grep("TOP123", market)) > 0)
    list_markets[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[market_level_total]][[total_market]][[input$currentperiod]][[input$refperiod]]
  })
  
  current_rnr_match <- reactive({
    req(current_total_table())
    idx <- as.numeric(values$tree_name)
    current_total_table()$rnr_match[idx]
  })
  
  rnr_data_prepared <- eventReactive(current_rnr_match(), {
    
    prod <- current_rnr_match()
    #df <- drivers_time_series[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[market_level_total]][[total_market]][[input$currentperiod]][[input$refperiod]][[idx]]
    
    #df[, 2] <- NULL
    
    dfr <- rnr_data[[1]][which(rnr_data[[1]][[input$prodhier]] == prod), c(1, 6, 7, 8, 9, 10, 11, 14)]
    colnames(dfr)[grep("Date",colnames(dfr),ignore.case = T)]="Date"
    #dfr <- merge(df, dfr, by = "Date", all.x = F, all.y = T)
    #dfr <- subset(dfr, select = -c(which(colnames(dfr) %in% hoverover_df$driver_short_names)))
    dfr <- dfr[!is.na(dfr$Date), ]
    rating_columns <- which(str_detect(colnames(dfr), '^[0-5]$'))
    colnames(dfr)[rating_columns] <- paste0("Rating_", colnames(dfr)[rating_columns])
    dfr <- dfr %>% 
      group_by(Date) %>% 
      summarise(Rating_0 = sum(Rating_0, na.rm = T),
                Rating_1 = sum(Rating_1, na.rm = T),
                Rating_2 = sum(Rating_2, na.rm = T),
                Rating_3 = sum(Rating_3, na.rm = T),
                Rating_4 = sum(Rating_4, na.rm = T),
                Rating_5 = sum(Rating_5, na.rm = T),
                AVERAGE_RATING = mean(AVERAGE_RATING, na.rm = T)
      )
    dfr[is.na(dfr)] <- 0
    
    dfr
  })
  
  #####header for modal
  output$reviewhtml <- renderUI({
    req(current_total_table())
    HTML(paste0('<h4 style = "color: green;">Review for <span style = "color: #3182bd;"><strong>', current_total_table()[as.numeric(values$tree_name), 1], '</span></strong>
                  <button type="button" class="btn btn-default round-button"data-dismiss="modal" style = "float: right; margin-top: -10px;">Close</button>'))
  })
  
  output$Reviewchart <- renderHighchart({
    req(rnr_data_prepared())
    
    bkcolor = "#ffffff"
    height = input$GetScreenHeight
    width = input$GetScreenWidth
    
    if (time_granularity == "WEEKLY") {
      formatstr = "%d%b%y"
    } else{
      formatstr = "%b%y"
    }
    
    dfr <- rnr_data_prepared()
    xax <- dfr$Date
    dfr$Date <- NULL
    lineser <- dfr[, ncol(dfr)]
    dfr[,ncol(dfr)] <- NULL
    dfr <- gather(dfr)
    dfr <- dfr %>%
      # we change the key to name to have the label in the legend
      group_by(name = key) %>%  
      # the data in this case is simple, is just .$value column
      do(data = .$value) 
    series <- list_parse(dfr)
    lineser <- as.matrix(lineser)
    hc <- highchart(width=0.80*width,height=0.25*height) %>% 
      hc_chart(marginLeft=70,marginRight=150,marginBottom=20,backgroundColor=bkcolor,zoomType = "xy",panKey="shift",panning=TRUE)%>% 
      hc_chart(type = "column") %>%
      hc_yAxis_multiples(
        list(lineWidth = 3,title=list(text="Ratings")),
        list(showLastLabel = FALSE,opposite = TRUE,title=list(text="Avg Rating"))
      ) %>% 
      hc_xAxis(categories = format(xax, format=formatstr),tickInterval=1,labels=list(align='left'))%>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        stacking = "normal",
        enableMouseTracking = T)
      ) %>% 
      hc_add_series_list(series) %>%
      hc_add_series(lineser, type = "line",name = "Avg Rating", yAxis = 1,
                    tooltip = list(pointFormat = "Avg Rating : {point.y:.2f}"), shared = FALSE) %>%
      hc_legend(enabled = TRUE,align = 'right',
                verticalAlign = 'top',
                layout = 'vertical',floating = T) %>%
      hc_tooltip(shared = FALSE, pointFormat = "Selected # Reviews : {point.y}<br>Total # Reviews : {point.total}")
    hc
  })
  
  output$ratmon <- renderUI({
    req(rnr_data_prepared())
    dateRangeInput("ratmon", label="Select Period", start = min(rnr_data_prepared()$Date), end = max(rnr_data_prepared()$Date),
                   min = min(rnr_data_prepared()$Date),
                   max = max(rnr_data_prepared()$Date),
                   format = "M-yyyy", startview = "year", weekstart = 0,
                   language = "en", separator = " to ", width = NULL)
    # selectizeInput("ratmon", "Select Period", choices = rnr_data_prepared()$Date)
  })
  
  output$revtxt <- DT::renderDataTable({
    
    req(current_rnr_match(), input$ratmon)#, input$revlang)
    target_dates <- ceiling_dates_to_month(input$ratmon)
    mind = target_dates[1]
    maxd = target_dates[2]
    # if (input$revlang)
    # text_col <- "REVIEW_TEXT_EN"
    # else
    text_col <- "REVIEW_TEXT"
    
    REVIEWS <- rnr_data[[3]]
    
    
    df <- REVIEWS[REVIEWS[[input$prodhier]] == current_rnr_match(),]
    
    rev_dates <- ceiling_dates_to_month(df$DATE)
    
    df <- filter(df, rev_dates >= mind, rev_dates <= maxd) %>%
      select_(.dots = c('REVIEW_RATING', text_col))
    
    df
  }, rownames = FALSE, options = list(dom = "ftp",pageLength = 10, autoWidth = T,scrollX=T,
                                      columnDefs = list(list(width = '95%', targets = c(1))))
  )

  
  output$revcloud <- renderPlot({
    
    req(current_rnr_match(), input$ratmon)#, input$revlang)
    target_dates <- ceiling_dates_to_month(input$ratmon)
    mind = target_dates[1]
    maxd = target_dates[2]
    
    # if(input$revlang)
    # text_col <- "KEY_WORD_EN"
    # else
    text_col <- "KEY_WORD"
    
    REVIEW_KEYS <- rnr_data[[4]]
    
    df <- REVIEW_KEYS[REVIEW_KEYS[[input$prodhier]] == current_rnr_match(),]

    rev_dates <- ceiling_dates_to_month(df$DATE)
    
    df <- filter(df, rev_dates >= mind, rev_dates <= maxd) %>%
      select_(.dots = c(text_col, 'SCORE'))
    
    df$SCORE <- round(df$SCORE * 100, 0)
    
    colnames(df) <- c("words", "freq")
    df <- na.omit(df)
    
    if (nrow(df) != 0) {
      suppressWarnings(wordcloud(
        words = df$words, freq = df$freq, min.freq = 1, max.words = 200,
        random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2")
      ))
    }
  })
  
  #################################Competition trend click####################################
  #observe event to call modal for competiton trend
  observeEvent(input$show_comp_trend, {
    select_category <- input$sharebase
    # Create modal
    showModal(modalDialog(
      title = HTML(paste0("<h3 style = 'color: green;'>Competition vs <span style = 'color: #3182bd;'><strong>", 
                          paste0(input$prod,"</strong></span> in <span style = 'color: #3182bd;'><strong>",gsub(paste0("/ ",input$sharebase),"",gsub("TOP123","",values$mkt),fixed=T),"</span></strong> within <span style = 'color: #3182bd;'><strong>", select_category, "</span></strong>"))),
      HTML('<button type="button" class="btn btn-default round-button"
           data-dismiss="modal" style = "float: right;">Close</button>'),
      # Use grVizOutput to render the tree in Shiny
      fluidPage(
        fluidRow(
          column(12,
                 highchartOutput("trendchartc", height = input$GetScreenHeight * .7) # get screen height from the jscode in ui.R
          ))
        
      ),
      size = "l",
      footer = NULL,
      easyClose = TRUE
      ))
  })
  #output server function get competitor trend
  output$trendchartc<-renderHighchart({
    getcomptrend(list_markets,brand_competitors_time_series,zone_mapping_list,values,input,market_level_total,total_market,time_granularity)
  })
  #################################Competition table click####################################
  #observe event to call modal for competiton trend
  observeEvent(input$show_comp_table, {
    select_category <- input$sharebase
    # Create modal
    showModal(modalDialog(
      title = HTML(paste0("<h3 style = 'color: green;'>Competitive Scoring for <span style = 'color: #3182bd;'><strong>", 
                          paste0(input$prod,"</strong></span> in <span style = 'color: #3182bd;'><strong>", gsub(paste0("/ ",input$sharebase),"",gsub("TOP123","",values$mkt),fixed=T),
                                 "</strong></span> within <span style = 'color: #3182bd;'><strong>",input$sharebase,"</span></strong></p>"))),
      HTML('<button type="button" class="btn btn-default round-button"
           data-dismiss="modal" style = "float: right;">Close</button>'),
      # Use grVizOutput to render the tree in Shiny
      fluidPage(
        fluidRow(
          column(12,
                 div(style='padding-bottom: 3cm;',DT::dataTableOutput("comptab")),
                 htmlOutput("comp_appendix")
          ))
      ),
      size = "l",
      footer = NULL,
      easyClose = TRUE
      ))
  })
  ####Competirotn appendix
  output$comp_appendix <- renderUI({
    # TODO: to paramaterize the threshold
    comappendix(input,minimal_treshold)
  })
  #output server function get competitor trend
  output$comptab<-DT::renderDataTable({
    getcomptab(rtb_competition,input,values,market_level_total,total_market)
  })
  #################################observe even on input selections####################################
  observeEvent(input$metrictype, {
    req((as.numeric(difftime(Sys.time(),starttime,units="secs")))>values$nstartdelay)
    req(!values$url_not_used)
    values$ddchanged=TRUE
    if (input$metrictype!="Value"){
      shinyjs::hide("divtab1summ")
      shinyjs::enable("prodhier")
      shinyjs::enable("prod")
    }else{
      shinyjs::show("divtab1summ")
      shinyjs::disable("prodhier")
      shinyjs::disable("prod")
    }
    market<-NULL
    values$mkt<-market
    proxy1 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy2 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy3 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy4 %>% selectCells(NULL) %>% selectRows(NULL)
    
    sharebases<-names(top_tables[[input$metrictype]])
    currsharebasetype<-input$sharebasetype
    
    if (!(input$sharebasetype %in% sharebases)){
      updateSelectizeInput(session,"sharebasetype",choices=sortcustom(sharebases))
    }else{
      updateSelectizeInput(session,"sharebasetype",choices=sortcustom(sharebases),selected=currsharebasetype)
    }
    shares<-names(top_tables[[input$metrictype]][[input$sharebasetype]])
    currshare<-input$sharebase
    
    if (!(input$sharebase %in% shares)){
      updateSelectizeInput(session,"sharebase",choices=sortcustom(shares))
    }else{
      updateSelectizeInput(session,"sharebase",choices=sortcustom(shares),selected=currshare)
    }
    prodhier<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]])
    currprodhier<-input$prodhier
    
    if (!(input$prodhier %in% prodhier)){
      updateSelectizeInput(session,"prodhier",choices=sortcustom(prodhier))
    }else{
      updateSelectizeInput(session,"prodhier",choices=sortcustom(prodhier),selected=currprodhier)
    }
    prod<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]])
    currprod<-input$prod
    
    if (!(input$prod %in% prod)){
      updateSelectizeInput(session,"prod",choices=sortcustom(prod))
    }else{
      updateSelectizeInput(session,"prod",choices=sortcustom(prod),selected=currprod)
    }
    if (length(names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]]))>1){
      Martyp<-setdiff(names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]]),market_level_total)
    }else{
      Martyp<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]])
    }
    
    currMartyp<-input$Martyp
    
    
    if (!(input$Martyp %in% Martyp)){
      updateSelectizeInput(session,"Martyp",choices=sortcustom(Martyp))
    }else{
      updateSelectizeInput(session,"Martyp",choices=sortcustom(Martyp),selected=currMartyp)
    }
    currentperiod<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]])
    currcurrentperiod<-input$currentperiod
    
    if (!(input$currentperiod %in% currentperiod)){
      updateSelectizeInput(session,"currentperiod",choices=sortcustom(currentperiod))
    }else{
      updateSelectizeInput(session,"currentperiod",choices=sortcustom(currentperiod),selected=currcurrentperiod)
    }
    refperiod<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[input$currentperiod]])
    currrefperiod<-input$refperiod
    
    if (!(input$refperiod %in% refperiod)){
      updateSelectizeInput(session,"refperiod",choices=sortcustom(refperiod))
    }else{
      updateSelectizeInput(session,"refperiod",choices=sortcustom(refperiod),selected=currrefperiod)
    }
    
    # display selected data
    output$ushctab1<-DT::renderDataTable({
      out <- tryCatch(
        {
          # hide("ushctab1")
        },
        error=function(cond) {
          nodata <- data.frame(No = paste0(input$prod," has no Alert in ", market))
          datatable(nodata, colnames = NULL, rownames = FALSE,  options = list(dom = 't'))
        },
        warning=function(cond) {
        },
        finally={
        }
      )
    },server=T)
    output$ushctab2<-DT::renderDataTable({
      out <- tryCatch(
        {
          # hide("ushctab1")
        },
        error=function(cond) {
          nodata <- data.frame(No = paste0(input$prod," has no Alert in ", market))
          datatable(nodata, colnames = NULL, rownames = FALSE,  options = list(dom = 't'))
        },
        warning=function(cond) {
        },
        finally={
        }
      )
    },server=T)
  },ignoreInit=T,priority=50)
  observeEvent(input$sharebasetype, {
    req((as.numeric(difftime(Sys.time(),starttime,units="secs")))>values$nstartdelay)
    req(!values$url_not_used)
    values$ddchanged=TRUE
    market<-NULL
    values$mkt<-market
    proxy1 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy2 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy3 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy4 %>% selectCells(NULL) %>% selectRows(NULL)
    shares<-names(top_tables[[input$metrictype]][[input$sharebasetype]])
    currshare<-input$sharebase
    if (!(input$sharebase %in% shares)){
      updateSelectizeInput(session,"sharebase",choices=sortcustom(shares))
    }else{
      updateSelectizeInput(session,"sharebase",choices=sortcustom(shares),selected=currshare)
    }
    if (input$metrictype!="Value"){
      prodhier<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]])
      currprodhier<-input$prodhier
      if (!(input$prodhier %in% prodhier)){
        updateSelectizeInput(session,"prodhier",choices=sortcustom(prodhier))
      }else{
        updateSelectizeInput(session,"prodhier",choices=sortcustom(prodhier),selected=currprodhier)
      }
      prod<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]])
      currprod<-input$prod
      if (!(input$prod %in% prod)){
        updateSelectizeInput(session,"prod",choices=sortcustom(prod))
      }else{
        updateSelectizeInput(session,"prod",choices=sortcustom(prod),selected=currprod)
      }
    }else{
      updateSelectizeInput(session,"prodhier",selected=input$sharebasetype,choices = input$sharebasetype)
      updateSelectizeInput(session,"prod",selected=input$sharebase,choices=input$sharebase)
    }
    
    if (length(names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]]))>1){
      Martyp<-setdiff(names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]]),market_level_total)
    }else{
      Martyp<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]])
    }
    currMartyp<-input$Martyp
    if (!(input$Martyp %in% Martyp)){
      updateSelectizeInput(session,"Martyp",choices=sortcustom(Martyp))
    }else{
      updateSelectizeInput(session,"Martyp",choices=sortcustom(Martyp),selected=currMartyp)
    }
    
    currentperiod<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]])
    currcurrentperiod<-input$currentperiod
    if (!(input$currentperiod %in% currentperiod)){
      updateSelectizeInput(session,"currentperiod",choices=sortcustom(currentperiod))
    }else{
      updateSelectizeInput(session,"currentperiod",choices=sortcustom(currentperiod),selected=currcurrentperiod)
    }
    refperiod<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[input$currentperiod]])
    currrefperiod<-input$refperiod
    if (!(input$refperiod %in% refperiod)){
      updateSelectizeInput(session,"refperiod",choices=sortcustom(refperiod))
    }else{
      updateSelectizeInput(session,"refperiod",choices=sortcustom(refperiod),selected=currrefperiod)
    }
    # display selected data
    output$ushctab1<-DT::renderDataTable({
      out <- tryCatch(
        {
          # hide("ushctab1")
        },
        error=function(cond) {
          nodata <- data.frame(No = paste0(input$prod," has no Alert in ", market))
          datatable(nodata, colnames = NULL, rownames = FALSE,  options = list(dom = 't'))
        },
        warning=function(cond) {
        },
        finally={
        }
      )
    },server=T)
    output$ushctab2<-DT::renderDataTable({
      out <- tryCatch(
        {
          # hide("ushctab1")
        },
        error=function(cond) {
          nodata <- data.frame(No = paste0(input$prod," has no Alert in ", market))
          datatable(nodata, colnames = NULL, rownames = FALSE,  options = list(dom = 't'))
        },
        warning=function(cond) {
        },
        finally={
        }
      )
    },server=T)
  },ignoreInit=T,priority=50)
  observeEvent(input$sharebase, {
    req((as.numeric(difftime(Sys.time(),starttime,units="secs")))>values$nstartdelay)
    req(!values$url_not_used)
    values$ddchanged=TRUE
    market<-NULL
    values$mkt<-market
    proxy1 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy2 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy3 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy4 %>% selectCells(NULL) %>% selectRows(NULL)
    if (input$metrictype!="Value"){
      prodhier<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]])
      currprodhier<-input$prodhier
      if (!(input$prodhier %in% prodhier)){
        updateSelectizeInput(session,"prodhier",choices=sortcustom(prodhier))
      }else{
        updateSelectizeInput(session,"prodhier",choices=sortcustom(prodhier),selected=currprodhier)
      }
      prod<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]])
      currprod<-input$prod
      if (!(input$prod %in% prod)){
        updateSelectizeInput(session,"prod",choices=sortcustom(prod))
      }else{
        updateSelectizeInput(session,"prod",choices=sortcustom(prod),selected=currprod)
      }
    }else{
      updateSelectizeInput(session,"prodhier",selected=input$sharebasetype,choices = input$sharebasetype)
      updateSelectizeInput(session,"prod",selected=input$sharebase,choices=input$sharebase)
    }
    
    if (length(names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]]))>1){
      Martyp<-setdiff(names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]]),market_level_total)
    }else{
      Martyp<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]])
    }
    currMartyp<-input$Martyp
    if (!(input$Martyp %in% Martyp)){
      updateSelectizeInput(session,"Martyp",choices=sortcustom(Martyp))
    }else{
      updateSelectizeInput(session,"Martyp",choices=sortcustom(Martyp),selected=currMartyp)
    }
    currentperiod<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]])
    currcurrentperiod<-input$currentperiod
    if (!(input$currentperiod %in% currentperiod)){
      updateSelectizeInput(session,"currentperiod",choices=sortcustom(currentperiod))
    }else{
      updateSelectizeInput(session,"currentperiod",choices=sortcustom(currentperiod),selected=currcurrentperiod)
    }
    refperiod<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[input$currentperiod]])
    currrefperiod<-input$refperiod
    if (!(input$refperiod %in% refperiod)){
      updateSelectizeInput(session,"refperiod",choices=sortcustom(refperiod))
    }else{
      updateSelectizeInput(session,"refperiod",choices=sortcustom(refperiod),selected=currrefperiod)
    }
    
    # display selected data
    output$ushctab1<-DT::renderDataTable({
      out <- tryCatch(
        {
          # hide("ushctab1")
        },
        error=function(cond) {
          nodata <- data.frame(No = paste0(input$prod," has no Alert in ", market))
          datatable(nodata, colnames = NULL, rownames = FALSE,  options = list(dom = 't'))
        },
        warning=function(cond) {
        },
        finally={
        }
      )
    },server=T)
    output$ushctab2<-DT::renderDataTable({
      out <- tryCatch(
        {
          # hide("ushctab1")
        },
        error=function(cond) {
          nodata <- data.frame(No = paste0(input$prod," has no Alert in ", market))
          datatable(nodata, colnames = NULL, rownames = FALSE,  options = list(dom = 't'))
        },
        warning=function(cond) {
        },
        finally={
        }
      )
    },server=T)
  },ignoreInit=T,priority=50)
  observeEvent(input$prodhier, {
    print(as.numeric(Sys.time()-starttime))
    req((as.numeric(difftime(Sys.time(),starttime,units="secs")))>values$nstartdelay)
    req(!values$url_not_used)
    values$ddchanged=TRUE
    market<-NULL
    values$mkt<-market
    proxy1 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy2 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy3 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy4 %>% selectCells(NULL) %>% selectRows(NULL)
    prod<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]])
    currprod<-input$prod
    if (!(input$prod %in% prod)){
      updateSelectizeInput(session,"prod",choices=sortcustom(prod))
    }else{
      updateSelectizeInput(session,"prod",choices=sortcustom(prod),selected=currprod)
    }
    
    if (length(names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]]))>1){
      Martyp<-setdiff(names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]]),market_level_total)
    }else{
      Martyp<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]])
    }
    
    currMartyp<-input$Martyp
    if (!(input$Martyp %in% Martyp)){
      updateSelectizeInput(session,"Martyp",choices=sortcustom(Martyp))
    }else{
      updateSelectizeInput(session,"Martyp",choices=sortcustom(Martyp),selected=currMartyp)
    }
    currentperiod<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]])
    currcurrentperiod<-input$currentperiod
    if (!(input$currentperiod %in% currentperiod)){
      updateSelectizeInput(session,"currentperiod",choices=sortcustom(currentperiod))
    }else{
      updateSelectizeInput(session,"currentperiod",choices=sortcustom(currentperiod),selected=currcurrentperiod)
    }
    refperiod<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[input$currentperiod]])
    currrefperiod<-input$refperiod
    if (!(input$refperiod %in% refperiod)){
      updateSelectizeInput(session,"refperiod",choices=sortcustom(refperiod))
    }else{
      updateSelectizeInput(session,"refperiod",choices=sortcustom(refperiod),selected=currrefperiod)
    }
    # proxy2 %>% reloadData(resetPaging = TRUE, clearSelection = c("all"))
    # display selected data
    output$ushctab1<-DT::renderDataTable({
      out <- tryCatch(
        {
          # hide("ushctab1")
        },
        error=function(cond) {
          nodata <- data.frame(No = paste0(input$prod," has no Alert in ", market))
          datatable(nodata, colnames = NULL, rownames = FALSE,  options = list(dom = 't'))
        },
        warning=function(cond) {
        },
        finally={
        }
      )
    },server=T)
    output$ushctab2<-DT::renderDataTable({
      out <- tryCatch(
        {
          # hide("ushctab1")
        },
        error=function(cond) {
          nodata <- data.frame(No = paste0(input$prod," has no Alert in ", market))
          datatable(nodata, colnames = NULL, rownames = FALSE,  options = list(dom = 't'))
        },
        warning=function(cond) {
        },
        finally={
        }
      )
    },server=T)
  },ignoreInit=T,priority=50)
  observeEvent(input$prod, {
    req((as.numeric(difftime(Sys.time(),starttime,units="secs")))>values$nstartdelay)
    req(!values$url_not_used)
    values$ddchanged=TRUE
    market<-NULL
    values$mkt<-market
    proxy1 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy2 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy3 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy4 %>% selectCells(NULL) %>% selectRows(NULL)
    if (length(names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]]))>1){
      Martyp<-setdiff(names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]]),market_level_total)
    }else{
      Martyp<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]])
    }
    currMartyp<-input$Martyp
    if (!(input$Martyp %in% Martyp)){
      updateSelectizeInput(session,"Martyp",choices=sortcustom(Martyp))
    }else{
      updateSelectizeInput(session,"Martyp",choices=sortcustom(Martyp),selected=currMartyp)
    }
    currentperiod<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]])
    currcurrentperiod<-input$currentperiod
    if (!(input$currentperiod %in% currentperiod)){
      updateSelectizeInput(session,"currentperiod",choices=sortcustom(currentperiod))
    }else{
      updateSelectizeInput(session,"currentperiod",choices=sortcustom(currentperiod),selected=currcurrentperiod)
    }
    refperiod<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[input$currentperiod]])
    currrefperiod<-input$refperiod
    if (!(input$refperiod %in% refperiod)){
      updateSelectizeInput(session,"refperiod",choices=sortcustom(refperiod))
    }else{
      updateSelectizeInput(session,"refperiod",choices=sortcustom(refperiod),selected=currrefperiod)
    }
    
    
    # proxy2 %>% reloadData(resetPaging = TRUE, clearSelection = c("all"))
    # display selected data
    output$ushctab1<-DT::renderDataTable({
      out <- tryCatch(
        {
          # hide("ushctab1")
        },
        error=function(cond) {
          nodata <- data.frame(No = paste0(input$prod," has no Alert in ", market))
          datatable(nodata, colnames = NULL, rownames = FALSE,  options = list(dom = 't'))
        },
        warning=function(cond) {
        },
        finally={
        }
      )
    },server=T)
    output$ushctab2<-DT::renderDataTable({
      out <- tryCatch(
        {
          # hide("ushctab1")
        },
        error=function(cond) {
          nodata <- data.frame(No = paste0(input$prod," has no Alert in ", market))
          datatable(nodata, colnames = NULL, rownames = FALSE,  options = list(dom = 't'))
        },
        warning=function(cond) {
        },
        finally={
        }
      )
    },server=T)
  },ignoreInit=T,priority=50)
  observeEvent(input$Martyp, {
    req((as.numeric(difftime(Sys.time(),starttime,units="secs")))>values$nstartdelay)
    req(!values$url_not_used)
    values$ddchanged=TRUE
    market<-NULL
    values$mkt<-market
    proxy1 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy2 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy3 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy4 %>% selectCells(NULL) %>% selectRows(NULL)
    currentperiod<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]])
    currcurrentperiod<-input$currentperiod
    if (!(input$currentperiod %in% currentperiod)){
      updateSelectizeInput(session,"currentperiod",choices=sortcustom(currentperiod))
    }else{
      updateSelectizeInput(session,"currentperiod",choices=sortcustom(currentperiod),selected=currcurrentperiod)
    }
    refperiod<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[input$currentperiod]])
    currrefperiod<-input$refperiod
    if (!(input$refperiod %in% refperiod)){
      updateSelectizeInput(session,"refperiod",choices=sortcustom(refperiod))
    }else{
      updateSelectizeInput(session,"refperiod",choices=sortcustom(refperiod),selected=currrefperiod)
    }
    
    # proxy2 %>% reloadData(resetPaging = TRUE, clearSelection = c("all"))
    # display selected data
    output$ushctab1<-DT::renderDataTable({
      out <- tryCatch(
        {
          # hide("ushctab1")
        },
        error=function(cond) {
          nodata <- data.frame(No = paste0(input$Brands," has no Alert in ", market))
          datatable(nodata, colnames = NULL, rownames = FALSE,  options = list(dom = 't'))
        },
        warning=function(cond) {
        },
        finally={
        }
      )
    },server=T)
    output$ushctab2<-DT::renderDataTable({
      out <- tryCatch(
        {
          # hide("ushctab1")
        },
        error=function(cond) {
          nodata <- data.frame(No = paste0(input$prod," has no Alert in ", market))
          datatable(nodata, colnames = NULL, rownames = FALSE,  options = list(dom = 't'))
        },
        warning=function(cond) {
        },
        finally={
        }
      )
    },server=T)
  },ignoreInit=T,priority=50)
  observeEvent(input$currentperiod, {
    print((as.numeric(Sys.time()-starttime)))
    req((as.numeric(difftime(Sys.time(),starttime,units="secs")))>values$nstartdelay)
    req(!values$url_not_used)
    values$ddchanged=TRUE
    market<-NULL
    values$mkt<-market
    proxy1 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy2 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy3 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy4 %>% selectCells(NULL) %>% selectRows(NULL)
    refperiod<-names(top_tables[[input$metrictype]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[input$currentperiod]])
    if (length(refperiod)==0){
      refperiod<-NULL
    }
    currrefperiod<-input$refperiod
    if (!(input$refperiod %in% refperiod) & length(refperiod)>0){
      updateSelectizeInput(session,"refperiod",choices=sortcustom(refperiod))
    }else if((input$refperiod %in% refperiod) & length(refperiod)>0){
      updateSelectizeInput(session,"refperiod",choices=sortcustom(refperiod),selected=currrefperiod)
    }else if(is.null(refperiod)){
      updateSelectizeInput(session,"refperiod",choices="",selected="")
    }
    # display selected data
    output$ushctab1<-DT::renderDataTable({
      out <- tryCatch(
        {
          # hide("ushctab1")
        },
        error=function(cond) {
          nodata <- data.frame(No = paste0(input$prod," has no Alert in ", market))
          datatable(nodata, colnames = NULL, rownames = FALSE,  options = list(dom = 't'))
        },
        warning=function(cond) {
        },
        finally={
        }
      )
    },server=T)
    output$ushctab2<-DT::renderDataTable({
      out <- tryCatch(
        {
          # hide("ushctab1")
        },
        error=function(cond) {
          nodata <- data.frame(No = paste0(input$prod," has no Alert in ", market))
          datatable(nodata, colnames = NULL, rownames = FALSE,  options = list(dom = 't'))
        },
        warning=function(cond) {
        },
        finally={
        }
      )
    },server=T)
  },ignoreInit=T,priority=50)
  observeEvent(input$refperiod, {
    req((as.numeric(difftime(Sys.time(),starttime,units="secs")))>values$nstartdelay)
    req(!values$url_not_used)
    values$ddchanged=TRUE
    values$mkt<-NULL
    proxy1 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy2 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy3 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy4 %>% selectCells(NULL) %>% selectRows(NULL)
    # display selected data
    output$ushctab1<-DT::renderDataTable({
      out <- tryCatch(
        {
          # hide("ushctab1")
        },
        error=function(cond) {
          nodata <- data.frame(No = paste0(input$prod," has no Alert in ", market))
          datatable(nodata, colnames = NULL, rownames = FALSE,  options = list(dom = 't'))
        },
        warning=function(cond) {
        },
        finally={
        }
      )
    },server=T)
    output$ushctab2<-DT::renderDataTable({
      out <- tryCatch(
        {
          # hide("ushctab1")
        },
        error=function(cond) {
          nodata <- data.frame(No = paste0(input$prod," has no Alert in ", market))
          datatable(nodata, colnames = NULL, rownames = FALSE,  options = list(dom = 't'))
        },
        warning=function(cond) {
        },
        finally={
        }
      )
    },server=T)
    query <- parseQueryString(session$clientData$url_search)
    if (length(query)>0){
      updateSelectizeInput(session, "onload",  selected="b")
    }
    
  },ignoreInit=T,priority=50)
  
  #########################observe event on total market click##############################
  observeEvent(input$totalmarkdt_cell_clicked, {
    # Only trigger if click on the market col
    shinyjs::show("ushctab1")
    req(input$totalmarkdt_cell_clicked$col == 0 & input$totalmarkdt_cell_clicked$value != "")
    values$ddchanged=TRUE
    market <- paste0("TOP123",strsplit(strsplit(input$totalmarkdt_cell_clicked$value,"'blue'>")[[1]][2],"</font>")[[1]][1])
    values$mkt<-market
    if (input$metrictype!="Value"){
      shinyjs::hide("divtab1summ")
    }else{
      shinyjs::show("divtab1summ")
    }
    proxy1 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy2 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy3 %>% selectCells(NULL) %>% selectRows(NULL)
    output$totalmarkdt <- DT::renderDataTable({
      wltable(top_tables,input,"total",market_level_total,total_market,headers_mapping)
    }, server = FALSE)
    
    if (market_to_path==T){
      values$mtpf=T
      values$mtp="MARKET_TO_PATH"

      # eval(parse(text=paste0("choice=c(\"Key Products\" = \"tab2\",\"Key Products and ",input$Martyp,"\"=\"tab1\")")))
      # showModal(modalDialog(
      #   title=HTML(paste0("<h3 style = 'color: green;'>Show me the:</h3>")),
      #   div(style='padding-top: 0.5cm;padding-bottom: 0.5cm;',actionButton("but1","The Key Products",icon=icon("table"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4",width="100%")),
      #   actionButton("but2",paste0("The Key ",input$Martyp,"/Products"),icon=icon("table"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4",width="100%"),
      #   size = "s",
      #   footer =  NULL,
      #   easyClose = TRUE
      # ))
    }else{
      values$mtpf=F
    }
    values$tottabtype="tab2"
    output$mktalrts_title <- renderUI({
      # Get Parameters for titles
      delay(5000,1+1)
      mktlerts_title(NULL,input,list_markets,values,drivers_time_series,market_level_total,total_market,hoverover_df)
      
    })
    output$ushctab1<-DT::renderDataTable({
      dtable<-getushctab1(values$mkt,input,list_markets,hoverover_df,values,market_level_total,total_market,multiplication_factor,headers_mapping,1)
      values$core1<-dtable
      dtable
    },server=T)
    big_comp_table <- getushctab1(market,input,list_markets,hoverover_df,values,market_level_total,total_market,multiplication_factor,headers_mapping,2)
    if (colnames(big_comp_table$x$data)[1] != "No") {
      shinyjs::show('ushctab2_box_ui')
    } else {
      shinyjs::hide('ushctab2_box_ui')
    }
    output$ushctab2 <- DT::renderDataTable({
      big_comp_table
    }, server = FALSE)
  })
  observeEvent(input$mtypec,{
    if (input$mtypec==F){
      values$mtypec=FALSE
      values$tottabtype="tab2"
      output$mktalrts_title <- renderUI({
        # Get Parameters for titles
        delay(5000,1+1)
        mktlerts_title(NULL,input,list_markets,values,drivers_time_series,market_level_total,total_market,hoverover_df)
      })
      output$ushctab1<-DT::renderDataTable({
        dtable<-getushctab1(values$mkt,input,list_markets,hoverover_df,values,market_level_total,total_market,multiplication_factor,headers_mapping,1)
        values$core1<-dtable
        dtable
      },server=T)
      output$ushctab2<-DT::renderDataTable({
        dtable<-getushctab1(values$mkt,input,list_markets,hoverover_df,values,market_level_total,total_market,multiplication_factor,headers_mapping,2)
        values$core2<-dtable
        dtable
      },server=T)
      values$ddchanged=TRUE
    }else{
      values$mtypec=TRUE
      values$tottabtype="tab1"
      values$mtp="MARKET_TO_PATH"
      output$mktalrts_title <- renderUI({
        # Get Parameters for titles
        delay(5000,1+1)
        mktlerts_title(NULL,input,list_markets,values,drivers_time_series,market_level_total,total_market,hoverover_df)
      })
      output$ushctab1<-DT::renderDataTable({
        dtable<-getushctab1(values$mkt,input,list_markets,hoverover_df,values,market_level_total,total_market,multiplication_factor,headers_mapping,1)
        values$core1<-dtable
        dtable
      },server=T)
      output$ushctab2<-DT::renderDataTable({
        dtable<-getushctab1(values$mkt,input,list_markets,hoverover_df,values,market_level_total,total_market,multiplication_factor,headers_mapping,2)
        values$core2<-dtable
        dtable
      },server=T)
      values$ddchanged=FALSE
    }
  })
  # observeEvent(input$but1,{
  #   values$tottabtype="tab2"
  #   output$mktalrts_title <- renderUI({
  #     # Get Parameters for titles
  #     delay(5000,1+1)
  #     mktlerts_title(NULL,input,list_markets,values,drivers_time_series,market_level_total,total_market,hoverover_df)
  #   })
  #   output$ushctab1<-DT::renderDataTable({
  #     dtable<-getushctab1(values$mkt,input,list_markets,hoverover_df,values,market_level_total,total_market,multiplication_factor,headers_mapping,1)
  #     values$core1<-dtable
  #     dtable
  #   },server=T)
  #   output$ushctab2<-DT::renderDataTable({
  #     dtable<-getushctab1(values$mkt,input,list_markets,hoverover_df,values,market_level_total,total_market,multiplication_factor,headers_mapping,2)
  #     values$core2<-dtable
  #     dtable
  #   },server=T)
  #   removeModal()
  # })
  # observeEvent(input$but2,{
  #   values$tottabtype="tab1"
  #   output$mktalrts_title <- renderUI({
  #     # Get Parameters for titles
  #     delay(5000,1+1)
  #     mktlerts_title(NULL,input,list_markets,values,drivers_time_series,market_level_total,total_market,hoverover_df)
  #   })
  #   output$ushctab1<-DT::renderDataTable({
  #     dtable<-getushctab1(values$mkt,input,list_markets,hoverover_df,values,market_level_total,total_market,multiplication_factor,headers_mapping,1)
  #     values$core1<-dtable
  #     dtable
  #   },server=T)
  #   output$ushctab2<-DT::renderDataTable({
  #     dtable<-getushctab1(values$mkt,input,list_markets,hoverover_df,values,market_level_total,total_market,multiplication_factor,headers_mapping,2)
  #     values$core2<-dtable
  #     dtable
  #   },server=T)
  #   removeModal()
  # })
  #########################observe event on winning market click##############################
  observeEvent(input$bmwin_cell_clicked, {
    # Only trigger if click on the market col
    shinyjs::show("ushctab1")
    values$mtpf=F
    
    req(input$bmwin_cell_clicked$col == 0 & input$bmwin_cell_clicked$value != "")
    market <- strsplit(strsplit(input$bmwin_cell_clicked$value,"'blue'>")[[1]][2],"</font>")[[1]][1]
    values$mkt<-market
    if (input$metrictype!="Value"){
      shinyjs::hide("divtab1summ")
    }else{
      shinyjs::show("divtab1summ")
    }
    proxy1 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy2 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy4 %>% selectCells(NULL) %>% selectRows(NULL)
    
    # display selected data
    output$mktalrts_title <- renderUI({
      # Get Parameters for titles
      delay(5000,1+1)
      mktlerts_title(NULL,input,list_markets,values,drivers_time_series,market_level_total,total_market,hoverover_df)
    })
    output$ushctab1<-DT::renderDataTable({
      dtable<-getushctab1(values$mkt,input,list_markets,hoverover_df,values,market_level_total,total_market,multiplication_factor,headers_mapping,1)
      values$core1<-dtable
      dtable
    },server=T)
    
    big_comp_table <- getushctab1(market,input,list_markets,hoverover_df,values,market_level_total,total_market,multiplication_factor,headers_mapping,2)
    if (colnames(big_comp_table$x$data)[1] != "No") {
      shinyjs::show('ushctab2_box_ui')
    } else {
      shinyjs::hide('ushctab2_box_ui')
    }
    output$ushctab2 <- DT::renderDataTable({
      big_comp_table
    }, server = FALSE)
    
    output$bmwin <- DT::renderDataTable({
      wltable(top_tables,input,"win",market_level_total,total_market,headers_mapping)
    }, server = TRUE)
    
  })
  #########################observe event on losing market click##############################
  observeEvent(input$bmloss_cell_clicked, {
    shinyjs::show("ushctab1")
    # Only trigger if click on the market col
    req(input$bmloss_cell_clicked$col == 0 & input$bmloss_cell_clicked$value != "")
    values$mtpf=F
    market <- strsplit(strsplit(input$bmloss_cell_clicked$value,"'blue'>")[[1]][2],"</font>")[[1]][1]
    values$mkt<-market
    if (input$metrictype!="Value"){
      shinyjs::hide("divtab1summ")
    }else{
      shinyjs::show("divtab1summ")
    }
    proxy1 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy3 %>% selectCells(NULL) %>% selectRows(NULL)
    proxy4 %>% selectCells(NULL) %>% selectRows(NULL)
    # proxy2 %>% reloadData(resetPaging = TRUE, clearSelection = c("all"))
    
    output$ushctab1<-DT::renderDataTable({
      dtable<-getushctab1(values$mkt,input,list_markets,hoverover_df,values,market_level_total,total_market,multiplication_factor,headers_mapping,1)
      values$core1<-dtable
      dtable
    },server=T)
    output$mktalrts_title <- renderUI({
      # Get Parameters for titles
      delay(5000,1+1)
      mktlerts_title(NULL,input,list_markets,values,drivers_time_series,market_level_total,total_market,hoverover_df)
    })
    big_comp_table <- getushctab1(market,input,list_markets,hoverover_df,values,market_level_total,total_market,multiplication_factor,headers_mapping,2)
    if (colnames(big_comp_table$x$data)[1] != "No") {
      shinyjs::show('ushctab2_box_ui')
    } else {
      shinyjs::hide('ushctab2_box_ui')
    }
    output$ushctab2 <- DT::renderDataTable({
      big_comp_table
    }, server = FALSE)
    
    output$bmloss <- DT::renderDataTable({
      wltable(top_tables,input,"loss",market_level_total,total_market,headers_mapping)
    }, server = FALSE)
  })
  #########################get top tables##############################
  #total market
  output$totalmarkdt <- DT::renderDataTable({
    wltable(top_tables,input,"total",market_level_total,total_market,headers_mapping)
  }, server = FALSE)
  
  # Market Winning Table
  output$bmwin <- DT::renderDataTable({
    wltable(top_tables,input,"win",market_level_total,total_market,headers_mapping)
  }, server = FALSE)
  
  # Market Losing Table
  output$bmloss <- DT::renderDataTable({
    wltable(top_tables,input,"loss",market_level_total,total_market,headers_mapping)
  }, server = FALSE)
  
  # # Create Best Path List
  
  # #get number of tree levels
  # output$treelevel <- renderUI({
  #   getlevlist(cristmas_tree_tables_full,input,values)
  # })
  #
  
  # #get full tree
  # output$tree <- renderVisNetwork({
  #   fulltree(cristmas_tree_tables_full,values,input,market_level_total,total_market)
  # })
  # #get compressed tree
  # output$comptree <- renderVisNetwork({
  #   comptree(cristmas_tree_tables_full,values,input,market_level_total,total_market)
  # })
  #
  # #generate networked tree
  # output$nettree <- renderVisNetwork({
  #   nettree(cristmas_tree_tables_full,values,input,market_level_total,total_market)
  # })
  #
  
  #
  
  # output$nvd3chart <- renderUI({
  #   chart_out_list<<-getnvd3(clist,llist,list_markets,values,input,market_level_total,total_market,drivers_time_series,period_lag)
  #   do.call(tagList, chart_out_list)
  # })
  #
  #
  # Hide the loading message when the rest of the server function has executed
  if (loadmsg!="DataNotFound"){
    hide(id = "loading-content", anim = TRUE, animType = "fade") 
  }
   
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste(gsub("TOP123","",title1), '_', Sys.Date(), '.csv', sep='') 
    },
    content = function(file) {
      bbb$Trend<-NULL
      bbb$tree<-NULL
      bbb$mainrow<-NULL
      bbb[,2:ncol(bbb)]<-round(bbb[,2:ncol(bbb)],1)
      for (i in 1:nrow(bbb)){
        bbb$Product[i]<-bbb$Product[i] %>% read_html() %>% html_nodes("span") %>% html_text()
      }
      
      write.csv(bbb, file, row.names = FALSE)
    }
  )
  #get parameteters from URL
  urlPars <- reactive({
    parseQueryString(session$clientData$url_search)
  })
  observeEvent(urlPars(), {
    req(length(urlPars()) == 0)
    req(values$url_not_used)
    values$nstartdelay=0
    update_inputs <- function(val, check_nm, select_type, warn, select_type_pretty = select_type){
      path <- check_nm[1]
      if (val %in% check_nm) {
        updateSelectizeInput(session, select_type, choices = check_nm, selected = val)
      }else {
        updateSelectizeInput(session, select_type, choices = check_nm,  selected = check_nm[1])
      }
      return(warn)
    }
    warn=""
    warn <- update_inputs(kpi_default, inputs_on_init[[1]], "metrictype", warn)
    warn <- update_inputs(share_base_type_default, inputs_on_init[[2]], "sharebasetype", warn, "Sharebase type")
    warn <- update_inputs(share_base_value_default, inputs_on_init[[3]], "sharebase", warn, "Sharebase")
    if (kpi_default=="Value"){
      warn <- update_inputs(product_line_type_default, inputs_on_init[[4]], "prodhier", warn, "Product Hierarchy")
      warn <- update_inputs(product_line_value_default, inputs_on_init[[5]], "prod", warn, "Product")
      shinyjs::disable("prodhier")
      shinyjs::disable("prod")
    }else{
      warn <- update_inputs(share_base_type_default, inputs_on_init[[4]], "prodhier", warn, "Product Hierarchy")
      warn <- update_inputs(share_base_value_default, inputs_on_init[[5]], "prod", warn, "Product")
    }
    
    warn <- update_inputs(market_type_default, inputs_on_init[[6]], "Martyp", warn, "Market Type")
    warn <- update_inputs(curent_period_default, inputs_on_init[[7]], "currentperiod", warn, "Current period")
    warn <- update_inputs(reference_period_default, inputs_on_init[[8]], "refperiod", warn, "Reference period")
    values$url_not_used <- FALSE
  },priority=100)
  
  observeEvent(urlPars(), {
    req(length(urlPars()) > 0)
    req(values$url_not_used)
    values$nstartdelay=10
    query <- urlPars()
    show <- query[["show"]]
    if (is.null(show) == TRUE) {
      show <- ""
    }
    style <- query[["style"]]
    if (is.null(style) == TRUE) {
      style <- ""
    }
    ## Read Parameters Current Period and Reference Period
    CP <- query$CP
    RP <- query$RP
    ## Fix the Current/Reference Period - This is HARD CODED
    CP <- switch(CP,
                 FYTD = "FYTD",
                 P1M = "Past 1 month",
                 P2M = "Past 2 months",
                 P3M = "Past 3 months",
                 P6M = "Past 6 months",
                 P12M = "Past 12 months",
                 "")
    RP <- switch(RP,
                 PP = "Previous period",
                 YA = "Year ago",
                 "")

    ## Read more parameters  
    KPI <- query[["KPI"]]; if(is.null(KPI) == TRUE){KPI <- ""}; 
    KPI <- gsub(pattern = "_", replacement = " ", x = KPI); KPI <- tolower(KPI)
    KPI <- paste(toupper(substr(KPI, 1, 1)), substr(KPI, 2, nchar(KPI)), sep="")
    MT <- query[["MT"]]; if(is.null(MT) == TRUE){MT <- ""}
    PH <- query[["PH"]]; if(is.null(PH) == TRUE){PH <- ""}
    SBT <- query[["SBT"]]; if(is.null(SBT) == TRUE){SBT <- ""}
    SB <- query[["SB"]]; if(is.null(SB) == TRUE){SB <- ""}
    PROD <- query[["PROD"]]; if(is.null(PROD) == TRUE){PROD <- ""}
    Market <- query[["Market"]]; if(is.null(Market) == TRUE){Market <- ""}
    Total<-query[["Total"]]; if(is.null(Total) == TRUE){Total <- ""}
    warn <- list(msg="", warnind=0, path = c())
    
    if (style == 2) {
      
      update_inputs <- function(val, check_nm, select_type, warn, select_type_pretty = select_type){
        path <- check_nm[1]
        if (val %in% check_nm) {
          updateSelectizeInput(session, select_type, choices = check_nm, selected = val)
          path <- val
        }else if (val == "") {
          updateSelectizeInput(session, select_type, choices = check_nm,  selected = check_nm[1])
        }else{
          warn$warnind = 1
          warn$msg = paste(warn$msg, select_type_pretty, " was changed from ", val , " to ", check_nm[1], "<br>")
          updateSelectizeInput(session, select_type, choices = check_nm,  selected = check_nm[1])
        }
        warn$path <- c(warn$path, path)
        return(warn)
      }
      
      warn <- update_inputs(KPI, names(top_tables), "metrictype", warn)
      warn <- update_inputs(SBT, names(from_named_nested_list(top_tables, warn$path)), "sharebasetype", warn, "Sharebase type")
      warn <- update_inputs(SB, names(from_named_nested_list(top_tables, warn$path)), "sharebase", warn, "Sharebase")
      warn <- update_inputs(PH, names(from_named_nested_list(top_tables, warn$path)), "prodhier", warn, "Product Hierarchy")
      warn <- update_inputs(PROD, names(from_named_nested_list(top_tables, warn$path)), "prod", warn, "Product")
      if (length(names(from_named_nested_list(top_tables, warn$path)))==1){
        warn <- update_inputs(MT, names(from_named_nested_list(top_tables, warn$path)), "Martyp", warn, "Market Type")
      }else{
        warn <- update_inputs(MT, setdiff(names(from_named_nested_list(top_tables, warn$path)),market_level_total), "Martyp", warn, "Market Type")
      }
      
      warn <- update_inputs(CP, names(from_named_nested_list(top_tables, warn$path)), "currentperiod", warn, "Current period")
      warn <- update_inputs(RP, names(from_named_nested_list(top_tables, warn$path)), "refperiod", warn, "Reference period")
      
    }
    if (nchar(warn$msg) > 0) {
      n = length(strsplit(warn$msg,"<br>")[[1]])
      m = length(unlist(strsplit(strsplit(warn$msg,"<br>")[[1]],"to"))[trimws(unlist(strsplit(strsplit(warn$msg,"<br>")[[1]],"to"))) != ""])
      if (n == m)
        warn$warnind = 0
    }
    if (warn$warnind == 1) {
      showModal(modalDialog(
        HTML(paste("<strong>Warning: Please note, we could not give you a template with the settings you requested…<br>", warn$msg, "</strong>","<button type=\"button\" class=\"btn btn-default round-button\"data-dismiss=\"modal\" style = \"float: right; margin-top: -15px;\">Close</button></h3>")),
        size = "m",
        footer =  NULL,
        easyClose = TRUE
      ))
    }else{
      showModal(modalDialog(
        HTML(paste("<strong>Please Wait. Filtering Selections</strong>","<button type=\"button\" class=\"btn btn-default round-button\"data-dismiss=\"modal\" style = \"float: right; margin-top: -15px;\">Close</button></h3>")),
        size = "s",
        footer =  NULL,
        easyClose = TRUE
      ))
      removeModal()
    }
    
    if (Market == total_market) {
      Market <- paste0("TOP123", Market)
    }
    if (show == 1) {
      values$urlmkt <- Market
      values$mkt <- Market
      if (gsub("TOP123","",Market) == total_market) {
        values$tottabtype = "tab2"
      } else{
        values$tottabtype = NULL            
      }
      #create custom input list from URL
      custom_input <- list()
      custom_input$metrictype <- warn$path[1]
      custom_input$sharebasetype <- warn$path[2]
      custom_input$sharebase <- warn$path[3]
      custom_input$prodhier <- warn$path[4]
      custom_input$prod <- warn$path[5]
      custom_input$Martyp <- warn$path[6]
      custom_input$currentperiod <- warn$path[7]
      custom_input$refperiod <- warn$path[8]
      values$ic<-FALSE
      output$ushctab1 <- DT::renderDataTable({
        dtable<-getushctab1(values$mkt,input,list_markets,hoverover_df,values,market_level_total,total_market,multiplication_factor,headers_mapping,1)
        values$core1<-dtable
        dtable
      }, server = T)
      
      output$mktalrts_title <- renderUI({
        # Get Parameters for titles
        delay(5000, 1 + 1)
        mktlerts_title(values$urlmkt,custom_input,list_markets,values,drivers_time_series,market_level_total,total_market,hoverover_df)
      })
      shinyjs::show("ushctab1")
      values$url_not_used <- FALSE
      
    }
  },priority=100)
  output$ushctab1summary<-DT::renderDataTable({
    df <- tryCatch({
      if (length(grep("TOP123",values$mkt))>0){
        x<-list_markets[["Value"]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[market_level_total]][[gsub("TOP123","",values$mkt)]][[input$currentperiod]][[input$refperiod]]
      }else{
        x<-list_markets[["Value"]][[input$sharebasetype]][[input$sharebase]][[input$prodhier]][[input$prod]][[input$Martyp]][[values$mkt]][[input$currentperiod]][[input$refperiod]]
      }
      df<-attr(x,"summary_tbl")
      row.names(df)<-NULL
      colnames(df)[1]<-"Gainers/Drainers"
      for (i in 1:nrow(df)){
        if (df[i,1]==TRUE){
          df[i,1]="Gainers"
        }else{
          df[i,1]="Drainers"
          for (j in 2:ncol(df)){
            df[i,j]<-gsub("/-","/",df[i,j])
            df[i,j]<-gsub("/","/-",df[i,j])
          }
        }
      }
      dataframe_to_datatable_3(df,show_button = FALSE)
    }, error = function(err) {
      as.data.frame("Summary table not found")
    }, finally = {
    })
    df
  })
  
})

