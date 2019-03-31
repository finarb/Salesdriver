#################################ui function##########################################
ui <- tagList(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "stylesheet", type = "text/css",href="bootstrap-toggle.min.css"),
    tags$script(src="bootstrap-toggle.min.js"),
    # tags$script(src="rsz.js"),
    tags$script(jscode),
    tags$script(jsbrowser),
    tags$script(gacode)
  ),
  div(id = "loading-content", loadmsg, 
      img(src = "ajax-loader-bar.gif"),
      p("For the best experience please use Chrome")),
      dashboardPage(title=filename,
      dashboardHeader(title = img(src = "samanta_logo.png", height = 40),dropdownMenuOutput("Messages1"),dropdownMenuOutput("Notifications1"),dropdownMenuOutput("Notifications2")),
      dashboardSidebar(
        sidebarMenu(
          menuItem("What and Why Alerts", tabName = "mktalrts", icon = icon("dashboard")),
          uiOutput("lastupdate"),
          tags$div(
            htmlOutput("backendupdate"),
            style = "
            position:absolute;
            top:50;
            padding: 1rem;
            width:100%;
            height:50px; /* Height of the footer */
            color: white;
            background-color: black;
            z-index: 1000;"
          )
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "mktalrts",
                  fluidPage(fluidRow(
                    useShinyjs(),
                    box(class = "no-border",
                        title = uiOutput("header_alert_title"), status = "primary", collapsed = FALSE, solidHeader = TRUE, width = 12,
                        collapsible = TRUE,
                        fluidRow(class = "input-panel",
                                 column(1,
                                        class = "pad-min", selectizeInput("metrictype",label="1. KPI",choices=inputs_on_init[[1]],selected=kpi_default)),
                                 column(1, class = "pad-min", selectizeInput("Martyp",label="2. Market Type",choices=inputs_on_init[[6]],selected=market_type_default)),
                                 column(1,
                                        class = "pad-min", selectizeInput("prodhier",label="3. Product Line",choices=inputs_on_init[[4]],selected=product_line_type_default),
                                        class = "pad-min", selectizeInput("prod",label="",choices=inputs_on_init[[5]],selected=product_line_value_default)),
                                 column(1,
                                        class = "pad-min", selectizeInput("sharebasetype",label="4. Share Base Type",choices=inputs_on_init[[2]],selected=share_base_type_default),
                                        class = "pad-min", selectizeInput("sharebase",label="",choices=inputs_on_init[[3]],selected=share_base_value_default)),

                                 column(1, 
                                        class = "pad-min", selectizeInput("currentperiod",label="5. Current Period",choices=inputs_on_init[[7]],selected=curent_period_default)),
                                 column(1, 
                                        class = "pad-min", selectizeInput("refperiod",label="6. Reference Period",choices=inputs_on_init[[8]],selected=reference_period_default)),
                                 column(6, box(class ="pad-min",
                                     title = uiOutput("totalstitle"), collapsed = FALSE, solidHeader = TRUE, width = 12,
                                     collapsible = TRUE,class = "pad-min",
                                     DT::dataTableOutput('totalmarkdt')))),
                        fluidRow(
                          column(6, htmlOutput("winning_title"), DT::dataTableOutput('bmwin')),
                          column(6, htmlOutput("losing_title"), DT::dataTableOutput('bmloss'))),
                          #div(id="ltitle",style='float: right;white-space:nowrap',column(6, htmlOutput("losing_title"), DT::dataTableOutput('bmloss')))),
                        fluidRow(class = "input-panel", column(12, htmlOutput("mktalrts_title"))),
                        fluidRow(
                          column(12 ,DT::dataTableOutput('ushctab1'))
                        )
                    ),
                    div(id = "divtab1summ",
                      box(id="tab1summ",
                          title = "Number of Gainers/Drainers and their Percent of the Market’s Value Growth", status = "primary", collapsed = FALSE, solidHeader = TRUE, width = 12,
                          fluidRow(
                            column(12 ,DT::dataTableOutput('ushctab1summary'))
                          )
                      )
                    ),
                    uiOutput('ushctab2_box_ui', style = "display: none;"),
                    box(class = "pad-side no-border",
                        title = "Appendix", status = "primary", collapsed = FALSE, solidHeader = TRUE, width = 12,
                        collapsible = TRUE,
                        htmlOutput("mktalrts_appendix"),
                        selectizeInput("onload","",choices=c("a","b"),selected="a")
                    )
                    
                    # box(class = "pad-side no-border",
                    #     title = "Appendix", status = "primary", collapsed = FALSE, solidHeader = TRUE, width = 12,
                    #     collapsible = TRUE,
                    #     selectInput("grphtype","Choose Graphtype",choices=c("Tree","Compressed View","Network View"),selected="Tree"),
                    #     conditionalPanel(
                    #       condition="input.grphtype=='Tree'",
                    #       uiOutput("treelevel"),
                    #       visNetworkOutput("tree" ,width = "100%", height = "800px")
                    #     ),
                    #     conditionalPanel(
                    #       condition="input.grphtype=='Compressed View'",
                    #       visNetworkOutput("comptree" ,width = "100%", height = "800px")
                    #     ),
                    #     conditionalPanel(
                    #       condition="input.grphtype=='Network View'",
                    #       visNetworkOutput("nettree" ,width = "100%", height = "800px")
                    #     )
                    # )
                  ))
          )
        )
      )
    )
)