options(java.parameters = "-Xmx3072m")
options(shiny.trace = FALSE)
# options(shiny.error=recover)
options(shiny.sanitize.errors=TRUE)
# Declare packages
suppressMessages(library(shinydashboard))
suppressMessages(library(DT))
suppressMessages(library(rvest))
suppressMessages(library(svglite))
suppressMessages(library(svgPanZoom))
suppressMessages(library(visNetwork))
suppressMessages(library(k3d3))
suppressMessages(library(shiny))
suppressMessages(library(htmlwidgets))
suppressMessages(library(sparkline))
suppressMessages(library(readr))
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(stringr))
suppressMessages(library(magrittr))
suppressMessages(library(sqldf))
suppressMessages(library(knitr))
suppressMessages(library(tidyr))
suppressMessages(library(shinyjs))
suppressMessages(require(RJSONIO))
suppressMessages(library(rjson))
suppressMessages(library(jsonlite))
suppressMessages(library(highcharter))
suppressMessages(library(reshape2))
suppressMessages(library(lubridate))
suppressMessages(library(wordcloud))
suppressMessages(library(RColorBrewer))
suppressMessages(library(plotly))
suppressMessages(library(shinyWidgets))
suppressMessages(library(log4r))
#set logger
# Create a new logger object with create.logger().
logger <- create.logger()

# Set the logger's file output: currently only allows flat files.
logfile(logger) <- file.path('./www/logging/base.log')

# Set the current level of the logger.
level(logger) <- "DEBUG"
# library(plotly)
# library(radialNetworkR)
# library(parcoords)
# library(networkD3)
#devtools::install_github("finarb/k3d3")
# JS code to get screen height
if(!exists("code_version")) code_version <- "1.3.1.0"
# Need to be placed here so it initialized after shiny is ready
jscode <- '$(document).on("shiny:connected", function(e) {
  var jsHeight = screen.height;
  var jsWidth = screen.width;
  Shiny.onInputChange("GetScreenHeight",jsHeight);
  Shiny.onInputChange("GetScreenWidth",jsWidth);
});'

# JS code to get userAgent information for browser detection
jsbrowser <- "$(document).on('shiny:connected', function(e) {
  var whichbrowser = navigator.userAgent;
  Shiny.onInputChange('whichbrowser', whichbrowser);
});
"
#ga code
gacode<-"(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
    (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
    m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

ga('create', 'UA-73066745-6', 'auto');
ga('send', 'pageview');"

####load correct app name
suppressWarnings(filelist<-file.info(list.files(path = ".", pattern = "RData")))
suppressWarnings(filename<-gsub(".RData","",rownames(filelist)[filelist$mtime==max(filelist$mtime)],ignore.case = TRUE))
suppressWarnings(filedate<-max(filelist$mtime))

# Declare packages options
options(sqldf.driver = "SQLite") # as per FAQ #7 force SQLite
options(gsubfn.engine = "R") # as per FAQ #5 use R code rather than tcltk
#source all files
# enforce at least this version of dplyr, because an earlier version on CRAN seems to have somewhat buggy resulting,
# in PricingIndex_PM in one segment of the data being propagated to another data segment
stopifnot( packageVersion("dplyr") >= '0.4.3.9001' )
#load correct data
suppressWarnings(filelist<-file.info(list.files(path = ".", pattern = "RData")))

datafilename=rownames(filelist)[filelist$mtime==max(filelist$mtime)]
if (length(datafilename)>=1){
  load(datafilename[1])
  loadmsg=paste0("Loading ", datafilename, "...")
}else{
  error(logger, "data Not found")
  loadmsg="DataNotFound"
  
}



#######copy definition file if it exists
if (loadmsg!="DataNotFound" ){
  load(datafilename[1])
  filename<-gsub(".RData","",rownames(filelist)[filelist$mtime==max(filelist$mtime)])
  filedef<-row.names(file.info(list.files(path = ".././filedefs/", pattern = filename)))
  file.copy(paste0(".././filedefs/",filedef), "./www/",overwrite=TRUE,copy.mode = TRUE, copy.date = FALSE)
  #create missing vars
  if(!exists("val_share_diff_front_end_name"))  val_share_diff_front_end_name <- "Val Shr DPP"
  if(!exists("time_granularity"))  time_granularity <- "MONTHLY"
  if(!exists("market_to_path"))  market_to_path <- FALSE
  if(!exists("attribute_color_map")){
    attribute_color_map <- NULL
  }else{
    attribute_color_map<-as.data.frame(attribute_color_map)
  }
  #Replace minimal_treshold with a fixed number of .2
  if(!exists("minimal_treshold")){
    minimal_treshold <- .2
  } else {
    minimal_treshold <- as.numeric(minimal_treshold)
  }
  if (!exists("big_competitors_treshold"))  big_competitors_treshold <- NULL
  #load all functions
  source_fnames <- dir(".././src103/", pattern = "^[a-z].*", full.names = T)
  invisible(sapply(source_fnames, function(fname) source(fname)) )
  inputs_on_init <- list()
  inputs_on_init[[1]]<-sortcustom(names(top_tables))
  if (!exists("kpi_default")){
    kpi_default<-inputs_on_init[[1]][1]
  }else{
    if (kpi_default %in% inputs_on_init[[1]]){
      inputs_on_init[[1]]<-c(kpi_default,setdiff(inputs_on_init[[1]],kpi_default))
    }else{
      kpi_default<-inputs_on_init[[1]][1]
    }
  }
  inputs_on_init[[2]]<-sortcustom(names(top_tables[[kpi_default]]))
  if (!exists("share_base_type_default")){
    share_base_type_default<-inputs_on_init[[2]][1]
  }else{
    if (share_base_type_default %in% inputs_on_init[[2]]){
      inputs_on_init[[2]]<-c(share_base_type_default,setdiff(inputs_on_init[[2]],share_base_type_default))
    }else{
      share_base_type_default<-inputs_on_init[[2]][1]
    }
  }
  inputs_on_init[[3]]<-sortcustom(names(top_tables[[kpi_default]][[share_base_type_default]]))
  if (!exists("share_base_value_default")){
    share_base_value_default<-inputs_on_init[[3]][1]
  }else{
    if (share_base_value_default %in% inputs_on_init[[3]]){
      inputs_on_init[[3]]<-c(share_base_value_default,setdiff(inputs_on_init[[3]],share_base_value_default))
    }else{
      share_base_value_default<-inputs_on_init[[3]][1]
    }
  }
  inputs_on_init[[4]]<-sortcustom(names(top_tables[[kpi_default]][[share_base_type_default]][[share_base_value_default]]))
  if (!exists("product_line_type_default")){
    product_line_type_default<-inputs_on_init[[4]][1]
  }else{
    if (product_line_type_default %in% inputs_on_init[[4]]){
      inputs_on_init[[4]]<-c(product_line_type_default,setdiff(inputs_on_init[[4]],product_line_type_default))
    }else{
      product_line_type_default<-inputs_on_init[[4]][1]
    }
  }
  inputs_on_init[[5]]<-sortcustom(names(top_tables[[kpi_default]][[share_base_type_default]][[share_base_value_default]][[product_line_type_default]]))
  if (!exists("product_line_value_default")){
    product_line_value_default<-inputs_on_init[[5]][1]
  }else{
    if (product_line_value_default %in% inputs_on_init[[5]]){
      inputs_on_init[[5]]<-c(product_line_value_default,setdiff(inputs_on_init[[5]],product_line_value_default))
    }else{
      product_line_value_default<-inputs_on_init[[5]][1]
    }
  }
  if (length(sortcustom(names(top_tables[[kpi_default]][[share_base_type_default]][[share_base_value_default]][[product_line_type_default]][[product_line_value_default]])))>1){
    inputs_on_init[[6]]<-setdiff(sortcustom(names(top_tables[[kpi_default]][[share_base_type_default]][[share_base_value_default]][[product_line_type_default]][[product_line_value_default]])),market_level_total)
  }else{
    inputs_on_init[[6]]<-sortcustom(names(top_tables[[kpi_default]][[share_base_type_default]][[share_base_value_default]][[product_line_type_default]][[product_line_value_default]]))
  }
  if (!exists("market_type_default")){
    market_type_default<-inputs_on_init[[6]][1]
  }else{
    if (market_type_default %in% inputs_on_init[[6]]){
      inputs_on_init[[6]]<-c(market_type_default,setdiff(inputs_on_init[[6]],market_type_default))
    }else{
      market_type_default<-inputs_on_init[[6]][1]
    }
  }
  inputs_on_init[[7]]<-sortcustom(names(top_tables[[kpi_default]][[share_base_type_default]][[share_base_value_default]][[product_line_type_default]][[product_line_value_default]][[market_type_default]]))
  if (!exists("curent_period_default")){
    curent_period_default<-inputs_on_init[[7]][1]
  }else{
    if (curent_period_default %in% inputs_on_init[[7]]){
      inputs_on_init[[7]]<-c(curent_period_default,setdiff(inputs_on_init[[7]],curent_period_default))
    }else{
      curent_period_default<-inputs_on_init[[7]][1]
    }
  }
  inputs_on_init[[8]]<-sortcustom(names(top_tables[[kpi_default]][[share_base_type_default]][[share_base_value_default]][[product_line_type_default]][[product_line_value_default]][[market_type_default]][[curent_period_default]]))
  if (!exists("reference_period_default")){
    reference_period_default<-inputs_on_init[[8]][1]
  }else{
    if (reference_period_default %in% inputs_on_init[[8]]){
      inputs_on_init[[8]]<-c(reference_period_default,setdiff(inputs_on_init[[8]],reference_period_default))
    }else{
      reference_period_default<-inputs_on_init[[8]][1]
    }
  }
}else{
  ui <- fluidPage(
    textOutput("errin")
  )
  
  # Server logic
  server <- function(input, output, session) {
    
    output$errin <- renderText({
      "No Data Found"
    })
  }
  shinyApp(ui, server)
  
}
