
suppressPackageStartupMessages({
  library(data.table)
  library(reshape2)
  library(dplyr)
  library(DT)
  library(sparkline)
})


# NOTE: - How-to on integration of sparklines and datatables http://leonawicz.github.io/HtmlWidgetExamples/ex_dt_sparkline.html
#       - Parameter documentation: http://omnipotent.net/jquery.sparkline/#s-docs
#       - https://github.com/htmlwidgets/sparkline/issues/3

sparkline_bar_string  <- "disableTooltips:false, width: 100, height:40, type: 'bar', barColor: 'purple', negBarColor: 'purple', highlightColor: 'black'"
sparkline_line_string <- "disableTooltips:false, width: 100, height:40, type: 'line', lineColor: 'purple', fillColor: '#ccc', highlightLineColor: 'orange', highlightSpotColor: 'orange'"
sparkline_box_string  <- "disableTooltips:false, width: 100, height:40, type: 'box', lineColor: 'purple', whiskerColor: 'black', outlierFillColor: 'black', outlierLineColor: 'black', medianColor: 'black', boxFillColor: 'orange', boxLineColor: 'black'"

callback_bar  = function(column_id) JS(paste0("$('.", column_id, ":not(:has(canvas))').sparkline('html', { ", sparkline_bar_string, " });"), collapse = "")
callback_line = function(column_id) JS(paste0("$('.", column_id, ":not(:has(canvas))').sparkline('html', { ", sparkline_line_string, "});"), collapse = "")
callback_box  = function(column_id) JS(paste0("$('.", column_id, ":not(:has(canvas))').sparkline('html', { ", sparkline_box_string, " });"), collapse = "")

sparklines_code = function(column_indices, spark_type = "bar")
{
  column_ids <- paste("column", column_indices, sep="_")
  
  # generate column definitions
  column_definitions <- lapply(seq_along(column_ids), 
                               function(i) {
                                  list(targets = column_indices[i], 
                                      render = JS(paste("function(data, type, full){ var data_split = data.split(',');
                                                  return '' + '<div class=divwrapper>' +
                                                              ' <span class=tooltipwrapper>' + Math.round(data_split[0]*10) / 10 + '</span>' +
                                                              ' <span class=", column_ids[i], ">' + data + '</span> ' + 
                                                              ' <span class=tooltipwrapper>' + Math.round(data_split[data_split.length - 1] * 10) /10 + '</span> </span>' + 
                                                              '</div>' }"))
#return '' + '<div class=divwrapper> <span class=tooltipwrapper>' +   Math.round(data_split[0]  100) / 100 + '</span>' + ' <span class=", column_ids[i], ">' + data + '</span> ' + '<span class=tooltipwrapper>' + Math.round(data_split[data_split.length - 1]  100) /100 + '</span> </div>' + '' }"))

                                  )
                               })
  
  # generate callback code
  #callbacks <- sapply(column_ids, function(column_id) callback_line(column_id)) %>% paste()
  if(spark_type == "bar") callbacks <- sapply(column_ids, function(column_id) callback_bar(column_id)) %>% paste()
  if(spark_type == "line") callbacks <- sapply(column_ids, function(column_id) callback_line(column_id)) %>% paste()
  if(spark_type == "box") callbacks <- sapply(column_ids, function(column_id) callback_box(column_id)) %>% paste()
  
  callback_fn <- JS(paste0("function (oSettings, json) {\n", paste0(callbacks, collapse = ""), "\n}"))
  
  #callback_fn %<>% paste(
  #".jqstooltip{
  #  box-sizing: content-box;
  #}", .)
  
  list(columnDefs = column_definitions, fnDrawCallback = callback_fn)
}
