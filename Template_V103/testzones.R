df<-drivers_time_series$`Value share`$MKT_ENTRIES_1$POME$BRAND$PAMPERS$Customer$AUCHAN$`Past 1 month`$`Previous period`$`1`
colnames(df)[2]<-gsub(" ","",colnames(df)[2])

bp_obj<-compute_bp_obj(df,colnames(df)[2])
bp_obj %<>% early_warning_tests()
p<-bp_ggplot2(bp_obj, 
              x = "Date",
              y = colnames(df)[2],
              ylabel =colnames(df)[2] )
ggb <- ggplot_build(p)
if (!is.null( bp_obj$early_warning$early_warning_idx)){
  p <- p + geom_vline(xintercept = head(tail(ggb$data[[1]]$x, bp_obj$early_warning$early_warning_idx), 1), 
                      linetype = 'longdash', 
                      color = "red")
}
m <- list(
  l = 10,
  r = 10,
  b = 10,
  t = 10,
  pad = 0
)

plotly::ggplotly(p) 
  #layout(autosize = F, width = 1200, height = 800)%>%
  layout(title = 'Highlighting with Rectangles',
         shapes = list(
           list(type = "rect",
                fillcolor = "red", line = list(color = "blue"), opacity = 0.3,
                x0 = 1411684131645, x1 = 1432627588607, xref = "Date",
                y0 = 0, y1 = 90, yref = colnames(df)[2]),
           list(type = "rect",
                fillcolor = "red", line = list(color = "blue"), opacity = 0.2,
                x0 = 1471684131645, x1 = 1492627588607, xref = "Date",
                y0 = 0, y1 = 90, yref = colnames(df)[2])))




library(plotly)
p <- plot_ly(economics, x = ~date, y = ~uempmed, name = "unemployment")

# add shapes to the layout
p <- layout(p, title = 'Highlighting with Rectangles',
            shapes = list(
              list(type = "rect",
                   fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                   x0 = "1980-01-01", x1 = "1985-01-01", xref = "x",
                   y0 = 4, y1 = 12.5, yref = "y"),
              list(type = "rect",
                   fillcolor = "blue", line = list(color = "blue"), opacity = 0.2,
                   x0 = "2000-01-01", x1 = "2005-01-01", xref = "x",
                   y0 = 4, y1 = 12.5, yref = "y")))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="shapes/unemployment")
chart_link