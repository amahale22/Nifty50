#Download test data using quantmod package

getSymbols('^AEX')
head(AEX)

#Preprocessing onn data
head(stockTemp)
#dygraphs plots every possible columns of data so remove unwanted derived columns
stockDyGraph <- stockTemp[, c(-1, -8, -9, -10, -11, -12)]
#RowNames should be date object
rownames(stockDyGraph) <- stockDyGraph[, 6]
#Remove date column
stockDyGraph <- stockDyGraph[, -6]
head(stockDyGraph)
#Plot stock data ensure date is in POSIXct format Plot only one variable
dygraph(stockDyGraph, main = "Nifty") %>% dyRangeSelector()


# Add one more Y axis to dygraph
dygraph(stockDyGraph, main = "Nifty Index") %>% dyRangeSelector() %>%
  dySeries("Shares.Traded", axis = 'y2') %>% #Add one more Y axsis and line to show Share traded
  dyOptions(
    colors = RColorBrewer::brewer.pal(5, "Set1"),
    axisLineColor = "navy",
    gridLineColor = "lightblue"
  ) %>% #manual color setting
  dyHighlight(
    highlightCircleSize = 5,
    highlightSeriesBackgroundAlpha = 0.2,
    highlightSeriesOpts = list(strokeWidth = 3)
  ) %>%
  dyAxis("x", label = "Date", drawGrid = F) %>%
  dyAxis("y", label = "Index", drawGrid = F) %>%
  dyLegend(show = "always", hideOnMouseOut = TRUE) #or show = "follow"



#%>% #Gives interactive points and highlighting
#dyOptions(fillGraph = TRUE, fillAlpha = 0.4) #Gives effect pf area map by coloring all area beneath the lines
#%>%
# dyRoller(rollPeriod = 10)
