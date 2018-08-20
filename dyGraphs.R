#Required when dat is imported from csv---Start
#Preprocessing onn data
head(stockTemp)
#dygraphs plots every possible columns of data so remove unwanted derived columns
stockDyGraph <- stockTemp[, c(-1, -8, -9, -10, -11, -12)]
#RowNames should be date object
rownames(stockDyGraph) <- stockDyGraph[, 6]
#Remove date column
stockDyGraph <- stockDyGraph[, -6]
head(stockDyGraph)
#Required when dat is imported from csv---End

#Data from quantmod---Start
#Visualise using dyGraphs
#Plot stock data ensure date is in POSIXct format Plot only one variable
dygraph(NSEI, main = "Nifty") %>% dyRangeSelector()


# Add one more Y axis to dygraph
dygraph(NSEI, main = "Nifty Index") %>% dyRangeSelector() %>%
  dySeries("NSEI.Volume", axis = 'y2') %>% #Add one more Y axsis and line to show Share traded
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

#dyOptions(fillGraph = TRUE, fillAlpha = 0.4) #Gives effect pf area map by coloring all area beneath the lines
#%>%
# dyRoller(rollPeriod = 10)

#Visualise using candlestick and xts pacakge (Plots 1st 4 columns as candles remaining are treated as lines)
dygraph(stockDyGraph[, c(-5)]) %>%
  dyCandlestick() 

#Using quantmod data
NSEICopy<-NSEI[,c(-5,-6)]
dygraph(NSEICopy) %>%
  dyCandlestick() 

#Add EMA 5 indicator using TTR package to dygraphs
stockDyGraph$EMA5<-EMA(stockDyGraph$Close,n=5)
stockDyGraph

NSEICopy$EMA100<-EMA(NSEICopy$NSEI.Close,n=100)
NSEICopy$EMA50<-EMA(NSEICopy$NSEI.Close,n=50)
NSEICopy$EMA5<-EMA(NSEICopy$NSEI.Close,n=5)

#Check if NSEI.CLOSE is normal in nature
head(NSEI)
shapiro.test(NSEI$NSEI.Close)






