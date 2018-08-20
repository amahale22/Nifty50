#Data when imported from csv---Start
# Read stock data from csv
stock<-read.csv("July1Aug12018.csv",stringsAsFactors = F)
head(stock)

str(stock)

# Remove Turnover column and change date to Posixct format for ease
stockTemp<-stock[,c(-7)]
stockTemp$DateNew<-as.POSIXct(stockTemp$Date,format="%d-%b-%Y")
#Data when imported from csv---End

#Use quantmod to get NSE data (Check for NA and remove if all values are missing and market was actually closed on that date)
startDate<-"2017-08-01"
endDate<-"2018-08-20"
getSymbols("^NSEI",from=startDate,to=endDate) #NIFTY50  INDIA
getSymbols("^GSPC",from=startDate,to=endDate) #S&P500  United States
getSymbols("^IXIC",from=startDate,to=endDate) #NASDAQ  United States
getSymbols("^N225",from=startDate,to=endDate) #NIKKEI 225 -JAPAN
getSymbols("^HSI",from=startDate,to=endDate) #HANGSENG -HONG KONG
getSymbols("^FTSE",from=startDate,to=endDate) #Financial Times SE -LONDON 


head(NSEI)
head(GSPC)
head(IXIC)
head(N225)
head(HSI)
NSEI<-NSEI[complete.cases(NSEI),]



# To derive new column of PrevClose and PrevOpen (Always 1st row of stock will have nothing to comapre so 0)
n<-nrow(stockTemp)
stockTemp$PrevOpen<-c(0,stockTemp$Open[1:n-1])
stockTemp$PrevClose<-c(0,stockTemp$Close[1:n-1])

# To derive difference between Current and Previous Close
# diff is an imp function here directly finds diff between 2 rows like 2bd row -1st row and so on
stockTemp$CloseDiff<-c(0,diff(stockTemp$Close,lag=1))


#To find returns (CloseDiff/PreviousClose) (1st row will be 1 and not 0)
closeVector<-c(1,stockTemp$Close[1:n-1])
closeVector

stockTemp$Sreturn<-stockTemp$CloseDiff/closeVector

# To find logreturns 
stockTemp$logReturns <- c(0, diff(log(stockTemp$Close), lag = 1))

head(stockTemp)

#To plot Line graoh of current close index wrt date 
a<-ggplot(data=stockTemp,aes(x=DateNew,y=Close))+
  geom_line(color="blue")+
  scale_x_datetime(date_breaks = "5 day",date_labels = format("%b %d"))+
  labs(title=paste("Nifty Index from ",stockTemp$DateNew[1]," to ",stockTemp$DateNew[n]),x="Date",y="Index")+
  theme_classic()

#To plot returns and log returns as function of time 
b<-ggplot(data=stockTemp,aes(x=DateNew,y=Sreturn))+
  geom_line(color="blue")+
  scale_x_datetime(date_breaks = "5 day",date_labels = format("%b %d"))+
  labs(title=paste("Nifty returns from ",stockTemp$DateNew[1]," to ",stockTemp$DateNew[n]),x="Date",y="Returns")+
  theme_classic()

c<-ggplot(data=stockTemp,aes(x=DateNew,y=logReturns))+
  geom_line(color="red")+
  scale_x_datetime(date_breaks = "5 day",date_labels = format("%b %d"))+
  labs(title=paste("Nifty log returns from ",stockTemp$DateNew[1]," to ",stockTemp$DateNew[n]),x="Date",y="logReturns")+
  theme_classic()

grid.arrange(a,b,c)





