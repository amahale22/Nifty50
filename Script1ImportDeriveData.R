#Data when imported from csv---Start
# Read stock data from csv
stock<-read.csv("July1Aug12018.csv",stringsAsFactors = F)
head(stock)

str(stock)

# Remove Turnover column and change date to Posixct format for ease
stockTemp<-stock[,c(-7)]
stockTemp$DateNew<-as.POSIXct(stockTemp$Date,format="%d-%b-%Y")
#Data when imported from csv---End

#Use quantmod to get symbols data (Check for NA and remove if all values are missing and market was actually closed on that date)
library(dplyr)
library(quantmod)
startDate<-"2017-08-01"
endDate<-"2018-09-19"
getSymbols("^NSEI",from=startDate,to=endDate) #NIFTY50  INDIA
getSymbols("^GSPC",from=startDate,to=endDate) #S&P500  United States
getSymbols("^IXIC",from=startDate,to=endDate) #NASDAQ  United States
getSymbols("^N225",from=startDate,to=endDate) #NIKKEI 225 -JAPAN
getSymbols("^HSI",from=startDate,to=endDate) #HANGSENG -HONG KONG
getSymbols("^FTSE",from=startDate,to=endDate) #Financial Times SE -LONDON 
getSymbols("INR=X",from=startDate,to=endDate) #USD/INR

head(NSEI)
head(GSPC)
head(IXIC)
head(N225)
head(FTSE)
head(HSI)
head(`INR=X`)
#NSEI<-NSEI[complete.cases(NSEI),]

#Get all symobols together
getSymbols(c("^NSEI","^GSPC","^IXIC","^N225","^HSI","^FTSE","INR=X"),from=startDate,to=endDate) #USD/INR


#Merge all xts to form master data
x1<-merge(NSEI,GSPC,join="inner")
x2<-merge(x1,IXIC,join="inner")
x3<-merge(x2,N225,join="inner")
x4<-merge(x3,HSI,join="inner")
x5<-merge(x4,FTSE,join="inner")
x6<-merge(x5,`INR=X`,join="inner")


head(x6)
summary(x6)
dim(x6)

dim(x6[!complete.cases(x6),]) #NAs found in data

#Either approximate, Remove NAs (Use zoo function na.locf (Last observation carried forward method))
x7<-na.locf(x6)
dim(x7[!complete.cases(x7),]) #NAs removed


# # To derive new column of PrevClose and PrevOpen (Always 1st row of stock will have nothing to comapre so 0)
# stockTemp$PrevOpen<-c(0,stockTemp$Open[1:n-1])
# stockTemp$PrevClose<-c(0,stockTemp$Close[1:n-1])

# To derive difference between Current and Previous Close
# diff is an imp function here directly finds diff between 2 rows like 2nd row -1st row and so on
n<-nrow(x7)
x7$NSEI.CloseDiff<-diff(x7$NSEI.Close,lag=1)
x7$NSEI.CloseDiff[1]<-0


#To find returns (CloseDiff/PreviousClose) (1st row will be 1 and not 0)
NSEIcloseVector<-c(1,x7$NSEI.Close[1:n-1])
NSEIcloseVector

x7$NSEI.Return<-x7$NSEI.CloseDiff/NSEIcloseVector

# To find logreturns 
x7$NSEI.LogReturn <- diff(log(x7$NSEI.Close), lag = 1)
x7$NSEI.LogReturn[1]<-0

head(x7)

dim(x7)

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





