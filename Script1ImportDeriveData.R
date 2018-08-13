# Read stock data from csv
stock<-read.csv("July1Aug12018.csv",stringsAsFactors = F)
head(stock)

str(stock)

# Remove traded volume column and change date to Posixct format for ease
stockTemp<-stock[,c(-6,-7)]

stockTemp$DateNew<-as.POSIXct(stockTemp$Date,format="%d-%b-%Y")


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





