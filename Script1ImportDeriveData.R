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

ggplot(data=stockTemp,aes(x=DateNew,y=Close))+
  geom_line(color="blue")+
  scale_x_datetime(date_breaks = "5 day",date_labels = format("%b %d"))+
  labs(title="Nifty direction for 1 month",x="Date",y="Index")+
  theme_classic()

