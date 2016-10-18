IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
Boeing = read.csv("BoeingStock.csv")

# 1.1
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")
str(IBM)

# 1.2 - 1.4
summary(IBM)

# 1.5
summary(GE)

# 1.6
summary(CocaCola)

#.1.7
summary(Boeing)

# 1.8
sd(ProcterGamble$StockPrice)

# 2.1
plot(CocaCola$Date, CocaCola$StockPrice)
plot(CocaCola$Date, CocaCola$StockPrice, type="l")

# 2.2 - 2.3
lines(ProcterGamble$Date, ProcterGamble$StockPrice)
plot(CocaCola$Date, CocaCola$StockPrice, type="l", col ="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue", lty=2)
abline(v=as.Date(c("2000-03-01")), lwd=2)

# 3.1 - 3.2
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210), xlab = "Year",ylab = "Stock Price")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="blue")
lines(GE$Date[301:432], GE$StockPrice[301:432], col="green")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="black")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="orange")

# 3.3
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-30")), lwd=2)

# 3.4
abline(v=as.Date(c("2004-01-01")), lwd=2)
abline(v=as.Date(c("2005-12-31")), lwd=2)

# 4.1
tapply(IBM$StockPrice, months(IBM$Date), mean)

# 4.2 - 4.3
tapply(GE$StockPrice, months(GE$Date), mean)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean)
tapply(Boeing$StockPrice, months(Boeing$Date), mean)