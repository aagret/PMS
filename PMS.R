
# test PMS

#open exel trade historyx

library(readxl)
library(data.table)

trades <- setDT(read_xlsx("FundClientTrades.xlsx"))
colnames(trades) <- c("Port","Ticker","Trade","Quantity","Cost","Date")

trades <- trades[Port == "DEQ",]
trades[, Date:=as.Date(as.character(Date), format="%Y%m%d")]
setkey(trades, Date, Ticker)

p0 <-  trades[Date == min(Date), .(Date, Ticker, Quantity, Cost)]

t1 <- trades[Date == min(Date[Date != min(Date)]), ]



# calc new portfolio

updatePortfolio <- function(port= portfolio, trad= trades) {
    
    newPort <- rbind(port[!Ticker %in% trad$Ticker,],
                     trad[!Ticker %in% port$Ticker, .(Date, Ticker, Quantity, Cost)])
    
    update  <- rbind(port[Ticker %in% trad$Ticker,],
                     trad[Ticker %in% port$Ticker, .(Date, Ticker, Quantity, Cost)])
    
    setkey(update, Date, Ticker)
           
    
    update[, ':=' (Date= unique(trad$Date),
                   Cost= sum(Quantity[Quantity > 0] * Cost[Quantity > 0]) / sum(Quantity[Quantity > 0]),
                   Quantity= sum(Quantity))
           , by= Ticker]


    newPort <- rbind(newPort, unique(update))
    
    setkey(newPort, Date, Ticker)
    
    return(newPort)
    
}


p1 <- updatePortfolio(p0, t1)