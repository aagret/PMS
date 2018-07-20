
# test PMS

#open exel trade historyx

library(readxl)
library(data.table)
library(plyr)

trades <- setDT(read_xlsx("FundClientTrades.xlsx"))
colnames(trades) <- c("Port","Ticker","Trade","Quantity","Cost","Date")

trades <- trades[Port == "DEQ",]
trades[, Date:=as.Date(as.character(Date), format="%Y%m%d")]
setkey(trades, Date, Ticker)

p0 <-  trades[Date == min(Date), .(Date, Ticker, Quantity, Cost)]

#t1 <- trades[Date == min(Date[Date != min(Date)]), ]

#t1 <- averagePosition(t1)

#p1 <- updatePortfolio(p0, t1)

newPort <- p0
newTrades <- trades[!Date %in% newPort$Date, ]

newTrades <- averagePosition(newTrades)



newPort <- list()
newPort[[1]] <- p0
for (x in 2:length(unique(newTrades$Date))) {
    
    dt <- unique(newTrades$Date)[x - 1]
    
    
    newPort[[x]]<- updatePortfolio(newPort[[x - 1]],
                                   newTrades[Date == dt, ])
}

# calc new portfolio

updatePortfolio <- function(port= portfolio, trad= trades) {
    
    newPort <- rbind(port[!Ticker %in% trad$Ticker,],
                     trad[!Ticker %in% port$Ticker, .(Date, Ticker, Quantity, Cost)])
    
    update  <- rbind(port[Ticker %in% trad$Ticker,],
                     trad[Ticker %in% port$Ticker, .(Date, Ticker, Quantity, Cost)])
    
           
    update [, Date:= unique(trad$Date)]
    newPort[, Date:= unique(trad$Date)]
    
    setkey(update, Date, Ticker)
    update <- averagePosition(update)
    
    newPort <- rbind(newPort, update)
    
    setkey(newPort, Date, Ticker)
    
    return(newPort)
    
}

# si multi daily trades il faut changer de logique et moyener les trades avant fusion


averagePosition <- function(trad= trades) {
    
    single <- trad[, .SD[.N == 1], by= .(Date, Ticker)]
    multi  <- trad[, .SD[.N != 1], by= .(Date, Ticker)]
    
    #average cost and sum postion
    multi[, ':=' (Cost= sum(Quantity[Quantity > 0] * Cost[Quantity > 0]) / sum(Quantity[Quantity > 0]),
                  Quantity= sum(Quantity))
          , by= .(Date, Ticker)]
    
    multi <- rbind(single, unique(multi))
    
    setkey(multi, Date, Ticker)
}
