
# test PMS

##Functions
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


#open exel trade history
library(readxl)
library(data.table)
library(plyr)

trades <- setDT(read_xlsx(path = "/home/Alexandre/DEQ_Trades/FundClientTrades.xlsx"))
colnames(trades) <- c("Port","Ticker","Trade","Quantity","Cost","Date")

trades <- trades[Port == "DEQ",]
trades[, Date:=as.Date(as.character(Date), format="%Y%m%d")]
trades[Trade != "SL", Trade:= "Purchase"]
trades[Trade == "SL", Trade:= "Sale"]
trades[Trade == "Sale", Quantity:= -Quantity]

setkey(trades, Date, Ticker)

# read trades from Olis
# olis <- fread("tradesFromOlis.csv")[, c(5, 2, 7, 10, 13)]
# colnames(olis) <- c("Date", "Ticker", "Trade","Quantity","Cost")
# 
# olis <- olis[Trade %in% c("Sale","Purchase"),]
# olis[, ':=' (Date= as.Date(Date, "%d.%m.%Y"),
#              Port="DEQ",
#              Quantity= as.numeric(gsub("'", "", Quantity)))]
# 
# setcolorder(olis, c(colnames(trades)))
# 
# olis <- unique(olis)
# 
# #load isin database
# isin <- fread("/home/artha/R-Projects/BBU_upload/tickerBloomDEQ.csv", header = TRUE)[, 2:3]
# colnames(isin) <- c("Ticker","Code")
# setkey(isin, Ticker)
# setkey(olis, Ticker)
# 
# olis <- isin[olis]
# olis[, Ticker:= Code]
# olis[grep(" LN Equity", Ticker), Cost:= Cost * 100]
# olis$Code <- NULL
# setkey(olis, Date, Ticker)


# read trades from Olis BBU
# bbu <- fread("tradesFromBBU.csv")[, c(1,2,6,7)]
# bbu[, ':=' (Date= as.Date(Date, "%Y-%m-%d"),
#             Port= "DEQ",
#             Quantity= Trade)]
# bbu[, Trade:=ifelse(Quantity < 0, "Sale", "Purchase")]
# bbu[Trade == "Sale", Quantity:= -Quantity]
# colnames(bbu) <- c("Date", "Ticker", "Trade", "Cost", "Port", "Quantity")
# 
# 
# setkey(bbu, Ticker)
# 
# bbu <- isin[bbu]
# 
# bbu[, Ticker:= Code]
# bbu$Code <- NULL
# bbu[grep(" LN Equity", Ticker), Cost:= Cost * 100]
# setcolorder(bbu, c(colnames(trades)))
# setkey(bbu, Date, Ticker)




#average position and prices if multi trades per security
trades <- averagePosition(trades)





# reconstruct portfolio from trades

# get initial portfolio on first day of existence
p0 <- trades[Date == min(Date), .(Date, Ticker, Quantity, Cost)]

# remove inital portfolio from trades database
newTrades <- trades[!Date %in% p0$Date, ]

# average position and prices if multi trades per security
newTrades <- averagePosition(newTrades)

# average position and prices if multi trades per security
newTrades <- averagePosition(newTrades)

# retrieve all portfolios
newPort <- list()
newPort[[1]] <- p0
for (x in 1:length(unique(newTrades$Date))) {
    
    dt <- unique(newTrades$Date)[x]
    
    
    newPort[[x+1]]<- updatePortfolio(newPort[[x]],
                                   newTrades[Date == dt, ])
}

newPort <- rbindlist(newPort)
pn <- newPort[Date == max(Date) & Quantity != 0,]

