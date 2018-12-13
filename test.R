
avg <- function(Q= Quantity, P= Price) {
    
    Position <- cumsum(Q)
    Cost     <-  P
    PnL      <- rep(0, length(Cost))
    
    if (length(Q) != 1) {
        
        # check if trade generate PnL
        check <- !(sign(shift(Position)) + sign(Position) == 0) %in% FALSE
        
        for (n in 2:length(Position)) {
            
            if (!check[n] & (sign(Position[n-1]) == sign(Q[n]))) {
                
                Cost[n] <- 
                    ((Cost[n-1] * Position[n-1]) +
                         (Cost[n] * Q[n])) /
                    Position[n]
            }
            
            if (!check[n] & (sign(Position[n-1]) != sign(Q[n]))) {
                
                Cost[n] <- Cost[n-1]
                
            }
            
            if (sign(Position[n-1]) != sign(Q[n]))
                
                PnL[n] <-
                    min(abs(Q[n]), abs(Position[n-1])) *
                    ifelse(Q[n] < 0, -1 , 1) *
                    (Cost[n-1] - P[n])
            
        }
     
        #trad[is.nan(Cost), Cost:=0]
        PnL[is.na(PnL)] <- 0
        PnL <- cumsum(PnL)
        
        #data.table(round(Position, 2), round(Cost, 2), round(PnL,2))
        
    }
    
    data.table(round(Position, 4), round(Cost, 4), round(PnL,2))
    
}

#open exel trade history
library(readxl)
library(data.table)
library(plyr)
library(TTR)

trades <- setDT(read_xlsx(path = "m:/Alexandre/DEQ_Trades/FundClientTrades.xlsx"))[,1:6]
colnames(trades) <- c("Port","Ticker","Trade","Quantity","Price","Date")

trades <- trades[Port == "DEQ",]
trades[, Date:=as.Date(as.character(Date), format="%Y%m%d")]
trades[Trade != "SL", Trade:= "Purchase"]
trades[Trade == "SL", Trade:= "Sale"]
trades[Trade == "Sale", Quantity:= -Quantity]

setkey(trades, Date, Ticker)


# calc average prices & trade P&L
trades[, c("Position", "Cost","PnL"):= avg(Quantity, Price), by= Ticker]

# create portfolio changes
port <- trades[, .SD[.N], by= .(Ticker, Date)][ , .(Date, Ticker, Position, Cost, PnL)]

# get initial portfolio
p0 <- port[Date == min(Date), .(Date, Ticker, Position, Cost, PnL)]

# construct daily portfolio
newPort <- list()
newPort[[1]] <- copy(p0)

for (x in 2:length(unique(port$Date))) {
    
    dt  <- unique(port$Date)[x]
    old <- newPort[[x-1]][!Ticker %in% port[Date== dt, Ticker]]
    old[, Date:= dt]
    
    newPort[[x]] <- rbind(old, port[Date == dt,])
    
}

newPort <- rbindlist(newPort)

# remove repeated empty positions
newPort <- rbind(newPort[Position !=0,],
           newPort[Position ==0, .SD[1], by= Ticker])

setkey(newPort, Date, Ticker)

# get most recent portfolio
pn <- newPort[Date == max(Date),]


# get historical data

library(Rblpapi)

tic   <- unique(newPort$Ticker)
start <- min(unique(newPort$Date))

con <- blpConnect()

prices <- ldply(bdh(tic, "PX_LAST", start, Sys.Date() -1))
colnames(prices) <- c("Ticker","Date","Last")
setDT(prices)
setkey(prices, Ticker)

cur    <- cbind(tic, bdp(tic, "CRNCY"))
colnames(cur) <- c("Ticker", "Cur")
setDT(cur)
cur[, Ticker:=as.character(Ticker)]
setkey(cur, Ticker)

prices <- cur[prices]
prices[, Cur:= toupper(Cur)]

prices <- prices[complete.cases(prices)]

tic    <- unique(paste0(toupper(as.character(cur$Cur)) , "EUR Curncy"))
fx     <- ldply(bdh(tic, "BID", start))

blpDisconnect(con)

colnames(fx) <- c("Cur", "Date", "Bid")
setDT(fx)

fx[, Cur:= gsub("EUR Curncy", "", Cur)]
#fx[Cur == "GBP", Cur:= "GBp"]

setkey(fx, Date, Cur)
setkey(prices, Date, Cur)

prices <- fx[prices]
prices[Cur == "EUR", Bid:= 1L]



#lastPrice <- prices[, .SD[.N], by= Ticker]
#prevPrice <- prices[, .SD[.N-1], by= Ticker]

setkey(prices, Date, Ticker)
prices[, d.Last:= c(0, ROC(Last)[-1]), by= Ticker]
prices[, d.Bid:=  c(0, ROC(Bid)[-1]),  by= Ticker]

dts <- unique(prices$Date)

pos <- list()

for (i in 1:length(dts)) {
    
    if (nrow(newPort[Date == dts[i],]) == 0) {
        pos[[i]] <- pos[[i-1]]
    } else{
        pos[[i]] <- newPort[Date == dts[i], ]
    }
   
    pos[[i]]$Date <- dts[i]
}
   
pos <- rbindlist(pos)
setkey(pos, Date, Ticker)

pos <- prices[pos]


pos[ , Value:= Position * Last]
pos[Cur %in% c("GBP", "GBp"), Value:= Value / 100]
pos[grepl("Index", Ticker), Value:= Value * 50]
pos[ ,EurValue:= Position * Last * Bid]

pos[grepl("Index", Ticker), ':=' (Value= Value* 50,
                                EurValue= EurValue *50)]

pos[ , Wght:= EurValue / sum(EurValue, na.rm= TRUE), by= Date]
    
pos[, Ctr:= (d.Last * d.Bid) * Wght]

#exp(cumsum(pos[, sum(Ctr, na.rm=TRUE), by= Date]$V1))
