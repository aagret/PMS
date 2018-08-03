
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
        
        data.table(Position, Cost, PnL)
        
    }
    
    data.table(Position, Cost, PnL)
    
}

#open exel trade history
library(readxl)
library(data.table)
library(plyr)

trades <- setDT(read_xlsx(path = "/home/Alexandre/DEQ_Trades/FundClientTrades.xlsx"))[,1:6]
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

prices <- bdh(tic, "PX_LAST", start)
