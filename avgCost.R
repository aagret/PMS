
trades <- data.frame(Date=c(1,2,2,3,4), Trade=c("B","B","S","B","S"), Quantity=c(1,1,1,2,4), Price=c(100,110,112,112,115))

setDT(trades, key = "Date")

trades[, Quantity:= ifelse(Trade=="B", Quantity, -Quantity)]






trades[, Position:= cumsum(Quantity)]


# check if trade generate PnL
check <- !(sign(shift(trades$Position)) + sign(trades$Position) == 0) %in% FALSE


trades[, Cost:=Price]
for (n in 2:nrow(trades)) {
    
    if (!check[n] & (sign(trades$Position[n-1]) == sign(trades$Quantity[n]))) {
        
        trades$Cost[n] <- 
            ((trades$Cost[n-1] * trades$Position[n-1]) +
                 (trades$Cost[n] * trades$Quantity[n])) /
            trades$Position[n]
    }
    
    if (!check[n] & (sign(trades$Position[n-1]) != sign(trades$Quantity[n]))) {
        
        trades$Cost[n] <- 
            trades$Cost[n-1]

    }
    
    if (sign(trades$Position[n-1]) != sign(trades$Quantity[n]))

        trades$PnL[n] <-
            min(abs(trades$Quantity[n]), abs(trades$Position[n-1])) *
            ifelse(trades$Quantity[n] < 0, -1 , 1) *
            (trades$Cost[n-1] - trades$Price[n])

}


#trades[is.nan(Cost), Cost:=0]
trades[is.na(PnL),   PnL:=0]





trades[, Cost:= avgTrades(trades)]


avgTrades <- function(trad= trades) {

    Q <- trades[, Quantity]
    P <- trades[, Price]
    
    trades[, Position:= cumsum(Q)]
    
    check <- !(sign(shift(Position)) + sign(Position) == 0) %in% FALSE
    
    trades[, Cost:=P]    
    
    for (n in 2:nrow(Q)) {
        
        if (!check[n] & (sign(Position[n-1]) == sign(Q[n]))) {
            
            Cost[n] <- 
                ((Cost[n-1] * Position[n-1]) +
                     (Cost[n] * Q[n])) /
                Position[n]
        }
        
        if (!check[n] & (sign(Position[n-1]) != sign(Q[n]))) {
            
            Cost[n] <- 
                Cost[n-1]
            
        }
        
        if (sign(Position[n-1]) != sign(Q[n]))
            
            PnL[n] <-
                min(abs(Q[n]), abs(Position[n-1])) *
                ifelse(Q[n] < 0, -1 , 1) *
                (Cost[n-1] - P[n])
        
    }
    
    #trades[is.nan(Cost), Cost:=0]
    trades[is.na(PnL),   PnL:= 0]
    
}
    
    
