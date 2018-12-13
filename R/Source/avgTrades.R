
# function to average Position, Cost and trade P&L
avgTrades <- function(Q= Quantity, P= Price) {
    
    Position <- cumsum(Q)
    Cost     <- P
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
            
            if (Position[n-1] == 0) Cost[n] <- P[n]  
            
        }
        
        #trad[is.nan(Cost), Cost:=0]
        PnL[is.na(PnL)] <- 0

        }
    
    data.table(round(Position, 4), round(Cost, 4), round(PnL,2))
    
}
