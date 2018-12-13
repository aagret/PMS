
# duplicate portfolios positions for all available dates
getDailyPositions <- function(port= portfolios, px= prices) {
    
    pos0 <- port[Date == min(Date), ]
                  
    positions <- list()
    positions[[1]] <- copy(pos0)
    
    for (x in 2:length(unique(px$Date))) {
        
        dt  <- unique(px$Date)[x]
        
        if (nrow(port[Date == dt,]) == 0) {
            
            positions[[x]] <- copy(positions[[x-1]])
            positions[[x]][ , Date:= dt]
            positions[[x]][ , c("Trade", "Quantity", "Price"):= NA]
            
        } else {
            
            positions[[x]] <- port[Date==dt, ]
        
        }


    }
    
    positions <- unique(rbindlist(positions))
    
    # remove repeated empty positions
    positions <- rbind(positions[Position !=0, ],
                       positions[Position ==0, .SD[1], by= Ticker])
    
    setkey(positions, Date, Ticker)
    
    positions[Position == 0, .SD[1], by= Ticker]
    
    # merge with prices
    positions <- px[positions]
    
    # calc positions value for NAV
    positions[ , Value:= Position * Last * Factor]
    
    positions[ , EurValue:= Value * Bid]
    
    # calc NAV 
    positions[, NAV:= sum(EurValue), by= Date]
    
    # calcposition weithg
    positions[ , Wght:= EurValue / NAV * 100]
    
    setkey(positions, Date, Ticker)

    
}
