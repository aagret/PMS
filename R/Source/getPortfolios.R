
# construct portolio on each trade date
getPortfolios <- function(trad= trades) {
    
    # get portfolio changes on each ctrade date
    port <- trades[, .SD[.N], by= .(Ticker, Date)]
    
    # extract initial portfolio
    p0 <- port[Date == min(Date),]# .(Date, Port, Ticker, Position)]
    
    # construct daily portfolio
    newPort <- list()
    newPort[[1]] <- copy(p0)
    
    for (x in 2:length(unique(port$Date))) {
        
        dt  <- unique(port$Date)[x]
        old <- newPort[[x-1]][!Ticker %in% port[Date== dt, Ticker]]
        old[, Date:= dt]
        old[,  c("Trade", "Quantity", "Price"):= NA ]
        
        newPort[[x]] <- rbind(old, port[Date == dt, ])
       
    }
    
    newPort <- rbindlist(newPort)

    # remove repeated empty positions
    newPort <- rbind(newPort[Position !=0, ],
                     newPort[Position ==0, .SD[1], by= Ticker])
    
    newPort <- unique(newPort)
    
    setkey(newPort, Date, Port, Ticker)

}
    
