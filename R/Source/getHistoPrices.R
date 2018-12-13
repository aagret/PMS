
# function to retrieve istorical prices for 
# all securities from first trade date

getHistoPrices <- function(port= portfolios) {
    
    tic   <- unique(port$Ticker)
    start <- min(unique(port$Date))
    end   <- Sys.Date() - 1
    
    con <- blpConnect()
    
    # get histo prices
    opt = c("nonTradingDayFillOption" = "NON_TRADING_WEEKDAYS",
            "nonTradingDayFillMethod" = "PREVIOUS_VALUE")
    
    prices <- ldply(bdh(tic, "PX_LAST", start, end,
                        include.non.trading.days = FALSE,
                        options= opt)) 
    
    colnames(prices) <- c("Ticker","Date","Last")
    
    setDT(prices)
    setkey(prices, Ticker)
    
    # get securities description
    des    <- cbind(tic, 
                    bdp(tic, c("CRNCY", "COUNTRY", "INDUSTRY_SECTOR", 
                               "GICS_SECTOR_NAME", "GICS_INDUSTRY_GROUP_NAME", 
                               "GICS_INDUSTRY_NAME",
                               "PX_POS_MULT_FACTOR")))
    
    colnames(des) <- c("Ticker", "Cur", "Country", "Sector", 
                       "GicsSector", "GicsGroup", "GicsSubGroup", 
                       "Factor")
    
    setDT(des)
    des[, Ticker:=as.character(Ticker)]
    setkey(des, Ticker)
    
    # merge prices and des
    prices <- des[prices]
    prices[Cur == "GBp", Factor:= Factor / 100]
    prices[, Cur:= toupper(Cur)]
    

    
    
    # get historical Fx rates for each currencies in Portfolios 
    tic    <- unique(paste0(toupper(as.character(des$Cur)) , "EUR Curncy"))
    fx     <- ldply(bdh(tic, "BID", start, end,
                        include.non.trading.days = FALSE,
                        options= opt)) 
    
    blpDisconnect(con)
    
    colnames(fx) <- c("Cur", "Date", "Bid")
    setDT(fx)
    
    fx[, Cur:= gsub("EUR Curncy", "", Cur)]

    
    
    # merge fx with prices
    setkey(fx, Date, Cur)
    setkey(prices, Date, Cur)
    
    prices <- fx[prices]
    prices[Cur == "EUR", Bid:= 1L]
    
    # remove market closed dates
    prices <- prices[complete.cases(prices)]
    
    setkey(prices, Date, Ticker)
    
}
