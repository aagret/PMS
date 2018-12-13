
# function to read trades from xls file
getTrades <- function() {
    
    # read xls file and name columns
    trades <- setDT(read_xlsx(path = "m:/Alexandre/DEQ_Trades/FundClientTrades.xlsx"))[,1:6]
    colnames(trades) <- c("Port","Ticker","Trade","Quantity","Price","Date")
    
    # only keep "DEQ" portfolio (for now)
    trades <- trades[Port == "DEQ",]
    
    # format data
    trades[, Date:=as.Date(as.character(Date), format="%Y%m%d")]
    trades[Trade != "SL", Trade:= "Purchase"]
    trades[Trade == "SL", Trade:= "Sale"]
    trades[Trade == "Sale", Quantity:= -Quantity]
    
    setkey(trades, Date, Port, Ticker)
    
}
