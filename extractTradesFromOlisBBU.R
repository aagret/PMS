
# get all trades based on positions



trades <- unique(allPositions[Category == "VMOB", .(Date, Code, Description, Quantity, BaseAmount)])
setkey(trades, Date, Code)
trades[, Trade:=c(0, diff(Quantity)), by= Code]


trades[, Trade:=replace(Trade, 1, Quantity), by= Code]
all <- trades
trades <- trades[Trade !=0, ]

trades[, Price:= c(0, diff(BaseAmount)) / Trade , by= Code]
trades[Trade == Quantity, Price:= BaseAmount / Quantity , by= Code]


fwrite(trades, "tradesFromXls.csv")

#terst
ultimateTrade <- trades[, min(all$Date[all$Date > max(Date)]), by=Code]
all[, min(all$Date[all$Date > max(Date)]), by= Code]

	
