
trades <- data.frame(Date=c(1,2,3,4,5), Trade=c("B","B","S","B","S"), Quantity=c(1,1,1,2,4), Price=c(100,110,112,112,115))

setDT(trades, key = "Date")

trades[, Quantity:= ifelse(Trade=="B", Quantity, -Quantity)]

trades[, Position:= cumsum(Quantity)]


# check if trade change Position
check <- !(sign(shift(trades$Position)) + sign(trades$Position) == 0) %in% FALSE

# trades[check, ':=' (Cost= Price,
#                     PnL= 0)]

trades[, Cost:= Price]
trades[, Cost:=ifelse(check, Price,
                      shift(Cost * Position) + (Cost * Quantity)) / Position]
