
dt <- as.Date("2018-07-26")

select <- newPort[Date == max(Date[Date <= dt]),]
select[, Date:= dt]
setkey(select, Date, Ticker)

s <- prices[select]




