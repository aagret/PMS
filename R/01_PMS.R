

#open exel trade history
library(readxl)
library(data.table)
library(plyr)
library(TTR)
library(Rblpapi)

# intit
codeDir <- "C:/Users/bloomberg/Documents/PMS/"

source(paste0(codeDir, "R/PmsFunctions.R"))

#read and format trades from xls file
trades <- getTrades()

# add POSITION
trades[, Position:= cumsum(Quantity), by= .(Ticker)]

# get portfolio positions on each trade date
portfolios <- getPortfolios(trades)

# get most recent portfolio
#pn <- portfolios[Date == max(Date),]

# get historical prices and classification for eaach security
prices <- getHistoPrices(portfolios)

# get all portfolio postions on every avilable date
positions <- getDailyPositions(portfolios, prices)


# add perf log returns
setkey( positions, Date, Cur)
positions[, d.Bid:= c(0, ROC(Bid)[-1]), by= Date]
          
setkey( positions, Date, Ticker)
positions[, ':=' (d.Last= c(0, ROC(Last)[-1]),
                  d.Trade= 0),
          by= Ticker]

# calc pnl on trade (log)
positions[Trade %in% c("Purchase", "Sale"), d.Trade:= 
              log(1 + 
                      ((Last - Price) * 
                           Quantity * Bid * Factor * 
                           Wght / NAV)),
          by= Ticker]

# 
# # positions weight to total portfolio
# positions[, Ctr:= Wght * (d.Bid + d.Last) / 100,
#           by= Ticker]




#** TEST *****
dt1 <- as.Date("2018-01-01")
dt2 <- as.Date("2018-08-21")
sector <- "Information Technology"


select <- copy(positions[Date >= max(Date[Date <= dt1]) & 
                             Date <= dt2 &
                             GicsSector == sector,])

plot(select[, 
            .(Date, exp(cumsum(d.Bid + d.Last + d.Trade)))],
     type="l")





