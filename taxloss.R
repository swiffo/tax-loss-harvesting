library(ggplot2)
library(dplyr)

SPY <- read.csv('http://ichart.finance.yahoo.com/table.csv?s=SPY&a=1&b=1&c=2005&g=d&ignore=.csv')
SPY$Date <- as.Date(as.character(SPY$Date))

sorted <- arrange(SPY, Date)

base <- sorted$Adj.Close
present <- base[2:length(base)]
dates <- sorted$Date[2:length(base)]
previous <- base[1:length(base)-1]

closeLevels <- data.frame(Date=dates, Close=present, PrevClose=previous)

closeLevels <- ( closeLevels %>% mutate(
  Delta=present - previous,
  PosDelta=pmax(Delta,0),
  NegDelta=pmin(Delta,0)
  ) )

total <- (closeLevels %>% group_by() %>% summarize(PosDelta=sum(PosDelta), NegDelta=sum(NegDelta)))
