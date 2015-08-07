library(RSQLite)
library(dplyr)

conn <- dbConnect(SQLite(), "restaurants.db")

res.dates <- dbGetQuery(conn, "SELECT * FROM Dates") %>% distinct

total.counts <-
  all.counts %>%
  group_by(NAICStype, Entrydate) %>%
  do(data.frame(count=sum(.$count)))

netchange.counts <-
  total.counts %>%
  group_by(NAICStype) %>%
  do(data.frame(Entrydate=min(.$Entrydate)))
netchange.counts <- merge(netchange.counts, total.counts)

names(netchange.counts)[3] <- "startcount"
netchange.counts$Entrydate <- NULL
netchange.counts <- merge(total.counts, netchange.counts, all.x=T)  
netchange.counts$diff <- netchange.counts$count - netchange.counts$startcount

netchange.counts <- netchange.counts[c('Entrydate', 'NAICStype', 'diff')]
all.diffs.wide <- reshape(netchange.counts, idvar = 'Entrydate', timevar='NAICStype', direction='wide')

all.diffs.wide <-
  all.diffs.wide %>%
  mutate(Entrydate = as.Date(Entrydate)) %>%
  arrange(Entrydate)
all.diffs.wide[is.na(all.diffs.wide)] <- 0
names(all.diffs.wide) <- c("date","Breweries","Mobile Food Services","Drinking Places",
                           "Full-Service Restaurants","Limited-Service Restaurants")
  
write.csv(all.diffs.wide, "data/netchanges.csv", row.names=F)
