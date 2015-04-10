library(reshape)
library(sp)

conn <- RSQLite::dbConnect(SQLite(), "restaurants.db")

wa.map <- rgdal::readOGR("zillow/ZillowNeighborhoods-WA.shp", layer="ZillowNeighborhoods-WA")
my.sub <- wa.map[wa.map$CITY == "Seattle", ]

res <- RSQLite::dbGetQuery(conn, "SELECT * FROM Restaurants")

df.points <- res[c('Latitude', 'Longitude')]
df.points$Latitude <- as.numeric(df.points$Latitude)
df.points$Longitude <- as.numeric(df.points$Longitude)
df.points$Name <- res$Name

sp::coordinates(df.points) <- ~ Longitude + Latitude 

all.matches <- c()
for(hood in my.sub$NAME){
  temp <- wa.map[wa.map$CITY == "Seattle" & wa.map$NAME == hood, ]
  proj4string(df.points) <- proj4string(temp)
  temp.over <- over(df.points, temp)
  
  temp.match <- cbind(res, temp.over)
  temp.match <- temp.match[complete.cases(temp.match$NAME), ]
  if(nrow(temp.match) == 0){
    next
  }
  temp.match$neighborhood <- hood
  all.matches <- rbind(all.matches, temp.match)
} 

#then calculate statistics
all.counts <- plyr::ddply(all.matches, c('neighborhood', 'NAICStype', 'entrydate'), function(x) data.frame(count=length(unique(x$Name))))
all.diffs <- plyr::ddply(all.counts, c('neighborhood', 'NAICStype'), function(x) data.frame(absdiff=sum(diff(x$count))))

all.diffs.wide <- melt(all.diffs, id.vars=c('neighborhood','NAICStype'))
all.diffs.wide$variable <- NULL

#Get lookup values for all neighborhoods
lookup <- data.frame(FIPS=row.names(my.sub),
                     neighborhood=my.sub$NAME)
lookup[] <- sapply(lookup, as.character)

total.changes <- cast(all.diffs.wide, neighborhood ~ NAICStype)
names(total.changes) <- c("neighborhood","Breweries_change", "MFS_change",
                          "DP_change", "FSR_change", "LSR_change")
total.changes <- merge(total.changes, lookup, all.y=T)

#Then get total counts for each type
raw.counts <- plyr::ddply(all.matches, c('neighborhood', 'NAICStype'), function(x) data.frame(count=length(unique(x$Name))))
total.counts <- cast(raw.counts, neighborhood ~ NAICStype)
total.counts <- merge(total.counts, lookup, all.y=T)
names(total.counts) <- c("neighborhood","Breweries_total", 
                          "MFS_total", "DP_total", "FSR_total",
                          "LSR_total", "FIPS")

writeout <- merge(total.counts, total.changes)
writeout[is.na(writeout)] <- 0
write.csv(writeout, "data/sncounts.csv", row.names=F)

RSQLite::dbDisconnect(conn)
