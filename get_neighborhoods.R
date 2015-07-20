library(reshape)
library(sp)
library(RSQLite)
library(dplyr)
library(grid)
library(RColorBrewer)

conn <- dbConnect(SQLite(), "restaurants.db")

wa.map <- rgdal::readOGR("zillow/ZillowNeighborhoods-WA.shp", layer="ZillowNeighborhoods-WA")
my.sub <- wa.map[wa.map$CITY == "Seattle", ]

res.addresses <- dbGetQuery(conn, "SELECT * FROM Addresses") %>% distinct
res.dates <- dbGetQuery(conn, "SELECT * FROM Dates") %>% distinct

res <- merge(res.addresses, res.dates)

df.points <- 
  res %>% 
  select(Name, Latitude, Longitude) %>%
  distinct

df.points$Latitude <- as.numeric(df.points$Latitude)
df.points$Longitude <- as.numeric(df.points$Longitude)

sp::coordinates(df.points) <- ~ Longitude + Latitude 

all.matches <- c()
for(hood in my.sub$NAME){
  temp <- wa.map[wa.map$CITY == "Seattle" & wa.map$NAME == hood, ]
  proj4string(df.points) <- proj4string(temp)
  temp.over <- over(df.points, temp)
  
  temp.match <- cbind(df.points, temp.over)
  temp.match <- temp.match[complete.cases(temp.match$NAME), ]
  if(nrow(temp.match) == 0){
    next
  }
  temp.match$neighborhood <- hood
  all.matches <- rbind(all.matches, temp.match)
} 

#then calculate statistics
all.matches <- merge(res, all.matches)
all.counts <- plyr::ddply(all.matches, c('neighborhood', 'NAICStype', 'Entrydate'), function(x) data.frame(count=length(unique(x$Name))))
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

writeout <- merge(total.counts, total.changes) %>%
  replace(is.na(.), 0)
write.csv(writeout, "data/sncounts.csv", row.names=F)

RSQLite::dbDisconnect(conn)

total.changes$FIPS <- NULL
names(total.changes) <- c("neighborhood", "Breweries", "Mobile Food Services",
                         "Drinking Places", "Full Service Restaurants",
                         "Limited Service Restaurants")
for.heatmap <- melt(total.changes)
names(for.heatmap) <- c("Neighborhood", "Classification", "value")
for.heatmap$num <- as.numeric(factor(for.heatmap$Neighborhood))
for.heatmap$class <- as.numeric(factor(for.heatmap$Classification))

values <- unique(as.numeric(for.heatmap$value))

pos.palate <- brewer.pal(length(which(values > 0)), "Greens")
neg.palate <- brewer.pal(length(which(values < 0)), "Reds")

colors.df <- data.frame(value=values[order(values)],
                 colors = c(rev(neg.palate),"#FFFFFF", pos.palate, "#D3D3D3"))

final.for.heatmap <- merge(for.heatmap, colors.df, all.x=T) %>%
  select(num, class, value, colors) %>%
  replace(is.na(colors), "#D3D3D3")

final.for.heatmap$value <- ifelse(final.for.heatmap$value =="", "no data", final.for.heatmap$value)
names(final.for.heatmap) <- c("Neighborhood", "Classification", "value", "Color")
write.csv(final.for.heatmap, 'data/heatmap_num.csv', row.names=F, quote=F)
