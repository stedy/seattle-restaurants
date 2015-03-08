library(RSQLite)
library(ggmap)
library(ggplot2)
library(sp)

conn <- dbConnect(SQLite(), "restaurants.db")

wa.map <- rgdal::readOGR("zillow/ZillowNeighborhoods-WA.shp", layer="ZillowNeighborhoods-WA")
my.sub <- wa.map[wa.map$CITY == "Seattle", ]
sp::plot(my.sub)

res <- dbGetQuery(conn, "SELECT * FROM Restaurants")

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


sp::plot(my.sub)
points(df.points$Latitude ~ df.points$Longitude, col = "red", cex = 1)
