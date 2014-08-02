# library stuff
install.packages("RCurl")
install.packages("RgoogleMaps")
install.packages("RJSONIO")
install.packages("data.table")
install.packages("Hmisc")
library("RCurl")
library("RgoogleMaps")
library("RJSONIO")
library("data.table")
library("Hmisc")


options(digits = 15) #needed so that we don't round any lats and longs
#setwd("~/R")
# set up region value index for matching later
Subregion <- read.csv("DEQsubregions.csv")
index <- Subregion$Subregion
values <- Subregion$GEOID10

# now we will import our data
GEOdata <- read.csv("DEQgeodata.csv")

#renames varablies to stardard R conventions
setnames(GEOdata, "I_ZIP", "I.ZIP")
setnames(GEOdata, "I_BLOCK", "I.BLOCK")
setnames(GEOdata, "O_INTERR1M1", "ADD")
GEOdata["ZIP.same"] <- NA
GEOdata$ZIP.same <- GEOdata$I.ZIP==GEOdata$ZIPCODE
summary(GEOdata$ZIP.same)


#this is the fuctions we will use to make location based on address and biased to a zip

DEQgeo <- function(zip, add)
{
  add <- gsub(' ','+',add) #Encode URL Parameters
  #Open Connection
  url <- "http://maps.googleapis.com/maps/api/geocode/json?&components=postal_code:%i|administrative_area:OR|country:US&sensor=false&address=%s"
  connectStr <- sprintf(url, zip, add)
  # connectStr <- paste('http://maps.googleapis.com/maps/api/geocode/json?&components=postal_code:%i|administrative_area:OR|country:US&sensor=false&address=%s',gcStr, sep="") 
  con <- url(connectStr)
  data.json <- fromJSON(paste(readLines(con), collapse=""))
  close(con)
  #Flatten the received JSON
  Sys.sleep(.2)
  data.json <- unlist(data.json)
  lat <- data.json["results.geometry.location.lat"]
  lng <- data.json["results.geometry.location.lng"]
  gcodes <- c(lat, lng)
  names(gcodes) <- c("Lat", "Lng")
  return (gcodes)
  Sys.sleep(.2)
}

#test to make sure it is working
DEQgeo(97213,"ne 66th and sandy")

#now lets get some geocodes
Geocoded <- with(GEOdata, data.frame(GEOdata, t(mapply(DEQgeo, (GEOdata$ZIPCODE), (GEOdata$ADD)))))

#need to make things numeric
Geocoded$Lat <- as.numeric(as.character(Geocoded$Lat))
Geocoded$Lng <- as.numeric(as.character(Geocoded$Lng))

#now lets get the other results for when we just put stuff in
Geocoded["zipadd"] <- NA
Geocoded$zipadd <- paste(Geocoded$ADD, Geocoded$ZIPCODE)


#new function, much like the old but with only 1 input
DEQgeoconf <- function(add)
{
  add <- gsub(' ','+',add) #Encode URL Parameters
  #Open Connection
  url <- "http://maps.googleapis.com/maps/api/geocode/json?&components=administrative_area:OR|country:US&sensor=false&address=%s"
  connectStr <- sprintf(url, add)
  # connectStr <- paste('http://maps.googleapis.com/maps/api/geocode/json?&components=postal_code:%i|administrative_area:OR|country:US&sensor=false&address=%s',gcStr, sep="") 
  con <- url(connectStr)
  data.json <- fromJSON(paste(readLines(con), collapse=""))
  close(con)
  #Flatten the received JSON
  Sys.sleep(.1)
  data.json <- unlist(data.json)
  lat <- data.json["results.geometry.location.lat"]
  lng <- data.json["results.geometry.location.lng"]
  gcodes <- c(lat, lng)
  names(gcodes) <- c("Lat.conf", "Lng.conf")
  return (gcodes)
  Sys.sleep(.1)
}

#test to make sure it still works
DEQgeoconf("66th and Sandy 97213")

#add this to our data.frame
Geocoded <- with(Geocoded, data.frame(Geocoded, t(sapply(Geocoded$zipadd, DEQgeoconf))))
Geocoded$Lat.conf <- as.numeric(as.character(Geocoded$Lat.conf))
Geocoded$Lng.conf <- as.numeric(as.character(Geocoded$Lng.conf))

#lets see how they compare
Geocoded["Lat.same"] <- NA
Geocoded$Lat.same <- Geocoded$Lat==Geocoded$Lat.conf
Geocoded["Lng.same"] <- NA
Geocoded$Lng.same <- Geocoded$Lng==Geocoded$Lng.conf



#get rid of those pesky NA's
GeocodedNONA <- na.omit(Geocoded)


# FCC's Census Block Conversions API
# http://www.fcc.gov/developers/census-block-conversions-api
latlong2fips <- function(latitude, longitude) {
  url <- "http://data.fcc.gov/api/block/find?format=json&latitude=%f&longitude=%f"
  url <- sprintf(url, latitude, longitude)
  json <- RCurl::getURL(url)
  json <- RJSONIO::fromJSON(json)
  as.numeric(json$Block['FIPS'])
}

#lets test this new function
latlong2fips(latitude=28.35975, longitude=-81.421988)

#get the FIPS code
GeocodedNONA <- with(GeocodedNONA, data.frame(GeocodedNONA, mapply(latlong2fips, GeocodedNONA$Lat, GeocodedNONA$Lng)))
GeocodedNONA <- with(GeocodedNONA, data.frame(GeocodedNONA, mapply(latlong2fips, GeocodedNONA$Lat.conf, GeocodedNONA$Lng.conf)))

# rename fip column, so annoying must be better way
setnames(GeocodedNONA, "mapply.latlong2fips..GeocodedNONA.Lat..GeocodedNONA.Lng.", "fips")
setnames(GeocodedNONA, "mapply.latlong2fips..GeocodedNONA.Lat.conf..GeocodedNONA.Lng.conf..1", "fips.conf")


#lets set up our data.frame for stuff
GeocodedNONA["blockgroup"] <- NA
GeocodedNONA$blockgroup <- as.numeric(substr(GeocodedNONA$fips, 1, 12))
GeocodedNONA["blockgroup.conf"] <- NA
GeocodedNONA$blockgroup.conf <- as.numeric(substr(GeocodedNONA$fips.conf, 1, 12))
GeocodedNONA["blockgroup.same"] <- NA
GeocodedNONA$blockgroup.same <- GeocodedNONA$blockgroup==GeocodedNONA$blockgroup.conf
GeocodedNONA["region"] <- NA
GeocodedNONA["region.conf"] <- NA
GeocodedNONA["region.same"] <- NA
GeocodedNONA$region <- index[match(GeocodedNONA$blockgroup, values)]
GeocodedNONA$region.conf <- index[match(GeocodedNONA$blockgroup.conf, values)]

GeocodedNONA$region.same <- GeocodedNONA$region==GeocodedNONA$region.conf
describe(GeocodedNONA$region.same)
describe(GeocodedNONA$region)

GEOcodedData <- GeocodedNONA[c(1,2,3,4,5,12,13)]

write.csv(GEOcodedData, file = "DEQdata-geocoded.csv")
