#Importing Libraries 
library("cowplot")
library("googleway")
library("ggplot2")
library("ggrepel")
library("ggspatial")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("tigris")
library("dplyr")
library("leaflet")
library("sp")
library("ggmap")
library("maptools")
library("broom")
library("httr")
library("rgdal")
library("ggmap")
library("tidyr")
library("stringr")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
library("knitr")

#read csvs into data frames 
reviews = read.csv("NY_AirBnB_reviews.csv")
listings = read.csv("NY_AirBnB_listings.csv")
nycLocations = read.csv("nyc_data_cleaned.csv")
r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)


#character conversion and removing NAs and removing $
nycLocations$Lat = as.numeric(nycLocations$Lat)
nycLocations$Long = as.numeric(nycLocations$Long)

head(listings$longitude)

listings$price = gsub("\\$", "", listings$price)
listings$price = as.numeric(listings$price)

listings$longitude = as.numeric(listings$longitude)
listings$latitude = as.numeric(listings$latitude)


nycLocations = na.omit(nycLocations)

#Mapping the neighborhoods 
nyc_neighborhoods_df <- tidy(nyc_neighborhoods)

ggplot() + 
  geom_polygon(data=nyc_neighborhoods_df, aes(x=long, y=lat, group=group))


#google maps register api key
register_google(key="AIzaSyCAwLNxEU-xmEE74hwZchoDIwzjlWmsF8o", write=TRUE)

newyork.map <- get_map(location= 'New York', 
                       maptype='roadmap', color='bw',source='google',zoom=10)


newyork.map.locs <- get_map(location= 'New York', 
                       maptype='roadmap', color='bw',source='google',zoom=12)

#Map of all the listings 
listings.map = ggmap(newyork.map) + 
  geom_point(data=listings,aes(x=longitude,y=latitude),size=.5, colour="red",alpha=0.75)+
  theme(axis.ticks = element_blank(),axis.text = element_blank())+
  xlab('')+ylab('')


listings.map

#Map of all the listings by price
listings$longitude
listings$latitude

price.listings.map = ggmap(newyork.map) + 
  geom_point(data=listings,aes(x=longitude,y=latitude, color=price),size=.25,alpha=0.75)+
  scale_colour_gradient(high="red",low='green')+ 
  theme(axis.ticks = element_blank(),axis.text = element_blank())+
  xlab('')+ylab('')

price.listings.map

##Map of all the listings by review score 
reviews.listings.map = ggmap(newyork.map) + 
  geom_point(data=listings,aes(x=longitude,y=latitude, color=review_scores_rating ),size=.25,alpha=0.75)+
  scale_colour_gradient(high="green",low='red')+ 
  theme(axis.ticks = element_blank(),axis.text = element_blank())+
  xlab('')+ylab('')

reviews.listings.map

#Map of all the listings by 
availability.listings.map = ggmap(newyork.map) + 
  geom_point(data=listings,aes(x=longitude,y=latitude, color=availability_365),size=.25,alpha=0.75)+
  scale_colour_gradient(high="green",low='red')+ 
  theme(axis.ticks = element_blank(),axis.text = element_blank())+
  xlab('')+ylab('')

availability.listings.map


#Map of tourist attractions 
nycLocations.map = ggmap(newyork.map.locs) + 
  geom_point(data=listings,aes(x=longitude,y=latitude, color=price),size=.25,alpha=0.75)+
  geom_point(data=nycLocations,aes(x=Long,y=Lat),size=1, colour="black",alpha=0.75)+
  scale_colour_gradient(high="green",low='red')+
  theme(axis.ticks = element_blank(),axis.text = element_blank())+
  xlab('')+ylab('')

nycLocations.map

listings$neighbourhood_group_cleansed

brooklyn = subset(listings, neighbourhood_group_cleansed=="Brooklyn")
queens = subset(listings, neighbourhood_group_cleansed=="Queens")
manhattan = subset(listings, neighbourhood_group_cleansed=="Manhattan")
bronx = subset(listings, neighbourhood_group_cleansed=="Bronx")
statenIsland = subset(listings, neighbourhood_group_cleansed=="Staten Island")

nrow(brooklyn)
nrow(queens)
nrow(manhattan)
nrow(bronx)
nrow(statenIsland)

listingsCount = c(nrow(brooklyn),
                   nrow(queens),
                   nrow(manhattan),
                   nrow(bronx),
                   nrow(statenIsland))
listingsCount

#NLP stuff 
reviews$comments<- gsub("@\\w+", "", reviews$comments)
reviews$comments <- gsub("https.+", "", reviews$comments)
reviews$comments <- gsub("\\d+\\w*\\d*", "", reviews$comments)
reviews$comments <- gsub("#\\w+", "", reviews$comments)
reviews$comments <- gsub("[^\x01-\x7F]", "", reviews$comments)
reviews$comments <- gsub("[[:punct:]]", " ", reviews$comments)
reviews$comments <- gsub("\n", " ", reviews$comments)
reviews$comments <- gsub("^\\s+", "", reviews$comments)
reviews$comments <- gsub("\\s+$", "", reviews$comments)
reviews$comments <- gsub("[ |\t]+", " ", reviews$comments)

commentsCorpus = Corpus(VectorSource(reviews$comments))
commentsCorpus <- tm_map(commentsCorpus, content_transformer(tolower))
commentsCorpus <- tm_map(commentsCorpus, removeNumbers)
commentsCorpus <- tm_map(commentsCorpus, removeWords, stopwords("english"))
commentsCorpus <- tm_map(commentsCorpus, removePunctuation)
commentsCorpus <- tm_map(commentsCorpus, stripWhitespace)

TermDocMat <- TermDocumentMatrix(commentsCorpus)
inspect(TermDocMat)

freqTerms <- findFreqTerms(TermDocMat, 15000)
freqTerms

#memory.limit(size=56000)
#wordFreq = colSums(as.matrix(TermDocMat))

findAssocs(TermDocMat, "word", corlimit=0.8)


polarityScores = read.csv("polarity_scores.csv")
summary(polarityScores)
