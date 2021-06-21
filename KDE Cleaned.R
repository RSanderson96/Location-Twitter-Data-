#Importing Packages
library(rgdal)
library(sf)
#library(rgdal)
library(rgeos)
library(spatstat)
library(raster)
library(dplyr)
library(maptools)
library(tmap)
library(leaflet)
library(mapview)
library(RColorBrewer)




Path = getwd() #this should be your desired working directory

#Import tweets
Tweets = read.csv("Recent_Convert.csv") #This should be your file of collected tweets

#Simplify the table to focus on aspects of interest
Tweet_Points = Tweets[,c("user_id", "created_at","source","Calc_Lat", "Calc_Long")] 

#Need to transform the twitter points into British National Grid
X<- Tweet_Points %>%
  st_as_sf(coords = c("Calc_Long", "Calc_Lat"), crs = 4326) %>%
  st_transform(27700) %>%
  st_coordinates() %>%
  as_tibble()
Tweet_Points[,"Calc_Long"] = X[,2]
Tweet_Points[,"Calc_Lat"] = X[,1]

#Making the spatial dataframe
Tweet_Points_SP <-SpatialPointsDataFrame(Tweet_Points[,4:5], Tweet_Points, proj4string = CRS("+init=EPSG:27700")) #convert to a spatial points frame
Tweet_Points_SP <- remove.duplicates(Tweet_Points_SP) #remove any duplicates
rm(Tweets,Tweet_Points,X) #remove unnecessary files

#Making the GB outline file in the raster package. This produces the basic outline of a country
#If using a different location, change 'GBR' to relevant 3 letter country code
#This can also be replaced by a shapefile of your desired polygon boundaries for point analysis
uK = raster::getData('GADM', country='GBR', level = 1) %>%
  st_as_sf()%>%
  st_cast("POLYGON") %>%
  mutate(area = st_area(.)) %>%
  arrange(desc(area)) %>%
  slice(1) # mainland US should be the largest
uK = as(uK, "Spatial") #changing file type
uK <- spTransform(uK, CRS("+init=epsg:27700")) #transforming to appropriate coordinate system

#Cutting the number of twitter points to the UK
Tweet_Points_SP <- Tweet_Points_SP[uK,] #remove any points outside of window of focus - in this case the window is the UK

#Making the variables for density analysis
window <- as.owin(uK) #making an observation window
#Making the point object
Tweet_Points_SP.ppp <- ppp(x=Tweet_Points_SP@coords[,1],y=Tweet_Points_SP@coords[,2],window=window)

plot(Tweet_Points_SP.ppp) #plot this to see the file made - border is England, points are tweets being analysed

#Quadrant Analysis

Q <- quadratcount(Tweet_Points_SP.ppp, nx= 6, ny=4)#Making quadrats - 4 rows and 6 columns
plot(Tweet_Points_SP.ppp, pch=20, cols="grey70", main=NULL)  # Plot points
plot(Q, add=TRUE)  # Add quadrat grid
Q.d <- intensity(Q) #plot the density
plot(intensity(Q, image=TRUE), main=NULL, las=1)  # Plot density raster, las?:http://rfunction.com/archives/1302
plot(Tweet_Points_SP.ppp, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points

#This plots the density of tweets in parts of the country, but is affected by MAUP

TP.KM <- rescale(Tweet_Points_SP.ppp, 1000, "km")  #rescale to produce points per km
w.km <- rescale(window, 1000, "km")

Q   <- quadratcount(TP.KM, nx= 6, ny=4)
Q.d <- intensity(Q)
plot(intensity(Q, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(TP.KM, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points
#Same plot, different scale

#KDE Analysis - https://rpubs.com/spring19cp6521/Week11_Wednesday1

K1 <- density(Tweet_Points_SP.ppp) # Using the default bandwidth
plot(K1, main=NULL, las=1)
contour(K1, add=TRUE)

K2 <- density(Tweet_Points_SP.ppp, sigma=10000) # Using a 50km bandwidth
plot(K2, main=NULL, las=1)
contour(K2, add=TRUE)

K3 <- density(Tweet_Points_SP.ppp, kernel = "disc", sigma=10000) # Using a sigma bandwidth
plot(K3, main=NULL, las=1)
contour(K3, add=TRUE)

#Comparison - repeat the process for the North East of England
#locate file with regional boundaries( from UK geoportal)
Regions = st_read(dsn = (paste0(Path, "/Regions")),layer="Regions_(December_2017)_Boundaries") 
# Subset the sf object
North_East <- Regions[Regions$rgn17nm %in% c("North East"), ] #select a region
rm(Regions)
North_East = as(North_East, "Spatial") #changing file type
North_East <- spTransform(North_East, CRS("+init=epsg:27700")) #change coordinate system
#Cutting the number of twitter points to the chosen region
NE_Tweets<- Tweet_Points_SP[North_East,] #remove any points outside of the MSOA boundaries
#Making the variables for density analysis
window <- as.owin(North_East) #New window
NE_Tweets.ppp <- ppp(x=NE_Tweets@coords[,1],y=NE_Tweets@coords[,2],window=window) #point object
#NE.KM <- rescale(NE_Tweets.ppp, 1000, "km")


NE1 <- density(NE_Tweets.ppp,bw.ppl) # bw.ppl bandwidth
plot(NE1, main=NULL, las=1)
contour(NE1, add=TRUE)

NE2 <- density(NE_Tweets.ppp) # Using the default bandwidth
plot(NE2, main=NULL, las=1)
contour(NE2, add=TRUE)

#And repeat for Tyne and Wear
UA = st_read(dsn = (paste0(Path, "/UAs")),layer="Counties_and_Unitary_Authorities_(December_2020)_UK_BFC")
Newcastle <- UA[UA$CTYUA20NM %in% c("Newcastle upon Tyne","Gateshead","North Tyneside", "South Tyneside",  "Sunderland"), ]
Newcastle = as(Newcastle, "Spatial") #changing file type
Newcastle <- spTransform(Newcastle, CRS("+init=epsg:27700"))
NCL_Tweets<- Tweet_Points_SP[Newcastle,] #remove any points outside of the MSOA boundaries
window <- as.owin(Newcastle)
NCL_Tweets.ppp <- ppp(x=NCL_Tweets@coords[,1],y=NCL_Tweets@coords[,2],window=window)
plot(NCL_Tweets.ppp)

NCL <- density(NCL_Tweets.ppp,bw.ppl) # Using the default bandwidth
plot(NCL, main=NULL, las=1)
contour(NCL, add=TRUE)




#producing maps
UK_Map<- raster(1000*K1) #convert KDE results to raster (UK)
epsg27700 <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs"
crs(UK_Map) <- sp::CRS(epsg27700) #Set coordinate system
#Select a colourbrewer palette
pal1 <- colorNumeric(
  palette = "Spectral",
  values(UK_Map),
  na.color = "transparent",
  reverse = TRUE)

NE_Map<- raster(1000*NE1) #convert KDE results to raster (North East)
crs(NE_Map) <- sp::CRS(epsg27700) #Set coordinate system
#Select a colourbrewer palette
pal2 <- colorNumeric(
  palette = "Spectral",
  values(NE_Map),
  na.color = "transparent",
  reverse = TRUE)

NCL_Map<- raster(1000*NCL) #convert KDE results to raster (North East)
crs(NCL_Map) <- sp::CRS(epsg27700) #Set coordinate system
#Select a colourbrewer palette
pal3 <- colorNumeric(
  palette = "Spectral",
  values(NCL_Map),
  na.color = "transparent",
  reverse = TRUE)


Map1<- leaflet() %>% 
  addTiles() %>%
  addRasterImage(UK_Map, colors = pal1, opacity = 0.8)%>%
  addLegend(pal = pal1, values = values(UK_Map),
            title = "KDE for England",opacity = 1,
            labFormat = labelFormat(digits = 10))

Map2<- leaflet() %>% 
  addTiles() %>%
  addRasterImage(NE_Map, colors = pal2, opacity = 0.8)%>%
  addLegend(pal = pal2, values = values(NE_Map),
            title = "KDE for North East England",opacity = 1,
            labFormat = labelFormat(digits = 11))


Map3<- leaflet() %>% 
  addTiles() %>%
  addRasterImage(NCL_Map, colors = pal3, opacity = 0.8)%>%
  addLegend(pal = pal3, values = values(NCL_Map),
            title = "KDE for Tyne & Wear",opacity = 1,
            labFormat = labelFormat(digits = 10))



