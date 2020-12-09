library(sf)
library(tidyverse)
library(here)
library(stringr)
library(ggplot2)
library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)

#Reading in all schools basefile
AllSchools <- read.csv(here::here("edubasealldata.csv"))


#FILTERING OUT FOR 16_18 students

#First, get all the schools that are operating (schools that are open (as of 2019))

OpenSchools <- AllSchools %>% 
  dplyr::filter(str_detect(EstablishmentStatus..name., "Open"))

hassixthform <- OpenSchools %>% 
  dplyr::filter(OfficialSixthForm..code.==1)

#filter by phase of education (16 plus and all through)
sixteenplus <- OpenSchools %>% 
  dplyr::filter(str_detect(PhaseOfEducation..name.,"16 plus|All-through"))

#Get secondary schools that have greater than 16 as statutory high age
SecondarySchools <- OpenSchools %>% 
  dplyr::filter(str_detect(PhaseOfEducation..name.,"Secondary")) %>% 
  dplyr::filter(StatutoryHighAge > 16)

#Get colleges
Colleges <- OpenSchools %>% 
  dplyr::filter(str_detect(EstablishmentTypeGroup..name.,"Colleges"))

#Merging these three to get all the schools i want to look at                
UK16plus <- rbind(SecondarySchools,sixteenplus,hassixthform,Colleges) %>% 
  distinct()

#Now filter for LONDON
LondonSchools <- UK16plus %>% 
  dplyr::filter(str_detect(DistrictAdministrative..code., "^E09"))


#plot
#Create a simplefeatures object out of the LondonSchools
LondonSchools_sf <- LondonSchools %>% 
  st_as_sf(., coords = c("Easting", "Northing")) %>% 
  st_set_crs(27700)
#Plot
ggplot() +
  geom_sf(data = LondonSchools_sf) +
  ggtitle("Map of School Locations within London")

#Now load London LSOA shapefile
LSOA <- st_read(here::here("statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/LSOA_2011_London_gen_MHW.shp")) %>% 
  st_transform(27700)


#Remove points outside of London
LondonSchools_sf <- LondonSchools_sf[LSOA,]


st_crs(LondonSchools_sf)
st_crs(LSOA)

#check to see that we're looking at the right thing
tmap_mode("view")
tm_shape(LSOA) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(LondonSchools_sf) +
  tm_dots(col = "green")

#read in the catchment flow CSV
Catchment <- read.csv(here::here("Catchments_SecSchootoLSOA_2016_LDS.csv"))

#Getting unique values just to check what schools are there
CatchmentDistinct <- unique(Catchment$ï..Secondary_School_URN)

#Now filter out the ones so we only get the ones with flows
FinalSchools <- filter(LondonSchools_sf, URN %in% CatchmentDistinct)

tmap_mode("view")
tm_shape(LSOA) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(LondonSchools_sf) +
  tm_dots(col = "green")+
  tm_shape(FinalSchools) + 
  tm_dots(col="blue")

#Filter the catchment data to only the ones we want
URNs <- unique(FinalSchools$URN)

#Filter out points that are not within LSOA shapefile
LSOAs <- unique(LSOA$LSOA11CD)
FinalCatchment <- subset(Catchment, ï..Secondary_School_URN %in% URNs)
FinalCatchment <- subset(FinalCatchment, LSOA_CODE %in% LSOAs)

#Cleaning the data (remove unecessary columns):
FinalCatchment <- dplyr::select(FinalCatchment, -c(Secondary2LSOA_Flow_No., LSOA_NAME))
FinalCatchment <- FinalCatchment %>% rename(URN="ï..Secondary_School_URN")

#Merge geometry column from schools to flow dataframe
CatchmentWithGeometry <- dplyr::left_join(FinalSchools,FinalCatchment,by="URN")

#Simple table
FlowsWithGeometry <- dplyr::select(CatchmentWithGeometry, c(Secondary_School_Name,LSOA_CODE, Pupil_count,geometry))


#Rename column
LSOA <- LSOA %>% rename(LSOA_CODE="LSOA11CD")

#take centroid of LSOA areas (st_centroid)
Points_LSOA <- st_centroid(LSOA)
#lets just look at LSOA Code and geometry
Points_LSOA <- dplyr::select(Points_LSOA, c(LSOA_CODE,geometry)) %>% 
  rename(Name="LSOA_CODE")

#get a df with just school name and geometry
Points_Schools <- dplyr::select(FlowsWithGeometry, c(Secondary_School_Name,geometry)) %>% 
  rename(Name="Secondary_School_Name")

#join points and secondary school names
zones=rbind(Points_Schools,Points_LSOA)

library(stplanr)

travel_lines <- od2line(flow = FlowsWithGeometry, zones = zones)
w <- FlowsWithGeometry$Pupil_count / max(FlowsWithGeometry$Pupil_count) *10
tmap_mode("view")
tm_shape(travel_lines)+
  tm_lines(col="black",lwd = 0.1)+
  tm_shape(FinalSchools) + 
  tm_dots(col="red",size=0.001)
 

tmap_mode("view")

tmaptools::palette_explorer()
tm_shape(LSOA) +
  tm_polygons(col = NA, alpha = 0.5, lwd=0.1)+
tm_shape(travel_lines) +
  tm_lines(palette = "plasma", breaks = c(0, 5, 10, 20, 40, 100, 200),
           lwd = w,
           scale = 9,
           title.lwd = "Number of pupils",
           alpha = 0.6,
           col = "Pupil_count",
           title = "Pupil Count")+
  tm_shape(FinalSchools) + 
  tm_dots(col="red",size=0.01)

#Calculate distances
# find distances
l_distances <- geo_length(travel_lines)
summary(l_distances)
travel_lines$distances <- l_distances

#add column for average distance travelled
travel_lines$total_distance <- ((travel_lines$Pupil_count)*(travel_lines$distances))
Sums_LSOA <- st_set_geometry(travel_lines,NULL) %>% 
  dplyr::select(., c(LSOA_CODE,Pupil_count,total_distance)) %>% group_by(LSOA_CODE) %>% summarize_all(sum)
Sums_LSOA <- transform(Sums_LSOA, average_distance = (total_distance / Pupil_count)/1000)

#LEt's join this to LSOA data and map
LSOA_with_average <- left_join(LSOA,Sums_LSOA, by="LSOA_CODE")

tm_shape(LSOA_with_average) +
  tm_polygons(col = "average_distance", title="Average Distance to School (km)", alpha = 0.9, lwd=0.1)

