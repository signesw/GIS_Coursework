#Let's build an index of "unwalkability" - the higher the score, the less
#the option for walking or cycling to school is
library(dplyr)
library(dprep)
LSOA_index <- LSOA_dist_inc %>% dplyr::select("LSOA_CODE","average_distance")


#check distribution of each factor
LSOA_index %>%
  ggplot( aes(x=average_distance)) +
  geom_histogram(bins=100, fill='skyblue', color='#69b3a2') + scale_x_log10()+
  ggtitle("Average school travel distances in London by LSOA")+
  geom_vline(xintercept = mean(LSOA_index$average_distance),linetype="dashed")+
  geom_vline(xintercept = median(LSOA_index$average_distance),linetype="solid")+
  xlab("Euclidian Travel Distance (km)")+
  ylab("Count")


#log transformation 
LSOA_index$logdist = log(LSOA_index$average_distance)
LSOA_index %>%
  ggplot( aes(x=logdist)) +
  geom_histogram(bins=100, fill='skyblue', color='#69b3a2') + #scale_x_log10()+
  ggtitle("Average school travel distances in London by LSOA")+
  geom_vline(xintercept = mean(LSOA_index$logdist),linetype="dashed")+
  geom_vline(xintercept = median(LSOA_index$logdist),linetype="solid")+
  xlab("Euclidian Travel Distance (km)")+
  ylab("Count")

#z-score normalisation
LSOA_index$zdist = scale(LSOA_index$logdist,center=TRUE,scale=TRUE)

LSOA_index %>%
  ggplot( aes(x=zdist)) +
  geom_histogram(bins=100, fill='skyblue', color='#69b3a2') + #scale_x_log10()+
  ggtitle("Z-score of log-transformed data")+
  geom_vline(xintercept = mean(LSOA_index$zdist),linetype="dashed")+
  geom_vline(xintercept = median(LSOA_index$zdist),linetype="solid")+
  xlab("Z-score Euclidian Travel Distance (km)")+
  ylab("Count")

## Now we want to look at traffic accidents - Load 2019 road casualty data supplied by tfl https://tfl.gov.uk/cdn/static/cms/documents/2019-gla-data-extract-casualty.csv
casualties <- read.csv(here::here("2019-gla-data-extract-casualty.csv")) %>% janitor::clean_names(.)

#We're interested in cyclist and pedestrian casualties
casualties <- casualties %>% 
  dplyr::filter(str_detect(mode_of_travel,"( PEDESTRIAN)|( PEDAL CYCLE)"))

#Make into sf object
casualties <- casualties %>% 
  st_as_sf(., coords = c("easting", "northing")) %>% 
  st_set_crs(27700) 

#Plot with LSOAs
tm_shape(LSOA) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(casualties) +
  tm_dots(col = "red",size=0.01)

#Now we want to link each casulaty with the corresponding LSOA
library(GISTools)
#add casualty count to each polygon
## add point count to each polygon
(LSOA$casualty <- lengths(st_intersects(LSOA, casualties)))

LSOA_casualties <- poly.counts(casualties, LSOA)
setNames(res, x@data$NAME_1)
