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

LSOA_index$zdist[LSOA_index$zdist > 3] <- 3
LSOA_index$zdist[LSOA_index$zdist < -3] <- -3

#_--------------------------- ROAD SAFETY ---------------------------------------------------
#Now we want to look at road safety for pedestrians and cyclists
accident_indicator <- read.csv(here::here("underlying_indicators.csv")) %>% janitor::clean_names() %>%
                      dplyr::select(.,c("lsoa_code_2011","road_traffic_accidents_indicator"))

accident_indicator %>%
  ggplot( aes(x=road_traffic_accidents_indicator)) +
  geom_histogram(bins=100, fill='skyblue', color='#69b3a2') + scale_x_log10()+
  ggtitle("Average number of road accident casualties between 2010-2018, by LSOA")+
  #geom_vline(xintercept = mean(airindex$air_quality_indicator),linetype="dashed")+
  #geom_vline(xintercept = median(airindex$air_quality_indicator),linetype="solid")+
  xlab("Road traffic accidents indicator")+
  ylab("Count")                       

#Add to index shapefile
LSOA_index <-dplyr::left_join(LSOA_index,accident_indicator,by=c("LSOA_CODE"="lsoa_code_2011"))

#Calculate log and z score
LSOA_index$logaccident = log(LSOA_index$road_traffic_accidents_indicator)
LSOA_index$zaccident = scale(LSOA_index$logaccident,center=TRUE,scale=TRUE)
  
#Deal with outliers
LSOA_index$zaccident[LSOA_index$zaccident > 3] <- 3
LSOA_index$zaccident[LSOA_index$zaccident < -3] <- -3

LSOA_index %>%
  ggplot( aes(x=zaccident)) +
  geom_histogram(bins=100, fill='skyblue', color='#69b3a2') + #scale_x_log10()+
  ggtitle("Average number of road accident casualties between 2010-2018, by LSOA")+
  #geom_vline(xintercept = mean(airindex$air_quality_indicator),linetype="dashed")+
  #geom_vline(xintercept = median(airindex$air_quality_indicator),linetype="solid")+
  xlab("Road traffic accidents indicator")+
  ylab("Count")                       


#--------------------------------------AIR POLLUTION---------------------------------------------------------------
# We will use air quality index score calculated as part of the indices of multiple deprivation from 2019
#https://data.london.gov.uk/dataset/indices-of-deprivation
airindex <- read.csv(here::here("underlying_indicators.csv")) %>% janitor::clean_names()
airindex <- airindex %>% dplyr::select(.,c("lsoa_code_2011","air_quality_indicator"))

airindex %>%
  ggplot( aes(x=air_quality_indicator)) +
  geom_histogram(bins=100, fill='skyblue', color='#69b3a2') +
  ggtitle("Average number of road accident casualties between 2010-2018, by LSOA")+
  geom_vline(xintercept = mean(airindex$air_quality_indicator),linetype="dashed")+
  geom_vline(xintercept = median(airindex$air_quality_indicator),linetype="solid")+
  xlab("air quality indicator")+
  ylab("Count")                       

#Data appears normally distributed, no need to transform
#add to index data frame
LSOA_index <-dplyr::left_join(LSOA_index,airindex,by=c("LSOA_CODE"="lsoa_code_2011"))

LSOA_index$zairquality = scale(LSOA_index$air_quality_indicator,center=TRUE,scale=TRUE)


#-------------------------------Access to cars--------------------------------------------
#cars per household from 2011 census
no_cars <- read.csv(here::here("lsoa-data.csv")) %>% janitor::clean_names() %>% 
          dplyr::select(.,c("lower_super_output_area","car_or_van_availability_no_cars_or_vans_in_household_2011_2"))

#Check ditribution

no_cars %>%
  ggplot( aes(x=car_or_van_availability_no_cars_or_vans_in_household_2011_2)) +
  geom_histogram(bins=100, fill='skyblue', color='#69b3a2') +#scale_x_log10()+
  ggtitle("Percentage of households with no car")+
  geom_vline(xintercept = mean(no_cars$car_or_van_availability_no_cars_or_vans_in_household_2011_2),linetype="dashed")+
  geom_vline(xintercept = median(no_cars$car_or_van_availability_no_cars_or_vans_in_household_2011_2),linetype="solid")+
  xlab("% of households with no car")+
  ylab("Count") 

#Normally distributed, no need to transform - calcualte z score
#first lets add it to index 

LSOA_index <-dplyr::left_join(LSOA_index,no_cars,by=c("LSOA_CODE"="lower_super_output_area"))
LSOA_index$zcars = scale(LSOA_index$car_or_van_availability_no_cars_or_vans_in_household_2011_2,center=TRUE,scale=TRUE)

LSOA_index %>%
  ggplot( aes(x=zcars)) +
  geom_histogram(bins=100, fill='skyblue', color='#69b3a2') +#scale_x_log10()+
  ggtitle("Percentage of households with no car")+
  #geom_vline(xintercept = mean(no_cars$car_or_van_availability_2011_no_cars_or_vans_in_household),linetype="dashed")+
  #geom_vline(xintercept = median(no_cars$car_or_van_availability_2011_no_cars_or_vans_in_household),linetype="solid")+
  xlab("% of households with no car")+
  ylab("Count") 
#-----------------------------Combination of factors--------------------------------------------------------
#First, let's assign weights
#Distance: 35%
#Air quality= 15%LL
#Road accidents= 15%
#Access to cars = 35%

LSOA_index$indexdist <- LSOA_index$zdist*0.35
LSOA_index$indexairquality <- LSOA_index$zairquality*0.15
LSOA_index$indexacc<- LSOA_index$zaccident*0.15
LSOA_index$indexcars<- LSOA_index$zcars*0.35


#Now add the three together to make final index
LSOA_index$final_index <- LSOA_index$indexdist +
  LSOA_index$indexairquality +
  LSOA_index$indexacc+
  LSOA_index$indexcars

# Let's map it

RelianceIndex <- ggplot() +
  geom_sf(data = LSOA_index, aes(fill = final_index),color=NA) +
  geom_sf(data = Boroughs, fill = "transparent",color = "gray",size = 0.5)+ 
  scale_fill_gradient2(high = "red", low = "blue", guide = "colorbar",breaks=c(-2,0,2),labels=c("Low","Medium","High")) +
  labs(fill = "Dependence on Public Transport")+
  theme_map()

RelianceIndex


#--------------------------- Plotting the indices by themselves--------------------------------
Distance <- ggplot() +
  geom_sf(data = LSOA_index, aes(fill = zdist), color=NA) +
  geom_sf(data = Boroughs, fill = "transparent",color = "white",size = 0.5)+ 
  scale_fill_gradient(high = "red", low = "white", guide = "colorbar",breaks=c(-2,0,2)) +
  labs(fill = "Distance to School")+
  theme_map()
Distance

Accidents <- ggplot() +
  geom_sf(data = LSOA_index, aes(fill = zaccident), color=NA) +
  geom_sf(data = Boroughs, fill = "transparent",color = "white",size = 0.5)+ 
  scale_fill_gradient(high = "blue", low = "white", guide = "colorbar",breaks=c(-2,0,2)) +
  labs(fill = "Road casualties involving cyclists and pedestrians")+
  theme_map()
Accidents

Air <- ggplot() +
  geom_sf(data = LSOA_index, aes(fill = zairquality), color=NA) +
  geom_sf(data = Boroughs, fill = "transparent",color = "white",size = 0.5)+ 
  scale_fill_gradient(high = "#024a28", low = "white", guide = "colorbar",breaks=c(-2,0,2)) +
  labs(fill = "Air pollution")+
  theme_map()
Air

Nocars <- ggplot() +
  geom_sf(data = LSOA_index, aes(fill = zcars), color=NA) +
  geom_sf(data = Boroughs, fill = "transparent",color = "white",size = 0.5)+ 
  scale_fill_gradient(high = "#470107", low = "white", guide = "colorbar") +
  labs(fill = "Households without access to car")+
  theme_map()
Nocars
