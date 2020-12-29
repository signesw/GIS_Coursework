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

#_--------------------------- ROAD SAFETY ---------------------------------------------------

## Now we want to look at traffic accidents - Load 2019 road casualty data supplied by tfl https://tfl.gov.uk/cdn/static/cms/documents/2019-gla-data-extract-casualty.csv
#casualties <- read.csv(here::here("2019-gla-data-extract-casualty.csv")) %>% janitor::clean_names(.)
accidents <-read.csv(here::here("road-casualties-severity-lsoa-msoa-ward.csv")) %>% janitor::clean_names()

colnms=c("x2010_total","x2011_total","x2012_total","x2013_total","x2014_total","x2015_total","x2016_total","x2017_total","x2018_total")
accidents[,colnms] <- lapply(accidents[,colnms], as.numeric) 
accidents[is.na(accidents)] <- 0
accidents$average <- rowSums(accidents[,colnms])/9

accidents %>%
  ggplot( aes(x=average)) +
  geom_histogram(bins=100, fill='skyblue', color='#69b3a2') + scale_x_log10()+
  ggtitle("Z-score of log-transformed data")+
  xlab("road accidents by LSOA")+
  ylab("Count")

#Let's map average accidents per LSOA
#First let's join accident data to our geomtery
accidents <- data.frame("LSOA_CODE"=accidents$lsoa_code, "accidents"=accidents$average)
LSOA_index <-dplyr::left_join(LSOA_index,accidents,by="LSOA_CODE")

#Log transformation of accidents
LSOA_index$logaccident = log(LSOA_index$accidents)
LSOA_index %>%
  ggplot( aes(x=accidents)) +
  geom_histogram(bins=100, fill='skyblue', color='#69b3a2') + scale_x_log10()+
  ggtitle("Average number of road accident casualties between 2010-2018, by LSOA")+
  geom_vline(xintercept = mean(LSOA_index$accidents),linetype="dashed")+
  geom_vline(xintercept = median(LSOA_index$accidents),linetype="solid")+
  xlab("Log of average number of casulaties")+
  ylab("Count")

#z-score normalisation
LSOA_index$zacc = scale(LSOA_index$logaccident,center=TRUE,scale=TRUE)

LSOA_index %>%
  ggplot( aes(x=zacc)) +
  geom_histogram(bins=100, fill='skyblue', color='#69b3a2') + #scale_x_log10()+
  ggtitle("Average number of road accident casualties between 2010-2018, by LSOA")+
  geom_vline(xintercept = mean(LSOA_index$zacc),linetype="dashed")+
  geom_vline(xintercept = median(LSOA_index$zacc),linetype="solid")+
  xlab("z-score of Log of average number of casulaties")+
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
lsoa_atlas <- read.csv(here::here("lsoa-data-old-boundaries-DataSheet.csv")) %>% janitor::clean_names()

no_cars <- lsoa_atlas %>% dplyr::select(.,c("lower_super_output_area","car_or_van_availability_2011_no_cars_or_vans_in_household"))
#remove lsoa atlas to free up space
rm(lsoa_atlas)

#Check ditribution

no_cars %>%
  ggplot( aes(x=car_or_van_availability_2011_no_cars_or_vans_in_household)) +
  geom_histogram(bins=100, fill='skyblue', color='#69b3a2') +#scale_x_log10()+
  ggtitle("Percentage of households with no car")+
  geom_vline(xintercept = mean(no_cars$car_or_van_availability_2011_no_cars_or_vans_in_household),linetype="dashed")+
  geom_vline(xintercept = median(no_cars$car_or_van_availability_2011_no_cars_or_vans_in_household),linetype="solid")+
  xlab("% of households with no car")+
  ylab("Count") 

#Normally distributed, no need to transform - calcualte z score
#first lets add it to index 

LSOA_index <-dplyr::left_join(LSOA_index,no_cars,by=c("LSOA_CODE"="lower_super_output_area"))
LSOA_index$zcars = scale(LSOA_index$car_or_van_availability_2011_no_cars_or_vans_in_household,center=TRUE,scale=TRUE)

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
#Distance: 25%
#Air quality= 25%LL
#Road accidents= 25%
#Access to cars = 25%

LSOA_index$zdist <- LSOA_index$zdist*0.25
LSOA_index$zairquality <- LSOA_index$zairquality*0.25
LSOA_index$zacc<- LSOA_index$zacc*0.25
LSOA_index$zcars<- LSOA_index$zacc*0.25


#Now add the three together to make final index
LSOA_index$final_index <- LSOA_index$zdist +
  LSOA_index$zairquality +
  LSOA_index$zacc+
  LSOA_index$zcars

# Let's map it

RelianceIndex <- ggplot() +
  geom_sf(data = LSOA_index, aes(fill = final_index),color=NA) +
  geom_sf(data = Boroughs, fill = "transparent",color = "gray",size = 0.5)+ 
  scale_fill_gradient(high = "red", low = "white", guide = "colorbar",breaks=c(-2,0,2),labels=c("Low","Medium","High")) +
  labs(fill = "Dependence on Public Transport")+
  theme_map()

RelianceIndex
palette_explorer()
