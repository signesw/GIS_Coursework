#Let's build an index of "unwalkability" - the higher the score, the less
#the option for walking or cycling to school is
library(dplyr)
library(dprep)
library(ggpubr)
LSOA_index <- dplyr::select(LSOA_with_average,c("LSOA_CODE","MSOA11CD","LAD11CD","LAD11NM","average_distance"))



#check distribution of each factor
dist <- LSOA_index %>%
  ggplot( aes(x=average_distance)) +
  geom_histogram(bins=100, fill='#470137', color='gray') + scale_x_log10()+
  xlab("Network Travel Distance (km)")+
  ylab("Count")

dist


LSOA_index$logdist <- log(LSOA_index$average_distance)

LSOA_index$zdist = scale(LSOA_index$logdist,center=TRUE,scale=TRUE)

LSOA_index$zdist[LSOA_index$zdist > 3] <- 3
LSOA_index$zdist[LSOA_index$zdist < -3] <- -3

#_--------------------------- ROAD SAFETY ---------------------------------------------------
#Now we want to look at road safety for pedestrians and cyclists
accident_indicator <- read.csv(here::here("underlying_indicators.csv")) %>% janitor::clean_names() %>%
                      dplyr::select(.,c("lsoa_code_2011","road_traffic_accidents_indicator"))

accidents <- accident_indicator %>%
  ggplot( aes(x=road_traffic_accidents_indicator)) +
  geom_histogram(bins=100, fill="#040147", color="gray") + scale_x_log10()+
  #ggtitle("Average number of road accident casualties between 2010-2018, by LSOA")+
  #geom_vline(xintercept = mean(airindex$air_quality_indicator),linetype="dashed")+
  #geom_vline(xintercept = median(airindex$air_quality_indicator),linetype="solid")+
  xlab("Road Traffic Accidents (per 1000 people)")+
  ylab("Count")                       

#Add to index shapefile
LSOA_index <-dplyr::left_join(LSOA_index,accident_indicator,by=c("LSOA_CODE"="lsoa_code_2011"))

#Calculate log and z score
LSOA_index$logaccident = log(LSOA_index$road_traffic_accidents_indicator)
LSOA_index$zaccident = scale(LSOA_index$logaccident,center=TRUE,scale=TRUE)
  
#Deal with outliers
LSOA_index$zaccident[LSOA_index$zaccident > 3] <- 3
LSOA_index$zaccident[LSOA_index$zaccident < -3] <- -3

#--------------------------------------AIR POLLUTION---------------------------------------------------------------
# We will use air quality index score calculated as part of the indices of multiple deprivation from 2019
#https://data.london.gov.uk/dataset/indices-of-deprivation
airindex <- read.csv(here::here("underlying_indicators.csv")) %>% janitor::clean_names()
airindex <- airindex %>% dplyr::select(.,c("lsoa_code_2011","air_quality_indicator"))

air <- airindex %>%
  ggplot( aes(x=air_quality_indicator)) +
  geom_histogram(bins=100, fill='#024a28', color='gray') +
  #ggtitle("Average number of road accident casualties between 2010-2018, by LSOA")+
  #geom_vline(xintercept = mean(airindex$air_quality_indicator),linetype="dashed")+
  #geom_vline(xintercept = median(airindex$air_quality_indicator),linetype="solid")+
  xlab("Air Quality Indicator")+
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

cars <- no_cars %>%
  ggplot( aes(x=car_or_van_availability_no_cars_or_vans_in_household_2011_2)) +
  geom_histogram(bins=100, fill='#470107', color='gray') +#scale_x_log10()+
  #ggtitle("Percentage of households with no car")+
  #geom_vline(xintercept = mean(no_cars$car_or_van_availability_no_cars_or_vans_in_household_2011_2),linetype="dashed")+
  #geom_vline(xintercept = median(no_cars$car_or_van_availability_no_cars_or_vans_in_household_2011_2),linetype="solid")+
  xlab("% of Households with No Car")+
  ylab("Count") 

cars

#Normally distributed, no need to transform - calcualte z score
#first lets add it to index 

LSOA_index <-dplyr::left_join(LSOA_index,no_cars,by=c("LSOA_CODE"="lower_super_output_area"))
LSOA_index$zcars = scale(LSOA_index$car_or_van_availability_no_cars_or_vans_in_household_2011_2,center=TRUE,scale=TRUE)

#-----------------------------Combination of factors--------------------------------------------------------
Boroughs <- st_read(here::here("statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")) %>% 
  st_transform(27700)

#First, let's assign weights
#Distance: 35%
#Air quality= 15%LL
#Road accidents= 15%
#Access to cars = 35%

LSOA_index$indexdist <- LSOA_index$zdist*0.45
LSOA_index$indexairquality <- LSOA_index$zairquality*0.10
LSOA_index$indexacc<- LSOA_index$zaccident*0.20
LSOA_index$indexcars<- LSOA_index$zcars*0.25


#Now add the three together to make final index
LSOA_index$final_index <- LSOA_index$indexdist +
  LSOA_index$indexairquality +
  LSOA_index$indexacc+
  LSOA_index$indexcars

LSOA_index %>%
  ggplot( aes(x=final_index)) +
  geom_histogram(bins=100, fill='skyblue', color='gray') +#scale_x_log10()+
  xlab("Index of Dependence on Public Transportation")+theme_bw()+
  ylab("Count")+
  scale_y_continuous(expand = c(0, 0), limits=c(0,200))

# Let's map it

RelianceIndex <- ggplot() +
  geom_sf(data = LSOA_index, aes(fill = final_index),color=NA) +
  geom_sf(data = Boroughs, fill = "transparent",color = "white",size = 0.5)+ 
  scale_fill_gradient2(high = "#8f0114", low = "#080185",mid="lightgray", guide = "colorbar") +
  labs(fill = "Dependence on \nPublic Transport \nIndex")+
  theme_map()+
  theme(legend.title = element_text(color = "black", size = 10))


save_plot("relaiance_index.png",RelianceIndex)
RelianceIndex


#--------------------------- Plotting the indices by themselves--------------------------------
#set aesthetics
dist <- dist + scale_y_continuous(expand = c(0, 0), limits=c(0,300)) + theme_bw()
dist <- ggdraw(add_sub(dist, expression(paste("Plotted on log"[10]," scale")),size=9))
accidents <- accidents + scale_y_continuous(expand = c(0, 0), limits=c(0,400)) + theme_bw()
accidents <-ggdraw(add_sub(accidents, expression(paste("Plotted on log"[10]," scale")),size=9))
air <- air + scale_y_continuous(expand = c(0, 0), limits=c(0,250)) + theme_bw()
cars <- cars+ scale_y_continuous(expand = c(0, 0), limits=c(0,100)) + theme_bw()

dist
cars

variables_hist <-  plot_grid(
    dist,accidents, air, cars,
    labels = "AUTO",
    label_size = 12,
    label_x = 0, label_y = 0,
    hjust = -0.5, vjust = -0.5
  )

variables_hist
save_plot("variables_hist.png",variables_hist,ncol=2,nrow=2)

#Get summary stats
summary(LSOA_index$road_traffic_accidents_indicator)
summary(LSOA_index$air_quality_indicator)
summary(LSOA_index$car_or_van_availability_no_cars_or_vans_in_household_2011_2)

sd(LSOA_index$road_traffic_accidents_indicator)
sd(LSOA_index$air_quality_indicator)
sd(LSOA_index$car_or_van_availability_no_cars_or_vans_in_household_2011_2)

var(LSOA_index$road_traffic_accidents_indicator)
var(LSOA_index$air_quality_indicator)
var(LSOA_index$car_or_van_availability_no_cars_or_vans_in_household_2011_2)

Distance <- ggplot() +
  geom_sf(data = LSOA_index, aes(fill = zdist), color=NA) +
  geom_sf(data = Boroughs, fill = "transparent",color = "white",size = 0.5)+ 
  scale_fill_gradient(high = "#470137", low = "white", guide = "colorbar",breaks=c(-2,0,2)) +
  labs(fill = "Z-Score")+
  theme_map()+ theme(legend.title = element_text(color = "black", size = 10))
Distance

Accidents <- ggplot() +
  geom_sf(data = LSOA_index, aes(fill = zaccident), color=NA) +
  geom_sf(data = Boroughs, fill = "transparent",color = "white",size = 0.5)+ 
  scale_fill_gradient(high = "#040147", low = "white", guide = "colorbar",breaks=c(-2,0,2)) +
  labs(fill = "Z-score")+
  theme_map()+ theme(legend.title = element_text(color = "black", size = 10))
Accidents

Air <- ggplot() +
  geom_sf(data = LSOA_index, aes(fill = zairquality), color=NA) +
  geom_sf(data = Boroughs, fill = "transparent",color = "white",size = 0.5)+ 
  scale_fill_gradient(high = "#024a28", low = "white", guide = "colorbar",breaks=c(-2,0,2)) +
  labs(fill = "Z-Score")+
  theme_map()+ theme(legend.title = element_text(color = "black", size = 10))
Air

Nocars <- ggplot() +
  geom_sf(data = LSOA_index, aes(fill = zcars), color=NA) +
  geom_sf(data = Boroughs, fill = "transparent",color = "white",size = 0.5)+ 
  scale_fill_gradient(high = "#470107", low = "white", guide = "colorbar",breaks=c(-2,0,2)) +
  labs(fill = "Z-Score")+
  theme_map() + theme(legend.title = element_text(color = "black", size = 10))

Nocars

labels <- c("School Distance", "Pedestrian/Cyclist Safety","Air pollution","Households without Cars")
#Plot them together 
Variables <- plot_grid(
  Distance,Accidents, Air, Nocars,
  labels = "AUTO",
  label_size = 12,
  label_x = 0, label_y = 0,
  hjust = -0.5, vjust = -0.5
)

Variables
save_plot("variables.png",Variables,ncol=2,nrow=2)
