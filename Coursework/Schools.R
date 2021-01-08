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
library(ggspatial)
library(geojsonio)
library(tmaptools)
library(viridis)
library(janitor)
library(cowplot)

#Reading in all schools basefile
AllSchools <- read.csv(here::here("edubasealldata.csv"))


#FILTERING OUT FOR 11_18 students

#First, get all the schools that are operating (schools that are open (as of 2019))

OpenSchools <- AllSchools %>% 
  dplyr::filter(str_detect(EstablishmentStatus..name., "Open"))


#Now filter for LONDON
LondonSchools <- OpenSchools %>% 
  dplyr::filter(str_detect(DistrictAdministrative..code., "^E09"))


#Create a simplefeatures object out of the LondonSchools
LondonSchools_sf <- LondonSchools %>% 
  st_as_sf(., coords = c("Easting", "Northing")) %>% 
  st_set_crs(27700)


#Now load London LSOA shapefile
LSOA <- st_read(here::here("statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/LSOA_2011_London_gen_MHW.shp")) %>% 
  st_transform(27700)
#Load boroughs shapedfile too 
Boroughs <- st_read(here::here("statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")) %>% 
st_transform(27700)

#Let's plot our Study Area
study_area <- ggplot()+ geom_sf(data=LSOA, color="#696868",size=0.1,linetype = "solid", fill='#ede9e8')+
          geom_sf(data=Boroughs,color="black",size=0.3,linetype = "solid", fill=NA)+
          theme_map()+
          annotation_scale(location = "bl")+
          annotation_north_arrow(location = "tl", which_north = "true",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering)


ggsave("study_area.png",study_area)

#Remove points outside of London
LondonSchools_sf <- LondonSchools_sf[LSOA,]

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
  tm_polygons(col = NA, alpha = 0.5, id="LSOA11NM") +
  tm_shape(LondonSchools_sf) +
    tm_dots(col = "green",id="EstablishmentName")+
  tm_shape(FinalSchools) + 
    tm_dots(col="blue",id="EstablishmentName")

#Get unique URNs
URNs <- unique(FinalSchools$URN)

#Filter out LSOAs that are not within LSOA shapefile
LSOAs <- unique(LSOA$LSOA11CD)
FinalCatchment <- subset(Catchment, ï..Secondary_School_URN %in% URNs) %>%  subset(., LSOA_CODE %in% LSOAs)

#Cleaning the data (remove unecessary columns):
FinalCatchment <- dplyr::select(FinalCatchment, -c(Secondary2LSOA_Flow_No.)) %>% 
  rename(URN="ï..Secondary_School_URN")

#Merge geometry column from schools to flow dataframe -
CatchmentWithGeometry <- dplyr::left_join(FinalSchools,FinalCatchment,by="URN")
#____________________________________________________________________________________________________________________________________________________________________________________
#                             ORGIN DESTINATION LINES
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

#Get the travel lines
travel_lines <- od2line(flow = FlowsWithGeometry, zones = zones)
#function for line widths as a proportion of the pupil count


tm_shape(LSOA) +
  tm_polygons(col = NA, alpha = 0.5, lwd=0.1)+
tm_shape(travel_lines) +
  tm_lines(palette = "plasma", breaks = c(0, 5, 10, 20, 40, 100,200),
           lwd = "Pupil_count",
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
travel_lines$distances <- l_distances/1000

#make a dataframe with a row for each pupil and the travel distance

total_students <- travel_lines[rep(1:nrow(travel_lines), travel_lines$Pupil_count),]


#add column for average distance travelled
travel_lines$total_distance <- ((travel_lines$Pupil_count)*(travel_lines$distances))
Sums_LSOA <- st_set_geometry(travel_lines,NULL) %>% 
  dplyr::select(., c(LSOA_CODE,Pupil_count,total_distance)) %>% group_by(LSOA_CODE) %>% summarize_all(sum)
Sums_LSOA <- transform(Sums_LSOA, average_distance = (total_distance / Pupil_count))

#Plot distribution of distances - all students

summarystats <- data.frame("stats"=c(median(total_students$distances),mean(total_students$distances)),
                           "Line"=c("Median","Mean"))


#Plot distribution of distances - per LSOA

summarystats <- data.frame("stats"=c(median(Sums_LSOA$average_distance),mean(Sums_LSOA$average_distance)),
                           "Line"=c("Median","Mean"))

Sums_LSOA$logdistance <- log(Sums_LSOA$average_distance)
Sums_LSOA %>%
  ggplot( aes(x=average_distance)) +
  geom_histogram(bins=100, fill='#470137', color='gray') + #scale_x_log10()+
  geom_vline(data = summarystats, 
             mapping = aes(xintercept=stats,
                           color = Line,
                           linetype=Line 
             ),
             show.legend = T)+
  xlab("Distance to School (km)")+
  ylab("Pupil Count")+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,350))+
  scale_color_manual(name = "Statistics", values = c(Median = "#6b0c1b", Mean = "#d1949e"))+
  guides(color = guide_legend(override.aes = list(linetype = c('solid','dashed'))),
         linetype = FALSE)+
  theme_bw()

##------------------------Get walking routes------------------------------------
library(osrm)
#reproject travel lines geometry to wsg84
travel_lines_transformed <- st_transform(travel_lines, 4326)
#try with small sample
desire_lines <- travel_lines_transformed[0:29508, ]
test <- travel_lines[25408:25409,]

rm(routes)

tmap_mode("view")
library(tictoc)
tic("total")
routes <- route(
  l = desire_lines,
  route_fun = osrmRoute,
  returnclass = "sf")
toc()



st_write(routes,"routes.geojson")

routes <- st_read(here::here("routes.geojson"))

tmap_mode("plot")
plt <- tm_shape(routes) +
  tm_lines(palette = "plasma", breaks = c(0, 5, 10, 20, 40, 100,200),
           lwd = "Pupil_count",
           scale = 9,
           id="route_number",
           title.lwd = "Number of pupils",
           alpha = 0.6,
           col = "Pupil_count",
           title = "Pupil Count")
  #tm_shape(FinalSchools)+
  #tm_dots(col="red",size=0.01,id="EstablishmentName")
plt

plt_lines <- ggplot()+
  geom_sf(data=LSOA, color="black",size=0.1,linetype = "dashed", fill=NA)+
  geom_sf(data=travel_lines, aes(color=Pupil_count,size=Pupil_count),alpha=0.6)+
  scale_size(name="Pupil count",breaks=c(0, 5, 10, 20, 40, 100,200),range=c(0,2))+
  scale_colour_stepsn(name="Pupil count", colours=viridisLite::plasma(6),
                      breaks=c(0, 5, 10, 20, 40, 100,200),
                      space = "Lab",
                      na.value = "grey50",
                      values=c(0,0.025,0.05,0.1,0.2,0.5,1),
                      guide = "coloursteps",
                      aesthetics = "colour")+
  geom_sf(data=FinalSchools, color="black", size=0.5)+
  annotation_scale(location = "bl")+
  annotation_north_arrow(location = "bl", which_north = "true",
                          height = unit(1, "cm"),
                          width = unit(1, "cm"),
                          pad_y = unit(0.2, "in"),
                          style = north_arrow_fancy_orienteering)+
  #scale_color_viridis_c()+
  theme_map()



plt_routes <- ggplot()+
  geom_sf(data=LSOA, color="black",size=0.1,linetype = "dashed", fill=NA)+
  geom_sf(data=routes, aes(color=Pupil_count,size=Pupil_count),alpha=0.6)+
  scale_size(name="Pupil count",breaks=c(0, 5, 10, 20, 40, 100,200),range=c(0,2))+
  scale_colour_stepsn(name="Pupil count", colours=viridisLite::plasma(6),
    breaks=c(0, 5, 10, 20, 40, 100,200),
    space = "Lab",
    na.value = "grey50",
    values=c(0,0.025,0.05,0.1,0.2,0.5,1),
    guide = "coloursteps",
    aesthetics = "colour")+
  geom_sf(data=FinalSchools, color="black", size=0.5)+
  annotation_scale(location = "bl",style="bar")+
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                        pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)+
 #scale_color_viridis_c()+
  theme_map()

plt_routes
rm(routes)
ggsave("lines.png",plt_lines) 
ggsave("routes.png", plt_routes)

##Now get distances  (drop geometry bc it's taking up too much space)
routes_no_geo <- st_set_geometry(routes,NULL)
rm(routes)

#add column for average distance travelled
routes_no_geo$total_distance_routes <- ((routes_no_geo$Pupil_count)*(routes_no_geo$distance))
Sums_LSOA_routes <- routes_no_geo %>% 
  dplyr::select(., c(LSOA_CODE,Pupil_count,total_distance_routes)) %>% group_by(LSOA_CODE) %>% summarize_all(sum)
Sums_LSOA_routes <- transform(Sums_LSOA_routes, average_distance = (total_distance_routes / Pupil_count))

#Plot histogram, straight line as well as route distance and compare
summarystatslines <- data.frame("stats"=c(median(Sums_LSOA$average_distance),mean(Sums_LSOA$average_distance)),
                           "Line"=c("Median","Mean"))
summarystatsroutes <- data.frame("stats"=c(median(Sums_LSOA_routes$average_distance),mean(Sums_LSOA_routes$average_distance)),
                                "Line"=c("Median","Mean"))

lines_hist <- ggplot(data=Sums_LSOA,aes(x=average_distance)) +
  geom_histogram(bins=100, fill='#470137', color='gray') + #scale_x_log10()+
  geom_vline(data = summarystatslines, 
             mapping = aes(xintercept=stats,
                           color = Line,
                           linetype=Line 
             ),
             show.legend = T)+
  xlab("Average Straight Line Distance to School (km)")+
  ylab("Count")+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,400))+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,15))+
  scale_color_manual(name = "Statistics", values = c(Median = "#6b0c1b", Mean = "#d1949e"))+
  guides(color = guide_legend(override.aes = list(linetype = c('solid','dashed'))),
         linetype = FALSE)+
  theme_bw()

routes_hist <- ggplot(data=Sums_LSOA_routes,aes(x=average_distance)) +
  geom_histogram(bins=100, fill='#470137', color='gray') + #scale_x_log10()+
  geom_vline(data = summarystatsroutes, 
             mapping = aes(xintercept=stats,
                           color = Line,
                           linetype=Line 
             ),
             show.legend = T)+
  xlab("Average Network Distance to School (km)")+
  ylab("Count")+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,400))+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,19))+
  scale_color_manual(name = "Statistics", values = c(Median = "#6b0c1b", Mean = "#d1949e"))+
  guides(color = guide_legend(override.aes = list(linetype = c('solid','dashed'))),
         linetype = FALSE)+
  theme_bw()

#Get summary stats for the two types of distance
summary(Sums_LSOA$average_distance)
summary(Sums_LSOA_routes$average_distance)
sd(Sums_LSOA$average_distance)
sd(Sums_LSOA_routes$average_distance)
var(Sums_LSOA$average_distance)
var(Sums_LSOA_routes$average_distance)

hists <- plot_grid(lines_hist, routes_hist, labels = "AUTO")
save_plot("histograms.png",hists,base_asp=3.36)

routes<-routes[!(routes$route_number==24699 | routes$route_number==24660),]

#------------------ GET QUANTILE BREAKS ----------------------
#First let's round average distance to 1 decimal place
Sums_LSOA_routes$average_distance_round=round(Sums_LSOA_routes$average_distance, digits = 1)
# get decile breaks based on the rounded values
Sums_LSOA_routes$deciles <- ntile(Sums_LSOA_routes$average_distance_round, 10)
dbreaks=quantile(Sums_LSOA_routes$average_distance_round, probs = seq(0, 1, 1/10))
dbreaks <- replace(dbreaks, c(1), 0)
#cut dataframe based on breaks
Sums_LSOA_routes<- mutate(Sums_LSOA_routes, deciles = cut(average_distance_round, dbreaks,c(1,2,3,4,5,6,7,8,9,10)))

#Plot distribution of average by decile - first let's round

mycolour <- colorRampPalette(c("#470137","#faebf7"))(10)

histbreaks <- seq(0,12,0.1)

histogram_legend <- Sums_LSOA_routes %>%
  ggplot(aes(x=average_distance_round)) +
  geom_histogram(binwidth=0.1,aes(fill = as.factor(deciles)),breaks=histbreaks,color=NA)+ #scale_x_log10()+
  scale_fill_manual(name = "Average distance deciles",values=rev(mycolour))+
  geom_vline(xintercept = (dbreaks), linetype="dashed")+
  guides(colour = guide_legend(nrow =1))+
  xlab("Average Network Travel Distance (km)")+
  ylab("Count")+
  theme_classic()+
  theme(legend.position="none")

histogram_legend <- histogram_legend + 
  scale_y_continuous("Count", expand = c(0, 0), breaks = seq(0, 300, 50), 
                     limits = c(0, 200)) + 
  scale_x_continuous("Average Travel Distance (km)", expand = c(0,0),breaks=seq(0,19,1))+
  theme(axis.text=element_text(size=7))

histogram_legend

#LEt's join this to LSOA data and map
LSOA_with_average <- left_join(LSOA,Sums_LSOA_routes, by="LSOA_CODE")


#calculate and group by deciles
LSOA_with_average$decile <- ntile(LSOA_with_average$average_distance, 10)
#Distribution of average distance travelled

tmap_mode("view")
tm_shape(LSOA_with_average) +
  tm_polygons(col = "decile", title="Average Distance to School (km)", alpha = 0.9, lwd=0.1)

#Do a ggplot with histogram as legend


legend <- ggplotGrob(histogram_legend)

Distance_deciles

Distance_deciles <- ggplot(LSOA_with_average) + 
  geom_sf(aes(fill=as.factor(decile)),color=NA,size = 0.001)+
  theme_void()+
  scale_fill_manual(name = "Average distance deciles",values=rev(mycolour),na.value="gray")+
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")

#Let's expand the London map to make place for our histogram
Final <- Distance_deciles + coord_sf(xlim = c(502500, 591956.7), ylim = c(145850.8, 201500)) +
  #adding the histogram
  annotation_custom(grob = ggplotGrob(histogram_legend), ymin=145850.8, ymax=168000.8,xmin=551500,xmax=591956.7)
#and plot
Final

save_plot("DistancePlot.png",Final)



  