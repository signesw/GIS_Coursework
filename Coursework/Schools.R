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
library(viridis)
library(janitor)
library(cowplot)

#Reading in all schools basefile
AllSchools <- read.csv(here::here("edubasealldata.csv"))


#FILTERING OUT FOR 11_18 students

#First, get all the schools that are operating (schools that are open (as of 2019))

OpenSchools <- AllSchools %>% 
  dplyr::filter(str_detect(EstablishmentStatus..name., "Open"))


#filter by phase of education (Secondary, 16 plus, and all through, and middle-deemed seocndary)
elevenplus <- OpenSchools %>% 
  dplyr::filter(str_detect(PhaseOfEducation..name.,"Secondary|16 plus|All-through|Middle deemed secondary"))

#Get colleges
Colleges <- OpenSchools %>% 
  dplyr::filter(str_detect(EstablishmentTypeGroup..name.,"Colleges"))

#Merging these two to get all the schools i want to look at and remove duplicates                
UK11plus <- rbind(elevenplus,Colleges) %>% 
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

#Filter the catchment data to only the ones we want
URNs <- unique(FinalSchools$URN)

#Filter out LSOAs that are not within LSOA shapefile
LSOAs <- unique(LSOA$LSOA11CD)
FinalCatchment <- subset(Catchment, ï..Secondary_School_URN %in% URNs) %>%  subset(., LSOA_CODE %in% LSOAs)

#Cleaning the data (remove unecessary columns):
FinalCatchment <- dplyr::select(FinalCatchment, -c(Secondary2LSOA_Flow_No.)) %>% 
  rename(URN="ï..Secondary_School_URN")

#------------------- Flows from POPULATION ESTIMATES ----------------------------
# read population csv - get the columns we want and filter for London
Population <- read.csv(here::here("Population Estimates","population estimates.csv")) %>% 
  dplyr::select(.,c(ï..LSOA.Code,LSOA.Name,LA.Code..2019.boundaries.,X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25))

#Filter to the unqiue LSOAs in the flow data
LSOA.names <- unique(FinalCatchment$LSOA_NAME)
Population <- subset(Population, LSOA.Name %in% LSOA.names) %>% rename(LSOA_NAME="LSOA.Name")
#We only want to add the age ranges of the schools
ageranges <- FinalSchools %>%  dplyr::select(.,c(URN, StatutoryLowAge,StatutoryHighAge))

#Now we want to add the statutory age range and age makeup for each LSOA
Catchmentwithage <- dplyr::right_join(ageranges,FinalCatchment,by="URN")
Catchmentwithage <- dplyr::left_join(Catchmentwithage,Population, by="LSOA_NAME")

ranges <- Catchmentwithage %>%  dplyr::select(.,c(StatutoryLowAge,StatutoryHighAge,X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25))
#get the column range 
ranges <- ranges %>% 
  mutate(low_column = StatutoryLowAge + 3, #pulling the columns for age
         high_column = StatutoryHighAge + 3)
#drop geometry column and convert all to numeric values
ranges <- st_set_geometry(ranges,NULL)
ranges <- as.data.frame(lapply(ranges,as.numeric))
ranges[is.na(ranges)] <- 0

#Calculate sum of children in complete age range of the school
all_students <- sapply(seq_len(nrow(ranges)), function(i) sum(ranges[i, ranges$low_column[i]:ranges$high_column[i]]))
ranges$all_students <- all_students

#calculate sum of 16-18 year olds in each LSOA
ranges$sixteen_18 <- ranges$X16 + ranges$X17 + ranges$X18

#Now see what portion of students are 16-18 year olds
ranges$proportion <- ranges$sixteen_18/ranges$all_students

#Now we want to add this column to our flow data -  round so there's at least 1
FinalCatchment$AdjPupil_count <- ceiling(ranges$proportion*FinalCatchment$Pupil_count)

#Merge geometry column from schools to flow dataframe -
CatchmentWithGeometry <- dplyr::left_join(FinalSchools,FinalCatchment,by="URN")
#____________________________________________________________________________________________________________________________________________________________________________________
#                             ORGIN DESTINATION LINES
#Simple table
FlowsWithGeometry <- dplyr::select(CatchmentWithGeometry, c(Secondary_School_Name,LSOA_CODE, Pupil_count, AdjPupil_count,geometry))

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


tmaptoolstm_shape(FinalSchools) +::palette_explorer()
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
travel_lines$distances <- l_distances

#make a dataframe with a row for each pupil and the travel distance

total_students <- travel_lines[rep(1:nrow(travel_lines), travel_lines$Pupil_count),]


#add column for average distance travelled
travel_lines$total_distance <- ((travel_lines$Pupil_count)*(travel_lines$distances))
Sums_LSOA <- st_set_geometry(travel_lines,NULL) %>% 
  dplyr::select(., c(LSOA_CODE,Pupil_count,total_distance)) %>% group_by(LSOA_CODE) %>% summarize_all(sum)
Sums_LSOA <- transform(Sums_LSOA, average_distance = (total_distance / Pupil_count)/1000)

#Plot distribution of distances - all students

total_students %>%
  ggplot( aes(x=distances)) +
  geom_histogram(bins=100, fill='skyblue', color='#69b3a2') + scale_x_log10()+
  ggtitle("School travel distances in London per Student")+
  geom_vline(xintercept = 4823)+
  xlab("Euclidian Travel Distance (m)")+
  ylab("Count")
  #Add some summary statistics

##------------------------Get walking routes------------------------------------
library(osrm)
#reproject travel lines geometry to wsg84
travel_lines_transformed <- st_transform(travel_lines, 4326)
#try with small sample
desire_lines <- travel_lines_transformed[2:200, ]
library(tictoc)
tic("total")
routes <- route(
  l = desire_lines,
  route_fun = osrmRoute,
  returnclass = "sf")
toc()


tm_shape(routes) +
  tm_lines(palette = "plasma", breaks = c(0, 5, 10, 20, 40, 100,200),
           lwd = "Pupil_count",
           scale = 9,
           title.lwd = "Number of pupils",
           alpha = 0.6,
           col = "Pupil_count",
           title = "Pupil Count") + 
  tm_shape(FinalSchools)+
  tm_dots(col="red",size=0.01,id="EstablishmentName")

#------------------ GET QUANTILE BREAKS ----------------------
#First let's round average distance to 1 decimal place
Sums_LSOA$average_distance_round=round(Sums_LSOA$average_distance, digits = 1)
# get decile breaks based on the rounded values
Sums_LSOA$deciles <- ntile(Sums_LSOA$average_distance_round, 10)
dbreaks=quantile(Sums_LSOA$average_distance_round, probs = seq(0, 1, 1/10))
dbreaks <- replace(dbreaks, c(1), 0)
#cut dataframe based on breaks
Sums_LSOA<- mutate(Sums_LSOA, deciles = cut(average_distance_round, dbreaks,c(1,2,3,4,5,6,7,8,9,10)))

#Plot distribution of average by decile - first let's round

df <- round(Sums_LSOA$average_distance,1)


colours <- get_brewer_pal("RdYlGn", n = 10)

histbreaks <- seq(0,12,0.1)

histogram_legend <- Sums_LSOA %>%
  ggplot(aes(x=average_distance_round)) +
  geom_histogram(binwidth=0.1,aes(fill = as.factor(deciles)),breaks=histbreaks)+ #scale_x_log10()+
  scale_fill_manual(name = "Average distance deciles",values=rev(colours))+
  geom_vline(xintercept = (dbreaks), linetype="dashed")+
  guides(colour = guide_legend(nrow =1))+
  xlab("Euclidian Travel Distance (km)")+
  ylab("Count")+
  theme_classic()+
  theme(legend.position="none")

histogram_legend <- histogram_legend + 
  scale_y_continuous("Count", expand = c(0, 0), breaks = seq(0, 300, 50), 
                     limits = c(0, 250)) + 
  scale_x_continuous("Euclidian Travel Distance (km)", expand = c(0,0),breaks=seq(0,12,1))


histogram_legend

palette_explorer()

#LEt's join this to LSOA data and map
LSOA_with_average <- left_join(LSOA,Sums_LSOA, by="LSOA_CODE")


#calculate and group by deciles
LSOA_with_average$decile <- ntile(LSOA_with_average$average_distance, 10)
ntile(LSOA_with_average$average_distance, 10)
quantile(LSOA_with_average$average_distance, prob = seq(0, 1, length = 10), type = 5,na.rm=TRUE)
#Distribution of average distance travelled

tmap_mode("view")
tm_shape(LSOA_with_average) +
  tm_polygons(col = "decile", title="Average Distance to School (km)", alpha = 0.9, lwd=0.1)

#Do a ggplot with histogram as legend

#add decile breaks to sf object
LSOA_with_average<- mutate(LSOA_with_average, deciles = cut(average_distance, breaks$brks))

legend <- ggplotGrob(histogram_legend) 

Distance_deciles <- ggplot(LSOA_with_average) + 
  geom_sf(aes(fill=as.factor(decile)),color=NA,size = 0.001)+
  theme_void()+
  scale_fill_manual(name = "Average distance deciles",values=rev(colours),na.value="gray")+
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")

#Let's expand the London map to make place for our histogram
Final <- Distance_deciles + coord_sf(xlim = c(502500, 591956.7), ylim = c(145850.8, 201500)) +
  #adding the histogram
  annotation_custom(
    grob = legend, 
    ymin=145850.8, 
    ymax=168000.8,
    xmin=551654,
    xmax=591956.7)
#and plot
Final

#---------------------------BIVARIATE MAP (MEDIAN INCOME AND DISTRIBUTION)--------------------------------------------------
library(biscale)

#Load income data and filter for London
Income <- read.csv(here::here("incomeestimates.csv")) %>% 
  dplyr::filter(str_detect(Local.authority.code, "^E09")) %>% 
  clean_names() %>% 
  rename(MSOA11CD="msoa_code") %>% 
  rename(income="net_annual_income_after_housing_costs_u_fffd") 

#Make a newdata table with just MSOA code and Income
Income <- dplyr::select(Income,MSOA11CD,income)
Income$income <- as.numeric(gsub("\\,", "",Income$income))

#Merge to LSOA with average
LSOA_dist_inc <- dplyr::left_join(LSOA_with_average, Income, by = "MSOA11CD")
#We want to remove the NA LSOAs because they will cause problems when plotting
#create classes
classes <- LSOA_dist_inc %>% bi_class(., x = average_distance, y = income, style = "fisher", dim = 3) %>% 
  drop_na()

#Map
map <- ggplot() +
  geom_sf(data = classes, mapping = aes(fill = bi_class), color = NA, size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "Brown", dim = 3) +
  bi_theme()
bi_legend()
legend <- bi_legend(pal = "Brown",
                    dim = 3,
                    xlab = "Higher Travel Distance",
                    ylab = "Higher Income",
                    size = 8)

legend

library(cowplot)

# combine map with legend
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, x=0.65, .1, 0.4, 0.4,scale=1)

draw_plot
finalPlot

#https://github.com/grssnbchr/bivariate-maps-ggplot2-sf/blob/master/analysis/index.Rmd"


sapply(LSOA_dist_inc)
#Z score normalisation of different factors: income, cycle infrastructure? Other factors?
#Check statutory walking distance?

#Statutory walking age - look at the ones that are just below it? -- look a average walking speeds and calculate walking times

# Look into developing an index? - how can I then incorporate LSOAs into it? --> maybe first identify areas and then look at road safety or something?
  