#-------------------INCOME----------------------------------------------
#Load income data and filter for London
Income <- read.csv(here::here("incomeestimates.csv")) %>% 
  dplyr::filter(str_detect(Local.authority.code, "^E09")) %>% 
  clean_names() %>% 
  rename(MSOA11CD="msoa_code") %>% 
  rename(income="net_annual_income_after_housing_costs_u_fffd") 

#Make a newdata table with just MSOA code and Income
Income <- dplyr::select(Income,MSOA11CD,income)
Income$income <- as.numeric(gsub("\\,", "",Income$income))

#Merge to LSOA with index df
LSOA_index <- dplyr::left_join(LSOA_index, Income, by = "MSOA11CD")
#We want to remove the NA LSOAs because they will cause problems when plotting
#create classes

LSOA_index %>%
  ggplot( aes(x=income)) +
  geom_histogram(bins=100, fill='skyblue', color='#69b3a2') + #scale_x_log10()+
  ggtitle("Percentage of households with no car")+
  geom_vline(xintercept = mean(LSOA_index$income),linetype="dashed")+
  geom_vline(xintercept = median(LSOA_index$income),linetype="solid")+
  xlab("Average household income by LSOA(£)")+
  ylab("Count") 

#Drop NAs - these will cause problems when making the classes
LSOA_bivariate<- 
  LSOA_index %>% drop_na()

# create 3 buckets for Income
income_buckets <- LSOA_bivariate%>%
  pull(income) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create 3 buckets for reliance
reliance_buckets <- LSOA_bivariate %>%
  pull(final_index) %>%
  quantile(probs = seq(0, 1, length.out = 4))

["#d3d3d3", "#b1b0d2", "#8e8cd1", "#9cbda6", "#839da5", "#697ea4", "#64a776", "#548b76", "#436f75"]
["#d3d3d3", "#9ac5cf", "#42b6ca", "#d399bf", "#9a8ebb", "#4284b7", "#d352ad", "#9a4ca9", "#4247a5"]
# create color scale that encodes two variables
# red for gini and blue for mean income
# the special notation with gather is due to readibility reasons
bivariate_color_scale <- tibble(
  "3 - 3" = "#64a776", # high distance, high income
  "2 - 3" = "#9cbda6",
  "1 - 3" = "#d3d3d3", # low distance, high income
  "3 - 2" = "#548b76",
  "2 - 2" = "#839da5", # medium distance, medium income
  "1 - 2" = "#b1b0d2",
  "3 - 1" = "#436f75", # high distance, low income
  "2 - 1" = "#697ea4",
  "1 - 1" = "#8e8cd1" # low distance, low income
) %>%
  gather("group", "fill")

# cut LSOA into groups defined above and join fill
LSOA_bivariate%<>%
  mutate(
    income_quantiles = cut(
      income,
      breaks = income_buckets,
      include.lowest = TRUE
    ),
    reliance_quantiles = cut(
      final_index,
      breaks = reliance_buckets,
      include.lowest = TRUE
    ),
    # by pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    group = paste(
      as.numeric(reliance_quantiles), "-",
      as.numeric(income_quantiles)
    )
  ) %>%
  # we now join the actual colour values per "group"
  # so each LSOA knows its colour value based on the avg distance and 
  # income value
  left_join(.,bivariate_color_scale, by = "group")

#________________________________Drawing the Map________________________________________________

#Load boroughs shapefile
Boroughs <- st_read(here::here("statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")) %>% 
  st_transform(27700)


map <- ggplot()+
  # use the same dataset a
  # first: draw the relief
  # color municipalities according to their distance / income combination
  geom_sf(data=LSOA,fill="darkgray",color=NA)+
  geom_sf(data=LSOA_bivariate,aes(fill = fill),color = NA) +
  scale_fill_identity() +
  # use thicker white stroke for Boroughs
  geom_sf(data = Boroughs,
    fill = "transparent",
    color = "white",
    size = 0.5) +
  # add titles
  #labs(x = NULL,
       #y = NULL,
       #title = "London's school travel distances and income",
       #subtitle = paste0("Average yearly income and average school travel distances",
                         #" for London children ages 11-18"),
       #caption = NA) +
  # add the theme
  theme_map()

basemap <- ggplot()+geom_sf(data=LSOA,aes(fill="gray"))+theme_map()

#make the legend
# separate the groups
bivariate_color_scale %<>%
  separate(group, into = c( "final_index","income"), sep = " - ") %>%
  mutate(income = as.integer(income),
         index = as.integer(final_index))

legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = final_index,
      y = income,
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(x = "Greater dependence \non public transport ⟶️" ,
       y = "⟵ Lower income️") +
  theme_map() +
  # make font small enough and rotate y label
  theme(axis.title = element_text(size = 6)
  ) +
  theme(axis.title.y = element_text(angle=90,vjust=1))+
  theme(axis.title.x=element_text(vjust=50)) +
  # make it square
  coord_fixed()
legend


Bivariate <- ggdraw() +
  #draw_plot(basemap, 0, 0, 1, 1) +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0, 0.03, 0.3, 0.3)

Bivariate 

save_plot("bivariate.png",Bivariate)
