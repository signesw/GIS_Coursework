#Drop NAs
LSOA_dist_inc <- 
  LSOA_dist_inc %>% drop_na()

# create 3 buckets for Income
income_buckets <- LSOA_dist_inc %>%
  pull(income) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create 3 buckets for travel distace
distance_buckets <- LSOA_dist_inc %>%
  pull(average_distance) %>%
  quantile(probs = seq(0, 1, length.out = 4))

["#d3d3d3", "#9ac5cf", "#42b6ca", "#d399bf", "#9a8ebb", "#4284b7", "#d352ad", "#9a4ca9", "#4247a5"]
# create color scale that encodes two variables
# red for gini and blue for mean income
# the special notation with gather is due to readibility reasons
bivariate_color_scale <- tibble(
  "3 - 3" = "#d352ad", # high distance, high income
  "2 - 3" = "#d399bf",
  "1 - 3" = "#d3d3d3", # low distance, high income
  "3 - 2" = "#9a4ca9",
  "2 - 2" = "#9a8ebb", # medium distance, medium income
  "1 - 2" = "#9ac5cf",
  "3 - 1" = "#4247a5", # high distance, low income
  "2 - 1" = "#4284b7",
  "1 - 1" = "#42b6ca" # low distance, low income
) %>%
  gather("group", "fill")

# cut LSOA into groups defined above and join fill
LSOA_dist_inc %<>%
  mutate(
    income_quantiles = cut(
      income,
      breaks = income_buckets,
      include.lowest = TRUE
    ),
    distance_quantiles = cut(
      average_distance,
      breaks = distance_buckets,
      include.lowest = TRUE
    ),
    # by pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    group = paste(
      as.numeric(distance_quantiles), "-",
      as.numeric(income_quantiles)
    )
  ) %>%
  # we now join the actual hex values per "group"
  # so each LSOA knows its hex value based on the avg distance and 
  # income value
  left_join(bivariate_color_scale, by = "group")

#________________________________Drawing the Map________________________________________________

#Load boroughs shapefile
Boroughs <- st_read(here::here("statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")) %>% 
  st_transform(27700)


map <- ggplot(data=LSOA_dist_inc)+
  # use the same dataset a
  # first: draw the relief
  # color municipalities according to their distance / income combination
  geom_sf(aes(fill = fill),color = NA) +
  scale_fill_identity() +
  # use thicker white stroke for Boroughs
  geom_sf(data = Boroughs,
    fill = "transparent",
    color = "white",
    size = 0.5) +
  # add titles
  labs(x = NULL,
       y = NULL,
       title = "London's school travel distances and income",
       subtitle = paste0("Average yearly income and average school travel distances",
                         " for London children ages 11-18"),
       caption = NA) +
  # add the theme
  theme_map()

#make the legend
# separate the groups
bivariate_color_scale %<>%
  separate(group, into = c( "average_distance","income"), sep = " - ") %>%
  mutate(income = as.integer(income),
         distance = as.integer(average_distance))

legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = distance,
      y = income,
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(x = "Higher distance ⟶️",
       y = "Higher income ⟶️") +
  theme_map() +
  # make font small enough and rotate y label
  theme(axis.title = element_text(size = 6)
  ) +
  theme(axis.title.y = element_text(angle=90))+
  theme(axis.title.x=element_text(vjust=30)) +
  # make it square
  coord_fixed()
legend


ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.075, 0.2, 0.2)
