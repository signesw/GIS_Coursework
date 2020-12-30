#Now we want to get some statistics to truly quantify this effect

#First we want to get the number of 11-18 year olds there are in London, we can do this from mid-year population estimates 2020
#Read population estimates file
Population <- read.csv(here::here("Population Estimates","population estimates.csv")) %>% 
  dplyr::select(.,c(ï..LSOA.Code,LSOA.Name,LA.Code..2019.boundaries.,X11,X12,X13,X14,X15,X16,X17,X18)) %>% dplyr::rename("LSOA_CODE"=ï..LSOA.Code)

#Now sum the columns to get the total number of children in each LSOA
Population$Total <- rowSums(Population[,c("X11","X12","X13","X14","X15","X16","X17","X18")])

#Add total children to index data frame and remove NAs
LSOA_index <- Population %>% 
  dplyr::select(.,c(LSOA_CODE,Total)) %>% left_join(LSOA_index,.,by="LSOA_CODE") %>% drop_na()

#Let's get quintiles of Reliance index
Reliance_quintiles <- LSOA_index %>%
  pull(final_index) %>%
  quantile


#Now we want to group them
LSOA_index <- LSOA_index %>% mutate(
    index_quintiles = as.numeric(cut(
      final_index,
      breaks = Reliance_quintiles,
      include.lowest = TRUE)))

#Now we can calculate what percentage of kids is in the 20% most reliant on public transportation
