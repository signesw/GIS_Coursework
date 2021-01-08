#Now we want to get some statistics to truly quantify this effect

#First we want to get the number of 11-18 year olds there are in London, we can do this from mid-year population estimates 2020
#Read population estimates file
Population <- read.csv(here::here("Population Estimates","population estimates.csv")) %>% 
  dplyr::select(.,c(ï..LSOA.Code,LSOA.Name,LA.Code..2019.boundaries.,X11,X12,X13,X14,X15,X16,X17,X18)) %>% dplyr::rename("LSOA_CODE"=ï..LSOA.Code)

#Now sum the columns to get the total number of children in each LSOA
Population$Total <- rowSums(Population[,c("X11","X12","X13","X14","X15","X16","X17","X18")])

#remove NAs
LSOA_index <- LSOA_index %>%  drop_na()

#Let's get quintiles of Reliance index
Reliance_quintiles <- LSOA_index %>%
  pull(final_index) %>%
  quantile(probs = seq(0, 1, length.out = 6))


#Now we want to group them
LSOA_index <- LSOA_index %>% mutate(
    index_quintiles = (cut(
      final_index,
      breaks = Reliance_quintiles,
      include.lowest = TRUE)))

LSOA_index <- LSOA_index %>% mutate(
  index_quintiles_groups = as.numeric(cut(
    final_index,
    breaks = Reliance_quintiles,
    include.lowest = TRUE)))

#Now we can calculate what percentage of kids is in the 20% most reliant on public transportation
#Let's map the quintiles 
IDPT <- ggplot()+
  geom_sf(data=LSOA,fill="darkgray",color=NA)+
  geom_sf(data=LSOA_index, aes(fill=index_quintiles),color=NA)+
  scale_fill_brewer(palette = "OrRd",labels = c("1", "2","3","4","5")) + 
  labs(fill = "Dependence on \nPublic Transport \nQuintile")+
  geom_sf(data = Boroughs,
          fill = "transparent",
          color = "white",
          size = 0.5) +
  theme_map()+
  theme(legend.title = element_text(color = "black", size = 10))

ggsave("Index_quintiles.png",IDPT)

#We want to subset the ones that are in the top 20% 
top_dependence <- LSOA_index %>% filter(index_quintiles_groups==5)

#-----------------------------Deprivation-------------------------------------------------------------
deprivation <- read.csv(here::here("IMD_data_2019.csv")) %>% clean_names() %>% 
  dplyr::select(.,c(lsoa_code_2011,index_of_multiple_deprivation_imd_decile_where_1_is_most_deprived_10_of_lso_as)) %>% dplyr::rename(c("LSOA_CODE"=lsoa_code_2011,"Decile"=index_of_multiple_deprivation_imd_decile_where_1_is_most_deprived_10_of_lso_as))

#join with LSOA
LSOA_IMD <- left_join(LSOA,deprivation,by="LSOA_CODE")

#Join with top_dependence index
top_dependence <- left_join(top_dependence,deprivation,by="LSOA_CODE")

#Get number in the top 20% 
Most_deprived <- top_dependence %>% filter(.,Decile<3)

proportion= nrow(Most_deprived)/nrow(top_dependence)*100
proportion

#Compare with all of london
Most_deprived_london <- deprivation %>% filter(.,Decile<3)
proportion_london <- nrow(Most_deprived_london)/nrow(deprivation)*100

#------------------------Race-------------------------------------------

BAME <- read.csv(here::here("lsoa-data.csv")) %>% clean_names() %>%  
  dplyr::select(.,c(lower_super_output_area,ethnic_group_bame_2011_2))

LSOA_Bame <- left_join(LSOA,BAME,by=c("LSOA_CODE"="lower_super_output_area"))

#Join data to the most dependent 
top_dependence <- left_join(top_dependence,BAME,by=c("LSOA_CODE"="lower_super_output_area"))

proportion_bame <- sum(top_dependence$ethnic_group_bame_2011_2)/nrow(top_dependence)

proportion_bame_london <- sum(BAME$ethnic_group_bame_2011_2)/nrow(BAME)

ggplot()+geom_sf(data=Boroughs,fill="gray",color="white")+theme_map()+geom_sf(data=top_dependence,aes(fill=ethnic_group_bame_2011_2),color=NA)+
  scale_fill_continuous(high="#85012f",low="white")+
  labs(fill="% BAME")

BAME <- ggplot()+geom_sf(data=Boroughs,fill="gray",color="white")+ 
  theme_map()+
  geom_sf(data=LSOA_Bame,aes(fill=ethnic_group_bame_2011_2),color=NA,size=0.005,alpha=0.2)+
  geom_sf(data=top_dependence,aes(fill=ethnic_group_bame_2011_2),color="white",size=0.01)+
  geom_sf(data=Boroughs,fill="transparent",color="white")+
  scale_fill_continuous(high="#850101",low="white")+
  labs(fill="% BAME")

BAME

save_plot("bame.png",BAME,ncol=1,nrow=1)

IMD <- ggplot()+geom_sf(data=Boroughs,fill="gray",color="white")+theme_map()+
  geom_sf(data=LSOA_IMD,aes(fill=Decile),color=NA,size=0.005,alpha=0.2)+
  geom_sf(data=top_dependence,aes(fill=Decile),color="white",size=0.01)+
  geom_sf(data=Boroughs,fill="transparent",color="white")+
  scale_fill_gradient(high="white",low="#00137d",breaks=c(1,2,3,4,5,6,7,8,9,10),minor_breaks=waiver(),guide = guide_legend(reverse = FALSE))+
  labs(fill="IMD \nPercentile")+theme(legend.text=element_text(size=8))+
  theme(legend.title=element_text(size=10))

IMD

BAME <- BAME +  theme(legend.key.size = unit(0.3, "cm"),legend.key.width = unit(0.5,"cm"),legend.title=element_text(size=10),legend.text=element_text(size=8))
IMD <- IMD+ theme(legend.key.size = unit(0.2, "cm"),legend.key.width = unit(0.5,"cm"))


Proportions <-  plot_grid(
  IMD,BAME,
  labels = "AUTO",
  label_size = 12,
  label_x = 0, label_y = 0,
  hjust = -0.5, vjust = -0.5
)

Proportions<- Proportions + draw_label("% in Deciles 1 & 2", x=0.05, y=0.12, hjust=0, fontface="bold", color = "black", size = 10)+
  draw_label("20% most dependent on transport = 38%", x=0.05, y=0.07, hjust=0,color = "black", size = 8)+
  draw_label("All of London = 16%", x=0.05, y=0.02, hjust=0,color = "black", size = 8)+
  draw_label("% BAME", x=0.55, y=0.12, hjust=0, fontface="bold", color = "black", size = 10)+
  draw_label("20% most dependent on transport = 46%", x=0.55, y=0.07, hjust=0,color = "black", size = 8)+
  draw_label("All of London = 39%", x=0.55, y=0.02, hjust=0,color = "black", size = 8)

Test
save_plot("proportions.png",Proportions,ncol=2,nrow=1)


##################################################################
#Filter out central London
Central_London <- top_dependence %>% 
  dplyr::filter(LAD11NM %in% c("City of London","Camden","Islington","Kensington and Chelsea","Lambeth","Southwark","Westminster"))

proportion_central_london <- nrow(Central_London)/nrow(top_dependence)*100

t <-  st_join(LSOA_index, Boroughs,join=st_contains,left = TRUE)


Central_London_LSOAs <- LSOA_index %>% 
  dplyr::filter(LAD11NM %in% c("City of London","Camden","Islington","Kensington and Chelsea","Lambeth","Southwark","Westminster"))

proportion_central_london_LSOA <- nrow(Central_London_LSOAs)/nrow(LSOA_index)*100
