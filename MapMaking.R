source('0_source.R')

#Diocese maps
MostDioceses = st_read('Input_data/Diocese.geojson')
ExtraDiosceses = st_read('Input_data/ExtraDiocese.geojson')

Dioceses = dplyr::bind_rows(MostDioceses, ExtraDiosceses)

Ireland_cropped <- st_crop(Dioceses, xmin = -10.66265, xmax = -5.432731,
                          ymin = 51.4199, ymax = 55.43514)

ggplot(Ireland_cropped,aes(fill=Catholics))+geom_sf()+
  scale_fill_continuous("Catholic Population")

#Map of Dublin parishes with populations
parish = st_read('DublinParishes_ITM.shp')

ggplot(parish,aes(fill=Male))+geom_sf()+
  scale_fill_continuous("Male Population")


#Map of Clare Hurling clubs
ClareHurling = read.csv('Input_data/Scrapped/ClareHurling.csv')
ClareHurling  = st_as_sf(ClareHurling,coords = c("Longitude","Latitude"))
st_crs(ClareHurling) = 4326
#29902

age_cty <- st_read('Input_data/ages.json')
Clare = age_cty[3,"COUNTY"]
nicounties =  st_read('Input_data/NIcounties.geojson')
nicounties$CountyName[5] = 'DERRY'
nicounties = nicounties[ , !(names(nicounties) %in% c('OBJECTID'))]
fscounties = age_cty[,!(names(age_cty) %in% c('Mean_Age')) ]
colnames(nicounties) = c('COUNTY','geometry')
counties = rbind(nicounties,fscounties)

#Map of Clare Hurling club levels and senior championship wins since 1980
ggplot(Clare) + 
  geom_sf()+
  geom_sf(data=ClareHurling,aes(color=level),size = 5)+
  scale_color_viridis(option = "D")
#1=Senior... 5=junior C
ggsave(paste("Maps/Clare_Hurling_Club_Level.pdf"))

ggplot(Clare) + 
  geom_sf()+
  geom_sf(data=ClareHurling,aes(color=Countytitles),size = 5)+
  scale_color_viridis(option = "B",direction = -1)
ggsave(paste("Maps/ClareSHtitles.pdf"))
#Map of senior football and hurling teams in Clare
ClareSH = read.csv('Input_data/Scrapped/ClareSHteams.csv')
ClareSH  = st_as_sf(ClareSH,coords = c("Longitude","Latitude"))
st_crs(ClareSH) = 4326

ClareSF = read.csv('Input_data/Scrapped/ClareSFteams.csv')
ClareSF  = st_as_sf(ClareSF,coords = c("Longitude","Latitude"))
st_crs(ClareSF) = 4326

col = c("Red","Blue")
ggplot(Clare) + 
  geom_sf()+
  geom_sf(data=ClareSF,aes(color='red'),size = 5)+
  geom_sf(data=ClareSH,aes(color='blue'),size = 3)+
  scale_colour_hue("Senior Teams", 
                   labels = c("Hurling", "Football"))

ggsave(paste("Maps/ClareSeniorTeams.pdf"))


#Map of all-Ireland hurling championships won by counties
Allirelands = read.csv('Input_data/Scrapped/HurlinAllIreland.csv')
Allirelands = Allirelands[,2:3]
Allirelands[1] = sapply(Allirelands[1],toupper)
countyhurlingwins = merge(counties,Allirelands,by.x='COUNTY',by.y='Team',all.x=TRUE)

ggplot(countyhurlingwins,aes(fill=Winners)) + 
  geom_sf()+
  scale_fill_continuous("Hurling All-Irelands")

ggsave(paste("Maps/SHC_County.pdf"))

#Map of clubs that have one a county SHC in the big hurling counties and Offaly
BigClubs = read.csv('Input_data/Scrapped/CountyWinners.csv')
BigClubs  = st_as_sf(BigClubs,coords = c("Longitude","Latitude"))
st_crs(BigClubs) = 4326

ggplot(countyhurlingwins) + 
  geom_sf()+
  geom_sf(data=BigClubs,aes(color='red'),size = 1)+
  scale_colour_hue("", 
                   labels = c("County Champions"))

ggsave(paste("Maps/BigHurlingClubs.pdf"))


library(ggspatial)
hexgrid <- st_make_grid(countyhurlingwins,square=FALSE,n=10) %>% 
  st_sf() %>% mutate(id=row_number())

hexgrid <- BigClubs %>% st_join(hexgrid) %>% 
  count(id) %>% st_drop_geometry() %>% 
  left_join(hexgrid,.) %>% mutate(n=ifelse(is.na(n),0,n))
ggplot(hexgrid) +  
  annotation_map_tile(zoomin=0,type='osmgrayscale') +
  geom_sf(data=hexgrid,aes(fill=n),alpha=0.3,lwd=NA) +
  scale_fill_distiller(name='BigClubs',direction=1,
                       palette='Reds')

ggsave(paste("Maps/Hexplots.pdf"))





