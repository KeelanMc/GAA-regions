#Join count test looking at All-Ireland hurling winning counties
source('0_source.R')
Allirelands = read.csv('Input_data/Scrapped/HurlinAllIreland.csv')
Allirelands = Allirelands[,2:3]
Allirelands[1] = sapply(Allirelands[1],toupper)
countyhurlingwins = merge(counties,Allirelands,by.x='COUNTY',by.y='Team',all.x=TRUE)
hurlingcounties <- countyhurlingwins %>% mutate(!is.na(Winners))
names(hurlingcounties)[4] = 'allirelandwinners'
ggplot(hurlingcounties,aes(fill=allirelandwinners)) + 
  geom_sf()

winred <- ifelse(hurlingcounties$allirelandwinners,'Blue','Red') %>% factor()
joincount.test(winred, 
               nb2listw(poly2nb(countyhurlingwins),style='B'))[[1]]

set.seed(1921) 
joincount.mc(winred, 
             nb2listw(poly2nb(countyhurlingwins),style='B'),nsim=1000)[[1]]
#Unsurprisingly reject null hypothesis that there is no clustering of hurling winners
countyhurlingwins$Winners[is.na(countyhurlingwins$Winners)]=0

moran.plot(countyhurlingwins$Winners, 
           nb2listw(poly2nb(countyhurlingwins),style='W'))

moran.test(countyhurlingwins$Winners, 
           nb2listw(poly2nb(countyhurlingwins),style='W'))

set.seed(1917)
moran.mc(countyhurlingwins$Winners, 
         nb2listw(poly2nb(countyhurlingwins),style='W'),nsim=1000)
#reject null hypothesis that there is no spatial component to 
#number of all-Irelands won by a county