source('0_source.R')
age_cty <- st_read('Input_data/ages.json')
Clare = age_cty[3,"COUNTY"]
nicounties =  st_read('Input_data/NIcounties.geojson')
nicounties$CountyName[5] = 'DERRY'
nicounties = nicounties[ , !(names(nicounties) %in% c('OBJECTID'))]
fscounties = age_cty[,!(names(age_cty) %in% c('Mean_Age')) ]
colnames(nicounties) = c('COUNTY','geometry')
counties = rbind(nicounties,fscounties)

countyhurlingwins = merge(counties,Allirelands,by.x='COUNTY',by.y='Team',all.x=TRUE)
countyhurlingwins$Winners[is.na(countyhurlingwins$Winners)]=0
countypop = read.csv('Input_data/Scrapped/Countypop.csv')
countypop = countypop[c('County','Population')]
countypop$Population =as.numeric(gsub(",","",countypop$Population))
countypop$County[countypop$County=='Londonderry']='Derry'
countypop$County = toupper(countypop$County)

countyhurlingwins = merge(countyhurlingwins,countypop,by.x = 'COUNTY',by.y='County')

spare_mtx <- st_intersects(counties, counties)
for (i in 1:length(spare_mtx)){
  spare_mtx[[i]] = spare_mtx[[i]][!(spare_mtx[[i]] == i)]
}
namesofneigh = lapply(spare_mtx, function(n){counties$COUNTY[n]})
names(namesofneigh) = countyhurlingwins$COUNTY

list.queen<-poly2nb(countyhurlingwins, queen=TRUE)
Bin_W <- nb2listw(list.queen, style="B")
#W<-nb2listw(list.queen, style="W", zero.policy=TRUE)


sar.chi<-lagsarlm(Winners~ Population, data=countyhurlingwins, Bin_W)
summary(sar.chi)
predict.sarlm(sar.chi)
countyhurlingwins$fitted  =predict(sar.chi)[1:32]

ggplot(countyhurlingwins,aes(fill=fitted)) + geom_sf(lwd=0.2) +
  scale_fill_viridis_c(name='Fitted\nValue',direction=-1)
scale(countyhurlingwins$Population)


AIC(sar.chi)
ggplot(countyhurlingwins,aes(x=Winners,y=fitted)) + 
  geom_point() + 
  geom_smooth(method=MASS::rlm,se = FALSE) + coord_equal()