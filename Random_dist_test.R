source('0_source.R')
BigClubs = read.csv('Input_data/Scrapped/CountyWinners.csv')
BigClubs  = st_as_sf(BigClubs,coords = c("Longitude","Latitude"))
st_crs(BigClubs) = 4326

dist_matrix = st_distance(BigClubs, BigClubs) 
diag(dist_matrix) <- NA

dist_matrix =as.data.frame(dist_matrix)


BigClubstest = as.data.frame(BigClubs)

indexnum = t(apply(dist_matrix, 1, order))
neighdist = t(apply(dist_matrix,1,sort))

neighdist=cbind(neighdist,c(1:114))

numneigh = 1
distneigh = matrix(nrow=nrow(BigClubstest), ncol=numneigh)
neighbours = matrix(nrow=nrow(BigClubstest), ncol=numneigh)
for (i in 1:nrow(BigClubstest)){
  neighbours[i,1:numneigh] = BigClubstest$Club[indexnum[,i]][1:numneigh]
  distneigh[i, 1:numneigh] = neighdist[i,1:numneigh]
}

meandist =apply(distneigh,1,mean)


#For a random process on a 2D plane, 
#the probability density of 2*pi*lambda*(d_i)^2 is chi squared with 2 dof
#Where d_i is the distance from point i to its nearest neighbour 
#and lambda is the density of points per unit area - (Skellam, 1951)

lambda <- length(meandist)/sum(st_area(countyhurlingwins))
test_stat <- 2 * pi * lambda * sum(meandist^2)
glue::glue(
  " Test statistic = {round(test_stat,1)}
  p-value = {round(pchisq(test_stat,2*length(meandist)),2)}")
#So I plan to run similar analysis on a county by county basis.