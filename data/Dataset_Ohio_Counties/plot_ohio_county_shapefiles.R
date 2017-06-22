# plot_ohio_county_shapefiles.r 6/21/2017 CJD
#
# plots ohio cases using the county shapefiles in R
#
library(sp)
library(rgdal)
library(ggplot2)
library(maptools)
library(rgeos)
library(scales)
library(RColorBrewer)


#First the simple way in 2 steps with units in shapefile
sf0=readShapeSpatial("cb_2015_us_county_500k.shp",proj4string=CRS("+proj=utm +zone=12+ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
sf<-subset(sf0,sf0$STATEFP==39)
sf$unnamed=sf$unnamed-446 #offset the number after pulling out Ohio's
sftransformed=spTransform(sf,CRS("+proj=longlat"))
#plot(sftransformed,axes=T)

# use the fortify function to put the shapefile object into a data frame for ggplot
sf.shp.f<-fortify(sftransformed,regions="unnamed")

#merge in ohio data
ohio_data<-read.csv(file="county_data_combined.csv")
sf.shp.f$id=as.numeric(sf.shp.f$id)-445 #offset the number after again
merge.shp.coef<-merge(sf.shp.f,ohio_data,by.x="id",by.y="ID",all.x=TRUE)
final.plot<-merge.shp.coef[order(merge.shp.coef$order),]

# create a set of labels
cnames<-aggregate(cbind(long,lat) ~ id,data=sf.shp.f,FUN=function(x) mean(range(x)))

# now plot out the map
# change the fill= value to whatever you want to plot in the final.plot variables
ggplot() +
  geom_polygon(data=final.plot,
               aes(x=long,y=lat,group=group,fill=Overdoses.2015),
               color="black",size=0.25) +
  coord_map()+
  # don't forget to change the label on the legend
  scale_fill_distiller(name="2015 Drug Overdoses",palette="Reds",breaks=pretty_breaks(n=4))+
  labs(title="Ohio Counties")+
  geom_text(data=cnames,aes(long,lat,label=sftransformed$NAME),size=1,fontface="bold")
#



  

