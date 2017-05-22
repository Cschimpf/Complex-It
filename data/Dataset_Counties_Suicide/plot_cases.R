# plot_cases.r 5/20/2017 CJD
#
# plots cases using the example county data in R
#
library(sp)
library(rgdal)
library(ggplot2)
library(maptools)
library(rgeos)
library(RColorBrewer)
#
# read the ohio county data with a column for clustered data
ohio_data<-read.csv(file="county_data_combined.csv")

#counties.shp=readShapeSpatial("cb_2015_us_county_500k.shp")
counties.shp=readOGR("cb_2015_us_county_500k.shp")

#get just the ohio counties
num.counties<-length(counties.shp$STATEFP)
counties_ohio.shp<-subset(counties.shp,counties.shp$STATEFP==39)

# create an ID field for the counties assuming they are in alphabetical order
#make sure the Shapefiles and the Data have the same ID number now
counties_ohio.shp$ID<ohio_data$ID

#merge the data and the shapefiles
merge.shp.coef<-merge(counties_ohio.shp.f,ohio_data,by.x="id",by.y="ID",all.x=TRUE)
final.plot<-merge.shp.coef[order(merge.shp.coef$order),]

#prepare a name area for the text plotting
cnames<-aggregate(cbind(long,lat)~County,data=final.plot,FUN=function(x) mean(range(x)))

#plot the figure with whatever column desired in fill=
# use display.brewer.all() to see the color schemes
ggplot() +
    geom_polygon(data=final.plot,
         aes(x=long,y=lat,group=group,fill=X2015.Death.Count),
         color="black",size=0.25) +
    coord_map()+
    scale_fill_distiller(name="percent",palette="RdYlGn")+
    #theme_nothing(legend=TRUE)+
    labs(title="Death Count in Ohio Counties")+
    geom_text(data=cnames,aes(long,lat,label=ohio_data$County),size=2,fontface="bold")
   

