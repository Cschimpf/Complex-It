# plot_wales_shapefiles.r 6/20/2017 CJD
#
# plots Wales cases using the shapefiles and deprivation data
#
library(sp)
library(rgdal)
library(ggplot2)
library(maptools)
library(mapproj)
library(rgeos)
library(scales)
library(RColorBrewer)



#Read in the deprivation data
data_depr<-read.csv("WalesIMD2014-2016Change.csv");

#Read in the shapefiles of wales administrative areas 
sf1=readOGR("WD_2011_BFE_Blaenau_Gwent.shp")
sf1=readOGR("WD_2011_BFE_Blaenau_Gwent.shp")
sf2=readOGR("WD_2011_BFE_Bridgend.shp")
sf3=readOGR("WD_2011_BFE_Caerphilly.shp")
sf4=readOGR("WD_2011_BFE_Cardiff.shp")
sf5=readOGR("WD_2011_BFE_Carmarthenshire.shp")
sf6=readOGR("WD_2011_BFE_Ceredigion.shp")
sf7=readOGR("WD_2011_BFE_Conwy.shp")
sf8=readOGR("WD_2011_BFE_Denbighshire.shp")
sf9=readOGR("WD_2011_BFE_Flintshire.shp")
sf10=readOGR("WD_2011_BFE_Gwynedd.shp")
sf11=readOGR("WD_2011_BFE_Isle_of_Anglesey.shp")
sf12=readOGR("WD_2011_BFE_Merthyr_Tydfil.shp")
sf13=readOGR("WD_2011_BFE_Monmouthshire.shp")
sf14=readOGR("WD_2011_BFE_Neath_Port_Talbot.shp")
sf15=readOGR("WD_2011_BFE_Newport.shp")
sf16=readOGR("WD_2011_BFE_Pembrokeshire.shp")
sf17=readOGR("WD_2011_BFE_Powys.shp")
sf18=readOGR("WD_2011_BFE_Rhondda_Cynon_Taf.shp")
sf19=readOGR("WD_2011_BFE_Swansea.shp")
sf20=readOGR("WD_2011_BFE_Torfaen.shp")
sf21=readOGR("WD_2011_BFE_Vale_of_Glamorgan.shp")
sf22=readOGR("WD_2011_BFE_Wrexham.shp")

#Transform them
sf1transformed=spTransform(sf1,CRS("+proj=longlat"))
sf2transformed=spTransform(sf2,CRS("+proj=longlat"))
sf3transformed=spTransform(sf3,CRS("+proj=longlat"))
sf4transformed=spTransform(sf4,CRS("+proj=longlat"))
sf5transformed=spTransform(sf5,CRS("+proj=longlat"))
sf6transformed=spTransform(sf6,CRS("+proj=longlat"))
sf7transformed=spTransform(sf7,CRS("+proj=longlat"))
sf8transformed=spTransform(sf8,CRS("+proj=longlat"))
sf9transformed=spTransform(sf9,CRS("+proj=longlat"))
sf10transformed=spTransform(sf10,CRS("+proj=longlat"))
sf11transformed=spTransform(sf11,CRS("+proj=longlat"))
sf12transformed=spTransform(sf12,CRS("+proj=longlat"))
sf13transformed=spTransform(sf13,CRS("+proj=longlat"))
sf14transformed=spTransform(sf14,CRS("+proj=longlat"))
sf15transformed=spTransform(sf15,CRS("+proj=longlat"))
sf16transformed=spTransform(sf16,CRS("+proj=longlat"))
sf17transformed=spTransform(sf17,CRS("+proj=longlat"))
sf18transformed=spTransform(sf18,CRS("+proj=longlat"))
sf19transformed=spTransform(sf19,CRS("+proj=longlat"))
sf20transformed=spTransform(sf20,CRS("+proj=longlat"))
sf21transformed=spTransform(sf21,CRS("+proj=longlat"))
sf22transformed=spTransform(sf22,CRS("+proj=longlat"))

# use the fortify function to put the shapefile object into a data frame for ggplot
sf1.shp.f<-fortify(sf1transformed,regions="sf1transformed$WD11NM")
sf2.shp.f<-fortify(sf2transformed,regions="sf2transformed$WD11NM")
sf3.shp.f<-fortify(sf3transformed,regions="sf3transformed$WD11NM")
sf4.shp.f<-fortify(sf4transformed,regions="sf4transformed$WD11NM")
sf5.shp.f<-fortify(sf5transformed,regions="sf5transformed$WD11NM")
sf6.shp.f<-fortify(sf6transformed,regions="sf6transformed$WD11NM")
sf7.shp.f<-fortify(sf7transformed,regions="sf7transformed$WD11NM")
sf8.shp.f<-fortify(sf8transformed,regions="sf8transformed$WD11NM")
sf9.shp.f<-fortify(sf9transformed,regions="sf9transformed$WD11NM")
sf10.shp.f<-fortify(sf10transformed,regions="sf10transformed$WD11NM")
sf11.shp.f<-fortify(sf11transformed,regions="sf11transformed$WD11NM")
sf12.shp.f<-fortify(sf12transformed,regions="sf12transformed$WD11NM")
sf13.shp.f<-fortify(sf13transformed,regions="sf13transformed$WD11NM")
sf14.shp.f<-fortify(sf14transformed,regions="sf14transformed$WD11NM")
sf15.shp.f<-fortify(sf15transformed,regions="sf15transformed$WD11NM")
sf16.shp.f<-fortify(sf16transformed,regions="sf16transformed$WD11NM")
sf17.shp.f<-fortify(sf17transformed,regions="sf17transformed$WD11NM")
sf18.shp.f<-fortify(sf18transformed,regions="sf18transformed$WD11NM")
sf19.shp.f<-fortify(sf19transformed,regions="sf19transformed$WD11NM")
sf20.shp.f<-fortify(sf20transformed,regions="sf20transformed$WD11NM")
sf21.shp.f<-fortify(sf21transformed,regions="sf21transformed$WD11NM")
sf22.shp.f<-fortify(sf22transformed,regions="sf22transformed$WD11NM")

# Assign a number to be color coded from the deprivation data set, for example cluster once you have one
# select a column first and don't forget to change the plot label below accordingly as desired
datacolumn=9
sf1.shp.f$order=data_depr[1,datacolumn];
sf2.shp.f$order=data_depr[2,datacolumn];
sf3.shp.f$order=data_depr[3,datacolumn];
sf4.shp.f$order=data_depr[4,datacolumn];
sf5.shp.f$order=data_depr[5,datacolumn];
sf6.shp.f$order=data_depr[6,datacolumn];
sf7.shp.f$order=data_depr[7,datacolumn];
sf8.shp.f$order=data_depr[8,datacolumn];
sf9.shp.f$order=data_depr[9,datacolumn];
sf10.shp.f$order=data_depr[10,datacolumn];
sf11.shp.f$order=data_depr[11,datacolumn];
sf12.shp.f$order=data_depr[12,datacolumn];
sf13.shp.f$order=data_depr[13,datacolumn];
sf14.shp.f$order=data_depr[14,datacolumn];
sf15.shp.f$order=data_depr[15,datacolumn];
sf16.shp.f$order=data_depr[16,datacolumn];
sf17.shp.f$order=data_depr[17,datacolumn];
sf18.shp.f$order=data_depr[18,datacolumn];
sf19.shp.f$order=data_depr[19,datacolumn];
sf20.shp.f$order=data_depr[20,datacolumn];
sf21.shp.f$order=data_depr[21,datacolumn];
sf22.shp.f$order=data_depr[22,datacolumn];


# now plot out the map
ggplot() +
  geom_polygon(data=sf1.shp.f,
               aes(x=long,y=lat,group=group,fill=order),
               color="black",size=0.25) +
  geom_polygon(data=sf2.shp.f,
               aes(x=long,y=lat,group=group,fill=order),
               color="black",size=0.25) +  coord_map()+
  geom_polygon(data=sf3.shp.f,
               aes(x=long,y=lat,group=group,fill=order),
               color="black",size=0.25) +  coord_map()+  
  geom_polygon(data=sf4.shp.f,
               aes(x=long,y=lat,group=group,fill=order),
               color="black",size=0.25) +  coord_map()+  
  geom_polygon(data=sf5.shp.f,
               aes(x=long,y=lat,group=group,fill=order),
               color="black",size=0.25) +  coord_map()+  
  geom_polygon(data=sf6.shp.f,
               aes(x=long,y=lat,group=group,fill=order),
               color="black",size=0.25) +  coord_map()+  
  geom_polygon(data=sf7.shp.f,
               aes(x=long,y=lat,group=group,fill=order),
               color="black",size=0.25) +  coord_map()+  
  geom_polygon(data=sf8.shp.f,
               aes(x=long,y=lat,group=group,fill=order),
               color="black",size=0.25) +  coord_map()+  
  geom_polygon(data=sf9.shp.f,
               aes(x=long,y=lat,group=group,fill=order),
               color="black",size=0.25) +  coord_map()+  
  geom_polygon(data=sf10.shp.f,
               aes(x=long,y=lat,group=group,fill=order),
               color="black",size=0.25) +  coord_map()+  
  geom_polygon(data=sf11.shp.f,
               aes(x=long,y=lat,group=group,fill=order),
               color="black",size=0.25) +  coord_map()+  
  geom_polygon(data=sf12.shp.f,
               aes(x=long,y=lat,group=group,fill=order),
               color="black",size=0.25) +  coord_map()+  
  geom_polygon(data=sf13.shp.f,
               aes(x=long,y=lat,group=group,fill=order),
               color="black",size=0.25) +  coord_map()+  
  geom_polygon(data=sf14.shp.f,
               aes(x=long,y=lat,group=group,fill=order),
               color="black",size=0.25) +  coord_map()+  
  geom_polygon(data=sf15.shp.f,
               aes(x=long,y=lat,group=group,fill=order),
               color="black",size=0.25) +  coord_map()+  
  geom_polygon(data=sf16.shp.f,
               aes(x=long,y=lat,group=group,fill=order),
               color="black",size=0.25) +  coord_map()+  
  geom_polygon(data=sf17.shp.f,
               aes(x=long,y=lat,group=group,fill=order),
               color="black",size=0.25) +  coord_map()+  
  geom_polygon(data=sf18.shp.f,
               aes(x=long,y=lat,group=group,fill=order),
               color="black",size=0.25) +  coord_map()+  
  geom_polygon(data=sf19.shp.f,
               aes(x=long,y=lat,group=group,fill=order),
               color="black",size=0.25) +  coord_map()+  
  geom_polygon(data=sf20.shp.f,
               aes(x=long,y=lat,group=group,fill=order),
               color="black",size=0.25) +  coord_map()+  
  geom_polygon(data=sf21.shp.f,
               aes(x=long,y=lat,group=group,fill=order),
               color="black",size=0.25) +  coord_map()+  
  geom_polygon(data=sf22.shp.f,
               aes(x=long,y=lat,group=group,fill=order),
               color="black",size=0.25) +  coord_map()+  
  scale_fill_distiller(name="Deaths 2016",palette="Reds",breaks=pretty_breaks(n=4))+
  labs(title="Wales Administrative Wards UOLA")
#

