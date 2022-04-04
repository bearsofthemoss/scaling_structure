

############### Packages ####################################################
library(neonUtilities)
library(lidR)
library(gstat)
library(rgdal)
library(rgeos)
library(data.table)
library(ggplot2)
library(tidyr)



########## Base it on the flux tower location?
# OSBS tower coordinates
osbs_tow<-c(403886.42, 3284767.49)
easting<-osbs_tow[1]
northing<-osbs_tow[2]


unde_tow<-c(5123162.89, 304366.95)
easting<-unde_tow[1]
northing<-unde_tow[2]

wref_tow<-c(5074636.87, 581417.80)
easting<-wref_tow[1]
northing<-wref_tow[2]


# download tile of aerial lidar that contains the tower
byTileAOP("DP1.30003.001", site="OSBS", year="2019", check.size = T,buffer = 900,
          easting=easting, northing=northing, savepath="neon_data")


# read in LAz files
### make a list of the files
osbsL<-list.files(path="neon_data\\DP1.30003.001\\2019\\FullSite\\D03\\2019_OSBS_5\\L1\\DiscreteLidar\\ClassifiedPointCloud", recursive = T, full.names = T)
undeL<-list.files(path="neon_data\\DP1.30003.001\\2019\\FullSite\\D05\\2019_UNDE_3\\L1\\DiscreteLidar\\ClassifiedPointCloud", recursive = T, full.names = T)
wrefL<-list.files(path="neon_data\\DP1.30003.001\\2019\\FullSite\\D16\\2019_WREF_3\\L1\\DiscreteLidar\\ClassifiedPointCloud", recursive = T, full.names = T)


# use 'readLAS' to read in the files
laz<-readLAS(wrefL)
plot(laz)
laz<-readLAS(undeL)
laz<-readLAS(undeL)

plot(laz)

## get the center of the lidar files
ext<-extent(laz)
ext
center<-c( (ext[1]+ext[2])/2 , (ext[3]+ext[4])/2 )


## for troubleshooting the grid
#center<-c(0,0)

## specify plot area plot areas for the kilometer of LiDAR data. 
# for 500
a<-250
lon<-seq(as.numeric((center[1]-300)), as.numeric((center[1]+300)) , sqrt((a^2)+(a^2)))
lat<-seq(as.numeric((center[2]-300)), as.numeric((center[2]+300)),  sqrt((a^2)+(a^2)))
coord<-as.data.frame(expand.grid(lon, lat))
coord
coord$area<-paste(a,"m")


plot(coord$Var1, coord$Var2)

#########################
#lon<-seq(as.numeric((center[1]-(500-(a/5)))), as.numeric((center[1]+(500-(a/5)))) , 2*sqrt((a^2)+(a^2)))
#lat<-seq(as.numeric((center[2]-(500-(a/5)))), as.numeric((center[2]+(500-(a/5)))),  2*sqrt((a^2)+(a^2)))
# coord10<-coord
# coord40<-coord
# coord100<-coord
# coord250<-coord
# coord500<-coord 
# 
# par(mfrow=c(3,2))
# plot(coord10$Var1, coord10$Var2, main="10 m plot area")
# plot(coord40$Var1, coord40$Var2, main="40 m plot area")
# plot(coord100$Var1, coord100$Var2, main="100 m plot area")
# plot(coord250$Var1, coord250$Var2, main="250 m plot area")
# plot(coord500$Var1, coord500$Var2, main="500 m plot area")


length(coord$Var1)

max(coord$Var1)-min(coord$Var1)
max(coord$Var2)-min(coord$Var2)

######################################################################
# the Loop
    #  Credit to Liz LaRue and the NEON tutorial this came from!
######################################################################


plot.metrics<-list()
a<-25 #plot area
for(i in c(1:length(coord$Var1))){    # the loop only goes for the first 4 rows because the first 4 use the C1laz. rows 5-8 need C2laz. 9-12 need C3laz.
  center<-coord[i, ]

  use.laz<-laz # this is where I set the loop to use the laz file 

# You shouldn't have to change anything below.
# select the xy location of the plot we are calculating canopy metrics for
x<-as.numeric(center[1])
y<-as.numeric(center[2])


#Cut out a 600 x 600 m buffer by adding 300 m to easting and northing coordinates (x,y).
data.cut <- lasclipRectangle( use.laz , xleft = (as.numeric(x - (a+100))), ybottom = (as.numeric(y - (a+100))),xright = (as.numeric(x + (a+100))), ytop = (as.numeric(y + (a+100))))

#Correct for ground height using a kriging function to interpolate elevation from ground points in the .laz file.
#If the function will not run, then you may need to checkfor outliers by adjusting the 'drop_z_' arguments when reading in the .laz files.
dtm <- grid_terrain(data.cut, 1, kriging(k = 10L))
data.200m<- lasnormalize(data.cut, dtm)
  #(this is called 200m but its actually 600m)
#plot(data.200m)

# Because edge effects are nasty (apparently), we'll further clip this to be 500 x 500 m
data.30m <- lasclipRectangle(data.200m, xleft = (x - a/2), ybottom = (y - a/2), xright = (x + a/2), ytop = (y + a/2))
  # again, this is called 30m but its actually whatever 'a' is set to
data.30m@data$Z[data.30m@data$Z <= .5] <- NA  


# Now run the metric calculations on the object 'laz_data'
laz_data<-data.30m
#  plot(laz_data)
structural_diversity_metrics <- function(laz_data) {
  chm <- grid_canopy(laz_data, res = 1, dsmtin()) 
  mean.max.canopy.ht <- mean(chm@data@values, na.rm = TRUE) 
  max.canopy.ht <- max(chm@data@values, na.rm=TRUE) 
  rumple <- rumple_index(chm) 
  top.rugosity <- sd(chm@data@values, na.rm = TRUE) 
  cells <- length(chm@data@values) 
  chm.0 <- chm
  chm.0[is.na(chm.0)] <- 0 
  zeros <- which(chm.0@data@values == 0) 
  deepgaps <- length(zeros) 
  deepgap.fraction <- deepgaps/cells 
  cover.fraction <- 1 - deepgap.fraction 
  vert.sd <- cloud_metrics(laz_data, sd(Z, na.rm = TRUE)) 
  sd.1m2 <- grid_metrics(laz_data, sd(Z), 1) 
  sd.sd <- sd(sd.1m2[,3], na.rm = TRUE) 
  Zs <- laz_data@data$Z
  Zs <- Zs[!is.na(Zs)]
  entro <- entropy(Zs, by = 1) 
  gap_frac <- gap_fraction_profile(Zs, dz = 1, z0=3)
  GFP.AOP <- mean(gap_frac$gf) 
  LADen<-LAD(Zs, dz = 1, k=0.5, z0=3) 
  VAI.AOP <- sum(LADen$lad, na.rm=TRUE) 
  VCI.AOP <- VCI(Zs, by = 1, zmax=100) 
  out.plot <- data.frame(
    matrix(c(x, y, mean.max.canopy.ht,max.canopy.ht, 
             rumple,deepgaps, deepgap.fraction, 
             cover.fraction, top.rugosity, vert.sd, 
             sd.sd, entro, GFP.AOP, VAI.AOP,VCI.AOP),
           ncol = 15)) 
  colnames(out.plot) <- 
    c("easting", "northing", "mean.max.canopy.ht.aop",
      "max.canopy.ht.aop", "rumple.aop", "deepgaps.aop",
      "deepgap.fraction.aop", "cover.fraction.aop",
      "top.rugosity.aop","vert.sd.aop","sd.sd.aop", 
      "entropy.aop", "GFP.AOP.aop",
      "VAI.AOP.aop", "VCI.AOP.aop") 
  print(out.plot)
}

plot.metrics[[i]]<-structural_diversity_metrics(laz_data)

}  # END LOOP

plot.metrics

pm25<-plot.metrics


# so Far AY has made 200, 150, 100, 50 , 10, 175, 125,  75, 25 




# post-loop processing
plot.10<-as.data.frame(rbindlist(pm10))
plot.10$plot_area<-10

plot.25<-as.data.frame(rbindlist(pm25))
plot.25$plot_area<-25

plot.50<-as.data.frame(rbindlist(pm50))
plot.50$plot_area<-50

plot.75<-as.data.frame(rbindlist(pm75))
plot.75$plot_area<-75

plot.100<-as.data.frame(rbindlist(pm100))
plot.100$plot_area<-100

plot.125<-as.data.frame(rbindlist(pm125))
plot.125$plot_area<-125

plot.150<-as.data.frame(rbindlist(pm150))
plot.150$plot_area<-150


plot.175<-as.data.frame(rbindlist(pm175))
plot.175$plot_area<-175

plot.200<-as.data.frame(rbindlist(pm200))
plot.200$plot_area<-200

plo<-rbind(plot.10, plot.25, plot.50, plot.75, plot.100,
plot.125, plot.150, plot.175, plot.200)

head(plo)

plo$plot<-rep(1:4,9)



# graph to see the 13 metrics
g<-gather(plo, "metric","value",3:15)
g

write.csv(g, file="WREF_25m intervals.csv")

ggplot(g, aes(x=plot_area, y=value, group=plot))+ geom_point()+geom_line()+
theme_classic()+facet_wrap(~metric, scales="free_y", nrow=2)+   theme(text=element_text(size=18))

p<-aggregate(g$value, by=list(metric=g$metric, plot_area=g$plot_area),FUN="sd")

ggplot(p, aes(x=plot_area, y=x))+ geom_point()+geom_line()+
  theme_classic()+facet_wrap(~metric, scales="free_y", nrow=2)+   theme(text=element_text(size=18))

