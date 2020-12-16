
library(ggplot2)


# pull in different plot areas and graph them.
m40<-read.csv("output_data/OSBS/40m_plot_area.csv")
m100<-read.csv("output_data/OSBS/100m_plot_area.csv")
m250<-read.csv("output_data/OSBS/250m_plot_area.csv")
m500<-read.csv("output_data/OSBS/500m_plot_area.csv")
#
OSBS<-rbind(m40, m100, m250, m500)
OSBS$site<-"OSBS"
###############################
m40<-read.csv("output_data/UNDE/40m_plot_area.csv")
m100<-read.csv("output_data/UNDE/100m_plot_area.csv")
m250<-read.csv("output_data/UNDE/250m_plot_area.csv")
m500<-read.csv("output_data/UNDE/500m_plot_area.csv")
#
UNDE<-rbind(m40, m100, m250)
UNDE$site<-"UNDE"
###############################
m10<-read.csv("output_data/WREF/40m_plot_area.csv")
m40<-read.csv("output_data/WREF/40m_plot_area.csv")
m100<-read.csv("output_data/WREF/100m_plot_area.csv")
m250<-read.csv("output_data/WREF/250m_plot_area.csv")
m500<-read.csv("output_data/WREF/500m_plot_area.csv")
#
WREF<-rbind(m40, m100, m250, m500)
WREF$site<-"WREF"

fsd<-rbind(WREF, OSBS, UNDE)

fsd$plot_area<-factor(fsd$plot_area, levels=c("40 m","100 m", "250 m","500 m"))
table(fsd$metric)

head(fsd)
library(tidyr)
spr<-spread(fsd, "metric","value")
names(spr)

sp<-spr[ ,c(2,3,4,5,6,7,10,11,12,14)]

head(sp)

use<-gather(sp, "metric","value",5:10)

library(ggplot2)
ggplot(use, aes(x=plot_area, y=value+0.1, fill=site))+geom_boxplot()+facet_wrap(~metric, scales="free_y")+scale_y_log10()+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(text=element_text(size=25))



write.csv(use, file="output_data/combined_scaling_data.csv")
