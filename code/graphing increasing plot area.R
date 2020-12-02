
library(ggplot2)


# pull in different plot areas and graph them.
m40<-read.csv("output_data/40m_plot_area.csv")
m100<-read.csv("output_data/100m_plot_area.csv")
m250<-read.csv("output_data/250m_plot_area.csv")
m500<-read.csv("output_data/500m_plot_area.csv")



fsd<-rbind(m40,m100, m250,m500)
fsd$plot_area<-factor(fsd$plot_area, levels=c("40m","100 m", "250 m","500 m"))
table(fsd$metric)

head(fsd)
library(tidyr)
spr<-spread(fsd, "metric","value")
names(spr)

sp<-spr[ ,c(2,3,4,6,7,10,11,12,14)]

head(sp)

use<-gather(sp, "metric","value",4:9)


ggplot(use, aes(x=plot_area, y=value+0.1))+geom_boxplot()+facet_wrap(~metric, scales="free_y")+scale_y_log10()+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(text=element_text(size=25))


