

# pull in different plot areas and graph them.
m40<-read.csv("output_data/40m_plot_area.csv")
m100<-read.csv("output_data/100m_plot_area.csv")
m250<-read.csv("output_data/250m_plot_area.csv")
m500<-read.csv("output_data/500m_plot_area.csv")



fsd<-rbind(m40,m100, m250,m500)
head(fsd)
ggplot(fsd, aes(x=plot_area, y=value))+geom_boxplot()+facet_wrap(~metric, scales="free_y")


