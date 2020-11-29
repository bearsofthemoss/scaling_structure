

# pull in different plot areas and graph them.
m40<-read.csv("output_data/40m_plot_area.csv")
m100<-read.csv("output_data/100m_plot_area.csv")
m250<-read.csv("output_data/250m_plot_area.csv")
m500<-read.csv("output_data/250m_plot_area.csv")



fsd<-rbind(m100, m250,m500)

ggplot(fsd, aes(x=area, y=value))+geom_boxplot()+facet_wrap(~metric)

