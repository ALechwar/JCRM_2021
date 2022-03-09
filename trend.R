library("ggplot2")
library("lme4")

shapiro.test(trend$spue) #normality check
hist(trend$spue)

model.1 <- glm(`number of minke whale sightings` ~ year + as.factor(season), offset = log(`number of surveys`), family = poisson(link = "log"), data = trend)
summary(model.1) #overdispersion in the data

par(mfrow=c(2,2)) #checking if data meets model assumptions
plot(model.1)


model.2 <- glm(`number of minke whale sightings` ~ year + as.factor(season), offset = log(`number of surveys`), family = quasipoisson(link = "log"), data = trend)
summary(model.2)

#graphs 
year_labels<-c('2008','2009', '2010', '2011','2012','2013','2014','2015','2016','2017','2018') #custom labels for x axis
trend$season<-as.character(trend$season)
trend$season_title<-trend$season

#create plot
x<-seq(1:10)
y<-plogis(coef(model.1)[1]+coef(model.1)[2]*x) 

ggplot(trend, aes(x = year, y = spue, shape=season)) + 
  geom_point(size = 2) +  
  theme_bw()+
  labs(x='Year', y='Sightings per unit effort', fill='Month')+
  scale_x_continuous(labels=year_labels, breaks=seq(0, 10, by=1))+
  geom_segment(aes(x=1,y=0.6798772,xend=10,yend=0.41943591), color='black') +
  scale_shape_manual(values=1:4, name='Month', 
                     labels=c("April-May", 'June-July', 'August-September', "October-November"))
