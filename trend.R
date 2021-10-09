library("ggplot2")
library("lme4")

shapiro.test(trend$spue) #normality check
hist(trend$spue)

mod<- glm(spue~year+poly(month, 2), data = trend, family = poisson) #investigating quadratic relationship between spue and month
summary(mod) #no quadratic relationship 

mod1<- glm(spue~year+as.factor(month), data = trend, family = poisson) #glm
summary(mod1)

par(mfrow=c(2,2)) #checking if data meets model assumptions
plot(mod1)


#graphs 
year_labels<-c('2008','2009', '2010', '2011','2012','2013','2014','2015','2016','2017','2018') #custom labels for x axis
trend$month<-as.character(trend$month)
trend$month_title<-trend$month

#change month names from numbric for the legend 
trend['month_title'][trend['month_title']=='4']<-'April'
trend['month_title'][trend['month_title']=='5']<-'May'
trend['month_title'][trend['month_title']=='6']<-'June'
trend['month_title'][trend['month_title']=='7']<-'July'
trend['month_title'][trend['month_title']=='8']<-'August'
trend['month_title'][trend['month_title']=='9']<-'September'
trend['month_title'][trend['month_title']=='10']<-'October'
trend['month_title'][trend['month_title']=='11']<-'November'

#calculate the points for trendline
x<-seq(1:10)
y<-plogis(coef(mod1)[1]+coef(mod1)[2]*x)  

#create plot
ggplot(trend, aes(x = year, y = spue, shape=month)) + 
  geom_point(size = 2) +  
  theme_bw()+
  labs(x='Year', y='Sightings per unit effort', fill='Month')+
  scale_x_continuous(labels=year_labels, breaks=seq(0, 10, by=1))+
  geom_segment(aes(x=1,y=0.5867881,xend=10,yend=0.3334756), color='black') +
  scale_shape_manual(values=1:8, name='Month', 
                   labels=c("April", 'May', 'June', 'July', 'July', 'August', 'September',"October", "November"))

