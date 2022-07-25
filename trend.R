shapiro.test(trend$spue) #normality check
hist(trend$spue)
library("ggplot2")

mod<- glm(spue~year+poly(season, 2), data = trend, family = poisson) #investigating quadratic relationship between spue and month
summary(mod) #no quadratic relationship 

mod1<- glm(spue~year+as.factor(season), data = trend, family = poisson) #glm
summary(mod1)
model.1 <- glm(`number of minke whale sightings` ~ year + as.factor(season), offset = log(`number of surveys`), family = poisson(link = "log"), data = trend)
summary(model.1) #overdispersion in the data

par(mfrow=c(2,2)) #checking if data meets model assumptions
plot(mod1)
plot(model.1)


model.2 <- glm(`number of minke whale sightings` ~ year + as.factor(season), offset = log(`number of surveys`), family = quasipoisson(link = "log"), data = trend)
summary(model.2)

#graphs 
year_labels<-c('2008','2009', '2010', '2011','2012','2013','2014','2015','2016','2017','2018') #custom labels for x axis
trend$month<-as.character(trend$month)
trend$month_title<-trend$month

model.3 <- glm(`number.of.minke.whale.sightings` ~ year, offset =
                 log(`number.of.surveys`), family = quasipoisson(link = "log"), data = trend) #overall trend only containing time as a factor

ypredict.3 <- predict(model.3, type="response", se = F,
                      newdata=data.frame(year=1:10,number.of.surveys=rep(1,10)))

dat.3 = data.frame(x=x,y=ypredict.3)

ggplot() +
  geom_point(data=trend, aes(x = year, y = spue, shape=season), size = 2)+
  theme_bw()+
  labs(x='Year', y='Sightings per unit effort', fill='Months')+
  scale_x_continuous(labels=year_labels, breaks=seq(0, 10, by=1))+
  
  scale_shape_manual(values=1:4, name='Month',labels=c("April-May", 'June-
July', 'August-September', "October-November"))+
  
  geom_line(data=dat.3,aes(x=x, y=y), color='red')
