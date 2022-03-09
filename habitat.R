library("mgcv")
library("DHARMa")

habitat$sediment<-as.factor(habitat$sediment)
cor.test(habitat$depth, habitat$sst)#collinearity
cor.test(habitat$depth, habitat$chlorophyll)
cor.test(habitat$chlorophyll, habitat$sst)
concurvity(mod1, full = TRUE) #concurvity

mod1 <- gam(status ~ s(depth, k=5)+s(sst, k=5)+s(chlorophyll, k=5)+sediment, data=habitat, family=binomial(link = "logit"), method = "REML") #full model
summary(mod1)

mod2 <- gam(status ~ s(depth, k=5)+s(sst, k=5)+s(chlorophyll, k=5), data=habitat, family=binomial(link = "logit"), method = "REML")
summary(mod2) #model simplification - sediment removed

mod3 <- gam(status ~ s(depth, k=5)+s(sst, k=5), data=habitat, family=binomial(link = "logit"), method = "REML")
summary(mod3) #model simplification - sediment and chlorophyll removed

AIC(mod1,mod2,mod3) #AIC test - identifying the best-fit model
simulateResiduals(mod1, plot = T) #checking if data meets model assumptions

#creating graphs
plot(mod1, all.terms = TRUE, pages = 1, shade=TRUE, trans = plogis, select=1, xlab="Depth (m)") #graphs
plot(mod1, all.terms = TRUE, pages = 1, shade=TRUE, trans = plogis, select=2, xlab="Sea Surface Temperature (°C)")
plot(mod1, all.terms = TRUE, pages = 1, shade=TRUE, trans = plogis, select=3, xlab="Chlorophyll a (mg/m3)")
plot(mod1, all.terms = TRUE, pages = 1, shade=TRUE, trans = plogis, select=4, xlab="Sediment")
