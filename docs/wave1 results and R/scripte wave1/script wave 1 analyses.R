### wave 1 analysis


library(psych)
library(tidyverse)
library(rcompanion)
library(broom)
library(vegan)
library(sjPlot) # for tab_model
library(lmerTest); library(lme4)
library(interplot)

setwd("/Users/nfrings/Library/Mobile Documents/com~apple~CloudDocs/Documents/PhD/Winter studie/wave1 results and R")

#### DCE policy decision part ####
data.decision <- read_csv("data/data.decision.full")
data.decision <- data.decision[,-c(1)]
#### Model of policy decisions  with all demographics and poliitcal orientation

# with numeric not as factors
model.dec.all.n <- glmer(decision~ co2 + tax + energieabhaengigkeit + zeitpunkt + gender + age + income + country + education + country +  politicalorientation_1 + (1|id), 
                         data=data.decision, family="binomial")
summary(model.dec.all.n)

tab_model(model.dec.all.n)

#### H1 :  controlled for age, gender, country of origin and income as per pre-registration

model.H1 <- glmer(decision~ co2 + tax + energieabhaengigkeit + zeitpunkt + gender + age + income + country  + (1|id),
                  data=data.decision, family="binomial")
summary(model.H1)
tab_model(model.H1)

model.H1.age <- glmer(decision~ co2 + tax + energieabhaengigkeit + zeitpunkt + age  + (1|id),
                  data=data.decision, family="binomial")
summary(model.H1.age)
tab_model(model.H1.age)

# without any demographic predictors
model.H1 <- glmer(decision~ co2 + tax + energieabhaengigkeit + zeitpunkt  + (1|id),
                      data=data.decision, family="binomial")
summary(model.H1)
tab_model(model.H1)

# without dem but with random slope and intercept
model.H1.random_si <- glmer(decision~ co2 + tax + energieabhaengigkeit + zeitpunkt  +
                             (1 + co2 + tax + energieabhaengigkeit + zeitpunkt | id) + (1|id),
                  data=data.decision, family="binomial")
summary(model.H1.random_si)
tab_model(model.H1.random_si)

# only random slopes no random intercept?
model.H1.random_s <- glmer(decision~ co2 + tax + energieabhaengigkeit + zeitpunkt  +
                              (1 + co2 + tax + energieabhaengigkeit + zeitpunkt | id),
                            data=data.decision, family="binomial")
summary(model.H1.random_s)
tab_model(model.H1.random_s)

# random slopes and demographics
data.decision$household_size.f <- factor(data.decision$household_size)
model.H1.random_s.d <- glmer(decision~ co2 + tax + energieabhaengigkeit + zeitpunkt  +
                              gender + age + income + country  +
                             (1 + co2 + tax + energieabhaengigkeit + zeitpunkt | id),
                           data=data.decision, family="binomial")
summary(model.H1.random_s.d)
tab_model(model.H1.random_s.d)

## H1 a
# co2 = 2, tax=3, energy = 4, zeit=5
# with data from H1c maybe
# based on:
#glmer(decision~ co2 + tax + energieabhaengigkeit + zeitpunkt  + (1| id) + (1 + co2 + tax + energieabhaengigkeit + zeitpunkt | id), data = data.decision,family = binomial(link = "logit"))

#summary(model.H1b.no.dem)
#tab_model(model.H1b.no.dem)

data.beta <- read.csv("data/ModelH1bAgents.n.csv")
data.beta <- data.beta %>%
  rename( co2.beta= "co2.30..CO2",
          tax.beta= "tax.6.",
          energy.beta = "energieabhaengigkeit.20."  ,
          time.beta =  "zeitpunktin.7.Jahren"     )

# hypothesis co2 and tax
#tax versus all others
t.test( data.beta$tax.beta, data.beta$energy.beta , paired=T, var.equal = T)
t.test( data.beta$tax.beta, data.beta$co2.beta , paired=T, var.equal = T)
t.test( data.beta$tax.beta, data.beta$time.beta , paired=T, var.equal = T)

# co2 
describe(data.beta$co2.beta)
# mean = 0.49, sd= 0.35 
describe(data.beta$tax.beta)
# mean = -2.6, sd= 2.19
describe(data.beta$energy.beta)
# mean = 0.29 , sd= 0.23
describe(data.beta$time.beta)
# mean = -1.25 , sd= 2.01

### trying to see distribution, not sure what test should do
ggplot(data.decision, aes(x=zeitpunkt, y=decision, color=tax)) +
  geom_jitter(width=.1, height=.1, alpha=0.4) +
  scale_color_manual(values=c("darkgreen", "darkorange")) +
  theme_minimal()

data_correlation <- data.decision[, c("co2.n", "tax.n" , "energieabhaengigkeit.n" , "zeitpunkt.n", "decision" )]
correlation <- rcorr(as.matrix(data_correlation))
correlation$r

res <- summary(model.H1.random_si)$residuals

plot(fitted(model.H1.random_si), res)

#create Q-Q plot for residuals
qqnorm(res)
#add a straight diagonal line to the plot
qqline(res)

plot(density(res))
plot(model.H1,which=1)
library("arm")
binnedplot(fitted(model.H1.random_si), 
           residuals(model.H1.random_si, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")
#
library(pROC); library(plotROC)
invisible(plot(roc(factor(ifelse(data.decision$response == "Dafür", 1, 0)), fitted(model.H1.random_si)), print.thres = c(.1, .5), col = "red", print.auc = T))

invisible(plot(roc(factor(ifelse(data.decision$response == "Dafür", 1, 0)), fitted(model.H1.random_si)),
               col = "red", 
               main = "ROC curves: logistic model 1 (red) vs. logistic model 2 (blue)"))

invisible(plot(roc(data.decision$decision,
                   fitted(model.H1)),
               print.auc = T, 
               col = "blue", 
               add = T))




## H1b 
## climate change concern : overall effect plus interaction between concern and implementation time factor
data.decision$concern.scaled.f <- factor(data.decision$concern.scaled, 
                                         levels= c(-3.75, -3.25,-3, -2.75,  -2.5, -2.25,  -2, -1.75,  -1.5,
                                                   -1.25, -1, -0.75,  -0.5, -0.25, 0,0.25, 0.5,  0.75, 1,  1.25),
                                         labels=c(-2.75, -2.75, -2.75, -2.75,
                                                  -2, -2, -2, -1.5, -1.5 ,
                                                  -1.25, -1, -0.75,  -0.5, -0.25, 0,0.25, 0.5,  0.75, 1,  1.25) )

model.H1b <- glmer(decision~ co2 + tax + energieabhaengigkeit + (zeitpunkt * concern.scaled) +(1 + co2 + tax + energieabhaengigkeit + zeitpunkt | id),
                   data = data.decision,family = binomial(link = "logit"))

summary(model.H1b)
tab_model(model.H1b)


#with slightly transformed scaled climate change concern as factor
class(data.decision$concern.scaled.f)
model.H1b.f <- glmer(decision~ co2 + tax + energieabhaengigkeit + (zeitpunkt * concern.scaled.f)   +
                       (1 + co2 + tax + energieabhaengigkeit + zeitpunkt | id),
                     data = data.decision,family = binomial(link = "logit"))

summary(model.H1b.f)
tab_model(model.H1b.f)

# non scaled factor would be with concern.f
e <- effects::effect("zeitpunkt*concern.scaled.f", model.H1b.f, 
                     xlevels=list(concern.scaled.f=seq(from=-3.75, to=1.25, length.out=14)) )
e <- as.data.frame(e)

ggplot(e, aes(concern.scaled.f, fit, color=zeitpunkt, group = zeitpunkt)) + 
  theme_minimal() + geom_line() + geom_point() + labs(x="climate concern (scaled)", y="probability") + ylim(0,1) +
  #  geom_errorbar(aes(ymin=lower, ymax=upper)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.1)

### per person ??

#Predict at person level
# not clean at all, but nothing else so far has worked
# these are predictions -> but it computes it for every choice so 16 per person

predframe <- predict(model.H1b,type="response" )
predframe <- predict(model.H1b,type="response",re.form=~(1 + co2 + tax + energieabhaengigkeit + zeitpunkt | id) )

data.decision$prediction <- predframe
# then i took the average prediction per person and timepoint so its 8 per time and person
predictions.model <- groupwiseMean(prediction ~ ResponseId + zeitpunkt,
                                   data= data.decision,
                                   traditional= FALSE,
                                   percentile =FALSE )
predictions.model <- predictions.model %>% select(!n)
predictions.model <- merge(predictions.model, data.cc.2, by="ResponseId")

# and used that and their cc concern score to plot the interaction for the prediciton of accepting a policy
ggplot(predictions.model,aes(x=concern.scaled, y=Mean, color=zeitpunkt))+
  geom_jitter(alpha=0.5) + 
  ylim(0,1) + theme_minimal()

# mit sjplot
plot_scatter(predictions.model, x=climate_concern, y=Mean, grp = zeitpunkt, fit.grps = "glm" , show.ci = TRUE, jitter=0.01)


plot_model(model.H1.random_s)
plot_model(model.H1.random_s,  
           type="pred", terms=c("zeitpunkt", "tax") )
plot_model(model.H1b, type="pred",
           terms=c("concern.scaled", "zeitpunkt", "tax") ) # predicted values
plot_model(model.H1b, type="emm", 
           terms=c("concern.scaled", "zeitpunkt", "tax") ) # predicted values



### model H1c
data.H1c <- read.csv("data/data.model.co2.c.csv")
data.H1c <- data.H1c %>%
  rename( co2.beta= "co2.30..CO2",
          tax.beta= "tax.6.",
          energy.beta = "energieabhaengigkeit.20."  ,
          time.beta =  "zeitpunktin.7.Jahren"     )
data.H1c$co2.beta.n <- as.numeric(data.H1c$co2.beta)

describe(data.H1c$co2.beta.n)
# this means in OR's from 0.53 OR to 4.44 OR 
model.cc.d <- lm(co2.beta.n ~ concern.scaled + gender + age + income + country, data=data.H1c )
summary(model.cc.d)
tab_model(model.cc.d)

ggplot(data.H1c, aes(x=co2.beta.n)) +
  geom_boxplot() +
  theme_minimal()
#range
# -0.6075599  1.5545940
quantile(data.H1c$co2.beta.n)
# 0.6740529 Q§ und 0.3166654 Q1 = interquantile range 0.3573875
# exlude everything 1.5* QR 
#-0.2194159; 1.210134
data.H1c.excl <- data.H1c %>% filter(co2.beta.n > -0.2194159) %>% filter(co2.beta.n < 1.210134)
ggplot(data.H1c.excl, aes(x=co2.beta.n)) +
  geom_boxplot() +
  theme_minimal()

# if limit that 

model.cc.d.excl <- lm(co2.beta.n ~ concern.scaled + gender + age + income + country, data=data.H1c.excl )
summary(model.cc.d.excl)
tab_model(model.cc.d.excl)

ggplot(data.H1c, aes(x=concern.scaled, y=co2.beta.n)) +
  geom_jitter() + geom_smooth() + theme_minimal()


plot_model(model.cc.d, type="emm", 
           terms=c("concern.scaled"))

plot_scatter(  data.H1c, concern.scaled, co2.beta.n,
               colors="black", fit.grps = "glm")

## proportion of yes choices for later modeling
proportion <- groupwiseSum(decision ~ ResponseId,
                           data =data.decision,
                           traditional = FALSE,
                           percentile = FALSE)

proportion.t <- groupwiseSum(decision ~ ResponseId + zeitpunkt,
                             data =data.decision,
                             traditional = FALSE,
                             percentile = FALSE)


##### accuracy ######
data.accuracy.long.total <- read.csv("data/data.accuracy.wave1.csv")
### histogram of estimates for each behavior
ggplot(data.accuracy.long.total, aes(x=estimate, fill=behavior, group=behavior )) + 
  facet_grid(behavior ~.) +
  geom_histogram(bins=60)  +
  theme_minimal() + scale_fill_viridis_d()

# to see distribution of high impact versus low impact behaviors, small values should be mostly driven by green behaviors high values by red behaviors
ggplot(data.accuracy.long.total, aes(x=estimate, fill=behavior, group=behavior )) + 
  geom_histogram(position="stack", bins=60)  +
  scale_fill_manual(values=c("darkgreen", "darkred","darkgreen", "darkgreen","darkred","darkorange","darkred","darkgreen"   )) +
  theme_minimal() 


### distributions
ggplot(data.accuracy.long.total, aes(x=estimation.bias) ) +
  geom_histogram()
ggplot(data.accuracy.long.total, aes(x=estimation.bias.log) ) +
  geom_histogram()

ggplot(data.accuracy.long.total, aes(x=absolute.error) ) +
  geom_histogram()
ggplot(data.accuracy.long.total, aes(x=absolute.error.log) ) +
  geom_histogram()

# one score per person, so averaged across 8 estimates
ggplot(data.accuracy.test, aes(x=mean.est.bias) ) +
  geom_histogram(bins=100)
ggplot(data.accuracy.test, aes(x=mean.abs.error) ) +
  geom_histogram(bins=100)

### to visualize estimation and actual values
data.estimate.mean <- groupwiseMean(estimate ~ behavior, 
                                    data =data.accuracy.long.total)

data.actual.mean <- groupwiseMean(actual ~ behavior, 
                                  data =data.accuracy.long.total,
                                  percentile= FALSE,
                                  traditional = FALSE)
accuracy.means <- merge(data.estimate.mean, data.actual.mean, by=c("behavior", "n"))
accuracy.means <- accuracy.means %>% 
  rename(mean.actual = Mean.y,
         mean.estimate= Mean.x)

### Plot for H3

# estimation accuracy of energy consumption with error bars
ggplot(accuracy.means, aes(x=mean.actual, y=mean.estimate, color=behavior)) +
  geom_point(size=3.8) +  ylim(0,1800) + xlim(0,1800) +
  geom_errorbar(aes(ymin=Trad.lower, ymax=Trad.upper), linewidth=1.3) +
  theme_minimal() + labs(x="actual value", y="mean estimate", color=NULL) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  scale_color_viridis_d() +
  theme(legend.text = element_text(size=11))

# without efficiency fridge and light
accuracy.means.curtail <- accuracy.means %>% filter(!behavior=="fridge_eff") %>% filter(!behavior=="light")
model.accuracy.1 <- lmer(actual ~ estimate + (1|ResponseId), data=data.accuracy.long.c)
summary(model.accuracy.1)

ggplot(accuracy.means.curtail, aes(x=mean.actual, y=mean.estimate, color=behavior)) +
  geom_point(size=3.8) +  
  ylim(0,1800) + xlim(0,1800) +
  geom_errorbar(aes(ymin=Trad.lower, ymax=Trad.upper), linewidth=1.3) +
  theme_minimal() + labs(x="actual value", y="mean estimate", color=NULL) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  scale_color_viridis_d() +
  geom_abline(intercept=542.5, slope=0.1046) +
  theme(legend.text = element_text(size=11))

# divided per country 
data.estimate.mean.c <- groupwiseMean(estimate ~ behavior + country, 
                                      data =data.accuracy.long.total)

data.actual.mean.c <- groupwiseMean(actual ~ behavior + country, 
                                    data =data.accuracy.long.total,
                                    percentile= FALSE,
                                    traditional = FALSE)

accuracy.means.c <- merge(data.estimate.mean.c, data.actual.mean.c, by=c("behavior", "country","n"))
accuracy.means.c <- accuracy.means.c %>% 
  rename(mean.actual = Mean.y,
         mean.estimate= Mean.x)

ggplot(accuracy.means.c, aes(x=mean.actual, y=mean.estimate, color=behavior)) +
  geom_point(size=2.8) +  ylim(0,1800) + xlim(0,1800) +
  facet_grid(country ~.) +
  geom_errorbar(aes(ymin=Trad.lower, ymax=Trad.upper)) +
  theme_minimal() + labs(x="actual value", y="mean estimate", color=NULL) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  scale_color_viridis_d() +
  theme(legend.text = element_text(size=11))
## there do seem to be differences 


### for later modeling ONLY CURTAILMENT !!!!!
#absolute error
data.accuracy.long.c <- data.accuracy.long.total %>% 
  filter(!behavior=="fridge_eff") %>% 
  filter(!behavior=="light")

data.abs.error.mean <- groupwiseMean(absolute.error.log ~ ResponseId,
                                     data= data.accuracy.long.c,
                                     percentile= FALSE)
data.abs.error.mean <- data.abs.error.mean %>%
  rename(mean.abs.error = Mean) %>% select(!n)

# same for estimation bias
data.est.bias.mean <- groupwiseMean(estimation.bias.log ~ResponseId,
                                    data= data.accuracy.long.c,
                                    percentile= FALSE,
                                    traditional = FALSE)

data.est.bias.mean <- data.est.bias.mean %>%
  rename(mean.est.bias = Mean) %>% select(!n)

# combine both for later modeling 
data.accuracy.test <- merge(data.est.bias.mean, data.abs.error.mean, by=c("ResponseId"))
data.accuracy.test$mean.est.bias.r <- round(data.accuracy.test$mean.est.bias, 2)
table(data.accuracy.test$mean.est.bias.r)
ggplot(data.accuracy.test, aes(x=mean.est.bias.r)) +
  geom_histogram() +
  theme_minimal()
data.accuracy.test$binary <- factor(ifelse(data.accuracy.test$mean.est.bias < 0, "under", "over"))

# Wynes linear regression analyses with demographic data, ..
model.accuracy <-lmer(estimate.log ~ actual.log + (1|ResponseId)  ,data=data.accuracy.long.total)
summary(model.accuracy)
model.accuracy.d <-lmer(estimate.log ~ actual.log + gender + age + income + country + (1|ResponseId)  ,data=data.accuracy.long.total)
summary(model.accuracy.d)
# country and gender also significant 


## Marghetis et al
model.acc <- lmer(estimate.transformed ~ actual.transformed + (1|ResponseId)  ,data=data.accuracy.long.total)
summary(model.acc)
## i dont understand what I am doing wrong but the range is not how it is supposed to be see next steps:
# 0.1207 = slope? # cor=0.09294927; und sd's sind estimate=0.7 und actual = 0.54 das kommt fast hin

# b actual = cor between estimate and true value * ratio between sd of estimate / sd of actual
# cor = correctness of individuals' underlying,understanding of appliances' relative energy use, cor of 1 = perfect understanding

# sd ratio to measure their use of the response scale; systematic overestimation of small values and underestimation of large values would 
#produce a ratio less than 1 with ratios closer to 0 indicating a more compressed use of the response scale. ??

## apparently gotta do this per person
data.accuracy.long.c$cor <- c(rep(NA, nrow(data.accuracy.long.c) ))
accList <- split(data.accuracy.long.c,data.accuracy.long.c$ResponseId)
length(accList)
names(accList) <- paste0("id",1:length(accList))

# tranfosrmed wie in marghetis wäre 9=estimate und 10=actual für non transformed values sonst 15 und 16=estimate
for (i in c(1:(nrow(data.accuracy.long.c)/6) )) { 
  accList[[c(i,22)]] <- cor(accList[[c(i,17)]], accList[[c(i,18)]])
}

data.cor <- data.frame(matrix(NA,    # Create empty data frame
                              nrow = 1522,
                              ncol = 2))

for (i in c(1:(nrow(data.accuracy.long.c)/6) )) { 
  data.cor$cor[[i]] <- cor(accList[[c(i,17)]], accList[[c(i,18)]])
}
names(data.cor)
acc.id <- unique(data.accuracy.long.c$ResponseId)
data.cor$X1 <- acc.id
data.cor <- data.cor %>% select(!X2) %>%
  rename(ResponseId = X1)
data.cor$cor <- as.numeric(data.cor$cor)
describe(data.cor$cor)
data.cor$cor.r <- round(data.cor$cor,1)
table(data.cor$cor.r)
data.cor.excl <- data.cor %>% filter(cor>0)

# same for sd
data.accuracy.long.c$sd <- c(rep(NA, nrow(data.accuracy.long.c) ))
accList.sd <- split(data.accuracy.long.c,data.accuracy.long.c$ResponseId)
data.acc.w <- unique(data.accuracy.long.c$actual.log)
sd(data.acc.w) # -> actual sd

for (i in c(1:(nrow(data.accuracy.long.c)/6) )) { 
  accList.sd[[c(i,23)]] <- (sd( accList.sd[[c(i,18)]]) /  sd(accList.sd[[c(i,17)]]) ) 
}

data.sd <- data.frame(matrix(NA,    # Create empty data frame
                             nrow = 1522,
                             ncol = 2))

for (i in c(1:(nrow(data.accuracy.long.c)/6) )) { 
  data.sd$sd[[i]] <-  (sd(accList.sd[[c(i,18)]]) / sd(accList.sd[[c(i,17)]]) ) 
}
names(data.sd)
data.sd$X1 <- acc.id
data.sd <- data.sd %>% select(!X2) %>%
  rename(ResponseId = X1)
data.sd$sd <- as.numeric(data.sd$sd)
describe(data.sd$sd)
data.sd.excl <- data.sd %>% filter(sd<1)

data.acc.models <- merge(data.cor, data.sd, by="ResponseId")
data.acc.models <- data.acc.models %>% 
  mutate(cor= as.numeric(cor),
         sd = as.numeric(sd))
cor(data.acc.models$sd, data.acc.models$cor)
data.acc.models.dec <- merge(data.acc.models, data.accuracy.policy, by="ResponseId")

ggplot(data.acc.models, aes(x=cor, y=sd))+
  geom_point() + theme_minimal()

data.acc.models.dec.trust <- merge(data.acc.models.dec, data.correlates.important,  by="ResponseId")
data.acc.models.dec.trust$co2 <- factor(data.acc.models.dec.trust$co2)
data.acc.models.dec.trust$energieabhaengigkeit <- factor(data.acc.models.dec.trust$energieabhaengigkeit)
data.acc.models.dec.trust$tax <- factor(data.acc.models.dec.trust$tax)
data.acc.models.dec.trust$zeitpunkt <- factor(data.acc.models.dec.trust$zeitpunkt)

model.policy.cor.sd <- glmer(decision ~ tax + co2 + energieabhaengigkeit +
                               cor  + sd + gender + age + income  + concern.scaled + trust.gov.scaled+ 
                               emotions.crisis.neg.tot + emotions.re.pos.scaled+ Sum.product*zeitpunkt + country + (1 + tax + co2 + energieabhaengigkeit + zeitpunkt|ResponseId),
                            family="binomial",
                            data=data.acc.models.dec.trust)
summary(model.policy.cor.sd)
tab_model(model.policy.cor.sd)


# cor = correctness of individuals' underlying,understanding of appliances' relative energy use, cor of 1 = perfect understanding

# systematic overestimation of small values and underestimation of large values would produce a ratio less than 1 
# with ratios closer to 0 indicating a more compressed use of the response scale. ??


model.cor <- lm(cor ~ gender + age + country + income, data=data.acc.models.excl.2)
summary(model.cor)
data.acc.models.excl.2$bactual <- data.acc.models.excl.2$cor*data.acc.models.excl.2$sd
describe(data.acc.models.excl.2$cor)
describe(data.acc.models.excl.2$bactual)

data.accuracy.model <- data.everthing %>% select(ResponseId,education , income ,gender ,age , country,
                                                 concern.scaled, V2, mean.abs.error, mean.est.bias)
data.accuracy.model <- unique(data.accuracy.model)
data.accuracy.model$education.f <- factor(data.accuracy.model$education,
                                        levels=c("no formal education", "obligatory school", "middle school", "degree"))

## accurcy by demographics and such 
model.accuracy.error <- lm(mean.abs.error ~ education.f + income + gender + age + country + 
                         concern.scaled + V2,
                       data=data.accuracy.model)
summary(model.accuracy.d)

model.accuracy.bias <- lm(mean.est.bias ~  income + gender + age + country + 
                             concern.scaled + V2,
                           data=data.accuracy.model)
summary(model.accuracy.bias)

plot_scatter(data.accuracy.model, concern.scaled, mean.est.bias)
plot_model(model.accuracy.bias, type="pred", terms=c("concern.scaled", "country"))
plot_model(model.accuracy.bias, type="pred", terms=c("V2"))

#https://www.sciencedirect.com/science/article/pii/S0272494415300049#appsec1
###  within person correlation between estimates and actual -log transformed
# package ‘rmcorr’
install.packages("rmcorr"); library("rmcorr")
rmcorr(ResponseId, estimate.log, actual.log, data=data.accuracy.long.c)
#r0.2758126.  ; degrees of freedom=7609 ;  p-value =6.153437e-133;   95% confidence interval = 0.2549256 0.2964424 

cor.test(data.accuracy.long.c$estimate.log, data.accuracy.long.c$actual.log)
#0.1568831

concern <- data.decision.id %>% select(ResponseId, concern.scaled)
data.accuracy.concern <- merge(data.accuracy.long.total, concern, by="ResponseId")

model.estimates <- lmer(estimate.log ~ actual.log + concern.scaled  + country + income + age + gender + (1|ResponseId), 
   data= data.accuracy.concern)
summary(model.estimates)
tab_model(model.estimates)
plot_model(model.estimates, type="emm", terms="actual.log", axis.lim=c(1.5, 3.1)) 
# actual.log range is 1.716003 3.074085

  
  
  
  
##### product choices  ####
data.product.2 <- read.csv("data/data.product.short.csv")
data.product.2 <- data.product.2[,-c(1)]

data.product.long <- read.csv("data/data.product.long.csv")
data.product.long <- data.product.long[,-c(1)]

## visual overview
data.product.sum <- data.product.2 %>%
  select(Sum) %>% group_by(Sum) %>% tally()
data.product.sum$prop <- data.product.sum$Sum/7
data.product.sum$percent <- data.product.sum$n/1628

## how many choice where pro-environmental
ggplot(data.product.sum, aes(x=prop, y=percent))  +
  geom_bar(stat="identity") +
  labs(x="proportion of pro-environmental choices", y="percentage of people") +
  theme_minimal()

# climate change concern and pro-env. prodiuct choice
ggplot(data.product.2, aes(x=concern.scaled, y=percent))  +
  geom_jitter() + geom_smooth() +
  labs(x="scaled climate change concern", y="proportion of pro-environmental choices") +
  theme_minimal()

ggplot(data.product.2, aes(x=concern.scaled, y=percent, color=country)) +
  geom_jitter() + geom_smooth() +
  theme_minimal()


### model
model.product.choice <- glmer(choice ~ price.level * energy.level + energy.level*concern.scaled +  price.level*concern.scaled + gender + income + country + age  + (1|ResponseId) ,
                              data=data.product.long, family = binomial(link = "logit") )

summary(model.product.choice)
tab_model(model.product.choice)

## product choice model visualisieren

plot_scatter(data.product.long, x=price.level, y=choice, grp= energy.level,
             dot.size=.5, jitter= 0.1)

plot_scatter(model.product.choice, x=price.level, y=choice, grp= energy.level,
             dot.size=.5, jitter= 0.1)

ggplot(data.product.long, aes(x=concern.scaled, y=choice, color=energy.level)) +
  geom_jitter(aes(shape=price.level), alpha=0.4 , height=.1) + scale_color_manual(values=c("darkgreen", "darkblue")) +
  theme_minimal() 

data.product.long.grouped <- data.product.long %>% select(concern.scaled, choice, energy.level, price.level) %>%
  group_by(concern.scaled, choice, energy.level, price.level) %>% tally()

ggplot(data.product.long.grouped, aes(x=concern.scaled, y=n, color=energy.level)) +
  facet_wrap(choice ~.) +
  geom_jitter(aes(shape=price.level), alpha=0.7 , height=.1) + scale_color_manual(values=c("darkgreen", "darkblue")) +
  theme_minimal() 

ggplot(data.product.long.grouped, aes(x=concern.scaled, y=choice, color=energy.level)) +
  geom_jitter(aes(shape=price.level, size=n), alpha=0.7 , height=.1) + scale_color_manual(values=c("darkgreen", "darkblue")) +
  theme_minimal() 

plot_model(model.product.choice, type = "eff", terms=c( "concern.scaled", "energy.level"))
plot_model(model.product.choice, type = "pred", terms=c( "concern.scaled", "energy.level"))

plot_model(model.product.choice, type = "eff", terms=c( "concern.scaled", "price.level"))
plot_model(model.product.choice, type = "pred", terms=c( "concern.scaled", "price.level"))



#### models between indiv and policy decisions and accuracy judgment ####

### predicting product choice with accuracy and policy support
data.product.long.2 <- data.product.long[,1:15]
data.product.long.2 <- data.product.long.2 %>% 
  rename(Sum.product = Sum)
data.accuracy.product <- merge(data.accuracy.test, data.product.long.2, by=c("ResponseId"))
proportion.d <- proportion %>% select(!n) %>%
  rename(Sum.policy = Sum)
data.accuracy.product <- merge(data.accuracy.product, proportion.d, by=c("ResponseId"))

model.product.acc <- glmer(choice ~ mean.abs.error  + gender + age + income + education + country + Sum.policy + concern.scaled + (1|ResponseId),
                          family="binomial",
                          data=data.accuracy.product)

summary(model.product.acc)
tab_model(model.product.acc)
plot_model(model.product.acc, type="emm", terms=c("mean.abs.error"))


# for sum not individual decisions
data.accuracy.product.short <- data.accuracy.product[, c(1:19,23)]
data.accuracy.product.short <- unique(data.accuracy.product.short)

# model predciting sum of proenevironmental product decisions
model.product.acc.Sum <- lm(Sum.product ~ mean.abs.error  + gender + age + income + education + country + Sum.policy + concern.scaled,
                            data=data.accuracy.product.short)

summary(model.product.acc.Sum)
tab_model(model.product.acc.Sum)

plot_model(model.product.acc.Sum, type="emm", terms="mean.abs.error")

# interaction with timepoint
data.accuracy.product2 <- merge(data.accuracy.product, proportion.t, by="ResponseId")
model.product.acc2 <- glmer(choice ~ mean.abs.error  + gender + age + income + education + country + Sum*zeitpunkt + concern.scaled + (1|ResponseId),
                           family="binomial",
                           data=data.accuracy.product2)

summary(model.product.acc2)
tab_model(model.product.acc2)

proportion.t1 <- proportion.t %>% filter(!zeitpunkt=="in 7 Jahren" )
proportion.t2 <- proportion.t %>% filter(zeitpunkt=="in 7 Jahren" )

data.accuracy.product.t1 <- merge(data.accuracy.product.short, proportion.t1, by=c("ResponseId"))

model.accuracy.product.t1.Sum <- lm(Sum.product ~ mean.abs.error  + gender + age + income + education + country + Sum + concern.scaled,
                                    data=data.accuracy.product.t1)
summary(model.accuracy.product.t1.Sum)
tab_model(model.accuracy.product.t1.Sum)


data.accuracy.product.t2 <- merge(data.accuracy.product.short, proportion.t2, by=c("ResponseId"))
model.accuracy.product.t2.Sum <- lm(Sum.product ~ mean.abs.error  + gender + age + income + education + country + Sum + concern.scaled,
                                    data=data.accuracy.product.t2)
summary(model.accuracy.product.t2.Sum)
tab_model(model.accuracy.product.t2.Sum)


## check same for policy decisions
data.accuracy.policy <- merge(data.accuracy.test, data.decision, by=c("ResponseId"))
product.Sum <- data.product.2 %>% select(ResponseId, Sum)
data.accuracy.policy <- merge(data.accuracy.policy, product.Sum, by=c("ResponseId"))
data.accuracy.policy <- data.accuracy.policy %>% rename(Sum.product = Sum)

set_label(data.accuracy.policy$Sum.product) <- "Sum of proenvironmental product choices"
set_label(data.accuracy.policy$mean.abs.error) <-  "mean absolute error"
set_label(data.accuracy.policy$zeitpunkt) <-  "implementation time"

model.policy.acc.t <- glmer(decision ~ tax + co2 + energieabhaengigkeit +
                              mean.abs.error   + gender + age + income  + concern.scaled + Sum.product*zeitpunkt + country + 
                              (1 + tax + co2 + energieabhaengigkeit + zeitpunkt|ResponseId),
                            family="binomial",
                           data=data.accuracy.policy)
summary(model.policy.acc.t)
tab_model(model.policy.acc.t)

plot_model(model.policy.acc.t, type="emm", terms=c("Sum.product","zeitpunkt"))
plot_model(model.policy.acc.t, type="emm", terms=c("mean.abs.error","zeitpunkt"))


### per timepoint getrennt
data.accuracy.policy.t1 <- data.accuracy.policy %>% filter(!zeitpunkt=="in 7 Jahren" )
data.accuracy.policy.t1 <- merge(data.accuracy.policy.t1, proportion.d, by=c("ResponseId"))
model.policy.acc.t1 <- glmer(decision ~ tax + co2 + energieabhaengigkeit + 
                               mean.abs.error   + gender + age + income + country  + concern.scaled + Sum.product + (1 + tax + co2 + energieabhaengigkeit|ResponseId),
                             family="binomial",
                            data=data.accuracy.policy.t1)
summary(model.policy.acc.t1)
tab_model(model.policy.acc.t1)

data.accuracy.policy.t2 <- data.accuracy.policy %>% filter(zeitpunkt=="in 7 Jahren" )
data.accuracy.policy.t2 <- merge(data.accuracy.policy.t2, proportion.d, by=c("ResponseId"))
model.policy.acc.t2 <- lmer(decision ~   tax + co2 + energieabhaengigkeit +
                              mean.abs.error   + gender + age + income + country  + concern.scaled + Sum.product  + (1 + tax + co2 + energieabhaengigkeit |ResponseId),
                            family="binomial",
                            data=data.accuracy.policy.t2)
summary(model.policy.acc.t2)
tab_model(model.policy.acc.t2)


# predicitng sum of accepted policies
data.accuracy.policy.sum <- merge(data.accuracy.policy, proportion.t, by=c("ResponseId", "zeitpunkt"))
data.accuracy.policy.sum <- data.accuracy.policy.sum %>% rename(Sum.time = Sum)
data.accuracy.policy.sum <- data.accuracy.policy.sum %>%
  select(ResponseId, gender, age, income , country, concern.scaled,  mean.abs.error, Sum.product,Sum.time,zeitpunkt)

data.accuracy.policy.sum <- unique(data.accuracy.policy.sum)

set_label(data.accuracy.policy.sum$mean.abs.error) <- "mean absolute error"
set_label(data.accuracy.policy.sum$zeitpunkt) <- "implementation time"
set_label(data.accuracy.policy.sum$Sum.time) <- "Sum of accepted renwable energy policies"
model.policy.acc.sum <- lmer(Sum.time ~    gender + age + income + country  + mean.abs.error+ concern.scaled  + Sum.product *zeitpunkt + (1|ResponseId),
                             data=data.accuracy.policy.sum )

tab_model(model.policy.acc.sum)
plot_model(model.policy.acc.sum, type="emm", terms=c("mean.abs.error", "zeitpunkt"), axis.lim = c(0,8))

##### complexity task ######
# prepare so matrix is symmetrical
data.complexity.long <- read_csv("data/data.complexity.long.csv")
data.complexity.long <- data.complexity.long[,-c(1)]

dfList <- split(data.complexity.long,data.complexity.long$ResponseId)
length(dfList)
names(dfList) <- paste0("id",1:length(dfList))

for(r in 1:length(dfList)) { 
  assign(paste0("matrix",r), NULL )
}


for (r in 1:length(dfList)) { 
  assign(paste0("matrix", r), dfList[[r]] %>% 
           mutate_at(1:2, factor, levels = unique(c(levels(.$v1), levels(.$v2)))) %>%
           xtabs(similarity.2 ~ v1 + v2, data=., sparse = FALSE) ) 
}

#### matrices ####
matrices <- (list(matrix1, matrix2, matrix3,matrix4,matrix5,matrix6,matrix7,matrix8,matrix9,matrix10,matrix11,matrix12,matrix13,matrix14 ,matrix15,
                  matrix16,matrix17,matrix18,matrix19,matrix20,matrix21,matrix22,matrix23,matrix24,matrix25,matrix26,matrix27,matrix28,matrix29,matrix30,
                  matrix31,matrix32,matrix33,matrix34,matrix35,matrix36,matrix37,matrix38,matrix39,matrix40,matrix41,matrix42,matrix43,matrix44,matrix45,
                  matrix46,matrix47,matrix48,matrix49,matrix50,matrix51,matrix52,matrix53,matrix54,matrix55,matrix56,matrix57,matrix58,matrix59,matrix60,
                  matrix61,matrix62,matrix63,matrix64,matrix65,matrix66,matrix67,matrix68,matrix69,matrix70,matrix71,matrix72,matrix73,matrix74,matrix75,
                  matrix76, matrix77,matrix78,matrix79,matrix80,matrix81,matrix82,matrix83,matrix84,matrix85,matrix86,matrix87,matrix88,matrix89,matrix90,
                  matrix91,matrix92,matrix93,matrix94,matrix95,matrix96,matrix97,matrix98,matrix99, matrix100,matrix101,matrix102,matrix103,matrix104,matrix105,
                  matrix106,matrix107,matrix108,matrix109,matrix110,matrix111,matrix112,matrix113,matrix114,matrix115,matrix116,matrix117,matrix118,matrix119,matrix120,
                  matrix121,matrix122,matrix123,matrix124,matrix125,matrix126,matrix127,matrix128,matrix129,matrix130,matrix131,matrix132,matrix133,matrix134,matrix135,
                  matrix136,matrix137,matrix138,matrix139,matrix140,matrix141,matrix142,matrix143,matrix144,matrix145,matrix146,matrix147,matrix148,matrix149,matrix150,
                  matrix151,matrix152,matrix153,matrix154,matrix155,matrix156,matrix157,matrix158,matrix159,matrix160,matrix161,matrix162,matrix163,matrix164,matrix165,
                  matrix166,matrix167,matrix168,matrix169,matrix170,matrix171,matrix172,matrix173,matrix174,matrix175,matrix176,matrix177,matrix178,matrix179,matrix180,
                  matrix181,matrix182,matrix183,matrix184,matrix185,matrix186,matrix187,matrix188,matrix189,matrix190, matrix191, matrix192, matrix193, matrix194, matrix195,
                  matrix196, matrix197, matrix198, matrix199, matrix200, matrix201, matrix202, matrix203, matrix204, matrix205, matrix206, matrix207, matrix208, matrix209, matrix210,
                  matrix211, matrix212 , matrix213, matrix214, matrix215, matrix216, matrix217, matrix218, matrix219, matrix220, matrix221, matrix222, matrix223, matrix224, matrix225,
                  matrix226, matrix227, matrix228, matrix229, matrix230, matrix231, matrix232, matrix233, matrix234, matrix235, matrix236, matrix237, matrix238, matrix239, matrix240,
                  matrix241, matrix242, matrix243, matrix244, matrix245, matrix246, matrix247, matrix248, matrix249, matrix250, matrix251, matrix252, matrix253, matrix254, matrix255,
                  matrix256, matrix257, matrix258,matrix259, matrix260, matrix261, matrix262, matrix263, matrix264, matrix265, matrix266, matrix267, matrix268, matrix269, matrix270, 
                  matrix271, matrix272, matrix273, matrix274,matrix275, matrix276, matrix277, matrix278, matrix279, matrix280, matrix281, matrix282, matrix283, matrix284, matrix285, 
                  matrix286, matrix287, matrix288, matrix289, matrix290,matrix291, matrix292, matrix293, matrix294, matrix295, matrix296, matrix297, matrix298, matrix299, matrix300, 
                  matrix301, matrix302, matrix303, matrix304, matrix305,matrix306, matrix307, matrix308, matrix309, matrix310, matrix311, matrix312, matrix313, matrix314, matrix315, 
                  matrix316, matrix317, matrix318, matrix319, matrix320, matrix321, matrix322, matrix323,matrix324, matrix325, matrix326, matrix327, matrix328, matrix329, matrix330, 
                  matrix331, matrix332, matrix333, matrix334, matrix335, matrix336, matrix337, matrix338, matrix339,matrix340, matrix341, matrix342, matrix343, matrix344, matrix345, 
                  matrix346, matrix347, matrix348, matrix349, matrix350, matrix351, matrix352, matrix353, matrix354, matrix355,matrix356, matrix357, matrix358, matrix359, matrix360, 
                  matrix361, matrix362, matrix363, matrix364, matrix365, matrix366, matrix367, matrix368, matrix369, matrix370, matrix371, matrix372,matrix373, matrix374, matrix375, 
                  matrix376, matrix377, matrix378, matrix379, matrix380, matrix381, matrix382, matrix383, matrix384, matrix385, matrix386, matrix387, matrix388, matrix389,matrix390,
                  matrix391, matrix392, matrix393, matrix394, matrix395, matrix396, matrix397, matrix398, matrix399, matrix400, matrix401, matrix402, matrix403, matrix404, matrix405,
                  matrix406, matrix407, matrix408, matrix409, matrix410, matrix411, matrix412, matrix413, matrix414, matrix415, matrix416, matrix417, matrix418, matrix419, matrix420,
                  matrix421, matrix422, matrix423, matrix424, matrix425, matrix426 , matrix427 , matrix428, matrix429, matrix430, matrix431, matrix432, matrix433, matrix434, matrix435,
                  matrix436, matrix437, matrix438, matrix439, matrix440, matrix441, matrix442, matrix443, matrix444, matrix445, matrix446, matrix447, matrix448, matrix449, matrix450,
                  matrix451, matrix452, matrix453, matrix454, matrix455, matrix456, matrix457, matrix458, matrix459, matrix460, matrix461, matrix462, matrix463 , matrix464, matrix465, 
                  matrix466, matrix467, matrix468, matrix469,matrix470, matrix471, matrix472, matrix473, matrix474, matrix475, matrix476, matrix477, matrix478, matrix479, matrix480, 
                  matrix481, matrix482, matrix483, matrix484,matrix485, matrix486, matrix487, matrix488, matrix489, matrix490, matrix491, matrix492, matrix493, matrix494, matrix495, 
                  matrix496, matrix497, matrix498, matrix499, matrix500, matrix501, matrix502, matrix503, matrix504, matrix505, matrix506, matrix507, matrix508, matrix509, matrix510, 
                  matrix511, matrix512, matrix513, matrix514,matrix515, matrix516, matrix517, matrix518, matrix519, matrix520, matrix521, matrix522, matrix523, matrix524, matrix525, 
                  matrix526, matrix527, matrix528, matrix529,matrix530, matrix531, matrix532, matrix533, matrix534, matrix535, matrix536, matrix537, matrix538, matrix539, matrix540,
                  matrix541, matrix542, matrix543, matrix544, matrix545,matrix546, matrix547, matrix548,matrix549, matrix550, matrix551, matrix552, matrix553, matrix554,matrix555,
                  matrix556, matrix557, matrix558, matrix559, matrix560, matrix561, matrix562, matrix563, matrix564, matrix565, matrix566, matrix567, matrix568, matrix569, matrix570, 
                  matrix571, matrix572, matrix573, matrix574,matrix575, matrix576, matrix577, matrix578, matrix579, matrix580, matrix581, matrix582, matrix583, matrix584, matrix585,
                  matrix586, matrix587, matrix588, matrix589, matrix590, matrix591, matrix592, matrix593, matrix594,matrix595, matrix596, matrix597, matrix598, matrix599, matrix600, 
                  matrix601, matrix602, matrix603, matrix604, matrix605, matrix606, matrix607, matrix608, matrix609, matrix610, matrix611, matrix612, matrix613, matrix614,matrix615, 
                  matrix616, matrix617, matrix618, matrix619, matrix620, matrix621, matrix622, matrix623, matrix624, matrix625, matrix626, matrix627, matrix628, matrix629, matrix630,
                  matrix631, matrix632, matrix633, matrix634, matrix635, matrix636, matrix637, matrix638, matrix639, matrix640, matrix641, matrix642, matrix643, matrix644, matrix645, 
                  matrix646, matrix647, matrix648, matrix649, matrix650, matrix651, matrix652, matrix653, matrix654, matrix655, matrix656, matrix657, matrix658, matrix659, matrix660,
                  matrix661, matrix662, matrix663, matrix664, matrix665, matrix666,matrix667, matrix668, matrix669, matrix670, matrix671, matrix672, matrix673, matrix674, matrix675, 
                  matrix676, matrix677, matrix678, matrix679, matrix680, matrix681, matrix682, matrix683, matrix684, matrix685, matrix686, matrix687, matrix688, matrix689, matrix690,
                  matrix691, matrix692, matrix693, matrix694, matrix695, matrix696, matrix697, matrix698, matrix699, matrix700, matrix701, matrix702, matrix703, matrix704, matrix705,
                  matrix706, matrix707, matrix708, matrix709, matrix710, matrix711, matrix712, matrix713, matrix714, matrix715, matrix716, matrix717, matrix718, matrix719, matrix720, 
                  matrix721, matrix722, matrix723, matrix724, matrix725, matrix726, matrix727, matrix728, matrix729, matrix730, matrix731, matrix732, matrix733, matrix734, matrix735,
                  matrix736, matrix737, matrix738, matrix739, matrix740, matrix741, matrix742, matrix743, matrix744, matrix745, matrix746, matrix747, matrix748, matrix749, matrix750,
                  matrix751, matrix752, matrix753, matrix754, matrix755, matrix756, matrix757, matrix758, matrix759, matrix760, matrix761, matrix762, matrix763, matrix764, matrix765,
                  matrix766, matrix767, matrix768, matrix769, matrix770, matrix771, matrix772, matrix773, matrix774, matrix775, matrix776, matrix777, matrix778, matrix779, matrix780,
                  matrix781, matrix782, matrix783, matrix784, matrix785, matrix786, matrix787, matrix788, matrix789, matrix790, matrix791, matrix792, matrix793, matrix794, matrix795,
                  matrix796, matrix797, matrix798, matrix799, matrix800, matrix801, matrix802, matrix803, matrix804, matrix805, matrix806, matrix807, matrix808, matrix809, matrix810,
                  matrix811, matrix812, matrix813, matrix814, matrix815, matrix816, matrix817, matrix818, matrix819, matrix820, matrix821, matrix822, matrix823, matrix824, matrix825,
                  matrix826, matrix827, matrix828, matrix829, matrix830, matrix831, matrix832, matrix833, matrix834, matrix835, matrix836, matrix837, matrix838, matrix839, matrix840,
                  matrix841, matrix842, matrix843, matrix844, matrix845, matrix846, matrix847, matrix848, matrix849, matrix850, matrix851, matrix852, matrix853, matrix854, matrix855,
                  matrix856, matrix857, matrix858, matrix859, matrix860, matrix861, matrix862, matrix863, matrix864, matrix865, matrix866, matrix867, matrix868, matrix869, matrix870,
                  matrix871, matrix872, matrix873, matrix874, matrix875, matrix876, matrix877, matrix878, matrix879, matrix880, matrix881, matrix882, matrix883, matrix884, matrix885,
                  matrix886, matrix887, matrix888, matrix889, matrix890, matrix891, matrix892, matrix893, matrix894, matrix895, matrix896, matrix897, matrix898, matrix899, matrix900,
                  matrix901, matrix902, matrix903, matrix904, matrix905, matrix906, matrix907, matrix908, matrix909, matrix910, matrix911, matrix912, matrix913, matrix914, matrix915,
                  matrix916, matrix917, matrix918, matrix919, matrix920, matrix921, matrix922, matrix923, matrix924, matrix925, matrix926, matrix927, matrix928, matrix929, matrix930,
                  matrix931, matrix932, matrix933, matrix934, matrix935, matrix936, matrix937, matrix938, matrix939, matrix940, matrix941, matrix942, matrix943, matrix944, matrix945,
                  matrix946, matrix947, matrix948, matrix949, matrix950, matrix951, matrix952, matrix953, matrix954, matrix955, matrix956, matrix957, matrix958, matrix959, matrix960,
                  matrix961, matrix962, matrix963, matrix964, matrix965, matrix966, matrix967, matrix968, matrix969, matrix970, matrix971, matrix972, matrix973, matrix974, matrix975,
                  matrix976, matrix977, matrix978, matrix979, matrix980, matrix981, matrix982, matrix983, matrix984, matrix985, matrix986, matrix987, matrix988, matrix989, matrix990, 
                  matrix991, matrix992, matrix993, matrix994, matrix995, matrix996, matrix997, matrix998, matrix999, matrix1000, matrix1001, matrix1002, matrix1003, matrix1004, matrix1005,
                  matrix1006, matrix1007, matrix1008, matrix1009, matrix1010, matrix1011, matrix1012, matrix1013, matrix1014, matrix1015, matrix1016, matrix1017, matrix1018, matrix1019, matrix1020,
                  matrix1021, matrix1022, matrix1023, matrix1024, matrix1025, matrix1026, matrix1027, matrix1028, matrix1029, matrix1030, matrix1031, matrix1032, matrix1033, matrix1034, matrix1035, 
                  matrix1036, matrix1037, matrix1038, matrix1039, matrix1040, matrix1041, matrix1042, matrix1043, matrix1044, matrix1045, matrix1046, matrix1047, matrix1048, matrix1049, matrix1050,
                  matrix1051, matrix1052, matrix1053, matrix1054, matrix1055, matrix1056, matrix1057, matrix1058, matrix1059, matrix1060, matrix1061, matrix1062, matrix1063, matrix1064, matrix1065,
                  matrix1066, matrix1067, matrix1068, matrix1069, matrix1070, matrix1071, matrix1072, matrix1073, matrix1074, matrix1075, matrix1076, matrix1077, matrix1078, matrix1079, matrix1080, 
                  matrix1081, matrix1082, matrix1083, matrix1084, matrix1085, matrix1086, matrix1087, matrix1088, matrix1089, matrix1090, matrix1091, matrix1092, matrix1093, matrix1094, matrix1095, 
                  matrix1096, matrix1097, matrix1098, matrix1099, matrix1100, matrix1101, matrix1102, matrix1103, matrix1104, matrix1105, matrix1106, matrix1107, matrix1108, matrix1109, matrix1110, 
                  matrix1111, matrix1112, matrix1113, matrix1114, matrix1115, matrix1116, matrix1117, matrix1118, matrix1119, matrix1120, matrix1121, matrix1122, matrix1123, matrix1124, matrix1125,
                  matrix1126, matrix1127, matrix1128, matrix1129, matrix1130, matrix1131, matrix1132, matrix1133, matrix1134, matrix1135, matrix1136, matrix1137, matrix1138, matrix1139, matrix1140, 
                  matrix1141, matrix1142, matrix1143, matrix1144, matrix1145, matrix1146, matrix1147, matrix1148, matrix1149, matrix1150, matrix1151, matrix1152, matrix1153, matrix1154, matrix1155,
                  matrix1156, matrix1157, matrix1158, matrix1159, matrix1160, matrix1161, matrix1162, matrix1163, matrix1164, matrix1165, matrix1166, matrix1167, matrix1168, matrix1169, matrix1170,
                  matrix1171, matrix1172, matrix1173, matrix1174, matrix1175, matrix1176, matrix1177, matrix1178, matrix1179, matrix1180, matrix1181, matrix1182, matrix1183, matrix1184, matrix1185,
                  matrix1186, matrix1187, matrix1188, matrix1189, matrix1190, matrix1191, matrix1192, matrix1193, matrix1194, matrix1195, matrix1196, matrix1197, matrix1198, matrix1199, matrix1200, 
                  matrix1201	, matrix1202	, matrix1203	, matrix1204	, matrix1205	, matrix1206	, matrix1207	, matrix1208	, matrix1209	, matrix1210	, matrix1211	, matrix1212	, matrix1213	, matrix1214	, matrix1215	, 
                  matrix1216	, matrix1217	, matrix1218	, matrix1219	, matrix1220	, matrix1221	, matrix1222	, matrix1223	, matrix1224	, matrix1225	, matrix1226	, matrix1227	, matrix1228	, matrix1229	, matrix1230	,
                  matrix1231	, matrix1232	, matrix1233	, matrix1234	, matrix1235	, matrix1236	, matrix1237	, matrix1238	, matrix1239	, matrix1240	, matrix1241	, matrix1242	, matrix1243	, matrix1244	, matrix1245	,
                  matrix1246	, matrix1247	, matrix1248	, matrix1249	, matrix1250	, matrix1251	, matrix1252	, matrix1253	, matrix1254	, matrix1255	, matrix1256	, matrix1257	, matrix1258	, matrix1259	, matrix1260	,
                  matrix1261	, matrix1262	, matrix1263	, matrix1264	, matrix1265	, matrix1266	, matrix1267	, matrix1268	, matrix1269	, matrix1270	, matrix1271	, matrix1272	, matrix1273	, matrix1274	, matrix1275	, 
                  matrix1276	, matrix1277	, matrix1278	, matrix1279	, matrix1280	, matrix1281	, matrix1282	, matrix1283	, matrix1284	, matrix1285	, matrix1286	, matrix1287	, matrix1288	, matrix1289	, matrix1290	, 
                  matrix1291	, matrix1292	, matrix1293	, matrix1294	, matrix1295	, matrix1296	, matrix1297	, matrix1298	, matrix1299	, matrix1300	, matrix1301	, matrix1302	, matrix1303	, matrix1304	, matrix1305	,
                  matrix1306	, matrix1307	, matrix1308	, matrix1309	, matrix1310	, matrix1311	, matrix1312	, matrix1313	, matrix1314	, matrix1315	, matrix1316	, matrix1317	, matrix1318	, matrix1319	, matrix1320	, 
                  matrix1321	, matrix1322	, matrix1323	, matrix1324	, matrix1325	, matrix1326	, matrix1327	, matrix1328	, matrix1329	, matrix1330	, matrix1331	, matrix1332	, matrix1333	, matrix1334	, matrix1335	, 
                  matrix1336	, matrix1337	, matrix1338	, matrix1339	, matrix1340	, matrix1341	, matrix1342	, matrix1343	, matrix1344	, matrix1345	, matrix1346	, matrix1347	, matrix1348	, matrix1349	, matrix1350	, 
                  matrix1351	, matrix1352	, matrix1353	, matrix1354	, matrix1355	, matrix1356	, matrix1357	, matrix1358	, matrix1359	, matrix1360	, matrix1361	, matrix1362	, matrix1363	, matrix1364	, matrix1365	, 
                  matrix1366	, matrix1367	, matrix1368	, matrix1369	, matrix1370	, matrix1371	, matrix1372	, matrix1373	, matrix1374	, matrix1375	, matrix1376	, matrix1377	, matrix1378	, matrix1379	, matrix1380	, 
                  matrix1381	, matrix1382	, matrix1383	, matrix1384	, matrix1385	, matrix1386	, matrix1387	, matrix1388	, matrix1389	, matrix1390	, matrix1391	, matrix1392	, matrix1393	, matrix1394	, matrix1395	, 
                  matrix1396	, matrix1397	, matrix1398	, matrix1399	, matrix1400	, matrix1401	, matrix1402	, matrix1403	, matrix1404	, matrix1405	, matrix1406	, matrix1407	, matrix1408	, matrix1409	, matrix1410	, 
                  matrix1411	, matrix1412	, matrix1413	, matrix1414	, matrix1415	, matrix1416	, matrix1417	, matrix1418	, matrix1419	, matrix1420	, matrix1421	, matrix1422	, matrix1423	, matrix1424	, matrix1425	, 
                  matrix1426	, matrix1427	, matrix1428	, matrix1429	, matrix1430	, matrix1431	, matrix1432	, matrix1433	, matrix1434	, matrix1435	, matrix1436	, matrix1437	, matrix1438	, matrix1439	, matrix1440	, 
                  matrix1441  , matrix1442	, matrix1443	, matrix1444	, matrix1445	, matrix1446	, matrix1447	, matrix1448	, matrix1449	, matrix1450	, matrix1451	, matrix1452	, matrix1453	, matrix1454	, matrix1455	, 
                  matrix1456	, matrix1457	, matrix1458	, matrix1459	, matrix1460	, matrix1461	, matrix1462	, matrix1463	, matrix1464	, matrix1465	, matrix1466	, matrix1467	, matrix1468	, matrix1469	, matrix1470	, 
                  matrix1471	, matrix1472	, matrix1473	, matrix1474	, matrix1475	, matrix1476	, matrix1477	, matrix1478	, matrix1479	, matrix1480	, matrix1481	, matrix1482	, matrix1483	, matrix1484	, matrix1485	, 
                  matrix1486	, matrix1487	, matrix1488	, matrix1489	, matrix1490	, matrix1491	, matrix1492	, matrix1493	, matrix1494	, matrix1495	, matrix1496	, matrix1497	, matrix1498	, matrix1499	, matrix1500	, 
                  matrix1501	, matrix1502	, matrix1503	, matrix1504	, matrix1505  , matrix1506	, matrix1507	, matrix1508	, matrix1509	, matrix1510	, matrix1511	, matrix1512	, matrix1513	, matrix1514	, matrix1515	,
                  matrix1516	, matrix1517	, matrix1518	, matrix1519	, matrix1520	, matrix1521	, matrix1522	, matrix1523	, matrix1524	, matrix1525	, matrix1526	, matrix1527	, matrix1528	, matrix1529	, matrix1530	, 
                  matrix1531	, matrix1532	, matrix1533	, matrix1534	, matrix1535	, matrix1536	, matrix1537	, matrix1538	, matrix1539	, matrix1540	, matrix1541	, matrix1542	, matrix1543	, matrix1544	, matrix1545	, 
                  matrix1546	, matrix1547	, matrix1548	, matrix1549	, matrix1550	, matrix1551	, matrix1552	, matrix1553	, matrix1554	, matrix1555  , matrix1556	, matrix1557	, matrix1558	, matrix1559	, matrix1560	,
                  matrix1561	, matrix1562	, matrix1563	, matrix1564	, matrix1565	, matrix1566	, matrix1567	, matrix1568	, matrix1569  , matrix1570	, matrix1571	, matrix1572	, matrix1573	, matrix1574	, matrix1575	, 
                  matrix1576	, matrix1577	, matrix1578	, matrix1579	, matrix1580	, matrix1581	, matrix1582	, matrix1583	, matrix1584	, matrix1585	, matrix1586	, matrix1587	, matrix1588	, matrix1589	, matrix1590	, 
                  matrix1591	, matrix1592	, matrix1593	, matrix1594	, matrix1595	, matrix1596	, matrix1597	, matrix1598	, matrix1599	, matrix1600	, matrix1601	, matrix1602	, matrix1603	, matrix1604	, matrix1605	, 
                  matrix1606	, matrix1607	, matrix1608	, matrix1609	, matrix1610  , matrix1611  , matrix1612  , matrix1613	, matrix1614	, matrix1615	, matrix1616	, matrix1617	, matrix1618	, matrix1619	, matrix1620	, 
                  matrix1621	, matrix1622	, matrix1623	, matrix1624	, matrix1625  , matrix1626  , matrix1627  , matrix1628
                  
))



#### scaling ####

### individual difference scaling without starting position 
scaling <- smacof:::indscal(matrices, type="ordinal",  itmax=75000)
scaling$stress
#0.1901146

scaling.3 <- smacof:::indscal(matrices, ndim=3, type="ordinal",  itmax=75000)
scaling.3$stress
# 0.09751012
scaling.4 <- smacof:::indscal(matrices, ndim=4, type="ordinal",  itmax=75000)
scaling.4$stress
#0.06478763
scaling.5 <- smacof:::indscal(matrices, ndim=5, type="ordinal",  itmax=75000)
scaling.5$stress
#0.0003783328

# manually create scree plot beacuse cant figure out with smacof
# probably pointless anyway because impossible to do one dimension with smacof...
stress.dim  <- data.frame(matrix(
  vector(), 4, 2, dimnames=list(c(), c("stress", "dimensions"))) )
stress.dim$stress <-c ( 0.1901146,  0.09751012, 0.06478763,0.0003783328 )
stress.dim$dimensions <-c ( 2,3,4, 5)
stress.dim$dimensions.f <- factor(stress.dim$dimensions)

ggplot(stress.dim, aes(x=dimensions, y=stress)) +
  geom_line() + #ylim(0,0.25) +
  geom_point(size=4)+
  xlab("number of dimensions") +
  ylab("stress") +
  ggtitle("Scree Plot")


arr <- array( unlist(matrices) , c(6,6,1628) )
matrix.average <- apply( arr , 1:2 , mean )

matrix.initial <- matrix(data=NA, nrow=6, ncol=2)
matrix.initial[,1] <- c(1, -1, -1, -1, 1, 1) *2
matrix.initial[,2]  <- c(-.05, .05, .05, -.05, .05, -.05) *-10

mds.model.mass1 <- MASS:::isoMDS(matrix.average, k=1, maxit=1000, trace=FALSE
                                                            )
mds.model.mass1$stress
#13.19789
mds.model.mass2 <- MASS:::isoMDS(matrix.average, k=2, maxit=1000, trace=FALSE
                                )
mds.model.mass2$stress
#0.007401066
mds.model.mass3 <- MASS:::isoMDS(matrix.average, k=3, maxit=1000, trace=FALSE)
mds.model.mass3$stress
#0.007603066
mds.model.mass4 <- MASS:::isoMDS(matrix.average, k=4, maxit=1000, trace=FALSE)
mds.model.mass4$stress
# 3.350095e-14

stress.dim1  <- data.frame(matrix(
  vector(), 4, 2, dimnames=list(c(), c("stress", "dimensions"))) )
stress.dim1$stress <-c ( 13.19789,0.007401066,  0.007603066-05, 3.350095e-14 )
stress.dim1$dimensions <-c ( 1,2,3,4)
ggplot(stress.dim1, aes(x=dimensions, y=stress)) +
  geom_line() + #ylim(0,0.25) +
  geom_point(size=4)+
  xlab("number of dimensions") +
  ylab("stress") +
  ggtitle("Scree Plot")

mds.model.1 <- vegan:::metaMDS(matrix.average, k=1, autotransform = FALSE, noshare=FALSE)
#0.120347
mds.model.2 <- vegan:::metaMDS(matrix.average, k=2, autotransform = FALSE, noshare=FALSE)
mds.model.2$stress
#8.481983e-05
mds.model.3 <- vegan:::metaMDS(matrix.average, k=3, autotransform = FALSE, noshare=FALSE)
mds.model.3$stress
# 0
mds.model.4 <- vegan:::metaMDS(matrix.average, k=4, autotransform = FALSE, noshare=FALSE)
mds.model.4$stress
#0

stress.dim2  <- data.frame(matrix(
  vector(), 4, 2, dimnames=list(c(), c("stress", "dimensions"))) )
stress.dim2$stress <-c ( 0.120347,  8.481983e-05, 0, 0 )
stress.dim2$dimensions <-c ( 1,2,3,4)

ggplot(stress.dim2, aes(x=dimensions, y=stress)) +
  geom_line() + #ylim(0,0.25) +
  geom_point(size=4)+
  xlab("number of dimensions") +
  ylab("stress") +
  ggtitle("Scree Plot")

# with vegan pacakge
nmds <- mds.model.2$points
nmds <- as.data.frame(nmds)
nmds$direction <- factor(c("pos","neg","neg",  "neg", "pos", "pos") )
nmds$strength <- factor(c("high","low", "low", "high", "low", "high") )
nmds$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco","Haare föhn vs luft", 
                       "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))
ggplot(nmds, aes(x=MDS1, y=MDS2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + labs(x=NULL, y=NULL, title = "nonmetric mds vegan package") +
  theme_minimal() + theme(legend.position = "none") +
  geom_text(aes(label = label), size=6, vjust=3, size=4)

# isomds
isomds <- mds.model.mass2$points
isomds <- as.data.frame(isomds)
isomds$direction <- factor(c("pos","neg","neg",  "neg", "pos", "pos") )
isomds$strength <- factor(c("high","low", "low", "high", "low", "high") )
isomds$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco","Haare föhn vs luft", 
                       "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))
ggplot(isomds, aes(x=V1, y=V2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + labs(x=NULL, y=NULL, subtitle = "nonmetric mds isoMDS-MASS package") +
  theme_minimal() + theme(legend.position = "none") +
  geom_text(aes(label = label), size=7, vjust=3, size=4)

#scaling <- smacof:::idioscal(matrices, type="ordinal",  itmax=35000) virtually exactly the same result, same with "ties secondary" or changing modulus

plot.indscal <- as.data.frame(scaling$gspace)
plot.indscal$direction <- factor(c("pos","neg","neg",  "neg", "pos", "pos") )
plot.indscal$strength <- factor(c("high","low", "low", "high", "low", "high") )
plot.indscal$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco","Haare föhn vs luft", 
                               "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))

indscal <- ggplot(plot.indscal, aes(x=D1, y=D2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + xlim(-.8, .8) +
  ylim(-.7, .7) +
  theme_minimal() +
  geom_text(aes(label = label), vjust=3, size=4)

###  individual difference scaling with starting position 
#init for starting values 
startconf.p <- matrix(data=NA, nrow=6, ncol=2)
startconf.p[,1] <- c(.1, -.1, -.1, -.1, .1, .1) *2
startconf.p[,2]  <- c(-.05, .05, .05, -.05, .05, -.05) *-4 #c(-.1, .1, .1, -.1, .1, -.1)

scaling.start.p <- smacof:::indscal(matrices, init=startconf.p, type="ordinal", itmax=75000)
summary(scaling.start.p)
scaling.start.p$stress
#0.1937694

plot.indscal.starting <- as.data.frame(scaling.start.p$gspace)
plot.indscal.starting$direction <- factor(c("pos","neg","neg",  "neg", "pos", "pos") )
plot.indscal.starting$strength <- factor(c("high","low", "low", "high", "low", "high") )
plot.indscal.starting$label <- factor(c("shower 6 vs 10 min", "dishwasher automatic vs eco","use hairdryer vs let dry", 
                                        "heat to 22° vs 20°", "wash at 30° vs 60°", "hanging clothes to dry vs tumble dry"))

ggplot(plot.indscal.starting, aes(x=D1, y=D2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + xlim(-.8, .8) +
  ylim(-.7, .7) + scale_color_manual(values=c("darkred", "darkgreen")) +
  theme_minimal() + theme(legend.position = "none") +
  labs(x="impact direction dimension (pos/neg)", y="impact strength dimension (high/low)") +
  geom_text(aes(label = label), vjust=3, size=4)

##### inidividual weights ####
### weights for dimensions
weights.scal.start <- as.matrix(scaling.start.p$cweights)

individual.weights <- matrix(data=1, nrow=1628, ncol=2)

for(r in 1:length(weights.scal.start )) { 
  individual.weights[r,2] <-  weights.scal.start[[c(r,4)]] ;
  individual.weights[r,1] <- weights.scal.start[[c(r,1)]] 
}

## V1 = D1, V2 = D2
individual.weights.2 <- as.data.frame(individual.weights)

### change!! either data.full, or data.complexity  or data.complexity.ch !!
individual.weights.2$ResponseId <- unique(data.complexity$ResponseId)

weights.total <- merge(individual.weights.2, data.decision.id, by="ResponseId")
weights.total$Sum <-  weights.total$V1 + weights.total$V2
weights.total$Sum.sq <- weights.total$V1^2  + weights.total$V2^2 

weights.total$Mean <- rowMeans(cbind(weights.total$V1, weights.total$V2))

ggplot(weights.total, aes(x=V1, y=V2, color=climate_concern)) +
  geom_point() +  xlim(0,1.7) + ylim(0,1.7) +
  geom_abline(slope=1) + labs(x="dimension 1", y="dimension 2") +
  theme_minimal() 


weights.total$difference <- weights.total$V1-weights.total$V2
weights.total$difference <- factor(ifelse(weights.total$difference > 0, 1, 0))
table(weights.total$difference)

weights.total$V1.rounded <- round(weights.total$V1,1)
table(weights.total$V1.rounded)
weights.total$V2.rounded <- round(weights.total$V2,1)
table(weights.total$V2.rounded)
weights.total$diff.rounded <- round(weights.total$V1-weights.total$V2,1)

describe(weights.total$V1)
describe(weights.total$V2)
table(weights.total$diff.rounded)
ggplot(weights.total, aes(x=V1)) +
  geom_boxplot()
ggplot(weights.total, aes(x=V2)) +
  geom_boxplot()

#### goodness of fit etc ####
##  "Goodness-of-Fit Assessment in Multidimensional Scaling and Unfolding"

#install.packages("MPsychoR", repos="http://R-Forge.R-project.org")
require("smacof")     
require("MPsychoR")
require("colorspace")
require("plotrix")

n <- c(1600, 1650)
ndim <- c(1, 2, 3)
nrep <- 500

## --- ordinal MDS
stressmat3 <- matrix(NA, ncol = length(ndim), nrow = length(n), dimnames = list(n, ndim))
for (i in 1:length(ndim)) {
  for (j in 1:length(n)) {
    cat(ndim[i], "/", n[j], "\n")
    stressmat3[j, i] <- mean(randomstress(n = n[j], ndim = ndim[i], nrep = nrep, type = "ordinal")) 
  }
}
stressmat3

# output
#1         2         3
#1600 0.5765138 0.4200105 0.3316804
#1650 0.5765387 0.4200516 0.3317310

# für 2 dimensions und 1600 wäre random stress =0.4200105 für 1650 Personen 0.4200516


stressnorms <- stressmat3
fitlaw1 <- smacof::mds(matrices, type = "ordinal")  
## These stress norms can be subject to plotting (cf. Fig. 1)
plot(fitlaw1, main = "Configuration Plot Ordinal MDS")
plot(fitlaw1, plot.type = "Shepard", main = "Shepard Diagram Ordinal MDS")

## scree plot
stressvec <- NULL
for (i in 1:4) stressvec[i] <- smacof::mds(matrices, ndim = i, type = "ordinal")$stress
plot(1:4, stressvec, pch = 20, type = "b", xlab = "Number of Dimensions", ylab = "Stress-1", main = "MDS Scree Plot", xaxp = c(1, 16, 15))

#shepard
plot(scaling.start.p, plot.type = "Shepard")

#Stress per point:
#  a     b     c     d     e     f 
# 15.45 17.77 18.27 16.45 15.54 16.52 
#From an interpretation point of view, one can give less emphasis to high SPP objects compared to the ones with low SPP
# so less emphasis to c= "use hairdryer vs let dry"
#"If the data setting permits, points with high SPP can be eliminated and the MDS model refitted
# exclude extremes ...
model.strength.dim <- lm(V2 ~ gender + income + age + concern.scaled  + education, data=weights.total)
summary(model.strength.dim)

model.direction.dim<- lm(V1 ~ gender + income + age + concern.scaled  + education, data=weights.total)
summary(model.direction.dim)


#### models with accuracy
weights.total.acc <- merge(weights.total, data.accuracy.test, by="ResponseId")

model.direction.dim.acc <- lm(V1 ~ mean.est.bias + concern.scaled + gender + age + income + education + country , data=weights.total.acc)
summary(model.direction.dim.acc)

# with everything else
weights.total <- weights.total %>% select(ResponseId, V1, V2)

#### hypotheses with complexity ####

###H4a: 
#Individual weights of the impact strength dimension will predict the number of climate-friendly choices 
#higher scores on the impact strength dimension variable are associated with a higher likelihood to choose the more energy efficient product options. 

# mit allen und ohne zeitpunkt getrennt weil policy  hier nicht drin ist
data.everthing <- merge(data.accuracy.product,  weights.total, by=c("ResponseId")    )
levels(data.everthing$binary)
data.everthing <- data.everthing %>% 
  mutate(est.binary = factor(binary, levels=c("under", "over")) )

model.H4a1 <- glmer(choice ~ est.binary + concern.scaled  + gender + age + income + education + country +  (1|ResponseId),
                          data=data.everthing, family="binomial")

summary(model.H4a1)
tab_model(model.H4a1)
# take out age, gender and education because not significant and model does not yet converge
model.H4a <- glmer(choice ~ est.binary + concern.scaled   + country +  (1|ResponseId),
                   data=data.everthing, family="binomial")

summary(model.H4a)
tab_model(model.H4a)

#  mean.absolute error had no effect either, neither did the more continuous variable est.bias in pre-reg used over versus under-estimation though so that included in model

plot_model(model.H4a, axis.lim =c(0.05, 20) )
plot_model(model.H4a, type="pred", terms=c("concern.scaled", "country") ) # predicted values
plot_model(model.H4a, type="emm", terms=c("concern.scaled", "country")  ) # predicted values

# just to check use continuous estimation bias instead of binary version
model.H4a.c <- glmer(choice ~ mean.est.bias + concern.scaled  + income  + country +  (1|ResponseId), data=data.everything, family="binomial")
tab_model(model.H4a.c)

data.everthing.unique <- data.everthing %>% select(ResponseId, concern.scaled, Sum.product, country, mean.est.bias, mean.abs.error, binary, V1, V2)
data.everthing.unique <- unique(data.everthing.unique)

ggplot(data.everthing.unique, aes(x=concern.scaled, y=Sum.product, color=country)) +
  geom_jitter(width=.01) + theme_minimal() + scale_color_manual(values=c("black", "darkred")) +
  geom_smooth()
ggplot(data.everthing.unique, aes(x=concern.scaled, y=Sum.product)) +
  geom_jitter(width=.01) + theme_minimal() +
  geom_smooth()





#### H4b
# with weight for impact strength direction
model.H4b1 <- glmer(choice ~ est.binary + concern.scaled + V2 +gender + age + income + education + country +  (1|ResponseId),
                   data=data.everthing, family="binomial")

summary(model.H4b1)
tab_model(model.H4b1)

model.H4b <- glmer(choice ~ est.binary + concern.scaled + V2 + country +  (1|ResponseId),
                   data=data.everthing, family="binomial")

summary(model.H4b)
tab_model(model.H4b)

model.H4b.c <- glmer(choice ~ mean.est.bias + concern.scaled + V2 + country +  (1|ResponseId),
                   data=data.everthing, family="binomial")

summary(model.H4b.c)


### other pre-registered hypotheses ####
names(data.pass)

#### H3a
# frequencies: 1 = never or max once per week, 2= 2-3 times per week, 3 = 3 or more times per week
#frequency_1= waschmaschine, frequency_2= trockner, frequency_3=spülmaschine,frequency_4=FÖhn
device.freq <- data.pass %>% select(ResponseId, starts_with("frequency"))
device.freq.full <- merge(device.freq, data.decision.id, by="ResponseId")

psych::alpha(device.freq.full[c("frequency_device_1","frequency_device_2","frequency_device_3","frequency_device_4")])
psych::alpha(device.freq.full[c("frequency_device_1","frequency_device_2","frequency_device_3")])
device.freq.full <- device.freq.full %>%
  mutate(mean.freq = rowMeans(cbind(frequency_device_1,frequency_device_2,frequency_device_3,frequency_device_4)),
         mean.freq.clean =rowMeans(cbind(frequency_device_1,frequency_device_2,frequency_device_3)) )

device.freq.long <- pivot_longer(device.freq.full, 2:5, names_to = "device", values_to = "frequency")

ggplot(device.freq.long, aes(x=frequency, fill=device)) +
  geom_histogram() + theme_minimal()

mean.frequencies <- groupwiseMean(frequency ~ ResponseId,
                                  data= device.freq.long,
                                  traditional =  FALSE,
                                  percentile = FALSE)
mean.frequencies <- na.omit(mean.frequencies)
mean.frequencies <- mean.frequencies %>% select(!n) %>%
  mutate(mean.device.freq = Mean)

device.freq.2 <- merge(device.freq, mean.frequencies, by="ResponseId")
ggplot(device.freq.2, aes(x=mean.device.freq)) +
  geom_histogram() + theme_minimal()

names(data.accuracy.test)
data.accuracy.frequency <- merge(data.accuracy.test, device.freq.full, by="ResponseId")

# device accuracy nicht sufficiently gut verteilt also als factor und dann reduced
data.accuracy.frequency$device.freq.f <- as.factor(data.accuracy.frequency$mean.freq)
# weighted mean von 2.25-3 frequencies =2.845506
data.accuracy.frequency$device.freq.f <- factor(data.accuracy.frequency$device.freq.f, 
                                              levels=c("1" ,"1.25",  "1.5" ,"1.75",  "2", "2.25",  "2.5", "2.75", "3" ),
                                              labels=c("1" ,"1.25",  "1.5" ,"1.75",  "2", "2.85","2.85","2.85","2.85" ) )
table(data.accuracy.frequency$device.freq.f)
# binary
data.accuracy.frequency$freq.binary <- ifelse(data.accuracy.frequency$mean.freq >1.5, "high", "low")
data.accuracy.frequency$freq.binary <- factor(data.accuracy.frequency$freq.binary, levels=c("low", "high"))

model.accuracy.frequency <- lm(mean.abs.error ~ gender + age + country + income + concern.scaled + device.freq.f ,
                         data= data.accuracy.frequency )
summary(model.accuracy.frequency)
tab_model(model.accuracy.frequency)

set_label(data.accuracy.frequency$mean.abs.error) <- "mean absolute error"
set_label(data.accuracy.frequency$mean.freq) <- "mean device frequency"

model.accuracy.freq2 <- lm(mean.abs.error ~ gender + age + country + income + concern.scaled + mean.freq ,
                               data= data.accuracy.frequency )
tab_model(model.accuracy.freq2)

set_theme(base = theme_classic() )
plot_model(model.accuracy.freq2, type="emm", 
           terms=c("mean.freq"), 
           axis.lim=c(0.35,0.95))

set_label(data.accuracy.frequency$mean.abs.error) <- "mean absolute error"
set_label(data.accuracy.frequency$concern.scaled) <- "climate change concern"
set_label(data.accuracy.frequency$freq.binary) <- "device frequency"

model.accuracy.freq.binary <- lm(mean.abs.error ~ gender + age + country + income + concern.scaled + freq.binary ,
                                 data= data.accuracy.frequency )
tab_model(model.accuracy.freq.binary)

set_theme(base = theme_classic() )
plot_model(model.accuracy.freq.binary, type="emm", 
           terms=c("concern.scaled","freq.binary"), 
           axis.lim=c(0.35,0.95))

# laut Karlijn van den Broek paper - B. Gatersleben, L. Steg, C. Vlek -2002 estimation bias und frequency connected - 
#müsste man dann aber wohl pro itme machen?
device.freq.long.acc <- merge(data.accuracy.test, device.freq.long, by="ResponseId")

model.accuracy.freq.item <- lmer(mean.abs.error ~ gender + age + country + income + concern.scaled + mean.freq + (1|device) + (1|ResponseId +device), 
                                data=device.freq.long.acc)

tab_model(model.accuracy.freq.item)


ggplot(data.accuracy.frequency, aes(x=mean.freq, y=mean.abs.error )) +
  geom_jitter() + geom_smooth() +
  theme_minimal()

acc.predict <- predict(model.accuracy.new)
data.accuracy.new3$pred <- acc.predict


ggplot(data.accuracy.new3, aes(x=device.freq.f, y=mean.abs.error, color=binary )) +
  geom_jitter() + geom_smooth() +
  theme_minimal()

#### other things of interest

# emotions

