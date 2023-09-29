
###new clean script wave 1 winter energy study


# prep
# load packages
library(psych)
library(tidyverse); library(ggplot2)
library(rcompanion)
library(broom)
library(vegan)
library(sjPlot) # for tab_model;
library(sjlabelled)
library(sjmisc)
library(lmerTest); library(lme4)
library(emmeans)
library("rmcorr")

setwd("/Users/nfrings/Library/Mobile Documents/com~apple~CloudDocs/Documents/PhD/Studies/Winter studie/results/wave1 results and R")

#### DCE policy decision part ####
data.decision <- read_csv("data/data.decision.csv")
data.decision <- data.decision[,-c(1)]

### if only want those that did both waves for better comparison: 
data.id.both <-  read_csv("data/data.id.both.csv")
data.id.both <- data.id.both[,-c(1)]
data.decision <- merge(data.decision, data.id.both, by="m" )

# change all variables to factors that should be a factor
data.decision$gender <- factor(data.decision$gender)
data.decision$age <- factor(data.decision$age)
data.decision$income <- factor(data.decision$income,
                               levels=c( "<1'500€\n<3'100CHF","<1'500- 2'499€\n3'100-4'299CHF", "2'500- 4'000€\n<4'300- 5'899CHF", "> 4'000€\n>5'900 CHF") )
data.decision$country <- factor(data.decision$country)
data.decision$education <- factor(data.decision$education, 
                                  levels=c("no formal education", "obligatory school", "middle school" , "degree"))
data.decision$decision <- factor(data.decision$decision)
data.decision$co2 <- factor(data.decision$co2)
data.decision$tax <- factor(data.decision$tax)
data.decision$energieabhaengigkeit <- factor(data.decision$energieabhaengigkeit)
data.decision$zeitpunkt <- factor(data.decision$zeitpunkt)
data.decision$politicalorientation.scaled <- scale(data.decision$politicalorientation_1, scale=FALSE)[,1]

#### Model of policy decisions  with all demographics and poliitcal orientation

# H1 :  controlled for age, gender, country of origin and income as per pre-registration
model.H1 <- glmer(decision~ co2 + tax + energieabhaengigkeit + zeitpunkt + gender + age + income + country  + (1|id),
                  data=data.decision, family="binomial")
summary(model.H1)
tab_model(model.H1)


# random slopes and demographics just for control
# income and gender and householdsize very much not significant for better overview excluded
data.decision$household_size.f <- as.factor(data.decision$household_size)
data.decision$politicalorientation.scaled <- scale(data.decision$politicalorientation_1,  scale=FALSE)[,1]
model.H1.random_s.d <- glmer(decision~ co2 + tax + energieabhaengigkeit + zeitpunkt  + 
                               income + gender +age + country + education +
                               (1 + co2 + tax + energieabhaengigkeit + zeitpunkt | m),
                             data=data.decision, family="binomial")
summary(model.H1.random_s.d)
tab_model(model.H1.random_s.d)

# age effect
em.H1.age <- emmeans(model.H1.random_s.d,specs="age")
pairs(em.H1.age, adjust="none")
em.H1.income <- emmeans(model.H1.random_s.d,specs="income")
pairs(em.H1.income, adjust="none")
# age1 to 4,5 age2 to 5, age3 to nothing -> differences between youngest and oldest age groups
plot_model(model.H1.random_s.d, type="pred", terms=c("politicalorientation.scaled[all]"))
cor.test(data.decision$concern.scaled, data.decision$politicalorientation.scaled)
# correlation between concern scaled und political orientation = -0.29 -> more right = less concern
plot_model(model.H1.random_s.d)


#### Final model used for H1 ###
# only random slopes and no demographics

# for plotting later set labels now
set_label(data.decision$tax) <- "tax (1% vs 6%)"
set_label(data.decision$energieabhaengigkeit) <- "energyindependence (10% vs 20%)"
set_label(data.decision$co2) <- "CO2 reduction (15% vs 30%)"
set_label(data.decision$zeitpunkt) <- "implementation (in 1 vs 7 years)"

# without demographics but with random slope
model.H1.random_si <- glmer(decision~ co2 + tax + energieabhaengigkeit + zeitpunkt  +
                              (1 + co2 + tax + energieabhaengigkeit + zeitpunkt | m),
                            data=data.decision, family="binomial")
summary(model.H1.random_si)
tab_model(model.H1.random_si)
plot_model(model.H1.random_si)

#### for H1 a ###
# basically just plotting model.H1.random_si
set_theme(base= theme_classic() )
plot_model(model.H1.random_si, axis.lim=c(0.025, 40))

### H1b ###
# adding climate change concern

set_label(data.decision$tax) <- "tax (1% vs 6%)"
set_label(data.decision$energieabhaengigkeit) <- "energyindependence (10% vs 20%)"
set_label(data.decision$co2) <- "CO2 reduction (15% vs 30%)"
set_label(data.decision$zeitpunkt) <- "implementation (in 1 vs 7 years)"

model.H1b <- glmer(decision~ co2 + tax + energieabhaengigkeit + 
                     (zeitpunkt * concern.scaled) +
                     (1 + co2 + tax + energieabhaengigkeit + zeitpunkt | m),
                   data = data.decision,family = binomial(link = "logit"))

summary(model.H1b)
tab_model(model.H1b)
plot_model(model.H1b, type="pred", 
           terms=c("concern.scaled", "zeitpunkt") , 
           colors=c( "darkgreen", "darkorange") )

# compare model fit to know if adding cc concern makes it much better
anova(model.H1.random_si, model.H1b)

# with demographics again
model.H1b.d <- glmer(decision~ co2 + tax + energieabhaengigkeit + 
                     (zeitpunkt * concern.scaled) +
                       income + gender +age + country + education +
                       (1 + co2 + tax + energieabhaengigkeit + zeitpunkt | m),
                   data = data.decision,family = binomial(link = "logit"))

summary(model.H1b.d)
tab_model(model.H1b.d)

set_theme(base= theme_classic() )
plot_model(model.H1b.d, type="pred", 
           terms=c("concern.scaled", "zeitpunkt") , 
           colors=c( "darkgreen", "darkorange") )

#### not very clean 
predframe <- predict(model.H1b,type="response",re.form=~(1 + co2 + tax + energieabhaengigkeit + zeitpunkt | id) )

data.decision$prediction <- predframe
# then i took the average prediction per person and timepoint so its 8 per time and person
predictions.model <- groupwiseMean(prediction ~ ResponseId + zeitpunkt,
                                   data= data.decision,
                                   traditional= FALSE,
                                   percentile =FALSE )
predictions.model <- predictions.model %>% select(!n)

data.concern <- data.decision %>% dplyr::select(ResponseId, concern.scaled, climate_concern)
data.concern <- unique(data.concern)
predictions.model <- merge(predictions.model, data.concern, by="ResponseId")
predictions.model <- unique(predictions.model)

# and used that and their cc concern score to plot the interaction for the prediciton of accepting a policy
ggplot(predictions.model,aes(x=concern.scaled, y=Mean, color=zeitpunkt))+
  geom_jitter(alpha=0.5) +
  ylim(0,1) + theme_minimal()

# mit sjplot
set_theme(base = theme_classic())
plot_scatter(predictions.model, x=concern.scaled, y=Mean, grp = zeitpunkt, fit.grps = "glm" , show.ci = TRUE, jitter=0.07, dot.size=0.5)


# for plotting with non scaled
set_label(data.decision$climate_concern) <- "climate change concern"

model.H1b.plot <- glmer(decision~ co2 + tax + energieabhaengigkeit + 
                     (zeitpunkt * climate_concern) +
                     (1 + co2 + tax + energieabhaengigkeit + zeitpunkt | id),
                   data = data.decision,family = binomial(link = "logit"))

summary(model.H1b.plot)
tab_model(model.H1b.plot)
plot_model(model.H1b.plot, type="emm", terms=c("climate_concern", "zeitpunkt", "tax"))

### model H1c ###
data.H1c <- read.csv("data/data.model.co2.c.csv")
data.H1c <- data.H1c %>%
  rename( co2.beta= "co2.30..CO2",
          tax.beta= "tax.6.",
          energy.beta = "energieabhaengigkeit.20."  ,
          time.beta =  "zeitpunktin.7.Jahren"     )
data.H1c$co2.beta.n <- as.numeric(data.H1c$co2.beta)
data.H1c$education <- factor(data.H1c$education,
                                    levels=c("no formal education", "obligatory school", "middle school",  "degree") )
data.H1c$income <- factor(data.H1c$income,
                                 levels=c( "<1'500€\n<3'100CHF","<1'500- 2'499€\n3'100-4'299CHF", "2'500- 4'000€\n<4'300- 5'899CHF", "> 4'000€\n>5'900 CHF") )
data.H1c$age <- factor(data.H1c$age)
data.H1c$gender <- factor(data.H1c$gender)

describe(data.H1c$co2.beta.n) # range =-0.61 1.55 
# this means in OR's from 0.54 to 4.71 OR 

set_label(data.H1c$concern.scaled) <-  "climate change concern (scaled)"
set_label(data.H1c$co2.beta.n) <-  "CO2 attribute importance"

# without interaction
model.cc.d <- lm(co2.beta.n ~  gender + age + income + country + education + 
                   climate_concern + politicalorientation_1 , data=data.H1c )
summary(model.cc.d)
tab_model(model.cc.d, digits=3)
plot_model(model.cc.d, type="emm", terms=c("climate_concern"), axis.lim=c(-0,1))

# with interaction cc concern and politcal orientation
model.cc.d.i <- lm(co2.beta.n ~  gender + age + income + country + education + 
                   politicalorientation_1*climate_concern + trust.gov, data=data.H1c )
summary(model.cc.d.i)
tab_model(model.cc.d.i, digits=3)

set_theme(base= theme_classic() )
plot_model(model.cc.d, type="emm", terms=c("politicalorientation_1","climate_concern"), axis.lim=c(-0,1))

ggplot(data.H1c, aes(x=co2.beta.n)) +
  geom_boxplot() +
  theme_minimal()

# outliers?

quantile(data.H1c$co2.beta.n)
# 0.6740529 Q3 und 0.3166654 Q1 = interquantile range 0.3573875
# exlude everything 1.5* QR 
#-0.2194159; 1.210134
data.H1c.excl <- data.H1c %>% filter(co2.beta.n > -0.2194159) %>% filter(co2.beta.n < 1.210134)
ggplot(data.H1c.excl, aes(x=co2.beta.n)) +
  geom_boxplot() +
  theme_minimal()

### rerun model with limit
model.cc.d.excl <- lm(co2.beta.n ~ concern.scaled + gender + age + income + country +
                        education + politicalorientation_1 + trust.gov, data=data.H1c.excl )
summary(model.cc.d.excl)
tab_model(model.cc.d.excl)

ggplot(data.H1c, aes(x=concern.scaled, y=co2.beta.n)) +
  geom_jitter() + geom_smooth() + theme_minimal()
plot_scatter(data.H1c.excl, concern.scaled, co2.beta.n, dot.size=.5, jitter= 0.1, fit.line = "lm")

###### complexity #####

#### models between product and policy, judgment
weights.total <- read.csv("data/weights.total.csv")
weights.total <- weights.total[,-c(1)]
weights.total <- weights.total %>% select(!Sum)

##### accuracy ######

data.accuracy.long.total <- read.csv("data/data.accuracy.long.total.csv")
data.accuracy.long.total <- data.accuracy.long.total[,-c(1)]

# data.accuracy.long.total <- merge(data.accuracy.long.total, id.both, by="ResponseId")

data.accuracy.long.total$gender <- factor(data.accuracy.long.total$gender)
data.accuracy.long.total$age <- factor(data.accuracy.long.total$age)
data.accuracy.long.total$income <- factor(data.accuracy.long.total$income,
                               levels=c( "<1'500€\n<3'100CHF","<1'500- 2'499€\n3'100-4'299CHF", "2'500- 4'000€\n<4'300- 5'899CHF", "> 4'000€\n>5'900 CHF") )
data.accuracy.long.total$country <- factor(data.accuracy.long.total$country)
data.accuracy.long.total$education <- factor(data.accuracy.long.total$education, 
                                  levels=c("no formal education", "obligatory school", "middle school" , "degree"))
# with more strict exclusion
data.accuracy.long.total <- read.csv("data/data.accuracy.long.total.excl.csv")
data.accuracy.long.total <- data.accuracy.long.total[,-c(1)]

### histogram of estimates for each behavior
ggplot(data.accuracy.long.total, aes(x=estimate, fill=behavior, group=behavior )) + 
  facet_grid(behavior ~.) +
  geom_histogram(bins=60)  +
  theme_minimal() + scale_fill_viridis_d()

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
  theme(axis.title = element_text(face = "bold"))  +
  theme(text=element_text(size = 18) ,axis.text = element_text(size = 17)  )  + 
  labs(x="actual value", y="mean estimate", color=NULL) +theme_minimal() +
  theme(axis.title = element_text(face = "bold"))  +
  theme(text=element_text(size = 18) ,axis.text = element_text(size = 17)  ) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  scale_color_viridis_d() +
  theme(legend.text = element_text(size=11))

# without efficiency fridge and light
accuracy.means.curtail <- accuracy.means %>% filter(!behavior=="fridge_eff") %>% filter(!behavior=="light")

data.accuracy.long.c <- data.accuracy.long.total %>% filter(!behavior=="fridge_eff") %>% filter(!behavior=="light")

accuracy.means.curtail$behavior <- factor(accuracy.means.curtail$behavior, 
                                          levels=c("dryer","heating", "shower", "dishwasher", "hairdryer", "washing"))
ggplot(accuracy.means.curtail, aes(x=mean.actual, y=mean.estimate, color=behavior)) +
  geom_point(size=4.4) +  
  ylim(0,1600) + xlim(0,1600) +
  geom_errorbar(aes(ymin=Trad.lower, ymax=Trad.upper), linewidth=1.3) +
  theme(axis.title = element_text(face = "bold"))  +
  theme(text=element_text(size = 18) ,axis.text = element_text(size = 17)  ) +
  labs(x="actual value", y="mean estimate", color=NULL) +theme_minimal() + 
  theme(axis.title = element_text(face = "bold"))  +
  theme(text=element_text(size = 18) ,axis.text = element_text(size = 17)  ) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  scale_color_viridis_d() +
  theme(legend.text = element_text(size=11))

# divided by country
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

model.accuracy.country <- lmer(actual ~ estimate +country + (1|ResponseId), data=data.accuracy.long.c)
summary(model.accuracy.country)

ggplot(accuracy.means.c, aes(x=mean.actual, y=mean.estimate, color=behavior, group=country)) +
  geom_point(size=3.8) +  
  ylim(0,1600) + xlim(0,1600) +
  geom_errorbar(aes(ymin=Trad.lower, ymax=Trad.upper), linewidth=1.3) +
  theme(axis.title = element_text(face = "bold"))  +
  theme(text=element_text(size = 18) ,axis.text = element_text(size = 17)  ) +
  labs(x="actual value", y="mean estimate", color=NULL) +theme_minimal() + 
  theme(axis.title = element_text(face = "bold"))  +
  theme(text=element_text(size = 18) ,axis.text = element_text(size = 17)  ) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  scale_color_viridis_d() 


### analysis for H3
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
data.accuracy.test <- merge(data.est.bias.mean, data.abs.error.mean, by=c("ResponseId"))
data.accuracy.test$mean.est.bias.r <- round(data.accuracy.test$mean.est.bias, 1)
ggplot(data.accuracy.test, aes(x=mean.est.bias.r)) +
  geom_histogram() +
  theme_minimal()
data.accuracy.test$binary <- factor(ifelse(data.accuracy.test$mean.est.bias < 0, "under", "over"))

## Wynes linear regression analyses of ESTIMATE with demographic data, ..

model.estimates.total <-lmer(estimate.log ~ actual.log + gender + age + income + country + education + 
                               climate_concern + emotions.crisis.neg.tot +mean.freq.clean + V2 + 
                              (1|ResponseId)  ,data=data.accuracy.long.total)
summary(model.estimates.total)
# estimates are signifcantly informed by actual estimates however the slop is only 0.1207
# country, gender  have an effect on estimates  with swiss people, females making higher estimates (this does not yet say anything about accuracy)
# those with higher climate change concern also higher estimates overall
tab_model(model.estimates.total, digits=3)
plot_model(model.estimates.total, type="emm", terms=c("climate_concern"))

####actual error now
# test mean error and over/under estimation

data.accuracy.model <- read.csv("data/data.accuracy.model.csv")
data.accuracy.model <- data.accuracy.model[,-c(1)]

data.accuracy.model$gender <- factor(data.accuracy.model$gender)
data.accuracy.model$age <- factor(data.accuracy.model$age)
data.accuracy.model$income <- factor(data.accuracy.model$income,
                                           levels=c( "<1'500€\n<3'100CHF","<1'500- 2'499€\n3'100-4'299CHF", "2'500- 4'000€\n<4'300- 5'899CHF", "> 4'000€\n>5'900 CHF") )
data.accuracy.model$country <- factor(data.accuracy.model$country)
data.accuracy.model$education <- factor(data.accuracy.model$education, 
                                             levels=c("no formal education", "obligatory school", "middle school" , "degree"))

# error 
set_label(data.accuracy.model$mean.freq.clean) <- "mean frequency device use"
set_label(data.accuracy.model$education.f) <- "education"
set_label(data.accuracy.model$mean.abs.error) <- "mean absolute error"

model.accuracy.error <- lm(mean.abs.error ~ gender + age + income + country + education + 
                             climate_concern  +mean.freq.clean + V1,
                           data=data.accuracy.model)

summary(model.accuracy.error)
tab_model(model.accuracy.error)
anova(model.accuracy.error)
plot_model(model.accuracy.error, type="emm", terms=c("climate_concern"))
plot_model(model.accuracy.error, type="emm", terms=c("V1"))
cor.test(data.accuracy.model$mean.abs.error, data.accuracy.model$V1)

plot_model(model.accuracy.error, 
           terms=c("genderfemale", "age50-59","age60-80", "education.fmiddle school","education.fdegree", "climate_concern", "mean.freq.clean"))
# interpretation of absolute error log is such that higher = higher error 0 = perfect estimate

#higher education levels have lower means absolute error values, higher income levels also tend to have lower error values, higher age also tends to lower error
# higher climate concern also lower error

# to test correlation between income and education
tbl <- as.matrix(table(data.accuracy.model$education, data.accuracy.model$income))
chi2 = chisq.test(tbl, correct=F)
c(chi2$statistic, chi2$p.value)
# cramers V = sqrt(chi-squared / (n * (k - 1)))
sqrt(chi2$statistic / (1522*(4-1)) )
cramersv(chi2) 
#0.1668524 
# this apparently constitutes a moderate relationship


# estimation bias 
model.accuracy.bias <- lm(mean.est.bias ~ gender + age + income + country + education + 
                            climate_concern  +mean.freq.clean + V1,
                          data=data.accuracy.model)
anova(model.accuracy.bias)
tab_model(model.accuracy.bias)
plot_model(model.accuracy.bias, type="emm", terms=c("V1","climate_concern"))
plot_model(model.accuracy.bias, type="emm", terms=c("climate_concern", "country"))

# interpretation of log mean estimation bias is such that higher and positive = higher over estimation // negative and lower values = low underestimation
# so both under versus over estimation and how strongly

# higher levels of cc concern are associated with less underestimation / more likely to be overestimating

plot_scatter(data.accuracy.model, concern.scaled, mean.est.bias, fit.line = "lm")

ggplot(data.accuracy.model, aes(x=mean.est.bias)) +
  geom_boxplot() +
  theme_minimal()
ggplot(data.accuracy.model, aes(x=climate_concern, y=V2)) +
  geom_jitter() +
  theme_minimal()


#same but per 3 high impact and 3 low impact behaviors

data.mean.estimation <- data.accuracy.long.c  %>% select(ResponseId, behavior, estimation.bias.log, actual)

data.mean.estimation$type <- factor(data.mean.estimation$behavior, 
                                    levels=c("dishwasher", "dryer","hairdryer","heating", "shower", "washing" ),
                                    labels =c("over", "under", "over", "under", "under", "over"))

data.mean.estimation.s <- groupwiseMean(estimation.bias.log ~ type + ResponseId,
                                        data=data.mean.estimation)


mean(data.mean.estimation.s$Mean)
# reverse log value 
10^(-0.1286705) #1/-0.7586291 -> by factor of 1.345

tapply(data.mean.estimation.s$Mean, data.mean.estimation.s$type, mean)

10^(0.3016063) # over by 2.002656
1/(10^(-0.5589472) )# 1/das = under by 3.62199

## correlation
#https://www.sciencedirect.com/science/article/pii/S0272494415300049#appsec1
###  within person correlation between estimates and actual -log transformed
# package ‘rmcorr’

rmcorr(ResponseId, estimate.log, actual.log, data=data.accuracy.long.c)
#r0.2758126.  ; degrees of freedom=7609 ;  p-value =6.153437e-133;   95% confidence interval = 0.2549256 0.2964424 

cor.test(data.accuracy.long.c$estimate.log, data.accuracy.long.c$actual.log)
#0.1568831

#### H3a ## RECHECK

set_label(data.accuracy.long.total$mean.abs.error) <- "mean absolute error"
set_label(data.accuracy.long.total$mean.freq.clean) <- "mean device frequency"

model.accuracy.freq2 <- lm(mean.abs.error ~ gender + age + country + income + education +
                             climate_concern + mean.freq.clean + V2 ,
                           data= data.accuracy.model )
tab_model(model.accuracy.freq2)

set_theme(base = theme_classic() )
plot_model(model.accuracy.freq2, type="emm", 
           terms=c("mean.freq.clean"), 
           axis.lim=c(0.35,0.95))

set_label(data.accuracy.long.total$mean.abs.error) <- "mean absolute error"
set_label(data.accuracy.long.total$concern.scaled) <- "climate change concern"
set_label(data.accuracy.long.total$freq.binary) <- "device frequency"

model.accuracy.freq.binary <- lm(mean.abs.error ~ gender + age + country + income + education +
                                   climate_concern + freq.binary ,
                                 data= data.accuracy.model )
tab_model(model.accuracy.freq.binary)

set_theme(base = theme_classic() )
plot_model(model.accuracy.freq.binary, type="emm", 
           terms=c("climate_concern","freq.binary"), 
           axis.lim=c(0.35,0.95))


anova(model.accuracy.freq.binary)
# age effect yes and income aswell
em <- emmeans(model.accuracy.freq.binary,specs="age")
pairs(em, adjust="none")
# for income: level 1 to 3, 4 and level 2 to 3, 4 -> so only first and 2 and third and fourth to each other not significant
# for age 1 to 4,5 ; 2 to 3,4,5 



## frequency effect for only those assessments where frequncy assessed

#=waschmaschine, trockner, =spülmaschine, (Föhn war sehr unhilfreich deswegen nicht drin)
# behaviors waschmaschine = e ; trockner= f, und dishwasher = b
data.accuracy.long.c <- merge(data.accuracy.long.c,  weights.total, by="ResponseId")
data.accuracy.long.c <- merge(data.accuracy.long.c,  data.correlates.important, by="ResponseId")
data.accuracy.long.c <- merge(data.accuracy.long.c,  data.accuracy.frequency.s, by="ResponseId")

data.accuracy.long.total.freq <- data.accuracy.long.c %>% 
  select(ResponseId, behavior, estimate, actual,actual.log, estimate.log,
         gender, income, age, education, country, politicalorientation_1,trust.gov, trust.gov.scaled,
         climate_concern, concern.scaled, starts_with("emotion"),  V2, mean.freq.clean, freq.binary)
data.accuracy.long.total.freq <- data.accuracy.long.total.freq %>% 
  filter(behavior=="dishwasher" | behavior=="dryer" | behavior=="washing")

# need to recalculate error and bias
data.accuracy.long.total.freq$absolute.error.log <- abs(log10(data.accuracy.long.total.freq$estimate/data.accuracy.long.total.freq$actual) )
data.accuracy.long.total.freq$estimation.bias.log <- log10(data.accuracy.long.total.freq$estimate/data.accuracy.long.total.freq$actual)

# absolute error
data.abs.error.freq <- groupwiseMean(absolute.error.log ~ ResponseId,
                                     data= data.accuracy.long.total.freq,
                                     percentile= FALSE)
data.abs.error.freq <- data.abs.error.freq %>%
  rename(abs.error.3 = Mean) %>% select(!n)

# same for estimation bias
data.est.bias.freq <- groupwiseMean(estimation.bias.log ~ResponseId,
                                    data= data.accuracy.long.total.freq,
                                    percentile= FALSE,
                                    traditional = FALSE)

data.est.bias.freq <- data.est.bias.freq %>%
  rename(est.bias.3 = Mean) %>% select(!n)

# combine both for later modeling 
data.accuracy.test.freq <- merge(data.est.bias.freq, data.abs.error.freq, by=c("ResponseId"))
data.accuracy.test.freq$binary <- factor(ifelse(data.accuracy.test.freq$est.bias.3 < 0, "under", "over"))
data.accuracy.total.freq <- data.accuracy.long.total.freq %>% 
  select(ResponseId,gender, income, age, education, country, politicalorientation_1,trust.gov, trust.gov.scaled,
         climate_concern, concern.scaled, starts_with("emotion"),  V2, mean.freq.clean, freq.binary)
data.accuracy.total.freq <- unique(data.accuracy.total.freq)
data.accuracy.test.freq2 <- merge(data.accuracy.test.freq,data.accuracy.total.freq, by="ResponseId" )


## model
set_label(data.accuracy.test.freq2$mean.freq.clean) <- "mean device frequency"
set_label(data.accuracy.test.freq2$abs.error.3) <- "absolute mean error"
set_label(data.accuracy.test.freq2$est.bias.3) <- "mean estimation bias"

model.accuracy.error.freq <- lm(abs.error.3 ~ gender + age + income + country + education + 
                             climate_concern  + mean.freq.clean + V2,
                           data=data.accuracy.test.freq2)

summary(model.accuracy.error.freq)
tab_model(model.accuracy.error.freq)

set_theme(base = theme_classic() )
plot_model(model.accuracy.error.freq, type="emm", terms=c("mean.freq.clean", "climate_concern" ),
           axis.lim=c(0.5, 0.8))

model.accuracy.bias.freq <- lm(est.bias.3 ~ gender + age + income + country + education + 
                                  climate_concern  + mean.freq.clean + V2,
                                data=data.accuracy.test.freq2)

tab_model(model.accuracy.bias.freq)
plot_model(model.accuracy.bias.freq, type="emm", 
           terms=c("mean.freq.clean","climate_concern"), axis.lim=c(-0.18, 0.3))

# time spent on the accuracy task
data.accuracy.time <- data.pass %>% select(ResponseId,`accuracy.time_Page Submit` )

data.accuracy.time <- merge(data.accuracy.time, data.accuracy.model, by="ResponseId")
data.accuracy.time <- data.accuracy.time %>% rename(accuracy.time =`accuracy.time_Page Submit`)
data.accuracy.time <- data.accuracy.time %>%
  mutate(Minutes.accuracy = minute(seconds_to_period(accuracy.time)) )
ggplot(data.accuracy.time, aes(x=Minutes.accuracy)) +
  geom_boxplot() + theme_minimal()

data.accuracy.time.excl <- data.accuracy.time %>% filter(Minutes.accuracy < 4) %>% filter (accuracy.time >30)
ggplot(data.accuracy.time.excl, aes(x=accuracy.time)) +
  geom_boxplot() + theme_minimal()

model.accuracy.error.excl<- lm(mean.abs.error ~ gender + age + income + country + education + 
                                  climate_concern  + mean.freq.clean + V2,
                                data=data.accuracy.time.excl)

tab_model(model.accuracy.error.excl)




##### product choices  ####
data.product.2 <- read.csv("data/data.product.short.csv")
data.product.2 <- data.product.2[,-c(1)]
data.product.2 <- data.product.2 %>% rename(politcalorientation_1 = politcalorientation)

data.product.long <- read.csv("data/data.product.long.csv")
data.product.long <- data.product.long[,-c(1)]
data.product.long <- data.product.long %>% rename(politcalorientation_1 = politcalorientation)

class(data.product.long$price.level)
data.product.long$gender <- factor(data.product.long$gender)
data.product.long$age <- factor(data.product.long$age)
data.product.long$country <- factor(data.product.long$country)
data.product.long$income <- factor(data.product.long$income,
                                 levels=c( "<1'500€\n<3'100CHF","<1'500- 2'499€\n3'100-4'299CHF", "2'500- 4'000€\n<4'300- 5'899CHF", "> 4'000€\n>5'900 CHF") )
data.product.long$education <- factor(data.product.long$education, 
                                    levels=c("no formal education", "obligatory school", "middle school" , "degree"))
data.product.long$choice <- factor(data.product.long$choice)
data.product.long$energy.level <- factor(data.product.long$energy.level)
data.product.long$price.level <- factor(data.product.long$price.level)

## visual overview
data.product.sum <- data.product.2 %>%
  select(Sum) %>% group_by(Sum) %>% tally()
data.product.sum$prop <- data.product.sum$Sum/7
data.product.sum$percent <- data.product.sum$n/1628

## how many choice where pro-environmental

ggplot(data.product.sum, aes(x=prop, y=percent))  +
  geom_bar(stat="identity") +
  theme(axis.title = element_text(face = "bold"))  +
  theme(text=element_text(size = 18) ,axis.text = element_text(size = 17)  ) +
  labs(x="proportion of pro-environmental choices", y="percentage of people") +theme_minimal() +
  theme(axis.title = element_text(face = "bold"))  +
  theme(text=element_text(size = 18)  ,axis.text = element_text(size = 17)  )


# climate change concern and pro-env. prodiuct choice
ggplot(data.product.2, aes(x=concern.scaled, y=percent))  +
  geom_jitter() + geom_smooth() +
  labs(x="scaled climate change concern", y="proportion of pro-environmental choices") +
  theme_minimal()

data.product.2$concern.median <- ifelse(data.product.2$climate_concern >5, "high", "low")
ggplot(data.product.2, aes( x=Sum, fill=concern.median))  +
  geom_bar() + scale_fill_manual(values=c("darkgreen", "darkred")) +
  labs(x="scaled climate change concern", y="proportion of pro-environmental choices") +
  theme_minimal()

ggplot(data.product.2, aes( x=Sum, fill=income))  +
    geom_bar(position="dodge") + scale_fill_viridis_d() +
  labs(x="scaled climate change concern", y="proportion of pro-environmental choices") +
    theme_minimal()

ggplot(data.product.2, aes( x=Sum, fill=country) ) +
  geom_bar() + 
  theme_minimal()
# levels of price and energy

ggplot(data.product.long, aes(x=climate_concern, y=choice, color=energy.level)) +
  geom_jitter(aes(shape=price.level), alpha=0.4 , height=.1) + scale_color_manual(values=c("darkgreen", "darkblue")) +
  theme_minimal() 

data.product.long.grouped <- data.product.long %>% select(ResponseId,climate_concern, choice) %>%
  group_by(ResponseId,climate_concern, choice) %>% tally()


# distribution how many choices pro-env.
ggplot(data.product.long.grouped, aes(x=climate_concern, y=n, fill=choice)) +
  geom_bar(stat="identity", position="dodge") + scale_fill_manual(values=c("#D20537", "#A5D7D2")) +
  theme_minimal() 



model.product.choice <- glmer(choice ~ price.level * energy.level + energy.level*climate_concern +  price.level*climate_concern + 
                              country*price.level + country*energy.level + gender + income  + age  + (1|ResponseId) + (1|country) ,
                              data=data.product.long, 
                              family = binomial(link = "logit") )

summary(model.product.choice)
tab_model(model.product.choice)
plot_model(model.product.choice)
plot_model(model.product.choice,  type="emm", terms=c("climate_concern", "energy.level", "country"))
plot_model(model.product.choice,  type="emm", terms=c("climate_concern", "price.level", "country"))
plot_model(model.product.choice,  type="emm", terms=c( "climate_concern","energy.level"))


# random slopes
model.product.choice.rs <- glmer(choice ~ price.level * energy.level + energy.level*concern.scaled +  price.level*concern.scaled +
                                   country + gender + income  + age  + education + (1|ResponseId) + 
                                   (1 + price.level + energy.level + concern.scaled| country) ,
                              data=data.product.long,
                              family = binomial(link = "logit") )

summary(model.product.choice.rs)
tab_model(model.product.choice.rs)
plot_model(model.product.choice.rs)


###H4a: 
#Individual weights of the impact strength dimension will predict the number of climate-friendly choices 
#higher scores on the impact strength dimension variable are associated with a higher likelihood to choose the more energy efficient product options. 

#### H4b
# with weight for impact strength direction

data.everything <- read.csv("data/data.everything.csv")
data.everything <- data.everything[,-c(1)]
data.everything$energy.level <- factor(data.everything$energy.level)
data.everything$price.level <- factor(data.everything$price.level)
data.everything$country <- factor(data.everything$country)
data.everything$age <- factor(data.everything$age)
data.everything$gender <- factor(data.everything$gender)
data.everything$income <- factor(data.everything$income,
                               levels=c( "<1'500€\n<3'100CHF","<1'500- 2'499€\n3'100-4'299CHF", "2'500- 4'000€\n<4'300- 5'899CHF", "> 4'000€\n>5'900 CHF") )
data.everything$education <- factor(data.everything$education, 
                                  levels=c("no formal education", "obligatory school", "middle school" , "degree"))

data.everything$politicalorientation.scaled <- scale(data.everything$politcalorientation, scale=FALSE)[,1]

model.H4a1 <- glmer(choice ~ price.level * energy.level + energy.level*concern.scaled +  
                      country*price.level + country*energy.level + politicalorientation.scaled +
                      est.binary  + V2+ gender + age + income + education +  (1|ResponseId),
                    data=data.everything, family="binomial")

summary(model.H4a1)
tab_model(model.H4a1)
plot_model(model.H4a1,  type="pred", terms=c("concern.scaled", "energy.level", "country"))
plot_model(model.H4a1,  type="pred", terms=c("concern.scaled", "price.level", "country"))
# take out age, gender and education because not significant and model does not yet converge, model fit still okay
model.H4a <- glmer(choice ~ price.level * energy.level  +  
                     (est.binary + concern.scaled)* energy.level + politicalorientation.scaled  +
                     country*price.level + country*energy.level + 
                     V2+ gender + age + income + education +  (1| ResponseId),
                   data=data.everything, family="binomial")

summary(model.H4a)
tab_model(model.H4a)
plot_model(model.H4a, type="pred", terms=c("est.binary", "energy.level"))
plot_model(model.H4a, type="pred", terms=c("est.binary", "price.level"))

model.H4a.c <- glmer(choice ~ price.level * energy.level  +  
                       (mean.abs.error + concern.scaled)* energy.level   +
                       country*price.level + country*energy.level  +
                       V2+ gender + age + income + education +  (1| ResponseId),
                     data=data.everything, family="binomial")

summary(model.H4a.c)
plot_model(model.H4a.c,
          rm.terms = c("education [obligatory school, middle school , degree]", "age", "age[]",
                       "age30-39", "age40-49", "age50-59", "age60-80", "income<1'500€\n<3'100CHF",
                       "income<1'500- 2'499€\n3'100-4'299CHF",
             "income [<1'500- 2'499€ 3'100-4'299CHF, <1'500- 2'499€\n3'100-4'299CHF, 2'500- 4'000€\n<4'300- 5'899CHF, > 4'000€\n>5'900 CHF]", "V2",
             "gender [male]") )

plot_model(model.H4a.c, type="pred", terms=c("concern.scaled", "energy.level"),
           colors="#BFCF5B", "#9CA84A")
plot_model(model.H4a.c, type="pred", terms=c("mean.abs.error", "energy.level"),
           colors="#BFCF5B", "#9CA84A")

# with judgment interaction energy level

model.choice.bias <-  glmer(choice ~ price.level * energy.level  + 
                               (mean.est.bias + concern.scaled)* energy.level +
                               country*price.level + country*energy.level + politicalorientation.scaled + trust.gov.scaled +
                               V2+ gender + age + income + education +  (1| ResponseId),
                             data=data.everything, family="binomial")

summary(model.choice.error)
tab_model(model.choice.error)
plot_model(model.choice.error, type="pred", terms=c("mean.abs.error", "energy.level"))
plot_model(model.choice.error, type="pred", terms=c("mean.abs.error", "concern.scaled" , "energy.level"))


# random slope because also for policy?
model.choice.error.s <-  glmer(choice ~ price.level * energy.level   +
                               (mean.abs.error+ concern.scaled)* energy.level +
                               country*price.level + country*energy.level + politicalorientation.scaled + trust.gov.scaled +
                                 V2+ gender + age + income + education +  
                                 (1 + price.level + energy.level| ResponseId),
                             data=data.everything, family="binomial")

summary(model.choice.error.s)
tab_model(model.choice.error.s)
plot_model(model.choice.error.s, type="pred", terms=c("mean.abs.error", "energy.level"))
plot_model(model.choice.error.s, type="pred", terms=c("mean.abs.error", "price.level"))



#### policy and judgment and product ####
##### check  for policy decisions

class(data.decision$zeitpunkt)
data.decision$Sum.product.scaled <- scale(data.decision$Sum.product)
data.decision$politicalorientation.scaled <- scale(data.decision$politicalorientation_1)

set_label(data.decision$Sum.product) <- "Sum of proenvironmental product choices"
set_label(data.decision$mean.abs.error) <-  "mean absolute error"
set_label(data.decision$zeitpunkt) <-  "implementation time"
data.decision$decision <- factor(data.decision$decision)

# without emotions.re.pos.scaled because highly correlate with concern climate change

model.policy.acc.t <- glmer(decision ~  co2  + tax +energieabhaengigkeit +
                              gender + age + income  + country + education +
                              trust.gov.scaled + politicalorientation.scaled  + emotions.crisis.neg.tot.sc +
                              (mean.abs.error + concern.scaled + Sum.product.scaled)*zeitpunkt   + 
                              (1 + tax + co2 + energieabhaengigkeit + zeitpunkt|ResponseId),
                            family="binomial",
                            data=data.decision)
summary(model.policy.acc.t)
tab_model(model.policy.acc.t)

plot_model(model.policy.acc.t, type="pred", terms=c("Sum.product.scaled","zeitpunkt"))
plot_model(model.policy.acc.t, type="pred", terms=c("mean.abs.error","zeitpunkt"))
plot_model(model.policy.acc.t, type="pred", terms=c("mean.abs.error","concern.scaled"))

# enter interactions of error and Sum.prodct seperately / step-wise (?)


# first without either
model.policy.acc.0 <- glmer(decision ~  co2  + tax +energieabhaengigkeit + zeitpunkt*concern.scaled +
                              gender + age + income  + country + education +
                              trust.gov.scaled + politicalorientation.scaled  +
                              (1 + tax + co2 + energieabhaengigkeit + zeitpunkt|ResponseId),
                            family="binomial",
                            data=data.decision)
tab_model(model.policy.acc.0)


# with error without Sum.product
model.policy.error.t <- glmer(decision ~  co2  + tax +energieabhaengigkeit +
                              gender + age + income  + country + education +
                              trust.gov.scaled + politicalorientation.scaled  +
                              (mean.abs.error + concern.scaled)*zeitpunkt   + 
                              (1 + tax + co2 + energieabhaengigkeit + zeitpunkt|ResponseId),
                            family="binomial",
                            data=data.decision)
tab_model(model.policy.error.t)
plot_model(model.policy.error.t, type="pred", terms=c("mean.abs.error","zeitpunkt"))

# without emotions.re.pos.scaled because highly correlate with concern climate change


# not scaled sum product because can acutally be 0
model.policy.product.t <- glmer(decision ~  co2  + tax +energieabhaengigkeit +
                                gender + age + income  + country + education +
                                trust.gov.scaled + politicalorientation.scaled  +
                                (Sum.product.scaled + concern.scaled)*zeitpunkt   + 
                                (1 + tax + co2 + energieabhaengigkeit + zeitpunkt|ResponseId),
                              family="binomial",
                              data=data.decision)

tab_model(model.policy.product.t)
plot_model(model.policy.product.t, type="pred", terms=c("Sum.product.scaled","zeitpunkt"))

# for not scaled sum product neither main effect nor interaction with time are signiciant 

# predicitng sum of accepted policies
data.decision$decision.n <- as.numeric(data.decision$decision)
data.decision$decision.n <- data.decision$decision.n -1
proportion.t <- groupwiseSum(decision.n ~ ResponseId + zeitpunkt,
                             data =data.decision,
                             traditional = FALSE,
                             percentile = FALSE)
data.accuracy.policy.sum <- merge(data.decision, proportion.t, by=c("ResponseId", "zeitpunkt"))
data.accuracy.policy.sum <- data.accuracy.policy.sum %>% rename(Sum.time = Sum)
data.accuracy.policy.sum <- data.accuracy.policy.sum %>%
  select(ResponseId, gender, age, income , country, education, climate_concern,  mean.abs.error, mean.est.bias, Sum.product,Sum.time,zeitpunkt, politicalorientation_1)
data.accuracy.policy.sum <- unique(data.accuracy.policy.sum)
data.accuracy.policy.sum$politicalorientation.scaled <- scale(data.accuracy.policy.sum$politicalorientation_1)
data.accuracy.policy.sum <- merge(data.accuracy.policy.sum, data.correlates.important, by=c("ResponseId"))

set_label(data.accuracy.policy.sum$mean.abs.error) <- "mean absolute error"
set_label(data.accuracy.policy.sum$zeitpunkt) <- "implementation time"
set_label(data.accuracy.policy.sum$Sum.time) <- "Sum of accepted renwable energy policies"
set_label(data.accuracy.policy.sum$politicalorientation.scaled) <- "politcal orientation"
set_label(data.accuracy.policy.sum$climate_concern) <- "climate concern"
set_label(data.accuracy.policy.sum$Sum.product) <- "Sum of pro-environmental product choices"

model.policy.acc.sum <- lmer(Sum.time ~    gender + age + income + country   +politicalorientation_1  + education + zeitpunkt +
                                emotions.crisis.neg.tot + trust.gov  +climate_concern + Sum.product + (1|ResponseId) +  (1 + zeitpunkt|ResponseId),
                             data=data.accuracy.policy.sum )

tab_model(model.policy.acc.sum)
plot_model(model.policy.acc.sum, type="emm", terms=c("mean.abs.error", "zeitpunkt"),
           colors=c( "darkgreen", "darkorange") )
plot_model(model.policy.acc.sum, type="emm", terms=c("climate_concern", "zeitpunkt"))
plot_model(model.policy.acc.sum, type="emm", terms=c("Sum.product", "zeitpunkt"))
plot_model(model.policy.acc.sum, type="pred", terms=c("Sum.product", "climate_concern"))
plot_model(model.policy.acc.sum, type="emm", terms=c("mean.abs.error", "climate_concern", "zeitpunkt"))
plot_model(model.policy.acc.sum, type="pred", terms=c("Sum.product", "climate_concern", "zeitpunkt"))

# mit interaction  (mean.abs.error +climate_concern + Sum.product) *zeitpunkt
# sieht das etwas anders aus - looks like the effect of less knowledge only hold for distant policies


data.accuracy.policy.sum.cor <- merge(data.accuracy.policy.sum, data.acc.models, by=c("ResponseId"))
model.policy.cor.sum <- lmer(Sum.time ~    gender + age + income + country + politicalorientation_1  + education +
                               emotions.crisis.neg.tot + trust.gov + (cor + sd + Sum.product+climate_concern) *zeitpunkt + (1|ResponseId),
                             data=data.accuracy.policy.sum.cor )
tab_model(model.policy.cor.sum)
plot_model(model.policy.cor.sum, type="emm", terms=c("Sum.product", "climate_concern", "zeitpunkt"))
plot_model(model.policy.cor.sum, type="emm", terms=c("sd", "climate_concern"))
plot_model(model.policy.cor.sum, type="emm", terms=c("cor", "climate_concern", "zeitpunkt"))

data.accuracy.product.short <- data.accuracy.product[,c(1:21, 25)]
data.accuracy.product.short <- unique(data.accuracy.product.short)
data.accuracy.product.cor <- merge(data.accuracy.product.short, data.correlates.important, by=c("ResponseId"))

# Sums by energy level
data.product.long$choice.n <- as.numeric(data.product.long$choice)
data.product.long$choice.n <- (data.product.long$choice.n)-1
data.product.sum <- groupwiseSum(choice.n ~ energy.level + ResponseId,
                                 data=data.product.long)
data.product.sum$proportion.product <- round(data.product.sum$Sum/data.product.sum$n,2)
data.product.sum <- data.product.sum %>% select(!n) %>% select(!Sum)

data.accuracy.product.cor.e <- merge(data.accuracy.product.cor,data.product.sum, by=c("ResponseId"))

set_label(data.accuracy.product.cor$mean.abs.error) <- "mean absolute error"
set_label(data.accuracy.product.cor$Sum.time) <- "Sum of accepted renwable energy policies"
set_label(data.accuracy.product.cor$climate_concern) <- "climate concern"
set_label(data.accuracy.product.cor$Sum.product) <- "Sum of pro-environmental product choices"

# withe stimation bias - H4
model.product.est.sum <- lm(Sum.product ~    gender + age + income + country  + mean.est.bias+ climate_concern  +
                              trust.gov + emotions.crisis.neg.tot +Sum.policy,
                            data=data.accuracy.product.cor )
tab_model(model.product.est.sum)
plot_model(model.product.est.sum, type="emm", terms=c("mean.est.bias"))

# with error
model.product.err.sum <- lm(Sum.product ~    gender + age + income + country +education +
                              mean.abs.error+ climate_concern  +
                               trust.gov + emotions.crisis.neg.tot +Sum.policy,
                            data=data.accuracy.product.cor )
tab_model(model.product.err.sum)
plot_model(model.product.err.sum, type="emm", terms=c("mean.abs.error", "climate_concern"))
# by energylevel
model.product.err.sum.e <- lmer(proportion.product ~    gender + age + income + country +education +
                              (mean.abs.error+ climate_concern)* energy.level  +
                              trust.gov + emotions.crisis.neg.tot +Sum.policy + (1|ResponseId),
                            data=data.accuracy.product.cor.e )
tab_model(model.product.err.sum.e)
plot_model(model.product.err.sum.e, type="emm", terms=c("mean.abs.error", "climate_concern", "energy.level"))
#proportion of choices by interaction error and energy level
plot_model(model.product.err.sum.e, type="emm", terms=c("mean.abs.error", "energy.level"),
           colors=c("darkorange", "darkgreen") )
plot_model(model.product.err.sum.e, type="emm", terms=c("climate_concern", "energy.level"),
           colors=c("darkorange", "darkgreen") )
# woth cor and sd
model.product.cor.sum.e <- lmer(proportion.product ~     gender + age + income + country +education +
                              (cor + sd+ climate_concern)* energy.level  +
                              trust.gov + emotions.crisis.neg.tot +Sum.policy + (1|ResponseId),
                            data=data.accuracy.product.cor.e )
tab_model(model.product.cor.sum.e)
plot_model(model.product.cor.sum.e, type="emm", terms=c("cor", "climate_concern", "energy.level"))
plot_model(model.product.cor.sum.e, type="emm", terms=c("cor", "energy.level"))

### polarization stuff
#polarization_perc, polarization_actual

data.polarization <- data.pass %>% select(ResponseId, polarization_perc, polarization_actual)
data.polarization2 <- merge(data.polarization, data.correlates.important2, by=c("ResponseId"))

ggplot(data.polarization2, aes(x=polarization_actual)) +
  geom_histogram() + theme_minimal()
# actual: -3 = halte massnahmen für absolut nicht notwendig; +3= absolut notwendig
ggplot(data.polarization2, aes(x=polarization_perc)) +
  geom_histogram() + theme_minimal()
# perceived:  -3 = sehr einig massnahmen absolut nicht notwendig; 0= sehr uneinig ; +3= sehr einig massnahmen absolut notwendig
data.polarization2$polarization_actual.f <- factor(data.polarization2$polarization_actual)
ggplot(data.polarization2, aes(x=polarization_perc, y=polarization_actual)) +
  geom_jitter(aes( color=polarization_actual.f)) + theme_minimal()
describe(data.polarization2$polarization_actual)

ggplot(data.polarization2, aes(x=polarization_actual, y=trust.gov)) +
  geom_jitter() + theme_minimal()


data.polarization.dec  <- merge(data.decision, data.polarization, by=c("ResponseId"))

model.H1b.p <- glmer(decision~ co2  + tax +energieabhaengigkeit + polarization_perc +trust.gov.scaled +
                          emotions.crisis.neg.tot   + zeitpunkt*concern.scaled +
                          (1 + co2 + tax + energieabhaengigkeit + zeitpunkt | id),
                        data = data.polarization.dec,family = binomial(link = "logit"))
summary(model.H1b.p)
tab_model(model.H1b.p)

model.H1b.p.a <- glmer(decision~ co2  + tax +energieabhaengigkeit + polarization_actual + trust.gov.scaled +
                       emotions.crisis.neg.tot   + zeitpunkt*concern.scaled +
                       (1 + co2 + tax + energieabhaengigkeit + zeitpunkt | id),
                     data = data.polarization.dec,family = binomial(link = "logit"))
summary(model.H1b.p.a)
tab_model(model.H1b.p.a)


###  other ideas #####


# rank order of estimates
# correct order = shower, heating, dryer, washing, hairdryer, dishwasher

data.rank.order <- data.accuracy.long.c %>% select(ResponseId, estimate, actual) %>%
  group_by(ResponseId) %>%
  mutate(ranks.estimate = rank(estimate, ties.method='average'),
         ranks.actual = rank(actual, ties.method='average') )

data.rank.order$ranks.actual.f <- factor(data.rank.order$ranks.actual)
data.rank.order$ranks.estimate.f <- factor(data.rank.order$ranks.estimate)

cor.test(x=data.rank.order$ranks.estimate, y=data.rank.order$ranks.actual, method = 'spearman')

# per person
accList2 <- split(data.rank.order,data.rank.order$ResponseId)
length(accList2)
names(accList2) <- paste0("id",1:length(accList2))


data.cor.rank <- data.frame(matrix(NA,    # Create empty data frame
                              nrow = 1522,
                              ncol = 2))

for (i in c(1:(nrow(data.rank.order)/6) )) { 
  data.cor.rank$cor.rank[[i]] <- cor( accList2[[c(i,4)]], accList2[[c(i,5)]],  method = 'spearman')
}

acc2.id <- unique(data.rank.order$ResponseId)
data.cor.rank$X1 <- acc2.id
data.cor.rank <- data.cor.rank %>% select(!X2) %>%
  rename(ResponseId = X1)
data.cor.rank$cor.rank <- as.numeric(data.cor.rank$cor.rank)

describe(data.cor.rank$cor.rank)
data.cor.rank$cor.rank.r <- round(data.cor.rank$cor.rank,1)

accuracy.cor.id <- data.cor.rank %>% 
  select(ResponseId, cor.rank) %>% 
  filter(cor.rank >0)

# those with at least 0 cor use this as exlucsion inclusion criteria

data.accuracy.product.cor.rank <- merge(data.accuracy.product.cor.e, data.cor.rank, by="ResponseId")

model.product.rank<- lmer(proportion.product ~    gender + age + income + country +education +
                                  (cor.rank+ climate_concern)* energy.level  +
                                  trust.gov + emotions.crisis.neg.tot +Sum.policy + (1|ResponseId),
                                data=data.accuracy.product.cor.rank )
plot_model(model.product.rank, type="pred", terms=c("cor.rank", "energy.level"),
           colors=c(  "darkorange", "darkgreen") )

data.accuracy.policy.rank <- merge(data.accuracy.policy.sum, data.cor.rank, by="ResponseId")
model.policy.acc.rank <- lmer(Sum.time ~    gender + age + income + country   +politicalorientation_1  + education +
                               emotions.crisis.neg.tot + trust.gov + (cor.rank +climate_concern + Sum.product) *zeitpunkt + (1|ResponseId),
                             data=data.accuracy.policy.rank )

plot_model(model.policy.acc.sum, type="emm", terms=c("mean.abs.error", "zeitpunkt"),
           colors=c( "darkgreen", "darkorange") )
plot_model(model.policy.acc.rank, type="emm", terms=c("cor.rank", "zeitpunkt"),
           colors=c( "darkgreen", "darkorange") )



###impacted ##
table(data.pass$impact)
# 1= sehr negativ
# 6= sehr positiv
data.decision$impact.scaled <-  scale(data.decision$impact ,scale=FALSE)[,1]
model.policy.acc.t2 <- glmer(decision ~  co2  + tax +energieabhaengigkeit +
                               gender + age   + country + education + income +
                               trust.gov.scaled + politicalorientation.scaled  + impact.scaled +
                               (mean.abs.error + concern.scaled + Sum.product.scaled)*zeitpunkt   + 
                               (1 + tax + co2 + energieabhaengigkeit + zeitpunkt|ResponseId),
                             family="binomial",
                             data=data.decision)
tab_model(model.policy.acc.t2)

model.policy.interaction1 <- glmer(decision ~  co2  + tax +energieabhaengigkeit +
                                   gender + age + income  + country + education +
                                   (trust.gov.scaled + politicalorientation.scaled  + concern.scaled + emotions.crisis.neg.tot.sc)*zeitpunkt   + 
                                   (1 + tax + co2 + energieabhaengigkeit + zeitpunkt|ResponseId),
                                 family="binomial",
                                 data=data.decision)
tab_model(model.policy.interaction1)
# crisis emtoions not associated with support for policies neither directly or with intetraction time
# without crisis emotions:

# interact climate concern and politcal orientation with time because conceivable that that makes a difference
# higher cc concern react more to immediate vs late, but for trust in government it shouldnt vary with time per se
# gender education etc.? not sure why it should either
model.policy.interaction <- glmer(decision ~  co2  + tax +energieabhaengigkeit +
                                   gender + age + income  + country + education + trust.gov +
                                   (politicalorientation_1  + climate_concern )*zeitpunkt   + 
                                   (1 + tax + co2 + energieabhaengigkeit + zeitpunkt|ResponseId),
                                 family="binomial",
                                 data=data.decision)
tab_model(model.policy.interaction)

model.product.H4<- glmer(choice ~   price.level * energy.level + energy.level*climate_concern +  
                                    country*price.level + country*energy.level  +  mean.abs.error*energy.level  + 
                                     gender +  income  +  (1 + energy.level + price.level|ResponseId),
                                  data=data.everything, family="binomial")
tab_model(model.product.H4)
plot_model(model.product.H4, type="emm", terms=c("climate_concern", "energy.level"))
plot_model(model.product.H4, type="emm", terms=c("mean.abs.error", "energy.level"))

# random intercept not random slope
model.product.H4.2 <- glmer(choice ~   price.level * energy.level + energy.level*climate_concern +  
                           country*price.level + country*energy.level  +  Sum.policy*energy.level   + emotions.crisis.neg.tot.sc +
                           gender +  income  +  (1|ResponseId),
                         data=data.everything, family="binomial")
tab_model(model.product.H4.2)
plot_model(model.product.H4.2, type="emm", terms=c("climate_concern", "energy.level"))
plot_model(model.product.H4.2, type="emm", terms=c("Sum.policy", "energy.level"))

###### emotions and decisions ####
model.policy.emot <- glmer(decision ~  co2  + tax +energieabhaengigkeit + zeitpunkt*concern.scaled +
                             gender + age + income  + country + education +
                             trust.gov.scaled + politicalorientation.scaled  + emotions.crisis.neg.tot +
                             (1 + tax + co2 + energieabhaengigkeit + zeitpunkt|ResponseId),
                           family="binomial", data=data.decision)
tab_model(model.policy.emot)
plot_model(model.policy.emot, type="pred", terms=c("emotions.crisis.neg.tot", "zeitpunkt"))

model.product.emot <- glmer(choice ~   price.level * energy.level + energy.level*climate_concern +  
                              country*price.level + country*energy.level   + emotions.crisis.neg.tot.sc +
                              gender +  income  + education + (1|ResponseId),
                            data=data.everything, family="binomial")
tab_model(model.product.H4.emotion)
plot_model(model.product.H4.emotion, type="emm", terms=c("emotions.crisis.neg.tot.sc", "energy.level"))


#### what predicts complexity / weight placed on impact strength dimension?
cor(data.accuracy.model$V2, data.accuracy.model$climate_concern)
cor(data.accuracy.model$V2, data.accuracy.model$mean.abs.error)

model.weight <- lm(V2 ~ mean.abs.error + gender + age + income + education + climate_concern , 
                     data=data.accuracy.model)
tab_model(model.weight)

set_theme(base= theme_classic() )

plot_model(model.weight, type="emm", terms=c("climate_concern"))


data.test <- data.everything %>% select(ResponseId, education, gender, country, age, income, V2, mean.abs.error, mean.est.bias, climate_concern, Sum.policy, Sum.product)
data.test <- unique(data.test)

model.product.weight <- lm(Sum.product ~ mean.abs.error + gender + age + income + education + climate_concern + V2, 
                   data=data.test)
tab_model(model.product.weight)


model.weight <- lm(V1 ~ mean.abs.error + gender + age + income + education + concern.scaled , 
                   data=data.accuracy.model4)
tab_model(model.weight) 




