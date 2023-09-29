#### winter study DCE

library(psych)
library(tidyverse)
library(rcompanion)
library(sjPlot) # for tab_model
library(lmerTest); library(lme4)
library(logistf)


data.decision <- read.csv(file.choose())

## explanations:
# long format, for each person 16 decisions 
# the decisions are not choices between different alternatives! Just Yes/No for one option
#("counter" but I am relatively sure that that is not the correct order so 1 does not mean it was the first they saw)
# 4 attributes in decision task, all have 2 levels
# co2: 15% or 30% reduction
#energieabhaengigkeit = energy independence 10% or 20%
# tax 1% or 6%
# zeitpunkt = implementation start in 1 year or in 7 years
# response = character variable, decision is the numeric choice
# the variables.n are the same just defined as numeric with the lower value 0 and hiher value 1
#RespsonseId and id are id'variables, one character one numeric

###### models  H1 and H1b######

#### Model of policy decisions  with all demographics and poliitcal orientation
model.dec.all <- glmer(decision~ co2 + tax + energieabhaengigkeit + zeitpunkt + gender + age + income + country + education + country +  politicalorientation_1 + (1|id), data=data.decision, family="binomial")
summary(model.dec.all)
tab_model(model.dec.all)
# with numeric not as factors just to check 
model.dec.all.n <- glmer(decision~ co2 + tax + energieabhaengigkeit + zeitpunkt + gender + age + income + country + education + country +  politicalorientation_1 + (1|id), data=data.decision, family="binomial")
summary(model.dec.all.n)

tab_model(model.dec.all.n)

#### H1 :  controlled for age, gender, country of origin and income as per pre-registration

model.H1 <- glmer(decision~ co2.f + tax.f + energieabhaengigkeit.f + zeitpunkt.f + gender + age + income + country  + (1|id),
                  data=data.decision, family="binomial")
summary(model.H1)
tab_model(model.H1)

model.H1.n <- glmer(decision~ co2.n + tax.n + energieabhaengigkeit.n + zeitpunkt.n   + gender + age + income + country + (1|id),
                    data=data.decision, family="binomial")
summary(model.H1.n)
tab_model(model.H1.n) # for  OR's

model.H1.no.dem <- glmer(decision~ co2.n + tax.n + energieabhaengigkeit.n + zeitpunkt.n  +  (1|id),
                  data=data.decision, family="binomial")
summary(model.H1.no.dem)
tab_model(model.H1.no.dem)

## H1b 
## climate change concern : overall effect plus interaction between concern and implementation time factor

model.H1b <- glmer(decision~ co2 + tax + energieabhaengigkeit + (zeitpunkt * concern.scaled) + gender + age + income + country  + (1| id),
                   data = data.decision,family = binomial(link = "logit"))

summary(model.H1b)
tab_model(model.H1b)

###### model H1c #####

# the purpose of this is supposed to be to extract the coefficient of the co2 attribute for each person

# this was the code I used before with "normal logistic regression" for each person
# there the coefficients var between -138 and 140 or something
co2_coefs = c(rep(NA,nrow(data.decision)/16))
 for (i in c(1:nrow(data.decision)/16)) { 
  data.decision.new <-
    data.decision[data.decision$id==i,]
  # Perform regression:
  reg_result <-  glm(decision~ co2 + tax + energieabhaengigkeit + zeitpunkt  ,data=data.decision.new, family="binomial")
  # Get coefficient:
  tmp_coef <- summary(reg_result)[[c(12,2)]]
  # Store coefficient:
  co2_coefs[i] <- tmp_coef[1]
}


# this is apparently a more robust logistic regression
# logistif package 
nrow(data.decision)/16
co2_coefs = c(rep(NA,nrow(data.decision)/16))
for (i in c(1:1577)) { 
  data.decision.new <-
    data.decision[data.decision$id==i,]
  # Perform regression:
  reg_result <-  logistf(decision~ co2 + tax + energieabhaengigkeit + zeitpunkt ,data=data.decision.new)
  # Get coefficient:
  tmp_coef <- coefficients(reg_result)[2]
  # Store coefficient:
  co2_coefs[i] <- tmp_coef[1]
}
# now the extremes are an OR of 0.016 and 107.7701 -> should probably limit it still

####
# then add all relevant demographics back -> since it does it stepwise matching like this works
id <-data.decision %>% select(id, ResponseId, gender, income, age, country, education, politicalorientation_1)
id <- unique(id)

data.coef <- as.data.frame(co2_coefs)
data.coef$ResponseId <- unique(id$ResponseId)
data.coef$gender <- factor(id$gender)
data.coef$income <- factor(id$income)
data.coef$age <- factor(id$age)
data.coef$country <- factor(id$country)
data.coef$education <- factor(id$education)
data.coef$politicalorientation_1 <- (id$politicalorientation_1)

describe(data.coef$co2_coefs)
ggplot(data.coef, aes(x=co2_coefs)) +
  geom_histogram(bins=50)
ggplot(data.coef, aes(x=co2_coefs)) +
  geom_boxplot()

# 2.5 would mean an OR of 12.2 so limit to something like that
data.coef.excl <- data.coef %>%
  filter(co2_coefs < 2.5) %>%
  filter(co2_coefs > -2.5) 

ggplot(data.coef.excl, aes(x=co2_coefs)) +
  geom_histogram(bins=50)
describe(data.coef.excl$co2_coefs)

data.c <- merge(data.coef.excl, data.cc.2, by="ResponseId")
data.c$id <- c(1:nrow(data.c))


model.cc.d <- lm(co2_coefs ~ concern.scaled + gender + age + income + country, data=data.c )
summary(model.cc.d)
tab_model(model.cc.d)

ggplot(data.c, aes(x=climate_concern, y=co2_coefs)) +
  geom_point() + geom_smooth()


### Maria code

model.H1b.no.dem <- glmer(decision~ co2 + tax + energieabhaengigkeit + zeitpunkt  + (1| id) + (1 + co2 + tax + energieabhaengigkeit + zeitpunkt | id),
                   data = data.decision,family = binomial(link = "logit"))

summary(model.H1b.no.dem)
tab_model(model.H1b)

### Maria code

### equivalent for me
AgentsModelH1.cc <- coef(model.H1b.no.dem)$id
Model_H1b <- coef(model.H1b.no.dem)
ModelH1bAgents<- cbind(rownames(AgentsModelH1.cc),AgentsModelH1.cc)
colnames(ModelH1bAgents)[colnames(ModelH1bAgents) == "rownames(ModelH1bAgents)"] <- "id"
write_csv(ModelH1bAgents, file = "ModelH1bAgents.n.csv") #here you can find the betas

data.beta <- read.csv(file.choose())
data.beta <- data.beta %>%
  rename(id = "rownames.AgentsModelH1.cc." )
names(data.beta)

describe(data.beta$co2.30..CO2)
data.beta.co2 <- data.beta %>%select (id, co2.30..CO2)
data.model.co2.c <- merge(data.beta.co2, id, by=c("id"))
data.model.co2.c <- merge(data.model.co2.c, data.cc.2, by=c("ResponseId"))

model.cc.d <- lm(co2.30..CO2 ~ concern.scaled + gender + age + income + country, data=data.model.co2.c )
summary(model.cc.d)
tab_model(model.cc.d)

ggplot(data.model.co2.c, aes(x=climate_concern, y=co2.30..CO2)) +
  geom_point() + geom_smooth()



