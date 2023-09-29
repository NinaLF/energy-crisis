### winter study wave 1 data preparation


#### prep data ###### 
library("lubridate")
library(jsonlite)
library(readxl)
library(psych)
library(tidyverse)
library(rcompanion)
library(broom)
library(vegan)
library(sjPlot) # for tab_model
library(lmerTest); library(lme4)
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



setwd("/Users/nfrings/Library/Mobile Documents/com~apple~CloudDocs/Documents/PhD/Studies/Winter studie/wave1 results and R")

data_raw0 <- read_csv("data/Energiekrise_Hauptstudie_15.+Dezember+2022_12.44.csv")

data_raw1 <- data_raw0[-c(1, 2),] %>% type_convert()

#### create subset to analyze Qualtrics parts of survey (i.e., understanding of scenario and Judgement Task Accuracy) ####
data_clean0 = subset(data_raw1, select = -c(Status,
                                            StartDate,
                                            EndDate,
                                            IPAddress,
                                            Progress,
                                            Finished,
                                            RecordedDate,
                                            RecipientLastName,
                                            RecipientFirstName,
                                            RecipientEmail,
                                            ExternalReference,
                                            LocationLatitude,
                                            LocationLongitude,
                                            DistributionChannel,
                                            UserLanguage
) )

data <- data_clean0
data$decision.time <- as.numeric(data$`decision.time_Page Submit`)

# time to complete survey
data <- data %>%
  mutate(Minutes.decision = minute(seconds_to_period(decision.time)) )

#quality check - filter out those that failed the attention check
table(data$`attention check`)

data.pass <- data %>% filter(`attention check`==4)  %>% filter(!gender==4 ) %>% filter(!gender==3)
# combine income and education that are seperate for DE and CH into one value
data.pass <- data.pass %>% rowwise() %>% 
  mutate(income = sum(income_ch, income_de, na.rm=TRUE))  %>% 
  rowwise() %>% 
  mutate(education = sum(education_ch, education_de, na.rm=TRUE))%>% 
  filter(!income==0)%>%  filter(!education==0)

# demographics as factors
data.pass <- data.pass %>%
  mutate(
         age = factor(age, levels=c(4,5,6,7,8), 
                      label= c("18-29", "30-39", "40-49", "50-59", "60-80" )),
         gender = factor(gender, levels = c(1,2),
                         labels=c("male", "female")),
         country = factor(country, levels=c(1,2 ),
                          labels=c("Germany", "Switzerland")),
         income = factor(income, levels = c(2,3,4,5),
                         labels=c(paste0("<1'500€", "\n", "<3'100CHF"), paste0("<1'500- 2'499€", "\n", "3'100-4'299CHF"),
                                  paste0("2'500- 4'000€", "\n", "<4'300- 5'899CHF"), paste0("> 4'000€", "\n", ">5'900 CHF") )) ,
         education = factor(education, levels=c(1,2,3,4,5,6), 
                            labels=c("no formal education", "obligatory school", "middle school", "degree", "degree", "degree") ))
# degree includes small number of abitur -> so degree or qualified to go to Uni if leave it as is too different sizes the factor

### checks for quota only for those that filled out the whole survey
data.full <- data.pass %>% select(ResponseId, gender, age, country, income,education, household_size, politicalorientation_1, impact)

# only gender which identifiy as male or female
data.full <- data.full %>% select(ResponseId,gender, age, country, income, education, household_size,politicalorientation_1, impact, events) %>%
  filter(gender=="female" | gender=="male")
nrow(data.full)

#
data.full <- na.omit(data.full)
nrow(data.full)
table(data.full$gender, data.full$country)
table(data.full$age, data.full$country)
table(data.full$income, data.full$country)



####### DCE ########
# data prep for DCE
labjs_column1 <- 'labjs.decision'

# Unpack the JSON data and discard the compressed version
data_raw1 %>%
  # Provide a fallback for missing data
  mutate(
    !!(labjs_column1) := recode(.[[labjs_column1]], .missing='[{}]') 
  ) %>%
  # Expand JSON-encoded data per participant
  group_by_all() %>%
  do(
    fromJSON(.[[labjs_column1]], flatten=T)
  ) %>%
  ungroup() %>%
  # Remove column containing raw JSON
  select(-matches(labjs_column1)) -> data1


data_clean1 = subset(data1, select = c(ResponseId,
                                       co2,
                                       tax,
                                       energieabhaengigkeit,
                                       zeitpunkt,
                                       counter,
                                       response
) )

data.decision <- na.omit(data_clean1)
# so only those that responded to all questions included
data.decision <- merge(data.decision,data.full, by="ResponseId" )
data.decision$id <- c(rep(1:(nrow(data.decision)/16), each=16))

# make dependent variable numeric
data.decision$decision <- data.decision$response
data.decision$decision[data.decision$decision=="Dafür"] <- 1
data.decision$decision[data.decision$decision=="Dagegen"] <- 0
data.decision$decision <- as.numeric(data.decision$decision)
# attributes as factors
data.decision <- data.decision %>%
  mutate(co2 = factor(co2),
         tax = factor(tax),
         energieabhaengigkeit = factor(energieabhaengigkeit),
         zeitpunkt = factor(zeitpunkt))

data.decision <- data.decision %>%
  mutate(co2.n= as.numeric(co2),
         tax.n = as.numeric(tax),
         energieabhaengigkeit.n = as.numeric(energieabhaengigkeit),
         zeitpunkt.n = as.numeric(zeitpunkt))

data.decision$co2.n <- data.decision$co2.n -1
data.decision$tax.n <- data.decision$tax.n -1
data.decision$energieabhaengigkeit.n <- data.decision$energieabhaengigkeit.n -1
data.decision$zeitpunkt.n <- data.decision$zeitpunkt.n -1

## proportion of yes choices for later modeling
proportion <- groupwiseSum(decision ~ ResponseId,
                           data =data.decision,
                           traditional = FALSE,
                           percentile = FALSE)

proportion.t <- groupwiseSum(decision ~ ResponseId + zeitpunkt,
                             data =data.decision,
                             traditional = FALSE,
                             percentile = FALSE)

## climate change concern : overall effect plus interaction between concern and implementation time factor
data.cc <- data.pass %>% select(ResponseId, starts_with("cc_c")) %>%
  mutate(climate_concern = rowMeans(cbind(cc_concerns_1,cc_concerns_2,cc_concerns_3,cc_concerns_4) ))
data.cc <- merge(data.cc, data.full, by=c("ResponseId"))
data.cc.2 <- data.cc %>% select(ResponseId, climate_concern)
data.cc.2$concern.scaled <-scale(data.cc.2$climate_concern, scale=FALSE)[,1]
describe(data.cc.2$concern.scaled)

table(round(data.cc.2$climate_concern,2))
# ignore because not helpful
#data.decision$concern.f <- factor(data.decision$climate_concern, levels= c(1, 1.5, 1.75, 2, 2.25,  2.5,2.75, 3, 3.25,3.5, 3.75 , 4, 4.25,  4.5, 4.75, 5, 5.25,5.5, 5.75, 6 ),labels=c( 1.762931,  1.762931,  1.762931,  1.762931,  1.762931,  1.762931,3.017442, 3.017442, 3.017442,3.630081, 3.630081,4, 4.25,  4.5, 4.75, 5, 5.25,5.5, 5.75, 6 ) )
# weighted means for those that grouped together
#(1*36 + 1.5*10 +1.75*14 + 2*11  + 2.25*22 + 2.5*23)/( 36 +  10 +  14   +11 +  22  + 23 )
#(32*2.75  + 56*3 +  41*3.25) /(32  + 56 +  41 )
#data.decision$concern.f2 <- as.numeric(as.character(data.decision$concern.f))
#data.decision$concern.f2  <- round(data.decision$concern.f2,2)
#data.decision$concern.scaled.t <- round(scale(data.decision$concern.f2)[,1],2)
#data.decision$concern.scaled.f  <- as.factor(data.decision$concern.scaled.t)

psych::alpha(data.cc[c("cc_concerns_1","cc_concerns_2","cc_concerns_3","cc_concerns_4")])

data.dec.c <- merge( data.decision, data.cc.2, by="ResponseId")

data.concern <- data.cc.2 %>% select(ResponseId, climate_concern, concern.scaled)
data.concern <- unique(data.concern)
#write_csv(data.dec.c, file = "data/data.decision.full") 

# für H1c

model.H1b.no.dem <- glmer(decision~ co2 + tax + energieabhaengigkeit + zeitpunkt + 
                            (1 + co2 + tax + energieabhaengigkeit + zeitpunkt | id),
                          data = data.decision,family = binomial(link = "logit"))

summary(model.H1b.no.dem)
tab_model(model.H1b.no.dem)
## beta weights
AgentsModelH1.cc <- coef(model.H1b.no.dem)$id
Model_H1b <- coef(model.H1b.no.dem)
ModelH1bAgents<- cbind(rownames(AgentsModelH1.cc),AgentsModelH1.cc)
colnames(ModelH1bAgents)[colnames(ModelH1bAgents) == "rownames(ModelH1bAgents)"] <- "id"
write_csv(ModelH1bAgents, file = "data/ModelH1bAgents.csv") #here you can find the betas

data.beta <- read.csv("data/ModelH1bAgents.csv")
data.beta <- data.beta %>%
  rename(id = "rownames.AgentsModelH1.cc." )

data.decision.id <- data.decision %>% 
  select(ResponseId, id, gender, age, country, income, education, politicalorientation_1, climate_concern, concern.scaled)
data.decision.id <- unique(data.decision.id)
data.model.co2.c <- merge(data.beta, data.decision.id, by=c("id"))
data.model.co2.c <- merge(data.model.co2.c, data.correlates.important, by=c("ResponseId"))
write_csv(data.model.co2.c, file = "data/data.model.co2.c.csv") #here you can find the betas



#### accuracy ####
data.accuracy <- data.pass %>% select(ResponseId,starts_with("accuracy"))
data.accuracy <- data.accuracy %>%
  mutate(Minutes.decision = minute(seconds_to_period(`accuracy.time_Page Submit`)) )
data.accuracy <- merge(data.accuracy, data.full, by="ResponseId")
data.accuracy <- data.accuracy[, -c(2:5)]

data.accuracy <- data.accuracy %>% 
  rename(dishwasher.est = accuracy_5,
         shower.est = accuracy_7,
         hairdryer.est = accuracy_8,
         washing.est = accuracy_9,
         dryer.est = accuracy_10,
         heating.est = accuracy_11,
         light.est = accuracy_12,
         fridge_eff.est = accuracy_13)

data.accuracy <- data.accuracy %>%
  mutate (dishwasher.act = 74) %>%
  mutate (shower.act = 1186) %>%
  mutate (dryer.act = 1036) %>%
  mutate (hairdryer.act = 91) %>%
  mutate (washing.act = 127) %>%
  mutate (heating.act = 1141) %>%
  mutate (light.act = 358) %>%
  mutate (fridge_eff.act = 52) 

data.accuracy <- na.omit(data.accuracy)
nrow(data.accuracy)
### patterns

# count the unique values, see if there is just 1 or 2, exclude those
data.accuracy.c <- data.accuracy %>% select(!starts_with("fridge")) %>% select(!starts_with("light"))

data.accuracy.c$same.strict <- apply(data.accuracy.c[,c(2:7)], 1, function(x) length(unique(x)) <= 1)
table(data.accuracy.c$same.strict )
data.accuracy.2 <- data.accuracy.c %>% filter(same.strict=="FALSE" )

# make long format out of estimate column
data.accuracy.long <- pivot_longer(data.accuracy.2, 2:7, names_to = "behavior", values_to = "estimate")
data.accuracy.long <- data.accuracy.long %>% filter(estimate > 0.8)


# prep to make estimate and actual values both in long format
data.accuracy.long.0 <- data.accuracy.long %>% select(ResponseId, behavior, estimate, gender, age, income, country, education, politicalorientation_1)
data.accuracy.long.0$behavior <- substring(data.accuracy.long.0$behavior, 1, str_length(data.accuracy.long.0$behavior)-4)
# same for actual values
data.accuracy.long.1 <- data.accuracy.long %>% select(ResponseId, ends_with(".act") )
data.accuracy.long.1 <- pivot_longer(data.accuracy.long.1, 2:7, names_to = "behavior", values_to = "actual")
data.accuracy.long.1$behavior <- substring(data.accuracy.long.1$behavior, 1, str_length(data.accuracy.long.1$behavior)-4)
data.accuracy.long.1 <- unique(data.accuracy.long.1)
# this merges both so this now has actual estimate matched per person per behavior
data.accuracy.long.total <- merge(data.accuracy.long.0, data.accuracy.long.1 , by=c("ResponseId", "behavior"), )

### transformations

## transformation for estimation bias -> first without log10 transformation to check how bad distribution then with log 10 transformation
data.accuracy.long.total$estimation.bias <- (data.accuracy.long.total$estimate/data.accuracy.long.total$actual)
data.accuracy.long.total$estimation.bias.log <- log10(data.accuracy.long.total$estimate/data.accuracy.long.total$actual)

# absolute error / estimation error
# a perfect estimate will result in an estimation error of zero
data.accuracy.long.total$absolute.error <- abs(data.accuracy.long.total$estimate/data.accuracy.long.total$actual)
data.accuracy.long.total$absolute.error.log <- abs(log10(data.accuracy.long.total$estimate/data.accuracy.long.total$actual) )

# absolute error
data.abs.error.mean <- groupwiseMean(absolute.error.log ~ ResponseId,
                                     data= data.accuracy.long.total,
                                     percentile= FALSE)
data.abs.error.mean <- data.abs.error.mean %>%
  rename(mean.abs.error = Mean) %>% select(!n)

# same for estimation bias
data.est.bias.mean <- groupwiseMean(estimation.bias.log ~ResponseId,
                                    data= data.accuracy.long.total,
                                    percentile= FALSE,
                                    traditional = FALSE)

data.est.bias.mean <- data.est.bias.mean %>%
  rename(mean.est.bias = Mean) %>% select(!n)

# combine both for later modeling 
data.accuracy.test <- merge(data.est.bias.mean, data.abs.error.mean, by=c("ResponseId"))
data.accuracy.test$binary <- factor(ifelse(data.accuracy.test$mean.est.bias < 0, "under", "over"))

# Wynes linear regression analyses with demographic data, ..
data.accuracy.long.total$actual.log <- log10(data.accuracy.long.total$actual)
data.accuracy.long.total$estimate.log <- log10(data.accuracy.long.total$estimate)

data.accuracy.long.total <- merge(data.accuracy.long.total, data.concern, by=c("ResponseId"))

#
write.csv(data.accuracy.long.total, "data/data.accuracy.wave1.csv")



# cronbachs alpha for this - wynes et al. do that but not completely sure that this is meant to be for the estimate per se?
psych::alpha(data.accuracy.2[c("dishwasher.est","shower.est","hairdryer.est","washing.est", 
                               "dryer.est","heating.est"  )])

# cronabchs alpha for error ?? seems like it would make more sens
data.accuracy.wide <- data.accuracy.long.total %>% select(ResponseId, estimate.log, actual.log, behavior)
data.accuracy.wide <-   pivot_wider(data.accuracy.wide, names_from ="behavior", values_from = c("estimate.log", "actual.log") )

data.accuracy.wide$error.dryer <- abs(log10(data.accuracy.wide$estimate.log_dryer/data.accuracy.wide$actual.log_dryer))
data.accuracy.wide$error.dishwasher <- abs(log10(data.accuracy.wide$estimate.log_dishwasher/data.accuracy.wide$actual.log_dishwasher))
data.accuracy.wide$error.hairdryer <- abs(log10(data.accuracy.wide$estimate.log_hairdryer/data.accuracy.wide$actual.log_hairdryer))
data.accuracy.wide$error.heating <- abs(log10(data.accuracy.wide$estimate.log_heating/data.accuracy.wide$actual.log_heating))
data.accuracy.wide$error.shower <- abs(log10(data.accuracy.wide$estimate.log_shower/data.accuracy.wide$actual.log_shower))
data.accuracy.wide$error.washing <- abs(log10(data.accuracy.wide$estimate.log_washing/data.accuracy.wide$actual.log_washing))

psych::alpha(data.accuracy.wide[c("error.washing","error.shower","error.heating","error.hairdryer", 
                               "error.dishwasher","error.dryer"  )])

# also for absolute error at least (?) acceptable cronbachs alpha, so either way would be an okay scale
ggplot(data.accuracy.long.total, aes(x=behavior, y=absolute.error.log)) +
  geom_boxplot() +
  theme_minimal()

data.accuracy.long.total$behavior.label <- factor(data.accuracy.long.total$behavior, 
                                              levels=c("dishwasher", "washing", "hairdryer",
                                                       "heating", "dryer", "shower"))
ggplot(data.accuracy.long.total,  aes(x=behavior.label, y=absolute.error.log)) +
  geom_boxplot() +
  theme_minimal()


ggplot(data.accuracy.long.total, aes(x=behavior, y=estimation.bias.log)) +
  geom_boxplot() +
  theme_minimal()

sum.bias <- groupwiseSum(estimation.bias.log ~ behavior + ResponseId,
                    data= data.accuracy.long.total,
                    traditional =FALSE)

ggplot(sum.bias, aes(x=reorder(behavior, -Sum), y=Sum)) +
  geom_boxplot() +
  theme_minimal()

# Margethis decompose error into cor and sd ratio #####

data.accuracy.long.total$cor <- c(rep(NA, nrow(data.accuracy.long.total) ))
accList <- split(data.accuracy.long.total,data.accuracy.long.total$ResponseId)
length(accList)
names(accList) <- paste0("id",1:length(accList))

# tranfosrmed wie in marghetis wäre 9=estimate und 10=actual für non transformed values sonst 15=actual log und 16=estimate log
for (i in c(1:(nrow(data.accuracy.long.total)/6) )) { 
  accList[[c(i,22)]] <- cor(accList[[c(i,15)]], accList[[c(i,16)]])
}

data.cor <- data.frame(matrix(NA,    # Create empty data frame
                              nrow = 1547,
                              ncol = 2))

for (i in c(1:(nrow(data.accuracy.long.total)/6) )) { 
  data.cor$cor[[i]] <- cor(accList[[c(i,15)]], accList[[c(i,16)]])
}
names(data.cor)
acc.id <- unique(data.accuracy.long.total$ResponseId)
data.cor$X1 <- acc.id
data.cor <- data.cor %>% select(!X2) %>%
  rename(ResponseId = X1)
data.cor$cor <- as.numeric(data.cor$cor)
describe(data.cor$cor)
data.cor$cor.r <- round(data.cor$cor,1)
table(data.cor$cor.r)
data.cor.excl <- data.cor %>% filter(cor>=0)

# same for sd estimate/actual
data.accuracy.long.total$sd <- c(rep(NA, nrow(data.accuracy.long.total) ))
accList.sd <- split(data.accuracy.long.total,data.accuracy.long.total$ResponseId)
data.acc.w <- unique(data.accuracy.long.total$actual.log)
sd(data.acc.w) # -> actual sd

for (i in c(1:(nrow(data.accuracy.long.total)/6) )) { 
  accList.sd[[c(i,23)]] <- (sd( accList.sd[[c(i,16)]]) /  sd(accList.sd[[c(i,15)]]) ) 
}

data.sd <- data.frame(matrix(NA,    # Create empty data frame
                             nrow = 1547,
                             ncol = 2))

for (i in c(1:(nrow(data.accuracy.long.total)/6) )) { 
  data.sd$sd[[i]] <-  (sd(accList.sd[[c(i,16)]]) / sd(accList.sd[[c(i,15)]]) ) 
}
names(data.sd)
data.sd$X1 <- acc.id
data.sd <- data.sd %>% select(!X2) %>%
  rename(ResponseId = X1)
data.sd$sd <- as.numeric(data.sd$sd)
describe(data.sd$sd)
data.sd.excl <- data.sd %>% filter(sd<=1)

data.acc.models <- merge(data.cor, data.sd, by="ResponseId")
data.acc.models <- data.acc.models %>% 
  mutate(cor= as.numeric(cor),
         sd = as.numeric(sd))

cor(data.acc.models$sd, data.acc.models$cor)

data.accuracy.test <- merge(data.accuracy.test, data.acc.models, by="ResponseId")

##### product choice ####

data.product <- data.pass %>% select(ResponseId, starts_with("product"))
data.product <- merge(data.product, data.full, by="ResponseId")
data.product <- merge(data.product, data.cc.2, by=c("ResponseId"))
data.product <- data.product %>% rename(politcalorientation = politicalorientation_1)
nrow(data.product)
data.product[2:29][data.product[2:29]==2] <- 0
data.product$Sum <- rowSums(data.product[2:29], na.rm=TRUE)

data.product.2 <- data.product %>% 
  rename( exp.low_che.int1 ="product_bmtw...70",
          exp.low_che.hig1= "product_twbv...71", 
          exp.low_mid.hig1 ="product_mvtw...72",
          exp.int_che.hig1 = "product_tmbv...73", 
          exp.int_mid.hig1 = "product_tmmv...74", 
          mid.low_che.int1 ="product_mwbm...75",
          mid.low_che.hig1  = "product_bvmw...76", 
          
          exp.low_che.int2 ="product_r_bmtw...77",
          exp.low_che.hig2 = "product_r_twbv...78",
          exp.low_mid.hig2 ="product_r_mvtw...79",
          exp.int_che.hig2 = "product_r_tmbv...80", 
          exp.int_mid.hig2 = "product_r_tmmv...81", 
          mid.low_che.int2 ="product_r_mwbm...82",
          mid.low_che.hig2  = "product_r_bvmw...83", 
          
          exp.low_che.int3 =  "product_r_bmtw...84",
          exp.low_mid.hig3 = "product_mvtw...93",
          exp.int_che.hig3 = "product_tmbv...94",
          exp.int_mid.hig3 =  "product_tmmv...95" ,
          mid.low_che.int3 = "product_mwbm...96",
          mid.low_che.hig3= "product_bvmw...97" ,
          exp.low_che.hig3 = "product_r_twbv...85",
          
          exp.low_mid.hig4 ="product_r_mvtw...86",
          exp.int_che.hig4 = "product_r_tmbv...87", 
          exp.int_mid.hig4 = "product_r_tmmv...88", 
          mid.low_che.int4 ="product_r_mwbm...89",
          mid.low_che.hig4  = "product_r_bvmw...90",
          exp.low_che.int4 =  "product_bmtw...91",
          exp.low_che.hig4 = "product_twbv...92",
          
  ) %>% select(!starts_with("product") )


data.product.2$exp.low_che.int <- rowSums(data.product.2[,c("exp.low_che.int1", "exp.low_che.int2", "exp.low_che.int3", "exp.low_che.int4" )], na.rm=TRUE)
data.product.2$exp.low_che.hig <- rowSums(data.product.2[,c("exp.low_che.hig1", "exp.low_che.hig2", "exp.low_che.hig3", "exp.low_che.hig4")], na.rm=TRUE)
data.product.2$exp.low_mid.hig <- rowSums(data.product.2[,c("exp.low_mid.hig1", "exp.low_mid.hig2", "exp.low_mid.hig3", "exp.low_mid.hig4")], na.rm=TRUE)
data.product.2$exp.int_che.hig <- rowSums(data.product.2[,c("exp.int_che.hig1", "exp.int_che.hig2", "exp.int_che.hig3", "exp.int_che.hig4")], na.rm=TRUE)
data.product.2$exp.int_mid.hig <- rowSums(data.product.2[,c("exp.int_mid.hig1", "exp.int_mid.hig2", "exp.int_mid.hig3", "exp.int_mid.hig4")], na.rm=TRUE)
data.product.2$mid.low_che.int <- rowSums(data.product.2[,c("mid.low_che.int1", "mid.low_che.int2", "mid.low_che.int3", "mid.low_che.int4")], na.rm=TRUE)
data.product.2$mid.low_che.hig <- rowSums(data.product.2[,c("mid.low_che.hig1", "mid.low_che.hig2", "mid.low_che.hig3", "mid.low_che.hig4")], na.rm=TRUE)

data.product.2 <- data.product.2 %>% select(!ends_with("1"))
data.product.2 <- data.product.2 %>% select(!ends_with("2"))
data.product.2 <- data.product.2 %>% select(!ends_with("3"))
data.product.2 <- data.product.2 %>% select(!ends_with("4"))


data.product.2$percent <- data.product.2$Sum/7

data.product.long <- pivot_longer(data.product.2, 14:20,names_to = "type", values_to = "choice" )

data.product.long$A <- substring(data.product.long$type, 1,7)
data.product.long$B <- substring(data.product.long$type, 9,)

data.product.long$priceA <- substring(data.product.long$A, 1,3)
data.product.long$priceB <- substring(data.product.long$B, 1,3)
data.product.long$energyA <- substring(data.product.long$A, 5,)
data.product.long$energyB <- substring(data.product.long$B, 5,)
data.product.long$trade.price <- factor(paste0(data.product.long$priceA,"-", data.product.long$priceB  ), 
                                        labels=c ("expensive-cheap", "expensive-mid", "mid-cheap") )
data.product.long$trade.energy <- factor(paste0(data.product.long$energyA,"-", data.product.long$energyB  ), 
                                         labels=c ("medium-high", "low-high", "low-medium") )

data.product.long.sum <- data.product.long %>% select(choice, trade.price, trade.energy) %>% 
  group_by(choice, trade.price, trade.energy) %>% tally()

ggplot(data.product.long.sum, aes(x=factor(choice), y=n, fill=trade.energy)) +
  geom_bar(stat="identity", position="dodge") + facet_grid(trade.price ~.) +
  scale_fill_viridis_d() +
  theme_minimal() + 
  labs(x="choice: 1=pro-environmental", y="amount", fill="trad-off energy") +
  theme(strip.text = element_text(size = 15),
        legend.text = element_text(size=14))

# neu 
data.product.long$price.level <-factor(data.product.long$trade.price, 
                                       levels=c("expensive-mid", "mid-cheap"  , "expensive-cheap"),
                                       labels=c( 1,1, 2 ))
data.product.long$energy.level <-factor(data.product.long$trade.energy, 
                                        levels=c("medium-high",  "low-medium" , "low-high" ),
                                        labels=c(1, 1, 2 ))


write.csv(data.product.long, "data/data.product.long.csv")
write.csv(data.product.2, "data/data.product.short.csv")
#### models between indiv and policy decisions and accuracy judgment ####

### predicting product choice with accuracy and policy support

data.product.long.2 <- data.product.long[,c(1:17,25,26)]
data.product.long.2 <- data.product.long.2 %>% select(!type)
data.product.long.2 <- data.product.long.2 %>% 
  rename(Sum.product = Sum)
data.accuracy.product <- merge(data.accuracy.test, data.product.long.2, by=c("ResponseId"))
proportion.d <- proportion %>% select(!n) %>%
  rename(Sum.policy = Sum)
data.accuracy.product <- merge(data.accuracy.product, proportion.d, by=c("ResponseId"))


data.accuracy.product.short <- data.accuracy.product[, c(1:22,28)]
data.accuracy.product.short <- unique(data.accuracy.product.short)

# model predciting sum of proenevironmental product decisions
model.product.acc.Sum <- lm(Sum.product ~ mean.abs.error  + gender + age + income + education + country + Sum.policy + concern.scaled,
                            data=data.accuracy.product.short)
summary(model.product.acc.Sum)
tab_model(model.product.acc.Sum)

# noch zeitpunkt
proportion.t1 <- proportion.t %>% filter(!zeitpunkt=="in 7 Jahren" )
proportion.t2 <- proportion.t %>% filter(zeitpunkt=="in 7 Jahren" )




## check same for policy decisions
data.accuracy.policy <- merge( data.dec.c, data.accuracy.test, by=c("ResponseId"))
product.Sum <- data.product.2 %>% select(ResponseId, Sum)
data.accuracy.policy <- merge(data.accuracy.policy, product.Sum, by=c("ResponseId"))
data.accuracy.policy <- data.accuracy.policy %>% rename(Sum.product = Sum)

### per timepoint getrennt
data.accuracy.policy.t1 <- data.accuracy.policy %>% filter(!zeitpunkt=="in 7 Jahren" )
data.accuracy.policy.t1 <- merge(data.accuracy.policy.t1, proportion.d, by=c("ResponseId"))


data.accuracy.policy.t2 <- data.accuracy.policy %>% filter(zeitpunkt=="in 7 Jahren" )
data.accuracy.policy.t2 <- merge(data.accuracy.policy.t2, proportion.d, by=c("ResponseId"))

# predicitng sum of accepted policies
data.accuracy.policy.sum <- merge(data.accuracy.policy, proportion.t, by=c("ResponseId", "zeitpunkt"))
data.accuracy.policy.sum <- data.accuracy.policy.sum %>% rename(Sum.time = Sum)

model.policy.acc.sum <- lmer(Sum.time ~    gender + age + income + country  + (concern.scaled + mean.abs.error +Sum.product) *zeitpunkt + (1|id),
                             data=data.accuracy.policy.sum )

summary(model.policy.acc.sum)



##### complexity task ######
# prepare so matrix is symmetrical
data.complexity.1 <- data_clean0 %>% select(ResponseId, m, ab:ef,`attention check`) %>% filter(`attention check`==4)
data.complexity.1 <- merge(data.full,data.complexity.1, by=c("ResponseId"))
data.complexity <- data.complexity.1 %>% select(ResponseId, ab:ef)

data.complexity$fa <- data.complexity$af
data.complexity$ea <- data.complexity$ae
data.complexity$da <- data.complexity$ad
data.complexity$ca <- data.complexity$ac
data.complexity$ba <- data.complexity$ab

data.complexity$fb <- data.complexity$bf
data.complexity$eb <- data.complexity$be
data.complexity$db <- data.complexity$bd
data.complexity$cb <- data.complexity$bc

data.complexity$fc <- data.complexity$cf
data.complexity$ec <- data.complexity$ce
data.complexity$dc <- data.complexity$cd

data.complexity$fd <- data.complexity$df
data.complexity$ed <- data.complexity$de

data.complexity$fe <- data.complexity$ef

data.complexity <- na.omit(data.complexity)
data.complexity.long <- pivot_longer(data.complexity, 2:31, names_to = "verhalten", values_to = "similarity")
data.complexity.long$verhalten2 <- data.complexity.long$verhalten
data.complexity.long$v2 <- substring(data.complexity.long$verhalten2, 2,)
data.complexity.long$v1 <- substring(data.complexity.long$verhalten, 1,1)

data.complexity.long$similarity <- as.numeric(data.complexity.long$similarity) 
data.complexity.long$similarity.2 <- (-(data.complexity.long$similarity) +10)

write.csv(data.complexity.long, "data.complexity.long.csv")

### ohne c weil ich war dumm
data.complexity.long.excl <- data.complexity.long %>% filter(!v2=="c") %>% filter(!v1=="c")



# only certain peoples

data.complexity.id <- data.accuracy.test %>% select(ResponseId)
data.complexity.long.excl <- merge(data.complexity.long.excl, data.complexity.id, by="ResponseId")
  
dfList <- split(data.complexity.long.excl,data.complexity.long.excl$ResponseId)
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
                  matrix1546	, matrix1547	
                  
))


#### scaling ####

# without start position
scaling <- smacof:::indscal(matrices, type="ordinal", itmax=75000)

plot.scaling <- as.data.frame(scaling$gspace)
plot.scaling$direction <- factor(c("pos","neg","neg",  "neg", "pos", "pos") )
plot.scaling$strength <- factor(c("high","low", "low", "high", "low", "high") )
plot.scaling$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco","Haare föhn vs luft", 
                                        "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))

ggplot(plot.scaling, aes(x=D1, y=D2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + xlim(-.8, .8) +
  ylim(-.7, .7) +
  theme_minimal() +
  scale_color_manual(values=c( "darkred", "darkgreen")) +
  geom_text(aes(label = label), vjust=3, size=6)

# für ohne c
plot.scaling.excl <- as.data.frame(scaling$gspace)
plot.scaling.excl$direction <- factor(c("pos","neg",  "neg", "pos", "pos") )
plot.scaling.excl$strength <- factor(c("high","low", "high", "low", "high") )
plot.scaling.excl$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco",
                               "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))

ggplot(plot.scaling.excl, aes(x=D1, y=D2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + xlim(-.8, .8) +
  ylim(-.7, .7) +
  theme_minimal() +
  scale_color_manual(values=c( "darkred", "darkgreen")) +
  geom_text(aes(label = label), vjust=3, size=6)

#
# euclidean distances of distances
matrices.2 <- matrices 
for (r in 1:length(matrices.2)) { 
  matrices.2[[r]] <-dist(matrices.2[[r]], method ="euclidean")
}
# euclidean distances of distances does not make much sense to me anyway and does not fundamentally  hange results (weights still not between 0 and 1 and still way 1400 vs 100 weighting d2 more)


###  individual difference scaling with starting position 
#init for starting values 
startconf.p <- matrix(data=NA, nrow=6, ncol=2)
startconf.p[,1] <- c(.1, -.1, -.1, -.1, .1, .1) *3
startconf.p[,2]  <- c(-.05, .05, .05, -.05, .05, -.05) *-3

scaling.start.p <- smacof:::indscal(matrices, init=startconf.p, type="ordinal", itmax=75000)
scaling.idioscl <- smacof:::idioscal(matrices, type="ordinal",init=startconf.p, itmax=75000)


plot.indscal.starting<- as.data.frame(scaling.start.p$gspace)
plot.indscal.starting$direction <- factor(c("pos","neg","neg",  "neg", "pos", "pos") )
plot.indscal.starting$strength <- factor(c("high","low", "low", "high", "low", "high") )
plot.indscal.starting$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco","Haare föhn vs luft", 
                                           "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))

ggplot(plot.indscal.starting, aes(x=D1, y=D2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + xlim(-.8, .8) +
  ylim(-.7, .7) +
  scale_color_manual(values=c( "darkred", "darkgreen")) +
  theme_minimal() +
  geom_text(aes(label = label), vjust=3, size=6)

### weights for dimensions
weights.scal.start <- as.matrix(scaling$cweights)

individual.weights <- matrix(data=1, nrow=1547, ncol=2)

for(r in 1:length(weights.scal.start )) { 
  individual.weights[r,2] <-  weights.scal.start[[c(r,4)]] ;
  individual.weights[r,1] <- weights.scal.start[[c(r,1)]] 
}

## V1 = D1, V2 = D2
individual.weights.2 <- as.data.frame(individual.weights)

individual.weights.2$ResponseId <- unique(data.complexity.id$ResponseId)
weights.total <- individual.weights.2
#weights.total$Sum <-  weights.total$V1 + weights.total$V2
#weights.total$Sum.sq <- weights.total$V1^2  + weights.total$V2^2 

#weights.total$Mean <- rowMeans(cbind(weights.total$V1, weights.total$V2))

weights.total$difference <- weights.total$V1-weights.total$V2
weights.total$difference <- factor(ifelse(weights.total$difference > 0, 1, 0))
table(weights.total$difference)
describe(weights.total$V2)  
# range von 0 bis 2.41 für dimension2, dimension1 0.4 1.28 

write.csv(weights.total, "data/weights.total.csv")


ggplot(weights.total, aes(x=V1, y=V2 )) +
  geom_point() + xlim(0,2.7) + ylim(0, 2.7)+
  theme_minimal()
  


individual.space <- scaling.idioscl$conf
individual.space.2 <- matrix(data=1, nrow=1547, ncol=12)

for(r in 1:length(individual.space)) { 
  individual.space.2[r,1] <-  individual.space [[c(r,1)]] ;
  individual.space.2[r,2] <- individual.space [[c(r,2)]];
  individual.space.2[r,3] <-  individual.space [[c(r,3)]] ;
  individual.space.2[r,4] <-  individual.space [[c(r,4)]] ;
  individual.space.2[r,5] <- individual.space [[c(r,5)]];
  individual.space.2[r,6] <-  individual.space [[c(r,6)]] ; 
  individual.space.2[r,7] <-  individual.space [[c(r,7)]] ;
  individual.space.2[r,8] <- individual.space [[c(r,8)]];
  individual.space.2[r,9] <-  individual.space [[c(r,9)]] ; 
  individual.space.2[r,10] <-  individual.space [[c(r,10)]] ;
  individual.space.2[r,11] <- individual.space [[c(r,11)]];
  individual.space.2[r,12] <-  individual.space [[c(r,12)]] ;
}

individual.space.2 <- as.data.frame(individual.space.2)
individual.space.long <- pivot_longer(individual.space.2, 1:6, names_to = "x", values_to = "x.value")
individual.space.long.2 <- pivot_longer(individual.space.long, 1:6, names_to = "y", values_to = "y.value")

individual.space.long.2$keep <- paste0(individual.space.long.2$x, individual.space.long.2$y)
individual.space.long.2 <- individual.space.long.2 %>% 
  filter(keep =="V1V7" | keep=="V2V8" | keep=="V3V9" |keep =="V4V10" | keep=="V5V11" | keep=="V6V12") 
individual.space.long.2$keep <- factor(individual.space.long.2$keep, levels=c("V1V7" ,"V2V8" ,"V3V9","V4V10","V5V11","V6V12") ,
                                       labels=c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco","Haare föhn vs luft", 
                                                "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner") )

ggplot(individual.space.long.2, aes(x=x.value, y=y.value, color=keep)) +
  geom_jitter() + labs(x="dimension 1", y="dimension 2", color=NULL) +
  theme_minimal() + scale_color_viridis_d()  +
  theme(strip.text = element_text(size = 15),
        legend.text = element_text(size=14))


  
### with product things

data.everything <- merge(data.accuracy.product, weights.total, by="ResponseId")
data.everything <- data.everything %>% 
  mutate(est.binary = factor(binary, levels=c("under", "over")) )
data.everything <- merge(data.everything, data.correlates.important,by="ResponseId")
write.csv(data.everything, "data/data.everything.csv")

#### other things of interest ####

# emotions and trust
names(data.pass)

data.correlates <- data.pass %>% 
  select(ResponseId, starts_with("emotions"), starts_with("trust"), ee_concerns) %>%
  mutate(emotions.re.pos = rowMeans(cbind(emotions_RE_hope,emotions_RE_interest),na.rm=TRUE),
         emotions.re.neg = rowMeans(cbind(emotions_RE_anger,emotions_RE_fear),na.rm=TRUE),
         emotions.crisis.pos = rowMeans(cbind(emotions_crisis_hope,emotions_crisis_interest),na.rm=TRUE),
         emotions.crisis.neg = rowMeans(cbind(emotions_crisis_anger,emotions_crisis_fear),na.rm=TRUE),
         trust.info.media = rowSums(cbind(trust_media_1,trust_media_ch_1), na.rm=TRUE),
         trust.info.gov = rowSums(cbind(trust_info_gov...101, trust_info_gov...102),na.rm=TRUE),
         trust.gov = rowMeans(cbind(trust_gov_ch_1,trust_gov_ch_2, trust_gov_ch_3, trust_gov_de_1, trust_gov_de_2, trust_gov_de_3), na.rm=TRUE)
  )
data.correlates.2 <- merge(data.correlates, data.full, by=c("ResponseId"))

data.correlates.2 <- merge(data.correlates.2, data.concern, by=c("ResponseId"))


#data.correlates.2$emotions.re.scaled <-scale(data.correlates.2$emotions.re, scale=FALSE)[,1]
data.correlates.2$emotions.re.neg.scaled <-scale(data.correlates.2$emotions.re.neg, scale=FALSE)[,1]
data.correlates.2$emotions.re.pos.scaled <-scale(data.correlates.2$emotions.re.pos, scale=FALSE)[,1]
data.correlates.2$emotions.crisis.neg.scaled <-scale(data.correlates.2$emotions.crisis.neg, scale=FALSE)[,1]
data.correlates.2$trust.gov.scaled <-scale(data.correlates.2$trust.gov, scale=FALSE)[,1]
data.correlates.2$emotions_crisis_hope.r <- -(data.correlates.2$emotions_crisis_hope)+7
data.correlates.2$emotions_crisis_interest.r <- -(data.correlates.2$emotions_crisis_interest)+7
data.correlates.2$emotions_RE_anger.r <- -(data.correlates.2$emotions_RE_anger)+7
data.correlates.2$emotions_RE_fear.r <- -(data.correlates.2$emotions_RE_fear)+7

data.correlates.2 <- data.correlates.2 %>%
  mutate(emotions.crisis.neg.tot =rowMeans(cbind(emotions_crisis_anger,emotions_crisis_fear, ee_concerns),na.rm=TRUE) )
data.correlates.2$emotions.crisis.neg.tot.sc <-scale(data.correlates.2$emotions.crisis.neg.tot, scale=FALSE)[,1]

psych::alpha(data.correlates.2[c("emotions_RE_anger","emotions_RE_fear")])
psych::alpha(data.correlates.2[c("emotions_RE_hope","emotions_RE_interest")])
psych::alpha(data.correlates.2[c("emotions_crisis_anger","emotions_crisis_fear")])
psych::alpha(data.correlates.2[c("emotions_crisis_hope","emotions_crisis_interest")])
psych::alpha(data.correlates.2[c("emotions_crisis_anger","emotions_crisis_fear","ee_concerns")])

data.emotions <- data.correlates.2 %>% select(!starts_with("trust")) %>% select(!starts_with("emotions."))

data.emotions.long <- pivot_longer(data.emotions, 2:10,names_to = "emotion", values_to = "value")
data.emotions.long$type.1 <- substr(data.emotions.long$emotion, 10, 15)
data.emotions.long$type <- factor(ifelse( data.emotions.long$type.1=="crisis", "crisis", "RE"))
data.emotions.long$type[data.emotions.long$type.1=="ns"] <- "crisis"
data.emotions.long.s <- data.emotions.long %>% select(emotion, value,type) %>%
  group_by(emotion, value,type) %>% tally()
data.emotions.long.s$emotion.label <- factor(data.emotions.long.s$emotion,
                                              levels=c("ee_concerns","emotions_crisis_anger", "emotions_crisis_fear", "emotions_crisis_hope", "emotions_crisis_interest",
                                                       "emotions_RE_anger","emotions_RE_fear","emotions_RE_hope", "emotions_RE_interest"),
                                                       labels=c("ee.concern","anger", "fear", "hope", "interest","anger", "fear", "hope", "interest"))
                                             
ggplot(data.emotions.long.s, aes(x=value, y=n, fill=emotion.label)) +
  geom_bar(stat="identity", position="dodge") +
  facet_grid(type ~.) +
  scale_fill_viridis_d()+
  theme_minimal()
  
data.correlates.important <- data.correlates.2 %>% select(ResponseId,emotions.re.pos, emotions.re.pos.scaled, emotions.re.neg.scaled, emotions.crisis.neg.tot,emotions.crisis.neg.tot.sc,
                                                          trust.gov, trust.gov.scaled)

data.accuracy.policy <- merge(data.accuracy.policy, data.correlates.important, by=c("ResponseId"))
data.accuracy.policy <- merge(data.accuracy.policy, weights.total, by=c("ResponseId"))

names(data.accuracy.policy)

write.csv(data.accuracy.policy, "data/data.decision.csv")


# tryign something

data.external <- merge(data.correlates.important, data.cc.2, by="ResponseId")
data.external <- merge(data.external, data.complexity.id,by="ResponseId")
data.external.c <- data.external$climate_concern

arr <- array( unlist(matrices) , c(6,6,1547) )
matrix.average <- apply( arr , 1:2 , mean )
res <- mds(matrix.average)


fit <- biplotmds(res, data.external.c, scale=TRUE)



# no fucking clue
cfit1 <- confEllipse(scaling.start.p)
plot(cfit1, ell = list(col = "gray", lty = 2), ylim = c(-0.04, 0.04))





#### other #### emotions
data.full$events.c <- as.character(data.full$events)
table(data.full$events.c)

data.events <- data.full %>%
  mutate (event_elec.blackout = 0) %>%
  mutate (event_gas.blackout = 0) %>%
  mutate (event_gas.higher.announce = 0) %>%
  mutate (event_elec.higher.announce = 0) %>%
  mutate (event_gas.less= 0) %>%
  mutate (event_elec.less = 0) %>%
  mutate (event_information.passiv = 0) %>%
  mutate (event_information.activ = 0) %>%
  mutate (event_other = 0) %>%
  mutate (event_none = 0) 
  
data.events$event_elec.blackout[grepl("1", data.events$events.c) == TRUE] <- 1
data.events$event_gas.blackout[grepl("2", data.events$events.c) == TRUE] <- 1
data.events$event_gas.higher.announce[grepl("3", data.events$events.c) == TRUE] <- 1
data.events$event_elec.higher.announce[grepl("4", data.events$events.c) == TRUE] <- 1
data.events$event_gas.less[grepl("5", data.events$events.c) == TRUE] <- 1
data.events$event_elec.less[grepl("6", data.events$events.c) == TRUE] <- 1
data.events$event_information.passiv[grepl("8", data.events$events.c) == TRUE] <- 1
data.events$event_information.activ[grepl("9", data.events$events.c) == TRUE] <- 1
data.events$event_other[grepl("10", data.events$events.c) == TRUE] <- 1
data.events$event_none[grepl("11", data.events$events.c) == TRUE] <- 1


data.events$event_elec.blackout2 <- ifelse(data.events$event_other==1 & data.events$event_elec.blackout==1 | 
                                             data.events$event_other==1 & data.events$event_elec.blackout==0 | 
                                             data.events$event_other==0 & data.events$event_elec.blackout==0 |
                                             data.events$event_none==0 & data.events$event_elec.blackout==0 |
                                             data.events$event_none==1 & data.events$event_elec.blackout==1 | 
                                             data.events$event_none==1 & data.events$event_elec.blackout==0 , 0, 1)


data.events <- data.events %>% select(!event_elec.blackout) %>% select(!events.c) %>% select(!events)

data.events.long <- pivot_longer(data.events, 10:19, names_to = "event.type", values_to = "experience")

data.events.long <- data.events.long %>% filter(experience==1)

data.events.long.s <- data.events.long %>% 
  select(event.type, country) %>% group_by(event.type, country) %>%tally()

ggplot(data.events.long.s, aes(x=event.type, y=n , fill=country)) +
  geom_bar(stat="identity") +
  theme_minimal()
# both country and income relative same amounts of experienced events



psych::alpha(data.events[c("event_elec.blackout2","event_gas.blackout", "event_elec.less","event_gas.less")])



data.events.d <- data.events %>% select(ResponseId,event_information.passiv, event_gas.higher.announce, event_elec.higher.announce, event_elec.blackout2 ,event_gas.blackout, event_elec.less,event_gas.less)


data.events.d$event_gas.higher.announce <- factor(data.events.dec$event_gas.higher.announce,
                                                    levels=c(0,1), labels=c("no","yes"))
data.events.d$event_information.passiv <- factor(data.events.dec$event_information.passiv,
                                                   levels=c(0,1), labels=c("no","yes"))



data.everything$politcalorientation.scaled <- scale(data.everything$politcalorientation, scale=FALSE)[,1]

model.H4.e <-  glmer(choice ~ price.level * energy.level   +
                                 (V1+ concern.scaled)* energy.level +
                                 country*price.level + country*energy.level + politcalorientation.scaled  +
                                  gender + age + income + education   +
                                 (1 + price.level + energy.level| ResponseId),
                               data=data.everything, family="binomial")

summary(model.H4.e)
tab_model(model.H4.e)
plot_model(model.H4.e, type="pred", terms=c("V1", "energy.level"))
plot_model(model.H4.e, type="pred", terms=c("concern.scaled", "energy.level"))

## V1 not related neither with interaction with energy difference nor without

## for policy
data.accuracy.policy$politcalorientation.scaled <- scale(data.accuracy.policy$politicalorientation_1, scale=FALSE)[,1]

model.policy.error.e <- glmer(decision ~  co2  + tax +energieabhaengigkeit +
                                gender + age + income  + country + education  +
                                trust.gov.scaled + politcalorientation.scaled  +
                                (V1 + concern.scaled)*zeitpunkt   + 
                                (1 + tax + co2 + energieabhaengigkeit + zeitpunkt|ResponseId),
                              family="binomial",
                              data=data.accuracy.policy)
tab_model(model.policy.error.e)
plot_model(model.policy.error.e, type="pred", terms=c("V1","zeitpunkt"))





anova(model.accuracy.freq.binary)
# age effect yes and income aswell
em <- emmeans(model.accuracy.freq.binary,specs="age")
pairs(em, adjust="none")
# for income: level 1 to 3, 4 and level 2 to 3, 4 -> so only first and 2 and third and fourth to each other not significant

# for age 1 to 4,5 ; 2 to 3,4,5 

#…when controlling for differences in x,y ,z // adjusting for differences , while keeping Ax y and z levels fixed


## for H3a
# frequencies: 1 = never or max once per week, 2= 2-3 times per week, 3 = 3 or more times per week
#frequency_1= waschmaschine, frequency_2= trockner, frequency_3=spülmaschine,frequency_4=FÖhn
device.freq <- data.pass %>% select(ResponseId, starts_with("frequency"))
device.freq.full <- merge(device.freq, data.full, by="ResponseId")
device.freq.full <- merge(device.freq.full, data.concern, by="ResponseId")

psych::alpha(device.freq.full[c("frequency_device_1","frequency_device_2","frequency_device_3","frequency_device_4")])
psych::alpha(device.freq.full[c("frequency_device_1","frequency_device_2","frequency_device_3")])
device.freq.full <- device.freq.full %>%
  mutate(mean.freq = rowMeans(cbind(frequency_device_1,frequency_device_2,frequency_device_3,frequency_device_4)),
         mean.freq.clean =rowMeans(cbind(frequency_device_1,frequency_device_2,frequency_device_3)) )

device.freq.long <- pivot_longer(device.freq.full, 2:4, names_to = "device", values_to = "frequency")

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

data.accuracy.frequency <- merge(data.accuracy.test, device.freq.full, by="ResponseId")
# binary
data.accuracy.frequency$freq.binary <- ifelse(data.accuracy.frequency$mean.freq.clean >=1.6, "high", "low")
data.accuracy.frequency$freq.binary <- factor(data.accuracy.frequency$freq.binary, levels=c("low", "high"))

write.csv(data.accuracy.frequency, "data/data.accuracy.frequency.csv")

data.accuracy.long.total2 <- merge(data.accuracy.long.total, data.correlates.important, by="ResponseId")
data.accuracy.frequency.s <- data.accuracy.frequency %>% select(ResponseId, mean.freq.clean, freq.binary)
data.accuracy.long.total3 <- merge(data.accuracy.long.total2, data.accuracy.frequency.s, by="ResponseId")
data.accuracy.long.total3 <- merge(data.accuracy.long.total3, weights.total, by="ResponseId")
data.accuracy.long.total3$education <- factor(data.accuracy.long.total3$education)
data.accuracy.long.total3$income <- factor(data.accuracy.long.total3$income)
data.accuracy.long.total3$ag <- factor(data.accuracy.long.total3$age)
data.accuracy.long.total3$gender <- factor(data.accuracy.long.total3$gender)
data.accuracy.long.total3$country <- factor(data.accuracy.long.total3$countr)
data.accuracy.test2 <- data.accuracy.test %>% select(ResponseId, mean.est.bias, mean.abs.error)
data.accuracy.long.total4 <- merge(data.accuracy.long.total3, data.accuracy.test2,by="ResponseId")

write.csv(data.accuracy.long.total4, "data/data.accuracy.long.total.csv")

### models for ACCURACY , so estimation bias and error
data.accuracy.model1 <- merge(data.accuracy.test, data.concern, by =c("ResponseId"))
data.accuracy.model2 <- merge(data.accuracy.model1, data.full, by=c("ResponseId"))
data.accuracy.model3 <- merge(data.accuracy.model2, data.correlates.important, by=c("ResponseId"))
data.accuracy.model4 <- merge(data.accuracy.model3, weights.total, by=c("ResponseId"))
data.accuracy.model <- merge(data.accuracy.model4, data.accuracy.frequency.s, by=c("ResponseId"))
data.accuracy.model$education.f <- factor(data.accuracy.model$education,
                                          levels=c("no formal education", "obligatory school", "middle school", "degree"))
write.csv(data.accuracy.model, "data/data.accuracy.model.csv")



