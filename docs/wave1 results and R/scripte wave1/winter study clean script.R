#### winter study

#### prep data ###### 
library("lubridate")
library(jsonlite)
library(psych)
library(tidyverse)
library(rcompanion)
library(broom)
library(vegan)
library(sjPlot) # for tab_model
library(lmerTest); library(lme4)

data_raw0 <- read_csv(file.choose() )

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
  mutate(Minutes.total = minute(seconds_to_period(`Duration (in seconds)`)),
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
data.full <- data.pass %>% select(ResponseId, Minutes.total, gender, age, country, income,education, politicalorientation_1, impact, events)

# only gender which identifiy as male or female
data.full <- data.full %>% select(ResponseId,Minutes.total,gender, age, country, income, education, politicalorientation_1, impact, events) %>%
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
labjs_column1 <- 'labjs-decision'

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

### graphical overview ###
##   amount of yes decisions
prop.table(table(data.decision$decision))

# general acceptance first
proportion <- groupwiseSum(decision ~ ResponseId,
                           data =data.decision,
                           traditional = FALSE,
                           percentile = FALSE)

people <- proportion %>% 
  group_by(Sum) %>% tally()

yes.general <- ggplot(people, aes(x=Sum, y=n)) +
  geom_bar(stat="identity", fill="#26867c") + 
  labs(x="amount of yes decisions", y="number of people", subtitle="total") +
  theme_minimal()

proportion.t <- groupwiseSum(decision ~ ResponseId + zeitpunkt,
                             data =data.decision,
                             traditional = FALSE,
                             percentile = FALSE)

proportion.t.people <- proportion.t %>% select(ResponseId,zeitpunkt,Sum) %>%
  group_by(zeitpunkt,Sum) %>% tally()

proportion.t.people$zeitpunkt <- factor(proportion.t.people$zeitpunkt)

yes.pertime <- ggplot(proportion.t.people, aes(x=Sum, y=n, fill=zeitpunkt)) +
  geom_bar(stat="identity", position="stack" ) +
  scale_fill_manual(values=c("darkgreen", "darkblue")) +
  labs(x="amount of yes decisions", y="number of people", subtitle="divided by timepoint") +
  theme_minimal()

yes.general | yes.pertime


###### models  H1 and H1b######
nrow(data.decision)/16
data.decision$id <- c(rep(1:(nrow(data.decision)/16), each=16))


#### Model of policy decisions  with all demographics and poliitcal orientation

# with numeric not as factors
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

## H1b 
## climate change concern : overall effect plus interaction between concern and implementation time factor
data.cc <- data.pass %>% select(ResponseId, starts_with("cc_c")) %>%
  mutate(climate_concern = rowMeans(cbind(cc_concerns_1,cc_concerns_2,cc_concerns_3,cc_concerns_4) ))
data.cc <- merge(data.cc, data.full, by=c("ResponseId"))
data.cc.2 <- data.cc %>% select(ResponseId, climate_concern)
data.cc.2$concern.scaled <-scale(data.cc.2$climate_concern, scale=FALSE)[,1]
describe(data.cc.2$concern.scaled)

psych::alpha(data.cc[c("cc_concerns_1","cc_concerns_2","cc_concerns_3","cc_concerns_4")])

data.dec.c <- merge( data.decision, data.cc.2, by="ResponseId")
write.csv(data.dec.c, "data.dce.wave1Dec2022.csv")


class(data.dec.c$zeitpunkt)
model.H1b <- glmer(decision~ co2 + tax + energieabhaengigkeit + (zeitpunkt * concern.scaled) + gender + age + income + country  + (1| id) ,
                   data = data.dec.c,family = binomial(link = "logit"))

summary(model.H1b)
tab_model(model.H1b)

### per person ??

model.H1b.2 <- glmer(decision~ co2 + tax + energieabhaengigkeit + (zeitpunkt * concern.scaled) + gender + age + income + country  + (1| id) +
                       (1 + co2 + tax + energieabhaengigkeit + (zeitpunkt * concern.scaled) | id),
                     data = data.dec.c,family = binomial(link = "logit"))
summary(model.H1b.2)

#Predict at person level
# not clean at all, but nothing else so far has worked
# these are predictions -> but it computes it for every choice so 16 per person
predframe <- predict(model.H1b.2,type="response",re.form=NA)
data.dec.c$prediction <- predframe
# then i took the average prediction per person and timepoint so its 8 per time and person
predictions.model <- groupwiseMean(prediction ~ ResponseId + zeitpunkt,
                                   data= data.dec.c,
                                   traditional= FALSE,
                                   percentile =FALSE
                                   )
predictions.model <- predictions.model %>% select(!n)

predictions.model <- merge(predictions.model, data.cc.2, by="ResponseId")


# and used that and their cc concern score to plot the interaction for the prediciton of accepting a policy
ggplot(predictions.model,aes(x=concern.scaled, y=Mean, color=zeitpunkt))+
  geom_jitter(alpha=0.5) + 
  ylim(0,1) + theme_minimal()


#library(visreg);
#visreg(model.H1b.2, "concern.scaled",by="zeitpunkt", type="conditional" ,scale="response", rug=2, xlab="cc concern", jitter=TRUE, ylab="P(policy accepted)")

## plot probabilities
library(interplot)

data.dec.c$concern.scaled.2 <- factor(data.dec.c$concern.scaled)

e <- effects::effect("zeitpunkt*concern.scaled", model.H1b, 
                     xlevels=list(concern.scaled=seq(from=-3.75, to=1.25, length.out=20)) )
e <- as.data.frame(e)
table(data.dec.c$concern.scaled)

ggplot(e, aes(concern.scaled, fit, color=zeitpunkt, group = zeitpunkt)) + 
  theme_minimal() + geom_line() + geom_point() + 
#  geom_errorbar(aes(ymin=lower, ymax=upper)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.1)



plot_model( model.H1b, type="pred", terms=c( "concern.scaled[all]", "zeitpunkt") )


ggplot(data.cc.2, aes(x=concern.scaled)) +
  geom_histogram(bins=50)

###### plotting H1b and interaction #######
summary(model.H1b)
b0 <- summary(model.H1b)[[c(10,1)]]
co2 <- summary(model.H1b)[[c(10,2)]]
tax<- summary(model.H1b)[[c(10,3)]]
energyindependence <- summary(model.H1b)[[c(10,4)]]
time <- summary(model.H1b)[[c(10,5)]]
climate_concern<- summary(model.H1b)[[c(10,6)]]
gender <- summary(model.H1b)[[c(10,7)]]
age30 <- summary(model.H1b)[[c(10,8)]]
age40 <- summary(model.H1b)[[c(10,9)]]
age50 <- summary(model.H1b)[[c(10,10)]]
age60 <- summary(model.H1b)[[c(10,11)]]
income2 <- summary(model.H1b)[[c(10,12)]]
income3 <- summary(model.H1b)[[c(10,13)]]
income4 <- summary(model.H1b)[[c(10,14)]]
country <- summary(model.H1b)[[c(10,15)]]
interaction.time.cc <- summary(model.H1b)[[c(10,16)]]

X1_range <- seq(from=min(data.dec.c$concern.scaled), to=max(data.dec.c$concern.scaled), length.out=20)

t1_logits <- b0 + 
  climate_concern*X1_range + 
  co2*0 + 
  tax*0 +
  energyindependence*0 +
  time*0 +
  gender*0 +
  age30*0 +
  age40*0 +
  age50*0 +
  age60*0 +
  income2*0 +
  income3*0 +
  income4*0 +
  country*0 +
  interaction.time.cc*X1_range*0    # the reference group

t2_logits <- b0 +
  climate_concern*X1_range + 
  co2*0 + 
  tax*0 +
  energyindependence*0 +
  time*1 +
  gender*0 +
  age30*0 +
  age40*0 +
  age50*0 +
  age60*0 +
  income2*0 +
  income3*0 +
  income4*0 +
  country*0 +
  interaction.time.cc*X1_range*1  

t2.co2.2_logits <- b0 +
  climate_concern*X1_range + 
  co2*1 + 
  tax*0 +
  energyindependence*0 +
  time*1 +
  gender*0 +
  age30*0 +
  age40*0 +
  age50*0 +
  age60*0 +
  income2*0 +
  income3*0 +
  income4*0 +
  country*0 +
  interaction.time.cc*X1_range*1  

t1.co2.2_logits<- b0 +
  climate_concern*X1_range + 
  co2*1 + 
  tax*0 +
  energyindependence*0 +
  time*0 +
  gender*0 +
  age30*0 +
  age40*0 +
  age50*0 +
  age60*0 +
  income2*0 +
  income3*0 +
  income4*0 +
  country*0 +
  interaction.time.cc*X1_range*0  

t1_probs <- exp(t1_logits)/(1 + exp(t1_logits))
t2_probs <- exp(t2_logits)/(1 + exp(t2_logits))
t1.co2.2_probs <- exp(t1.co2.2_logits)/(1 + exp(t1.co2.2_logits))
t2.co2.2_probs <-  exp(t2.co2.2_logits)/(1 + exp(t2.co2.2_logits))

# We'll start by plotting the ref group:
plot(X1_range, t1_probs, 
     ylim=c(0,1),
     type="l", 
     lwd=3, 
     lty=2, 
     col="gold", 
     xlab="climate change concern", ylab="P(outcome)", main="Probability of policy being accepted")

lines(X1_range, t2_probs, 
      type="l", 
      lwd=3, 
      lty=3, 
      col="turquoise2")


#### just for fun to also show co2
lines(X1_range, t2.co2.2_probs, 
      type="l", 
      lwd=3, 
      lty=4, 
      col="darkblue")

lines(X1_range, t1.co2.2_probs, 
      type="l", 
      lwd=3, 
      lty=4, 
      col="darkorange")


###### model H1c #####

# Maria's code

model.H1b.no.dem <- glmer(decision~ co2 + tax + energieabhaengigkeit + zeitpunkt + (1 + co2 + tax + energieabhaengigkeit + zeitpunkt | id),
                          data = data.decision,family = binomial(link = "logit"))

summary(model.H1b)
tab_model(model.H1b)
### equivalent for me
AgentsModelH1.cc <- coef(model.H1b)$id
Model_H1b <- coef(model.H1b)
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


write.csv(data.dec.c, "data.dce.winter_study.csv")

##### complexity task ######
# prepare so matrix is symmetrical
data.complexity.1 <- data_clean0 %>% select(ResponseId, ab:ef,`attention check`) %>% filter(`attention check`==4)
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
                  matrix1621	, matrix1622	, matrix1623	, matrix1624	, matrix1625 
                  
))

#### scaling ####

### individual difference scaling without starting position 
scaling <- smacof:::indscal(matrices, type="ordinal",  itmax=75000)
summary(scaling)
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
scaling.start.p <- smacof:::indscal(matrices, init=startconf.p, type="ordinal", itmax=15000)
summary(scaling.start.p)

plot.indscal.starting <- as.data.frame(scaling.start.p$gspace)
plot.indscal.starting$direction <- factor(c("pos","neg","neg",  "neg", "pos", "pos") )
plot.indscal.starting$strength <- factor(c("high","low", "low", "high", "low", "high") )
plot.indscal.starting$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco","Haare föhn vs luft", 
                                        "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))

indscal.starting <- ggplot(plot.indscal.starting, aes(x=D1, y=D2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + xlim(-.8, .8) +
  ylim(-.7, .7) +
  theme_minimal() +
  geom_text(aes(label = label), vjust=3, size=4)


### euclidian

matrices.2 <- matrices
for (r in 1:length(dfList)){ 
  matrices.2[[r]] <- dist(matrices.2[[r]], method ="euclidean")
}

scaling.euc <- smacof:::indscal(matrices.2, init=startconf.p, type="ordinal",  itmax=75000)
scaling.euc$stress
plot.indscal.euc <- as.data.frame(scaling.euc$gspace)
plot.indscal.euc$direction <- factor(c("pos","neg","neg",  "neg", "pos", "pos") )
plot.indscal.euc$strength <- factor(c("high","low", "low", "high", "low", "high") )
plot.indscal.euc$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco","Haare föhn vs luft", 
                                        "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))

indscal.euc <- ggplot(plot.indscal.euc, aes(x=D1, y=D2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + xlim(-.8, .8) +
  ylim(-.7, .7) +
  theme_minimal() +
  geom_text(aes(label = label), vjust=3, size=4)

### other methods
##### MDS ####

#  Make a 3D array from list of matrices

###### HOWEVER MANY MATRICES YOU HAVE NEEDS TO BE PUT IN THE LAST PLACE SO WHERE NOW IT SAYS 1569 """
arr <- array( unlist(matrices) , c(6,6,1628) )

#  Get mean of third dimension
matrix.average <- apply( arr , 1:2 , mean )

## mds in smacof package
mds.model <- smacof:::mds(matrix.average, ndim=2 ,type="ordinal", itmax=7500)
summary(mds.model)
mds.model$stress
### stress is higher for 1 dimension ........ 0.1319797

mds.ordinal.smacof <- mds.model$conf
mds.ordinal.smacof <- as.data.frame(mds.ordinal.smacof)
mds.ordinal.smacof$direction <- factor(c("pos","neg","neg",  "neg", "pos", "pos") )
mds.ordinal.smacof$strength <- factor(c("high","low", "low", "high", "low", "high") )
mds.ordinal.smacof$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco","Haare föhn vs luft", 
                                     "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))


plot.mds.ordinal.smacof <- ggplot(mds.ordinal.smacof, aes(x=D1, y=D2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + 
  theme_minimal() + labs(x=NULL, y=NULL, subtitle = "ordinal mds smacof package") +
  theme(legend.position = "none") +
  geom_text(aes(label = label), vjust=3, size=4)

### with Isoplot package -> that not available in this version of R


# with vegan package ??
mds.model.3 <- vegan:::metaMDS(matrix.average, k=2, autotransform = FALSE, noshare=FALSE, try=10, maxit=7500)
nmds <- mds.model.3$points
nmds <- as.data.frame(nmds)
nmds$direction <- factor(c("pos","neg","neg",  "neg", "pos", "pos") )
nmds$strength <- factor(c("high","low", "low", "high", "low", "high") )
nmds$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco","Haare föhn vs luft", 
                       "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))

plot.nmds <- ggplot(nmds, aes(x=MDS1, y=MDS2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + labs(x=NULL, y=NULL, subtitle = "nonmetric mds vegan package") +
  theme_minimal() + theme(legend.position = "none") +
  geom_text(aes(label = label), vjust=3, size=4)

#with cmdscale
mds.model.4 <-cmdscale(matrix.average, k=2)
mds.model.4

mds.metric <- mds.model.4
mds.metric <- as.data.frame(mds.metric)
mds.metric$direction <- factor(c("pos","neg","neg",  "neg", "pos", "pos") )
mds.metric$strength <- factor(c("high","low", "low", "high", "low", "high") )
mds.metric$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco","Haare föhn vs luft", 
                             "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))

plot.mds.metric <- ggplot(mds.metric, aes(x=V1, y=V2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) +
  theme_minimal() + theme(legend.position = "none") +
  labs(x=NULL, y=NULL, subtitle = "metric mds cmdscale") +
  geom_text(aes(label = label), vjust=3, size=4)


## MASS package
mds.model.2 <- MASS:::isoMDS(matrix.average, k=2, maxit=7500, trace=FALSE)

mds.MASS <- mds.model.2$points
mds.MASS <- as.data.frame(mds.MASS)
mds.MASS$direction <- factor(c("pos","neg","neg",  "neg", "pos", "pos") )
mds.MASS$strength <- factor(c("high","low", "low", "high", "low", "high") )
mds.MASS$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco","Haare föhn vs luft", 
                           "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))

plot.mds.MASS <- ggplot(mds.MASS, aes(x=V1, y=V2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + labs(x=NULL, y=NULL, subtitle = "nonmetric mds MASS package") +
  theme_minimal() + theme(legend.position = "none") +
  geom_text(aes(label = label), vjust=3, size=4)

(plot.mds.metric / plot.mds.MASS ) | ( plot.mds.ordinal.smacof / plot.nmds)


detach("package:vegan", unload=TRUE); detach("package:MASS", unload=TRUE)



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
data.accuracy$same.strict <- apply(data.accuracy[,c(2:9)], 1, function(x) length(unique(x)) <= 2)

data.accuracy.2 <- data.accuracy %>% filter(same.strict=="FALSE" )
## because hairdryer.est is lowest and  heating.est is highest before exlucding so difference between these
data.accuracy.2$sensible <- data.accuracy.2$heating.est - data.accuracy.2$hairdryer.est
data.accuracy.3 <- data.accuracy.2 %>% filter(sensible >0)

# make long format out of estimate column
data.accuracy.long <- pivot_longer(data.accuracy.2, 2:9, names_to = "behavior", values_to = "estimate")

# prep to make estimate and actual values both in long format
data.accuracy.long.0 <- data.accuracy.long %>% select(ResponseId, behavior, estimate, gender, age, income, country)
data.accuracy.long.0$behavior <- substring(data.accuracy.long.0$behavior, 1, str_length(data.accuracy.long.0$behavior)-4)
# same for actual values
data.accuracy.long.1 <- data.accuracy.long %>% select(ResponseId, ends_with(".act") )
data.accuracy.long.1 <- pivot_longer(data.accuracy.long.1, 2:9, names_to = "behavior", values_to = "actual")
data.accuracy.long.1$behavior <- substring(data.accuracy.long.1$behavior, 1, str_length(data.accuracy.long.1$behavior)-4)
data.accuracy.long.1 <- unique(data.accuracy.long.1)
# this merges both so this now has actual estimate matched per person per behavior
data.accuracy.long.total <- merge(data.accuracy.long.0, data.accuracy.long.1 , by=c("ResponseId", "behavior"), )

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

# so that can visualize difference between actual and estimate in boxplot
data.accuracy.long.total.2 <- pivot_longer(data.accuracy.long.total,c("estimate", "actual"), names_to = "type", values_to = "value")

ggplot(data.accuracy.long.total.2, aes(x=behavior, y=value, color=type )) + 
  coord_flip() +
  geom_boxplot() + 
  scale_color_manual(values=c("darkgreen", "darkblue")) +
  theme_minimal()

### transformations
# this is from Marghetis? to do log10 minus the mean of the log transformed actual values
data.accuracy.long.total$estimate.transformed <- (2.418587 - log10( data.accuracy.long.total$estimate))
data.accuracy.long.total$actual.transformed <-   (2.418587 - log10( data.accuracy.long.total$actual) )
cor.test(data.accuracy.long.total$estimate.transformed, data.accuracy.long.total$actual.transformed)

ggplot(data.accuracy.long.total, aes(x=estimate.transformed)) +
  geom_histogram()
ggplot(data.accuracy.long.total, aes(x=actual.transformed)) +
  geom_histogram(bins=50)

model.acc <- lm(estimate.transformed ~ actual.transformed ,data=data.accuracy.long.total)
summary(model.acc)

## alternative is to do it just with the log values, like Wynes and attari

### esimtation bias and estimation error
#an estimation ratio of 10 (ten times higher than actual) becomes log10(10) = 1 and a ratio of 0.1 (one-tenth of actual) becomes log10(0.1) = -1. 

## transformation for estimation bias -> first without log10 transformation to check how bad distribution then with log 10 transformation
data.accuracy.long.total$estimation.bias <- (data.accuracy.long.total$estimate/data.accuracy.long.total$actual)
data.accuracy.long.total$estimation.bias.log <- log10(data.accuracy.long.total$estimate/data.accuracy.long.total$actual)

ggplot(data.accuracy.long.total, aes(x=estimation.bias) ) +
  geom_histogram()
ggplot(data.accuracy.long.total, aes(x=estimation.bias.log) ) +
  geom_histogram()
describe(data.accuracy.long.total$estimation.bias.log)

# log seems relative good

# absolute error / estimation error
# a perfect estimate will result in an estimation error of zero
data.accuracy.long.total$absolute.error <- abs(data.accuracy.long.total$estimate/data.accuracy.long.total$actual)
data.accuracy.long.total$absolute.error.log <- abs(log10(data.accuracy.long.total$estimate/data.accuracy.long.total$actual) )

ggplot(data.accuracy.long.total, aes(x=absolute.error) ) +
  geom_histogram()
ggplot(data.accuracy.long.total, aes(x=absolute.error.log) ) +
  geom_histogram()
describe(data.accuracy.long.total$absolute.error.log)
# skew is a bit high but definitely better than the not transformed error

# means of the transformed estimation bias and absolute error per Person

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

ggplot(data.accuracy.test, aes(x=mean.est.bias) ) +
  geom_histogram(bins=100)
ggplot(data.accuracy.test, aes(x=mean.abs.error) ) +
  geom_histogram(bins=100)
# "Mean biases and their associated confidence intervals are computed by converting back to original units."
# "For example, a mean bias of log10(0.1) = -1 is reported 10hoch−1 * 100% = 10% of the actual value." ^^

## means per behavior to visulaize under / over estimation
# this in orginal unit (because distribution does not really matter for the plot)
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

# estimation accuracy with error bars 
ggplot(accuracy.means, aes(x=mean.actual, y=mean.estimate, color=behavior)) +
  geom_point(size=2.8) +  ylim(0,1800) + xlim(0,1800) +
  geom_errorbar(aes(ymin=Trad.lower, ymax=Trad.upper)) +
  theme_minimal() + labs(x="actual value", y="mean estimate", color=NULL) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  scale_color_viridis_d() +
  theme(legend.text = element_text(size=11))

accuracy.means$error.bar <- accuracy.means$Trad.upper - accuracy.means$Trad.lower
# they do not seem to be really different in the graph but the confidence intervals do differ in size

# divided per country just out of curiosity

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

# Wynes linear regression analyses with demographic data, ..
data.accuracy.long.total$actual.log <- log10(data.accuracy.long.total$actual)
data.accuracy.long.total$estimate.log <- log10(data.accuracy.long.total$estimate)

model.accuracy <-lmer(estimate.log ~ actual.log + (1|ResponseId)  ,data=data.accuracy.long.total)
summary(model.accuracy)
model.accuracy.d <-lmer(estimate.log ~ actual.log + gender + age + income + country + (1|ResponseId)  ,data=data.accuracy.long.total)
summary(model.accuracy.d)
# country and gender significant 


#### Marghetis et al
# log10(estimated)~b0 + bactual log10(actual)
model.acc <- lmer(estimate.transformed ~ actual.transformed + (1|ResponseId)  ,data=data.accuracy.long.total)
summary(model.acc)

# b actual = cor between estimate and true value * ratio between sd of estimate / sd of actual
# cor = correctness of individuals' underlying,understanding of appliances' relative energy use, cor of 1 = perfect understanding
cor(data.accuracy.long.total$estimate.transformed, data.accuracy.long.total$actual.transformed)
## = 0.093
# sd ratio to measure their use of the response scale; systematic overestimation of small values and underestimation of large values would 
#produce a ratio less than 1 with ratios closer to 0 indicating a more compressed use of the response scale. ??

sd(data.accuracy.long.total$estimate.transformed) / sd(data.accuracy.long.total$actual.transformed)

## apparently gotta do this per person though
data.accuracy.long.total$cor <- c(rep(NA, nrow(data.accuracy.long.total) ))
accList <- split(data.accuracy.long.total,data.accuracy.long.total$ResponseId)
length(accList)
names(accList) <- paste0("id",1:length(accList))

for (i in c(1:(nrow(data.accuracy.long.total)/8) )) { 
  accList[[c(i,17)]] <- cor(accList[[c(i,9)]], accList[[c(i,10)]])
}

data.cor <- data.frame(matrix(NA,    # Create empty data frame
                          nrow = (nrow(data.accuracy.long.total)/8),
                          ncol = 2))

for (i in c(1:(nrow(data.accuracy.long.total)/8) )) { 
  data.cor$cor[[i]] <- cor(accList[[c(i,9)]], accList[[c(i,10)]])
}
names(data.cor)
acc.id <- unique(data.accuracy.long.total$ResponseId)
data.cor$X1 <- acc.id
data.cor <- data.cor %>% select(!X2) %>%
  rename(ResponseId = X1)


# same for sd
data.accuracy.long.total$sd <- c(rep(NA, nrow(data.accuracy.long.total) ))
accList.sd <- split(data.accuracy.long.total,data.accuracy.long.total$ResponseId)
data.acc.w <- unique(data.accuracy.long.total$actual.log)
sd(data.acc.w) # -> actual sd

for (i in c(1:(nrow(data.accuracy.long.total)/8) )) { 
  accList.sd[[c(i,17)]] <- (sd(accList.sd[[c(i,16)]]) / 0.5754181 ) 
}

data.sd <- data.frame(matrix(NA,    # Create empty data frame
                              nrow = (nrow(data.accuracy.long.total)/8),
                              ncol = 2))

for (i in c(1:(nrow(data.accuracy.long.total)/8) )) { 
  data.sd$sd[[i]] <-  (sd(accList.sd[[c(i,16)]]) / 0.5754181 ) 
}
names(data.sd)
data.sd$X1 <- acc.id
data.sd <- data.sd %>% select(!X2) %>%
  rename(ResponseId = X1)

data.acc.models <- merge(data.cor, data.sd, by="ResponseId")
data.acc.models <- data.acc.models %>% 
  mutate(cor= as.numeric(cor),
         sd = as.numeric(sd))
data.acc.models <- merge(data.acc.models, id, by="ResponseId")
data.acc.models <- merge(data.acc.models, data.cc.2, by="ResponseId")


# b actual = cor between estimate and true value * ratio between sd of estimate / sd of actual
# cor = correctness of individuals' underlying,understanding of appliances' relative energy use, cor of 1 = perfect understanding
describe(data.acc.models$cor)
## mean is 0.17 

# sd ratio to measure their use of the response scale; systematic overestimation of small values and underestimation of large values would 
#produce a ratio less than 1 with ratios closer to 0 indicating a more compressed use of the response scale. ??
describe(data.acc.models$sd)



