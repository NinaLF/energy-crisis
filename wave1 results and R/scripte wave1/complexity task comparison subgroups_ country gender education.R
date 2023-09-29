
#### complexity subgroups #####

#### prep data 
library("lubridate")
library(jsonlite)
library(psych)
library(tidyverse)
library(rcompanion)
library(broom)
library(smacof)
library(vegan)
library(sjPlot) # for tab_model


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

data.full <- na.omit(data.full)
nrow(data.full)

# climate concern
data.cc <- data.pass %>% select(ResponseId, starts_with("cc_c")) %>%
  mutate(climate_concern = rowMeans(cbind(cc_concerns_1,cc_concerns_2,cc_concerns_3,cc_concerns_4) ))
data.cc <- merge(data.cc, data.full, by=c("ResponseId"))
data.cc.2 <- data.cc %>% select(ResponseId, climate_concern)
data.cc.2$concern.scaled <-scale(data.cc.2$climate_concern, scale=FALSE)[,1]

####### complexity
data.complexity.1 <- data_clean0 %>% select(ResponseId, ab:ef,`attention check`) %>% filter(`attention check`==4)
data.complexity.1 <- merge(data.full,data.complexity.1, by=c("ResponseId"))

##### per country #####

#### for germany ####
data.complexity.de <- data.complexity.1 %>%  filter(country=="Germany")  %>% select(ResponseId, ab:ef) 

data.complexity.de$fa <- data.complexity.de$af
data.complexity.de$ea <- data.complexity.de$ae
data.complexity.de$da <- data.complexity.de$ad
data.complexity.de$ca <- data.complexity.de$ac
data.complexity.de$ba <- data.complexity.de$ab

data.complexity.de$fb <- data.complexity.de$bf
data.complexity.de$eb <- data.complexity.de$be
data.complexity.de$db <- data.complexity.de$bd
data.complexity.de$cb <- data.complexity.de$bc

data.complexity.de$fc <- data.complexity.de$cf
data.complexity.de$ec <- data.complexity.de$ce
data.complexity.de$dc <- data.complexity.de$cd

data.complexity.de$fd <- data.complexity.de$df
data.complexity.de$ed <- data.complexity.de$de

data.complexity.de$fe <- data.complexity.de$ef

data.complexity.de <- na.omit(data.complexity.de)
data.complexity.de.long <- pivot_longer(data.complexity.de, 2:31, names_to = "verhalten", values_to = "similarity")
data.complexity.de.long$verhalten2 <- data.complexity.de.long$verhalten
data.complexity.de.long$v2 <- substring(data.complexity.de.long$verhalten2, 2,)
data.complexity.de.long$v1 <- substring(data.complexity.de.long$verhalten, 1,1)

data.complexity.de.long$similarity <- as.numeric(data.complexity.de.long$similarity) 
data.complexity.de.long$similarity.2 <- (-(data.complexity.de.long$similarity) +10)

dfList.de <- split(data.complexity.de.long,data.complexity.de.long$ResponseId)
length(dfList.de)
names(dfList.de) <- paste0("id",1:length(dfList.de))

for(r in 1:length(dfList.de)) { 
  assign(paste0("matrix.de",r), NULL )
}


for (r in 1:length(dfList.de)) { 
  assign(paste0("matrix.de", r), dfList.de[[r]] %>% 
           mutate_at(1:2, factor, levels = unique(c(levels(.$v1), levels(.$v2)))) %>%
           xtabs(similarity.2 ~ v1 + v2, data=., sparse = FALSE) ) 
}

#### matrices ####
matrices.de<- (list(matrix.de1, matrix.de2, matrix.de3,matrix.de4,matrix.de5,matrix.de6,matrix.de7,matrix.de8,matrix.de9,matrix.de10,matrix.de11,matrix.de12,matrix.de13,matrix.de14 ,matrix.de15,
                  matrix.de16,matrix.de17,matrix.de18,matrix.de19,matrix.de20,matrix.de21,matrix.de22,matrix.de23,matrix.de24,matrix.de25,matrix.de26,matrix.de27,matrix.de28,matrix.de29,matrix.de30,
                  matrix.de31,matrix.de32,matrix.de33,matrix.de34,matrix.de35,matrix.de36,matrix.de37,matrix.de38,matrix.de39,matrix.de40,matrix.de41,matrix.de42,matrix.de43,matrix.de44,matrix.de45,
                  matrix.de46,matrix.de47,matrix.de48,matrix.de49,matrix.de50,matrix.de51,matrix.de52,matrix.de53,matrix.de54,matrix.de55,matrix.de56,matrix.de57,matrix.de58,matrix.de59,matrix.de60,
                  matrix.de61,matrix.de62,matrix.de63,matrix.de64,matrix.de65,matrix.de66,matrix.de67,matrix.de68,matrix.de69,matrix.de70,matrix.de71,matrix.de72,matrix.de73,matrix.de74,matrix.de75,
                  matrix.de76, matrix.de77,matrix.de78,matrix.de79,matrix.de80,matrix.de81,matrix.de82,matrix.de83,matrix.de84,matrix.de85,matrix.de86,matrix.de87,matrix.de88,matrix.de89,matrix.de90,
                  matrix.de91,matrix.de92,matrix.de93,matrix.de94,matrix.de95,matrix.de96,matrix.de97,matrix.de98,matrix.de99, matrix.de100,matrix.de101,matrix.de102,matrix.de103,matrix.de104,matrix.de105,
                  matrix.de106,matrix.de107,matrix.de108,matrix.de109,matrix.de110,matrix.de111,matrix.de112,matrix.de113,matrix.de114,matrix.de115,matrix.de116,matrix.de117,matrix.de118,matrix.de119,matrix.de120,
                  matrix.de121,matrix.de122,matrix.de123,matrix.de124,matrix.de125,matrix.de126,matrix.de127,matrix.de128,matrix.de129,matrix.de130,matrix.de131,matrix.de132,matrix.de133,matrix.de134,matrix.de135,
                  matrix.de136,matrix.de137,matrix.de138,matrix.de139,matrix.de140,matrix.de141,matrix.de142,matrix.de143,matrix.de144,matrix.de145,matrix.de146,matrix.de147,matrix.de148,matrix.de149,matrix.de150,
                  matrix.de151,matrix.de152,matrix.de153,matrix.de154,matrix.de155,matrix.de156,matrix.de157,matrix.de158,matrix.de159,matrix.de160,matrix.de161,matrix.de162,matrix.de163,matrix.de164,matrix.de165,
                  matrix.de166,matrix.de167,matrix.de168,matrix.de169,matrix.de170,matrix.de171,matrix.de172,matrix.de173,matrix.de174,matrix.de175,matrix.de176,matrix.de177,matrix.de178,matrix.de179,matrix.de180,
                  matrix.de181,matrix.de182,matrix.de183,matrix.de184,matrix.de185,matrix.de186,matrix.de187,matrix.de188,matrix.de189,matrix.de190, matrix.de191, matrix.de192, matrix.de193, matrix.de194, matrix.de195,
                  matrix.de196, matrix.de197, matrix.de198, matrix.de199, matrix.de200, matrix.de201, matrix.de202, matrix.de203, matrix.de204, matrix.de205, matrix.de206, matrix.de207, matrix.de208, matrix.de209, matrix.de210,
                  matrix.de211, matrix.de212 , matrix.de213, matrix.de214, matrix.de215, matrix.de216, matrix.de217, matrix.de218, matrix.de219, matrix.de220, matrix.de221, matrix.de222, matrix.de223, matrix.de224, matrix.de225,
                  matrix.de226, matrix.de227, matrix.de228, matrix.de229, matrix.de230, matrix.de231, matrix.de232, matrix.de233, matrix.de234, matrix.de235, matrix.de236, matrix.de237, matrix.de238, matrix.de239, matrix.de240,
                  matrix.de241, matrix.de242, matrix.de243, matrix.de244, matrix.de245, matrix.de246, matrix.de247, matrix.de248, matrix.de249, matrix.de250, matrix.de251, matrix.de252, matrix.de253, matrix.de254, matrix.de255,
                  matrix.de256, matrix.de257, matrix.de258,matrix.de259, matrix.de260, matrix.de261, matrix.de262, matrix.de263, matrix.de264, matrix.de265, matrix.de266, matrix.de267, matrix.de268, matrix.de269, matrix.de270, 
                  matrix.de271, matrix.de272, matrix.de273, matrix.de274,matrix.de275, matrix.de276, matrix.de277, matrix.de278, matrix.de279, matrix.de280, matrix.de281, matrix.de282, matrix.de283, matrix.de284, matrix.de285, 
                  matrix.de286, matrix.de287, matrix.de288, matrix.de289, matrix.de290,matrix.de291, matrix.de292, matrix.de293, matrix.de294, matrix.de295, matrix.de296, matrix.de297, matrix.de298, matrix.de299, matrix.de300, 
                  matrix.de301, matrix.de302, matrix.de303, matrix.de304, matrix.de305,matrix.de306, matrix.de307, matrix.de308, matrix.de309, matrix.de310, matrix.de311, matrix.de312, matrix.de313, matrix.de314, matrix.de315, 
                  matrix.de316, matrix.de317, matrix.de318, matrix.de319, matrix.de320, matrix.de321, matrix.de322, matrix.de323,matrix.de324, matrix.de325, matrix.de326, matrix.de327, matrix.de328, matrix.de329, matrix.de330, 
                  matrix.de331, matrix.de332, matrix.de333, matrix.de334, matrix.de335, matrix.de336, matrix.de337, matrix.de338, matrix.de339,matrix.de340, matrix.de341, matrix.de342, matrix.de343, matrix.de344, matrix.de345, 
                  matrix.de346, matrix.de347, matrix.de348, matrix.de349, matrix.de350, matrix.de351, matrix.de352, matrix.de353, matrix.de354, matrix.de355,matrix.de356, matrix.de357, matrix.de358, matrix.de359, matrix.de360, 
                  matrix.de361, matrix.de362, matrix.de363, matrix.de364, matrix.de365, matrix.de366, matrix.de367, matrix.de368, matrix.de369, matrix.de370, matrix.de371, matrix.de372,matrix.de373, matrix.de374, matrix.de375, 
                  matrix.de376, matrix.de377, matrix.de378, matrix.de379, matrix.de380, matrix.de381, matrix.de382, matrix.de383, matrix.de384, matrix.de385, matrix.de386, matrix.de387, matrix.de388, matrix.de389,matrix.de390,
                  matrix.de391, matrix.de392, matrix.de393, matrix.de394, matrix.de395, matrix.de396, matrix.de397, matrix.de398, matrix.de399, matrix.de400, matrix.de401, matrix.de402, matrix.de403, matrix.de404, matrix.de405,
                  matrix.de406, matrix.de407, matrix.de408, matrix.de409, matrix.de410, matrix.de411, matrix.de412, matrix.de413, matrix.de414, matrix.de415, matrix.de416, matrix.de417, matrix.de418, matrix.de419, matrix.de420,
                  matrix.de421, matrix.de422, matrix.de423, matrix.de424, matrix.de425, matrix.de426 , matrix.de427 , matrix.de428, matrix.de429, matrix.de430, matrix.de431, matrix.de432, matrix.de433, matrix.de434, matrix.de435,
                  matrix.de436, matrix.de437, matrix.de438, matrix.de439, matrix.de440, matrix.de441, matrix.de442, matrix.de443, matrix.de444, matrix.de445, matrix.de446, matrix.de447, matrix.de448, matrix.de449, matrix.de450,
                  matrix.de451, matrix.de452, matrix.de453, matrix.de454, matrix.de455, matrix.de456, matrix.de457, matrix.de458, matrix.de459, matrix.de460, matrix.de461, matrix.de462, matrix.de463 , matrix.de464, matrix.de465, 
                  matrix.de466, matrix.de467, matrix.de468, matrix.de469,matrix.de470, matrix.de471, matrix.de472, matrix.de473, matrix.de474, matrix.de475, matrix.de476, matrix.de477, matrix.de478, matrix.de479, matrix.de480, 
                  matrix.de481, matrix.de482, matrix.de483, matrix.de484,matrix.de485, matrix.de486, matrix.de487, matrix.de488, matrix.de489, matrix.de490, matrix.de491, matrix.de492, matrix.de493, matrix.de494, matrix.de495, 
                  matrix.de496, matrix.de497, matrix.de498, matrix.de499, matrix.de500, matrix.de501, matrix.de502, matrix.de503, matrix.de504, matrix.de505, matrix.de506, matrix.de507, matrix.de508, matrix.de509, matrix.de510, 
                  matrix.de511, matrix.de512, matrix.de513, matrix.de514,matrix.de515, matrix.de516, matrix.de517, matrix.de518, matrix.de519, matrix.de520, matrix.de521, matrix.de522, matrix.de523, matrix.de524, matrix.de525, 
                  matrix.de526, matrix.de527, matrix.de528, matrix.de529,matrix.de530, matrix.de531, matrix.de532, matrix.de533, matrix.de534, matrix.de535, matrix.de536, matrix.de537, matrix.de538, matrix.de539, matrix.de540,
                  matrix.de541, matrix.de542, matrix.de543, matrix.de544, matrix.de545,matrix.de546, matrix.de547, matrix.de548,matrix.de549, matrix.de550, matrix.de551, matrix.de552, matrix.de553, matrix.de554,matrix.de555,
                  matrix.de556, matrix.de557, matrix.de558, matrix.de559, matrix.de560, matrix.de561, matrix.de562, matrix.de563, matrix.de564, matrix.de565, matrix.de566, matrix.de567, matrix.de568, matrix.de569, matrix.de570, 
                  matrix.de571, matrix.de572, matrix.de573, matrix.de574,matrix.de575, matrix.de576, matrix.de577, matrix.de578, matrix.de579, matrix.de580, matrix.de581, matrix.de582, matrix.de583, matrix.de584, matrix.de585,
                  matrix.de586, matrix.de587, matrix.de588, matrix.de589, matrix.de590, matrix.de591, matrix.de592, matrix.de593, matrix.de594,matrix.de595, matrix.de596, matrix.de597, matrix.de598, matrix.de599, matrix.de600, 
                  matrix.de601, matrix.de602, matrix.de603, matrix.de604, matrix.de605, matrix.de606, matrix.de607, matrix.de608, matrix.de609, matrix.de610, matrix.de611, matrix.de612, matrix.de613, matrix.de614,matrix.de615, 
                  matrix.de616, matrix.de617, matrix.de618, matrix.de619, matrix.de620, matrix.de621, matrix.de622, matrix.de623, matrix.de624, matrix.de625, matrix.de626, matrix.de627, matrix.de628, matrix.de629, matrix.de630,
                  matrix.de631, matrix.de632, matrix.de633, matrix.de634, matrix.de635, matrix.de636, matrix.de637, matrix.de638, matrix.de639, matrix.de640, matrix.de641, matrix.de642, matrix.de643, matrix.de644, matrix.de645, 
                  matrix.de646, matrix.de647, matrix.de648, matrix.de649, matrix.de650, matrix.de651, matrix.de652, matrix.de653, matrix.de654, matrix.de655, matrix.de656, matrix.de657, matrix.de658, matrix.de659, matrix.de660,
                  matrix.de661, matrix.de662, matrix.de663, matrix.de664, matrix.de665, matrix.de666,matrix.de667, matrix.de668, matrix.de669, matrix.de670, matrix.de671, matrix.de672, matrix.de673, matrix.de674, matrix.de675, 
                  matrix.de676, matrix.de677, matrix.de678, matrix.de679, matrix.de680, matrix.de681, matrix.de682, matrix.de683, matrix.de684, matrix.de685, matrix.de686, matrix.de687, matrix.de688, matrix.de689, matrix.de690,
                  matrix.de691, matrix.de692, matrix.de693, matrix.de694, matrix.de695, matrix.de696, matrix.de697, matrix.de698, matrix.de699, matrix.de700, matrix.de701, matrix.de702, matrix.de703, matrix.de704, matrix.de705,
                  matrix.de706, matrix.de707, matrix.de708, matrix.de709, matrix.de710, matrix.de711, matrix.de712, matrix.de713, matrix.de714, matrix.de715, matrix.de716, matrix.de717, matrix.de718, matrix.de719, matrix.de720, 
                  matrix.de721, matrix.de722, matrix.de723, matrix.de724, matrix.de725, matrix.de726, matrix.de727, matrix.de728, matrix.de729, matrix.de730, matrix.de731, matrix.de732, matrix.de733, matrix.de734, matrix.de735,
                  matrix.de736, matrix.de737, matrix.de738, matrix.de739, matrix.de740, matrix.de741, matrix.de742, matrix.de743, matrix.de744, matrix.de745, matrix.de746, matrix.de747, matrix.de748, matrix.de749, matrix.de750,
                  matrix.de751, matrix.de752, matrix.de753, matrix.de754, matrix.de755, matrix.de756, matrix.de757, matrix.de758, matrix.de759, matrix.de760, matrix.de761, matrix.de762, matrix.de763, matrix.de764, matrix.de765,
                  matrix.de766, matrix.de767, matrix.de768, matrix.de769, matrix.de770, matrix.de771, matrix.de772, matrix.de773, matrix.de774, matrix.de775, matrix.de776, matrix.de777, matrix.de778, matrix.de779, matrix.de780,
                  matrix.de781, matrix.de782, matrix.de783, matrix.de784, matrix.de785, matrix.de786, matrix.de787, matrix.de788, matrix.de789, matrix.de790, matrix.de791, matrix.de792, matrix.de793, matrix.de794, matrix.de795,
                  matrix.de796, matrix.de797, matrix.de798, matrix.de799, matrix.de800, matrix.de801, matrix.de802, matrix.de803, matrix.de804, matrix.de805, matrix.de806, matrix.de807, matrix.de808, matrix.de809, matrix.de810,
                  matrix.de811, matrix.de812, matrix.de813, matrix.de814, matrix.de815, matrix.de816, matrix.de817, matrix.de818, matrix.de819, matrix.de820 
                  
))

## trying tranfsormation just to see what happens
library(proxy)
matrix.de1.d <- as.dist(matrix.de1)


rowSums(matrix.de1)
rowSums.dist(matrix.de1.d)

### individual difference scaling without starting position 
scaling.de <- smacof:::indscal(matrices.de, type="ordinal",  itmax=75000)
scaling.de$stress
names(scaling)
names(summary(scaling))

matrices.de.3 <- matrices.de
for (r in 1:length(dfList.de)){ 
  matrices.de.3[[r]] <- as.matrix(matrices.de.3[[r]])
}
scaling.de2 <- smacof:::indscal(matrices.de.3, type="ratio",  itmax=75000)
scaling.de2$stress ## stress much higher... and does not really help the configuration anyway 
summary(scaling.de2)
       
#scaling <- smacof:::idioscal(matrices, type="ordinal",  itmax=35000) virtually exactly the same result, same with "ties secondary" or changing modulus

plot.indscal.de <- as.data.frame(scaling.de$gspace)
plot.indscal.de$direction <- factor(c("pos","neg","neg",  "neg", "pos", "pos") )
plot.indscal.de$strength <- factor(c("high","low", "low", "high", "low", "high") )
plot.indscal.de$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco","Haare föhn vs luft", 
                               "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))

ggplot(plot.indscal.de, aes(x=D1, y=D2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + xlim(-.8, .8) +
  ylim(-.7, .7) +
  theme_minimal() +
  geom_text(aes(label = label), vjust=3, size=4)

###  individual difference scaling with starting position 
#init for starting values
startconf.p <- matrix(data=NA, nrow=6, ncol=2)
startconf.p <- matrix(data=NA, nrow=6, ncol=2)
startconf.p[,1] <- c(.1, -.1, -.1, -.1, .1, .1) *2
startconf.p[,2]  <- c(-.05, .05, .05, -.05, .05, -.05) *-4

scaling.start.de <- smacof:::indscal(matrices.de, init=startconf.p, type="ordinal", itmax=75000)
scaling.start.de$stress

plot.indscal.starting.de <- as.data.frame(scaling.start.de$gspace)
plot.indscal.starting.de$direction <- factor(c("pos","neg","neg",  "neg", "pos", "pos") )
plot.indscal.starting.de$strength <- factor(c("high","low", "low", "high", "low", "high") )
plot.indscal.starting.de$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco","Haare föhn vs luft", 
                                        "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))


ggplot(plot.indscal.starting.de, aes(x=D1, y=D2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + xlim(-.8, .8) +
  ylim(-.7, .7) +
  theme_minimal() +
  geom_text(aes(label = label), vjust=3, size=4)



##### individual weights ####
### weights for dimensions
weights.scal.start.de <- as.matrix(scaling.start.de$cweights)

individual.weights.de <- matrix(data=1, nrow=820, ncol=2)

for(r in 1:length(weights.scal.start.de)) { 
  individual.weights.de[r,2] <-  weights.scal.start.de [[c(r,4)]] ;
  individual.weights.de[r,1] <- weights.scal.start.de [[c(r,1)]] 
}

## V1 = D1, V2 = D2
individual.weights.de.2 <- as.data.frame(individual.weights.de)

### change!! either data.full, or data.complexity.de or data.complexity.ch !!
individual.weights.de.2$ResponseId <- unique(data.complexity.de$ResponseId)

individual.weights.de.2 <- merge(individual.weights.de.2, data.full, by=c("ResponseId"))

weights.total.de <- merge(individual.weights.de.2, data.cc.2, by="ResponseId")
weights.total.de$Sum <- weights.total.de$V1 + weights.total.de$V2
weights.total.de$Sum.sq <- weights.total.de$V1^2  + weights.total.de$V2^2 

weights.total.de$Mean <- rowMeans(cbind(weights.total.de$V1, weights.total.de$V2))

ggplot(weights.total.de, aes(x=V1, y=V2, color=climate_concern)) +
  geom_point() +  xlim(0,1.8) + ylim(0,1.8) +
  geom_abline(slope=1) +
  theme_minimal() 


weights.total.de$difference <- weights.total.de$V1-weights.total.de$V2
weights.total.de$difference <- factor(ifelse(weights.total.de$difference < 0, 1, 0))
table(weights.total.de$difference)

weights.total.de$V1.rounded <- round(weights.total.de$V1,1)
table(weights.total.de$V1.rounded)
weights.total.de$V2.rounded <- round(weights.total.de$V2,1)
table(weights.total.de$V2.rounded)

describe(weights.total.de$V1)
describe(weights.total.de$V2)

model.strength.dim.de <- lm(V2 ~ gender + income + age + concern.scaled  + education, data=weights.total.de)
summary(model.strength.dim.de)

model.direction.dim.de <- lm(V1 ~ gender + income + age + concern.scaled  + education, data=weights.total.de)
summary(model.direction.dim.de)

# with accuracy
weights.total.acc.de <- merge(weights.total.de,  data.accuracy.test, by="ResponseId")

model.direction.dim.acc.de <- lm(V1 ~ gender + income + age + concern.scaled  + education + mean.est.bias , data=weights.total.acc.de)
summary(model.direction.dim.acc.de)
  

#### for Switzerland ####
data.complexity.ch <- data.complexity.1 %>%  filter(!country=="Germany")  %>% select(ResponseId, ab:ef) 

data.complexity.ch$fa <- data.complexity.ch$af
data.complexity.ch$ea <- data.complexity.ch$ae
data.complexity.ch$da <- data.complexity.ch$ad
data.complexity.ch$ca <- data.complexity.ch$ac
data.complexity.ch$ba <- data.complexity.ch$ab

data.complexity.ch$fb <- data.complexity.ch$bf
data.complexity.ch$eb <- data.complexity.ch$be
data.complexity.ch$db <- data.complexity.ch$bd
data.complexity.ch$cb <- data.complexity.ch$bc

data.complexity.ch$fc <- data.complexity.ch$cf
data.complexity.ch$ec <- data.complexity.ch$ce
data.complexity.ch$dc <- data.complexity.ch$cd

data.complexity.ch$fd <- data.complexity.ch$df
data.complexity.ch$ed <- data.complexity.ch$de

data.complexity.ch$fe <- data.complexity.ch$ef

data.complexity.ch <- na.omit(data.complexity.ch)
data.complexity.ch.long <- pivot_longer(data.complexity.ch, 2:31, names_to = "verhalten", values_to = "similarity")
data.complexity.ch.long$verhalten2 <- data.complexity.ch.long$verhalten
data.complexity.ch.long$v2 <- substring(data.complexity.ch.long$verhalten2, 2,)
data.complexity.ch.long$v1 <- substring(data.complexity.ch.long$verhalten, 1,1)

data.complexity.ch.long$similarity <- as.numeric(data.complexity.ch.long$similarity) 
data.complexity.ch.long$similarity.2 <- (-(data.complexity.ch.long$similarity) +10)

dfList.ch <- split(data.complexity.ch.long,data.complexity.ch.long$ResponseId)
length(dfList.ch)
names(dfList.ch) <- paste0("id",1:length(dfList.ch))

for(r in 1:length(dfList.ch)) { 
  assign(paste0("matrix.ch",r), NULL )
}


for (r in 1:length(dfList.ch)) { 
  assign(paste0("matrix.ch", r), dfList.ch[[r]] %>% 
           mutate_at(1:2, factor, levels = unique(c(levels(.$v1), levels(.$v2)))) %>%
           xtabs(similarity.2 ~ v1 + v2, data=., sparse = FALSE) ) 
}

matrices.ch<- (list(matrix.ch1, matrix.ch2, matrix.ch3,matrix.ch4,matrix.ch5,matrix.ch6,matrix.ch7,matrix.ch8,matrix.ch9,matrix.ch10,matrix.ch11,matrix.ch12,matrix.ch13,matrix.ch14 ,matrix.ch15,
                    matrix.ch16,matrix.ch17,matrix.ch18,matrix.ch19,matrix.ch20,matrix.ch21,matrix.ch22,matrix.ch23,matrix.ch24,matrix.ch25,matrix.ch26,matrix.ch27,matrix.ch28,matrix.ch29,matrix.ch30,
                    matrix.ch31,matrix.ch32,matrix.ch33,matrix.ch34,matrix.ch35,matrix.ch36,matrix.ch37,matrix.ch38,matrix.ch39,matrix.ch40,matrix.ch41,matrix.ch42,matrix.ch43,matrix.ch44,matrix.ch45,
                    matrix.ch46,matrix.ch47,matrix.ch48,matrix.ch49,matrix.ch50,matrix.ch51,matrix.ch52,matrix.ch53,matrix.ch54,matrix.ch55,matrix.ch56,matrix.ch57,matrix.ch58,matrix.ch59,matrix.ch60,
                    matrix.ch61,matrix.ch62,matrix.ch63,matrix.ch64,matrix.ch65,matrix.ch66,matrix.ch67,matrix.ch68,matrix.ch69,matrix.ch70,matrix.ch71,matrix.ch72,matrix.ch73,matrix.ch74,matrix.ch75,
                    matrix.ch76, matrix.ch77,matrix.ch78,matrix.ch79,matrix.ch80,matrix.ch81,matrix.ch82,matrix.ch83,matrix.ch84,matrix.ch85,matrix.ch86,matrix.ch87,matrix.ch88,matrix.ch89,matrix.ch90,
                    matrix.ch91,matrix.ch92,matrix.ch93,matrix.ch94,matrix.ch95,matrix.ch96,matrix.ch97,matrix.ch98,matrix.ch99, matrix.ch100,matrix.ch101,matrix.ch102,matrix.ch103,matrix.ch104,matrix.ch105,
                    matrix.ch106,matrix.ch107,matrix.ch108,matrix.ch109,matrix.ch110,matrix.ch111,matrix.ch112,matrix.ch113,matrix.ch114,matrix.ch115,matrix.ch116,matrix.ch117,matrix.ch118,matrix.ch119,matrix.ch120,
                    matrix.ch121,matrix.ch122,matrix.ch123,matrix.ch124,matrix.ch125,matrix.ch126,matrix.ch127,matrix.ch128,matrix.ch129,matrix.ch130,matrix.ch131,matrix.ch132,matrix.ch133,matrix.ch134,matrix.ch135,
                    matrix.ch136,matrix.ch137,matrix.ch138,matrix.ch139,matrix.ch140,matrix.ch141,matrix.ch142,matrix.ch143,matrix.ch144,matrix.ch145,matrix.ch146,matrix.ch147,matrix.ch148,matrix.ch149,matrix.ch150,
                    matrix.ch151,matrix.ch152,matrix.ch153,matrix.ch154,matrix.ch155,matrix.ch156,matrix.ch157,matrix.ch158,matrix.ch159,matrix.ch160,matrix.ch161,matrix.ch162,matrix.ch163,matrix.ch164,matrix.ch165,
                    matrix.ch166,matrix.ch167,matrix.ch168,matrix.ch169,matrix.ch170,matrix.ch171,matrix.ch172,matrix.ch173,matrix.ch174,matrix.ch175,matrix.ch176,matrix.ch177,matrix.ch178,matrix.ch179,matrix.ch180,
                    matrix.ch181,matrix.ch182,matrix.ch183,matrix.ch184,matrix.ch185,matrix.ch186,matrix.ch187,matrix.ch188,matrix.ch189,matrix.ch190, matrix.ch191, matrix.ch192, matrix.ch193, matrix.ch194, matrix.ch195,
                    matrix.ch196, matrix.ch197, matrix.ch198, matrix.ch199, matrix.ch200, matrix.ch201, matrix.ch202, matrix.ch203, matrix.ch204, matrix.ch205, matrix.ch206, matrix.ch207, matrix.ch208, matrix.ch209, matrix.ch210,
                    matrix.ch211, matrix.ch212 , matrix.ch213, matrix.ch214, matrix.ch215, matrix.ch216, matrix.ch217, matrix.ch218, matrix.ch219, matrix.ch220, matrix.ch221, matrix.ch222, matrix.ch223, matrix.ch224, matrix.ch225,
                    matrix.ch226, matrix.ch227, matrix.ch228, matrix.ch229, matrix.ch230, matrix.ch231, matrix.ch232, matrix.ch233, matrix.ch234, matrix.ch235, matrix.ch236, matrix.ch237, matrix.ch238, matrix.ch239, matrix.ch240,
                    matrix.ch241, matrix.ch242, matrix.ch243, matrix.ch244, matrix.ch245, matrix.ch246, matrix.ch247, matrix.ch248, matrix.ch249, matrix.ch250, matrix.ch251, matrix.ch252, matrix.ch253, matrix.ch254, matrix.ch255,
                    matrix.ch256, matrix.ch257, matrix.ch258,matrix.ch259, matrix.ch260, matrix.ch261, matrix.ch262, matrix.ch263, matrix.ch264, matrix.ch265, matrix.ch266, matrix.ch267, matrix.ch268, matrix.ch269, matrix.ch270, 
                    matrix.ch271, matrix.ch272, matrix.ch273, matrix.ch274,matrix.ch275, matrix.ch276, matrix.ch277, matrix.ch278, matrix.ch279, matrix.ch280, matrix.ch281, matrix.ch282, matrix.ch283, matrix.ch284, matrix.ch285, 
                    matrix.ch286, matrix.ch287, matrix.ch288, matrix.ch289, matrix.ch290,matrix.ch291, matrix.ch292, matrix.ch293, matrix.ch294, matrix.ch295, matrix.ch296, matrix.ch297, matrix.ch298, matrix.ch299, matrix.ch300, 
                    matrix.ch301, matrix.ch302, matrix.ch303, matrix.ch304, matrix.ch305,matrix.ch306, matrix.ch307, matrix.ch308, matrix.ch309, matrix.ch310, matrix.ch311, matrix.ch312, matrix.ch313, matrix.ch314, matrix.ch315, 
                    matrix.ch316, matrix.ch317, matrix.ch318, matrix.ch319, matrix.ch320, matrix.ch321, matrix.ch322, matrix.ch323,matrix.ch324, matrix.ch325, matrix.ch326, matrix.ch327, matrix.ch328, matrix.ch329, matrix.ch330, 
                    matrix.ch331, matrix.ch332, matrix.ch333, matrix.ch334, matrix.ch335, matrix.ch336, matrix.ch337, matrix.ch338, matrix.ch339,matrix.ch340, matrix.ch341, matrix.ch342, matrix.ch343, matrix.ch344, matrix.ch345, 
                    matrix.ch346, matrix.ch347, matrix.ch348, matrix.ch349, matrix.ch350, matrix.ch351, matrix.ch352, matrix.ch353, matrix.ch354, matrix.ch355,matrix.ch356, matrix.ch357, matrix.ch358, matrix.ch359, matrix.ch360, 
                    matrix.ch361, matrix.ch362, matrix.ch363, matrix.ch364, matrix.ch365, matrix.ch366, matrix.ch367, matrix.ch368, matrix.ch369, matrix.ch370, matrix.ch371, matrix.ch372,matrix.ch373, matrix.ch374, matrix.ch375, 
                    matrix.ch376, matrix.ch377, matrix.ch378, matrix.ch379, matrix.ch380, matrix.ch381, matrix.ch382, matrix.ch383, matrix.ch384, matrix.ch385, matrix.ch386, matrix.ch387, matrix.ch388, matrix.ch389,matrix.ch390,
                    matrix.ch391, matrix.ch392, matrix.ch393, matrix.ch394, matrix.ch395, matrix.ch396, matrix.ch397, matrix.ch398, matrix.ch399, matrix.ch400, matrix.ch401, matrix.ch402, matrix.ch403, matrix.ch404, matrix.ch405,
                    matrix.ch406, matrix.ch407, matrix.ch408, matrix.ch409, matrix.ch410, matrix.ch411, matrix.ch412, matrix.ch413, matrix.ch414, matrix.ch415, matrix.ch416, matrix.ch417, matrix.ch418, matrix.ch419, matrix.ch420,
                    matrix.ch421, matrix.ch422, matrix.ch423, matrix.ch424, matrix.ch425, matrix.ch426 , matrix.ch427 , matrix.ch428, matrix.ch429, matrix.ch430, matrix.ch431, matrix.ch432, matrix.ch433, matrix.ch434, matrix.ch435,
                    matrix.ch436, matrix.ch437, matrix.ch438, matrix.ch439, matrix.ch440, matrix.ch441, matrix.ch442, matrix.ch443, matrix.ch444, matrix.ch445, matrix.ch446, matrix.ch447, matrix.ch448, matrix.ch449, matrix.ch450,
                    matrix.ch451, matrix.ch452, matrix.ch453, matrix.ch454, matrix.ch455, matrix.ch456, matrix.ch457, matrix.ch458, matrix.ch459, matrix.ch460, matrix.ch461, matrix.ch462, matrix.ch463 , matrix.ch464, matrix.ch465, 
                    matrix.ch466, matrix.ch467, matrix.ch468, matrix.ch469,matrix.ch470, matrix.ch471, matrix.ch472, matrix.ch473, matrix.ch474, matrix.ch475, matrix.ch476, matrix.ch477, matrix.ch478, matrix.ch479, matrix.ch480, 
                    matrix.ch481, matrix.ch482, matrix.ch483, matrix.ch484,matrix.ch485, matrix.ch486, matrix.ch487, matrix.ch488, matrix.ch489, matrix.ch490, matrix.ch491, matrix.ch492, matrix.ch493, matrix.ch494, matrix.ch495, 
                    matrix.ch496, matrix.ch497, matrix.ch498, matrix.ch499, matrix.ch500, matrix.ch501, matrix.ch502, matrix.ch503, matrix.ch504, matrix.ch505, matrix.ch506, matrix.ch507, matrix.ch508, matrix.ch509, matrix.ch510, 
                    matrix.ch511, matrix.ch512, matrix.ch513, matrix.ch514,matrix.ch515, matrix.ch516, matrix.ch517, matrix.ch518, matrix.ch519, matrix.ch520, matrix.ch521, matrix.ch522, matrix.ch523, matrix.ch524, matrix.ch525, 
                    matrix.ch526, matrix.ch527, matrix.ch528, matrix.ch529,matrix.ch530, matrix.ch531, matrix.ch532, matrix.ch533, matrix.ch534, matrix.ch535, matrix.ch536, matrix.ch537, matrix.ch538, matrix.ch539, matrix.ch540,
                    matrix.ch541, matrix.ch542, matrix.ch543, matrix.ch544, matrix.ch545,matrix.ch546, matrix.ch547, matrix.ch548,matrix.ch549, matrix.ch550, matrix.ch551, matrix.ch552, matrix.ch553, matrix.ch554,matrix.ch555,
                    matrix.ch556, matrix.ch557, matrix.ch558, matrix.ch559, matrix.ch560, matrix.ch561, matrix.ch562, matrix.ch563, matrix.ch564, matrix.ch565, matrix.ch566, matrix.ch567, matrix.ch568, matrix.ch569, matrix.ch570, 
                    matrix.ch571, matrix.ch572, matrix.ch573, matrix.ch574,matrix.ch575, matrix.ch576, matrix.ch577, matrix.ch578, matrix.ch579, matrix.ch580, matrix.ch581, matrix.ch582, matrix.ch583, matrix.ch584, matrix.ch585,
                    matrix.ch586, matrix.ch587, matrix.ch588, matrix.ch589, matrix.ch590, matrix.ch591, matrix.ch592, matrix.ch593, matrix.ch594,matrix.ch595, matrix.ch596, matrix.ch597, matrix.ch598, matrix.ch599, matrix.ch600, 
                    matrix.ch601, matrix.ch602, matrix.ch603, matrix.ch604, matrix.ch605, matrix.ch606, matrix.ch607, matrix.ch608, matrix.ch609, matrix.ch610, matrix.ch611, matrix.ch612, matrix.ch613, matrix.ch614,matrix.ch615, 
                    matrix.ch616, matrix.ch617, matrix.ch618, matrix.ch619, matrix.ch620, matrix.ch621, matrix.ch622, matrix.ch623, matrix.ch624, matrix.ch625, matrix.ch626, matrix.ch627, matrix.ch628, matrix.ch629, matrix.ch630,
                    matrix.ch631, matrix.ch632, matrix.ch633, matrix.ch634, matrix.ch635, matrix.ch636, matrix.ch637, matrix.ch638, matrix.ch639, matrix.ch640, matrix.ch641, matrix.ch642, matrix.ch643, matrix.ch644, matrix.ch645, 
                    matrix.ch646, matrix.ch647, matrix.ch648, matrix.ch649, matrix.ch650, matrix.ch651, matrix.ch652, matrix.ch653, matrix.ch654, matrix.ch655, matrix.ch656, matrix.ch657, matrix.ch658, matrix.ch659, matrix.ch660,
                    matrix.ch661, matrix.ch662, matrix.ch663, matrix.ch664, matrix.ch665, matrix.ch666,matrix.ch667, matrix.ch668, matrix.ch669, matrix.ch670, matrix.ch671, matrix.ch672, matrix.ch673, matrix.ch674, matrix.ch675, 
                    matrix.ch676, matrix.ch677, matrix.ch678, matrix.ch679, matrix.ch680, matrix.ch681, matrix.ch682, matrix.ch683, matrix.ch684, matrix.ch685, matrix.ch686, matrix.ch687, matrix.ch688, matrix.ch689, matrix.ch690,
                    matrix.ch691, matrix.ch692, matrix.ch693, matrix.ch694, matrix.ch695, matrix.ch696, matrix.ch697, matrix.ch698, matrix.ch699, matrix.ch700, matrix.ch701, matrix.ch702, matrix.ch703, matrix.ch704, matrix.ch705,
                    matrix.ch706, matrix.ch707, matrix.ch708, matrix.ch709, matrix.ch710, matrix.ch711, matrix.ch712, matrix.ch713, matrix.ch714, matrix.ch715, matrix.ch716, matrix.ch717, matrix.ch718, matrix.ch719, matrix.ch720, 
                    matrix.ch721, matrix.ch722, matrix.ch723, matrix.ch724, matrix.ch725, matrix.ch726, matrix.ch727, matrix.ch728, matrix.ch729, matrix.ch730, matrix.ch731, matrix.ch732, matrix.ch733, matrix.ch734, matrix.ch735,
                    matrix.ch736, matrix.ch737, matrix.ch738, matrix.ch739, matrix.ch740, matrix.ch741, matrix.ch742, matrix.ch743, matrix.ch744, matrix.ch745, matrix.ch746, matrix.ch747, matrix.ch748, matrix.ch749, matrix.ch750,
                    matrix.ch751, matrix.ch752, matrix.ch753, matrix.ch754, matrix.ch755, matrix.ch756, matrix.ch757, matrix.ch758, matrix.ch759, matrix.ch760, matrix.ch761, matrix.ch762, matrix.ch763, matrix.ch764, matrix.ch765,
                    matrix.ch766, matrix.ch767, matrix.ch768, matrix.ch769, matrix.ch770, matrix.ch771, matrix.ch772, matrix.ch773, matrix.ch774, matrix.ch775, matrix.ch776, matrix.ch777, matrix.ch778, matrix.ch779, matrix.ch780,
                    matrix.ch781, matrix.ch782, matrix.ch783, matrix.ch784, matrix.ch785, matrix.ch786, matrix.ch787, matrix.ch788, matrix.ch789, matrix.ch790, matrix.ch791, matrix.ch792, matrix.ch793, matrix.ch794, matrix.ch795,
                    matrix.ch796, matrix.ch797, matrix.ch798, matrix.ch799, matrix.ch800, matrix.ch801, matrix.ch802, matrix.ch803, matrix.ch804, matrix.ch805, matrix.ch806, matrix.ch807, matrix.ch808
))


### individual difference scaling without starting position 
scaling.ch <- smacof:::indscal(matrices.ch, type="ordinal",  itmax=75000)
scaling.ch$stress
#scaling <- smacof:::idioscal(matrices, type="ordinal",  itmax=35000) virtually exactly the same result, same with "ties secondary" or changing modulus

plot.indscal.ch <- as.data.frame(scaling.ch$gspace)
plot.indscal.ch$direction <- factor(c("pos","neg","neg",  "neg", "pos", "pos") )
plot.indscal.ch$strength <- factor(c("high","low", "low", "high", "low", "high") )
plot.indscal.ch$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco","Haare föhn vs luft", 
                                  "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))

ggplot(plot.indscal.ch, aes(x=D1, y=D2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + xlim(-.8, .8) +
  ylim(-.7, .7) +
  theme_minimal() +
  geom_text(aes(label = label), vjust=3, size=4)

# does not conform to the 2 dimensions that we want

###  individual difference scaling with starting position 
#init for starting values 

scaling.start.ch <- smacof:::indscal(matrices.ch, init=startconf.p, type="ordinal", itmax=75000)
scaling.start.ch$stress

plot.indscal.starting.ch <- as.data.frame(scaling.start.ch$gspace)
plot.indscal.starting.ch$direction <- factor(c("pos","neg","neg",  "neg", "pos", "pos") )
plot.indscal.starting.ch$strength <- factor(c("high","low", "low", "high", "low", "high") )
plot.indscal.starting.ch$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco","Haare föhn vs luft", 
                                           "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))
ggplot(plot.indscal.starting.ch, aes(x=D1, y=D2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + xlim(-.8, .8) +
  ylim(-.7, .7) +
  theme_minimal() +
  geom_text(aes(label = label), vjust=3, size=4)


#### indivudal weights ####

weights.scal.start.ch <- as.matrix(scaling.euc.ch$cweights)

individual.weights.ch <- matrix(data=1, nrow=808, ncol=2)

for(r in 1:length(weights.scal.start.ch)) { 
  individual.weights.ch[r,2] <-  weights.scal.start.ch[[c(r,4)]] ;
  individual.weights.ch[r,1] <- weights.scal.start.ch[[c(r,1)]] 
}

## V1 = D1, V2 = D2
individual.weights.ch.2 <- as.data.frame(individual.weights.ch)

individual.weights.ch.2$ResponseId <- unique(data.complexity.ch$ResponseId)

individual.weights.ch.2 <- merge(individual.weights.ch.2, data.full, by=c("ResponseId"))

weights.total.ch <- merge(individual.weights.ch.2, data.cc.2, by="ResponseId")
weights.total.ch$Sum <- weights.total.ch$V1 + weights.total.ch$V2
weights.total.ch$Sum.sq <- weights.total.ch$V1^2  + weights.total.ch$V2^2 

weights.total.ch$Mean <- rowMeans(cbind(weights.total.ch$V1, weights.total.ch$V2))

ggplot(weights.total.ch, aes(x=V1, y=V2, color=climate_concern)) +
  geom_point() + # xlim(0,2) + ylim(0,2) +
  theme_minimal() 


weights.total.ch$difference <- weights.total.ch$V1-weights.total.ch$V2
weights.total.ch$difference <- factor(ifelse(weights.total.ch$difference < 0, 1, 0))
table(weights.total.ch$difference)

describe(weights.total.ch$V1)
describe(weights.total.ch$V2)

model.strength.dim.ch <- lm(V2 ~ gender + income + age + concern.scaled  + education, data=weights.total.ch)
summary(model.strength.dim.ch)

model.direction.dim.ch <- lm(V1 ~ gender + income + age + concern.scaled  + education, data=weights.total.ch)
summary(model.direction.dim.ch)

# with accuracy
weights.total.acc.ch <- merge(weights.total.ch,  data.accuracy.test, by="ResponseId")

model.direction.dim.acc.ch <- lm(V1 ~ gender + income + age + concern.scaled  + education + mean.est.bias , data=weights.total.acc.ch)
summary(model.direction.dim.acc.ch)

#### for gender ####

#### for female ####
data.complexity.fe <- data.complexity.1 %>%  filter(gender=="female")  %>% select(ResponseId, ab:ef) 

data.complexity.fe$fa <- data.complexity.fe$af
data.complexity.fe$ea <- data.complexity.fe$ae
data.complexity.fe$da <- data.complexity.fe$ad
data.complexity.fe$ca <- data.complexity.fe$ac
data.complexity.fe$ba <- data.complexity.fe$ab

data.complexity.fe$fb <- data.complexity.fe$bf
data.complexity.fe$eb <- data.complexity.fe$be
data.complexity.fe$db <- data.complexity.fe$bd
data.complexity.fe$cb <- data.complexity.fe$bc

data.complexity.fe$fc <- data.complexity.fe$cf
data.complexity.fe$ec <- data.complexity.fe$ce
data.complexity.fe$dc <- data.complexity.fe$cd

data.complexity.fe$fd <- data.complexity.fe$df
data.complexity.fe$ed <- data.complexity.fe$de

data.complexity.fe$fe <- data.complexity.fe$ef

data.complexity.fe <- na.omit(data.complexity.fe)
data.complexity.fe.long <- pivot_longer(data.complexity.fe, 2:31, names_to = "verhalten", values_to = "similarity")
data.complexity.fe.long$verhalten2 <- data.complexity.fe.long$verhalten
data.complexity.fe.long$v2 <- substring(data.complexity.fe.long$verhalten2, 2,)
data.complexity.fe.long$v1 <- substring(data.complexity.fe.long$verhalten, 1,1)

data.complexity.fe.long$similarity <- as.numeric(data.complexity.fe.long$similarity) 
data.complexity.fe.long$similarity.2 <- (-(data.complexity.fe.long$similarity) +10)

dfList.fe <- split(data.complexity.fe.long,data.complexity.fe.long$ResponseId)
length(dfList.fe)
names(dfList.fe) <- paste0("id",1:length(dfList.fe))

for(r in 1:length(dfList.fe)) { 
  assign(paste0("matrix.fe",r), NULL )
}


for (r in 1:length(dfList.fe)) { 
  assign(paste0("matrix.fe", r), dfList.fe[[r]] %>% 
           mutate_at(1:2, factor, levels = unique(c(levels(.$v1), levels(.$v2)))) %>%
           xtabs(similarity.2 ~ v1 + v2, data=., sparse = FALSE) ) 
}

#### matrices ####
matrices.fe<- (list(matrix.fe1, matrix.fe2, matrix.fe3,matrix.fe4,matrix.fe5,matrix.fe6,matrix.fe7,matrix.fe8,matrix.fe9,matrix.fe10,matrix.fe11,matrix.fe12,matrix.fe13,matrix.fe14 ,matrix.fe15,
                    matrix.fe16,matrix.fe17,matrix.fe18,matrix.fe19,matrix.fe20,matrix.fe21,matrix.fe22,matrix.fe23,matrix.fe24,matrix.fe25,matrix.fe26,matrix.fe27,matrix.fe28,matrix.fe29,matrix.fe30,
                    matrix.fe31,matrix.fe32,matrix.fe33,matrix.fe34,matrix.fe35,matrix.fe36,matrix.fe37,matrix.fe38,matrix.fe39,matrix.fe40,matrix.fe41,matrix.fe42,matrix.fe43,matrix.fe44,matrix.fe45,
                    matrix.fe46,matrix.fe47,matrix.fe48,matrix.fe49,matrix.fe50,matrix.fe51,matrix.fe52,matrix.fe53,matrix.fe54,matrix.fe55,matrix.fe56,matrix.fe57,matrix.fe58,matrix.fe59,matrix.fe60,
                    matrix.fe61,matrix.fe62,matrix.fe63,matrix.fe64,matrix.fe65,matrix.fe66,matrix.fe67,matrix.fe68,matrix.fe69,matrix.fe70,matrix.fe71,matrix.fe72,matrix.fe73,matrix.fe74,matrix.fe75,
                    matrix.fe76, matrix.fe77,matrix.fe78,matrix.fe79,matrix.fe80,matrix.fe81,matrix.fe82,matrix.fe83,matrix.fe84,matrix.fe85,matrix.fe86,matrix.fe87,matrix.fe88,matrix.fe89,matrix.fe90,
                    matrix.fe91,matrix.fe92,matrix.fe93,matrix.fe94,matrix.fe95,matrix.fe96,matrix.fe97,matrix.fe98,matrix.fe99, matrix.fe100,matrix.fe101,matrix.fe102,matrix.fe103,matrix.fe104,matrix.fe105,
                    matrix.fe106,matrix.fe107,matrix.fe108,matrix.fe109,matrix.fe110,matrix.fe111,matrix.fe112,matrix.fe113,matrix.fe114,matrix.fe115,matrix.fe116,matrix.fe117,matrix.fe118,matrix.fe119,matrix.fe120,
                    matrix.fe121,matrix.fe122,matrix.fe123,matrix.fe124,matrix.fe125,matrix.fe126,matrix.fe127,matrix.fe128,matrix.fe129,matrix.fe130,matrix.fe131,matrix.fe132,matrix.fe133,matrix.fe134,matrix.fe135,
                    matrix.fe136,matrix.fe137,matrix.fe138,matrix.fe139,matrix.fe140,matrix.fe141,matrix.fe142,matrix.fe143,matrix.fe144,matrix.fe145,matrix.fe146,matrix.fe147,matrix.fe148,matrix.fe149,matrix.fe150,
                    matrix.fe151,matrix.fe152,matrix.fe153,matrix.fe154,matrix.fe155,matrix.fe156,matrix.fe157,matrix.fe158,matrix.fe159,matrix.fe160,matrix.fe161,matrix.fe162,matrix.fe163,matrix.fe164,matrix.fe165,
                    matrix.fe166,matrix.fe167,matrix.fe168,matrix.fe169,matrix.fe170,matrix.fe171,matrix.fe172,matrix.fe173,matrix.fe174,matrix.fe175,matrix.fe176,matrix.fe177,matrix.fe178,matrix.fe179,matrix.fe180,
                    matrix.fe181,matrix.fe182,matrix.fe183,matrix.fe184,matrix.fe185,matrix.fe186,matrix.fe187,matrix.fe188,matrix.fe189,matrix.fe190, matrix.fe191, matrix.fe192, matrix.fe193, matrix.fe194, matrix.fe195,
                    matrix.fe196, matrix.fe197, matrix.fe198, matrix.fe199, matrix.fe200, matrix.fe201, matrix.fe202, matrix.fe203, matrix.fe204, matrix.fe205, matrix.fe206, matrix.fe207, matrix.fe208, matrix.fe209, matrix.fe210,
                    matrix.fe211, matrix.fe212 , matrix.fe213, matrix.fe214, matrix.fe215, matrix.fe216, matrix.fe217, matrix.fe218, matrix.fe219, matrix.fe220, matrix.fe221, matrix.fe222, matrix.fe223, matrix.fe224, matrix.fe225,
                    matrix.fe226, matrix.fe227, matrix.fe228, matrix.fe229, matrix.fe230, matrix.fe231, matrix.fe232, matrix.fe233, matrix.fe234, matrix.fe235, matrix.fe236, matrix.fe237, matrix.fe238, matrix.fe239, matrix.fe240,
                    matrix.fe241, matrix.fe242, matrix.fe243, matrix.fe244, matrix.fe245, matrix.fe246, matrix.fe247, matrix.fe248, matrix.fe249, matrix.fe250, matrix.fe251, matrix.fe252, matrix.fe253, matrix.fe254, matrix.fe255,
                    matrix.fe256, matrix.fe257, matrix.fe258,matrix.fe259, matrix.fe260, matrix.fe261, matrix.fe262, matrix.fe263, matrix.fe264, matrix.fe265, matrix.fe266, matrix.fe267, matrix.fe268, matrix.fe269, matrix.fe270, 
                    matrix.fe271, matrix.fe272, matrix.fe273, matrix.fe274,matrix.fe275, matrix.fe276, matrix.fe277, matrix.fe278, matrix.fe279, matrix.fe280, matrix.fe281, matrix.fe282, matrix.fe283, matrix.fe284, matrix.fe285, 
                    matrix.fe286, matrix.fe287, matrix.fe288, matrix.fe289, matrix.fe290,matrix.fe291, matrix.fe292, matrix.fe293, matrix.fe294, matrix.fe295, matrix.fe296, matrix.fe297, matrix.fe298, matrix.fe299, matrix.fe300, 
                    matrix.fe301, matrix.fe302, matrix.fe303, matrix.fe304, matrix.fe305,matrix.fe306, matrix.fe307, matrix.fe308, matrix.fe309, matrix.fe310, matrix.fe311, matrix.fe312, matrix.fe313, matrix.fe314, matrix.fe315, 
                    matrix.fe316, matrix.fe317, matrix.fe318, matrix.fe319, matrix.fe320, matrix.fe321, matrix.fe322, matrix.fe323,matrix.fe324, matrix.fe325, matrix.fe326, matrix.fe327, matrix.fe328, matrix.fe329, matrix.fe330, 
                    matrix.fe331, matrix.fe332, matrix.fe333, matrix.fe334, matrix.fe335, matrix.fe336, matrix.fe337, matrix.fe338, matrix.fe339,matrix.fe340, matrix.fe341, matrix.fe342, matrix.fe343, matrix.fe344, matrix.fe345, 
                    matrix.fe346, matrix.fe347, matrix.fe348, matrix.fe349, matrix.fe350, matrix.fe351, matrix.fe352, matrix.fe353, matrix.fe354, matrix.fe355,matrix.fe356, matrix.fe357, matrix.fe358, matrix.fe359, matrix.fe360, 
                    matrix.fe361, matrix.fe362, matrix.fe363, matrix.fe364, matrix.fe365, matrix.fe366, matrix.fe367, matrix.fe368, matrix.fe369, matrix.fe370, matrix.fe371, matrix.fe372,matrix.fe373, matrix.fe374, matrix.fe375, 
                    matrix.fe376, matrix.fe377, matrix.fe378, matrix.fe379, matrix.fe380, matrix.fe381, matrix.fe382, matrix.fe383, matrix.fe384, matrix.fe385, matrix.fe386, matrix.fe387, matrix.fe388, matrix.fe389,matrix.fe390,
                    matrix.fe391, matrix.fe392, matrix.fe393, matrix.fe394, matrix.fe395, matrix.fe396, matrix.fe397, matrix.fe398, matrix.fe399, matrix.fe400, matrix.fe401, matrix.fe402, matrix.fe403, matrix.fe404, matrix.fe405,
                    matrix.fe406, matrix.fe407, matrix.fe408, matrix.fe409, matrix.fe410, matrix.fe411, matrix.fe412, matrix.fe413, matrix.fe414, matrix.fe415, matrix.fe416, matrix.fe417, matrix.fe418, matrix.fe419, matrix.fe420,
                    matrix.fe421, matrix.fe422, matrix.fe423, matrix.fe424, matrix.fe425, matrix.fe426 , matrix.fe427 , matrix.fe428, matrix.fe429, matrix.fe430, matrix.fe431, matrix.fe432, matrix.fe433, matrix.fe434, matrix.fe435,
                    matrix.fe436, matrix.fe437, matrix.fe438, matrix.fe439, matrix.fe440, matrix.fe441, matrix.fe442, matrix.fe443, matrix.fe444, matrix.fe445, matrix.fe446, matrix.fe447, matrix.fe448, matrix.fe449, matrix.fe450,
                    matrix.fe451, matrix.fe452, matrix.fe453, matrix.fe454, matrix.fe455, matrix.fe456, matrix.fe457, matrix.fe458, matrix.fe459, matrix.fe460, matrix.fe461, matrix.fe462, matrix.fe463 , matrix.fe464, matrix.fe465, 
                    matrix.fe466, matrix.fe467, matrix.fe468, matrix.fe469,matrix.fe470, matrix.fe471, matrix.fe472, matrix.fe473, matrix.fe474, matrix.fe475, matrix.fe476, matrix.fe477, matrix.fe478, matrix.fe479, matrix.fe480, 
                    matrix.fe481, matrix.fe482, matrix.fe483, matrix.fe484,matrix.fe485, matrix.fe486, matrix.fe487, matrix.fe488, matrix.fe489, matrix.fe490, matrix.fe491, matrix.fe492, matrix.fe493, matrix.fe494, matrix.fe495, 
                    matrix.fe496, matrix.fe497, matrix.fe498, matrix.fe499, matrix.fe500, matrix.fe501, matrix.fe502, matrix.fe503, matrix.fe504, matrix.fe505, matrix.fe506, matrix.fe507, matrix.fe508, matrix.fe509, matrix.fe510, 
                    matrix.fe511, matrix.fe512, matrix.fe513, matrix.fe514,matrix.fe515, matrix.fe516, matrix.fe517, matrix.fe518, matrix.fe519, matrix.fe520, matrix.fe521, matrix.fe522, matrix.fe523, matrix.fe524, matrix.fe525, 
                    matrix.fe526, matrix.fe527, matrix.fe528, matrix.fe529,matrix.fe530, matrix.fe531, matrix.fe532, matrix.fe533, matrix.fe534, matrix.fe535, matrix.fe536, matrix.fe537, matrix.fe538, matrix.fe539, matrix.fe540,
                    matrix.fe541, matrix.fe542, matrix.fe543, matrix.fe544, matrix.fe545,matrix.fe546, matrix.fe547, matrix.fe548,matrix.fe549, matrix.fe550, matrix.fe551, matrix.fe552, matrix.fe553, matrix.fe554,matrix.fe555,
                    matrix.fe556, matrix.fe557, matrix.fe558, matrix.fe559, matrix.fe560, matrix.fe561, matrix.fe562, matrix.fe563, matrix.fe564, matrix.fe565, matrix.fe566, matrix.fe567, matrix.fe568, matrix.fe569, matrix.fe570, 
                    matrix.fe571, matrix.fe572, matrix.fe573, matrix.fe574,matrix.fe575, matrix.fe576, matrix.fe577, matrix.fe578, matrix.fe579, matrix.fe580, matrix.fe581, matrix.fe582, matrix.fe583, matrix.fe584, matrix.fe585,
                    matrix.fe586, matrix.fe587, matrix.fe588, matrix.fe589, matrix.fe590, matrix.fe591, matrix.fe592, matrix.fe593, matrix.fe594,matrix.fe595, matrix.fe596, matrix.fe597, matrix.fe598, matrix.fe599, matrix.fe600, 
                    matrix.fe601, matrix.fe602, matrix.fe603, matrix.fe604, matrix.fe605, matrix.fe606, matrix.fe607, matrix.fe608, matrix.fe609, matrix.fe610, matrix.fe611, matrix.fe612, matrix.fe613, matrix.fe614,matrix.fe615, 
                    matrix.fe616, matrix.fe617, matrix.fe618, matrix.fe619, matrix.fe620, matrix.fe621, matrix.fe622, matrix.fe623, matrix.fe624, matrix.fe625, matrix.fe626, matrix.fe627, matrix.fe628, matrix.fe629, matrix.fe630,
                    matrix.fe631, matrix.fe632, matrix.fe633, matrix.fe634, matrix.fe635, matrix.fe636, matrix.fe637, matrix.fe638, matrix.fe639, matrix.fe640, matrix.fe641, matrix.fe642, matrix.fe643, matrix.fe644, matrix.fe645, 
                    matrix.fe646, matrix.fe647, matrix.fe648, matrix.fe649, matrix.fe650, matrix.fe651, matrix.fe652, matrix.fe653, matrix.fe654, matrix.fe655, matrix.fe656, matrix.fe657, matrix.fe658, matrix.fe659, matrix.fe660,
                    matrix.fe661, matrix.fe662, matrix.fe663, matrix.fe664, matrix.fe665, matrix.fe666,matrix.fe667, matrix.fe668, matrix.fe669, matrix.fe670, matrix.fe671, matrix.fe672, matrix.fe673, matrix.fe674, matrix.fe675, 
                    matrix.fe676, matrix.fe677, matrix.fe678, matrix.fe679, matrix.fe680, matrix.fe681, matrix.fe682, matrix.fe683, matrix.fe684, matrix.fe685, matrix.fe686, matrix.fe687, matrix.fe688, matrix.fe689, matrix.fe690,
                    matrix.fe691, matrix.fe692, matrix.fe693, matrix.fe694, matrix.fe695, matrix.fe696, matrix.fe697, matrix.fe698, matrix.fe699, matrix.fe700, matrix.fe701, matrix.fe702, matrix.fe703, matrix.fe704, matrix.fe705,
                    matrix.fe706, matrix.fe707, matrix.fe708, matrix.fe709, matrix.fe710, matrix.fe711, matrix.fe712, matrix.fe713, matrix.fe714, matrix.fe715, matrix.fe716, matrix.fe717, matrix.fe718, matrix.fe719, matrix.fe720, 
                    matrix.fe721, matrix.fe722, matrix.fe723, matrix.fe724, matrix.fe725, matrix.fe726, matrix.fe727, matrix.fe728, matrix.fe729, matrix.fe730, matrix.fe731, matrix.fe732, matrix.fe733, matrix.fe734, matrix.fe735,
                    matrix.fe736, matrix.fe737, matrix.fe738, matrix.fe739, matrix.fe740, matrix.fe741, matrix.fe742, matrix.fe743, matrix.fe744, matrix.fe745, matrix.fe746, matrix.fe747, matrix.fe748, matrix.fe749, matrix.fe750,
                    matrix.fe751, matrix.fe752, matrix.fe753, matrix.fe754, matrix.fe755, matrix.fe756, matrix.fe757, matrix.fe758, matrix.fe759, matrix.fe760, matrix.fe761, matrix.fe762, matrix.fe763, matrix.fe764, matrix.fe765,
                    matrix.fe766, matrix.fe767, matrix.fe768, matrix.fe769, matrix.fe770, matrix.fe771, matrix.fe772, matrix.fe773, matrix.fe774, matrix.fe775, matrix.fe776, matrix.fe777, matrix.fe778, matrix.fe779, matrix.fe780,
                    matrix.fe781, matrix.fe782, matrix.fe783, matrix.fe784, matrix.fe785, matrix.fe786, matrix.fe787, matrix.fe788, matrix.fe789, matrix.fe790, matrix.fe791, matrix.fe792, matrix.fe793, matrix.fe794, matrix.fe795,
                    matrix.fe796, matrix.fe797, matrix.fe798, matrix.fe799, matrix.fe800, matrix.fe801, matrix.fe802, matrix.fe803, matrix.fe804, matrix.fe805, matrix.fe806, matrix.fe807, matrix.fe808, matrix.fe809, matrix.fe810,
                    matrix.fe811, matrix.fe812, matrix.fe813
                    
))



###  individual difference scaling with starting position 
#init for starting values
startconf.p <- matrix(data=NA, nrow=6, ncol=2)
startconf.p[,1] <- c(.1, -.1, -.1, -.1, .1, .1) *2
startconf.p[,2]  <- c(-.05, .05, .05, -.05, .05, -.05) *-4
scaling.start.fe <- smacof:::indscal(matrices.fe, init=startconf.p, type="ordinal", itmax=75000)
scaling.start.fe$stress
# 0.1977359

plot.indscal.starting.fe <- as.data.frame(scaling.start.fe$gspace)
plot.indscal.starting.fe$direction <- factor(c("pos","neg","neg",  "neg", "pos", "pos") )
plot.indscal.starting.fe$strength <- factor(c("high","low", "low", "high", "low", "high") )
plot.indscal.starting.fe$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco","Haare föhn vs luft", 
                                           "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))


ggplot(plot.indscal.starting.fe, aes(x=D1, y=D2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + xlim(-.8, .8) +
  ylim(-.7, .7) +
  theme_minimal() +
  geom_text(aes(label = label), vjust=3, size=4)


#### for male ####
data.complexity.ma <- data.complexity.1 %>%  filter(gender=="male")  %>% select(ResponseId, ab:ef) 

data.complexity.ma$fa <- data.complexity.ma$af
data.complexity.ma$ea <- data.complexity.ma$ae
data.complexity.ma$da <- data.complexity.ma$ad
data.complexity.ma$ca <- data.complexity.ma$ac
data.complexity.ma$ba <- data.complexity.ma$ab

data.complexity.ma$fb <- data.complexity.ma$bf
data.complexity.ma$eb <- data.complexity.ma$be
data.complexity.ma$db <- data.complexity.ma$bd
data.complexity.ma$cb <- data.complexity.ma$bc

data.complexity.ma$fc <- data.complexity.ma$cf
data.complexity.ma$ec <- data.complexity.ma$ce
data.complexity.ma$dc <- data.complexity.ma$cd

data.complexity.ma$fd <- data.complexity.ma$df
data.complexity.ma$ed <- data.complexity.ma$de

data.complexity.ma$fe <- data.complexity.ma$ef

data.complexity.ma <- na.omit(data.complexity.ma)
data.complexity.ma.long <- pivot_longer(data.complexity.ma, 2:31, names_to = "verhalten", values_to = "similarity")
data.complexity.ma.long$verhalten2 <- data.complexity.ma.long$verhalten
data.complexity.ma.long$v2 <- substring(data.complexity.ma.long$verhalten2, 2,)
data.complexity.ma.long$v1 <- substring(data.complexity.ma.long$verhalten, 1,1)

data.complexity.ma.long$similarity <- as.numeric(data.complexity.ma.long$similarity) 
data.complexity.ma.long$similarity.2 <- (-(data.complexity.ma.long$similarity) +10)

dfList.ma <- split(data.complexity.ma.long,data.complexity.ma.long$ResponseId)
length(dfList.ma)
names(dfList.ma) <- paste0("id",1:length(dfList.ma))

for(r in 1:length(dfList.ma)) { 
  assign(paste0("matrix.ma",r), NULL )
}


for (r in 1:length(dfList.ma)) { 
  assign(paste0("matrix.ma", r), dfList.ma[[r]] %>% 
           mutate_at(1:2, factor, levels = unique(c(levels(.$v1), levels(.$v2)))) %>%
           xtabs(similarity.2 ~ v1 + v2, data=., sparse = FALSE) ) 
}

#### matrices ####
matrices.ma<- (list(matrix.ma1, matrix.ma2, matrix.ma3,matrix.ma4,matrix.ma5,matrix.ma6,matrix.ma7,matrix.ma8,matrix.ma9,matrix.ma10,matrix.ma11,matrix.ma12,matrix.ma13,matrix.ma14 ,matrix.ma15,
                    matrix.ma16,matrix.ma17,matrix.ma18,matrix.ma19,matrix.ma20,matrix.ma21,matrix.ma22,matrix.ma23,matrix.ma24,matrix.ma25,matrix.ma26,matrix.ma27,matrix.ma28,matrix.ma29,matrix.ma30,
                    matrix.ma31,matrix.ma32,matrix.ma33,matrix.ma34,matrix.ma35,matrix.ma36,matrix.ma37,matrix.ma38,matrix.ma39,matrix.ma40,matrix.ma41,matrix.ma42,matrix.ma43,matrix.ma44,matrix.ma45,
                    matrix.ma46,matrix.ma47,matrix.ma48,matrix.ma49,matrix.ma50,matrix.ma51,matrix.ma52,matrix.ma53,matrix.ma54,matrix.ma55,matrix.ma56,matrix.ma57,matrix.ma58,matrix.ma59,matrix.ma60,
                    matrix.ma61,matrix.ma62,matrix.ma63,matrix.ma64,matrix.ma65,matrix.ma66,matrix.ma67,matrix.ma68,matrix.ma69,matrix.ma70,matrix.ma71,matrix.ma72,matrix.ma73,matrix.ma74,matrix.ma75,
                    matrix.ma76, matrix.ma77,matrix.ma78,matrix.ma79,matrix.ma80,matrix.ma81,matrix.ma82,matrix.ma83,matrix.ma84,matrix.ma85,matrix.ma86,matrix.ma87,matrix.ma88,matrix.ma89,matrix.ma90,
                    matrix.ma91,matrix.ma92,matrix.ma93,matrix.ma94,matrix.ma95,matrix.ma96,matrix.ma97,matrix.ma98,matrix.ma99, matrix.ma100,matrix.ma101,matrix.ma102,matrix.ma103,matrix.ma104,matrix.ma105,
                    matrix.ma106,matrix.ma107,matrix.ma108,matrix.ma109,matrix.ma110,matrix.ma111,matrix.ma112,matrix.ma113,matrix.ma114,matrix.ma115,matrix.ma116,matrix.ma117,matrix.ma118,matrix.ma119,matrix.ma120,
                    matrix.ma121,matrix.ma122,matrix.ma123,matrix.ma124,matrix.ma125,matrix.ma126,matrix.ma127,matrix.ma128,matrix.ma129,matrix.ma130,matrix.ma131,matrix.ma132,matrix.ma133,matrix.ma134,matrix.ma135,
                    matrix.ma136,matrix.ma137,matrix.ma138,matrix.ma139,matrix.ma140,matrix.ma141,matrix.ma142,matrix.ma143,matrix.ma144,matrix.ma145,matrix.ma146,matrix.ma147,matrix.ma148,matrix.ma149,matrix.ma150,
                    matrix.ma151,matrix.ma152,matrix.ma153,matrix.ma154,matrix.ma155,matrix.ma156,matrix.ma157,matrix.ma158,matrix.ma159,matrix.ma160,matrix.ma161,matrix.ma162,matrix.ma163,matrix.ma164,matrix.ma165,
                    matrix.ma166,matrix.ma167,matrix.ma168,matrix.ma169,matrix.ma170,matrix.ma171,matrix.ma172,matrix.ma173,matrix.ma174,matrix.ma175,matrix.ma176,matrix.ma177,matrix.ma178,matrix.ma179,matrix.ma180,
                    matrix.ma181,matrix.ma182,matrix.ma183,matrix.ma184,matrix.ma185,matrix.ma186,matrix.ma187,matrix.ma188,matrix.ma189,matrix.ma190, matrix.ma191, matrix.ma192, matrix.ma193, matrix.ma194, matrix.ma195,
                    matrix.ma196, matrix.ma197, matrix.ma198, matrix.ma199, matrix.ma200, matrix.ma201, matrix.ma202, matrix.ma203, matrix.ma204, matrix.ma205, matrix.ma206, matrix.ma207, matrix.ma208, matrix.ma209, matrix.ma210,
                    matrix.ma211, matrix.ma212 , matrix.ma213, matrix.ma214, matrix.ma215, matrix.ma216, matrix.ma217, matrix.ma218, matrix.ma219, matrix.ma220, matrix.ma221, matrix.ma222, matrix.ma223, matrix.ma224, matrix.ma225,
                    matrix.ma226, matrix.ma227, matrix.ma228, matrix.ma229, matrix.ma230, matrix.ma231, matrix.ma232, matrix.ma233, matrix.ma234, matrix.ma235, matrix.ma236, matrix.ma237, matrix.ma238, matrix.ma239, matrix.ma240,
                    matrix.ma241, matrix.ma242, matrix.ma243, matrix.ma244, matrix.ma245, matrix.ma246, matrix.ma247, matrix.ma248, matrix.ma249, matrix.ma250, matrix.ma251, matrix.ma252, matrix.ma253, matrix.ma254, matrix.ma255,
                    matrix.ma256, matrix.ma257, matrix.ma258,matrix.ma259, matrix.ma260, matrix.ma261, matrix.ma262, matrix.ma263, matrix.ma264, matrix.ma265, matrix.ma266, matrix.ma267, matrix.ma268, matrix.ma269, matrix.ma270, 
                    matrix.ma271, matrix.ma272, matrix.ma273, matrix.ma274,matrix.ma275, matrix.ma276, matrix.ma277, matrix.ma278, matrix.ma279, matrix.ma280, matrix.ma281, matrix.ma282, matrix.ma283, matrix.ma284, matrix.ma285, 
                    matrix.ma286, matrix.ma287, matrix.ma288, matrix.ma289, matrix.ma290,matrix.ma291, matrix.ma292, matrix.ma293, matrix.ma294, matrix.ma295, matrix.ma296, matrix.ma297, matrix.ma298, matrix.ma299, matrix.ma300, 
                    matrix.ma301, matrix.ma302, matrix.ma303, matrix.ma304, matrix.ma305,matrix.ma306, matrix.ma307, matrix.ma308, matrix.ma309, matrix.ma310, matrix.ma311, matrix.ma312, matrix.ma313, matrix.ma314, matrix.ma315, 
                    matrix.ma316, matrix.ma317, matrix.ma318, matrix.ma319, matrix.ma320, matrix.ma321, matrix.ma322, matrix.ma323,matrix.ma324, matrix.ma325, matrix.ma326, matrix.ma327, matrix.ma328, matrix.ma329, matrix.ma330, 
                    matrix.ma331, matrix.ma332, matrix.ma333, matrix.ma334, matrix.ma335, matrix.ma336, matrix.ma337, matrix.ma338, matrix.ma339,matrix.ma340, matrix.ma341, matrix.ma342, matrix.ma343, matrix.ma344, matrix.ma345, 
                    matrix.ma346, matrix.ma347, matrix.ma348, matrix.ma349, matrix.ma350, matrix.ma351, matrix.ma352, matrix.ma353, matrix.ma354, matrix.ma355,matrix.ma356, matrix.ma357, matrix.ma358, matrix.ma359, matrix.ma360, 
                    matrix.ma361, matrix.ma362, matrix.ma363, matrix.ma364, matrix.ma365, matrix.ma366, matrix.ma367, matrix.ma368, matrix.ma369, matrix.ma370, matrix.ma371, matrix.ma372,matrix.ma373, matrix.ma374, matrix.ma375, 
                    matrix.ma376, matrix.ma377, matrix.ma378, matrix.ma379, matrix.ma380, matrix.ma381, matrix.ma382, matrix.ma383, matrix.ma384, matrix.ma385, matrix.ma386, matrix.ma387, matrix.ma388, matrix.ma389,matrix.ma390,
                    matrix.ma391, matrix.ma392, matrix.ma393, matrix.ma394, matrix.ma395, matrix.ma396, matrix.ma397, matrix.ma398, matrix.ma399, matrix.ma400, matrix.ma401, matrix.ma402, matrix.ma403, matrix.ma404, matrix.ma405,
                    matrix.ma406, matrix.ma407, matrix.ma408, matrix.ma409, matrix.ma410, matrix.ma411, matrix.ma412, matrix.ma413, matrix.ma414, matrix.ma415, matrix.ma416, matrix.ma417, matrix.ma418, matrix.ma419, matrix.ma420,
                    matrix.ma421, matrix.ma422, matrix.ma423, matrix.ma424, matrix.ma425, matrix.ma426 , matrix.ma427 , matrix.ma428, matrix.ma429, matrix.ma430, matrix.ma431, matrix.ma432, matrix.ma433, matrix.ma434, matrix.ma435,
                    matrix.ma436, matrix.ma437, matrix.ma438, matrix.ma439, matrix.ma440, matrix.ma441, matrix.ma442, matrix.ma443, matrix.ma444, matrix.ma445, matrix.ma446, matrix.ma447, matrix.ma448, matrix.ma449, matrix.ma450,
                    matrix.ma451, matrix.ma452, matrix.ma453, matrix.ma454, matrix.ma455, matrix.ma456, matrix.ma457, matrix.ma458, matrix.ma459, matrix.ma460, matrix.ma461, matrix.ma462, matrix.ma463 , matrix.ma464, matrix.ma465, 
                    matrix.ma466, matrix.ma467, matrix.ma468, matrix.ma469,matrix.ma470, matrix.ma471, matrix.ma472, matrix.ma473, matrix.ma474, matrix.ma475, matrix.ma476, matrix.ma477, matrix.ma478, matrix.ma479, matrix.ma480, 
                    matrix.ma481, matrix.ma482, matrix.ma483, matrix.ma484,matrix.ma485, matrix.ma486, matrix.ma487, matrix.ma488, matrix.ma489, matrix.ma490, matrix.ma491, matrix.ma492, matrix.ma493, matrix.ma494, matrix.ma495, 
                    matrix.ma496, matrix.ma497, matrix.ma498, matrix.ma499, matrix.ma500, matrix.ma501, matrix.ma502, matrix.ma503, matrix.ma504, matrix.ma505, matrix.ma506, matrix.ma507, matrix.ma508, matrix.ma509, matrix.ma510, 
                    matrix.ma511, matrix.ma512, matrix.ma513, matrix.ma514,matrix.ma515, matrix.ma516, matrix.ma517, matrix.ma518, matrix.ma519, matrix.ma520, matrix.ma521, matrix.ma522, matrix.ma523, matrix.ma524, matrix.ma525, 
                    matrix.ma526, matrix.ma527, matrix.ma528, matrix.ma529,matrix.ma530, matrix.ma531, matrix.ma532, matrix.ma533, matrix.ma534, matrix.ma535, matrix.ma536, matrix.ma537, matrix.ma538, matrix.ma539, matrix.ma540,
                    matrix.ma541, matrix.ma542, matrix.ma543, matrix.ma544, matrix.ma545,matrix.ma546, matrix.ma547, matrix.ma548,matrix.ma549, matrix.ma550, matrix.ma551, matrix.ma552, matrix.ma553, matrix.ma554,matrix.ma555,
                    matrix.ma556, matrix.ma557, matrix.ma558, matrix.ma559, matrix.ma560, matrix.ma561, matrix.ma562, matrix.ma563, matrix.ma564, matrix.ma565, matrix.ma566, matrix.ma567, matrix.ma568, matrix.ma569, matrix.ma570, 
                    matrix.ma571, matrix.ma572, matrix.ma573, matrix.ma574,matrix.ma575, matrix.ma576, matrix.ma577, matrix.ma578, matrix.ma579, matrix.ma580, matrix.ma581, matrix.ma582, matrix.ma583, matrix.ma584, matrix.ma585,
                    matrix.ma586, matrix.ma587, matrix.ma588, matrix.ma589, matrix.ma590, matrix.ma591, matrix.ma592, matrix.ma593, matrix.ma594,matrix.ma595, matrix.ma596, matrix.ma597, matrix.ma598, matrix.ma599, matrix.ma600, 
                    matrix.ma601, matrix.ma602, matrix.ma603, matrix.ma604, matrix.ma605, matrix.ma606, matrix.ma607, matrix.ma608, matrix.ma609, matrix.ma610, matrix.ma611, matrix.ma612, matrix.ma613, matrix.ma614,matrix.ma615, 
                    matrix.ma616, matrix.ma617, matrix.ma618, matrix.ma619, matrix.ma620, matrix.ma621, matrix.ma622, matrix.ma623, matrix.ma624, matrix.ma625, matrix.ma626, matrix.ma627, matrix.ma628, matrix.ma629, matrix.ma630,
                    matrix.ma631, matrix.ma632, matrix.ma633, matrix.ma634, matrix.ma635, matrix.ma636, matrix.ma637, matrix.ma638, matrix.ma639, matrix.ma640, matrix.ma641, matrix.ma642, matrix.ma643, matrix.ma644, matrix.ma645, 
                    matrix.ma646, matrix.ma647, matrix.ma648, matrix.ma649, matrix.ma650, matrix.ma651, matrix.ma652, matrix.ma653, matrix.ma654, matrix.ma655, matrix.ma656, matrix.ma657, matrix.ma658, matrix.ma659, matrix.ma660,
                    matrix.ma661, matrix.ma662, matrix.ma663, matrix.ma664, matrix.ma665, matrix.ma666,matrix.ma667, matrix.ma668, matrix.ma669, matrix.ma670, matrix.ma671, matrix.ma672, matrix.ma673, matrix.ma674, matrix.ma675, 
                    matrix.ma676, matrix.ma677, matrix.ma678, matrix.ma679, matrix.ma680, matrix.ma681, matrix.ma682, matrix.ma683, matrix.ma684, matrix.ma685, matrix.ma686, matrix.ma687, matrix.ma688, matrix.ma689, matrix.ma690,
                    matrix.ma691, matrix.ma692, matrix.ma693, matrix.ma694, matrix.ma695, matrix.ma696, matrix.ma697, matrix.ma698, matrix.ma699, matrix.ma700, matrix.ma701, matrix.ma702, matrix.ma703, matrix.ma704, matrix.ma705,
                    matrix.ma706, matrix.ma707, matrix.ma708, matrix.ma709, matrix.ma710, matrix.ma711, matrix.ma712, matrix.ma713, matrix.ma714, matrix.ma715, matrix.ma716, matrix.ma717, matrix.ma718, matrix.ma719, matrix.ma720, 
                    matrix.ma721, matrix.ma722, matrix.ma723, matrix.ma724, matrix.ma725, matrix.ma726, matrix.ma727, matrix.ma728, matrix.ma729, matrix.ma730, matrix.ma731, matrix.ma732, matrix.ma733, matrix.ma734, matrix.ma735,
                    matrix.ma736, matrix.ma737, matrix.ma738, matrix.ma739, matrix.ma740, matrix.ma741, matrix.ma742, matrix.ma743, matrix.ma744, matrix.ma745, matrix.ma746, matrix.ma747, matrix.ma748, matrix.ma749, matrix.ma750,
                    matrix.ma751, matrix.ma752, matrix.ma753, matrix.ma754, matrix.ma755, matrix.ma756, matrix.ma757, matrix.ma758, matrix.ma759, matrix.ma760, matrix.ma761, matrix.ma762, matrix.ma763, matrix.ma764, matrix.ma765,
                    matrix.ma766, matrix.ma767, matrix.ma768, matrix.ma769, matrix.ma770, matrix.ma771, matrix.ma772, matrix.ma773, matrix.ma774, matrix.ma775, matrix.ma776, matrix.ma777, matrix.ma778, matrix.ma779, matrix.ma780,
                    matrix.ma781, matrix.ma782, matrix.ma783, matrix.ma784, matrix.ma785, matrix.ma786, matrix.ma787, matrix.ma788, matrix.ma789, matrix.ma790, matrix.ma791, matrix.ma792, matrix.ma793, matrix.ma794, matrix.ma795,
                    matrix.ma796, matrix.ma797, matrix.ma798, matrix.ma799, matrix.ma800, matrix.ma801, matrix.ma802, matrix.ma803, matrix.ma804, matrix.ma805, matrix.ma806, matrix.ma807, matrix.ma808, matrix.ma809, matrix.ma810,
                    matrix.ma811, matrix.ma812, matrix.ma813, matrix.ma814, matrix.ma815
                    
))

###  individual difference scaling with starting position 
#init for starting values
scaling.start.ma <- smacof:::indscal(matrices.ma, init=startconf.p, type="ordinal", itmax=75000)
scaling.start.ma$stress
#0.1942585

plot.indscal.starting.ma <- as.data.frame(scaling.start.ma$gspace)
plot.indscal.starting.ma$direction <- factor(c("pos","neg","neg",  "neg", "pos", "pos") )
plot.indscal.starting.ma$strength <- factor(c("high","low", "low", "high", "low", "high") )
plot.indscal.starting.ma$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco","Haare föhn vs luft", 
                                           "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))


ggplot(plot.indscal.starting.ma, aes(x=D1, y=D2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + xlim(-.8, .8) +
  ylim(-.7, .7) +
  theme_minimal() +
  geom_text(aes(label = label), vjust=3, size=4)


#### procrustes transformation etc. to compare country and gender#####

fit.proc.c <- Procrustes(scaling.start.de$gspace, scaling.start.ch$gspace)
fit.proc.c

plot(fit.proc.c, plot.type = "jointplot", plot.dim = c(1,2),
     xlab= "impact direction", ylab ="impact strength", 
    legend = list (labels = c("Germany", "Switzerland") ) )

fit.proc.g <- smacof::Procrustes(scaling.start.ma$gspace, scaling.start.fe$gspace)
fit.proc.g
# from vegan package
plot(fit.proc.g, plot.type = "jointplot", plot.dim = c(1,2) ,
     xlab= "impact direction", ylab ="impact strength", 
     legend = list (labels = c("male", "female") ) )


##### by education classes #######

#### up to obligatory school (includes no formal education) ####

data.complexity.low.e <- data.complexity.1 %>%  
  filter(education=="no formal education" | education=="obligatory school") %>% 
  select(ResponseId, ab:ef) 

data.complexity.low.e$fa <- data.complexity.low.e$af
data.complexity.low.e$ea <- data.complexity.low.e$ae
data.complexity.low.e$da <- data.complexity.low.e$ad
data.complexity.low.e$ca <- data.complexity.low.e$ac
data.complexity.low.e$ba <- data.complexity.low.e$ab

data.complexity.low.e$fb <- data.complexity.low.e$bf
data.complexity.low.e$eb <- data.complexity.low.e$be
data.complexity.low.e$db <- data.complexity.low.e$bd
data.complexity.low.e$cb <- data.complexity.low.e$bc

data.complexity.low.e$fc <- data.complexity.low.e$cf
data.complexity.low.e$ec <- data.complexity.low.e$ce
data.complexity.low.e$dc <- data.complexity.low.e$cd

data.complexity.low.e$fd <- data.complexity.low.e$df
data.complexity.low.e$ed <- data.complexity.low.e$de

data.complexity.low.e$fe <- data.complexity.low.e$ef

data.complexity.low.e <- na.omit(data.complexity.low.e)
data.complexity.low.e.long <- pivot_longer(data.complexity.low.e, 2:31, names_to = "verhalten", values_to = "similarity")
data.complexity.low.e.long$verhalten2 <- data.complexity.low.e.long$verhalten
data.complexity.low.e.long$v2 <- substring(data.complexity.low.e.long$verhalten2, 2,)
data.complexity.low.e.long$v1 <- substring(data.complexity.low.e.long$verhalten, 1,1)

data.complexity.low.e.long$similarity <- as.numeric(data.complexity.low.e.long$similarity) 
data.complexity.low.e.long$similarity.2 <- (-(data.complexity.low.e.long$similarity) +10)

dfList.low.e <- split(data.complexity.low.e.long,data.complexity.low.e.long$ResponseId)
length(dfList.low.e)
names(dfList.low.e) <- paste0("id",1:length(dfList.low.e))

for(r in 1:length(dfList.low.e)) { 
  assign(paste0("matrix.low.e",r), NULL )
}


for (r in 1:length(dfList.low.e)) { 
  assign(paste0("matrix.low.e", r), dfList.low.e[[r]] %>% 
           mutate_at(1:2, factor, levels = unique(c(levels(.$v1), levels(.$v2)))) %>%
           xtabs(similarity.2 ~ v1 + v2, data=., sparse = FALSE) ) 
}

#### matrices ####
matrices.low.e<- (list(matrix.low.e1, matrix.low.e2, matrix.low.e3,matrix.low.e4,matrix.low.e5,matrix.low.e6,matrix.low.e7,matrix.low.e8,matrix.low.e9,matrix.low.e10,matrix.low.e11,matrix.low.e12,matrix.low.e13,matrix.low.e14 ,matrix.low.e15,
                    matrix.low.e16,matrix.low.e17,matrix.low.e18,matrix.low.e19,matrix.low.e20,matrix.low.e21,matrix.low.e22,matrix.low.e23,matrix.low.e24,matrix.low.e25,matrix.low.e26,matrix.low.e27,matrix.low.e28,matrix.low.e29,matrix.low.e30,
                    matrix.low.e31,matrix.low.e32,matrix.low.e33,matrix.low.e34,matrix.low.e35,matrix.low.e36,matrix.low.e37,matrix.low.e38,matrix.low.e39,matrix.low.e40,matrix.low.e41,matrix.low.e42,matrix.low.e43,matrix.low.e44,matrix.low.e45,
                    matrix.low.e46,matrix.low.e47,matrix.low.e48,matrix.low.e49,matrix.low.e50,matrix.low.e51,matrix.low.e52,matrix.low.e53,matrix.low.e54,matrix.low.e55,matrix.low.e56,matrix.low.e57,matrix.low.e58,matrix.low.e59,matrix.low.e60,
                    matrix.low.e61,matrix.low.e62,matrix.low.e63,matrix.low.e64,matrix.low.e65,matrix.low.e66,matrix.low.e67,matrix.low.e68,matrix.low.e69,matrix.low.e70,matrix.low.e71,matrix.low.e72,matrix.low.e73,matrix.low.e74,matrix.low.e75,
                    matrix.low.e76, matrix.low.e77,matrix.low.e78,matrix.low.e79,matrix.low.e80,matrix.low.e81,matrix.low.e82,matrix.low.e83,matrix.low.e84,matrix.low.e85,matrix.low.e86,matrix.low.e87,matrix.low.e88,matrix.low.e89,matrix.low.e90,
                    matrix.low.e91,matrix.low.e92,matrix.low.e93,matrix.low.e94,matrix.low.e95,matrix.low.e96,matrix.low.e97,matrix.low.e98,matrix.low.e99, matrix.low.e100,matrix.low.e101,matrix.low.e102,matrix.low.e103,matrix.low.e104,matrix.low.e105,
                    matrix.low.e106,matrix.low.e107,matrix.low.e108,matrix.low.e109,matrix.low.e110,matrix.low.e111,matrix.low.e112,matrix.low.e113,matrix.low.e114,matrix.low.e115,matrix.low.e116,matrix.low.e117,matrix.low.e118,matrix.low.e119,matrix.low.e120,
                    matrix.low.e121,matrix.low.e122,matrix.low.e123,matrix.low.e124,matrix.low.e125,matrix.low.e126,matrix.low.e127,matrix.low.e128,matrix.low.e129,matrix.low.e130,matrix.low.e131,matrix.low.e132,matrix.low.e133,matrix.low.e134,matrix.low.e135,
                    matrix.low.e136,matrix.low.e137,matrix.low.e138,matrix.low.e139,matrix.low.e140,matrix.low.e141,matrix.low.e142,matrix.low.e143,matrix.low.e144,matrix.low.e145,matrix.low.e146,matrix.low.e147,matrix.low.e148,matrix.low.e149,matrix.low.e150,
                    matrix.low.e151,matrix.low.e152,matrix.low.e153,matrix.low.e154,matrix.low.e155,matrix.low.e156,matrix.low.e157,matrix.low.e158,matrix.low.e159,matrix.low.e160,matrix.low.e161,matrix.low.e162,matrix.low.e163,matrix.low.e164,matrix.low.e165,
                    matrix.low.e166,matrix.low.e167,matrix.low.e168,matrix.low.e169,matrix.low.e170,matrix.low.e171,matrix.low.e172,matrix.low.e173,matrix.low.e174,matrix.low.e175,matrix.low.e176,matrix.low.e177,matrix.low.e178,matrix.low.e179,matrix.low.e180,
                    matrix.low.e181,matrix.low.e182,matrix.low.e183,matrix.low.e184,matrix.low.e185,matrix.low.e186,matrix.low.e187,matrix.low.e188,matrix.low.e189,matrix.low.e190, matrix.low.e191, matrix.low.e192, matrix.low.e193, matrix.low.e194, matrix.low.e195,
                    matrix.low.e196, matrix.low.e197, matrix.low.e198, matrix.low.e199, matrix.low.e200, matrix.low.e201, matrix.low.e202, matrix.low.e203, matrix.low.e204, matrix.low.e205, matrix.low.e206, matrix.low.e207, matrix.low.e208, matrix.low.e209, matrix.low.e210,
                    matrix.low.e211, matrix.low.e212 , matrix.low.e213, matrix.low.e214, matrix.low.e215, matrix.low.e216, matrix.low.e217, matrix.low.e218, matrix.low.e219, matrix.low.e220, matrix.low.e221, matrix.low.e222, matrix.low.e223, matrix.low.e224, matrix.low.e225,
                    matrix.low.e226, matrix.low.e227, matrix.low.e228, matrix.low.e229, matrix.low.e230, matrix.low.e231, matrix.low.e232, matrix.low.e233, matrix.low.e234, matrix.low.e235, matrix.low.e236, matrix.low.e237, matrix.low.e238, matrix.low.e239, matrix.low.e240,
                    matrix.low.e241, matrix.low.e242, matrix.low.e243, matrix.low.e244, matrix.low.e245, matrix.low.e246, matrix.low.e247, matrix.low.e248, matrix.low.e249, matrix.low.e250, matrix.low.e251, matrix.low.e252, matrix.low.e253, matrix.low.e254, matrix.low.e255,
                    matrix.low.e256, matrix.low.e257, matrix.low.e258,matrix.low.e259, matrix.low.e260, matrix.low.e261, matrix.low.e262, matrix.low.e263, matrix.low.e264, matrix.low.e265, matrix.low.e266, matrix.low.e267, matrix.low.e268, matrix.low.e269, matrix.low.e270, 
                    matrix.low.e271, matrix.low.e272, matrix.low.e273, matrix.low.e274,matrix.low.e275, matrix.low.e276, matrix.low.e277, matrix.low.e278, matrix.low.e279, matrix.low.e280, matrix.low.e281, matrix.low.e282, matrix.low.e283, matrix.low.e284, matrix.low.e285, 
                    matrix.low.e286, matrix.low.e287, matrix.low.e288, matrix.low.e289, matrix.low.e290,matrix.low.e291, matrix.low.e292, matrix.low.e293, matrix.low.e294, matrix.low.e295, matrix.low.e296, matrix.low.e297, matrix.low.e298, matrix.low.e299, matrix.low.e300, 
                    matrix.low.e301, matrix.low.e302, matrix.low.e303, matrix.low.e304, matrix.low.e305,matrix.low.e306, matrix.low.e307, matrix.low.e308, matrix.low.e309, matrix.low.e310, matrix.low.e311, matrix.low.e312, matrix.low.e313, matrix.low.e314, matrix.low.e315, 
                    matrix.low.e316, matrix.low.e317, matrix.low.e318, matrix.low.e319, matrix.low.e320, matrix.low.e321, matrix.low.e322, matrix.low.e323,matrix.low.e324, matrix.low.e325, matrix.low.e326, matrix.low.e327, matrix.low.e328, matrix.low.e329, matrix.low.e330, 
                    matrix.low.e331, matrix.low.e332, matrix.low.e333, matrix.low.e334, matrix.low.e335, matrix.low.e336, matrix.low.e337, matrix.low.e338, matrix.low.e339,matrix.low.e340, matrix.low.e341, matrix.low.e342, matrix.low.e343, matrix.low.e344, matrix.low.e345, 
                    matrix.low.e346, matrix.low.e347, matrix.low.e348, matrix.low.e349, matrix.low.e350, matrix.low.e351, matrix.low.e352, matrix.low.e353, matrix.low.e354, matrix.low.e355,matrix.low.e356, matrix.low.e357, matrix.low.e358, matrix.low.e359, matrix.low.e360, 
                    matrix.low.e361, matrix.low.e362, matrix.low.e363, matrix.low.e364, matrix.low.e365, matrix.low.e366, matrix.low.e367, matrix.low.e368, matrix.low.e369, matrix.low.e370, matrix.low.e371, matrix.low.e372,matrix.low.e373, matrix.low.e374, matrix.low.e375, 
                    matrix.low.e376, matrix.low.e377, matrix.low.e378, matrix.low.e379, matrix.low.e380, matrix.low.e381, matrix.low.e382, matrix.low.e383, matrix.low.e384, matrix.low.e385, matrix.low.e386, matrix.low.e387, matrix.low.e388, matrix.low.e389,matrix.low.e390,
                    matrix.low.e391, matrix.low.e392, matrix.low.e393, matrix.low.e394, matrix.low.e395, matrix.low.e396, matrix.low.e397, matrix.low.e398, matrix.low.e399, matrix.low.e400, matrix.low.e401, matrix.low.e402, matrix.low.e403, matrix.low.e404, matrix.low.e405,
                    matrix.low.e406, matrix.low.e407, matrix.low.e408, matrix.low.e409, matrix.low.e410, matrix.low.e411, matrix.low.e412, matrix.low.e413, matrix.low.e414, matrix.low.e415, matrix.low.e416, matrix.low.e417, matrix.low.e418, matrix.low.e419, matrix.low.e420,
                    matrix.low.e421, matrix.low.e422, matrix.low.e423, matrix.low.e424, matrix.low.e425, matrix.low.e426 , matrix.low.e427 , matrix.low.e428, matrix.low.e429, matrix.low.e430, matrix.low.e431, matrix.low.e432, matrix.low.e433, matrix.low.e434, matrix.low.e435,
                    matrix.low.e436, matrix.low.e437, matrix.low.e438, matrix.low.e439, matrix.low.e440, matrix.low.e441, matrix.low.e442, matrix.low.e443, matrix.low.e444, matrix.low.e445, matrix.low.e446, matrix.low.e447, matrix.low.e448, matrix.low.e449, matrix.low.e450,
                    matrix.low.e451, matrix.low.e452, matrix.low.e453, matrix.low.e454, matrix.low.e455, matrix.low.e456, matrix.low.e457, matrix.low.e458, matrix.low.e459, matrix.low.e460, matrix.low.e461, matrix.low.e462, matrix.low.e463 , matrix.low.e464, matrix.low.e465, 
                    matrix.low.e466, matrix.low.e467, matrix.low.e468, matrix.low.e469,matrix.low.e470, matrix.low.e471, matrix.low.e472, matrix.low.e473, matrix.low.e474, matrix.low.e475, matrix.low.e476, matrix.low.e477, matrix.low.e478, matrix.low.e479, matrix.low.e480, 
                    matrix.low.e481, matrix.low.e482, matrix.low.e483, matrix.low.e484,matrix.low.e485, matrix.low.e486, matrix.low.e487, matrix.low.e488, matrix.low.e489, matrix.low.e490, matrix.low.e491, matrix.low.e492, matrix.low.e493, matrix.low.e494, matrix.low.e495, 
                    matrix.low.e496, matrix.low.e497, matrix.low.e498, matrix.low.e499, matrix.low.e500, matrix.low.e501, matrix.low.e502, matrix.low.e503, matrix.low.e504, matrix.low.e505, matrix.low.e506, matrix.low.e507, matrix.low.e508, matrix.low.e509, matrix.low.e510, 
                    matrix.low.e511, matrix.low.e512, matrix.low.e513, matrix.low.e514,matrix.low.e515, matrix.low.e516, matrix.low.e517, matrix.low.e518, matrix.low.e519, matrix.low.e520, matrix.low.e521, matrix.low.e522, matrix.low.e523, matrix.low.e524, matrix.low.e525, 
                    matrix.low.e526, matrix.low.e527, matrix.low.e528, matrix.low.e529,matrix.low.e530, matrix.low.e531, matrix.low.e532, matrix.low.e533, matrix.low.e534, matrix.low.e535, matrix.low.e536, matrix.low.e537, matrix.low.e538, matrix.low.e539, matrix.low.e540,
                    matrix.low.e541, matrix.low.e542, matrix.low.e543, matrix.low.e544, matrix.low.e545,matrix.low.e546, matrix.low.e547, matrix.low.e548,matrix.low.e549, matrix.low.e550, matrix.low.e551, matrix.low.e552, matrix.low.e553, matrix.low.e554,matrix.low.e555,
                    matrix.low.e556, matrix.low.e557, matrix.low.e558, matrix.low.e559, matrix.low.e560, matrix.low.e561, matrix.low.e562, matrix.low.e563, matrix.low.e564, matrix.low.e565, matrix.low.e566, matrix.low.e567, matrix.low.e568, matrix.low.e569, matrix.low.e570, 
                    matrix.low.e571, matrix.low.e572, matrix.low.e573, matrix.low.e574,matrix.low.e575, matrix.low.e576, matrix.low.e577, matrix.low.e578, matrix.low.e579, matrix.low.e580, matrix.low.e581, matrix.low.e582, matrix.low.e583, matrix.low.e584, matrix.low.e585,
                    matrix.low.e586, matrix.low.e587, matrix.low.e588, matrix.low.e589, matrix.low.e590, matrix.low.e591, matrix.low.e592, matrix.low.e593, matrix.low.e594,matrix.low.e595, matrix.low.e596, matrix.low.e597, matrix.low.e598, matrix.low.e599, matrix.low.e600, 
                    matrix.low.e601, matrix.low.e602, matrix.low.e603, matrix.low.e604, matrix.low.e605, matrix.low.e606, matrix.low.e607, matrix.low.e608, matrix.low.e609, matrix.low.e610, matrix.low.e611, matrix.low.e612, matrix.low.e613, matrix.low.e614,matrix.low.e615, 
                    matrix.low.e616, matrix.low.e617, matrix.low.e618, matrix.low.e619, matrix.low.e620, matrix.low.e621, matrix.low.e622, matrix.low.e623, matrix.low.e624, matrix.low.e625, matrix.low.e626, matrix.low.e627, matrix.low.e628, matrix.low.e629, matrix.low.e630,
                    matrix.low.e631, matrix.low.e632, matrix.low.e633, matrix.low.e634, matrix.low.e635, matrix.low.e636, matrix.low.e637, matrix.low.e638, matrix.low.e639, matrix.low.e640, matrix.low.e641, matrix.low.e642, matrix.low.e643, matrix.low.e644, matrix.low.e645, 
                    matrix.low.e646, matrix.low.e647, matrix.low.e648, matrix.low.e649, matrix.low.e650, matrix.low.e651, matrix.low.e652, matrix.low.e653, matrix.low.e654, matrix.low.e655, matrix.low.e656, matrix.low.e657, matrix.low.e658, matrix.low.e659, matrix.low.e660,
                    matrix.low.e661, matrix.low.e662, matrix.low.e663, matrix.low.e664, matrix.low.e665, matrix.low.e666,matrix.low.e667, matrix.low.e668, matrix.low.e669, matrix.low.e670, matrix.low.e671, matrix.low.e672, matrix.low.e673, matrix.low.e674, matrix.low.e675, 
                    matrix.low.e676, matrix.low.e677, matrix.low.e678, matrix.low.e679, matrix.low.e680, matrix.low.e681, matrix.low.e682, matrix.low.e683, matrix.low.e684, matrix.low.e685, matrix.low.e686, matrix.low.e687, matrix.low.e688, matrix.low.e689, matrix.low.e690,
                    matrix.low.e691, matrix.low.e692, matrix.low.e693, matrix.low.e694, matrix.low.e695, matrix.low.e696, matrix.low.e697, matrix.low.e698, matrix.low.e699, matrix.low.e700, matrix.low.e701, matrix.low.e702, matrix.low.e703, matrix.low.e704, matrix.low.e705,
                    matrix.low.e706, matrix.low.e707, matrix.low.e708, matrix.low.e709, matrix.low.e710, matrix.low.e711, matrix.low.e712, matrix.low.e713, matrix.low.e714, matrix.low.e715, matrix.low.e716, matrix.low.e717, matrix.low.e718, matrix.low.e719, matrix.low.e720, 
                    matrix.low.e721, matrix.low.e722, matrix.low.e723, matrix.low.e724, matrix.low.e725, matrix.low.e726, matrix.low.e727, matrix.low.e728, matrix.low.e729, matrix.low.e730, matrix.low.e731, matrix.low.e732, matrix.low.e733, matrix.low.e734, matrix.low.e735,
                    matrix.low.e736, matrix.low.e737, matrix.low.e738, matrix.low.e739, matrix.low.e740, matrix.low.e741, matrix.low.e742, matrix.low.e743, matrix.low.e744, matrix.low.e745, matrix.low.e746, matrix.low.e747, matrix.low.e748, matrix.low.e749, matrix.low.e750,
                    matrix.low.e751, matrix.low.e752, matrix.low.e753, matrix.low.e754, matrix.low.e755, matrix.low.e756, matrix.low.e757
))


###  individual difference scaling with starting position 
#init for starting values
scaling.start.low.e <- smacof:::indscal(matrices.low.e, init=startconf.p, type="ordinal", itmax=75000)
scaling.start.low.e$stress
# 0.1939013

plot.indscal.starting.low.e <- as.data.frame(scaling.start.low.e$gspace)
plot.indscal.starting.low.e$direction <- factor(c("pos","neg","neg",  "neg", "pos", "pos") )
plot.indscal.starting.low.e$strength <- factor(c("high","low", "low", "high", "low", "high") )
plot.indscal.starting.low.e$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco","Haare föhn vs luft", 
                                           "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))


ggplot(plot.indscal.starting.low.e, aes(x=D1, y=D2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + xlim(-.8, .8) +
  ylim(-.7, .7) +
  theme_minimal() +
  geom_text(aes(label = label), vjust=3, size=4)


### at least middle school, high school or degree ####

data.complexity.high.e <- data.complexity.1 %>%  
  filter(!education=="no formal education" ) %>% filter(!education=="obligatory school") %>% 
  select(ResponseId, ab:ef) 

data.complexity.high.e$fa <- data.complexity.high.e$af
data.complexity.high.e$ea <- data.complexity.high.e$ae
data.complexity.high.e$da <- data.complexity.high.e$ad
data.complexity.high.e$ca <- data.complexity.high.e$ac
data.complexity.high.e$ba <- data.complexity.high.e$ab

data.complexity.high.e$fb <- data.complexity.high.e$bf
data.complexity.high.e$eb <- data.complexity.high.e$be
data.complexity.high.e$db <- data.complexity.high.e$bd
data.complexity.high.e$cb <- data.complexity.high.e$bc

data.complexity.high.e$fc <- data.complexity.high.e$cf
data.complexity.high.e$ec <- data.complexity.high.e$ce
data.complexity.high.e$dc <- data.complexity.high.e$cd

data.complexity.high.e$fd <- data.complexity.high.e$df
data.complexity.high.e$ed <- data.complexity.high.e$de

data.complexity.high.e$fe <- data.complexity.high.e$ef

data.complexity.high.e <- na.omit(data.complexity.high.e)
data.complexity.high.e.long <- pivot_longer(data.complexity.high.e, 2:31, names_to = "verhalten", values_to = "similarity")
data.complexity.high.e.long$verhalten2 <- data.complexity.high.e.long$verhalten
data.complexity.high.e.long$v2 <- substring(data.complexity.high.e.long$verhalten2, 2,)
data.complexity.high.e.long$v1 <- substring(data.complexity.high.e.long$verhalten, 1,1)

data.complexity.high.e.long$similarity <- as.numeric(data.complexity.high.e.long$similarity) 
data.complexity.high.e.long$similarity.2 <- (-(data.complexity.high.e.long$similarity) +10)

dfList.high.e <- split(data.complexity.high.e.long,data.complexity.high.e.long$ResponseId)
length(dfList.high.e)
names(dfList.high.e) <- paste0("id",1:length(dfList.high.e))

for(r in 1:length(dfList.high.e)) { 
  assign(paste0("matrix.high.e",r), NULL )
}


for (r in 1:length(dfList.high.e)) { 
  assign(paste0("matrix.high.e", r), dfList.high.e[[r]] %>% 
           mutate_at(1:2, factor, levels = unique(c(levels(.$v1), levels(.$v2)))) %>%
           xtabs(similarity.2 ~ v1 + v2, data=., sparse = FALSE) ) 
}

#### matrices ####
matrices.high.e<- (list(matrix.high.e1, matrix.high.e2, matrix.high.e3,matrix.high.e4,matrix.high.e5,matrix.high.e6,matrix.high.e7,matrix.high.e8,matrix.high.e9,matrix.high.e10,matrix.high.e11,matrix.high.e12,matrix.high.e13,matrix.high.e14 ,matrix.high.e15,
                       matrix.high.e16,matrix.high.e17,matrix.high.e18,matrix.high.e19,matrix.high.e20,matrix.high.e21,matrix.high.e22,matrix.high.e23,matrix.high.e24,matrix.high.e25,matrix.high.e26,matrix.high.e27,matrix.high.e28,matrix.high.e29,matrix.high.e30,
                       matrix.high.e31,matrix.high.e32,matrix.high.e33,matrix.high.e34,matrix.high.e35,matrix.high.e36,matrix.high.e37,matrix.high.e38,matrix.high.e39,matrix.high.e40,matrix.high.e41,matrix.high.e42,matrix.high.e43,matrix.high.e44,matrix.high.e45,
                       matrix.high.e46,matrix.high.e47,matrix.high.e48,matrix.high.e49,matrix.high.e50,matrix.high.e51,matrix.high.e52,matrix.high.e53,matrix.high.e54,matrix.high.e55,matrix.high.e56,matrix.high.e57,matrix.high.e58,matrix.high.e59,matrix.high.e60,
                       matrix.high.e61,matrix.high.e62,matrix.high.e63,matrix.high.e64,matrix.high.e65,matrix.high.e66,matrix.high.e67,matrix.high.e68,matrix.high.e69,matrix.high.e70,matrix.high.e71,matrix.high.e72,matrix.high.e73,matrix.high.e74,matrix.high.e75,
                       matrix.high.e76, matrix.high.e77,matrix.high.e78,matrix.high.e79,matrix.high.e80,matrix.high.e81,matrix.high.e82,matrix.high.e83,matrix.high.e84,matrix.high.e85,matrix.high.e86,matrix.high.e87,matrix.high.e88,matrix.high.e89,matrix.high.e90,
                       matrix.high.e91,matrix.high.e92,matrix.high.e93,matrix.high.e94,matrix.high.e95,matrix.high.e96,matrix.high.e97,matrix.high.e98,matrix.high.e99, matrix.high.e100,matrix.high.e101,matrix.high.e102,matrix.high.e103,matrix.high.e104,matrix.high.e105,
                       matrix.high.e106,matrix.high.e107,matrix.high.e108,matrix.high.e109,matrix.high.e110,matrix.high.e111,matrix.high.e112,matrix.high.e113,matrix.high.e114,matrix.high.e115,matrix.high.e116,matrix.high.e117,matrix.high.e118,matrix.high.e119,matrix.high.e120,
                       matrix.high.e121,matrix.high.e122,matrix.high.e123,matrix.high.e124,matrix.high.e125,matrix.high.e126,matrix.high.e127,matrix.high.e128,matrix.high.e129,matrix.high.e130,matrix.high.e131,matrix.high.e132,matrix.high.e133,matrix.high.e134,matrix.high.e135,
                       matrix.high.e136,matrix.high.e137,matrix.high.e138,matrix.high.e139,matrix.high.e140,matrix.high.e141,matrix.high.e142,matrix.high.e143,matrix.high.e144,matrix.high.e145,matrix.high.e146,matrix.high.e147,matrix.high.e148,matrix.high.e149,matrix.high.e150,
                       matrix.high.e151,matrix.high.e152,matrix.high.e153,matrix.high.e154,matrix.high.e155,matrix.high.e156,matrix.high.e157,matrix.high.e158,matrix.high.e159,matrix.high.e160,matrix.high.e161,matrix.high.e162,matrix.high.e163,matrix.high.e164,matrix.high.e165,
                       matrix.high.e166,matrix.high.e167,matrix.high.e168,matrix.high.e169,matrix.high.e170,matrix.high.e171,matrix.high.e172,matrix.high.e173,matrix.high.e174,matrix.high.e175,matrix.high.e176,matrix.high.e177,matrix.high.e178,matrix.high.e179,matrix.high.e180,
                       matrix.high.e181,matrix.high.e182,matrix.high.e183,matrix.high.e184,matrix.high.e185,matrix.high.e186,matrix.high.e187,matrix.high.e188,matrix.high.e189,matrix.high.e190, matrix.high.e191, matrix.high.e192, matrix.high.e193, matrix.high.e194, matrix.high.e195,
                       matrix.high.e196, matrix.high.e197, matrix.high.e198, matrix.high.e199, matrix.high.e200, matrix.high.e201, matrix.high.e202, matrix.high.e203, matrix.high.e204, matrix.high.e205, matrix.high.e206, matrix.high.e207, matrix.high.e208, matrix.high.e209, matrix.high.e210,
                       matrix.high.e211, matrix.high.e212 , matrix.high.e213, matrix.high.e214, matrix.high.e215, matrix.high.e216, matrix.high.e217, matrix.high.e218, matrix.high.e219, matrix.high.e220, matrix.high.e221, matrix.high.e222, matrix.high.e223, matrix.high.e224, matrix.high.e225,
                       matrix.high.e226, matrix.high.e227, matrix.high.e228, matrix.high.e229, matrix.high.e230, matrix.high.e231, matrix.high.e232, matrix.high.e233, matrix.high.e234, matrix.high.e235, matrix.high.e236, matrix.high.e237, matrix.high.e238, matrix.high.e239, matrix.high.e240,
                       matrix.high.e241, matrix.high.e242, matrix.high.e243, matrix.high.e244, matrix.high.e245, matrix.high.e246, matrix.high.e247, matrix.high.e248, matrix.high.e249, matrix.high.e250, matrix.high.e251, matrix.high.e252, matrix.high.e253, matrix.high.e254, matrix.high.e255,
                       matrix.high.e256, matrix.high.e257, matrix.high.e258,matrix.high.e259, matrix.high.e260, matrix.high.e261, matrix.high.e262, matrix.high.e263, matrix.high.e264, matrix.high.e265, matrix.high.e266, matrix.high.e267, matrix.high.e268, matrix.high.e269, matrix.high.e270, 
                       matrix.high.e271, matrix.high.e272, matrix.high.e273, matrix.high.e274,matrix.high.e275, matrix.high.e276, matrix.high.e277, matrix.high.e278, matrix.high.e279, matrix.high.e280, matrix.high.e281, matrix.high.e282, matrix.high.e283, matrix.high.e284, matrix.high.e285, 
                       matrix.high.e286, matrix.high.e287, matrix.high.e288, matrix.high.e289, matrix.high.e290,matrix.high.e291, matrix.high.e292, matrix.high.e293, matrix.high.e294, matrix.high.e295, matrix.high.e296, matrix.high.e297, matrix.high.e298, matrix.high.e299, matrix.high.e300, 
                       matrix.high.e301, matrix.high.e302, matrix.high.e303, matrix.high.e304, matrix.high.e305,matrix.high.e306, matrix.high.e307, matrix.high.e308, matrix.high.e309, matrix.high.e310, matrix.high.e311, matrix.high.e312, matrix.high.e313, matrix.high.e314, matrix.high.e315, 
                       matrix.high.e316, matrix.high.e317, matrix.high.e318, matrix.high.e319, matrix.high.e320, matrix.high.e321, matrix.high.e322, matrix.high.e323,matrix.high.e324, matrix.high.e325, matrix.high.e326, matrix.high.e327, matrix.high.e328, matrix.high.e329, matrix.high.e330, 
                       matrix.high.e331, matrix.high.e332, matrix.high.e333, matrix.high.e334, matrix.high.e335, matrix.high.e336, matrix.high.e337, matrix.high.e338, matrix.high.e339,matrix.high.e340, matrix.high.e341, matrix.high.e342, matrix.high.e343, matrix.high.e344, matrix.high.e345, 
                       matrix.high.e346, matrix.high.e347, matrix.high.e348, matrix.high.e349, matrix.high.e350, matrix.high.e351, matrix.high.e352, matrix.high.e353, matrix.high.e354, matrix.high.e355,matrix.high.e356, matrix.high.e357, matrix.high.e358, matrix.high.e359, matrix.high.e360, 
                       matrix.high.e361, matrix.high.e362, matrix.high.e363, matrix.high.e364, matrix.high.e365, matrix.high.e366, matrix.high.e367, matrix.high.e368, matrix.high.e369, matrix.high.e370, matrix.high.e371, matrix.high.e372,matrix.high.e373, matrix.high.e374, matrix.high.e375, 
                       matrix.high.e376, matrix.high.e377, matrix.high.e378, matrix.high.e379, matrix.high.e380, matrix.high.e381, matrix.high.e382, matrix.high.e383, matrix.high.e384, matrix.high.e385, matrix.high.e386, matrix.high.e387, matrix.high.e388, matrix.high.e389,matrix.high.e390,
                       matrix.high.e391, matrix.high.e392, matrix.high.e393, matrix.high.e394, matrix.high.e395, matrix.high.e396, matrix.high.e397, matrix.high.e398, matrix.high.e399, matrix.high.e400, matrix.high.e401, matrix.high.e402, matrix.high.e403, matrix.high.e404, matrix.high.e405,
                       matrix.high.e406, matrix.high.e407, matrix.high.e408, matrix.high.e409, matrix.high.e410, matrix.high.e411, matrix.high.e412, matrix.high.e413, matrix.high.e414, matrix.high.e415, matrix.high.e416, matrix.high.e417, matrix.high.e418, matrix.high.e419, matrix.high.e420,
                       matrix.high.e421, matrix.high.e422, matrix.high.e423, matrix.high.e424, matrix.high.e425, matrix.high.e426 , matrix.high.e427 , matrix.high.e428, matrix.high.e429, matrix.high.e430, matrix.high.e431, matrix.high.e432, matrix.high.e433, matrix.high.e434, matrix.high.e435,
                       matrix.high.e436, matrix.high.e437, matrix.high.e438, matrix.high.e439, matrix.high.e440, matrix.high.e441, matrix.high.e442, matrix.high.e443, matrix.high.e444, matrix.high.e445, matrix.high.e446, matrix.high.e447, matrix.high.e448, matrix.high.e449, matrix.high.e450,
                       matrix.high.e451, matrix.high.e452, matrix.high.e453, matrix.high.e454, matrix.high.e455, matrix.high.e456, matrix.high.e457, matrix.high.e458, matrix.high.e459, matrix.high.e460, matrix.high.e461, matrix.high.e462, matrix.high.e463 , matrix.high.e464, matrix.high.e465, 
                       matrix.high.e466, matrix.high.e467, matrix.high.e468, matrix.high.e469,matrix.high.e470, matrix.high.e471, matrix.high.e472, matrix.high.e473, matrix.high.e474, matrix.high.e475, matrix.high.e476, matrix.high.e477, matrix.high.e478, matrix.high.e479, matrix.high.e480, 
                       matrix.high.e481, matrix.high.e482, matrix.high.e483, matrix.high.e484,matrix.high.e485, matrix.high.e486, matrix.high.e487, matrix.high.e488, matrix.high.e489, matrix.high.e490, matrix.high.e491, matrix.high.e492, matrix.high.e493, matrix.high.e494, matrix.high.e495, 
                       matrix.high.e496, matrix.high.e497, matrix.high.e498, matrix.high.e499, matrix.high.e500, matrix.high.e501, matrix.high.e502, matrix.high.e503, matrix.high.e504, matrix.high.e505, matrix.high.e506, matrix.high.e507, matrix.high.e508, matrix.high.e509, matrix.high.e510, 
                       matrix.high.e511, matrix.high.e512, matrix.high.e513, matrix.high.e514,matrix.high.e515, matrix.high.e516, matrix.high.e517, matrix.high.e518, matrix.high.e519, matrix.high.e520, matrix.high.e521, matrix.high.e522, matrix.high.e523, matrix.high.e524, matrix.high.e525, 
                       matrix.high.e526, matrix.high.e527, matrix.high.e528, matrix.high.e529,matrix.high.e530, matrix.high.e531, matrix.high.e532, matrix.high.e533, matrix.high.e534, matrix.high.e535, matrix.high.e536, matrix.high.e537, matrix.high.e538, matrix.high.e539, matrix.high.e540,
                       matrix.high.e541, matrix.high.e542, matrix.high.e543, matrix.high.e544, matrix.high.e545,matrix.high.e546, matrix.high.e547, matrix.high.e548,matrix.high.e549, matrix.high.e550, matrix.high.e551, matrix.high.e552, matrix.high.e553, matrix.high.e554,matrix.high.e555,
                       matrix.high.e556, matrix.high.e557, matrix.high.e558, matrix.high.e559, matrix.high.e560, matrix.high.e561, matrix.high.e562, matrix.high.e563, matrix.high.e564, matrix.high.e565, matrix.high.e566, matrix.high.e567, matrix.high.e568, matrix.high.e569, matrix.high.e570, 
                       matrix.high.e571, matrix.high.e572, matrix.high.e573, matrix.high.e574,matrix.high.e575, matrix.high.e576, matrix.high.e577, matrix.high.e578, matrix.high.e579, matrix.high.e580, matrix.high.e581, matrix.high.e582, matrix.high.e583, matrix.high.e584, matrix.high.e585,
                       matrix.high.e586, matrix.high.e587, matrix.high.e588, matrix.high.e589, matrix.high.e590, matrix.high.e591, matrix.high.e592, matrix.high.e593, matrix.high.e594,matrix.high.e595, matrix.high.e596, matrix.high.e597, matrix.high.e598, matrix.high.e599, matrix.high.e600, 
                       matrix.high.e601, matrix.high.e602, matrix.high.e603, matrix.high.e604, matrix.high.e605, matrix.high.e606, matrix.high.e607, matrix.high.e608, matrix.high.e609, matrix.high.e610, matrix.high.e611, matrix.high.e612, matrix.high.e613, matrix.high.e614,matrix.high.e615, 
                       matrix.high.e616, matrix.high.e617, matrix.high.e618, matrix.high.e619, matrix.high.e620, matrix.high.e621, matrix.high.e622, matrix.high.e623, matrix.high.e624, matrix.high.e625, matrix.high.e626, matrix.high.e627, matrix.high.e628, matrix.high.e629, matrix.high.e630,
                       matrix.high.e631, matrix.high.e632, matrix.high.e633, matrix.high.e634, matrix.high.e635, matrix.high.e636, matrix.high.e637, matrix.high.e638, matrix.high.e639, matrix.high.e640, matrix.high.e641, matrix.high.e642, matrix.high.e643, matrix.high.e644, matrix.high.e645, 
                       matrix.high.e646, matrix.high.e647, matrix.high.e648, matrix.high.e649, matrix.high.e650, matrix.high.e651, matrix.high.e652, matrix.high.e653, matrix.high.e654, matrix.high.e655, matrix.high.e656, matrix.high.e657, matrix.high.e658, matrix.high.e659, matrix.high.e660,
                       matrix.high.e661, matrix.high.e662, matrix.high.e663, matrix.high.e664, matrix.high.e665, matrix.high.e666,matrix.high.e667, matrix.high.e668, matrix.high.e669, matrix.high.e670, matrix.high.e671, matrix.high.e672, matrix.high.e673, matrix.high.e674, matrix.high.e675, 
                       matrix.high.e676, matrix.high.e677, matrix.high.e678, matrix.high.e679, matrix.high.e680, matrix.high.e681, matrix.high.e682, matrix.high.e683, matrix.high.e684, matrix.high.e685, matrix.high.e686, matrix.high.e687, matrix.high.e688, matrix.high.e689, matrix.high.e690,
                       matrix.high.e691, matrix.high.e692, matrix.high.e693, matrix.high.e694, matrix.high.e695, matrix.high.e696, matrix.high.e697, matrix.high.e698, matrix.high.e699, matrix.high.e700, matrix.high.e701, matrix.high.e702, matrix.high.e703, matrix.high.e704, matrix.high.e705,
                       matrix.high.e706, matrix.high.e707, matrix.high.e708, matrix.high.e709, matrix.high.e710, matrix.high.e711, matrix.high.e712, matrix.high.e713, matrix.high.e714, matrix.high.e715, matrix.high.e716, matrix.high.e717, matrix.high.e718, matrix.high.e719, matrix.high.e720, 
                       matrix.high.e721, matrix.high.e722, matrix.high.e723, matrix.high.e724, matrix.high.e725, matrix.high.e726, matrix.high.e727, matrix.high.e728, matrix.high.e729, matrix.high.e730, matrix.high.e731, matrix.high.e732, matrix.high.e733, matrix.high.e734, matrix.high.e735,
                       matrix.high.e736, matrix.high.e737, matrix.high.e738, matrix.high.e739, matrix.high.e740, matrix.high.e741, matrix.high.e742, matrix.high.e743, matrix.high.e744, matrix.high.e745, matrix.high.e746, matrix.high.e747, matrix.high.e748, matrix.high.e749, matrix.high.e750,
                       matrix.high.e751, matrix.high.e752, matrix.high.e753, matrix.high.e754, matrix.high.e755, matrix.high.e756, matrix.high.e757, matrix.high.e758, matrix.high.e759, matrix.high.e760, matrix.high.e761, matrix.high.e762, matrix.high.e763, matrix.high.e764, matrix.high.e765,
                       matrix.high.e766, matrix.high.e767, matrix.high.e768, matrix.high.e769, matrix.high.e770, matrix.high.e771, matrix.high.e772, matrix.high.e773, matrix.high.e774, matrix.high.e775, matrix.high.e776, matrix.high.e777, matrix.high.e778, matrix.high.e779, matrix.high.e780,
                       matrix.high.e781, matrix.high.e782, matrix.high.e783, matrix.high.e784, matrix.high.e785, matrix.high.e786, matrix.high.e787, matrix.high.e788, matrix.high.e789, matrix.high.e790, matrix.high.e791, matrix.high.e792, matrix.high.e793, matrix.high.e794, matrix.high.e795,
                       matrix.high.e796, matrix.high.e797, matrix.high.e798, matrix.high.e799, matrix.high.e800, matrix.high.e801, matrix.high.e802, matrix.high.e803, matrix.high.e804, matrix.high.e805, matrix.high.e806, matrix.high.e807, matrix.high.e808, matrix.high.e809, matrix.high.e810,
                       matrix.high.e811, matrix.high.e812, matrix.high.e813, matrix.high.e814, matrix.high.e815, matrix.high.e816, matrix.high.e817, matrix.high.e818, matrix.high.e819, matrix.high.e820, matrix.high.e821, matrix.high.e822, matrix.high.e823, matrix.high.e824, matrix.high.e825,
                       matrix.high.e826, matrix.high.e827, matrix.high.e828, matrix.high.e829, matrix.high.e830, matrix.high.e831, matrix.high.e832, matrix.high.e833, matrix.high.e834, matrix.high.e835, matrix.high.e836, matrix.high.e837, matrix.high.e838, matrix.high.e839, matrix.high.e840,
                       matrix.high.e841, matrix.high.e842, matrix.high.e843, matrix.high.e844, matrix.high.e845, matrix.high.e846, matrix.high.e847, matrix.high.e848, matrix.high.e849, matrix.high.e850, matrix.high.e851, matrix.high.e852, matrix.high.e853, matrix.high.e854, matrix.high.e855,
                       matrix.high.e856, matrix.high.e857, matrix.high.e858, matrix.high.e859, matrix.high.e860, matrix.high.e861, matrix.high.e862, matrix.high.e863, matrix.high.e864, matrix.high.e865, matrix.high.e866, matrix.high.e867, matrix.high.e868, matrix.high.e869, matrix.high.e870,
                       matrix.high.e871
                       
))

###  individual difference scaling with starting position 
#init for starting values
scaling.start.high.e <- smacof:::indscal(matrices.high.e, init=startconf.p, type="ordinal", itmax=75000)
scaling.start.high.e$stress
#0.1935133

plot.indscal.starting.high.e <- as.data.frame(scaling.start.high.e$gspace)
plot.indscal.starting.high.e$direction <- factor(c("pos","neg","neg",  "neg", "pos", "pos") )
plot.indscal.starting.high.e$strength <- factor(c("high","low", "low", "high", "low", "high") )
plot.indscal.starting.high.e$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco","Haare föhn vs luft", 
                                              "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))


ggplot(plot.indscal.starting.high.e, aes(x=D1, y=D2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + xlim(-.8, .8) +
  ylim(-.7, .7) +
  theme_minimal() +
  geom_text(aes(label = label), vjust=3, size=4)


## procrustes transformation  to compare education ####

fit.proc.e <- smacof::Procrustes(scaling.start.low.e$gspace, scaling.start.high.e$gspace)
fit.proc.e

plot(fit.proc.e, plot.type = "jointplot", plot.dim = c(1,2),
     xlab= "impact direction", ylab ="impact strength", 
     legend = list (labels = c("low education", "high education") ) )


#### for device frequency binary low or high ####
data.frequency.c <- data.accuracy.frequency %>% select(ResponseId, freq.binary)
data.complexity.freq <- merge(data.complexity.1, data.frequency.c, by="ResponseId")

# for low device frequency
data.complexity.freq.low <- data.complexity.freq %>%  
  filter(freq.binary=="low") %>% 
  select(ResponseId, ab:ef) 

data.complexity.freq.low$fa <- data.complexity.freq.low$af
data.complexity.freq.low$ea <- data.complexity.freq.low$ae
data.complexity.freq.low$da <- data.complexity.freq.low$ad
data.complexity.freq.low$ca <- data.complexity.freq.low$ac
data.complexity.freq.low$ba <- data.complexity.freq.low$ab

data.complexity.freq.low$fb <- data.complexity.freq.low$bf
data.complexity.freq.low$eb <- data.complexity.freq.low$be
data.complexity.freq.low$db <- data.complexity.freq.low$bd
data.complexity.freq.low$cb <- data.complexity.freq.low$bc

data.complexity.freq.low$fc <- data.complexity.freq.low$cf
data.complexity.freq.low$ec <- data.complexity.freq.low$ce
data.complexity.freq.low$dc <- data.complexity.freq.low$cd

data.complexity.freq.low$fd <- data.complexity.freq.low$df
data.complexity.freq.low$ed <- data.complexity.freq.low$de

data.complexity.freq.low$fe <- data.complexity.freq.low$ef

data.complexity.freq.low <- na.omit(data.complexity.freq.low)
data.complexity.freq.low.long <- pivot_longer(data.complexity.freq.low, 2:31, names_to = "verhalten", values_to = "similarity")
data.complexity.freq.low.long$verhalten2 <- data.complexity.freq.low.long$verhalten
data.complexity.freq.low.long$v2 <- substring(data.complexity.freq.low.long$verhalten2, 2,)
data.complexity.freq.low.long$v1 <- substring(data.complexity.freq.low.long$verhalten, 1,1)

data.complexity.freq.low.long$similarity <- as.numeric(data.complexity.freq.low.long$similarity) 
data.complexity.freq.low.long$similarity.2 <- (-(data.complexity.freq.low.long$similarity) +10)

dfList.freq.low <- split(data.complexity.freq.low.long,data.complexity.freq.low.long$ResponseId)
length(dfList.freq.low)
names(dfList.freq.low) <- paste0("id",1:length(dfList.freq.low))

for(r in 1:length(dfList.freq.low)) { 
  assign(paste0("matrix.freq.low",r), NULL )
}


for (r in 1:length(dfList.freq.low)) { 
  assign(paste0("matrix.freq.low", r), dfList.freq.low[[r]] %>% 
           mutate_at(1:2, factor, levels = unique(c(levels(.$v1), levels(.$v2)))) %>%
           xtabs(similarity.2 ~ v1 + v2, data=., sparse = FALSE) ) 
}

#### matrices ####
matrices.freq.low<- (list(matrix.freq.low1, matrix.freq.low2, matrix.freq.low3,matrix.freq.low4,matrix.freq.low5,matrix.freq.low6,matrix.freq.low7,matrix.freq.low8,matrix.freq.low9,matrix.freq.low10,matrix.freq.low11,matrix.freq.low12,matrix.freq.low13,matrix.freq.low14 ,matrix.freq.low15,
                       matrix.freq.low16,matrix.freq.low17,matrix.freq.low18,matrix.freq.low19,matrix.freq.low20,matrix.freq.low21,matrix.freq.low22,matrix.freq.low23,matrix.freq.low24,matrix.freq.low25,matrix.freq.low26,matrix.freq.low27,matrix.freq.low28,matrix.freq.low29,matrix.freq.low30,
                       matrix.freq.low31,matrix.freq.low32,matrix.freq.low33,matrix.freq.low34,matrix.freq.low35,matrix.freq.low36,matrix.freq.low37,matrix.freq.low38,matrix.freq.low39,matrix.freq.low40,matrix.freq.low41,matrix.freq.low42,matrix.freq.low43,matrix.freq.low44,matrix.freq.low45,
                       matrix.freq.low46,matrix.freq.low47,matrix.freq.low48,matrix.freq.low49,matrix.freq.low50,matrix.freq.low51,matrix.freq.low52,matrix.freq.low53,matrix.freq.low54,matrix.freq.low55,matrix.freq.low56,matrix.freq.low57,matrix.freq.low58,matrix.freq.low59,matrix.freq.low60,
                       matrix.freq.low61,matrix.freq.low62,matrix.freq.low63,matrix.freq.low64,matrix.freq.low65,matrix.freq.low66,matrix.freq.low67,matrix.freq.low68,matrix.freq.low69,matrix.freq.low70,matrix.freq.low71,matrix.freq.low72,matrix.freq.low73,matrix.freq.low74,matrix.freq.low75,
                       matrix.freq.low76, matrix.freq.low77,matrix.freq.low78,matrix.freq.low79,matrix.freq.low80,matrix.freq.low81,matrix.freq.low82,matrix.freq.low83,matrix.freq.low84,matrix.freq.low85,matrix.freq.low86,matrix.freq.low87,matrix.freq.low88,matrix.freq.low89,matrix.freq.low90,
                       matrix.freq.low91,matrix.freq.low92,matrix.freq.low93,matrix.freq.low94,matrix.freq.low95,matrix.freq.low96,matrix.freq.low97,matrix.freq.low98,matrix.freq.low99, matrix.freq.low100,matrix.freq.low101,matrix.freq.low102,matrix.freq.low103,matrix.freq.low104,matrix.freq.low105,
                       matrix.freq.low106,matrix.freq.low107,matrix.freq.low108,matrix.freq.low109,matrix.freq.low110,matrix.freq.low111,matrix.freq.low112,matrix.freq.low113,matrix.freq.low114,matrix.freq.low115,matrix.freq.low116,matrix.freq.low117,matrix.freq.low118,matrix.freq.low119,matrix.freq.low120,
                       matrix.freq.low121,matrix.freq.low122,matrix.freq.low123,matrix.freq.low124,matrix.freq.low125,matrix.freq.low126,matrix.freq.low127,matrix.freq.low128,matrix.freq.low129,matrix.freq.low130,matrix.freq.low131,matrix.freq.low132,matrix.freq.low133,matrix.freq.low134,matrix.freq.low135,
                       matrix.freq.low136,matrix.freq.low137,matrix.freq.low138,matrix.freq.low139,matrix.freq.low140,matrix.freq.low141,matrix.freq.low142,matrix.freq.low143,matrix.freq.low144,matrix.freq.low145,matrix.freq.low146,matrix.freq.low147,matrix.freq.low148,matrix.freq.low149,matrix.freq.low150,
                       matrix.freq.low151,matrix.freq.low152,matrix.freq.low153,matrix.freq.low154,matrix.freq.low155,matrix.freq.low156,matrix.freq.low157,matrix.freq.low158,matrix.freq.low159,matrix.freq.low160,matrix.freq.low161,matrix.freq.low162,matrix.freq.low163,matrix.freq.low164,matrix.freq.low165,
                       matrix.freq.low166,matrix.freq.low167,matrix.freq.low168,matrix.freq.low169,matrix.freq.low170,matrix.freq.low171,matrix.freq.low172,matrix.freq.low173,matrix.freq.low174,matrix.freq.low175,matrix.freq.low176,matrix.freq.low177,matrix.freq.low178,matrix.freq.low179,matrix.freq.low180,
                       matrix.freq.low181,matrix.freq.low182,matrix.freq.low183,matrix.freq.low184,matrix.freq.low185,matrix.freq.low186,matrix.freq.low187,matrix.freq.low188,matrix.freq.low189,matrix.freq.low190, matrix.freq.low191, matrix.freq.low192, matrix.freq.low193, matrix.freq.low194, matrix.freq.low195,
                       matrix.freq.low196, matrix.freq.low197, matrix.freq.low198, matrix.freq.low199, matrix.freq.low200, matrix.freq.low201, matrix.freq.low202, matrix.freq.low203, matrix.freq.low204, matrix.freq.low205, matrix.freq.low206, matrix.freq.low207, matrix.freq.low208, matrix.freq.low209, matrix.freq.low210,
                       matrix.freq.low211, matrix.freq.low212 , matrix.freq.low213, matrix.freq.low214, matrix.freq.low215, matrix.freq.low216, matrix.freq.low217, matrix.freq.low218, matrix.freq.low219, matrix.freq.low220, matrix.freq.low221, matrix.freq.low222, matrix.freq.low223, matrix.freq.low224, matrix.freq.low225,
                       matrix.freq.low226, matrix.freq.low227, matrix.freq.low228, matrix.freq.low229, matrix.freq.low230, matrix.freq.low231, matrix.freq.low232, matrix.freq.low233, matrix.freq.low234, matrix.freq.low235, matrix.freq.low236, matrix.freq.low237, matrix.freq.low238, matrix.freq.low239, matrix.freq.low240,
                       matrix.freq.low241, matrix.freq.low242, matrix.freq.low243, matrix.freq.low244, matrix.freq.low245, matrix.freq.low246, matrix.freq.low247, matrix.freq.low248, matrix.freq.low249, matrix.freq.low250, matrix.freq.low251, matrix.freq.low252, matrix.freq.low253, matrix.freq.low254, matrix.freq.low255,
                       matrix.freq.low256, matrix.freq.low257, matrix.freq.low258,matrix.freq.low259, matrix.freq.low260, matrix.freq.low261, matrix.freq.low262, matrix.freq.low263, matrix.freq.low264, matrix.freq.low265, matrix.freq.low266, matrix.freq.low267, matrix.freq.low268, matrix.freq.low269, matrix.freq.low270, 
                       matrix.freq.low271, matrix.freq.low272, matrix.freq.low273, matrix.freq.low274,matrix.freq.low275, matrix.freq.low276, matrix.freq.low277, matrix.freq.low278, matrix.freq.low279, matrix.freq.low280, matrix.freq.low281, matrix.freq.low282, matrix.freq.low283, matrix.freq.low284, matrix.freq.low285, 
                       matrix.freq.low286, matrix.freq.low287, matrix.freq.low288, matrix.freq.low289, matrix.freq.low290,matrix.freq.low291, matrix.freq.low292, matrix.freq.low293, matrix.freq.low294, matrix.freq.low295, matrix.freq.low296, matrix.freq.low297, matrix.freq.low298, matrix.freq.low299, matrix.freq.low300, 
                       matrix.freq.low301, matrix.freq.low302, matrix.freq.low303, matrix.freq.low304, matrix.freq.low305,matrix.freq.low306, matrix.freq.low307, matrix.freq.low308, matrix.freq.low309, matrix.freq.low310, matrix.freq.low311, matrix.freq.low312, matrix.freq.low313, matrix.freq.low314, matrix.freq.low315, 
                       matrix.freq.low316, matrix.freq.low317, matrix.freq.low318, matrix.freq.low319, matrix.freq.low320, matrix.freq.low321, matrix.freq.low322, matrix.freq.low323,matrix.freq.low324, matrix.freq.low325, matrix.freq.low326, matrix.freq.low327, matrix.freq.low328, matrix.freq.low329, matrix.freq.low330, 
                       matrix.freq.low331, matrix.freq.low332, matrix.freq.low333, matrix.freq.low334, matrix.freq.low335, matrix.freq.low336, matrix.freq.low337, matrix.freq.low338, matrix.freq.low339,matrix.freq.low340, matrix.freq.low341, matrix.freq.low342, matrix.freq.low343, matrix.freq.low344, matrix.freq.low345, 
                       matrix.freq.low346, matrix.freq.low347, matrix.freq.low348, matrix.freq.low349, matrix.freq.low350, matrix.freq.low351, matrix.freq.low352, matrix.freq.low353, matrix.freq.low354, matrix.freq.low355,matrix.freq.low356, matrix.freq.low357, matrix.freq.low358, matrix.freq.low359, matrix.freq.low360, 
                       matrix.freq.low361, matrix.freq.low362, matrix.freq.low363, matrix.freq.low364, matrix.freq.low365, matrix.freq.low366, matrix.freq.low367, matrix.freq.low368, matrix.freq.low369, matrix.freq.low370, matrix.freq.low371, matrix.freq.low372,matrix.freq.low373, matrix.freq.low374, matrix.freq.low375, 
                       matrix.freq.low376, matrix.freq.low377, matrix.freq.low378, matrix.freq.low379, matrix.freq.low380, matrix.freq.low381, matrix.freq.low382, matrix.freq.low383, matrix.freq.low384, matrix.freq.low385, matrix.freq.low386, matrix.freq.low387, matrix.freq.low388, matrix.freq.low389,matrix.freq.low390,
                       matrix.freq.low391, matrix.freq.low392, matrix.freq.low393, matrix.freq.low394, matrix.freq.low395, matrix.freq.low396, matrix.freq.low397, matrix.freq.low398, matrix.freq.low399, matrix.freq.low400, matrix.freq.low401, matrix.freq.low402, matrix.freq.low403, matrix.freq.low404, matrix.freq.low405,
                       matrix.freq.low406, matrix.freq.low407, matrix.freq.low408, matrix.freq.low409, matrix.freq.low410, matrix.freq.low411, matrix.freq.low412, matrix.freq.low413, matrix.freq.low414, matrix.freq.low415, matrix.freq.low416, matrix.freq.low417, matrix.freq.low418, matrix.freq.low419, matrix.freq.low420,
                       matrix.freq.low421, matrix.freq.low422, matrix.freq.low423, matrix.freq.low424, matrix.freq.low425, matrix.freq.low426 , matrix.freq.low427 , matrix.freq.low428, matrix.freq.low429, matrix.freq.low430, matrix.freq.low431, matrix.freq.low432, matrix.freq.low433, matrix.freq.low434, matrix.freq.low435,
                       matrix.freq.low436, matrix.freq.low437, matrix.freq.low438, matrix.freq.low439, matrix.freq.low440, matrix.freq.low441, matrix.freq.low442, matrix.freq.low443, matrix.freq.low444, matrix.freq.low445, matrix.freq.low446, matrix.freq.low447, matrix.freq.low448, matrix.freq.low449, matrix.freq.low450,
                       matrix.freq.low451, matrix.freq.low452, matrix.freq.low453, matrix.freq.low454, matrix.freq.low455, matrix.freq.low456, matrix.freq.low457, matrix.freq.low458, matrix.freq.low459, matrix.freq.low460, matrix.freq.low461, matrix.freq.low462, matrix.freq.low463 , matrix.freq.low464, matrix.freq.low465, 
                       matrix.freq.low466, matrix.freq.low467, matrix.freq.low468, matrix.freq.low469,matrix.freq.low470, matrix.freq.low471, matrix.freq.low472, matrix.freq.low473, matrix.freq.low474, matrix.freq.low475, matrix.freq.low476, matrix.freq.low477, matrix.freq.low478, matrix.freq.low479, matrix.freq.low480, 
                       matrix.freq.low481, matrix.freq.low482, matrix.freq.low483, matrix.freq.low484,matrix.freq.low485, matrix.freq.low486, matrix.freq.low487, matrix.freq.low488, matrix.freq.low489, matrix.freq.low490, matrix.freq.low491, matrix.freq.low492, matrix.freq.low493, matrix.freq.low494, matrix.freq.low495, 
                       matrix.freq.low496, matrix.freq.low497, matrix.freq.low498, matrix.freq.low499, matrix.freq.low500, matrix.freq.low501, matrix.freq.low502, matrix.freq.low503, matrix.freq.low504, matrix.freq.low505, matrix.freq.low506, matrix.freq.low507, matrix.freq.low508, matrix.freq.low509, matrix.freq.low510, 
                       matrix.freq.low511, matrix.freq.low512, matrix.freq.low513, matrix.freq.low514,matrix.freq.low515, matrix.freq.low516, matrix.freq.low517, matrix.freq.low518, matrix.freq.low519, matrix.freq.low520, matrix.freq.low521, matrix.freq.low522, matrix.freq.low523, matrix.freq.low524, matrix.freq.low525, 
                       matrix.freq.low526, matrix.freq.low527, matrix.freq.low528, matrix.freq.low529,matrix.freq.low530, matrix.freq.low531, matrix.freq.low532, matrix.freq.low533, matrix.freq.low534, matrix.freq.low535, matrix.freq.low536, matrix.freq.low537, matrix.freq.low538, matrix.freq.low539, matrix.freq.low540,
                       matrix.freq.low541, matrix.freq.low542, matrix.freq.low543, matrix.freq.low544, matrix.freq.low545,matrix.freq.low546, matrix.freq.low547, matrix.freq.low548,matrix.freq.low549, matrix.freq.low550, matrix.freq.low551, matrix.freq.low552, matrix.freq.low553, matrix.freq.low554,matrix.freq.low555,
                       matrix.freq.low556, matrix.freq.low557, matrix.freq.low558, matrix.freq.low559, matrix.freq.low560, matrix.freq.low561, matrix.freq.low562, matrix.freq.low563, matrix.freq.low564, matrix.freq.low565, matrix.freq.low566, matrix.freq.low567, matrix.freq.low568, matrix.freq.low569, matrix.freq.low570, 
                       matrix.freq.low571, matrix.freq.low572, matrix.freq.low573, matrix.freq.low574,matrix.freq.low575, matrix.freq.low576, matrix.freq.low577, matrix.freq.low578, matrix.freq.low579, matrix.freq.low580, matrix.freq.low581, matrix.freq.low582, matrix.freq.low583, matrix.freq.low584, matrix.freq.low585,
                       matrix.freq.low586, matrix.freq.low587, matrix.freq.low588, matrix.freq.low589, matrix.freq.low590, matrix.freq.low591, matrix.freq.low592, matrix.freq.low593, matrix.freq.low594,matrix.freq.low595, matrix.freq.low596, matrix.freq.low597, matrix.freq.low598, matrix.freq.low599, matrix.freq.low600, 
                       matrix.freq.low601, matrix.freq.low602, matrix.freq.low603, matrix.freq.low604, matrix.freq.low605, matrix.freq.low606, matrix.freq.low607, matrix.freq.low608, matrix.freq.low609, matrix.freq.low610, matrix.freq.low611, matrix.freq.low612, matrix.freq.low613, matrix.freq.low614,matrix.freq.low615, 
                       matrix.freq.low616, matrix.freq.low617, matrix.freq.low618, matrix.freq.low619, matrix.freq.low620, matrix.freq.low621, matrix.freq.low622, matrix.freq.low623, matrix.freq.low624, matrix.freq.low625, matrix.freq.low626, matrix.freq.low627, matrix.freq.low628, matrix.freq.low629, matrix.freq.low630,
                       matrix.freq.low631, matrix.freq.low632, matrix.freq.low633, matrix.freq.low634, matrix.freq.low635, matrix.freq.low636, matrix.freq.low637, matrix.freq.low638, matrix.freq.low639, matrix.freq.low640, matrix.freq.low641, matrix.freq.low642, matrix.freq.low643, matrix.freq.low644, matrix.freq.low645, 
                       matrix.freq.low646, matrix.freq.low647, matrix.freq.low648, matrix.freq.low649, matrix.freq.low650, matrix.freq.low651, matrix.freq.low652, matrix.freq.low653, matrix.freq.low654, matrix.freq.low655, matrix.freq.low656, matrix.freq.low657, matrix.freq.low658, matrix.freq.low659, matrix.freq.low660,
                       matrix.freq.low661, matrix.freq.low662, matrix.freq.low663, matrix.freq.low664, matrix.freq.low665, matrix.freq.low666,matrix.freq.low667, matrix.freq.low668, matrix.freq.low669, matrix.freq.low670, matrix.freq.low671, matrix.freq.low672, matrix.freq.low673, matrix.freq.low674, matrix.freq.low675, 
                       matrix.freq.low676, matrix.freq.low677, matrix.freq.low678, matrix.freq.low679, matrix.freq.low680, matrix.freq.low681, matrix.freq.low682, matrix.freq.low683, matrix.freq.low684, matrix.freq.low685, matrix.freq.low686, matrix.freq.low687, matrix.freq.low688, matrix.freq.low689, matrix.freq.low690,
                       matrix.freq.low691, matrix.freq.low692, matrix.freq.low693, matrix.freq.low694, matrix.freq.low695, matrix.freq.low696, matrix.freq.low697, matrix.freq.low698, matrix.freq.low699, matrix.freq.low700, matrix.freq.low701, matrix.freq.low702, matrix.freq.low703, matrix.freq.low704, matrix.freq.low705,
                       matrix.freq.low706, matrix.freq.low707, matrix.freq.low708, matrix.freq.low709, matrix.freq.low710, matrix.freq.low711, matrix.freq.low712, matrix.freq.low713, matrix.freq.low714, matrix.freq.low715, matrix.freq.low716, matrix.freq.low717, matrix.freq.low718, matrix.freq.low719, matrix.freq.low720, 
                       matrix.freq.low721, matrix.freq.low722, matrix.freq.low723, matrix.freq.low724, matrix.freq.low725, matrix.freq.low726, matrix.freq.low727, matrix.freq.low728, matrix.freq.low729, matrix.freq.low730, matrix.freq.low731, matrix.freq.low732, matrix.freq.low733, matrix.freq.low734, matrix.freq.low735,
                       matrix.freq.low736, matrix.freq.low737, matrix.freq.low738, matrix.freq.low739, matrix.freq.low740, matrix.freq.low741, matrix.freq.low742, matrix.freq.low743, matrix.freq.low744, matrix.freq.low745, matrix.freq.low746, matrix.freq.low747, matrix.freq.low748, matrix.freq.low749, matrix.freq.low750,
                       matrix.freq.low751, matrix.freq.low752, matrix.freq.low753, matrix.freq.low754, matrix.freq.low755, matrix.freq.low756, matrix.freq.low757,
                       matrix.freq.low758, matrix.freq.low759, matrix.freq.low760, matrix.freq.low761, matrix.freq.low762, matrix.freq.low763, matrix.freq.low764, matrix.freq.low765,
                       matrix.freq.low766, matrix.freq.low767, matrix.freq.low768, matrix.freq.low769, matrix.freq.low770, matrix.freq.low771, matrix.freq.low772, matrix.freq.low773, matrix.freq.low774, matrix.freq.low775, matrix.freq.low776, matrix.freq.low777, matrix.freq.low778, matrix.freq.low779, matrix.freq.low780,
                       matrix.freq.low781, matrix.freq.low782, matrix.freq.low783, matrix.freq.low784, matrix.freq.low785, matrix.freq.low786, matrix.freq.low787, matrix.freq.low788, matrix.freq.low789, matrix.freq.low790, matrix.freq.low791, matrix.freq.low792, matrix.freq.low793, matrix.freq.low794, matrix.freq.low795,
                       matrix.freq.low796, matrix.freq.low797, matrix.freq.low798, matrix.freq.low799, matrix.freq.low800, matrix.freq.low801, matrix.freq.low802, matrix.freq.low803, matrix.freq.low804, matrix.freq.low805, matrix.freq.low806, matrix.freq.low807, matrix.freq.low808, matrix.freq.low809, matrix.freq.low810,
                       matrix.freq.low811, matrix.freq.low812, matrix.freq.low813, matrix.freq.low814, matrix.freq.low815, matrix.freq.low816, matrix.freq.low817, matrix.freq.low818, matrix.freq.low819, matrix.freq.low820, matrix.freq.low821, matrix.freq.low822, matrix.freq.low823, matrix.freq.low824, matrix.freq.low825,
                       matrix.freq.low826, matrix.freq.low827, matrix.freq.low828, matrix.freq.low829, matrix.freq.low830, matrix.freq.low831, matrix.freq.low832, matrix.freq.low833, matrix.freq.low834, matrix.freq.low835, matrix.freq.low836, matrix.freq.low837, matrix.freq.low838, matrix.freq.low839, matrix.freq.low840,
                       matrix.freq.low841, matrix.freq.low842, matrix.freq.low843, matrix.freq.low844, matrix.freq.low845, matrix.freq.low846, matrix.freq.low847, matrix.freq.low848, matrix.freq.low849, matrix.freq.low850, matrix.freq.low851, matrix.freq.low852, matrix.freq.low853, matrix.freq.low854, matrix.freq.low855,
                       matrix.freq.low856, matrix.freq.low857, matrix.freq.low858, matrix.freq.low859, matrix.freq.low860, matrix.freq.low861, matrix.freq.low862, matrix.freq.low863, matrix.freq.low864, matrix.freq.low865, matrix.freq.low866, matrix.freq.low867, matrix.freq.low868, matrix.freq.low869, matrix.freq.low870,
                       matrix.freq.low871, matrix.freq.low872, matrix.freq.low873
))


###  individual difference scaling with starting position 
#init for starting values
scaling.start.freq.low <- smacof:::indscal(matrices.freq.low, init=startconf.p, type="ordinal", itmax=75000)
scaling.start.freq.low$stress
# 0.1942388

plot.indscal.starting.freq.low <- as.data.frame(scaling.start.freq.low$gspace)
plot.indscal.starting.freq.low$direction <- factor(c("pos","neg","neg",  "neg", "pos", "pos") )
plot.indscal.starting.freq.low$strength <- factor(c("high","low", "low", "high", "low", "high") )
plot.indscal.starting.freq.low$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco","Haare föhn vs luft", 
                                              "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))

ggplot(plot.indscal.starting.freq.low, aes(x=D1, y=D2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + xlim(-.8, .8) +
  ylim(-.7, .7) +
  theme_minimal() +
  geom_text(aes(label = label), vjust=3, size=4)

#### for high device frequency
data.complexity.freq.high <- data.complexity.freq %>%  
  filter(freq.binary=="high") %>% 
  select(ResponseId, ab:ef) 

data.complexity.freq.high$fa <- data.complexity.freq.high$af
data.complexity.freq.high$ea <- data.complexity.freq.high$ae
data.complexity.freq.high$da <- data.complexity.freq.high$ad
data.complexity.freq.high$ca <- data.complexity.freq.high$ac
data.complexity.freq.high$ba <- data.complexity.freq.high$ab

data.complexity.freq.high$fb <- data.complexity.freq.high$bf
data.complexity.freq.high$eb <- data.complexity.freq.high$be
data.complexity.freq.high$db <- data.complexity.freq.high$bd
data.complexity.freq.high$cb <- data.complexity.freq.high$bc

data.complexity.freq.high$fc <- data.complexity.freq.high$cf
data.complexity.freq.high$ec <- data.complexity.freq.high$ce
data.complexity.freq.high$dc <- data.complexity.freq.high$cd

data.complexity.freq.high$fd <- data.complexity.freq.high$df
data.complexity.freq.high$ed <- data.complexity.freq.high$de

data.complexity.freq.high$fe <- data.complexity.freq.high$ef

data.complexity.freq.high <- na.omit(data.complexity.freq.high)
data.complexity.freq.high.long <- pivot_longer(data.complexity.freq.high, 2:31, names_to = "verhalten", values_to = "similarity")
data.complexity.freq.high.long$verhalten2 <- data.complexity.freq.high.long$verhalten
data.complexity.freq.high.long$v2 <- substring(data.complexity.freq.high.long$verhalten2, 2,)
data.complexity.freq.high.long$v1 <- substring(data.complexity.freq.high.long$verhalten, 1,1)

data.complexity.freq.high.long$similarity <- as.numeric(data.complexity.freq.high.long$similarity) 
data.complexity.freq.high.long$similarity.2 <- (-(data.complexity.freq.high.long$similarity) +10)

dfList.freq.high <- split(data.complexity.freq.high.long,data.complexity.freq.high.long$ResponseId)
length(dfList.freq.high)
names(dfList.freq.high) <- paste0("id",1:length(dfList.freq.high))

for(r in 1:length(dfList.freq.high)) { 
  assign(paste0("matrix.freq.high",r), NULL )
}


for (r in 1:length(dfList.freq.high)) { 
  assign(paste0("matrix.freq.high", r), dfList.freq.high[[r]] %>% 
           mutate_at(1:2, factor, levels = unique(c(levels(.$v1), levels(.$v2)))) %>%
           xtabs(similarity.2 ~ v1 + v2, data=., sparse = FALSE) ) 
}

#### matrices ####
matrices.freq.high<- (list(matrix.freq.high1, matrix.freq.high2, matrix.freq.high3,matrix.freq.high4,matrix.freq.high5,matrix.freq.high6,matrix.freq.high7,matrix.freq.high8,matrix.freq.high9,matrix.freq.high10,matrix.freq.high11,matrix.freq.high12,matrix.freq.high13,matrix.freq.high14 ,matrix.freq.high15,
                          matrix.freq.high16,matrix.freq.high17,matrix.freq.high18,matrix.freq.high19,matrix.freq.high20,matrix.freq.high21,matrix.freq.high22,matrix.freq.high23,matrix.freq.high24,matrix.freq.high25,matrix.freq.high26,matrix.freq.high27,matrix.freq.high28,matrix.freq.high29,matrix.freq.high30,
                          matrix.freq.high31,matrix.freq.high32,matrix.freq.high33,matrix.freq.high34,matrix.freq.high35,matrix.freq.high36,matrix.freq.high37,matrix.freq.high38,matrix.freq.high39,matrix.freq.high40,matrix.freq.high41,matrix.freq.high42,matrix.freq.high43,matrix.freq.high44,matrix.freq.high45,
                          matrix.freq.high46,matrix.freq.high47,matrix.freq.high48,matrix.freq.high49,matrix.freq.high50,matrix.freq.high51,matrix.freq.high52,matrix.freq.high53,matrix.freq.high54,matrix.freq.high55,matrix.freq.high56,matrix.freq.high57,matrix.freq.high58,matrix.freq.high59,matrix.freq.high60,
                          matrix.freq.high61,matrix.freq.high62,matrix.freq.high63,matrix.freq.high64,matrix.freq.high65,matrix.freq.high66,matrix.freq.high67,matrix.freq.high68,matrix.freq.high69,matrix.freq.high70,matrix.freq.high71,matrix.freq.high72,matrix.freq.high73,matrix.freq.high74,matrix.freq.high75,
                          matrix.freq.high76, matrix.freq.high77,matrix.freq.high78,matrix.freq.high79,matrix.freq.high80,matrix.freq.high81,matrix.freq.high82,matrix.freq.high83,matrix.freq.high84,matrix.freq.high85,matrix.freq.high86,matrix.freq.high87,matrix.freq.high88,matrix.freq.high89,matrix.freq.high90,
                          matrix.freq.high91,matrix.freq.high92,matrix.freq.high93,matrix.freq.high94,matrix.freq.high95,matrix.freq.high96,matrix.freq.high97,matrix.freq.high98,matrix.freq.high99, matrix.freq.high100,matrix.freq.high101,matrix.freq.high102,matrix.freq.high103,matrix.freq.high104,matrix.freq.high105,
                          matrix.freq.high106,matrix.freq.high107,matrix.freq.high108,matrix.freq.high109,matrix.freq.high110,matrix.freq.high111,matrix.freq.high112,matrix.freq.high113,matrix.freq.high114,matrix.freq.high115,matrix.freq.high116,matrix.freq.high117,matrix.freq.high118,matrix.freq.high119,matrix.freq.high120,
                          matrix.freq.high121,matrix.freq.high122,matrix.freq.high123,matrix.freq.high124,matrix.freq.high125,matrix.freq.high126,matrix.freq.high127,matrix.freq.high128,matrix.freq.high129,matrix.freq.high130,matrix.freq.high131,matrix.freq.high132,matrix.freq.high133,matrix.freq.high134,matrix.freq.high135,
                          matrix.freq.high136,matrix.freq.high137,matrix.freq.high138,matrix.freq.high139,matrix.freq.high140,matrix.freq.high141,matrix.freq.high142,matrix.freq.high143,matrix.freq.high144,matrix.freq.high145,matrix.freq.high146,matrix.freq.high147,matrix.freq.high148,matrix.freq.high149,matrix.freq.high150,
                          matrix.freq.high151,matrix.freq.high152,matrix.freq.high153,matrix.freq.high154,matrix.freq.high155,matrix.freq.high156,matrix.freq.high157,matrix.freq.high158,matrix.freq.high159,matrix.freq.high160,matrix.freq.high161,matrix.freq.high162,matrix.freq.high163,matrix.freq.high164,matrix.freq.high165,
                          matrix.freq.high166,matrix.freq.high167,matrix.freq.high168,matrix.freq.high169,matrix.freq.high170,matrix.freq.high171,matrix.freq.high172,matrix.freq.high173,matrix.freq.high174,matrix.freq.high175,matrix.freq.high176,matrix.freq.high177,matrix.freq.high178,matrix.freq.high179,matrix.freq.high180,
                          matrix.freq.high181,matrix.freq.high182,matrix.freq.high183,matrix.freq.high184,matrix.freq.high185,matrix.freq.high186,matrix.freq.high187,matrix.freq.high188,matrix.freq.high189,matrix.freq.high190, matrix.freq.high191, matrix.freq.high192, matrix.freq.high193, matrix.freq.high194, matrix.freq.high195,
                          matrix.freq.high196, matrix.freq.high197, matrix.freq.high198, matrix.freq.high199, matrix.freq.high200, matrix.freq.high201, matrix.freq.high202, matrix.freq.high203, matrix.freq.high204, matrix.freq.high205, matrix.freq.high206, matrix.freq.high207, matrix.freq.high208, matrix.freq.high209, matrix.freq.high210,
                          matrix.freq.high211, matrix.freq.high212 , matrix.freq.high213, matrix.freq.high214, matrix.freq.high215, matrix.freq.high216, matrix.freq.high217, matrix.freq.high218, matrix.freq.high219, matrix.freq.high220, matrix.freq.high221, matrix.freq.high222, matrix.freq.high223, matrix.freq.high224, matrix.freq.high225,
                          matrix.freq.high226, matrix.freq.high227, matrix.freq.high228, matrix.freq.high229, matrix.freq.high230, matrix.freq.high231, matrix.freq.high232, matrix.freq.high233, matrix.freq.high234, matrix.freq.high235, matrix.freq.high236, matrix.freq.high237, matrix.freq.high238, matrix.freq.high239, matrix.freq.high240,
                          matrix.freq.high241, matrix.freq.high242, matrix.freq.high243, matrix.freq.high244, matrix.freq.high245, matrix.freq.high246, matrix.freq.high247, matrix.freq.high248, matrix.freq.high249, matrix.freq.high250, matrix.freq.high251, matrix.freq.high252, matrix.freq.high253, matrix.freq.high254, matrix.freq.high255,
                          matrix.freq.high256, matrix.freq.high257, matrix.freq.high258,matrix.freq.high259, matrix.freq.high260, matrix.freq.high261, matrix.freq.high262, matrix.freq.high263, matrix.freq.high264, matrix.freq.high265, matrix.freq.high266, matrix.freq.high267, matrix.freq.high268, matrix.freq.high269, matrix.freq.high270, 
                          matrix.freq.high271, matrix.freq.high272, matrix.freq.high273, matrix.freq.high274,matrix.freq.high275, matrix.freq.high276, matrix.freq.high277, matrix.freq.high278, matrix.freq.high279, matrix.freq.high280, matrix.freq.high281, matrix.freq.high282, matrix.freq.high283, matrix.freq.high284, matrix.freq.high285, 
                          matrix.freq.high286, matrix.freq.high287, matrix.freq.high288, matrix.freq.high289, matrix.freq.high290,matrix.freq.high291, matrix.freq.high292, matrix.freq.high293, matrix.freq.high294, matrix.freq.high295, matrix.freq.high296, matrix.freq.high297, matrix.freq.high298, matrix.freq.high299, matrix.freq.high300, 
                          matrix.freq.high301, matrix.freq.high302, matrix.freq.high303, matrix.freq.high304, matrix.freq.high305,matrix.freq.high306, matrix.freq.high307, matrix.freq.high308, matrix.freq.high309, matrix.freq.high310, matrix.freq.high311, matrix.freq.high312, matrix.freq.high313, matrix.freq.high314, matrix.freq.high315, 
                          matrix.freq.high316, matrix.freq.high317, matrix.freq.high318, matrix.freq.high319, matrix.freq.high320, matrix.freq.high321, matrix.freq.high322, matrix.freq.high323,matrix.freq.high324, matrix.freq.high325, matrix.freq.high326, matrix.freq.high327, matrix.freq.high328, matrix.freq.high329, matrix.freq.high330, 
                          matrix.freq.high331, matrix.freq.high332, matrix.freq.high333, matrix.freq.high334, matrix.freq.high335, matrix.freq.high336, matrix.freq.high337, matrix.freq.high338, matrix.freq.high339,matrix.freq.high340, matrix.freq.high341, matrix.freq.high342, matrix.freq.high343, matrix.freq.high344, matrix.freq.high345, 
                          matrix.freq.high346, matrix.freq.high347, matrix.freq.high348, matrix.freq.high349, matrix.freq.high350, matrix.freq.high351, matrix.freq.high352, matrix.freq.high353, matrix.freq.high354, matrix.freq.high355,matrix.freq.high356, matrix.freq.high357, matrix.freq.high358, matrix.freq.high359, matrix.freq.high360, 
                          matrix.freq.high361, matrix.freq.high362, matrix.freq.high363, matrix.freq.high364, matrix.freq.high365, matrix.freq.high366, matrix.freq.high367, matrix.freq.high368, matrix.freq.high369, matrix.freq.high370, matrix.freq.high371, matrix.freq.high372,matrix.freq.high373, matrix.freq.high374, matrix.freq.high375, 
                          matrix.freq.high376, matrix.freq.high377, matrix.freq.high378, matrix.freq.high379, matrix.freq.high380, matrix.freq.high381, matrix.freq.high382, matrix.freq.high383, matrix.freq.high384, matrix.freq.high385, matrix.freq.high386, matrix.freq.high387, matrix.freq.high388, matrix.freq.high389,matrix.freq.high390,
                          matrix.freq.high391, matrix.freq.high392, matrix.freq.high393, matrix.freq.high394, matrix.freq.high395, matrix.freq.high396, matrix.freq.high397, matrix.freq.high398, matrix.freq.high399, matrix.freq.high400, matrix.freq.high401, matrix.freq.high402, matrix.freq.high403, matrix.freq.high404, matrix.freq.high405,
                          matrix.freq.high406, matrix.freq.high407, matrix.freq.high408, matrix.freq.high409, matrix.freq.high410, matrix.freq.high411, matrix.freq.high412, matrix.freq.high413, matrix.freq.high414, matrix.freq.high415, matrix.freq.high416, matrix.freq.high417, matrix.freq.high418, matrix.freq.high419, matrix.freq.high420,
                          matrix.freq.high421, matrix.freq.high422, matrix.freq.high423, matrix.freq.high424, matrix.freq.high425, matrix.freq.high426 , matrix.freq.high427 , matrix.freq.high428, matrix.freq.high429, matrix.freq.high430, matrix.freq.high431, matrix.freq.high432, matrix.freq.high433, matrix.freq.high434, matrix.freq.high435,
                          matrix.freq.high436, matrix.freq.high437, matrix.freq.high438, matrix.freq.high439, matrix.freq.high440, matrix.freq.high441, matrix.freq.high442, matrix.freq.high443, matrix.freq.high444, matrix.freq.high445, matrix.freq.high446, matrix.freq.high447, matrix.freq.high448, matrix.freq.high449, matrix.freq.high450,
                          matrix.freq.high451, matrix.freq.high452, matrix.freq.high453, matrix.freq.high454, matrix.freq.high455, matrix.freq.high456, matrix.freq.high457, matrix.freq.high458, matrix.freq.high459, matrix.freq.high460, matrix.freq.high461, matrix.freq.high462, matrix.freq.high463 , matrix.freq.high464, matrix.freq.high465, 
                          matrix.freq.high466, matrix.freq.high467, matrix.freq.high468, matrix.freq.high469,matrix.freq.high470, matrix.freq.high471, matrix.freq.high472, matrix.freq.high473, matrix.freq.high474, matrix.freq.high475, matrix.freq.high476, matrix.freq.high477, matrix.freq.high478, matrix.freq.high479, matrix.freq.high480, 
                          matrix.freq.high481, matrix.freq.high482, matrix.freq.high483, matrix.freq.high484,matrix.freq.high485, matrix.freq.high486, matrix.freq.high487, matrix.freq.high488, matrix.freq.high489, matrix.freq.high490, matrix.freq.high491, matrix.freq.high492, matrix.freq.high493, matrix.freq.high494, matrix.freq.high495, 
                          matrix.freq.high496, matrix.freq.high497, matrix.freq.high498, matrix.freq.high499, matrix.freq.high500, matrix.freq.high501, matrix.freq.high502, matrix.freq.high503, matrix.freq.high504, matrix.freq.high505, matrix.freq.high506, matrix.freq.high507, matrix.freq.high508, matrix.freq.high509, matrix.freq.high510, 
                          matrix.freq.high511, matrix.freq.high512, matrix.freq.high513, matrix.freq.high514,matrix.freq.high515, matrix.freq.high516, matrix.freq.high517, matrix.freq.high518, matrix.freq.high519, matrix.freq.high520, matrix.freq.high521, matrix.freq.high522, matrix.freq.high523, matrix.freq.high524, matrix.freq.high525, 
                          matrix.freq.high526, matrix.freq.high527, matrix.freq.high528, matrix.freq.high529,matrix.freq.high530, matrix.freq.high531, matrix.freq.high532, matrix.freq.high533, matrix.freq.high534, matrix.freq.high535, matrix.freq.high536, matrix.freq.high537, matrix.freq.high538, matrix.freq.high539, matrix.freq.high540,
                          matrix.freq.high541, matrix.freq.high542, matrix.freq.high543, matrix.freq.high544, matrix.freq.high545,matrix.freq.high546, matrix.freq.high547, matrix.freq.high548,matrix.freq.high549, matrix.freq.high550, matrix.freq.high551, matrix.freq.high552, matrix.freq.high553, matrix.freq.high554,matrix.freq.high555,
                          matrix.freq.high556, matrix.freq.high557, matrix.freq.high558, matrix.freq.high559, matrix.freq.high560, matrix.freq.high561, matrix.freq.high562, matrix.freq.high563, matrix.freq.high564, matrix.freq.high565, matrix.freq.high566, matrix.freq.high567, matrix.freq.high568, matrix.freq.high569, matrix.freq.high570, 
                          matrix.freq.high571, matrix.freq.high572, matrix.freq.high573, matrix.freq.high574,matrix.freq.high575, matrix.freq.high576, matrix.freq.high577, matrix.freq.high578, matrix.freq.high579, matrix.freq.high580, matrix.freq.high581, matrix.freq.high582, matrix.freq.high583, matrix.freq.high584, matrix.freq.high585,
                          matrix.freq.high586, matrix.freq.high587, matrix.freq.high588, matrix.freq.high589, matrix.freq.high590, matrix.freq.high591, matrix.freq.high592, matrix.freq.high593, matrix.freq.high594,matrix.freq.high595, matrix.freq.high596, matrix.freq.high597, matrix.freq.high598
))


###  individual difference scaling with starting position 
#init for starting values
scaling.start.freq.high <- smacof:::indscal(matrices.freq.high, init=startconf.p, type="ordinal", itmax=75000)
scaling.start.freq.high$stress
# 0.1952215

plot.indscal.starting.freq.high <- as.data.frame(scaling.start.freq.high$gspace)
plot.indscal.starting.freq.high$direction <- factor(c("pos","neg","neg",  "neg", "pos", "pos") )
plot.indscal.starting.freq.high$strength <- factor(c("high","low", "low", "high", "low", "high") )
plot.indscal.starting.freq.high$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco","Haare föhn vs luft", 
                                                 "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))


ggplot(plot.indscal.starting.freq.high, aes(x=D1, y=D2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + xlim(-.8, .8) +
  ylim(-.7, .7) +
  theme_minimal() +
  geom_text(aes(label = label), vjust=3, size=4)

##### procrustes transformation to compare device frequency ####
fit.proc.f <- smacof::Procrustes(scaling.start.freq.low$gspace, scaling.start.freq.high$gspace)
fit.proc.f

plot(fit.proc.f, plot.type = "jointplot", plot.dim = c(1,2),
     xlab= "impact direction", ylab ="impact strength", 
     legend = list (labels = c("low frequency", "high frequency") ) )



#### by numeracy for wave 2 #####
data.complexity.1 <- data.pass.2 %>% select(ResponseId, ab:ef, attention.check, numeracy, country) %>% filter(attention.check==4)
data.complexity.1$numeracy.f <- ifelse(data.complexity.1$numeracy==25, "numeracy high", "numeracy low")

data.complexity <- data.complexity.1 %>% select(ResponseId, numeracy.f, ab:ef)
data.complexity.high <- data.complexity %>% filter(numeracy.f=="numeracy high")

data.complexity.high$fa <- data.complexity.high$af
data.complexity.high$ea <- data.complexity.high$ae
data.complexity.high$da <- data.complexity.high$ad
data.complexity.high$ca <- data.complexity.high$ac
data.complexity.high$ba <- data.complexity.high$ab

data.complexity.high$fb <- data.complexity.high$bf
data.complexity.high$eb <- data.complexity.high$be
data.complexity.high$db <- data.complexity.high$bd
data.complexity.high$cb <- data.complexity.high$bc

data.complexity.high$fc <- data.complexity.high$cf
data.complexity.high$ec <- data.complexity.high$ce
data.complexity.high$dc <- data.complexity.high$cd

data.complexity.high$fd <- data.complexity.high$df
data.complexity.high$ed <- data.complexity.high$de

data.complexity.high$fe <- data.complexity.high$ef

data.complexity.high <- na.omit(data.complexity.high)
data.complexity.high.long <- pivot_longer(data.complexity.high, 3:32, names_to = "verhalten", values_to = "similarity")
data.complexity.high.long$verhalten2 <- data.complexity.high.long$verhalten
data.complexity.high.long$v2 <- substring(data.complexity.high.long$verhalten2, 2,)
data.complexity.high.long$v1 <- substring(data.complexity.high.long$verhalten, 1,1)

data.complexity.high.long$similarity <- as.numeric(data.complexity.high.long$similarity) 
data.complexity.high.long$similarity.2 <- (-(data.complexity.high.long$similarity) +10)

### ohne c weil ich war dumm
data.complexity.high.long.excl <- data.complexity.high.long %>% filter(!v2=="c") %>% filter(!v1=="c")

dfList.high <- split(data.complexity.high.long.excl,data.complexity.high.long.excl$ResponseId)
length(dfList.high)
names(dfList.high) <- paste0("id",1:length(dfList.high))

for(r in 1:length(dfList.high)) { 
  assign(paste0("matrix.high",r), NULL )
}


for (r in 1:length(dfList.high)) { 
  assign(paste0("matrix.high", r), dfList.high[[r]] %>% 
           mutate_at(1:2, factor, levels = unique(c(levels(.$v1), levels(.$v2)))) %>%
           xtabs(similarity.2 ~ v1 + v2, data=., sparse = FALSE) ) 
}

#### matrices ####
matrices.high<- (list(matrix.high1, matrix.high2, matrix.high3,matrix.high4,matrix.high5,matrix.high6,matrix.high7,matrix.high8,matrix.high9,matrix.high10,matrix.high11,matrix.high12,matrix.high13,matrix.high14 ,matrix.high15,
                    matrix.high16,matrix.high17,matrix.high18,matrix.high19,matrix.high20,matrix.high21,matrix.high22,matrix.high23,matrix.high24,matrix.high25,matrix.high26,matrix.high27,matrix.high28,matrix.high29,matrix.high30,
                    matrix.high31,matrix.high32,matrix.high33,matrix.high34,matrix.high35,matrix.high36,matrix.high37,matrix.high38,matrix.high39,matrix.high40,matrix.high41,matrix.high42,matrix.high43,matrix.high44,matrix.high45,
                    matrix.high46,matrix.high47,matrix.high48,matrix.high49,matrix.high50,matrix.high51,matrix.high52,matrix.high53,matrix.high54,matrix.high55,matrix.high56,matrix.high57,matrix.high58,matrix.high59,matrix.high60,
                    matrix.high61,matrix.high62,matrix.high63,matrix.high64,matrix.high65,matrix.high66,matrix.high67,matrix.high68,matrix.high69,matrix.high70,matrix.high71,matrix.high72,matrix.high73,matrix.high74,matrix.high75,
                    matrix.high76, matrix.high77,matrix.high78,matrix.high79,matrix.high80,matrix.high81,matrix.high82,matrix.high83,matrix.high84,matrix.high85,matrix.high86,matrix.high87,matrix.high88,matrix.high89,matrix.high90,
                    matrix.high91,matrix.high92,matrix.high93,matrix.high94,matrix.high95,matrix.high96,matrix.high97,matrix.high98,matrix.high99, matrix.high100,matrix.high101,matrix.high102,matrix.high103,matrix.high104,matrix.high105,
                    matrix.high106,matrix.high107,matrix.high108,matrix.high109,matrix.high110,matrix.high111,matrix.high112,matrix.high113,matrix.high114,matrix.high115,matrix.high116,matrix.high117,matrix.high118,matrix.high119,matrix.high120,
                    matrix.high121,matrix.high122,matrix.high123,matrix.high124,matrix.high125,matrix.high126,matrix.high127,matrix.high128,matrix.high129,matrix.high130,matrix.high131,matrix.high132,matrix.high133,matrix.high134,matrix.high135,
                    matrix.high136,matrix.high137,matrix.high138,matrix.high139,matrix.high140,matrix.high141,matrix.high142,matrix.high143,matrix.high144,matrix.high145,matrix.high146,matrix.high147,matrix.high148,matrix.high149,matrix.high150,
                    matrix.high151,matrix.high152,matrix.high153,matrix.high154,matrix.high155,matrix.high156,matrix.high157,matrix.high158,matrix.high159,matrix.high160,matrix.high161,matrix.high162,matrix.high163,matrix.high164,matrix.high165,
                    matrix.high166,matrix.high167,matrix.high168,matrix.high169,matrix.high170,matrix.high171,matrix.high172,matrix.high173,matrix.high174,matrix.high175,matrix.high176,matrix.high177,matrix.high178,matrix.high179,matrix.high180,
                    matrix.high181,matrix.high182,matrix.high183,matrix.high184,matrix.high185,matrix.high186,matrix.high187,matrix.high188,matrix.high189,matrix.high190, matrix.high191, matrix.high192, matrix.high193, matrix.high194, matrix.high195,
                    matrix.high196, matrix.high197, matrix.high198, matrix.high199, matrix.high200, matrix.high201, matrix.high202, matrix.high203, matrix.high204, matrix.high205, matrix.high206, matrix.high207, matrix.high208, matrix.high209, matrix.high210,
                    matrix.high211, matrix.high212 , matrix.high213, matrix.high214, matrix.high215, matrix.high216, matrix.high217, matrix.high218, matrix.high219, matrix.high220, matrix.high221, matrix.high222, matrix.high223, matrix.high224, matrix.high225,
                    matrix.high226, matrix.high227, matrix.high228, matrix.high229, matrix.high230, matrix.high231, matrix.high232, matrix.high233, matrix.high234, matrix.high235, matrix.high236, matrix.high237, matrix.high238 ))


### individual difference scaling without starting position 
scaling.high <- smacof:::indscal(matrices.high, type="ordinal", itmax=75000)
scaling.high$stress

plot.indscal.high <- as.data.frame(scaling.high$gspace)
plot.indscal.high$direction <- factor(c("pos","neg",  "neg", "pos", "pos") )
plot.indscal.high$strength <- factor(c("high","low", "high", "low", "high") )
plot.indscal.high$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco",
                               "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))
ggplot(plot.indscal.high, aes(x=D1, y=D2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + xlim(-.8, .8) +
  ylim(-.7, .7) +
  theme_minimal() +
  scale_color_manual(values=c( "darkred", "darkgreen")) +
  geom_text(aes(label = label), vjust=3, size=6)

#### for low numeracy ####
data.complexity.low <- data.complexity %>% filter(!numeracy.f=="numeracy high")

data.complexity.low$fa <- data.complexity.low$af
data.complexity.low$ea <- data.complexity.low$ae
data.complexity.low$da <- data.complexity.low$ad
data.complexity.low$ca <- data.complexity.low$ac
data.complexity.low$ba <- data.complexity.low$ab

data.complexity.low$fb <- data.complexity.low$bf
data.complexity.low$eb <- data.complexity.low$be
data.complexity.low$db <- data.complexity.low$bd
data.complexity.low$cb <- data.complexity.low$bc

data.complexity.low$fc <- data.complexity.low$cf
data.complexity.low$ec <- data.complexity.low$ce
data.complexity.low$dc <- data.complexity.low$cd

data.complexity.low$fd <- data.complexity.low$df
data.complexity.low$ed <- data.complexity.low$de

data.complexity.low$fe <- data.complexity.low$ef

data.complexity.low <- na.omit(data.complexity.low)
data.complexity.low.long <- pivot_longer(data.complexity.low, 3:32, names_to = "verhalten", values_to = "similarity")
data.complexity.low.long$verhalten2 <- data.complexity.low.long$verhalten
data.complexity.low.long$v2 <- substring(data.complexity.low.long$verhalten2, 2,)
data.complexity.low.long$v1 <- substring(data.complexity.low.long$verhalten, 1,1)

data.complexity.low.long$similarity <- as.numeric(data.complexity.low.long$similarity) 
data.complexity.low.long$similarity.2 <- (-(data.complexity.low.long$similarity) +10)

data.complexity.low.long.excl <- data.complexity.low.long %>% filter(!v2=="c") %>% filter(!v1=="c")

dfList.low <- split(data.complexity.low.long.excl,data.complexity.low.long.excl$ResponseId)
length(dfList.low)

names(dfList.low) <- paste0("id",1:length(dfList.low))

for(r in 1:length(dfList.low)) { 
  assign(paste0("matrix.low",r), NULL )
}


for (r in 1:length(dfList.low)) { 
  assign(paste0("matrix.low", r), dfList.low[[r]] %>% 
           mutate_at(1:2, factor, levels = unique(c(levels(.$v1), levels(.$v2)))) %>%
           xtabs(similarity.2 ~ v1 + v2, data=., sparse = FALSE) ) 
}

matrices.low<- (list(matrix.low1, matrix.low2, matrix.low3,matrix.low4,matrix.low5,matrix.low6,matrix.low7,matrix.low8,matrix.low9,matrix.low10,matrix.low11,matrix.low12,matrix.low13,matrix.low14 ,matrix.low15,
                    matrix.low16,matrix.low17,matrix.low18,matrix.low19,matrix.low20,matrix.low21,matrix.low22,matrix.low23,matrix.low24,matrix.low25,matrix.low26,matrix.low27,matrix.low28,matrix.low29,matrix.low30,
                    matrix.low31,matrix.low32,matrix.low33,matrix.low34,matrix.low35,matrix.low36,matrix.low37,matrix.low38,matrix.low39,matrix.low40,matrix.low41,matrix.low42,matrix.low43,matrix.low44,matrix.low45,
                    matrix.low46,matrix.low47,matrix.low48,matrix.low49,matrix.low50,matrix.low51,matrix.low52,matrix.low53,matrix.low54,matrix.low55,matrix.low56,matrix.low57,matrix.low58,matrix.low59,matrix.low60,
                    matrix.low61,matrix.low62,matrix.low63,matrix.low64,matrix.low65,matrix.low66,matrix.low67,matrix.low68,matrix.low69,matrix.low70,matrix.low71,matrix.low72,matrix.low73,matrix.low74,matrix.low75,
                    matrix.low76, matrix.low77,matrix.low78,matrix.low79,matrix.low80,matrix.low81,matrix.low82,matrix.low83,matrix.low84,matrix.low85,matrix.low86,matrix.low87,matrix.low88,matrix.low89,matrix.low90,
                    matrix.low91,matrix.low92,matrix.low93,matrix.low94,matrix.low95,matrix.low96,matrix.low97,matrix.low98,matrix.low99, matrix.low100,matrix.low101,matrix.low102,matrix.low103,matrix.low104,matrix.low105,
                    matrix.low106,matrix.low107,matrix.low108,matrix.low109,matrix.low110,matrix.low111,matrix.low112,matrix.low113,matrix.low114,matrix.low115,matrix.low116,matrix.low117,matrix.low118,matrix.low119,matrix.low120,
                    matrix.low121,matrix.low122,matrix.low123,matrix.low124,matrix.low125,matrix.low126,matrix.low127,matrix.low128,matrix.low129,matrix.low130,matrix.low131,matrix.low132,matrix.low133,matrix.low134,matrix.low135,
                    matrix.low136,matrix.low137,matrix.low138,matrix.low139,matrix.low140,matrix.low141,matrix.low142,matrix.low143,matrix.low144,matrix.low145,matrix.low146,matrix.low147,matrix.low148,matrix.low149,matrix.low150,
                    matrix.low151,matrix.low152,matrix.low153,matrix.low154,matrix.low155,matrix.low156,matrix.low157,matrix.low158,matrix.low159,matrix.low160,matrix.low161,matrix.low162,matrix.low163,matrix.low164,matrix.low165,
                    matrix.low166,matrix.low167,matrix.low168,matrix.low169,matrix.low170,matrix.low171,matrix.low172,matrix.low173,matrix.low174,matrix.low175,matrix.low176,matrix.low177,matrix.low178,matrix.low179,matrix.low180,
                    matrix.low181,matrix.low182,matrix.low183,matrix.low184,matrix.low185,matrix.low186,matrix.low187,matrix.low188,matrix.low189,matrix.low190, matrix.low191, matrix.low192, matrix.low193, matrix.low194, matrix.low195,
                    matrix.low196, matrix.low197, matrix.low198, matrix.low199, matrix.low200, matrix.low201, matrix.low202, matrix.low203, matrix.low204, matrix.low205, matrix.low206, matrix.low207, matrix.low208, matrix.low209, matrix.low210,
                    matrix.low211, matrix.low212 , matrix.low213, matrix.low214, matrix.low215, matrix.low216, matrix.low217, matrix.low218, matrix.low219, matrix.low220, matrix.low221, matrix.low222, matrix.low223, matrix.low224, matrix.low225,
                    matrix.low226, matrix.low227, matrix.low228, matrix.low229, matrix.low230, matrix.low231, matrix.low232, matrix.low233, matrix.low234, matrix.low235, matrix.low236, matrix.low237, matrix.low238, matrix.low239, matrix.low240,
                    matrix.low241, matrix.low242, matrix.low243, matrix.low244, matrix.low245, matrix.low246, matrix.low247, matrix.low248, matrix.low249, matrix.low250, matrix.low251, matrix.low252, matrix.low253, matrix.low254, matrix.low255,
                    matrix.low256, matrix.low257, matrix.low258,matrix.low259, matrix.low260, matrix.low261, matrix.low262, matrix.low263, matrix.low264, matrix.low265, matrix.low266, matrix.low267, matrix.low268, matrix.low269, matrix.low270, 
                    matrix.low271, matrix.low272, matrix.low273, matrix.low274,matrix.low275, matrix.low276, matrix.low277, matrix.low278, matrix.low279, matrix.low280, matrix.low281, matrix.low282, matrix.low283, matrix.low284, matrix.low285, 
                    matrix.low286, matrix.low287, matrix.low288, matrix.low289, matrix.low290,matrix.low291, matrix.low292, matrix.low293, matrix.low294, matrix.low295, matrix.low296, matrix.low297, matrix.low298, matrix.low299, matrix.low300, 
                    matrix.low301, matrix.low302, matrix.low303, matrix.low304, matrix.low305,matrix.low306, matrix.low307, matrix.low308, matrix.low309, matrix.low310, matrix.low311, matrix.low312, matrix.low313, matrix.low314, matrix.low315, 
                    matrix.low316, matrix.low317, matrix.low318, matrix.low319, matrix.low320, matrix.low321, matrix.low322, matrix.low323,matrix.low324, matrix.low325, matrix.low326, matrix.low327, matrix.low328, matrix.low329, matrix.low330, 
                    matrix.low331, matrix.low332, matrix.low333, matrix.low334, matrix.low335, matrix.low336, matrix.low337, matrix.low338, matrix.low339,matrix.low340, matrix.low341, matrix.low342, matrix.low343, matrix.low344, matrix.low345, 
                    matrix.low346, matrix.low347, matrix.low348, matrix.low349, matrix.low350, matrix.low351, matrix.low352, matrix.low353, matrix.low354, matrix.low355,matrix.low356, matrix.low357, matrix.low358, matrix.low359, matrix.low360, 
                    matrix.low361, matrix.low362, matrix.low363, matrix.low364, matrix.low365, matrix.low366, matrix.low367, matrix.low368, matrix.low369, matrix.low370, matrix.low371, matrix.low372,matrix.low373, matrix.low374, matrix.low375, 
                    matrix.low376, matrix.low377, matrix.low378, matrix.low379, matrix.low380, matrix.low381, matrix.low382, matrix.low383, matrix.low384, matrix.low385, matrix.low386, matrix.low387, matrix.low388, matrix.low389,matrix.low390,
                    matrix.low391, matrix.low392, matrix.low393, matrix.low394, matrix.low395, matrix.low396, matrix.low397, matrix.low398, matrix.low399, matrix.low400, matrix.low401, matrix.low402, matrix.low403, matrix.low404, matrix.low405,
                    matrix.low406, matrix.low407, matrix.low408, matrix.low409, matrix.low410, matrix.low411, matrix.low412, matrix.low413, matrix.low414, matrix.low415, matrix.low416, matrix.low417, matrix.low418, matrix.low419, matrix.low420,
                    matrix.low421, matrix.low422, matrix.low423, matrix.low424, matrix.low425, matrix.low426 , matrix.low427 , matrix.low428, matrix.low429, matrix.low430, matrix.low431, matrix.low432, matrix.low433, matrix.low434, matrix.low435,
                    matrix.low436, matrix.low437, matrix.low438, matrix.low439, matrix.low440, matrix.low441, matrix.low442, matrix.low443, matrix.low444, matrix.low445, matrix.low446, matrix.low447, matrix.low448, matrix.low449, matrix.low450,
                    matrix.low451, matrix.low452, matrix.low453, matrix.low454, matrix.low455, matrix.low456, matrix.low457, matrix.low458, matrix.low459, matrix.low460, matrix.low461, matrix.low462, matrix.low463 , matrix.low464, matrix.low465, 
                    matrix.low466, matrix.low467, matrix.low468, matrix.low469,matrix.low470, matrix.low471, matrix.low472, matrix.low473, matrix.low474, matrix.low475, matrix.low476, matrix.low477, matrix.low478, matrix.low479, matrix.low480, 
                    matrix.low481, matrix.low482, matrix.low483, matrix.low484,matrix.low485, matrix.low486, matrix.low487, matrix.low488, matrix.low489, matrix.low490, matrix.low491, matrix.low492, matrix.low493, matrix.low494, matrix.low495, 
                    matrix.low496, matrix.low497, matrix.low498, matrix.low499, matrix.low500, matrix.low501, matrix.low502, matrix.low503, matrix.low504, matrix.low505, matrix.low506, matrix.low507, matrix.low508, matrix.low509, matrix.low510, 
                    matrix.low511, matrix.low512, matrix.low513, matrix.low514,matrix.low515, matrix.low516, matrix.low517, matrix.low518, matrix.low519, matrix.low520, matrix.low521, matrix.low522, matrix.low523, matrix.low524, matrix.low525, 
                    matrix.low526, matrix.low527, matrix.low528, matrix.low529,matrix.low530, matrix.low531, matrix.low532, matrix.low533, matrix.low534, matrix.low535, matrix.low536, matrix.low537, matrix.low538, matrix.low539, matrix.low540,
                    matrix.low541, matrix.low542, matrix.low543, matrix.low544, matrix.low545,matrix.low546, matrix.low547, matrix.low548,matrix.low549, matrix.low550, matrix.low551, matrix.low552, matrix.low553, matrix.low554,matrix.low555,
                    matrix.low556, matrix.low557, matrix.low558, matrix.low559, matrix.low560, matrix.low561, matrix.low562, matrix.low563, matrix.low564, matrix.low565, matrix.low566, matrix.low567, matrix.low568, matrix.low569, matrix.low570, 
                    matrix.low571, matrix.low572, matrix.low573, matrix.low574,matrix.low575, matrix.low576, matrix.low577, matrix.low578, matrix.low579, matrix.low580, matrix.low581, matrix.low582, matrix.low583, matrix.low584, matrix.low585,
                    matrix.low586, matrix.low587, matrix.low588, matrix.low589, matrix.low590, matrix.low591, matrix.low592, matrix.low593, matrix.low594,matrix.low595, matrix.low596, matrix.low597, matrix.low598, matrix.low599, matrix.low600, 
                    matrix.low601, matrix.low602, matrix.low603, matrix.low604, matrix.low605, matrix.low606, matrix.low607, matrix.low608, matrix.low609, matrix.low610, matrix.low611, matrix.low612, matrix.low613, matrix.low614,matrix.low615, 
                    matrix.low616, matrix.low617, matrix.low618, matrix.low619, matrix.low620, matrix.low621, matrix.low622, matrix.low623, matrix.low624, matrix.low625, matrix.low626, matrix.low627, matrix.low628, matrix.low629, matrix.low630,
                    matrix.low631, matrix.low632, matrix.low633, matrix.low634, matrix.low635, matrix.low636, matrix.low637, matrix.low638, matrix.low639, matrix.low640, matrix.low641, matrix.low642, matrix.low643, matrix.low644, matrix.low645, 
                    matrix.low646, matrix.low647, matrix.low648, matrix.low649, matrix.low650, matrix.low651, matrix.low652, matrix.low653, matrix.low654, matrix.low655, matrix.low656, matrix.low657, matrix.low658, matrix.low659, matrix.low660,
                    matrix.low661, matrix.low662, matrix.low663, matrix.low664, matrix.low665, matrix.low666,matrix.low667, matrix.low668, matrix.low669, matrix.low670, matrix.low671, matrix.low672, matrix.low673, matrix.low674, matrix.low675, 
                    matrix.low676, matrix.low677, matrix.low678, matrix.low679, matrix.low680, matrix.low681, matrix.low682, matrix.low683, matrix.low684, matrix.low685, matrix.low686, matrix.low687, matrix.low688, matrix.low689, matrix.low690,
                    matrix.low691, matrix.low692, matrix.low693, matrix.low694, matrix.low695, matrix.low696, matrix.low697, matrix.low698, matrix.low699, matrix.low700, matrix.low701, matrix.low702, matrix.low703, matrix.low704, matrix.low705,
                    matrix.low706, matrix.low707, matrix.low708, matrix.low709, matrix.low710, matrix.low711, matrix.low712, matrix.low713, matrix.low714, matrix.low715, matrix.low716, matrix.low717, matrix.low718, matrix.low719, matrix.low720, 
                    matrix.low721, matrix.low722, matrix.low723, matrix.low724, matrix.low725, matrix.low726, matrix.low727, matrix.low728, matrix.low729, matrix.low730, matrix.low731, matrix.low732, matrix.low733, matrix.low734, matrix.low735,
                    matrix.low736, matrix.low737, matrix.low738, matrix.low739, matrix.low740, matrix.low741, matrix.low742, matrix.low743, matrix.low744, matrix.low745, matrix.low746, matrix.low747, matrix.low748, matrix.low749, matrix.low750,
                    matrix.low751, matrix.low752, matrix.low753, matrix.low754, matrix.low755, matrix.low756, matrix.low757, matrix.low758, matrix.low759, matrix.low760, matrix.low761, matrix.low762, matrix.low763, matrix.low764, matrix.low765,
                    matrix.low766, matrix.low767, matrix.low768, matrix.low769, matrix.low770, matrix.low771, matrix.low772, matrix.low773, matrix.low774, matrix.low775, matrix.low776, matrix.low777, matrix.low778, matrix.low779, matrix.low780,
                    matrix.low781, matrix.low782, matrix.low783, matrix.low784, matrix.low785, matrix.low786, matrix.low787, matrix.low788, matrix.low789, matrix.low790, matrix.low791, matrix.low792, matrix.low793, matrix.low794, matrix.low795,
                    matrix.low796, matrix.low797, matrix.low798, matrix.low799, matrix.low800, matrix.low801, matrix.low802, matrix.low803, matrix.low804, matrix.low805, matrix.low806, matrix.low807, matrix.low808, matrix.low809, matrix.low810,
                    matrix.low811, matrix.low812, matrix.low813, matrix.low814, matrix.low815, matrix.low816, matrix.low817, matrix.low818, matrix.low819, matrix.low820, matrix.low821, matrix.low822, matrix.low823, matrix.low824, matrix.low825,
                    matrix.low826, matrix.low827, matrix.low828, matrix.low829, matrix.low830, matrix.low831, matrix.low832, matrix.low833, matrix.low834, matrix.low835, matrix.low836, matrix.low837, matrix.low838, matrix.low839, matrix.low840,
                    matrix.low841, matrix.low842, matrix.low843, matrix.low844, matrix.low845, matrix.low846, matrix.low847, matrix.low848, matrix.low849, matrix.low850, matrix.low851, matrix.low852, matrix.low853, matrix.low854, matrix.low855,
                    matrix.low856, matrix.low857, matrix.low858, matrix.low859 ))

### individual difference scaling without starting position 
scaling.low <- smacof:::indscal(matrices.low, type="ordinal", itmax=75000)
scaling.low$stress

plot.indscal.low <- as.data.frame(scaling.low$gspace)
plot.indscal.low$direction <- factor(c("pos","neg",  "neg", "pos", "pos") )
plot.indscal.low$strength <- factor(c("high","low", "high", "low", "high") )
plot.indscal.low$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco",
                               "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))
ggplot(plot.indscal.low, aes(x=D1, y=D2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + xlim(-.8, .8) +
  ylim(-.7, .7) +
  theme_minimal() +
  scale_color_manual(values=c( "darkred", "darkgreen")) +
  geom_text(aes(label = label), vjust=3, size=6)



