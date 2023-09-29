###

data.raw <- read.csv(file.choose())
data.raw <- data.raw[3:nrow(data.raw), ]

data.experiment <-data.raw %>% filter(Group=="Experiment")
data.control <-data.raw %>% filter(Group=="Control")

data.complexity <- data.raw %>% select(ResponseId,ac:jl)
#data.complexity <- data.control %>% select(ResponseId,ac:jl)

data.complexity[data.complexity==""] <- NA
data.complexity <- na.omit(data.complexity)

data.complexity$la <- data.complexity$al
data.complexity$ka <- data.complexity$ak
data.complexity$ja <- data.complexity$aj
data.complexity$ia <- data.complexity$ai
data.complexity$ha <- data.complexity$ah
data.complexity$ga <- data.complexity$ag
data.complexity$fa <- data.complexity$af
data.complexity$ea <- data.complexity$ae
data.complexity$da <- data.complexity$ad
data.complexity$ca <- data.complexity$ac
data.complexity$ba <- data.complexity$ab

data.complexity$lb <- data.complexity$bl
data.complexity$kb <- data.complexity$bk
data.complexity$jb <- data.complexity$bj
data.complexity$ib <- data.complexity$bi
data.complexity$hb <- data.complexity$bh
data.complexity$gb <- data.complexity$bg
data.complexity$fb <- data.complexity$bf
data.complexity$eb <- data.complexity$be
data.complexity$db <- data.complexity$bd
data.complexity$cb <- data.complexity$bc

data.complexity$lc <- data.complexity$cl
data.complexity$kc <- data.complexity$ck
data.complexity$jc <- data.complexity$cj
data.complexity$ic <- data.complexity$ci
data.complexity$hc <- data.complexity$ch
data.complexity$gc <- data.complexity$cg
data.complexity$fc <- data.complexity$cf
data.complexity$ec <- data.complexity$ce
data.complexity$dc <- data.complexity$cd

data.complexity$ld <- data.complexity$dl
data.complexity$kd <- data.complexity$dk
data.complexity$jd <- data.complexity$dj
data.complexity$id <- data.complexity$di
data.complexity$hd <- data.complexity$dh
data.complexity$gd <- data.complexity$dg
data.complexity$fd <- data.complexity$df
data.complexity$ed <- data.complexity$de

data.complexity$le <- data.complexity$el
data.complexity$ke <- data.complexity$ek
data.complexity$je <- data.complexity$ej
data.complexity$ie <- data.complexity$ei
data.complexity$he <- data.complexity$eh
data.complexity$ge <- data.complexity$eg
data.complexity$fe <- data.complexity$ef

data.complexity$lf <- data.complexity$fl
data.complexity$kf <- data.complexity$fk
data.complexity$jf <- data.complexity$fj
data.complexity$"if" <- data.complexity$fi
data.complexity$hf <- data.complexity$fh
data.complexity$gf <- data.complexity$fg

data.complexity$lg <- data.complexity$gl
data.complexity$kg <- data.complexity$gk
data.complexity$jg <- data.complexity$gj
data.complexity$ig <- data.complexity$gi
data.complexity$hg <- data.complexity$gh

data.complexity$lh <- data.complexity$hl
data.complexity$kh <- data.complexity$hk
data.complexity$jh <- data.complexity$hj
data.complexity$ih <- data.complexity$hi

data.complexity$li <- data.complexity$il
data.complexity$ki <- data.complexity$ik
data.complexity$ji <- data.complexity$ij

data.complexity$lj <- data.complexity$jl
data.complexity$kj <- data.complexity$jk

data.complexity$lk <- data.complexity$kl


data.complexity <- data.complexity %>% select(!bad.quality.2) 
data.complexity.long <- pivot_longer(data.complexity, 2:133, names_to = "verhalten", values_to = "similarity")
data.complexity.long$verhalten2 <- data.complexity.long$verhalten
data.complexity.long$v2 <- substring(data.complexity.long$verhalten2, 2,)
data.complexity.long$v1 <- substring(data.complexity.long$verhalten, 1,1)

data.complexity.long$similarity <- as.numeric(data.complexity.long$similarity) 
data.complexity.long$similarity.2 <- (-(data.complexity.long$similarity) +10)

dfList <- split(data.complexity.long,data.complexity.long$ResponseId)
length(dfList)
names(dfList) <- paste0("id",1:511)

for (r in 1:length(dfList)) { 
  assign(paste0("matrix",r), NULL )
}


for (r in 1:length(dfList)) { 
  assign(paste0("matrix", r), dfList[[r]] %>% 
           mutate_at(1:2, factor, levels = unique(c(levels(.$v1), levels(.$v2)))) %>%
           xtabs(similarity.2 ~ v1 + v2, data=., sparse = FALSE) ) 
}

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
                  matrix511 ))

scaling <- smacof:::indscal(matrices, type="ratio",  itmax=75000)
# scale the matrix?????

matrices.2 <- matrices
for (r in 1:length(dfList)){ 
  matrices.2[[r]] <- dist(matrices.2[[r]], method ="euclidean")
}


scaling.euc <- smacof:::idioscal(matrices, type="ordinal",  itmax=75000)

plot(scaling, type = "p", pch = 25, col = 4, label.conf = list(label = TRUE, pos = 3, col = 4))


plot.m <- as.data.frame(scaling.euc$gspace)
plot.m$direction <- factor(c("pos","pos","pos",  "pos", "pos", "pos",
                             "neg", "neg", "neg", "neg", "neg", "neg") )
plot.m$strength <- factor(c("high","high", "high", 
                            "alow","alow","alow", 
                            "high", "high","high", 
                            "alow","alow","alow" ) )
plot.m$label <- factor(c("change 12 year old car", 
                         "ebike vs car for >15km",
                         "meeting skype vs fly Bremen-lisbon",
                         "not use AC","work home vs drive 20km",
                         "turn of engine at red lights",
                         "vacation fly teneriffa vs drive ostsee",
                         "car vs train for 50km per day",
                         "get second car",
                         "to little tire pressure",
                         "use dachträger if not necessary",
                         "car vs bike") )


ggplot(plot.m, aes(x=D1, y=D2, color=direction)) +
  geom_point(aes(size= strength ) ) + xlim(-.9, .9) +
  ylim(-.9, .9) +
  theme_minimal() +
  geom_text(aes(label = label), vjust=3, size=4)

###
individual.space <- scaling.euc$conf
individual.space.2 <- matrix(data=1, nrow=511, ncol=24)

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
  
  individual.space.2[r,13] <-  individual.space [[c(r,13)]] ;
  individual.space.2[r,14] <- individual.space [[c(r,14)]];
  individual.space.2[r,15] <-  individual.space [[c(r,15)]] ;
  individual.space.2[r,16] <-  individual.space [[c(r,16)]] ;
  individual.space.2[r,17] <- individual.space [[c(r,17)]];
  individual.space.2[r,18] <-  individual.space [[c(r,18)]] ; 
  individual.space.2[r,19] <-  individual.space [[c(r,19)]] ;
  individual.space.2[r,20] <- individual.space [[c(r,20)]];
  individual.space.2[r,21] <-  individual.space [[c(r,21)]] ; 
  individual.space.2[r,22] <-  individual.space [[c(r,22)]] ;
  individual.space.2[r,23] <- individual.space [[c(r,23)]];
  individual.space.2[r,24] <-  individual.space [[c(r,24)]] ;
  
  
}

individual.space.2 <- as.data.frame(individual.space.2)
individual.space.long <- pivot_longer(individual.space.2, 1:12, names_to = "x", values_to = "x.value")
individual.space.long.2 <- pivot_longer(individual.space.long, 1:12, names_to = "y", values_to = "y.value")

individual.space.long.2$keep <- paste0(individual.space.long.2$x, individual.space.long.2$y)
individual.space.long.2 <- individual.space.long.2 %>% 
  filter(keep =="V1V13" | keep=="V2V14" | keep=="V3V15" |keep =="V4V16" | keep=="V5V17" | keep=="V6V18"  |
           keep =="V7V19" | keep=="V8V20" | keep=="V9V21"  | keep=="V10V22" | keep=="V11V23"| keep=="V12V24"   ) 
individual.space.long.2$keep <- factor(individual.space.long.2$keep, levels=c("V1V13" ,"V2V14" ,"V3V15","V4V16","V5V17","V6V18",
                                                                              "V7V19" ,"V8V20" ,"V9V21","V10V22","V11V23","V12V24") ,
                                       labels=(c("change 12 year old car", 
                                                       "ebike vs car for >15km",
                                                       "meeting skype vs fly Bremen-lisbon",
                                                       "not use AC","work home vs drive 20km",
                                                       "turn of engine at red lights",
                                                       "vacation fly teneriffa vs drive ostsee",
                                                       "car vs train for 50km per day",
                                                       "get second car",
                                                       "to little tire pressure",
                                                       "use dachträger if not necessary",
                                                       "car vs bike") ) )

ggplot(individual.space.long.2, aes(x=x.value, y=y.value, color=keep)) +
  geom_jitter() + labs(x="dimension 1", y="dimension 2", color=NULL) +
  theme_minimal() + scale_color_viridis_d()  +
  theme(strip.text = element_text(size = 15),
        legend.text = element_text(size=14))

### weights for dimensions
weights.scal.start <- as.matrix(scaling$cweights)

individual.weights <- matrix(data=1, nrow=511, ncol=2)

for(r in 1:length(weights.scal.start)) { 
  individual.weights[r,2] <-  weights.scal.start [[c(r,4)]] ;
  individual.weights[r,1] <- weights.scal.start [[c(r,1)]] 
}

## V1 = D1, V2 = D2
individual.weights.2 <- as.data.frame(individual.weights)
individual.weights.2$difference <- individual.weights.2$V1-individual.weights.2$V2
individual.weights.2$difference <- factor(ifelse(individual.weights.2$difference < 0, 1, 0))
individual.weights.2$sum <- individual.weights.2$V1 + individual.weights.2$V2
individual.weights.2$mean <- ((individual.weights.2$V1+individual.weights.2$V2)/2)

range(individual.weights.2$V1)
range(individual.weights.2$V2)

ggplot(individual.weights.2, aes(x=V1, y=V2)) +
  geom_point() + # xlim(0,2) + ylim(0,2) +
  theme_minimal()
      
individual.weights.2$ResponseId <- unique(data.complexity.long$ResponseId)
table(individual.weights.2$difference)


