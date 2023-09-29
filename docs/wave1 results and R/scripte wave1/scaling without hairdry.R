

## complexity without hairdryer

##### complexity task ######
# prepare so matrix is symmetrical
data.complexity.long <- read_csv("data/data.complexity.long.csv")
data.complexity.long <- data.complexity.long[,-c(1)]
data.complexity.long <-data.complexity.long %>% filter(!v1=="c") %>% filter (!v2=="c")

dfList.e <- split(data.complexity.long,data.complexity.long$ResponseId)
length(dfList.e)
names(dfList.e) <- paste0("id",1:length(dfList.e))

for(r in 1:length(dfList.e)) { 
  assign(paste0("matrix",r), NULL )
}


for (r in 1:length(dfList.e)) { 
  assign(paste0("matrix.e", r), dfList.e[[r]] %>% 
           mutate_at(1:2, factor, levels = unique(c(levels(.$v1), levels(.$v2)))) %>%
           xtabs(similarity.2 ~ v1 + v2, data=., sparse = FALSE) ) 
}

#### matrices ####
matrices.e <- (list(matrix.e1, matrix.e2, matrix.e3,matrix.e4,matrix.e5,matrix.e6,matrix.e7,matrix.e8,matrix.e9,matrix.e10,matrix.e11,matrix.e12,matrix.e13,matrix.e14 ,matrix.e15,
                  matrix.e16,matrix.e17,matrix.e18,matrix.e19,matrix.e20,matrix.e21,matrix.e22,matrix.e23,matrix.e24,matrix.e25,matrix.e26,matrix.e27,matrix.e28,matrix.e29,matrix.e30,
                  matrix.e31,matrix.e32,matrix.e33,matrix.e34,matrix.e35,matrix.e36,matrix.e37,matrix.e38,matrix.e39,matrix.e40,matrix.e41,matrix.e42,matrix.e43,matrix.e44,matrix.e45,
                  matrix.e46,matrix.e47,matrix.e48,matrix.e49,matrix.e50,matrix.e51,matrix.e52,matrix.e53,matrix.e54,matrix.e55,matrix.e56,matrix.e57,matrix.e58,matrix.e59,matrix.e60,
                  matrix.e61,matrix.e62,matrix.e63,matrix.e64,matrix.e65,matrix.e66,matrix.e67,matrix.e68,matrix.e69,matrix.e70,matrix.e71,matrix.e72,matrix.e73,matrix.e74,matrix.e75,
                  matrix.e76, matrix.e77,matrix.e78,matrix.e79,matrix.e80,matrix.e81,matrix.e82,matrix.e83,matrix.e84,matrix.e85,matrix.e86,matrix.e87,matrix.e88,matrix.e89,matrix.e90,
                  matrix.e91,matrix.e92,matrix.e93,matrix.e94,matrix.e95,matrix.e96,matrix.e97,matrix.e98,matrix.e99, matrix.e100,matrix.e101,matrix.e102,matrix.e103,matrix.e104,matrix.e105,
                  matrix.e106,matrix.e107,matrix.e108,matrix.e109,matrix.e110,matrix.e111,matrix.e112,matrix.e113,matrix.e114,matrix.e115,matrix.e116,matrix.e117,matrix.e118,matrix.e119,matrix.e120,
                  matrix.e121,matrix.e122,matrix.e123,matrix.e124,matrix.e125,matrix.e126,matrix.e127,matrix.e128,matrix.e129,matrix.e130,matrix.e131,matrix.e132,matrix.e133,matrix.e134,matrix.e135,
                  matrix.e136,matrix.e137,matrix.e138,matrix.e139,matrix.e140,matrix.e141,matrix.e142,matrix.e143,matrix.e144,matrix.e145,matrix.e146,matrix.e147,matrix.e148,matrix.e149,matrix.e150,
                  matrix.e151,matrix.e152,matrix.e153,matrix.e154,matrix.e155,matrix.e156,matrix.e157,matrix.e158,matrix.e159,matrix.e160,matrix.e161,matrix.e162,matrix.e163,matrix.e164,matrix.e165,
                  matrix.e166,matrix.e167,matrix.e168,matrix.e169,matrix.e170,matrix.e171,matrix.e172,matrix.e173,matrix.e174,matrix.e175,matrix.e176,matrix.e177,matrix.e178,matrix.e179,matrix.e180,
                  matrix.e181,matrix.e182,matrix.e183,matrix.e184,matrix.e185,matrix.e186,matrix.e187,matrix.e188,matrix.e189,matrix.e190, matrix.e191, matrix.e192, matrix.e193, matrix.e194, matrix.e195,
                  matrix.e196, matrix.e197, matrix.e198, matrix.e199, matrix.e200, matrix.e201, matrix.e202, matrix.e203, matrix.e204, matrix.e205, matrix.e206, matrix.e207, matrix.e208, matrix.e209, matrix.e210,
                  matrix.e211, matrix.e212 , matrix.e213, matrix.e214, matrix.e215, matrix.e216, matrix.e217, matrix.e218, matrix.e219, matrix.e220, matrix.e221, matrix.e222, matrix.e223, matrix.e224, matrix.e225,
                  matrix.e226, matrix.e227, matrix.e228, matrix.e229, matrix.e230, matrix.e231, matrix.e232, matrix.e233, matrix.e234, matrix.e235, matrix.e236, matrix.e237, matrix.e238, matrix.e239, matrix.e240,
                  matrix.e241, matrix.e242, matrix.e243, matrix.e244, matrix.e245, matrix.e246, matrix.e247, matrix.e248, matrix.e249, matrix.e250, matrix.e251, matrix.e252, matrix.e253, matrix.e254, matrix.e255,
                  matrix.e256, matrix.e257, matrix.e258,matrix.e259, matrix.e260, matrix.e261, matrix.e262, matrix.e263, matrix.e264, matrix.e265, matrix.e266, matrix.e267, matrix.e268, matrix.e269, matrix.e270, 
                  matrix.e271, matrix.e272, matrix.e273, matrix.e274,matrix.e275, matrix.e276, matrix.e277, matrix.e278, matrix.e279, matrix.e280, matrix.e281, matrix.e282, matrix.e283, matrix.e284, matrix.e285, 
                  matrix.e286, matrix.e287, matrix.e288, matrix.e289, matrix.e290,matrix.e291, matrix.e292, matrix.e293, matrix.e294, matrix.e295, matrix.e296, matrix.e297, matrix.e298, matrix.e299, matrix.e300, 
                  matrix.e301, matrix.e302, matrix.e303, matrix.e304, matrix.e305,matrix.e306, matrix.e307, matrix.e308, matrix.e309, matrix.e310, matrix.e311, matrix.e312, matrix.e313, matrix.e314, matrix.e315, 
                  matrix.e316, matrix.e317, matrix.e318, matrix.e319, matrix.e320, matrix.e321, matrix.e322, matrix.e323,matrix.e324, matrix.e325, matrix.e326, matrix.e327, matrix.e328, matrix.e329, matrix.e330, 
                  matrix.e331, matrix.e332, matrix.e333, matrix.e334, matrix.e335, matrix.e336, matrix.e337, matrix.e338, matrix.e339,matrix.e340, matrix.e341, matrix.e342, matrix.e343, matrix.e344, matrix.e345, 
                  matrix.e346, matrix.e347, matrix.e348, matrix.e349, matrix.e350, matrix.e351, matrix.e352, matrix.e353, matrix.e354, matrix.e355,matrix.e356, matrix.e357, matrix.e358, matrix.e359, matrix.e360, 
                  matrix.e361, matrix.e362, matrix.e363, matrix.e364, matrix.e365, matrix.e366, matrix.e367, matrix.e368, matrix.e369, matrix.e370, matrix.e371, matrix.e372,matrix.e373, matrix.e374, matrix.e375, 
                  matrix.e376, matrix.e377, matrix.e378, matrix.e379, matrix.e380, matrix.e381, matrix.e382, matrix.e383, matrix.e384, matrix.e385, matrix.e386, matrix.e387, matrix.e388, matrix.e389,matrix.e390,
                  matrix.e391, matrix.e392, matrix.e393, matrix.e394, matrix.e395, matrix.e396, matrix.e397, matrix.e398, matrix.e399, matrix.e400, matrix.e401, matrix.e402, matrix.e403, matrix.e404, matrix.e405,
                  matrix.e406, matrix.e407, matrix.e408, matrix.e409, matrix.e410, matrix.e411, matrix.e412, matrix.e413, matrix.e414, matrix.e415, matrix.e416, matrix.e417, matrix.e418, matrix.e419, matrix.e420,
                  matrix.e421, matrix.e422, matrix.e423, matrix.e424, matrix.e425, matrix.e426 , matrix.e427 , matrix.e428, matrix.e429, matrix.e430, matrix.e431, matrix.e432, matrix.e433, matrix.e434, matrix.e435,
                  matrix.e436, matrix.e437, matrix.e438, matrix.e439, matrix.e440, matrix.e441, matrix.e442, matrix.e443, matrix.e444, matrix.e445, matrix.e446, matrix.e447, matrix.e448, matrix.e449, matrix.e450,
                  matrix.e451, matrix.e452, matrix.e453, matrix.e454, matrix.e455, matrix.e456, matrix.e457, matrix.e458, matrix.e459, matrix.e460, matrix.e461, matrix.e462, matrix.e463 , matrix.e464, matrix.e465, 
                  matrix.e466, matrix.e467, matrix.e468, matrix.e469,matrix.e470, matrix.e471, matrix.e472, matrix.e473, matrix.e474, matrix.e475, matrix.e476, matrix.e477, matrix.e478, matrix.e479, matrix.e480, 
                  matrix.e481, matrix.e482, matrix.e483, matrix.e484,matrix.e485, matrix.e486, matrix.e487, matrix.e488, matrix.e489, matrix.e490, matrix.e491, matrix.e492, matrix.e493, matrix.e494, matrix.e495, 
                  matrix.e496, matrix.e497, matrix.e498, matrix.e499, matrix.e500, matrix.e501, matrix.e502, matrix.e503, matrix.e504, matrix.e505, matrix.e506, matrix.e507, matrix.e508, matrix.e509, matrix.e510, 
                  matrix.e511, matrix.e512, matrix.e513, matrix.e514,matrix.e515, matrix.e516, matrix.e517, matrix.e518, matrix.e519, matrix.e520, matrix.e521, matrix.e522, matrix.e523, matrix.e524, matrix.e525, 
                  matrix.e526, matrix.e527, matrix.e528, matrix.e529,matrix.e530, matrix.e531, matrix.e532, matrix.e533, matrix.e534, matrix.e535, matrix.e536, matrix.e537, matrix.e538, matrix.e539, matrix.e540,
                  matrix.e541, matrix.e542, matrix.e543, matrix.e544, matrix.e545,matrix.e546, matrix.e547, matrix.e548,matrix.e549, matrix.e550, matrix.e551, matrix.e552, matrix.e553, matrix.e554,matrix.e555,
                  matrix.e556, matrix.e557, matrix.e558, matrix.e559, matrix.e560, matrix.e561, matrix.e562, matrix.e563, matrix.e564, matrix.e565, matrix.e566, matrix.e567, matrix.e568, matrix.e569, matrix.e570, 
                  matrix.e571, matrix.e572, matrix.e573, matrix.e574,matrix.e575, matrix.e576, matrix.e577, matrix.e578, matrix.e579, matrix.e580, matrix.e581, matrix.e582, matrix.e583, matrix.e584, matrix.e585,
                  matrix.e586, matrix.e587, matrix.e588, matrix.e589, matrix.e590, matrix.e591, matrix.e592, matrix.e593, matrix.e594,matrix.e595, matrix.e596, matrix.e597, matrix.e598, matrix.e599, matrix.e600, 
                  matrix.e601, matrix.e602, matrix.e603, matrix.e604, matrix.e605, matrix.e606, matrix.e607, matrix.e608, matrix.e609, matrix.e610, matrix.e611, matrix.e612, matrix.e613, matrix.e614,matrix.e615, 
                  matrix.e616, matrix.e617, matrix.e618, matrix.e619, matrix.e620, matrix.e621, matrix.e622, matrix.e623, matrix.e624, matrix.e625, matrix.e626, matrix.e627, matrix.e628, matrix.e629, matrix.e630,
                  matrix.e631, matrix.e632, matrix.e633, matrix.e634, matrix.e635, matrix.e636, matrix.e637, matrix.e638, matrix.e639, matrix.e640, matrix.e641, matrix.e642, matrix.e643, matrix.e644, matrix.e645, 
                  matrix.e646, matrix.e647, matrix.e648, matrix.e649, matrix.e650, matrix.e651, matrix.e652, matrix.e653, matrix.e654, matrix.e655, matrix.e656, matrix.e657, matrix.e658, matrix.e659, matrix.e660,
                  matrix.e661, matrix.e662, matrix.e663, matrix.e664, matrix.e665, matrix.e666,matrix.e667, matrix.e668, matrix.e669, matrix.e670, matrix.e671, matrix.e672, matrix.e673, matrix.e674, matrix.e675, 
                  matrix.e676, matrix.e677, matrix.e678, matrix.e679, matrix.e680, matrix.e681, matrix.e682, matrix.e683, matrix.e684, matrix.e685, matrix.e686, matrix.e687, matrix.e688, matrix.e689, matrix.e690,
                  matrix.e691, matrix.e692, matrix.e693, matrix.e694, matrix.e695, matrix.e696, matrix.e697, matrix.e698, matrix.e699, matrix.e700, matrix.e701, matrix.e702, matrix.e703, matrix.e704, matrix.e705,
                  matrix.e706, matrix.e707, matrix.e708, matrix.e709, matrix.e710, matrix.e711, matrix.e712, matrix.e713, matrix.e714, matrix.e715, matrix.e716, matrix.e717, matrix.e718, matrix.e719, matrix.e720, 
                  matrix.e721, matrix.e722, matrix.e723, matrix.e724, matrix.e725, matrix.e726, matrix.e727, matrix.e728, matrix.e729, matrix.e730, matrix.e731, matrix.e732, matrix.e733, matrix.e734, matrix.e735,
                  matrix.e736, matrix.e737, matrix.e738, matrix.e739, matrix.e740, matrix.e741, matrix.e742, matrix.e743, matrix.e744, matrix.e745, matrix.e746, matrix.e747, matrix.e748, matrix.e749, matrix.e750,
                  matrix.e751, matrix.e752, matrix.e753, matrix.e754, matrix.e755, matrix.e756, matrix.e757, matrix.e758, matrix.e759, matrix.e760, matrix.e761, matrix.e762, matrix.e763, matrix.e764, matrix.e765,
                  matrix.e766, matrix.e767, matrix.e768, matrix.e769, matrix.e770, matrix.e771, matrix.e772, matrix.e773, matrix.e774, matrix.e775, matrix.e776, matrix.e777, matrix.e778, matrix.e779, matrix.e780,
                  matrix.e781, matrix.e782, matrix.e783, matrix.e784, matrix.e785, matrix.e786, matrix.e787, matrix.e788, matrix.e789, matrix.e790, matrix.e791, matrix.e792, matrix.e793, matrix.e794, matrix.e795,
                  matrix.e796, matrix.e797, matrix.e798, matrix.e799, matrix.e800, matrix.e801, matrix.e802, matrix.e803, matrix.e804, matrix.e805, matrix.e806, matrix.e807, matrix.e808, matrix.e809, matrix.e810,
                  matrix.e811, matrix.e812, matrix.e813, matrix.e814, matrix.e815, matrix.e816, matrix.e817, matrix.e818, matrix.e819, matrix.e820, matrix.e821, matrix.e822, matrix.e823, matrix.e824, matrix.e825,
                  matrix.e826, matrix.e827, matrix.e828, matrix.e829, matrix.e830, matrix.e831, matrix.e832, matrix.e833, matrix.e834, matrix.e835, matrix.e836, matrix.e837, matrix.e838, matrix.e839, matrix.e840,
                  matrix.e841, matrix.e842, matrix.e843, matrix.e844, matrix.e845, matrix.e846, matrix.e847, matrix.e848, matrix.e849, matrix.e850, matrix.e851, matrix.e852, matrix.e853, matrix.e854, matrix.e855,
                  matrix.e856, matrix.e857, matrix.e858, matrix.e859, matrix.e860, matrix.e861, matrix.e862, matrix.e863, matrix.e864, matrix.e865, matrix.e866, matrix.e867, matrix.e868, matrix.e869, matrix.e870,
                  matrix.e871, matrix.e872, matrix.e873, matrix.e874, matrix.e875, matrix.e876, matrix.e877, matrix.e878, matrix.e879, matrix.e880, matrix.e881, matrix.e882, matrix.e883, matrix.e884, matrix.e885,
                  matrix.e886, matrix.e887, matrix.e888, matrix.e889, matrix.e890, matrix.e891, matrix.e892, matrix.e893, matrix.e894, matrix.e895, matrix.e896, matrix.e897, matrix.e898, matrix.e899, matrix.e900,
                  matrix.e901, matrix.e902, matrix.e903, matrix.e904, matrix.e905, matrix.e906, matrix.e907, matrix.e908, matrix.e909, matrix.e910, matrix.e911, matrix.e912, matrix.e913, matrix.e914, matrix.e915,
                  matrix.e916, matrix.e917, matrix.e918, matrix.e919, matrix.e920, matrix.e921, matrix.e922, matrix.e923, matrix.e924, matrix.e925, matrix.e926, matrix.e927, matrix.e928, matrix.e929, matrix.e930,
                  matrix.e931, matrix.e932, matrix.e933, matrix.e934, matrix.e935, matrix.e936, matrix.e937, matrix.e938, matrix.e939, matrix.e940, matrix.e941, matrix.e942, matrix.e943, matrix.e944, matrix.e945,
                  matrix.e946, matrix.e947, matrix.e948, matrix.e949, matrix.e950, matrix.e951, matrix.e952, matrix.e953, matrix.e954, matrix.e955, matrix.e956, matrix.e957, matrix.e958, matrix.e959, matrix.e960,
                  matrix.e961, matrix.e962, matrix.e963, matrix.e964, matrix.e965, matrix.e966, matrix.e967, matrix.e968, matrix.e969, matrix.e970, matrix.e971, matrix.e972, matrix.e973, matrix.e974, matrix.e975,
                  matrix.e976, matrix.e977, matrix.e978, matrix.e979, matrix.e980, matrix.e981, matrix.e982, matrix.e983, matrix.e984, matrix.e985, matrix.e986, matrix.e987, matrix.e988, matrix.e989, matrix.e990, 
                  matrix.e991, matrix.e992, matrix.e993, matrix.e994, matrix.e995, matrix.e996, matrix.e997, matrix.e998, matrix.e999, matrix.e1000, matrix.e1001, matrix.e1002, matrix.e1003, matrix.e1004, matrix.e1005,
                  matrix.e1006, matrix.e1007, matrix.e1008, matrix.e1009, matrix.e1010, matrix.e1011, matrix.e1012, matrix.e1013, matrix.e1014, matrix.e1015, matrix.e1016, matrix.e1017, matrix.e1018, matrix.e1019, matrix.e1020,
                  matrix.e1021, matrix.e1022, matrix.e1023, matrix.e1024, matrix.e1025, matrix.e1026, matrix.e1027, matrix.e1028, matrix.e1029, matrix.e1030, matrix.e1031, matrix.e1032, matrix.e1033, matrix.e1034, matrix.e1035, 
                  matrix.e1036, matrix.e1037, matrix.e1038, matrix.e1039, matrix.e1040, matrix.e1041, matrix.e1042, matrix.e1043, matrix.e1044, matrix.e1045, matrix.e1046, matrix.e1047, matrix.e1048, matrix.e1049, matrix.e1050,
                  matrix.e1051, matrix.e1052, matrix.e1053, matrix.e1054, matrix.e1055, matrix.e1056, matrix.e1057, matrix.e1058, matrix.e1059, matrix.e1060, matrix.e1061, matrix.e1062, matrix.e1063, matrix.e1064, matrix.e1065,
                  matrix.e1066, matrix.e1067, matrix.e1068, matrix.e1069, matrix.e1070, matrix.e1071, matrix.e1072, matrix.e1073, matrix.e1074, matrix.e1075, matrix.e1076, matrix.e1077, matrix.e1078, matrix.e1079, matrix.e1080, 
                  matrix.e1081, matrix.e1082, matrix.e1083, matrix.e1084, matrix.e1085, matrix.e1086, matrix.e1087, matrix.e1088, matrix.e1089, matrix.e1090, matrix.e1091, matrix.e1092, matrix.e1093, matrix.e1094, matrix.e1095, 
                  matrix.e1096, matrix.e1097, matrix.e1098, matrix.e1099, matrix.e1100, matrix.e1101, matrix.e1102, matrix.e1103, matrix.e1104, matrix.e1105, matrix.e1106, matrix.e1107, matrix.e1108, matrix.e1109, matrix.e1110, 
                  matrix.e1111, matrix.e1112, matrix.e1113, matrix.e1114, matrix.e1115, matrix.e1116, matrix.e1117, matrix.e1118, matrix.e1119, matrix.e1120, matrix.e1121, matrix.e1122, matrix.e1123, matrix.e1124, matrix.e1125,
                  matrix.e1126, matrix.e1127, matrix.e1128, matrix.e1129, matrix.e1130, matrix.e1131, matrix.e1132, matrix.e1133, matrix.e1134, matrix.e1135, matrix.e1136, matrix.e1137, matrix.e1138, matrix.e1139, matrix.e1140, 
                  matrix.e1141, matrix.e1142, matrix.e1143, matrix.e1144, matrix.e1145, matrix.e1146, matrix.e1147, matrix.e1148, matrix.e1149, matrix.e1150, matrix.e1151, matrix.e1152, matrix.e1153, matrix.e1154, matrix.e1155,
                  matrix.e1156, matrix.e1157, matrix.e1158, matrix.e1159, matrix.e1160, matrix.e1161, matrix.e1162, matrix.e1163, matrix.e1164, matrix.e1165, matrix.e1166, matrix.e1167, matrix.e1168, matrix.e1169, matrix.e1170,
                  matrix.e1171, matrix.e1172, matrix.e1173, matrix.e1174, matrix.e1175, matrix.e1176, matrix.e1177, matrix.e1178, matrix.e1179, matrix.e1180, matrix.e1181, matrix.e1182, matrix.e1183, matrix.e1184, matrix.e1185,
                  matrix.e1186, matrix.e1187, matrix.e1188, matrix.e1189, matrix.e1190, matrix.e1191, matrix.e1192, matrix.e1193, matrix.e1194, matrix.e1195, matrix.e1196, matrix.e1197, matrix.e1198, matrix.e1199, matrix.e1200, 
                  matrix.e1201	, matrix.e1202	, matrix.e1203	, matrix.e1204	, matrix.e1205	, matrix.e1206	, matrix.e1207	, matrix.e1208	, matrix.e1209	, matrix.e1210	, matrix.e1211	, matrix.e1212	, matrix.e1213	, matrix.e1214	, matrix.e1215	, 
                  matrix.e1216	, matrix.e1217	, matrix.e1218	, matrix.e1219	, matrix.e1220	, matrix.e1221	, matrix.e1222	, matrix.e1223	, matrix.e1224	, matrix.e1225	, matrix.e1226	, matrix.e1227	, matrix.e1228	, matrix.e1229	, matrix.e1230	,
                  matrix.e1231	, matrix.e1232	, matrix.e1233	, matrix.e1234	, matrix.e1235	, matrix.e1236	, matrix.e1237	, matrix.e1238	, matrix.e1239	, matrix.e1240	, matrix.e1241	, matrix.e1242	, matrix.e1243	, matrix.e1244	, matrix.e1245	,
                  matrix.e1246	, matrix.e1247	, matrix.e1248	, matrix.e1249	, matrix.e1250	, matrix.e1251	, matrix.e1252	, matrix.e1253	, matrix.e1254	, matrix.e1255	, matrix.e1256	, matrix.e1257	, matrix.e1258	, matrix.e1259	, matrix.e1260	,
                  matrix.e1261	, matrix.e1262	, matrix.e1263	, matrix.e1264	, matrix.e1265	, matrix.e1266	, matrix.e1267	, matrix.e1268	, matrix.e1269	, matrix.e1270	, matrix.e1271	, matrix.e1272	, matrix.e1273	, matrix.e1274	, matrix.e1275	, 
                  matrix.e1276	, matrix.e1277	, matrix.e1278	, matrix.e1279	, matrix.e1280	, matrix.e1281	, matrix.e1282	, matrix.e1283	, matrix.e1284	, matrix.e1285	, matrix.e1286	, matrix.e1287	, matrix.e1288	, matrix.e1289	, matrix.e1290	, 
                  matrix.e1291	, matrix.e1292	, matrix.e1293	, matrix.e1294	, matrix.e1295	, matrix.e1296	, matrix.e1297	, matrix.e1298	, matrix.e1299	, matrix.e1300	, matrix.e1301	, matrix.e1302	, matrix.e1303	, matrix.e1304	, matrix.e1305	,
                  matrix.e1306	, matrix.e1307	, matrix.e1308	, matrix.e1309	, matrix.e1310	, matrix.e1311	, matrix.e1312	, matrix.e1313	, matrix.e1314	, matrix.e1315	, matrix.e1316	, matrix.e1317	, matrix.e1318	, matrix.e1319	, matrix.e1320	, 
                  matrix.e1321	, matrix.e1322	, matrix.e1323	, matrix.e1324	, matrix.e1325	, matrix.e1326	, matrix.e1327	, matrix.e1328	, matrix.e1329	, matrix.e1330	, matrix.e1331	, matrix.e1332	, matrix.e1333	, matrix.e1334	, matrix.e1335	, 
                  matrix.e1336	, matrix.e1337	, matrix.e1338	, matrix.e1339	, matrix.e1340	, matrix.e1341	, matrix.e1342	, matrix.e1343	, matrix.e1344	, matrix.e1345	, matrix.e1346	, matrix.e1347	, matrix.e1348	, matrix.e1349	, matrix.e1350	, 
                  matrix.e1351	, matrix.e1352	, matrix.e1353	, matrix.e1354	, matrix.e1355	, matrix.e1356	, matrix.e1357	, matrix.e1358	, matrix.e1359	, matrix.e1360	, matrix.e1361	, matrix.e1362	, matrix.e1363	, matrix.e1364	, matrix.e1365	, 
                  matrix.e1366	, matrix.e1367	, matrix.e1368	, matrix.e1369	, matrix.e1370	, matrix.e1371	, matrix.e1372	, matrix.e1373	, matrix.e1374	, matrix.e1375	, matrix.e1376	, matrix.e1377	, matrix.e1378	, matrix.e1379	, matrix.e1380	, 
                  matrix.e1381	, matrix.e1382	, matrix.e1383	, matrix.e1384	, matrix.e1385	, matrix.e1386	, matrix.e1387	, matrix.e1388	, matrix.e1389	, matrix.e1390	, matrix.e1391	, matrix.e1392	, matrix.e1393	, matrix.e1394	, matrix.e1395	, 
                  matrix.e1396	, matrix.e1397	, matrix.e1398	, matrix.e1399	, matrix.e1400	, matrix.e1401	, matrix.e1402	, matrix.e1403	, matrix.e1404	, matrix.e1405	, matrix.e1406	, matrix.e1407	, matrix.e1408	, matrix.e1409	, matrix.e1410	, 
                  matrix.e1411	, matrix.e1412	, matrix.e1413	, matrix.e1414	, matrix.e1415	, matrix.e1416	, matrix.e1417	, matrix.e1418	, matrix.e1419	, matrix.e1420	, matrix.e1421	, matrix.e1422	, matrix.e1423	, matrix.e1424	, matrix.e1425	, 
                  matrix.e1426	, matrix.e1427	, matrix.e1428	, matrix.e1429	, matrix.e1430	, matrix.e1431	, matrix.e1432	, matrix.e1433	, matrix.e1434	, matrix.e1435	, matrix.e1436	, matrix.e1437	, matrix.e1438	, matrix.e1439	, matrix.e1440	, 
                  matrix.e1441  , matrix.e1442	, matrix.e1443	, matrix.e1444	, matrix.e1445	, matrix.e1446	, matrix.e1447	, matrix.e1448	, matrix.e1449	, matrix.e1450	, matrix.e1451	, matrix.e1452	, matrix.e1453	, matrix.e1454	, matrix.e1455	, 
                  matrix.e1456	, matrix.e1457	, matrix.e1458	, matrix.e1459	, matrix.e1460	, matrix.e1461	, matrix.e1462	, matrix.e1463	, matrix.e1464	, matrix.e1465	, matrix.e1466	, matrix.e1467	, matrix.e1468	, matrix.e1469	, matrix.e1470	, 
                  matrix.e1471	, matrix.e1472	, matrix.e1473	, matrix.e1474	, matrix.e1475	, matrix.e1476	, matrix.e1477	, matrix.e1478	, matrix.e1479	, matrix.e1480	, matrix.e1481	, matrix.e1482	, matrix.e1483	, matrix.e1484	, matrix.e1485	, 
                  matrix.e1486	, matrix.e1487	, matrix.e1488	, matrix.e1489	, matrix.e1490	, matrix.e1491	, matrix.e1492	, matrix.e1493	, matrix.e1494	, matrix.e1495	, matrix.e1496	, matrix.e1497	, matrix.e1498	, matrix.e1499	, matrix.e1500	, 
                  matrix.e1501	, matrix.e1502	, matrix.e1503	, matrix.e1504	, matrix.e1505  , matrix.e1506	, matrix.e1507	, matrix.e1508	, matrix.e1509	, matrix.e1510	, matrix.e1511	, matrix.e1512	, matrix.e1513	, matrix.e1514	, matrix.e1515	,
                  matrix.e1516	, matrix.e1517	, matrix.e1518	, matrix.e1519	, matrix.e1520	, matrix.e1521	, matrix.e1522	, matrix.e1523	, matrix.e1524	, matrix.e1525	, matrix.e1526	, matrix.e1527	, matrix.e1528	, matrix.e1529	, matrix.e1530	, 
                  matrix.e1531	, matrix.e1532	, matrix.e1533	, matrix.e1534	, matrix.e1535	, matrix.e1536	, matrix.e1537	, matrix.e1538	, matrix.e1539	, matrix.e1540	, matrix.e1541	, matrix.e1542	, matrix.e1543	, matrix.e1544	, matrix.e1545	, 
                  matrix.e1546	, matrix.e1547	, matrix.e1548	, matrix.e1549	, matrix.e1550	, matrix.e1551	, matrix.e1552	, matrix.e1553	, matrix.e1554	, matrix.e1555  , matrix.e1556	, matrix.e1557	, matrix.e1558	, matrix.e1559	, matrix.e1560	,
                  matrix.e1561	, matrix.e1562	, matrix.e1563	, matrix.e1564	, matrix.e1565	, matrix.e1566	, matrix.e1567	, matrix.e1568	, matrix.e1569  , matrix.e1570	, matrix.e1571	, matrix.e1572	, matrix.e1573	, matrix.e1574	, matrix.e1575	, 
                  matrix.e1576	, matrix.e1577	, matrix.e1578	, matrix.e1579	, matrix.e1580	, matrix.e1581	, matrix.e1582	, matrix.e1583	, matrix.e1584	, matrix.e1585	, matrix.e1586	, matrix.e1587	, matrix.e1588	, matrix.e1589	, matrix.e1590	, 
                  matrix.e1591	, matrix.e1592	, matrix.e1593	, matrix.e1594	, matrix.e1595	, matrix.e1596	, matrix.e1597	, matrix.e1598	, matrix.e1599	, matrix.e1600	, matrix.e1601	, matrix.e1602	, matrix.e1603	, matrix.e1604	, matrix.e1605	, 
                  matrix.e1606	, matrix.e1607	, matrix.e1608	, matrix.e1609	, matrix.e1610  , matrix.e1611  , matrix.e1612  , matrix.e1613	, matrix.e1614	, matrix.e1615	, matrix.e1616	, matrix.e1617	, matrix.e1618	, matrix.e1619	, matrix.e1620	, 
                  matrix.e1621	, matrix.e1622	, matrix.e1623	, matrix.e1624	, matrix.e1625  , matrix.e1626  , matrix.e1627  , matrix.e1628
                  
))



#### scaling ####

### individual difference scaling without starting position 
scaling.e <- smacof:::indscal(matrices.e, type="ordinal",  itmax=75000)
scaling.e$stress
#   0.1548243 versus with all 6 0.1901146 

plot.indscal <- as.data.frame(scaling.e$gspace)
plot.indscal$direction <- factor(c("pos","neg",  "neg", "pos", "pos") )
plot.indscal$strength <- factor(c("high","low", "high", "low", "high") )
plot.indscal$label <- factor(c("duschen 6 vs 10 min", "Geschirrspüler automatik vs eco",
                               "heizen auf 22 statt 20", "Wäsche bei 30 statt 60", "Wäsche trocknen lassen vs Trockner"))

ggplot(plot.indscal, aes(x=D1, y=D2, color=direction)) +
  geom_point(aes(size= desc(strength)) ) + xlim(-.8, .8) +
  ylim(-.7, .7) +
  theme_minimal() +
  geom_text(aes(label = label), vjust=3, size=4)

###  individual difference scaling with starting position 
#init for starting values 
startconf.p <- matrix(data=NA, nrow=5, ncol=2)
startconf.p[,1] <- c(.1, -.1, -.1, .1, .1) *2
startconf.p[,2]  <- c(-.05, .05,  -.05, .05, -.05) *-4 #c(-.1, .1, .1, -.1, .1, -.1)
scaling.start.p.e <- smacof:::indscal(matrices.e, init=startconf.p, type="ordinal", itmax=75000)
summary(scaling.start.p.e)
scaling.start.p.e$stress
#0.1937694

plot.indscal.starting <- as.data.frame(scaling.start.p.e$gspace)
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