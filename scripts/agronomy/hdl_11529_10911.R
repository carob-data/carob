# R script for "carob"


carob_script <- function(path) {

"Under CSISA Phase II, Nutrient Omission Plot Technique (NOPT) trials were conducted in eight districts of Bihar, ten districts of Uttar Pradesh and ten districts of Odisha. Partner institutions include Bihar Agriculture University (BAU), Banaras Hindu University (BHU), Orissa University of Agriculture and Technology (OUAT), Central Rice Research Institute (CRRI), Odisha."

	uri <- "hdl:11529/10911"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		publication=NA,
		data_organization = "IRRI",
		carob_contributor="Robert Hijmans",
		carob_date="2024-01-27",
		data_type= "experiment",
		response_vars = "yield",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;Zn_fertilizer;B_fertilizer;variety;irrigated",
		project="CSISA"
 	)

	f1 <- ff[basename(ff) == "CSISA_NOPT_Rice_RawDataFINAL.csv"]
	f2 <- ff[basename(ff) == "CSISA_NOPT_Wheat_RawDataFINAL.csv"]

	r1 <- read.csv(f1, check.names=FALSE)
	r2 <- read.csv(f2, check.names=FALSE)
	r <- carobiner::bindr(r1, r2)

	id1 <- c('UP038','UP037','BH013','OD130','BH012','UP058','UP041','UP040','BH010','BH014','UP047','UP083','OD127','UP035','BH011','OD124','UP039','OD128','BH058','OD129','OD207','UP082','UP080','OD122','UP036','OD118','UP053','OD155','BH052','UP055','OD120','BH015','BH057','OD114','OD195','OD195','OD189','OD189','OD123','UP069','OD147','OD192','OD131','UP076','UP056','OD190','OD202','UP072','UP079','BH028','BH056','OD113','OD201','OD201','OD205','OD205','BH059','UP049','BH055','UP050','UP073','OD204','UP070','UP066','UP062','UP059','OD016','UP057','OD126','BH051','UP064','UP077','OD125','UP067','OD151','UP075', 'BH054','UP068','OD145','OD103','BH114','OD116','OD059','OD206','UP029','UP060','OD053','OD117','UP052','UP027','UP074','OD198','UP046','BH053','OD171','UP033','UP026','UP084','UP086','UP061','BH023','OD054','OD119','UP071','UP063','OD058','OD229','OD055','OD115','BH024','UP030','UP051','UP088','OD021','BH115','BH116','BH111','UP043','OD153','BH030','UP065','OD019','UP032','OD061','OD224','BH009','BH005','BH027','UP048','OD203','UP042','OD144','OD146','BH018','OD230','OD236','OD056','OD149','OD149','OD036','OD036','UP054','OD228','OD154','OD154','OD138','OD138','OD226','BH036','UP028','UP034','OD170','BH050','OD225','UP087','UP085','OD223','OD152','OD111','BH022','OD232','OD227','OD235','OD141','OD141','OD012','OD012','OD057','OD157','UP045','OD231','OD196','OD197','BH029','OD222','OD051','OD233','OD135','OD200','OD200','OD191','OD191','BH021','BH035','UP025','OD199','OD199','OD199','OD139','OD139','OD139','OD042','OD042','OD042','BH049','OD060','OD025','OD234','BH032','OD136','BH002','UP044','UP031','OD046','OD067','OD211','OD050','OD052','OD142','BH045','OD049','OD049','OD188','OD188','OD150','OD134','OD035','OD238','OD048','BH020','BH025','OD089','OD194','OD194','OD217','OD217','OD112','UP081','OD047','OD239','UP007','BH017','OD121','OD156','OD045','OD092','BH039','OD245','OD133','OD137','OD086','OD041','OD015','OD005','OD026','OD085','OD148','OD091','OD044','BH041','UP002','OD084','OD158','OD143','OD132','BH038','OD237','OD140','OD090','BH047','OD244','BH026','BH043','OD013','OD034','OD066','OD043','OD252','OD087','BH007','OD040','OD088','OD022','OD218','OD242','BH016','OD063','OD065','BH033','OD101','OD033','OD033','OD193','OD193','OD240','OD029','BH040','UP078','BH044','BH019','OD062','OD162','BH055','BH001','OD068','BH006','BH095','BH096','BH051','OD093','OD069','OD032','OD094','OD024','OD071','OD017','UP008','OD215','OD081','BH054','BH058','BH057','BH028','BH034','OD159','OD020','BH008','OD220','OD220','OD214','OD214','OD095','BH046','UP004','OD010','BH099','BH056','BH052','OD208','BH037','OD246','OD038','OD009','BH042','OD064','OD172','BH048','OD241','OD166','BH109','BH059','OD212','UP003','OD070','OD007','OD096','OD030','OD098','OD253','OD037','OD097','OD011','OD099','BH004','UP006','UP005','OD219','OD219','OD023','OD023','OD082','OD004','OD083','OD247','UP019','OD163','OD002','BH031','UP014','UP018','OD031','BH003','UP001','OD221','OD106','OD243','BH100','UP020','OD080','OD014','OD169','UP015','UP010','UP017','OD027','OD107','OD168','OD209','OD182','UP023','OD008','OD100','OD001','OD102','OD104','OD003','UP021','UP011','OD006','UP024','OD167','UP012','OD018','OD075','UP016','OD105','OD073','OD210','OD254','OD249','UP022','OD180','UP009','OD039','BH085','OD164','BH079','OD165','OD216','OD109','OD213','BH053','OD074','OD028','OD177','OD183','OD183','OD181','OD181','BH074','OD250')
	id2 <- c('BH110','UP013','OD072','OD108','OD248','OD175','OD110','OD187','BH092','OD176','OD173','OD078','OD251','OD179','OD186','OD161','OD079','OD174','OD077','OD184','OD076','OD178','OD160','OD185')

	geo <- data.frame(
		SiteID = c(id1, id2),
		adm2 = c('Kushi Nagar', 'Kushi Nagar', 'Bhojpur', 'Mayurbhanj', 'Bhojpur', 'Maharaj Gang', 'Kushi Nagar', 'Kushi Nagar', 'Bhojpur', 'Bhojpur', 'Kushinagar', 'Sant Kabir Nagar', 'Mayurbhanj', 'Kushi Nagar', 'Bhojpur', 'Mayurbhanj', 'Kushi Nagar', 'Mayurbhanj', 'Vaishali', 'Mayurbhanj', 'Puri', 'Sant Kabir Nagar', 'Sant Kabir Nagar', 'Mayurbhanj', 'Kushi Nagar', 'Mayurbhanj', 'Maharaj Gang', 'Puri', 'Samastipur', 'Maharaj Gang', 'Mayurbhanj', 'Bhojpur', 'Vaishali', 'Mayurbhanj', 'Puri', 'Puri', 'Puri', 'Puri', 'Mayurbhanj', 'Maharajganj', 'Mayurbhanj', 'Puri', 'Mayurbhanj', 'Maharajganj', 'Maharaj Gang', 'Puri', 'Puri', 'Maharajganj', 'Sant Kabir Nagar', 'Mujaffarpur', 'Vaishali', 'Mayurbhanj', 'Puri', 'Puri', 'Puri', 'Puri', 'Vaishali', 'Kushinagar', 'Vaishali', 'Kushinagar', 'Maharajganj', 'Puri', 'Maharajganj', 'Maharajganj', 'Maharaj Gang', 'Maharaj Gang', 'Bhadrak', 'Maharaj Gang', 'Mayurbhanj', 'Samastipur', 'Maharajganj', 'Sant Kabir Nagar', 'Mayurbhanj', 'Maharajganj', 'Mayurbhanj', 'Maharajganj', 'Samastipur', 'Maharajganj', 'Mayurbhanj', 'Keonjhar', 'Vaishali', 'Mayurbhanj', 'Bhadrak', 'Puri', 'Gorakhpur', 'Maharaj Gang', 'Bhadrak', 'Mayurbhanj', 'Maharaj Gang', 'Gorakhpur', 'Maharajganj', 'Puri', 'Kushinagar', 'Samastipur', 'Puri', 'Gorakhpur', 'Gorakhpur', 'Santkabirnagar', 'Santkabirnagar', 'Maharaj Gang', 'Bhagalpur', 'Bhadrak', 'Mayurbhanj', 'Maharajganj', 'Maharaj Gang', 'Bhadrak', 'Puri', 'Bhadrak', 'Mayurbhanj', 'Bhagalpur', 'Gorakhpur', 'Maharaj Gang', 'Santkabirnagar', 'Bhadrak', 'Vaishali', 'Vaishali', 'Samastipur', 'Kushinagar', 'Mayurbhanj', 'Mujaffarpur', 'Maharajganj', 'Bhadrak', 'Gorakhpur', 'Bhadrak', 'Puri', 'Bhojpur', 'Ara', 'Mujaffarpur', 'Kushinagar', 'Puri', 'Kushinagar', 'Mayurbhanj', 'Mayurbhanj', 'Begusarai', 'Puri', 'Puri', 'Bhadrak', 'Bhadrak', 'Mayurbhanj', 'Bhadrak', 'Mayurbhanj', 'Maharaj Gang', 'Puri', 'Mayurbhanj', 'Mayurbhanj', 'Mayurbhanj', 'Mayurbhanj', 'Puri', 'Patna', 'Gorakhpur', 'Gorakhpur', 'Puri', 'Samastipur', 'Puri', 'Santkabirnagar', 'Santkabirnagar', 'Puri', 'Mayurbhanj', 'Mayurbhanj', 'Bhagalpur', 'Puri', 'Puri', 'Puri', 'Mayurbhanj', 'Bhadrak', 'Mayurbhanj', 'Bhadrak', 'Bhadrak', 'Puri', 'Kushinagar', 'Puri', 'Puri', 'Puri', 'Mujaffarpur', 'Puri', 'Bhadrak', 'Puri', 'Mayurbhanj', 'Puri', 'Puri', 'Puri', 'Puri', 'Bhagalpur', 'Patna', 'Gorakhpur', 'Puri', 'Bhadrak', 'Mayurbhanj', 'Puri', 'Bhadrak', 'Mayurbhanj', 'Puri', 'Bhadrak', 'Mayurbhanj', 'Samastipur', 'Bhadrak', 'Bhadrak', 'Puri', 'Patna', 'Mayurbhanj', 'Ara', 'Kushinagar', 'Gorakhpur', 'Bhadrak', 'Cuttack', 'Puri', 'Bhadrak', 'Bhadrak', 'Mayurbhanj', 'Rohtas', 'Bhadrak', 'Puri', 'Bhadrak', 'Puri', 'Mayurbhanj', 'Mayurbhanj', 'Bhadrak', 'Sambalpur', 'Bhadrak', 'Bhagalpur', 'Bhagalpur', 'Kendrapara', 'Puri', 'Puri', 'Puri', 'Puri', 'Mayurbhanj', 'Sant Kabir Nagar', 'Bhadrak', 'Sambalpur', 'Ballia', 'Begusarai', 'Mayurbhanj', 'Puri', 'Bhadrak', 'Kendrapara', 'Rohtas', 'Sambalpur', 'Mayurbhanj', 'Mayurbhanj', 'Kendrapara', 'Bhadrak', 'Bhadrak', 'Balasore', 'Bhadrak', 'Kendrapara', 'Mayurbhanj', 'Kendrapara', 'Bhadrak', 'Rohtas', 'Ballia', 'Kendrapara', 'Puri', 'Mayurbhanj', 'Mayurbhanj', 'Rohtas', 'Sambalpur', 'Mayurbhanj', 'Kendrapara', 'Rohtas', 'Sambalpur', 'Bhagalpur', 'Rohtas', 'Bhadrak', 'Bhadrak', 'Cuttack', 'Bhadrak', 'Sundergarh', 'Kendrapara', 'Ara', 'Bhadrak', 'Kendrapara', 'Bhadrak', 'Puri', 'Sambalpur', 'Begusarai', 'Cuttack', 'Cuttack', 'Patna', 'Keonjhar', 'Puri', 'Bhadrak', 'Puri', 'Bhadrak', 'Sambalpur', 'Bhadrak', 'Rohtas', 'Sant Kabir Nagar', 'Rohtas', 'Bhagalpur', 'Cuttack', 'Puri', 'Muzaffarpur', 'Ara', 'Cuttack', 'Ara', 'Rohtas', 'Rohtas', 'Muzaffarpur', 'Kendrapara', 'Cuttack', 'Bhadrak', 'Kendrapara', 'Bhadrak', 'Cuttack', 'Bhadrak', 'Ballia', 'Puri', 'Dhenkanal', 'Muzaffarpur', 'Muzaffarpur', 'Muzaffarpur', 'Bhagalpur', 'Patna', 'Puri', 'Bhadrak', 'Ara', 'Puri', 'Puri', 'Puri', 'Puri', 'Kendrapara', 'Rohtas', 'Ballia', 'Balasore', 'Rohtas', 'Muzaffarpur', 'Muzaffarpur', 'Puri', 'Patna', 'Sambalpur', 'Bhadrak', 'Balasore', 'Rohtas', 'Cuttack', 'Puri', 'Rohtas', 'Sambalpur', 'Puri', 'Samastipur', 'Muzaffarpur', 'Puri', 'Ballia', 'Cuttack', 'Balasore', 'Kendrapara', 'Bhadrak', 'Kendrapara', 'Sundergarh', 'Bhadrak', 'Kendrapara', 'Bhadrak', 'Keonjhar', 'Ara', 'Ballia', 'Ballia', 'Puri', 'Bhadrak', 'Puri', 'Bhadrak', 'Dhenkanal', 'Balasore', 'Dhenkanal', 'Sundergarh', 'Ghazipur', 'Puri', 'Balasore', 'Patna', 'Chandauli', 'Ghazipur', 'Bhadrak', 'Ara', 'Ballia', 'Puri', 'Keonjhar', 'Sambalpur', 'Rohtas', 'Ghazipur', 'Dhenkanal', 'Bhadrak', 'Puri', 'Chandauli', 'Chandauli', 'Ghazipur', 'Bhadrak', 'Keonjhar', 'Puri', 'Puri', 'Puri', 'Ghazipur', 'Balasore', 'Keonjhar', 'Balasore', 'Keonjhar', 'Keonjhar', 'Balasore', 'Ghazipur', 'Chandauli', 'Balasore', 'Ghazipur', 'Puri', 'Chandauli', 'Bhadrak', 'Dhenkanal', 'Chandauli', 'Keonjhar', 'Dhenkanal', 'Puri', 'Sundergarh', 'Sundergarh', 'Ghazipur', 'Puri', 'Chandauli', 'Bhadrak', 'Rohtas', 'Puri', 'Rohtas', 'Puri', 'Puri', 'Keonjhar', 'Puri', 'Muzaffarpur', 'Dhenkanal', 'Bhadrak', 'Puri', 'Puri', 'Puri', 'Puri', 'Puri', 'Rohtas', 'Sundergarh', 'Samastipur', 'Chandauli', 'Dhenkanal', 'Keonjhar', 'Sundergarh', 'Puri', 'Keonjhar', 'Puri', 'Rohtas', 'Puri', 'Puri', 'Dhenkanal', 'Sundergarh', 'Puri', 'Puri', 'Puri', 'Dhenkanal', 'Puri', 'Dhenkanal', 'Puri', 'Dhenkanal', 'Puri', 'Puri', 'Puri'),
		longitude = c(84.6786, 84.6786, 84.4464, 85.9542, 84.5931, 83.3581, 84.4056, 84.6866, 84.5898, 84.4445, 84.6851, 82.9426, 85.9602, 84.4517, 84.594, 86.7017, 84.6866, 85.9997, 85.1336, 86.485, 85.9571, 82.9426, 82.9396, 86.6943, 84.4517, 86.4488, 83.3514, 85.8102, 86.5843, 83.3517, 86.163, 84.5639, 85.4693, 86.2663, 85.4748, 86.71, 85.4748, 86.71, 86.4488, 83.3365, NA, 85.5344, 85.9844, 83.3598, 83.3517, 85.4915, 86.1804, 83.3698, 82.9454, 85.6168, 85.4693, 86.4207, 95.4803, 85.4356, 95.4803, 85.4356, 85.1834, 84.7387, 85.4358, 84.7387, 83.3622, 86.4749, 83.3338, 83.333, 83.236, 83.2134, NA, 83.3501, 86.18, 86.5852, 83.3365, 82.9435, 85.9989, 83.3579, 86.1644, 83.3463, 86.585, 83.3706, NA, 85.45, 85.1016, 86.152, 86.5885, 86.4, 83.3959, 83.2191, 86.5978, 86.1527, 83.3401, 83.3859, 83.3586, 85.9187, 83.9441, 86.5843, 86.5728, 82.9538, 83.3862, 82.9514, 82.9451, 83.2146, 87.1704, 86.5984, 86.1129, 83.3317, 83.236, 86.5912, 85.9275, 86.568, 86.7397, NA, 83.4928, 83.3401, 82.9538, NA, 85.1016, 85.1018, 85.6521, 83.4016, 86.5839, 85.6188, 83.3365, NA, 82.9357, 86.5917, 85.9096, NA, 84.65, 85.2001, 83.9514, 85.6484, 83.4024, NA, NA, 85.9194, 85.9247, 85.8876, 86.5906, NA, NA, NA, NA, 83.3514, 85.9289, 86.601, NA, 86.601, NA, 85.9127, 84.9721, 83.3859, 83.4024, 85.9669, 86.5852, 85.9096, 82.9357, 82.9514, 85.933, 86.1013, 86.9315, 87.1649, 85.9677, 85.9309, 85.8892, NA, NA, NA, NA, 86.5912, 86.2612, 84.5592, 85.8868, 85.9262, 85.9379, 85.6015, 85.9329, NA, 85.9242, NA, 85.5057, 85.9743, 85.5057, 85.9743, 86.9128, 84.8578, 83.3862, 85.9683, NA, NA, 85.9683, NA, NA, 85.9683, NA, NA, 86.5854, 86.5897, NA, 85.959, 84.8527, NA, 84.595, 84.5589, 83.4928, NA, 86.163, 85.7777, NA, 86.5775, NA, 84.1155, NA, 85.55, NA, 85.55, NA, NA, NA, NA, NA, 87.9837, NA, NA, 85.6684, 86.2034, 85.6684, 86.2034, 86.2372, 82.9423, NA, NA, 84.1836, 86.2188, 86.9303, 85.8102, NA, NA, 84.1297, NA, NA, NA, NA, NA, NA, 86.66, NA, NA, NA, NA, NA, 84.1884, 84.376, NA, 85.7717, NA, NA, 84.1289, NA, NA, NA, 84.1292, NA, NA, 84.1044, NA, NA, 86.163, NA, 84.4, NA, 84.5843, NA, NA, NA, 85.8039, NA, 86.2017, 86.163, 86.163, 84.8577, 85.44, 85.9005, NA, 85.9005, NA, NA, NA, 84.1855, 82.9435, 84.1075, 87.8852, 86.163, 85.85, 85.9523, 84.594, 86.117, 84.5851, 84.1855, 84.1075, 85.6009, NA, 86.117, NA, NA, NA, 86.117, NA, 84.1833, 85.922, 85.35, 85.9668, 85.6019, 85.6027, 87.8852, 84.8542, 85.7726, NA, 84.6502, 85.9749, 85.831, 85.9749, 85.831, NA, 84.1147, 84.528, 86.603, 84.1292, 85.9523, 85.6009, 85.8157, 84.9723, NA, NA, 86.611, 84.2481, 86.163, 86, 84.113, NA, 86.3676, 85.6521, 85.6016, 85.7004, 84.5343, 86.117, 86.517, NA, NA, NA, 84.11, NA, NA, NA, 85.44, 84.5919, 83.964, 83.9634, 85.9099, NA, 85.9099, NA, 85.35, 86.89, 85.35, 84.4318, 83.4795, 85.85, 86.63, 84.8541, 83.3391, 83.4622, NA, 84.5919, 84.362, 85.9616, 85.78, NA, 84.113, 83.4021, NA, NA, 85.9747, 83.3346, 83.4212, 83.6253, NA, 85.76, 85.9096, 85.9525, 85.5, 83.2716, 86.653, 85.44, 86.58, 85.45, 85.76, 86.51, 83.4561, 83.3351, 86.564, 83.5743, 85.9016, 83.4618, NA, NA, 83.3304, 85.761, 85.35, 86.733, 84.1, 83.9718, 83.5806, 85.48, 83.4189, NA, NA, 85.9693, NA, 85.909, 85.9771, 85.76, 85.9699, 85.9521, 85.35, NA, 85.49, 86.1, 85.5, 86.1, 85.5, NA, 84.8451, 85.6521, 83.4636, 85.35, 85.73, 84.2467, 86.3, 85.78, 86.1, NA, 85.54, 85.51, NA, 84.5799, 85.48, 85.51, 85.8833, 85.35, 85.51, NA, 85.51, NA, 85.49, 85.7795, 85.54),
		latitude = c(26.6895, 26.6895, 25.4788, 21.7673, 25.4622, 27.1389, 26.4135, 26.6901, 25.4544, 25.4807, 26.7288, 26.8409, 21.7778, 26.7299, 25.4626, 21.8284, 26.6901, 21.7656, 25.9341, 21.8159, 19.9764, 26.8409, 26.8188, NA, 26.7299, 21.9101, 27.1425, 20.2632, 26.5336, 27.1424, 21.9874, 25.4174, 25.6836, 22.1517, 19.58, 20.769, 19.58, 20.769, 21.9101, 27.1476, NA, 19.563, NA, 27.161, 27.1424, 20.408, 20.87, 27.1524, 26.8166, 26.5134, 25.6836, 21.9777, 19.5948, 20.613, 19.5948, 20.613, 25.7841, 26.6893, 25.8013, 26.6893, 27.1544, 20.794, 27.1462, 27.1451, 27.8961, 27.6769, NA, 27.1404, 21.8185, 26.3184, 27.1476, 26.8304, 21.7657, 27.1422, 21.9846, 27.1566, 26.5338, 27.1514, NA, 21.39, 25.9686, 21.9842, 21.7559, 19.9669, 26.5159, 27.8131, 21.7575, 21.9862, 27.1392, 26.5096, 27.1587, 20.46, 26.8853, 26.5336, 20.1064, 26.8591, 26.5092, 26.8001, 26.8295, 27.8211, 25.1354, 21.762, 21.9285, 27.146, 27.8961, 21.7964, 20.3219, 21.1016, 22.3833, NA, 26.7471, 27.1392, 26.8591, NA, 25.9685, 25.9685, 25.9842, 26.5066, 21.9694, 26.5038, 27.1476, NA, 26.8365, 21.7944, 20.7981, NA, 25.4849, 26.2515, 26.8836, 20.154, 26.507, NA, NA, 25.6008, 19.9994, 20.368, 21.7966, NA, NA, NA, NA, 27.1425, 20.2876, 21.9695, NA, 21.9695, NA, 20.8338, 25.5845, 26.5096, 26.507, 20.4892, 26.3184, 20.1244, 26.8365, 26.8001, 20.4439, 21.957, 21.9271, 25.1326, 20.7431, 20.227, 20.3698, NA, NA, NA, NA, 21.7906, 21.85, 26.7268, 20.244, 19.9059, 19.9104, 25.9357, 20.9908, NA, 20.4039, NA, 20.654, 19.9069, 20.654, 19.9069, 25.1046, 25.5162, 26.5092, 19.9116, NA, NA, 19.9116, NA, NA, 19.9116, NA, NA, 26.3178, 21.8188, NA, 20.474, 25.516, NA, 25.4623, 26.7254, 26.7471, NA, 20.479, 20.759, NA, 21.8574, NA, 25.2409, NA, 20.2, NA, 20.2, NA, NA, NA, NA, NA, 25.1564, NA, NA, 19.9931, 19.9711, 19.9931, 19.9711, 21.9866, 26.837, NA, NA, 25.8747, 25.4016, 21.9194, 20.2631, NA, NA, 25.2375, NA, NA, NA, NA, NA, NA, 21.12, NA, NA, NA, NA, NA, 25.1867, 25.9584, NA, 20.3612, NA, NA, 25.2367, NA, NA, NA, 25.2363, NA, NA, 25.2036, NA, NA, 20.479, NA, 22.12, NA, 25.4508, NA, NA, NA, 20.863, NA, 25.4516, 20.479, 20.479, 25.5185, 21.37, 19.9101, NA, 19.9101, NA, NA, NA, 25.1989, 26.8304, 25.2038, 25.1519, 20.479, 20.2416, 26.1689, 25.4626, 20.481, 25.4525, 25.1989, 25.2038, 25.9344, NA, 20.481, NA, NA, NA, 20.481, NA, 25.8747, 19.9996, 20.36, 26.1689, 25.9358, 25.936, 25.1519, 25.5165, 20.3449, NA, 25.4688, 20.535, 20.454, 20.535, 20.454, NA, 25.2461, 25.9062, 21.149, 25.2363, 26.1692, 25.9344, 20.683, 25.5845, NA, NA, 21.15, 25.1928, 20.479, 19.3, 25.2404, NA, 20.9475, 25.9842, 25.9361, 19.9351, 25.9068, 20.481, 21.15, NA, NA, NA, 22.3, NA, NA, NA, 21.36, 25.4624, 25.9342, 25.9352, 19.9079, NA, 19.9079, NA, 20.36, 21.36, 20.36, 22.124, 25.5887, 20.2408, 21.29, 25.5187, 25.1301, 25.6302, NA, 25.463, 25.9575, 20.395, 21.61, NA, 25.2404, 25.5866, 20.36, NA, 20.4915, 25.1124, 25.2914, 25.6238, NA, 21.65, 20.1245, 19.9005, 20.2, 25.6073, 21.258, 21.36, 21.14, 21.39, 21.65, 21.15, 25.5456, 25.1032, 21.144, 25.3895, 20.1075, 25.3208, NA, NA, 25.1306, 21.651, 20.36, 19.9992, 22.34, 22.1065, 25.5272, 20.5, 25.2922, NA, NA, 20.4572, NA, 20.7427, 20.824, 21.65, 20.221, 26.1689, 20.36, NA, 20.2, 19.59, 20.2, 19.59, 20.2, NA, 21.6861, 25.9842, 25.3231, 20.36, 21.6, 21.9897, 19.59, 21.61, 19.59, NA, 20.3, 20.4, NA, 22.1902, 20.5, 20.4, 20.1064, 20.36, 20.4, NA, 20.4, NA, 20.4, 20.4095, 20.3)
	)
	 
	r <- merge(r, geo, by="SiteID", all.x=TRUE)
	
	d <- data.frame(
		country = "India",
		crop=tolower(r$Crop), 
		previous_crop = tolower(r$PCRP),
		yield_part = "grain",
		season=tolower(r$Season), 
		trial_id = r$SiteID, 
		soil_texture = tolower(r$STYP), 
		soil_color = tolower(r$SCLR), 
		variety = r$VAR, 
		variety_type = tolower(r$VTYP), 
		planting_date = r$SDATE,
		transplanting_date = r$TDATE,
		harvest_date = r$HDATE,
		season_constraint = trimws(tolower(r$CSTR)),
		soil_constraint = trimws(tolower(r$SCON)),
		irrigation_source = trimws(tolower(r$IRRS)),
		adm2 = r$adm2,
		longitude = r$longitude,
		latitude = r$latitude,
		on_farm = TRUE,
		is_survey = FALSE
	)

	d$irrigated <- r$AECO != "Rainfed"
	d$irrigated[r$AECO == ""] <- NA
	d$geo_from_source <- FALSE 
  
	d[d==""] <- NA
	adm1 <- substr(r$SiteID, 1, 2)
	adm1 <- gsub("UP", "Uttar Pradesh", adm1)
	adm1 <- gsub("BH", "Bihar", adm1)
	d$adm1 <- gsub("OD", "Odisha", adm1)

	irn <- gsub("thrice", "3", tolower(r$IRRN))
	irn <- gsub("once", "1", irn)
	irn <- gsub("20-22", "21", irn)
	irn <- gsub("20-23", "21", irn)
	irn <- gsub("20-24", "22", irn)
	irn <- gsub("20-25", "22", irn)
	d$irrigation_number = suppressWarnings(as.integer(irn))
	d$land_prep_method[d$crop=="wheat"] <- r$ESTM[d$crop=="wheat"]
	d$land_prep_method[d$land_prep_method == "ZeroTill"] <- "none"
	d$land_prep_method <- gsub("reduced till", "reduced tillage", tolower(d$land_prep_method))
	d$land_prep_method <- gsub(" till$", "", d$land_prep_method)

	d$transplanting_method[d$crop=="rice"] <- r$ESTM[d$crop=="rice"]
	d$transplanting_method[	d$transplanting_method == "" ] <- NA
	d$transplanting_method <- gsub(" rice| transplanting", "", tolower(d$transplanting_method))
	d$transplanting_method <- gsub("mechanical", "mechanized", d$transplanting_method)
	d$transplanting_method <- gsub("direct seeded", "none", d$transplanting_method)
	
	d$soil_texture <- gsub("loamy$", "loam", d$soil_texture)
	d$soil_texture <- gsub("sandy$", "sand", d$soil_texture)
	
	for (v in c("planting_date", "transplanting_date", "harvest_date")) {
		d[[v]] <- as.character(as.Date(d[[v]], "%m/%d/%y"))
	}

	getM <- function(v) {
		x <- r[, c(colnames(r)[grep(v, colnames(r))])]
		colnames(x) <- gsub(v, "", colnames(x))
		data.frame(id=1:nrow(x), x)
	}
		
	colnames(r)[colnames(r) == "YLDK"] <- "YLD+K"
	colnames(r)[colnames(r) == "YLDZN"] <- "YLD+ZN"
	n <- getM("-N$")
	p <- getM("-P$")
	k <- getM("-K$")
	kk <- getM("\\+K$")
	z <- getM("-ZN$")
	zz <- getM("\\+ZN$")
	b <- getM("B$")
	colnames(b) <- gsub("\\.$", "", colnames(b))
	colnames(b)[6] <- "B"
	nut <- rbind(n, p, k, z, kk, zz)
	nut <- carobiner::bindr(nut, b)
	nut <- carobiner::change_names(nut, 
			c("N", "P2O5", "K2O", "ZNSO4", "YLD", "B"),
			c(paste0(c("N", "P", "K", "Zn"), "_fertilizer"), "yield", "B_fertilizer"))
	nut$P_fertilizer <- nut$P_fertilizer / 2.29
	nut$K_fertilizer <- nut$K_fertilizer / 1.2051
	nut$Zn_fertilizer <- nut$Zn_fertilizer * 0.15
	# nut$B_fertilizer <- nut$B_fertilizer / ??
	nut$yield <- nut$yield * 1000
	nut$B_fertilizer[is.na(	nut$B_fertilizer )] <- 0 

	d <- cbind(d[nut$id, ], nut[,-1])
	d <- d[!is.na(d$yield), ]

	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path)
}

