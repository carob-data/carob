# R script for "carob"
# license: GPL (>=3)

## ISSUES
## At some point, marketable yield (the amount of crop sold) is greater than total yield (the total amount of crop harvested).
## Some plot areas are very small and others are very large compared to the crop weight harvested; this needs to be investigated.
## 


carob_script <- function(path) {

"
Mali Africa Research in Sustainable Intensification for the Next Generation (Africa RISING) Baseline Evaluation Survey

As part of the US government's Feed the Future initiative that aims to address global hunger and food security issues in sub-Saharan Africa, the US Agency for International Development is supporting three multi-stakeholder agricultural research projects under Africa Research In Sustainable Intensification for the Next Generation (Africa RISING - AR) program. The overall aim of the program is to transform agricultural systems through sustainable intensification projects in Ghana, Ethiopia, Tanzania, Malawi, Mali, and (potentially) Zambia. In West Africa, IITA works with multi-disciplinary R4D partners in selected communities located in Northern Ghana and Southern Mali. More particularly, in Southern Mali the AR-WA project focuses on sorghum-millet-legume-vegetable-livestock systems in the Bougouni, Yanfolila and Koutiala districts, which are situated in the Sikasso region. The Africa RISING partners in Mali include several international institutions: the International Crops Research Institute for the Semi-Arid Tropics (ICRISAT), the International Livestock Research Institute (ILRI), the Asian Vegetable Research and Development Center (AVRDC), the International Center for Research in Agroforestry or World Agroforestry Center (ICRAF); as well as local partners: L'Association Malienne d'Eveil et de Développement Durable (AMEDD), L'Association Malienne pour la Sécurité et la Souveraineté Alimentaires (AMASSA), Mouvement Biologique du Mali (MOBIOM).
"
	uri <- "doi:10.7910/DVN/UDKSBJ"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=6,
		data_organization = "IFPRI",
		publication = NA,
		project = "Africa RISING",
		carob_date = "2026-02-20",
		design = "unitOfAnalysis",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		carob_contributor = "Cedric Ngakou",
		completion = 70,	
		notes = "only files with useful information for carob have been process"
	)
	

	f1 <- ff[basename(ff) == "001_Interview.dta"]
	f2 <- ff[basename(ff) == "002_HHvisit.dta"]
	f3 <- ff[basename(ff) == "003_Section B.dta"]
	f4 <- ff[basename(ff) == "004_Section C.dta"]
	f5 <- ff[basename(ff) == "005_Section D.dta"]
	f6 <- ff[basename(ff) == "006_Section E.dta"]
	f7 <- ff[basename(ff) == "007_Section F.dta"]
	f8 <- ff[basename(ff) == "008_Section G1.dta"]
	f9 <- ff[basename(ff) == "009_Section G2.dta"]
	f10 <- ff[basename(ff) == "010_Section G3.dta"]
	f11 <- ff[basename(ff) == "011_Section G4.dta"]
	f12 <- ff[basename(ff) == "012_Section H.dta"]
	f13 <- ff[basename(ff) == "013_Section I.dta"]
	f14 <- ff[basename(ff) == "014_Section J1.dta"]
	f15 <- ff[basename(ff) == "015_Section J2.dta"]
	f16 <- ff[basename(ff) == "016_Section K1.dta"]
	f17 <- ff[basename(ff) == "017_Section K2.dta"]
	f18 <- ff[basename(ff) == "018_Section L.dta"]
	f19 <- ff[basename(ff) == "019_Section M.dta"]
	f20 <- ff[basename(ff) == "020_Section N1.dta"]
	f21 <- ff[basename(ff) == "021_Section N2.dta"]
	f22 <- ff[basename(ff) == "022_Section N3.dta"]
	f23 <- ff[basename(ff) == "023_Section N4.dta"]
	f24 <- ff[basename(ff) == "024_Section O.dta"]
	f25 <- ff[basename(ff) == "025_Section P.dta"]
	f26 <- ff[basename(ff) == "026_Section Q1_month.dta"]
	f27 <- ff[basename(ff) == "027_Section Q1_week.dta"]
	f28 <- ff[basename(ff) == "028_Section Q2.dta"]
	f29 <- ff[basename(ff) == "029_Section R.dta"]
	f30 <- ff[basename(ff) == "030_Filter.dta"]
	f31 <- ff[basename(ff) == "031_Coordinates.dta"]
	f32 <- ff[basename(ff) == "032_Section CA.dta"]
	f33 <- ff[basename(ff) == "033_Section CB.dta"]
	f34 <- ff[basename(ff) == "034_Section CC.dta"]
	f35 <- ff[basename(ff) == "035_Section CD1.dta"]
	f36 <- ff[basename(ff) == "036_Section CD2.dta"]
	f37 <- ff[basename(ff) == "037_Section CE1.dta"]
	f38 <- ff[basename(ff) == "038_Section CE2.dta"]
	f39 <- ff[basename(ff) == "039_Section CE3.dta"]
	f40 <- ff[basename(ff) == "040_Section CE4.dta"]
	f41 <- ff[basename(ff) == "041_Section CE5.dta"]
	f42 <- ff[basename(ff) == "042_Section CF.dta"]
	f43 <- ff[basename(ff) == "043_Section CG1.dta"]
	f44 <- ff[basename(ff) == "044_Section CG2.dta"]
	f45 <- ff[basename(ff) == "045_Section CG3.dta"]
	f46 <- ff[basename(ff) == "046_Section CH1.dta"]
	f47 <- ff[basename(ff) == "047_Section CH2.dta"]

	r1 <- haven::read_dta(f1)|> carobiner:::unlabel()
	#r2 <- haven::read_dta(f2)|> carobiner:::unlabel()
	r3 <- haven::read_dta(f3)|> carobiner:::unlabel()
	#r4 <- haven::read_dta(f4)|> carobiner:::unlabel()
	#r5 <- haven::read_dta(f5)|> carobiner:::unlabel()
	r6 <- haven::read_dta(f6)|> carobiner:::unlabel()
	r7 <- haven::read_dta(f7)|> carobiner:::unlabel()
	r8 <- haven::read_dta(f8)|> carobiner:::unlabel()
	r9 <- haven::read_dta(f9)|> carobiner:::unlabel()
	#r10 <- haven::read_dta(f10)|> carobiner:::unlabel()
	r11 <- haven::read_dta(f11) |> carobiner:::unlabel()
	r12 <- haven::read_dta(f12)|> carobiner:::unlabel()
	r13 <- haven::read_dta(f13)|> carobiner:::unlabel()
	#r14 <- haven::read_dta(f14)|> carobiner:::unlabel()
	#r15 <- haven::read_dta(f15)
	#r16 <- haven::read_dta(f16)
	#r17 <- haven::read_dta(f17)
	#r18 <- haven::read_dta(f18)
	#r19 <- haven::read_dta(f19)
	#r20 <- haven::read_dta(f20)
	#r21 <- haven::read_dta(f21)
	#r22 <- haven::read_dta(f22)
	#r23 <- haven::read_dta(f23)
	#r24 <- haven::read_dta(f24)
	#r25 <- haven::read_dta(f25)
	#r26 <- haven::read_dta(f26)
	#r27 <- haven::read_dta(f27)
	#r28 <- haven::read_dta(f28)
	#r29 <- haven::read_dta(f29)
	#r30 <- haven::read_dta(f30)
	r31 <- haven::read_dta(f31)|> carobiner:::unlabel()
	r32 <- haven::read_dta(f32)|> carobiner:::unlabel()
	#r33 <- haven::read_dta(f33)
	#r34 <- haven::read_dta(f34)
	#r35 <- haven::read_dta(f35)
	#r36 <- haven::read_dta(f36)
	#r37 <- haven::read_dta(f37)
	#r38 <- haven::read_dta(f38)
	#r39 <- haven::read_dta(f39)
	#r40 <- haven::read_dta(f40)
	#r41 <- haven::read_dta(f41)
	#r42 <- haven::read_dta(f42)
	#r43 <- haven::read_dta(f43)
	#r44 <- haven::read_dta(f44)
	r45 <- haven::read_dta(f45)|> carobiner:::unlabel()
	#r46 <- haven::read_dta(f46)
	#r47 <- haven::read_dta(f47)

#### Process interview data
	d1 <- data.frame(
		hhid = as.character(r1$hhid),
		weight = r1$weight,
		#r1$consent,
		adm2 = r1$a1,
		adm3 = r1$a2,
		location = r1$a3,
		#relalationship_hh = r1$a15,
		translator_used = grepl("Yes", r1$a16),
		farmer_religion = r1$a18,
		#bean_method_used= r1$a25,
		treatment = r1$treat
	)

	
	### process data from Section B
	
	d2 <- data.frame(
	   hhid = as.character(r3$hhid),
	   adm2 = r3$a1,
	   adm3 = r3$a2,
	   location = r3$a3,
	   weight = r3$weight,
	   field_id = as.character(r3$pid),
	   farmer_gender = r3$b3,
	   farmer_age = r3$b4a,
	   farmer_marital_status = r3$b6,
	   farmer_education_level = r3$b7,
	   #previous_activity = r3$b9,
	   treatment = r3$treat
	)
	
	## merge d1 and d2
 d	<- merge(d1, d2, by= intersect(names(d1), names(d2)), all = TRUE)
	
	## process data from  Section E
 
	d3 <- data.frame(
	   hhid = as.character(r6$hhid),
	   adm2 = r6$a1,
	   adm3 = r6$a2,
	   location = r6$a3,
	   weight = r6$weight,
	   field_id = as.character(r6$parcid),
	   farmland = ifelse(grepl("M2", r6$e3b), r6$e3a_HA/10000, r6$e3a_HA) , 
	   farmland_owned = ifelse(grepl("^Yes$", r6$e4), 
	                    ifelse(grepl("M2", r6$e3b), r6$e3a_HA/10000, r6$e3a_HA), NA) ,
	   communal_land = ifelse(grepl("communal$", r6$e4), 
	                    ifelse(grepl("M2", r6$e3b), r6$e3a_HA/10000, r6$e3a_HA), NA),
	   farmland_rentedin = ifelse(grepl("No, we rent", r6$e4), 
	                       ifelse(grepl("M2", r6$e3b), r6$e3a_HA/10000, r6$e3a_HA), NA),
	   #cropland_used = r6$e8b,
	   irrigation_method = ifelse(grepl("Surface", r6$e9), "surface", 
	                        ifelse(grepl("Rain",r6$e9), "none",
	                       ifelse(grepl("Groundwater|combination", r6$e9), "unknown", r6$e9))) ,
	   irrigated = ifelse(!is.na(r6$e9), TRUE, FALSE),
	   irrigation_source = ifelse(is.na(r6$e10), r6$e9,  r6$e10),
	   #r6$e11,
	   soil_texture = tolower(r6$e12),
	   soil_color = r6$e14,
	   plot_slope = r6$e15,
	   treatment = r6$treat
	) 
	
	### merge d and d3
	d	<- merge(d, d3, by= intersect(names(d), names(d3)), all = TRUE)
	#d <- d[!duplicated(d$id),]
	#d$id <- NULL
	
	### process data from Section F
	
	d5 <- data.frame(
	   hhid = as.character(r7$hhid),
	   adm2 = r7$a1,
	   adm3 = r7$a2,
	   location = r7$a3,
	   weight = r7$weight,
	   field_id = as.character(r7$parcid),
	   plot_id = as.character(r7$plotid),
	   #plot_nbr = r7$f1b,
	   croping_system  = ifelse(grepl("Yes",  r7$f3), "rotation",
	                     ifelse(grepl("No", r7$f3), "none", r7$f3)),
	   #fallow_used = ifelse(grepl("Yes", r7$f4a), TRUE, 
	                 #ifelse(grepl("No", r7$f4a), FALSE, r7$f4a)) ,
	   land_prep_method = r7$f5a,
	   land_prep_implement = ifelse(grepl("Animal, disc plough", r7$f5a), "animal;disc plough",
	                             ifelse(grepl("Animal, mouldboard plough", r7$f5a), "animal;mouldboard plough",
	                             ifelse(grepl("Tractor, mouldboard plough", r7$f5a), "tractor;mouldboard plough",
	                             ifelse(grepl("ractor, disc plough", r7$f5a),"tractor;disc plough",
	                             ifelse(grepl("Hand hoe", r7$f5a), "hoe", "unknown"))))),
	   #r7$f6,
	   OM_amount1 = r7$f7a ,
	   OM_amount2 = r7$f13a,
	   Organic_fertilizer_price = rowSums(r7[, c("f8", "f13d")]),
	   fertilizer_type = tolower(r7$f10a),
	   #fertilizer_date = r7$f10b,
	   OM_type = r7$f12,
	   # r7$f11a,
	   fertilizer_amount = r7$f11a,
	   fertilizer_price = r7$f11c,
	   #soil_erosion = r7$f15,
	   #soil_erosion_method = r7$f16a,
	   treatment = r7$treat
	)

	
	### Section G1
	d6 <- data.frame(
	   hhid = as.character(r8$hhid),
	   adm2 = r8$a1,
	   adm3 = r8$a2,
	   location = r8$a3,
	   #farm_names =trimws(r8$e1a),
	   field_id = as.character(r8$parcid),
	   plot_id = as.character(r8$plotid),
	   crop = tolower(r8$cropid),
	   farmland = ifelse(grepl("M2", r8$g1_4b), r8$g1_4a_HA/10000, 
	              ifelse(grepl("Hectare", r8$g1_4b), r8$g1_4a_HA, NA)) ,
	   plot_area = ifelse(grepl("M2", r8$g1_4b), r8$g1_4a/10000, 
	               ifelse(grepl("Hectare", r8$g1_4b), r8$g1_4a, NA)),
	   planting_area_pct = r8$g1_5,
	   yield = r8$g1_6a*r8$g1_6_conv, # 
	   convert_fact= r8$g1_6_conv, 
	   treatment = r8$treat
	)
	
	
	dd	<- merge(d5, d6, by= intersect(names(d5), names(d6)), all = TRUE)	
	
	d	<- merge(d, dd, by= intersect(names(d), names(dd)), all = TRUE)
	
	
	### Process data from Section G2
	d7 <- data.frame(
	   hhid = as.character(r9$hhid),
	   adm2 = r9$a1,
	   adm3 = r9$a2,
	   location = r9$a3,
	   field_id = as.character(r9$parcid),
	   plot_id = as.character(r9$plotid),
	   crop = tolower(r9$cropid),
	   seed_rate1 = r9$g2_2a *r9$g2_2_conv,
	   seed_rate2 = r9$g2_3a *r9$g2_3_conv,
	   seed_rate3 = r9$g2_4a *r9$g2_5_conv,
	   seed_rate4 = r9$g2_5a *r9$g2_5_conv,
	   seed_treatment = ifelse(!is.na(r9$g2_5a_KG), "improve", "traditional"),
	   #persticide_herbicide = r9$g2_6,
	   treatment = r9$treat
	)
	
	d7$seed_rate <- rowSums(d7[, c("seed_rate1", "seed_rate2", "seed_rate3", "seed_rate4")], na.rm = TRUE)
	d7$seed_rate1 <- d7$seed_rate2 <- d7$seed_rate3 <- d7$seed_rate4 <- NULL
	
	d	<- merge(d, d7, by= intersect(names(d), names(d7)), all = TRUE)
	
	## Section G4
	
	d8 <- data.frame(
	   hhid = as.character(r11$hhid),
	   adm2 = r11$a1,
	   adm3 = r11$a2,
	   location = r11$a3,
	   weight = r11$weight,
	   crop = tolower(r11$cropid),
	   seed_source = r11$g4_5,
	   variety = ifelse(is.na(r11$g4_8a) & !is.na(r11$g4_8c), r11$g4_8c ,
	             ifelse(is.na(r11$g4_8a) & !is.na(r11$g4_8b), r11$g4_8b,  r11$g4_8a)) ,
	   treatment = r11$treat
	)
	
	d	<- merge(d, d8, by= intersect(names(d), names(d8)), all = TRUE)
	
	### 
	d9 <- data.frame(
	   hhid = as.character(r12$hhid),
	   field_id = as.character(r12$parcid),
	   plot_id = as.character(r12$plotid),
	   adm2 = r12$a1,
	   adm3 = r12$a2,
	   location = r12$a3,
	   crop = tolower(r12$cropid),
	   #crop_loss = r12$h2a_KG,
	   #harvest_pct = r12$h2d,
	   dmy_residue = r12$h5a *r12$h5_conv, # to Kg
	   yield_marketable = r12$h12a * r12$h12_conv,# to kg
	   residue_prevcrop = r12$h7a *r12$h7_conv,# to kg
	   crop_price = r12$h13_KG,#per kg
	   currency = "XOF",##
	   market_type = r12$h14,
	   treatment = r12$treat
	)
	
	
	d	<- merge(d, d9, by= intersect(names(d), names(d9)), all = TRUE)
	
	d10 <- data.frame(
	   hhid = as.character(r13$hhid),
	   adm2 = r13$a1,
	   adm3 = r13$a2,
	   location = r13$a3,
	   weight = r13$weight,
	   crop = tolower(r13$cropid),
	   dmy_storage = r13$i4a *r13$i4_conv,
	  treatment =  r13$treat
	)
	
	d	<- merge(d, d10, by= intersect(names(d), names(d10)), all = TRUE)

 d$weight <- NULL
 
 d$yield <- d$yield/d$plot_area
 d$dmy_residue <- d$dmy_residue/d$plot_area
 d$dmy_storage <- d$dmy_storage/d$plot_area
 d$yield_marketable <- d$yield_marketable/d$plot_area
 d$residue_prevcrop <- d$residue_prevcrop/d$plot_area
 d$seed_rate <- d$seed_rate/d$plot_area
 d$plot_area <- d$plot_area *10000 # to m2
 d$fertilizer_price <- as.character(d$fertilizer_price/d$fertilizer_amount) ## XOF/kg
 ## add OM_amount
 cols <- grep("^OM_amount", names(d), value = TRUE)
 d[cols] <- d[cols] * d$convert_fact
 d$OM_amount <- rowSums(d[cols], na.rm = TRUE)
 d$convert_fact <- d$OM_amount1 <- d$OM_amount2 <-  NULL
 ### remove rows with the planting_area_pct equal to Zero
 d <- d[which(d$planting_area_pct!=0),]
 
 
 ### adding long and lat coordinate 
 d11 <- data.frame(
    id = r32$cid,
    adm2 = r32$ca1,
    adm3 = r32$ca2,
    location = r32$ca3
   # treatment = ifelse(!grepl("control", r32$site), "Treatment", r32$site)
 )
 
 geo <- data.frame(
    id = r31$cid,
    latitude = r31$ca4a1 +r31$ca4a2/60+ r31$ca4a3/3600,
    longitude = r31$ca4b1 + r31$ca4b2/60 + r31$ca4b3/3600,
    elevation = r31$ca4c,
    geo_from_source = TRUE
 )

 dd <- merge(d11, geo, by= "id", all = TRUE)
 	
d <- merge(d, dd, by = intersect(names(d), names(dd)), all.x = TRUE) 

### 

### Fixing conflict coordinate errors 

geo <-  data.frame(
   location = c("Tiere", "Zansoni", "Konseguela", "M'Pessoba", "N'golonianasso", "Bobola-Zangasso"),
   long = c(-5.406, -5.569, -5.880, -5.7169, -5.6832, -4.991),
   lat = c(11.899, 12.6090, 12.4068, 12.666, 12.4229, 12.539),
   geo_from = FALSE
)

d <- merge(d, geo, by = "location", all.x = TRUE) 
d$longitude[!is.na(d$long)] <- d$long[!is.na(d$long)]
d$latitude[!is.na(d$lat)] <- d$lat[!is.na(d$lat)]
d$geo_from_source[!is.na(d$geo_from)] <- d$geo_from[!is.na(d$geo_from)]
d$id <- d$planting_area_pct <- d$long <-  d$lat <- d$geo_from <- d$convert_fact <- NULL 

### Fixing soil texture

P <- carobiner::fix_name(d$soil_texture)
P <- gsub("sand/loam", "sandy loam", P)
P <- gsub("sand and clay", "sandy clay", P)
P <- gsub("^rocky soil$", "coarse", P)
P <- gsub("clay and rocky soil", "clay coarse", P)
P <- gsub("other|don't know", NA, P)
d$soil_texture <- P

### Fixing land_prep_method

P <- carobiner::fix_name(d$land_prep_method)
P <- gsub("Animal, mouldboard plough|Animal, disc plough", "ploughing", P)
P <- gsub("Mixed method|Other", "unknown", P)
P <- gsub("Hand hoe", "hoeing", P)
P <- gsub("Zero/minimum tillage", "minimum tillage", P)
P <- gsub("Tractor, mouldboard plough|Tractor, disc plough", "ploughing", P)
P <- gsub("Planting pits", "unknown", P)
P <- gsub("Ripping", "ripping", P)
P <- gsub("Strip/zonal tillage", "strip tillage", P)

d$land_prep_method <- P

### fixing fertilizer type

P <- carobiner::fix_name(d$fertilizer_type)
P <- gsub("uree", "urea", P)
P <- gsub("other|cotton complex|cereal complex|a combination|organic manure|compost", "unknown", P)
P <- gsub("dap", "DAP", P)
P <- gsub("npk", "NPK", P)
d$fertilizer_type <- P

## Fixing crop names
d$crop <- ifelse(grepl("other", d$crop), "none", d$crop)
P <- carobiner::fix_name(d$crop)
P <- gsub("african aubergine|aubergine european", "eggplant", P)
P <- gsub("amaranthus", "amaranth", P)
P <- gsub("bambara nut|bambara nuts|nuts", "bambara groundnut", P)
P <- gsub("betterave/betterave sucre", "beetroot", P)
P <- gsub("bitter leaves", "vegetable", P) # bitter leaf
P <- gsub("carrots", "carrot", P)
P <- gsub("chick-peas", "chickpeas", P)
P <- gsub("coffe", "coffee", P)
P <- gsub("courge/courgette", "zucchini", P)
P <- gsub("cowpeas|cow-peas", "cowpea", P)
P <- gsub("echalotte", "shallot", P)
P <- gsub("garden eggs", "eggplant", P)
P <- gsub("green paper|green pepper", "pepper", P)
P <- gsub("guinean sorrel", "sorrel", P)
P <- gsub("iganme|igname", "yam", P)
P <- gsub("irish potato", "potato", P)
P <- gsub("natural trees", "none", P)
P <- gsub("planted fodder|tubers", "none", P)
P <- gsub("palm tree", "palm tree", P)
P <- gsub("pasture/grazing|fallow", "none", P)
P <- gsub("pawpaw/papaya", "pawpaw", P)
P <- gsub("peas", "pea", P)
P <- gsub("pigeonpeas|^pigeonpea$", "pigeon pea", P)
P <- gsub("planted trees", "none", P)
P <- gsub("red paper|red pepper", "pepper", P)
P <- gsub("soyabean", "soybean", P)
P <- gsub("sugar cane", "sugarcane", P)
P <- gsub("sweet potato", "sweetpotato", P)
P <- gsub("tomatoes", "tomato", P)
P <- gsub("^bean$", "common bean", P)
d$crop <- P

## fixing OM_type

P <- carobiner::fix_name(d$OM_type)
P <- gsub("A combination of organic inputs|Other", "unknown", P)
P <- gsub("Crop residue from this farm", "unknown", P)
P <- gsub("Household waste", "unknown", P)
P <- gsub("Mulch/compost", "compost", P)
P <- gsub("None", "none", P)
d$OM_type <- P
##### 


d$country <- "Mali"
d$adm1 <- "Sikasso"
d$trial_id <- paste(d$location, d$hhid, sep = "-")
d$planting_date <- as.character(NA)
d$on_farm <- FALSE 
d$is_survey <- TRUE
d$yield_part <- "none"
d$yield_moisture <- as.numeric(NA)


d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

### remove duplicate records
d <- unique(d)

carobiner::write_files(path, meta, d)
}


