# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Tanzania Africa Research in Sustainable Intensification for the Next Generation (Africa RISING) Baseline Evaluation Survey

As part of the US government's Feed the Future initiative that aims to address global hunger and food security issues in sub-Saharan Africa, the US Agency for International Development is supporting three multi-stakeholder agricultural research projects under Africa Research In Sustainable Intensification for the Next Generation (Africa RISING - AR) program. The overall aim of the program is to transform agricultural systems through sustainable intensification projects in Ghana, Ethiopia, Tanzania, Malawi, Mali, and (potentially) Zambia. In Tanzania, the project, led by the International Institute of Tropical Agriculture (IITA), will be supporting cereal-based farming systems. Multiple participatory and adaptive agricultural interventions are currently taking place in Babati, Kongwa, and Kiteto, three districts in Tanzania, led by researchers from the IITA. Experts from IITA have supported or introduced intercropping, drought-tolerant crop varieties, water harvesting practices, and organic fertilizer application. The International Food Policy Research Institute (IFPRI) leads the monitoring and evaluation (M&E) activities of the AR program. As part of the M&E activities in Tanzania, IFPRI contracted Economic Development Initiatives (EDI) to conduct baseline household and community surveys in Babati, Kongwa, and Kiteto districts. The main objective of this survey is to collect high-quality baseline household data to support the M&E activities of the AR program in Tanzania. More specifically, the survey aims to collect detailed information on the composition of the household, employment, health, agriculture, income and expenditures, credit, assets, subjective welfare and food security, shocks, and the anthropometric status of children and women.
"

	uri <- "doi:10.7910/DVN/PPUL2W"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=3,
		data_organization = "IFPRI",
		publication = NA,
		project = NA,
		carob_date = "2026-04-01",
		design = "unitOfAnalysis" ,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		carob_contributor = "Cedric Ngakou",
		completion = 70,	
		notes = "We process only files with useful information for carob"
	)
	

	f1 <- ff[basename(ff) == "001_HHVisits1.dta"]
	f2 <- ff[basename(ff) == "002_HHVisits2.dta"]
	f3 <- ff[basename(ff) == "003_filter.dta"]
	f4 <- ff[basename(ff) == "004_interview.dta"]
	f5 <- ff[basename(ff) == "005_sectionB.dta"]
	f6 <- ff[basename(ff) == "006_sectionC.dta"]
	f7 <- ff[basename(ff) == "007_sectionD.dta"]
	f8 <- ff[basename(ff) == "008_sectionE.dta"]
	f9 <- ff[basename(ff) == "009_sectionF.dta"]
	f10 <- ff[basename(ff) == "010_sectionG1.dta"]
	f11 <- ff[basename(ff) == "011_sectionG2.dta"]
	f12 <- ff[basename(ff) == "012_sectionG3.dta"]
	f13 <- ff[basename(ff) == "013_sectionG4.dta"]
	f14 <- ff[basename(ff) == "014_sectionH.dta"]
	f15 <- ff[basename(ff) == "015_sectionI.dta"]
	f16 <- ff[basename(ff) == "016_sectionJ1.dta"]
	f17 <- ff[basename(ff) == "017_sectionJ2.dta"]
	f18 <- ff[basename(ff) == "018_sectionK.dta"]
	f19 <- ff[basename(ff) == "019_sectionL1.dta"]
	f20 <- ff[basename(ff) == "020_sectionL2.dta"]
	f21 <- ff[basename(ff) == "021_sectionM.dta"]
	f22 <- ff[basename(ff) == "022_sectionN.dta"]
	f23 <- ff[basename(ff) == "023_sectionO1.dta"]
	f24 <- ff[basename(ff) == "024_sectionO2.dta"]
	f25 <- ff[basename(ff) == "025_sectionO3.dta"]
	f26 <- ff[basename(ff) == "026_sectionP.dta"]
	f27 <- ff[basename(ff) == "027_sectionQ.dta"]
	f28 <- ff[basename(ff) == "028_sectionR1_month.dta"]
	f29 <- ff[basename(ff) == "029_sectionR1_week.dta"]
	f30 <- ff[basename(ff) == "030_sectionR2.dta"]
	f31 <- ff[basename(ff) == "031_sectionS.dta"]
	f32 <- ff[basename(ff) == "032_sectionT.dta"]
	f33 <- ff[basename(ff) == "033_sectionU1.dta"]
	f34 <- ff[basename(ff) == "034_sectionCA1.dta"]
	f35 <- ff[basename(ff) == "035_sectionCB1.dta"]
	f36 <- ff[basename(ff) == "036_sectionCC.dta"]
	f37 <- ff[basename(ff) == "037_sectionCD1.dta"]
	f38 <- ff[basename(ff) == "038_sectionCD2.dta"]
	f39 <- ff[basename(ff) == "039_sectionCE1.dta"]
	f40 <- ff[basename(ff) == "040_sectionCE2.dta"]
	f41 <- ff[basename(ff) == "041_sectionCE3.dta"]
	f42 <- ff[basename(ff) == "042_sectionCE4.dta"]
	f43 <- ff[basename(ff) == "043_sectionCF.dta"]
	f44 <- ff[basename(ff) == "044_sectionCG1.dta"]
	f45 <- ff[basename(ff) == "045_sectionCG2.dta"]
	f46 <- ff[basename(ff) == "046_sectionCG3.dta"]
	f47 <- ff[basename(ff) == "047_sectionCH1.dta"]
	f48 <- ff[basename(ff) == "048_sectionCH2.dta"]
	f49 <- ff[basename(ff) == "049_SamplingWeights.dta"]

	#r1 <- haven::read_dta(f1) |> carobiner:::unlabel()
	#r2 <- haven::read_dta(f2) |> carobiner:::unlabel()
	#r3 <- haven::read_dta(f3) |> carobiner:::unlabel()
	r4 <- haven::read_dta(f4) |> carobiner:::unlabel()
	r5 <- haven::read_dta(f5) |> carobiner:::unlabel()
	#r6 <- haven::read_dta(f6) |> carobiner:::unlabel()
	#r7 <- haven::read_dta(f7) |> carobiner:::unlabel()
	r8 <- haven::read_dta(f8) |> carobiner:::unlabel()
	r9 <- haven::read_dta(f9) |> carobiner:::unlabel()
	r10 <- haven::read_dta(f10) |> carobiner:::unlabel()
	r11 <- haven::read_dta(f11) |> carobiner:::unlabel()
	#r12 <- haven::read_dta(f12) |> carobiner:::unlabel()
	#r13 <- haven::read_dta(f13) |> carobiner:::unlabel()
	r14 <- haven::read_dta(f14) |> carobiner:::unlabel()
	# r15 <- haven::read_dta(f15) |> carobiner:::unlabel()
	# r16 <- haven::read_dta(f16) |> carobiner:::unlabel()
	# r17 <- haven::read_dta(f17) |> carobiner:::unlabel()
	# r18 <- haven::read_dta(f18)
	# r19 <- haven::read_dta(f19)
	# r20 <- haven::read_dta(f20)
	# r21 <- haven::read_dta(f21)
	# r22 <- haven::read_dta(f22)
	# r23 <- haven::read_dta(f23)
	# r24 <- haven::read_dta(f24)
	# r25 <- haven::read_dta(f25)
	# r26 <- haven::read_dta(f26)
	# r27 <- haven::read_dta(f27)
	# r28 <- haven::read_dta(f28)
	# r29 <- haven::read_dta(f29)
	# r30 <- haven::read_dta(f30)
	# r31 <- haven::read_dta(f31)
	# r32 <- haven::read_dta(f32)
	# r33 <- haven::read_dta(f33)
	r34 <- haven::read_dta(f34) |> carobiner:::unlabel()
	# r35 <- haven::read_dta(f35)
	# r36 <- haven::read_dta(f36)
	# r37 <- haven::read_dta(f37)
	# r38 <- haven::read_dta(f38)
	# r39 <- haven::read_dta(f39)
	# r40 <- haven::read_dta(f40)
	# r41 <- haven::read_dta(f41)
	# r42 <- haven::read_dta(f42)
	# r43 <- haven::read_dta(f43)
	# r44 <- haven::read_dta(f44)
	# r45 <- haven::read_dta(f45)
	# r46 <- haven::read_dta(f46)
	# r47 <- haven::read_dta(f47)
	# r48 <- haven::read_dta(f48)
	# r49 <- haven::read_dta(f49)
	
 ####################
	
	d1 <- data.frame(
	   hhid = as.character(r4$hhid),
	   adm1 = r4$a1,
	   adm2 = r4$a2,
	   location = r4$a3
	   
	)
	
	d2 <- data.frame(
	   hhid = as.character(r5$hhid),
	   farmer_gender = r5$b3,
	   farmer_age = r5$b4a,
	   farmer_education = r5$b6,
	   farmer_civil_status = r5$b10
	)
	
	### merge d1 and d2
	d <- merge(d1, d2, by= intersect(names(d1), names(d2)), all = TRUE)
	
 #####
	d3 <- data.frame(
	   hhid = as.character(r8$hhid),
	   field_id = as.character(r8$parcelid),
	   farmland = ifelse(grepl("Acre", r8$e2b), r8$e2a*0.4047,
	                     ifelse(grepl("Meter square", r8$e2b), r8$e2a/10000, r8$e2a)) ,
	   irrigated = grepl("Irrigation", r8$e9),
	   #irrigation_method = r8$e10,
	   soil_texture = tolower(gsub("Sand/loam", "sandy loam", r8$e12)),
	   soil_color = r8$e14,
	   plot_slope = r8$e15
	)
	
	### merge d and d3
	d <- merge(d, d3, by= intersect(names(d), names(d3)), all = TRUE)
	
	
	d4 <- data.frame(
	   hhid = as.character(r9$hhid),
	   plot_id = as.character(r9$plotid),
	   field_id = as.character(r9$f2a),
	   land_prep_method = r9$f6a,
	   OM_amount = ifelse(grepl("\\(50 kg bag\\)", r9$f9b), r9$f9a*50,
	               ifelse(grepl("\\(90kg bag\\)", r9$f9b), r9$f9a*90, 
	               ifelse(grepl("Plate", r9$f9b), r9$f9a*0.32, 
	               ifelse(grepl("Bucket/Tin", r9$f9b), r9$f9a*16.5,
	               ifelse(grepl("Heap", r9$f9b), r9$f9a*0.10,
	               ifelse(grepl("Cane/Basket", r9$f9b), r9$f9a*0.14, 
	               ifelse(grepl("Gram", r9$f9b), r9$f9a/1000, r9$f9a))))))),
	   OM_type = gsub("Generated on farm|Obtained off-farm|Other", "unknown", r9$f10),
	   OM_price = r9$f9b_q
	)
	
	### merge d and d4
	d <- merge(d, d4, by= intersect(names(d), names(d4)), all = TRUE)
	
	d5 <- data.frame(
	   hhid = as.character(r10$hhid),
	   season = r10$plantingseasonid,
	   field_id = as.character(r10$g1_2),
	   plot_id = as.character(r10$g1_3),
	   crop = tolower(r10$g1_4),
	   plot_area = ifelse(grepl("Acre",  r10$g1_6b), r10$g1_6a*0.4047,
	                      ifelse(grepl("Meter square",  r10$g1_6b), r10$g1_6a/10000, r10$g1_6a)),
	   
	   yield = ifelse(grepl("\\(50 kg bag\\)", r10$g1_7b), r10$g1_7a*50,
	           ifelse(grepl("\\(90kg bag\\)", r10$g1_7b), r10$g1_7a*90, 
	           ifelse(grepl("Bale", r10$g1_7b), r10$g1_7a*0.1, 
	           ifelse(grepl("Bucket/Tin", r10$g1_7b), r10$g1_7a*16.5,
	           ifelse(grepl("Heap", r10$g1_7b), r10$g1_7a*0.10,
	           ifelse(grepl("Unit or Piece", r10$g1_7b), r10$g1_7a*0.40,
	           ifelse(grepl("Gram", r10$g1_7b),r10$g1_7a/1000, r10$g1_7a)))))))
	)
	
	### merge d and d5
	d5 <- d5[d5$yield>0,]
	gg <- aggregate(. ~ hhid + season + field_id + plot_id + crop, d5, function(x) mean(x))
	d <- merge(d, gg, by= intersect(names(d), names(gg)), all = TRUE)
	
	
	d6 <- data.frame(
	   hhid = as.character(r11$hhid),
	   crop = tolower(r11$g2_4),
	   field_id = as.character(r11$g2_2),
	   plot_id = as.character(r11$g2_3),
	   season = r11$plantingseasonid,
	   seed_rate = rowSums(r11[, c("g2_5a", "g2_6a", "g2_7a", "g2_8a")], na.rm = TRUE) ,
	   unit1 = ifelse(is.na(r11$g2_5b)& !is.na(r11$g2_6b), r11$g2_6b, 
	                  ifelse(is.na(r11$g2_5b)& !is.na(r11$g2_7b), r11$g2_7b, 
	                         ifelse(is.na(r11$g2_5b)& !is.na(r11$g2_8b), r11$g2_8b, r11$g2_5b))) ,
	   seed_price = rowSums(r11[, c("g2_5c", "g2_6c", "g2_7c", "g2_8c")], na.rm = TRUE),
	   fertilizer_type = r11$g2_11,
	   fertilizer_amount = ifelse(grepl("Unit or Piece", r11$g2_13b), r11$g2_13a*0.40,
	                       ifelse(grepl("Bucket/Tin", r11$g2_13b), r11$g2_13a*16.5, 
	                       ifelse(grepl("\\(90kg bag\\)", r11$g2_13b), r11$g2_13a*90,
	                       ifelse(grepl("\\(50 kg bag\\)", r11$g2_13b), r11$g2_13a*50,
	                       ifelse(grepl("Gram", r11$g2_13b), r11$g2_13a/1000, r11$g2_13a))))) ,
	   fertilizer_price = r11$g2_13c
	)
	
	d6$seed_rate <- ifelse(grepl("\\(50 kg bag\\)", d6$unit1), d6$seed_rate*50,
	                ifelse(grepl("\\(90kg bag\\)", d6$unit1), d6$seed_rate*90, 
	                ifelse(grepl("Unit or Piece", d6$unit1), d6$seed_rate*0.40, 
	                ifelse(grepl("Bucket/Tin", d6$unit1), d6$seed_rate*16.5,
	                ifelse(grepl("Gram", d6$unit1), d6$seed_rate/1000, d6$seed_rate)))))	
	d6$unit1 <- NULL
	
	### merge d and d6
	d6[d6=="-99"] <- NA 
	d6 <- d6[d6$seed_rate>0,]
	d <- merge(d, d6, by= intersect(names(d), names(d6)), all = TRUE)
	
	
	d7 <- data.frame(
	   hhid = as.character(r14$hhid),
	   crop = tolower(r14$h2),
	   fwy_residue =  ifelse(grepl("\\(50 kg bag\\)", r14$h6b), r14$h6a*50,
	                  ifelse(grepl("\\(90kg bag\\)", r14$h6b), r14$h6a*90, 
	                  ifelse(grepl("Bale", r14$h6b), r14$h6a*0.1, 
	                  ifelse(grepl("Bucket/Tin", r14$h6b), r14$h6a*16.5,
	                  ifelse(grepl("Heap",r14$h6b), r14$h6a*0.10,
	                  ifelse(grepl("Unit or Piece", r14$h6b), r14$h6a*0.40,
	                  ifelse(grepl("Gram", r14$h6b),r14$h6a/1000,r14$h6a))))))),
	   
	   residue_prevcrop = ifelse(grepl("\\(50 kg bag\\)", r14$h8b), r14$h8a*50,
	                     ifelse(grepl("\\(90kg bag\\)", r14$h8b), r14$h8a*90, 
	                     ifelse(grepl("Bale", r14$h8b), r14$h8a*0.1, 
	                     ifelse(grepl("Bucket/Tin", r14$h8b), r14$h8a*16.5,
	                     ifelse(grepl("Heap",r14$h8b), r14$h8a*0.10,
	                     ifelse(grepl("Unit or Piece", r14$h8b), r14$h8a*0.40,
	                     ifelse(grepl("Gram", r14$h8b),r14$h8a/1000,r14$h8a))))))),
	   
	   yield_marketable = ifelse(grepl("\\(50 kg bag\\)", r14$h8b), r14$h13a*50,
	                      ifelse(grepl("\\(90kg bag\\)", r14$h8b), r14$h13a*90, 
	                      ifelse(grepl("Bale", r14$h8b), r14$h13a*0.1, 
	                      ifelse(grepl("Bucket/Tin", r14$h8b), r14$h13a*16.5,
	                      ifelse(grepl("Heap",r14$h8b), r14$h13a*0.10,
	                      ifelse(grepl("Unit or Piece", r14$h8b), r14$h13a*0.40,
	                      ifelse(grepl("Gram", r14$h8b),r14$h13a/1000,r14$h13a))))))),
	   crop_price = r14$h14
	)
	
	d7[d7=="-99"|d7=="-98"|d7=="-97"] <- NA 
	d7 <- d7[!is.na(d7$fwy_residue),] 
	### merge d and d7
	Agg <- aggregate(. ~ hhid + crop, d7, function(x) mean(x))
	d <- merge(d, Agg, by= intersect(names(d), names(Agg)), all = TRUE)
	
	
	d$fertilizer_price <- as.character(d$fertilizer_price/d$fertilizer_amount)
	d$fertilizer_amount <- d$fertilizer_amount/d$plot_area
	d$OM_amount <- d$OM_amount/d$plot_area
	d$crop_price <- ifelse(d$yield_marketable !=0, d$crop_price/d$yield_marketable, d$crop_price) 
	d$yield <- d$yield/d$plot_area
	d$yield_marketable <- d$yield_marketable/d$plot_area
	d$residue_prevcrop <- d$residue_prevcrop/d$plot_area
	d$fwy_residue <- d$fwy_residue/d$plot_area
	seed_price <- d$seed_price/d$seed_rate
	d$seed_rate <- d$seed_rate/d$plot_area
	d$plot_area <- d$plot_area*10000 # m2
	
	
	### Adding Lon and Lat coordinate
	
	geo <- data.frame(
	   location = c("Dareda", "Gidas", "Bashnet", "Dabil", "Madunga", "Dosidos", "Magugu", "Mwada", "Arri", "Gallapo", "Chitego", "Mlali", "Chiwe", "Sagara", "Makawa", "Ugogoni", "Njoge", "Njoro", "Makame"),
	   longitude = c(35.553, 35.6749, 35.41053, 35.4670, 35.4170, 36.4173, 35.7912, 35.9026, 35.60618, 35.8503, 36.3780, 36.75017, 36.737, 36.533, 36.5409, 36.4331, 36.6885, 36.50223, 36.7315),
	   latitude = c(-4.2161, -4.4197, -4.2324, -4.2649, -4.1483, -5.62025, -4.0314, -3.911, -4.2267, -4.28326, -5.6139, -6.28239, -6.1580, -6.246, -5.752, -6.1659, -5.9436, -5.2486, -4.6305)
	)
	
	d <- merge(d, geo, by="location", all.x = TRUE)
	##### Fixing Crop names
	
	P <- carobiner::fix_name(d$crop)
	P <- gsub("bambara nuts", "bambara groundnut", P)
	P <- gsub("^bean$", "common bean", P)
	P <- gsub("chick-peas", "chickpea", P)
	P <- gsub("cow-peas", "cowpea", P)
	P <- gsub("green pepper", "pepper", P)
	P <- gsub("irish potato", "potato", P)
	P <- gsub("^nuts$", "groundnut", P)
	P <- gsub("other vegetables", "vegetable", P)
	P <- gsub("^peas$", "pea", P)
	P <- gsub("pigeonpea", "pigeon pea", P)
	P <- gsub("sugar cane", "sugarcane", P)
	P <- gsub("sweet potato", "sweetpotato", P)
	P <- gsub("tomatoes", "tomato", P)
	P <- gsub("other perennial|other pulses|planted fodder|planted trees", "unknown", P)
	P <- gsub("unknown, nuts", "groundnut", P)
	d$crop <- P
	
	#### Fixing land prep method
	
	P <- carobiner::fix_name(d$land_prep_method)
	P <- gsub("Animal, disc plough|^disc plough$", "ploughing", P)
	P <- gsub("Animal, mouldboard plough|^mouldboard plough$", "ploughing", P)
	P <- gsub("Did not plough", "none", P)
	P <- gsub("Hand hoe", "hoeing", P)
	P <- gsub("Mixed method", "unknown", P)
	P <- gsub("Strip/zonal tillage", "strip tillage", P)
	P <- gsub("Planting pits", "unknown", P)
	#P <- gsub("Tractor", "mechanical puddling", P)
	P <- gsub("Zero/minimum tillage", "minimum tillage", P)
	P <- gsub("Tractor, disc plough", "mechanical puddling;ploughing", P)
	P <- gsub("Tractor, mouldboard plough", "mechanical puddling;ploughing", P)
	P <- gsub("Other", "unknown", P)
	d$land_prep_method <- P
 
	P <- carobiner::fix_name(d$fertilizer_type)
	P <- gsub("D. Compound", "D-compound", P)
	P <- gsub("Urea", "urea", P)
	P <- gsub("A combination|Other", "unknown", P)
	P <- gsub("None", "none", P)
	d$fertilizer_type <- P
	
	d$soil_texture <- gsub("other", NA, d$soil_texture)
	
	d$country <- "Tanzania"
	d$currency = "TZS"
	d$on_farm <- FALSE
	d$is_survey <- TRUE
	d$planting_date <- as.character(NA)
	d$geo_from_source <- FALSE
	d$yield_part <- "none"
	d$trial_id <- paste(d$hhid, d$adm3, d$season, sep="-")
	d$season <- NULL
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
  
	### drop rows with missing site information (adm1, adm2 and location)
	d <- d[!is.na(d$adm1),]
	
	#### remove duplicate rows
	
	d <- unique(d)
	
	carobiner::write_files(path, meta, d)
}


