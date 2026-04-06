# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Ghana Africa Research in Sustainable Intensification for the Next Generation (Africa RISING) Baseline Evaluation Survey

As part of the US government's Feed the Future initiative that aims to address global hunger and food security issues in sub-Saharan Africa, the US Agency for International Development is supporting three multi-stakeholder agricultural research projects under Africa Research In Sustainable Intensification for the Next Generation (Africa RISING - AR) program. The overall aim of the program is to transform agricultural systems through sustainable intensification projects in Ghana, Ethiopia, Tanzania, Malawi, Mali, and (potentially) Zambia. In West Africa, IITA works with multi-disciplinary R4D partners in selected communities located in Northern Ghana and Southern Mali. More particularly, in Northern Ghana three regions were chosen for the study: the Northern, Upper-East and Upper-West regions. These areas cover both maize-based and rice-vegetables-based systems and therefore allow to address the production constraints characterizing both realities7. As IFPRI (2012) highlights, the northern regions of Ghana are characterized by small land holdings and low input - low output farming systems, which adversely impact food security. In particular, they are subject to a seasonal cycle of food insecurity of three to seven months for cereals (i.e., maize, millet and sorghum) and four to seven months for legumes (i.e., groundnuts, cowpeas, and soybeans). These crops in the savannahs are often produced in a continuous monoculture, steadily depleting soil natural resources and causing the yields per unit area to fall to very low levels. The poverty profile of Ghana identifies the three northern regions as the poorest and most hunger-stricken areas in the country. Gender inequalities are also apparent in these regions, since women have limited access to resources and therefore limited capacity to generate income on their own.
"


	uri <- "doi:10.7910/DVN/QUB9UT"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=7,
		data_organization = "IFPRI",
		publication = NA,
		project = NA,
		carob_date = "2026-04-04",
		design = "unitOfAnalysis", 
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		carob_contributor = "Cedric Ngakou",
		completion = 70,	
		notes = "We process only files with useful information for carob"
	)
	
	
	f1 <- ff[basename(ff) == "001_HHvisit.dta"]
	f2 <- ff[basename(ff) == "002_Interview.dta"]
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
	f23 <- ff[basename(ff) == "023_Section O.dta"]
	f24 <- ff[basename(ff) == "024_Section P.dta"]
	f25 <- ff[basename(ff) == "025_Section P_Foodconversions.dta"]
	f26 <- ff[basename(ff) == "026_Section Q1_month.dta"]
	f27 <- ff[basename(ff) == "027_Section Q1_week.dta"]
	f28 <- ff[basename(ff) == "028_Section Q2.dta"]
	f29 <- ff[basename(ff) == "029_Section R.dta"]
	f30 <- ff[basename(ff) == "030_Filter.dta"]
	f31 <- ff[basename(ff) == "031_Section CA.dta"]
	f32 <- ff[basename(ff) == "032_Section CB.dta"]
	f33 <- ff[basename(ff) == "033_Section CC.dta"]
	f34 <- ff[basename(ff) == "034_Section CD1.dta"]
	f35 <- ff[basename(ff) == "035_Section CD2.dta"]
	f36 <- ff[basename(ff) == "036_Section CE1.dta"]
	f37 <- ff[basename(ff) == "037_Section CE2.dta"]
	f38 <- ff[basename(ff) == "038_Section CE3.dta"]
	f39 <- ff[basename(ff) == "039_Section CE4.dta"]
	f40 <- ff[basename(ff) == "040_Section CF.dta"]
	f41 <- ff[basename(ff) == "041_Section CG1.dta"]
	f42 <- ff[basename(ff) == "042_Section CG2.dta"]
	f43 <- ff[basename(ff) == "043_Section CG3.dta"]
	f44 <- ff[basename(ff) == "044_Section CH1.dta"]
	f45 <- ff[basename(ff) == "045_Section CH2.dta"]

	r1 <- haven::read_dta(f1) |> carobiner:::unlabel()
	r2 <- haven::read_dta(f2) |> carobiner:::unlabel()
	r3 <- haven::read_dta(f3) |> carobiner:::unlabel()
	#r4 <- haven::read_dta(f4) |> carobiner:::unlabel()
	#r5 <- haven::read_dta(f5) |> carobiner:::unlabel()
	r6 <- haven::read_dta(f6) |> carobiner:::unlabel()
	r7 <- haven::read_dta(f7) |> carobiner:::unlabel()
	r8 <- haven::read_dta(f8) |> carobiner:::unlabel()
	r9 <- haven::read_dta(f9) |> carobiner:::unlabel()
	#r10 <- haven::read_dta(f10) |> carobiner:::unlabel()
	#r11 <- haven::read_dta(f11) |> carobiner:::unlabel()
	r12 <- haven::read_dta(f12) |> carobiner:::unlabel()
	# r13 <- haven::read_dta(f13) |> carobiner:::unlabel()
	# r14 <- haven::read_dta(f14) |> carobiner:::unlabel()
	# r15 <- haven::read_dta(f15) |> carobiner:::unlabel()
	# r16 <- haven::read_dta(f16) |> carobiner:::unlabel()
	# r17 <- haven::read_dta(f17) |> carobiner:::unlabel()
	# r18 <- haven::read_dta(f18) |> carobiner:::unlabel()
	# r19 <- haven::read_dta(f19) |> carobiner:::unlabel()
	# r20 <- haven::read_dta(f20) |> carobiner:::unlabel()
	# r21 <- haven::read_dta(f21) |> carobiner:::unlabel()
	# r22 <- haven::read_dta(f22) |> carobiner:::unlabel()
	# r23 <- haven::read_dta(f23) |> carobiner:::unlabel()
	# r24 <- haven::read_dta(f24) |> carobiner:::unlabel()
	# r25 <- haven::read_dta(f25) |> carobiner:::unlabel()
	# r26 <- haven::read_dta(f26) |> carobiner:::unlabel()
	# r27 <- haven::read_dta(f27) |> carobiner:::unlabel()
	# r28 <- haven::read_dta(f28) |> carobiner:::unlabel()
	# r29 <- haven::read_dta(f29) |> carobiner:::unlabel()
	# r30 <- haven::read_dta(f30) |> carobiner:::unlabel()
	# r31 <- haven::read_dta(f31) |> carobiner:::unlabel()
	# r32 <- haven::read_dta(f32) |> carobiner:::unlabel()
	# r33 <- haven::read_dta(f33) |> carobiner:::unlabel()
	# r34 <- haven::read_dta(f34) |> carobiner:::unlabel()
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


	d1 <- data.frame(
	   hhid = r1$hhid,
	   adm1 = r1$a1,
	   adm2 = r1$a2,
	   location = r1$a3,
	   treatment = r1$treat
	   
	)
	
	d2 <- data.frame(
	   hhid = as.character(r3$hhid),
	   adm1 = r3$a1,
	   adm2 = r3$a2,
	   location = r3$a3,
	   farmer_gender = r3$b3,
	   farmer_age = r3$b4a,
	   farmer_marital_stutus = r3$b6,
	   farmer_education_level = trimws(r3$b7),
	   treatment = r3$treat
	)
	
	d3 <- data.frame(
	   hhid = as.character(r6$hhid),
	   adm1 = r6$a1,
	   adm2 = r6$a2,
	   location = r6$a3,
	   field_id = as.character(r6$parcid),
	   farmland = r6$e3a_HA,
	   irrigated = grepl("Rain", r6$e9),
	   irrigation_method = ifelse(grepl("Surface", r6$e9), "surface",
	                       ifelse(grepl("Groundwater", r6$e9), "groundwater", "unknown")) ,
	   irrigation_source = r6$e10,
	   soil_texture = tolower(gsub("Sand/loam", "sandy loam", r6$e12)),
	   soil_color = r6$e14,
	   plot_slope = r6$e15,
	   treatment  = r6$treat
	)
	##### merge d2 and d3
	
	d <- merge(d3, d2[!duplicated(d2$hhid),], by= intersect(names(d2), names(d3)), all = TRUE)
	
	d4 <- data.frame(
	   hhid = as.character(r7$hhid),
	   adm1 = r7$a1,
	   adm2 = r7$a2,
	   location = r7$a3,
	   field_id = as.character(r7$parcid),
	   plot_id = as.character(r7$plotid),
	   land_prep_method = tolower(ifelse(is.na(r7$f5a), r7$f5b,  r7$f5a)),
	   OM_amount = ifelse(is.na(r7$f7a), r7$f7b , r7$f7a) ,
	   OM_price = rowSums(r7[, c("f8", "f13d")], na.rm = TRUE),
	   fertilizer_type = r7$f10a,
	   fertilizer_amount = rowSums(r7[, c("f11a", "f11c")], na.rm = TRUE) ,
	   OM_type = ifelse(!is.na(ifelse(is.na(r7$f7a), r7$f7b , r7$f7a)), paste("manure", r7$f12, sep = ";"), r7$f12) ,
	   OM_amount1 = as.numeric(ifelse(is.na(r7$f13a), r7$f13b , r7$f13a)),
	   treatment = r7$treat
	   
	)
	
	d4$OM_amount <- rowSums(d4[, c("OM_amount", "OM_amount1")], na.rm = TRUE)
	d4$OM_amount1 <- NULL
	#### merge d and d4
	d <- merge(d, d4, by= intersect(names(d), names(d4)), all = TRUE)
	
	#####
	d5 <- data.frame(
	   hhid = as.character(r8$hhid),
	   adm1 = r8$a1,
	   adm2 = r8$a2,
	   location = r8$a3,
	   field_id = as.character(r8$parcid),
	   plot_id = as.character(r8$plotid),
	   crop = tolower(r8$cropid),
	   plot_area = r8$g1_4a_HA,
	   yield = r8$g1_6a_KG,
	   treatment = r8$treat
	)
	
	#### merge d and d5
	d5 <- d5[d5$yield>0,]
	gg <- aggregate(. ~ hhid + adm1 +adm2 + location + field_id + plot_id + crop + treatment, d5, function(x) mean(x))
	d <- merge(d, gg, by= intersect(names(d), names(gg)), all = TRUE)
	
	#####
	d6 <- data.frame(
	   hhid = as.character(r9$hhid),
	   adm1 = r9$a1,
	   adm2 = r9$a2,
	   location = r9$a3,
	   field_id = as.character(r9$parcid),
	   plot_id = as.character(r9$plotid),
	   crop = tolower(r9$cropid),
	   seed_rate = rowSums(r9[, c("g2_2a_KG", "g2_3a_KG", "g2_4a_KG", "g2_5a_KG")], na.rm = TRUE) ,
	   treatment = r9$treat
	)
	
	#### merge d and d6
	d6[d6=="-99"] <- NA 
	d6 <- d6[d6$seed_rate>0,]
	d <- merge(d, d6, by= intersect(names(d), names(d6)), all = TRUE)
	
	########
	d7 <- data.frame(
	   hhid = as.character(r12$hhid),
	   adm1 = r12$a1,
	   adm2 = r12$a2,
	   location = r12$a3,
	   field_id = as.character(r12$parcid),
	   plot_id = as.character(r12$plotid),
	   crop = tolower(r12$cropid),
	   fwy_residue = r12$h5a_KG,
	   residue_prevcrop = r12$h7a_KG,
	   yield_marketable = r12$h12a_KG,
	   crop_price = r12$h13_KG,
	   treament = r12$treat
	)
	
	#### merge d and d7
	d <- merge(d, d7, by= intersect(names(d), names(d7)), all = TRUE)
	
	##### Adding Lon and lat coordinate
	
	geo <- data.frame(
	   location = c("Nyagli", "Pase", "Tanina", "Goripie", "Fian", "Goli", "Goriyiri", "Gyilli", "Issa", "Naro", "Papu", "Sa Gie", "Tabiase", "Wogu", "Cheyohi No. 2", "Gbanjon", "Tingoli", "Disiga", "Duko", "Gushie", "Jana", "Kadia", "Kukobila", "Nabogu", "Pigu", "Tibali", "Tindan", "Arigu", "Kukua", "Nasia", "Gia", "Nyangua", "Shia"),
	   longitude = c(-2.4004, -2.711, -2.482, -2.271, -2.467, -2.633, -2.619, -2.631, -2.3347, -2.462, -2.579, -2.35, -2.341, -2.386, -0.984, -1.102, -1.0458, -0.8065, -0.9486, -0.860, -0.806, -0.8567, -0.8062, -0.822, -0.8272, -0.8760, -0.90341, -0.8756, -0.8182, -0.8033, -0.2500, -1.1067, 0.5670),
	   latitude = c(10.1306, 10.0379, 9.806, 9.973, 10.388, 10.295, 10.325, 10.2013, 10.389, 10.326, 10.235, 10.259, 10.386, 10.425, 9.442, 9.451, 9.334, 10.124, 9.560, 9.808, 9.4781, 9.906, 10.1239, 9.7473, 9.9762, 9.6633, 9.669, 10.576, 10.3048, 10.1608, 5.5404, 10.8806, 6.7938)
	)
	
	d <- merge(d, geo, by= "location", all.x = TRUE)
	
	### Fixing
	
	geo1 <- data.frame(
	   adm2 = c("West Maprusi", "Kassena Nankana East", "Salvelugu", "Wa West", "Tolon/Kumbungu", "Nadowli", "Bongo", "Talensi-Nabdam"),
	   lon = c(-0.853,  -1.3242, -0.824, -2.6395, -1.066, -2.663, -0.807, -0.7655),
	   lat = c(10.4143, 10.753, 9.6163, 10.006, 9.4330, 10.3668, 10.909, 10.7039)
	)
	
	d <- merge(d, geo1, by= "adm2", all.x = TRUE)
	
	d$longitude[is.na(d$longitude)] <- d$lon[is.na(d$longitude)]
	d$latitude[is.na(d$latitude)] <- d$lat[is.na(d$latitude)]
	d$lat <- d$lon <- NULL
	
	
	
	
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
	
	
	#### fixing crop names
	P <- carobiner::fix_name(d$crop)
	P <- gsub("ayoyo", "unknown", P)
	P <- gsub("bambara nuts", "groundnut", P)
	P <- gsub("^bean$", "common bean", P)
	P <- gsub("fallow", "none", P)
	P <- gsub("garden eggs", "eggplant", P)
	P <- gsub("irish potato", "potato", P)
	P <- gsub("other cereals", "cereal", P)
	P <- gsub("pigeonpea", "pigeon pea", P)
	P <- gsub("planted trees", "unknown", P)
	P <- gsub("red pepper", "pepper", P)
	P <- gsub("tomatoes", "tomato", P)
	P <- gsub("sweet potato", "sweetpotato", P)
	P <- gsub("other uses|other pulses, nuts|other roots, tubers|other crops", "unknown", P)
	P <- gsub("other vegetable", "vegetable", P)
	d$crop <- P 
	
	#### fixing land_prep method
	P <- carobiner::fix_name(d$land_prep_method)
	P <- gsub("hand hoe", "hoeing", P)
	P <- gsub("tractor, disc plough", "mechanical puddling;ploughing", P)
	P <- gsub("zero/minimum tillage", "minimum tillage", P)
	P <- gsub("animal, disc plough", "manual puddling;ploughing", P)
	P <- gsub("tractor, mouldboard plough", "mechanical puddling;ploughing", P)
	P <- gsub("animal, mouldboard plough", "manual puddling;ploughing", P)
	P <- gsub("mixed method|other", "unknown", P)
	P <- gsub("tied ridge tillage", "tied ridges", P)
	d$land_prep_method <- P
	
	#### Fixing fertilizer type
	P <- carobiner::fix_name(d$fertilizer_type)
	P <- gsub("S. Compound", "S-compound", P)
	P <- gsub("Urea", "urea", P)
	P <- gsub("Super D", "DSP", P)
	P <- gsub("A combination|Other", "unknown", P)
	P <- gsub("SA", "AS", P)
	P <- gsub("D. Compound", "D-compound", P)
	P <- gsub("None", "none", P)
	d$fertilizer_type <- P
	
	### Fixing OM_type
	d$OM_type <- ifelse(grepl("Crop residue|Household|A combination", d$OM_type), "farmyard manure;unknown", d$OM_type)
	P <- carobiner::fix_name(d$OM_type)
	P <- gsub("manure;None", "farmyard manure", P)
	P <- gsub("manure;Mulch/compost", "farmyard manure;compost", P)
	P <- gsub("manure;Other", "farmyard manure;unknown", P)
   d$OM_type <- P
   
   
   d$soil_texture <- gsub("other", NA, d$soil_texture)
   
   d$country <- "Ghana"
   d$currency = "GHS"
   d$on_farm <- FALSE
   d$is_survey <- TRUE
   d$planting_date <- as.character(NA)
   d$geo_from_source <- FALSE
   d$yield_part <- "none"
   d$trial_id <- paste(d$hhid, d$location, sep="-")
   d$yield_moisture <- as.numeric(NA)
   d$yield_isfresh <- TRUE
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   ### remove duplicate rows
   d <- unique(d)
   
	carobiner::write_files(path, meta, d)
}




