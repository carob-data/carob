# R script for "carob"
# license: GPL (>=3)

## ISSUES
### Absence of a converter from local units to standard units
### Some plot areas are extremely large compared to the crop weight harvested on the plot.
### 

carob_script <- function(path) {

"
Ethiopia Africa Research in Sustainable Intensification for the Next Generation (Africa RISING) Baseline Evaluation Survey

As part of the US government's Feed the Future initiative that aims to address global hunger and food security issues in sub-Saharan Africa, the US Agency for International Development is supporting multi-stakeholder agricultural research projects under Africa Research In Sustainable Intensification for the Next Generation (Africa RISING - AR) program. The overall aim of the program is to transform agricultural systems through sustainable intensification projects in Ghana, Ethiopia, Tanzania, Malawi, Mali, and (potentially) Zambia. In Ethiopia, the project, led by the International Livestock Research Institute (ILRI), will be supporting crop-livestock farming systems. Multiple participatory and adaptive agricultural interventions are currently taking place in eight kebeles (Goshe Bado, Gudo Beret, Salka, Ilu-Sanbitu, Jawe, Upper Gana, Emba Hasti and Tsibet) in four regions (Amhara, Oromia, Southern Nations Nationalities and Peoples (SNNP), and Tigray) in Ethiopia, led by researchers from the ILRI. Experts from ILRI have supported or introduced intercropping, new crop varieties, water conservation practices, and integrated tree cropping.  The International Food Policy Research Institute (IFPRI) leads the monitoring and evaluation (M&E) activities of the AR program. As part of the M&E activities in Ethiopia, IFPRI contracted BDS Center for Development Research to conduct baseline household and community surveys in Amhara, Oromia, SNNP, and Tigray regions. The main objective of this survey is to collect high-quality baseline household data to support the M&E activities of the AR Program in Ethiopia. More specifically, the survey aims to collect detailed information on the composition of the household, employment, health, agriculture, income and expenditures, credit, assets, subjective welfare and food security, shocks, and the anthropometric status of children and women.
"

	uri <- "doi:10.7910/DVN/H6RWOO"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=5,
		data_organization = "IFPRI",
		publication = NA,
		project = NA,
		carob_date = "2026-02-23",
		design = "unitOfAnalysis",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		carob_contributor = "Cedric Ngakou",
		completion = 70,	
		notes = "We only process files with useful information for carob"
	)
	

	f1 <- ff[basename(ff) == "005_Interview.dta"]
	f2 <- ff[basename(ff) == "006_HH_visit.dta"]
	f3 <- ff[basename(ff) == "007_SectionB.dta"]
	f4 <- ff[basename(ff) == "008_SectionC.dta"]
	f5 <- ff[basename(ff) == "009_SectionD.dta"]
	f6 <- ff[basename(ff) == "010_SectionE.dta"]
	f7 <- ff[basename(ff) == "011_SectionF.dta"]
	f8 <- ff[basename(ff) == "012_SectionG1.dta"]
	f9 <- ff[basename(ff) == "013_SectionG2.dta"]
	f10 <- ff[basename(ff) == "014_SectionG3.dta"]
	f11 <- ff[basename(ff) == "015_SectionG4.dta"]
	f12 <- ff[basename(ff) == "016_SectionH.dta"]
	f13 <- ff[basename(ff) == "017_SectionI.dta"]
	f14 <- ff[basename(ff) == "018_SectionJ1.dta"]
	f15 <- ff[basename(ff) == "019_SectionJ2a.dta"]
	f16 <- ff[basename(ff) == "020_SectionJ2b.dta"]
	f17 <- ff[basename(ff) == "021_SectionK1.dta"]
	f18 <- ff[basename(ff) == "022_SectionK2.dta"]
	f19 <- ff[basename(ff) == "023_SectionL.dta"]
	f20 <- ff[basename(ff) == "024_SectionM1.dta"]
	f21 <- ff[basename(ff) == "025_SectionM2.dta"]
	f22 <- ff[basename(ff) == "026_SectionN1.dta"]
	f23 <- ff[basename(ff) == "027_SectionN2.dta"]
	f24 <- ff[basename(ff) == "028_SectionN3.dta"]
	f25 <- ff[basename(ff) == "029_SectionN4.dta"]
	f26 <- ff[basename(ff) == "030_SectionO.dta"]
	f27 <- ff[basename(ff) == "031_SectionP.dta"]
	f28 <- ff[basename(ff) == "032_SectionQ1a.dta"]
	f29 <- ff[basename(ff) == "033_SectionQ1b.dta"]
	f30 <- ff[basename(ff) == "034_SectionQ2a.dta"]
	f31 <- ff[basename(ff) == "035_SectionQ2b.dta"]
	f32 <- ff[basename(ff) == "036_SectionR1.dta"]
	f33 <- ff[basename(ff) == "037_SectionR2.dta"]
	f34 <- ff[basename(ff) == "038_SectionS.dta"]
	f35 <- ff[basename(ff) == "039_SectionT.dta"]
	f36 <- ff[basename(ff) == "040_Filter.dta"]
	f37 <- ff[basename(ff) == "041_SectionCA.dta"]
	f38 <- ff[basename(ff) == "042_SectionCB.dta"]
	f39 <- ff[basename(ff) == "043_SectionCC.dta"]
	f40 <- ff[basename(ff) == "044_SectionCD1.dta"]
	f41 <- ff[basename(ff) == "045_SectionCD2.dta"]
	f42 <- ff[basename(ff) == "046_SectionCE1.dta"]
	f43 <- ff[basename(ff) == "047_SectionCE2.dta"]
	f44 <- ff[basename(ff) == "048_SectionCE3.dta"]
	f45 <- ff[basename(ff) == "049_SectionCE4.dta"]
	f46 <- ff[basename(ff) == "050_SectionCF.dta"]
	f47 <- ff[basename(ff) == "051_SectionCG1.dta"]
	f48 <- ff[basename(ff) == "052_SectionCG2.dta"]
	f49 <- ff[basename(ff) == "053_SectionCG3.dta"]
	f50 <- ff[basename(ff) == "054_SectionCH1.dta"]
	f51 <- ff[basename(ff) == "055_SectionCH2.dta"]
	f52 <- ff[basename(ff) == "sectionCI1.dta"]
	f53 <- ff[basename(ff) == "SectionCI2.dta"]

	r1 <- haven::read_dta(f1) |> carobiner:::unlabel()
	#r2 <- haven::read_dta(f2) |> carobiner:::unlabel()
	#r3 <- haven::read_dta(f3) |> carobiner:::unlabel()
	#r4 <- haven::read_dta(f4) |> carobiner:::unlabel()
	#r5 <- haven::read_dta(f5) |> carobiner:::unlabel()
	r6 <- haven::read_dta(f6) |> carobiner:::unlabel()
	r7 <- haven::read_dta(f7) |> carobiner:::unlabel()
	r8 <- haven::read_dta(f8) |> carobiner:::unlabel()
	r9 <- haven::read_dta(f9) |> carobiner:::unlabel()
	#r10 <- haven::read_dta(f10) |> carobiner:::unlabel()
	r11 <- haven::read_dta(f11) |> carobiner:::unlabel()
	r12 <- haven::read_dta(f12) |> carobiner:::unlabel()
	r13 <- haven::read_dta(f13)|> carobiner:::unlabel()
	#r14 <- haven::read_dta(f14) |> carobiner:::unlabel()
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
	# r35 <- haven::read_dta(f35) |> carobiner:::unlabel()
	#r36 <- haven::read_dta(f36) |> carobiner:::unlabel()
	r37 <- haven::read_dta(f37) |> carobiner:::unlabel()
	r38 <- haven::read_dta(f38) |> carobiner:::unlabel()
	# r39 <- haven::read_dta(f39) #|> carobiner:::unlabel()
	# r40 <- haven::read_dta(f40) #|> carobiner:::unlabel()
	# r41 <- haven::read_dta(f41) #|> carobiner:::unlabel()
	# r42 <- haven::read_dta(f42) #|> carobiner:::unlabel()
	# r43 <- haven::read_dta(f43) #|> carobiner:::unlabel()
	# r44 <- haven::read_dta(f44) #|> carobiner:::unlabel()
	# r45 <- haven::read_dta(f45) #|> carobiner:::unlabel()
	# r46 <- haven::read_dta(f46) #|> carobiner:::unlabel()
	# r47 <- haven::read_dta(f47) #|> carobiner:::unlabel()
	# r48 <- haven::read_dta(f48) #|> carobiner:::unlabel()
	# r49 <- haven::read_dta(f49) #|> carobiner:::unlabel()
	# r50 <- haven::read_dta(f50) #|> carobiner:::unlabel()
	r51 <- haven::read_dta(f51) |> carobiner:::unlabel()
	r52 <- haven::read_dta(f52) |> carobiner:::unlabel()
	r53 <- haven::read_dta(f53)|> carobiner:::unlabel()

#### process 
	d1 <- data.frame(
	   hhid = as.character(r1$hhid),
	   group = tolower(r1$group),
	   adm1 = carobiner::fix_name(r1$a1, "Title") ,
	   adm2 = r1$a2,
	   adm3 = r1$a3,
	   location = r1$a4,
	   translator_used = grepl("YES", r1$a17),
	   farmer_religion = r1$a19
	   
	)
	
	### 
	 d2 <- data.frame(
	    hhid = as.character(r6$hhid),
	    group = tolower(r6$group),
	    adm1 = carobiner::fix_name(r6$a1, "Title"),
	    adm2 = r6$a2,
	    adm3 = r6$a3,
	    location = r6$a4,
	    field_id = r6$id,
	    farmland = r6$e3a,
	    farmland_owned = ifelse(grepl("Yes",  r6$e4), r6$e3a, NA),
	    irrigation_method = ifelse(grepl("Rain", ifelse(is.na(r6$e10a), r6$e10b, r6$e10a)), "none", 
	                        ifelse(grepl("Groundwater", ifelse(is.na(r6$e10a), r6$e10b, r6$e10a)), "sub-irrigation", 
	                        ifelse(grepl("Surface", ifelse(is.na(r6$e10a), r6$e10b, r6$e10a)),"surface", "unknown"))) ,
	    irrigated = !grepl("Rain", ifelse(is.na(r6$e10a), r6$e10b, r6$e10a)),
	    irrigation_source = trimws(ifelse(is.na(r6$e11), ifelse(is.na(r6$e10a), r6$e10b, r6$e10a), r6$e11)) ,
	    soil_texture = gsub("other", NA, tolower(r6$e13)),
	    soil_color = r6$e15,
	    plot_slope = r6$e16
	 )
	 
	 ## merge d1 and d2
	 
	 d <- merge(d1, d2, by= intersect(names(d1), names(d2)), all = TRUE)
	 
	 ################  ######################
	 d3 <- data.frame(
	    hhid = as.character(r7$hhid),
	    group = tolower(r7$group),
	    adm1 = carobiner::fix_name(r7$a1, "Title"),
	    adm2 = r7$a2,
	    adm3 = r7$a3,
	    location = r7$a4,
	    field_id = as.character(r7$f1a),
	    plot_id = as.character(r7$f2),
	    crop_system = ifelse(grepl("YES", r7$f7), "rotation", "none") ,
	    land_prep_method = as.character(r7$f10b),
	    OM_amount = ifelse(grepl("Quintal", r7$f12b),r7$f12a*100,r7$f12a) ,
	    OM_type = r7$f14,
	    OM_amount1 = ifelse(grepl("Quintal",r7$f15b),r7$f15a*100,r7$f15a),
	    fertilizer_type = r7$f16,
	    fertilizer_amount = ifelse(grepl("Quintal", r7$f18b), r7$f18a*100, 
	                        ifelse(grepl("Gram", r7$f18b), r7$f18a/1000, r7$f18a)) ,
	    fertilizer_price = as.character(r7$f18c),
	    soil_erosion = gsub(";NA", "", paste(r7$f26a, r7$f26b, r7$f26c, sep = ";"))
	    
	 )
	 
	 d3$OM_amount <- rowSums(d3[, c("OM_amount", "OM_amount1")], na.rm = TRUE)
    d3$OM_amount1 <- NULL
	 ### merge d and d3
	 
	 d <- merge(d, d3, by= intersect(names(d), names(d3)), all = TRUE)
	 
	 #### ################################
	 d4 <- data.frame(
	    hhid = as.character(r8$hhid),
	    group = tolower(r8$group),
	    adm1 = carobiner::fix_name(r8$a1, "Title"),
	    adm2 = r8$a2,
	    adm3 = r8$a3,
	    location = r8$a4,
	    season = r8$g1_1,
	    field_id = as.character(r8$g1_2),
	    plot_id = as.character(r8$g1_3),
	    crop = tolower(r8$g1_4),
	    variety_type = ifelse(grepl("YES", r8$g1_5), "improve", 
	                    ifelse(grepl("dont know", r8$g1_5),"unknown", "none" )) ,
	    plot_area = r8$g1_6a,
	    unit_area = r8$g1_6b,
	    planting_area_pct = r8$g1_6c,
	    yield = ifelse(grepl("Quintal", r8$g1_7b),  r8$g1_7a*100, r8$g1_7a),
	    yield_unit = r8$g1_7b
	 )
	 
	 ### merge d and d4
	  
	 d <- merge(d, d4, by = intersect(names(d), names(d4)), all = TRUE)
	 
	 ####################### #########
	 d5 <- data.frame(
	    hhid = as.character(r9$hhid),
	    group = tolower(r9$group),
	    adm1 = carobiner::fix_name(r9$a1, "Title"),
	    adm2 = r9$a2,
	    adm3 = r9$a3,
	    location = r9$a4,
	    season = r9$g2_1,
	    field_id = as.character(r9$g2_2),
	    plot_id = as.character(r9$g2_3),
	    crop = tolower(r9$g2_4),
	    seed_rate1 = ifelse(grepl("Quintal", r9$g2_5b), r9$g2_5a*100, 
	                ifelse(grepl("Gram", r9$g2_5b), r9$g2_5a/100, r9$g2_5a)),
	    
	    seed_rate2 = ifelse(grepl("Quintal", r9$g2_6b), r9$g2_6a*100, 
	                        ifelse(grepl("Gram", r9$g2_6b), r9$g2_6a/100, r9$g2_6a)),
	    
	    seed_rate3 = ifelse(grepl("Quintal", r9$g2_7b), r9$g2_7a*100, 
	                        ifelse(grepl("Gram", r9$g2_7b), r9$g2_7a/100, r9$g2_7a)),
	    
	    seed_rate4 = ifelse(grepl("Quintal", r9$g2_8b), r9$g2_8a*100, 
	                        ifelse(grepl("Gram", r9$g2_8b), r9$g2_8a/100, r9$g2_8a)),
	    
	    seed_rate5 = ifelse(grepl("Quintal", r9$g2_9b), r9$g2_9a *100, 
	                        ifelse(grepl("Gram", r9$g2_9b), r9$g2_9a /100, r9$g2_9a))
	    
	 )
	 
	 d5$seed_rate <- rowSums(d5[, c("seed_rate1", "seed_rate2", "seed_rate3", "seed_rate4", "seed_rate5")], na.rm = TRUE)
	 d5$seed_rate1 <- d5$seed_rate2 <- d5$seed_rate3 <- d5$seed_rate4 <- d5$seed_rate5 <- NULL
	 #### merge d and d5
	 
	 d <- merge(d, d5, by = intersect(names(d),names(d5)), all = TRUE)
	 
	 ############################## 
	 d6 <- data.frame(
	    hhid = as.character(r11$hhid),
	    group = tolower(r11$group),
	    adm1 = carobiner::fix_name(r11$a1, "Title"),
	    adm2 = r11$a2,
	    adm3 = r11$a3,
	    location = r11$a4,
	    crop = tolower(r11$g4_2),
	    seed_source = r11$g4_6,
	    variety = ifelse(is.na(r11$g4_10a) & !is.na(r11$g4_10b), r11$g4_10b,
	              ifelse(is.na(r11$g4_10a) & !is.na(r11$g4_10c), r11$g4_10c,  r11$g4_10a)),
	    variety_traits = ifelse(is.na(r11$g4_12a) & !is.na(r11$g4_12b), r11$g4_12b, r11$g4_12a) 
	    
	 )
	 
	 d <- merge(d, d6, by = intersect(names(d), names(d6)), all = TRUE) 
	 
	 
	 ### #######################"
	 d7 <- data.frame(
	    hhid = as.character(r12$hhid),
	    group = tolower(r12$group),
	    adm1 = carobiner::fix_name(r12$a1, "Title"),
	    adm2 = r12$a2,
	    adm3 = r12$a3,
	    location = r12$a4,
	    #season = r12$h1,
	    crop = tolower(r12$h2),
	    yield_loss = r12$h3c,
	    dmy_residue = ifelse(grepl("Quintal", r12$h6b), r12$h6a*100, r12$h6a) ,
	    #unit_stover = r12$h6b,
	    residue_prevcrop =ifelse(grepl("Quintal", r12$h8b), r12$h8a*100, r12$h8a) ,
	     #r12$h8b,
	    #r12$h8c  
	    yield_marketable = ifelse(grepl("Quintal", r12$h13b), r12$h13a*100, r12$h13a),
	    crop_price = r12$h14,
	    currency = "ETB",
	    market_type = r12$h16
	 )
	
	 #### merge d and d7
	 d <- merge(d, d7, by = intersect(names(d), names(d7)), all = TRUE)
	 
	 
	 #### Adding long and lat coordinate
	 d9 <-  data.frame(
	    id = r37$cid,
	    adm1 =carobiner::fix_name(r37$ca1, "Title"),
	    adm2 = r37$ca2,
	    adm3 = gsub("Lemo", "Lemmo", r37$ca3),
	    location = r37$ca4,
	    latitude = r37$ca5a_degree + r37$ca5a_minute/60 + r37$ca5a_second/3600,
	    longitude = r37$ca5b_degree + r37$ca5b_minute/60 + r37$ca5b_second/3600 ,
	    elevation = r37$ca5c,
	    geo_from_source = TRUE
	 )
	 
##### merge d and d8	 
	 d <- merge(d, d9, by= intersect(names(d), names(d9)), all = TRUE)
	 
	 #### 
	  d12 <- data.frame(
	     id = r53$cid,
	     adm1 = carobiner::fix_name(r53$ca1, "Title") ,
	     adm2 = r53$ca2,
	     adm3 = gsub("Lemo", "Lemmo", r53$ca3),
	     location = r53$ca4,
	     unit_area= r53$ci5,
	     convert_factor_land = r53$ci6
	  )
	 
	 
	 d12[d12==-99|d12== 99|d12==-0.25] <- NA
	 d12 <- d12[!is.na(d12$convert_factor_land),]
	 
	 ### adding covertor for local unit to standart unit of lan area
	 d <- merge(d, d12, by= intersect(names(d), names(d12)), all.x = TRUE)
	 
	 d$plot_area <- d$plot_area*d$convert_factor_land
	 
	 
	 ### remove 99 and -99 in the data
	 d[d==-99| d==99|d==-198| d==-9900] <- NA
	 d <- d[which(d$plot_area!=0),]
	 d$yield <- d$yield/d$plot_area
	 d$yield_marketable <- d$yield_marketable/d$plot_area
	 d$dmy_residue <- d$dmy_residue/d$plot_area
	 d$yield_loss <- d$yield_loss/d$plot_area
	 d$plot_area <- d$plot_area*10000 # m2
	 d$adm1 <- gsub("SNNPR", "Snnpr", d$adm1)
	 
	 #d <- d[grepl("Quintal|Kilogrammes|Gram", d$yield_unit),]
	 d$id <- d$season <- d$unit_area <- d$yield_unit <- d$group <-d$convert_factor_land <- d$planting_area_pct <- NULL
	 ### fixing fertilizer type
	 
	 P <- carobiner::fix_name(d$fertilizer_type)
	 P <- gsub("Urea and DAP", "urea;DAP", P)
	 P <- gsub("Urea", "urea", P)
	 P <- gsub("None", "none", P)
	 d$fertilizer_type <- P
	 
	 ## Fixing crop names
	 d$crop <- ifelse(grepl("other", d$crop), "none", d$crop)
	 P <- carobiner::fix_name(d$crop)
	 P <- gsub("abishe", "fenugreek", P)
	 P <- gsub("cabbage/tikel gomen", "cabbage", P)
	 P <- gsub("chick-peas", "chickpea", P)
	 P <- gsub("cow-pea \\(ater\\)", "cowpea", P)
	 P <- gsub("ensosela", "balsam", P)
	 P <- gsub("eucalyptus", "none", P)
	 P <- gsub("fallow", "none", P)
	 P <- gsub("ginge", "ginger", P)
	 P <- gsub("green pepper", "pepper", P)
	 P <- gsub("greens/gomen", "gomenzer", P)
	 P <- gsub("horricot beans/boleke", "common bean", P)
	 P <- gsub("horse/faba beans", "faba bean", P)
	 P <- gsub("khat/chat", "khat", P)
	 P <- gsub("lentils", "lentil", P)
	 P <- gsub("linseed/telba", "flax", P)
	 P <- gsub("niger seed/neug", "niger", P)
	 P <- gsub("pasture/grazing", "pasture", P)
	 P <- gsub("planted fodder", "none", P)
	 P <- gsub("planted trees", "none", P)
	 P <- gsub("red/chili pepper", "chili pepper", P)
	 P <- gsub("sesame/selit", "sesame", P)
	 P <- gsub("sugar cane", "sugarcane", P)
	 P <- gsub("sun flower/suf", "sunflower", P)
	 P <- gsub("teff-black/mixed", "teff", P)
	 P <- gsub("teff-white", "teff", P)
	 d$crop <- P
	
	 ## fixing OM_type
	 
	 P <- carobiner::fix_name(d$OM_type)
	 P <- gsub("A combination of organic inputs|Other", "unknown", P)
	 P <- gsub("Crop residue from this farm|Crop residue from other farms", "unknown", P)
	 P <- gsub("Household waste", "unknown", P)
	 P <- gsub("Mulch/compost", "compost", P)
	 P <- gsub("None", "none", P)
	 d$OM_type <- P
	 
	 
	 ### fixing long and lat errors
	 d$longitude[d$location=="Upper Gana"] <- 38.717
	 d$latitude[d$location=="Upper Gana"] <- 9.0111 
	 d$geo_from_source[d$location=="Upper Gana"] <- FALSE   
	 d$country <- "Ethiopia"
	 d$trial_id <- paste(d$location, d$hhid, sep = "-")
	 d$planting_date <- as.character(NA) 
	 d$on_farm <- FALSE
	 d$is_survey <- TRUE
	 d$yield_part <- "none"
	 d$yield_moisture <- as.numeric(NA)
	 yield_isfresh <- TRUE
	 d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

	### remove duplicate 
	 
	 d <- unique(d)
	 
carobiner::write_files(path, meta, d)

}

