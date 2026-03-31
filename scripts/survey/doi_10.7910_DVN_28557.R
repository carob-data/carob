# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Malawi Africa Research in Sustainable Intensification for the Next Generation (Africa RISING) Baseline Evaluation Survey

As part of the US government&#39;s Feed the Future initiative that aims to address global hunger and food security issues in sub-Saharan Africa, the US Agency for International Development is supporting three multi-stakeholder agricultural research projects under Africa Research In Sustainable Intensification for the Next Generation (Africa RISING - AR) program. The overall aim of the program is to transform agricultural systems through sustainable intensification projects in Ghana, Tanzania, Malawi, Ethiopia and (potentially) Zambia. In Malawi, the project (led by IITA) will be supporting cereal-based Farming Systems.  The International Food Policy Research Institute (IFPRI) leads the monitoring and evaluation (M&#38;E) activities of the AR program. As part of the M&#38;E activities in Malawi, IFPRI contracted Invest in Knowledge Initiative (IKI) to conduct baseline household and community surveys in Ntcheu and Dedza districts.  Ntcheu and Dedza are the two districts in which participatory action research and adaptive experimentation is currently being conducted, led by researchers from Michigan State University (MSU). Interventions in these two districts involve &#34;mother and baby&#34; adaptive trials. Four intervention extension planning areas (EPAs) have been identified within the two districts to implement the research activities, with each EPA having 2 &#34;mother&#34; trials and about 110 &#34;baby&#34; farmers.  The main objective of this survey is to collect high-quality baseline household data to support the M&#38;E activities of the AR program in Malawi. More specifically, the survey aims to collect detailed information on the composition of the household, employment, health, agriculture, income and expenditures, credit, assets, subjective welfare and food security, shocks, and the anthropometric status of children and women.
"

	uri <- "doi:10.7910/DVN/28557"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=8,
		data_organization = "IFPRI",
		publication = NA,
		project = NA,
		carob_date = "2026-03-30",
		design = "unitOfAnalysis",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		carob_contributor = "Cedric Ngakou",
		completion = 70,	
		notes = "we process only files with useful information for carob"
	)
	

	f1 <- ff[basename(ff) == "001_com_sec_a1.dta"]
	f2 <- ff[basename(ff) == "001_sec_a.dta"]
	f3 <- ff[basename(ff) == "002_com_sec_cb.dta"]
	f4 <- ff[basename(ff) == "002_sec_b.dta"]
	f5 <- ff[basename(ff) == "003_com_sec_cc.dta"]
	f6 <- ff[basename(ff) == "003_sec_c.dta"]
	f7 <- ff[basename(ff) == "004_com_sec_cd_1to4.dta"]
	f8 <- ff[basename(ff) == "004_sec_d.dta"]
	f9 <- ff[basename(ff) == "005_com_sec_cd_5to8c.dta"]
	f10 <- ff[basename(ff) == "005_sec_e.dta"]
	f11 <- ff[basename(ff) == "006_com_sec_ce_1ato1b.dta"]
	f12 <- ff[basename(ff) == "006_sec_f.dta"]
	f13 <- ff[basename(ff) == "007_com_sec_ce_2ato2c.dta"]
	f14 <- ff[basename(ff) == "007_sec_g1_1to10.dta"]
	f15 <- ff[basename(ff) == "008_com_sec_ce_3to10.dta"]
	f16 <- ff[basename(ff) == "008_sec_g2_1to14.dta"]
	f17 <- ff[basename(ff) == "009_com_sec_ce_11ato11f.dta"]
	f18 <- ff[basename(ff) == "009_sec_g3_1to13.dta"]
	f19 <- ff[basename(ff) == "010_com_sec_cf.dta"]
	f20 <- ff[basename(ff) == "010_sec_g4_1to12b_1.dta"]
	f21 <- ff[basename(ff) == "011_com_sec_cg_1to4.dta"]
	f22 <- ff[basename(ff) == "011_sec_h.dta"]
	f23 <- ff[basename(ff) == "012_com_sec_cg_5to9c.dta"]
	f24 <- ff[basename(ff) == "012_sec_i.dta"]
	f25 <- ff[basename(ff) == "013_com_sec_cg_10to14.dta"]
	f26 <- ff[basename(ff) == "013_sec_j1.dta"]
	f27 <- ff[basename(ff) == "014_com_sec_ch_1to2c.dta"]
	f28 <- ff[basename(ff) == "014_sec_j2.dta"]
	f29 <- ff[basename(ff) == "015_com_sec_ch_3to4g.dta"]
	f30 <- ff[basename(ff) == "015_sec_k.dta"]
	f31 <- ff[basename(ff) == "016_com_sec_ci.dta"]
	f32 <- ff[basename(ff) == "016_sec_l1.dta"]
	f33 <- ff[basename(ff) == "017_sec_l_6to17c.dta"]
	f34 <- ff[basename(ff) == "018_sec_m.dta"]
	f35 <- ff[basename(ff) == "019_sec_n_1to10.dta"]
	f36 <- ff[basename(ff) == "020_sec_n_12to18.dta"]
	f37 <- ff[basename(ff) == "021_sec_o_1to10.dta"]
	f38 <- ff[basename(ff) == "022_sec_o_11to12b.dta"]
	f39 <- ff[basename(ff) == "023_sec_o_13to14.dta"]
	f40 <- ff[basename(ff) == "024_sec_o_15to16.dta"]
	f41 <- ff[basename(ff) == "025_sec_p.dta"]
	f42 <- ff[basename(ff) == "026_sec_q.dta"]
	f43 <- ff[basename(ff) == "027_sec_r.dta"]
	f44 <- ff[basename(ff) == "028_sec_s1_1to3.dta"]
	f45 <- ff[basename(ff) == "029_sec_s1_4to6.dta"]
	f46 <- ff[basename(ff) == "030_sec_s2_1to3.dta"]
	f47 <- ff[basename(ff) == "031_sec_s2_4to7.dta"]
	f48 <- ff[basename(ff) == "032_sec_t.dta"]
	f49 <- ff[basename(ff) == "033_sec_u.dta"]
	f50 <- ff[basename(ff) == "034_sec_v.dta"]

	r1 <- haven::read_dta(f1)|> carobiner:::unlabel()
	r2 <- haven::read_dta(f2) |> carobiner:::unlabel()
	#r3 <- haven::read_dta(f3)
	r4 <- haven::read_dta(f4) |> carobiner:::unlabel()
	#r5 <- haven::read_dta(f5)
	#r6 <- haven::read_dta(f6)
	#r7 <- haven::read_dta(f7)
	#r8 <- haven::read_dta(f8)
	#r9 <- haven::read_dta(f9)
	r10 <- haven::read_dta(f10) |> carobiner:::unlabel()
	#r11 <- haven::read_dta(f11)
	r12 <- haven::read_dta(f12) |> carobiner:::unlabel()
	#r13 <- haven::read_dta(f13)
	r14 <- haven::read_dta(f14) |> carobiner:::unlabel()
	#r15 <- haven::read_dta(f15)
	r16 <- haven::read_dta(f16) |> carobiner:::unlabel()
	#r17 <- haven::read_dta(f17)
	#r18 <- haven::read_dta(f18)
	#r19 <- haven::read_dta(f19)
	r20 <- haven::read_dta(f20) |> carobiner:::unlabel()
	#r21 <- haven::read_dta(f21)
	r22 <- haven::read_dta(f22) |> carobiner:::unlabel()
	#r23 <- haven::read_dta(f23)
	#r24 <- haven::read_dta(f24)
	# r25 <- haven::read_dta(f25)
	# r26 <- haven::read_dta(f26)
	# r27 <- haven::read_dta(f27)
	# r28 <- haven::read_dta(f28)
	# r29 <- haven::read_dta(f29)
	# r30 <- haven::read_dta(f30)
	r31 <- haven::read_dta(f31) |> carobiner:::unlabel()
	# r32 <- haven::read_dta(f32)
	# r33 <- haven::read_dta(f33)
	# r34 <- haven::read_dta(f34)
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
	# r50 <- haven::read_dta(f50)

	
##### process file
	
	d11 <- data.frame(
	   treatment = r1$site,
	   adm1 = r1$ca1,
	   adm2 = r1$ca2,
	   adm3 = r1$ca3,
	   location = r1$ca4,
	   latitude = r1$ca5a,
	   longitude = r1$ca5b,
	   elevation = r1$ca5c

	)
	
	d12 <- data.frame(
	   hhid = as.character(r2$hhid),
	   adm1 = r2$a1,
	   adm2 = r2$a2,
	   adm3 = r2$a3,
	   location = r2$a4
	   
	)

	### merge d11 and d12
	d1 <- merge(d11, d12, by= intersect(names(d11), names(d12)), all = TRUE)
	
	####
	d2 <- data.frame(
	   hhid = as.character(r4$hhid),
	   farmer_gender = r4$b3,
	   farmer_age = r4$b4a,
	   farmer_education_level = r4$b6,
	   farmer_marital_status = r4$b10
	)
	
	### merge d1 and d2 
	d <- merge(d1, d2, by= intersect(names(d1), names(d2)), all = TRUE)
	
	####
	d3 <- data.frame(
	   hhid = as.character(r10$hhid),
	   field_id = as.character(r10$parcelid),
	   cropland_used = ifelse(grepl("Acre", r10$e3b), r10$e3a*0.4047, 
	                    ifelse(grepl("M2", r10$e3b), r10$e3a/10000, r10$e3a)),
	   farmland_owned = ifelse(grepl("Yes", r10$e4), ifelse(grepl("Acre", r10$e3b), r10$e3a*0.4047, 
	                                                 ifelse(grepl("M2", r10$e3b), r10$e3a/10000, r10$e3a)), NA) ,
	   irrigation_method = ifelse(is.na(r10$e10a), r10$e10b, r10$e10a),
	   irrigated = grepl("irrigation|Multiple",ifelse(is.na(r10$e10a), r10$e10b, r10$e10a) ),
	   irrigation_source = r10$e11,
	   soil_type = r10$e13,
	   soil_color = r10$e15,
	   plot_slope = r10$e16
	   
	)
	
	#### merge d and d3 
	
	d <- merge(d, d3[!duplicated(d3$hhid),], by= intersect(names(d), names(d3)), all = TRUE)
	
	####
	d4 <- data.frame(
	   hhid = as.character(r12$hhid),
	   field_id = as.character(r12$f2a),
	   land_prep_method =  ifelse(!is.na(r12$f6b), paste(r12$f6a, "minimum tillage", sep = ";"), r12$f6a) ,
	   OM_amount_manure = ifelse(grepl("50kg", r12$f9b), r12$f9a*50,
	                      ifelse(grepl("90kg", r12$f9b), r12$f9a*90, 
	                      ifelse(grepl("Plate", r12$f9b), r12$f9a*0.32, 
	                      ifelse(grepl("Bucket/Tin", r12$f9b), r12$f9a*16.5,
	                      ifelse(grepl("Heap", r12$f9b), r12$f9a*0.10,
	                      ifelse(grepl("Cane/Basket", r12$f9b), r12$f9a*0.14, r12$f9a)))))),
	   OM_type = ifelse(!is.na(r12$f9a), paste("farmyard manure", r12$f12a, sep = ";"), r12$f12a) ,
	   OM_amount = ifelse(grepl("50kg", r12$f12c), r12$f12b*50,
	               ifelse(grepl("90kg", r12$f12c), r12$f12b*90, 
	               ifelse(grepl("Plate", r12$f12c), r12$f12b*0.32, 
	               ifelse(grepl("Bucket/Tin", r12$f12c), r12$f12b*16.5,
	               ifelse(grepl("Heap", r12$f12c), r12$f12b*0.10,
	               ifelse(grepl("Cane/Basket", r12$f12c), r12$f12b*0.14, 
	               ifelse(grepl("Gram", r12$f12c), r12$f12b/1000, r12$f12b)))))))
	)
	
	### merge d and d4
	d4$OM_amount <- rowSums(d4[, c("OM_amount", "OM_amount_manure")], na.rm = TRUE)
	d4$OM_amount_manure <- NULL
	cc <-  d4[!duplicated(d4[, c("hhid", "field_id")]),]
	d <- merge(d, cc, by= intersect(names(d), names(cc)), all = TRUE)
	
	###
	d5 <- data.frame(
	   hhid = as.character(r14$hhid),
	   season = r14$g1_1,
	   field_id = as.character(r14$g1_2),
	   crop = tolower(r14$g1_4),
	   plot_area = ifelse(grepl("Acre", r14$g1_6b), r14$g1_6a*0.4047,  
	                      ifelse(grepl("M2", r14$g1_6b), r14$g1_6a/10000, r14$g1_6a)),
	   yield = ifelse(grepl("50kg", r14$g1_7b), r14$g1_7a*50,
	           ifelse(grepl("90kg", r14$g1_7b), r14$g1_7a*90, 
	           ifelse(grepl("Plate", r14$g1_7b), r14$g1_7a*0.32, 
	           ifelse(grepl("Unit or Piece", r14$g1_7b), r14$g1_7a*0.40, 
	           ifelse(grepl("Bucket/Tin", r14$g1_7b), r14$g1_7a*16.5,
	           ifelse(grepl("Heap", r14$g1_7b), r14$g1_7a*0.10,
	           ifelse(grepl("Sachet/tube", r14$g1_7b), r14$g1_7a*0.025,
	           ifelse(grepl("Cup", r14$g1_7b), r14$g1_7a*0.06,
	           ifelse(grepl("Bale", r14$g1_7b), r14$g1_7a*0.1, r14$g1_7a))))))))) 
	   
	)
	
	gg <- aggregate(. ~ hhid + season + field_id +crop, d5, function(x) mean(x))
	
	### merge d and gg
	
	d <- merge(d, gg, by= intersect(names(d), names(gg)), all = TRUE)
	#####
	d6 <- data.frame(
	   hhid = as.character(r16$hhid),
	   season = r16$g2_1,
	   field_id = as.character(r16$g2_2),
	   crop = tolower(r16$g2_4),
	   seed_rate = rowSums(r16[, c("g2_5a", "g2_6a", "g2_7a", "g2_8a")], na.rm = TRUE) ,
	   unit1 = ifelse(is.na(r16$g2_5b) & !is.na(r16$g2_6b), r16$g2_6b,
	           ifelse(is.na(r16$g2_5b) & !is.na(r16$g2_7b), r16$g2_7b,
	           ifelse(is.na(r16$g2_5b) & !is.na(r16$g2_8b), r16$g2_8b, r16$g2_5b))) ,
	   seed_price = rowSums(r16[, c("g2_5c", "g2_7c", "g2_8c")], na.rm = TRUE) ,
	   fertilizer_type = r16$g2_11,
	   fertilizer_amount = ifelse(grepl("50kg", r16$g2_13b), r16$g2_13a*50, 
	                       ifelse(grepl("Bucket/Tin", r16$g2_13b), r16$g2_13a*16.5,
	                       ifelse(grepl("Plate", r16$g2_13b), r16$g2_13a*0.32,
	                       ifelse(grepl("Cup", r16$g2_13b), r16$g2_13a*0.06, 
	                       ifelse(grepl("Gram", r16$g2_13b), r16$g2_13a/1000, r16$g2_13a))))),
	   fertilizer_price = r16$g2_13c
	   
	)
	
   d6$seed_rate <- ifelse(grepl("50kg", d6$unit1), d6$seed_rate*50,
                   ifelse(grepl("90kg", d6$unit1), d6$seed_rate*90, 
                   ifelse(grepl("Plate", d6$unit1), d6$seed_rate*0.32, 
                   ifelse(grepl("Unit or Piece", d6$unit1), d6$seed_rate*0.40, 
                   ifelse(grepl("Bucket/Tin", d6$unit1), d6$seed_rate*16.5,
                   ifelse(grepl("Heap", d6$unit1), d6$seed_rate*0.10,
                   ifelse(grepl("Sachet/tube", d6$unit1), d6$seed_rate*0.025,
                   ifelse(grepl("Cup", d6$unit1), d6$seed_rate*0.06,
                   ifelse(grepl("Bale", d6$unit1), d6$seed_rate*0.1, d6$seed_rate)))))))))	
	d6$unit1 <- NULL
   
	d6 <- d6[d6$seed_rate>0, ]
	#### merge d and d6
	
	gg1 <- aggregate(. ~ hhid + season + field_id +crop +fertilizer_type, d6, function(x) mean(x))
	
	d <- merge(d, gg1, by= intersect(names(d), names(gg1)), all = TRUE)
	
	#####
	d7 <- data.frame(
	   hhid = as.character(r20$hhid),
	   crop = tolower(r20$g4_2),
	   seed_source = r20$g4_6,
	   variety = ifelse(is.na(r20$g4_10a) & !is.na(r20$g4_10b), r20$g4_10b,
	             ifelse(is.na(r20$g4_10a) & !is.na(r20$g4_10c), r20$g4_10c, r20$g4_10a)) 
	)
	
	### merge d and d7
	d <- merge(d, d7, by= intersect(names(d), names(d7)), all = TRUE)
	
	#####
	d8 <- data.frame(
	   hhid = as.character(r22$hhid),
	   season = r22$h1,
	   crop = tolower(r22$h2),
	   fwy_residue = ifelse(grepl("50kg", r22$h6b), r22$h6a*50,
	                 ifelse(grepl("90kg", r22$h6b), r22$h6a*90, 
	                 ifelse(grepl("Plate", r22$h6b), r22$h6a*0.32, 
	                 ifelse(grepl("Unit or Piece", r22$h6b), r22$h6a*0.40, 
	                 ifelse(grepl("Bucket/Tin", r22$h6b), r22$h6a*16.5,
	                 ifelse(grepl("Heap", r22$h6b), r22$h6a*0.10,
	                 ifelse(grepl("Sachet/tube", r22$h6b), r22$h6a*0.025,
	                 ifelse(grepl("Cup", r22$h6b), r22$h6a*0.06,
	                 ifelse(grepl("Bale", r22$h6b), r22$h6a*0.1, r22$h6a))))))))),
	   
	   residue_prevcrop = ifelse(grepl("50kg", r22$h8b), r22$h8a*50,
	                      ifelse(grepl("90kg", r22$h8b), r22$h8a*90, 
	                      ifelse(grepl("Gram", r22$h8b), r22$h8a/1000, 
	                      ifelse(grepl("Unit or Piece", r22$h8b), r22$h8a*0.40, 
	                      ifelse(grepl("Bucket/Tin", r22$h8b), r22$h8a*16.5,
	                      ifelse(grepl("Heap", r22$h8b), r22$h8a*0.10,
	                      ifelse(grepl("Bale", r22$h8b), r22$h6a*0.1, r22$h8a))))))),
	  
	   crop_price = r22$h16,
	   yield_marketable = ifelse(grepl("50kg", r22$h15b), r22$h15a*50,
	                      ifelse(grepl("90kg", r22$h15b), r22$h15a*90, 
	                      ifelse(grepl("Gram", r22$h15b), r22$h15a/1000, 
	                      ifelse(grepl("Unit or Piece", r22$h15b), r22$h15a*0.40, 
	                      ifelse(grepl("Bucket/Tin", r22$h15b), r22$h15a*16.5,
	                      ifelse(grepl("Heap", r22$h15b),r22$h15a*0.10,
	                      ifelse(grepl("Plate", r22$h15b),r22$h15a*0.32,
	                      ifelse(grepl("Cane/Basket", r22$h15b),r22$h15a*0.14,
	                      ifelse(grepl("Bale", r22$h15b), r22$h6a*0.1, r22$h15a))))))))),
	   market_type = r22$h17
	)
	
	####
	### merge d and d8
	d <- merge(d, d8, by= intersect(names(d), names(d8)), all = TRUE)
	
	d <- d[!is.na(d$latitude),]
   
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
	
	
	#### Fixing crop names 
	P <- carobiner::fix_name(d$crop)
	P <- gsub("bambara nuts", "bambara groundnut", P)
	P <- gsub("^bean$", "common bean", P)
	P <- gsub("chick-peas", "chickpea", P)
	P <- gsub("cow-peas,", "cowpea", P)
	P <- gsub("irish potato", "potato", P)
	P <- gsub("other crops|other perennial|other pulses", "unknown", P)
	P <- gsub("^nuts$", "groundnut", P)
	P <- gsub("other vegetables", "vegetable", P)
	P <- gsub("^peas$", "pea", P)
	P <- gsub("pigeonpea", "pigeon pea", P)
	P <- gsub("sugar cane", "sugarcane", P)
	P <- gsub("sweet potato", "sweetpotato", P)
	P <- gsub("tomatoes", "tomato", P)
	P <- gsub("unknown, nuts", "groundnut", P)
	P <- gsub("cow-peas", "cowpea", P)
	d$crop <- P
	
	d$season <- ifelse(grepl("Rainy", d$season), "wet",
	            ifelse(grepl("Dry", d$season), "dry", d$season))

	
	### Fixing fertilizer type 
	
	P <- carobiner::fix_name(d$fertilizer_type)
	P <- gsub("D. Compound", "D-compound", P)
	P <- gsub("Urea", "urea", P)
	P <- gsub("Other|A combination", "unknown", P)
	d$fertilizer_type <- P

	###
	
	P <- carobiner::fix_name(d$land_prep_method)
	P <- gsub("Zero/minimum tillage;minimum tillage", "minimum tillage", P)
	P <- gsub("Hand hoe", "hoeing", P)
	P <- gsub("Other", "unknown", P)
	P <- gsub("Animal, mouldboard plough", "ploughing", P)
	d$land_prep_method <- P
	
	### Fixing OM_type
	P <- carobiner::fix_name(d$OM_type)
	P <- gsub("A combination of organic inputs", "minimum tillage", "unknown", P)
	P <- gsub("Crop residue from other farms|Crop residue from this farm", "unknown", P)
	P <- gsub("Household refuse|Other", "unknown", P)
	P <- gsub("Mulch/compost", "ploughing", P)
	P <- gsub("None", "none", P)
	d$OM_type <- P
	
	### fixing irrigation
	
	d$irrigation_method <- ifelse(grepl("Groundwater", d$irrigation_method), "groundwater",
	                       ifelse(grepl("Surface", d$irrigation_method), "surface", "none"))
	
	d$country <- "Malawi"
	d$currency = "MWK"
	d$on_farm <- FALSE
	d$is_survey <- TRUE
	d$planting_date <- as.character(NA)
	d$geo_from_source <- TRUE
	d$yield_part <- "none"
	d$trial_id <- paste(d$location, d$hhid, sep="-")
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d <- d[!is.na(d$hhid),]
	### drop duplicate records 
	
	d <- unique(d)
	
	carobiner::write_files(path, meta, d)
}


