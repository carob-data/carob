# R script for "carob"
# license: GPL (>=3)

## ISSUES
###  Some units need to be converted from local units to standard units (e.g., some yield values are given in crates).
### Some plot areas are extremely high and probably need to be checked.


carob_script <- function(path) {

"
Bean technology adoption and its impact on smallholder farmers’ productivity, bean consumption; and food security data, Zimbabwe

This data was derived from the 2018 nationwide endline survey of a project that evaluated the adoption and impact of improved bean varieties in Zimbabwe. The country, a PABRA member, was a flagship for a 2014 initiative aimed at enhancing food security and incomes through bean research. The endline survey was implemented as an activity of the flagship initiative funded by Swiss Agency for Development and Cooperation (SDC) and Global Affairs Canada (formerly Canadian International Development Agency [CIDA]) through the Pan-Africa Bean Research Alliance (PABRA)/CIAT and the Southern Africa Bean Research Network (SABRN). The dataset was analyzed to determine the degree of influence project interventions had on bean production, the utilization of promoted technologies, and overall household welfare. Furthermore, the collection aimed to derive lessons on the efficacy and underlying reasons for the outcomes of various interventions.

The data originate from a panel of households established in 2016, enabling a longitudinal analysis that accounts for time-invariant unobservable household characteristics. The dataset includes household, plot, and village-level information organized in modules: 1) Household & Location: (identification, Demographics, assets, and social networks), 2) Agricultural Practices (Bean varieties, cultivation methods, inputs, and harvests), 3) Land holding and utilization, 4) Field level data on bean area, production in previous cropping seasons 5) bean variety identification sample collection, 6) Institutional Access (Credit and agricultural services), 7) Post-Harvest & Markets (Bean utilization and marketing); 8) Non-chemical bean management strategies and post-harvest handling (ICM/IPM); 9) farmer preferences; 10) Food Security (variety Trait preferences and food security indicators) and 11) Income from others/none agriculture to your household.  The data are organized into 19 separate files, each of which contains a common unique household identifier (hhid) that can be used to merge them Additional information was also collected at community level through focus group discussions, used to assess the extent of spillover effects by profiling direct and indirect intervention communities. By collecting data from the same households and communities, we aimed to address potential unobservable effects assumed to be fixed over the three years.

 Methodology:Trained enumerators collected data using a pre-tested digital questionnaire (CAPI) from the heads or spouses of the same households originally surveyed in 2016. This fourth-year, follow-up survey captured both household and agricultural plot-level information. To assess spillover effects, we also conducted community-level key informant interviews, profiling both direct and indirect beneficiary communities. This panel design allows for controlling unobservable, time-invariant characteristics over the three-year period.
"

	uri <- "doi:10.7910/DVN/HU2BLN"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "AGRITEX; DRSS; CIAT", ## DRSS: Department of Research and Specialist Services
		publication = NA,
		project = NA,
		carob_date = "2026-02-26",
		design = "unitOfAnalysis",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		carob_contributor = "Cedric Ngakou",
		completion = 70,	
		notes = "We process only files with useful data for carob."
	)
	

	f1 <- ff[basename(ff) == "Bean fields production and harvest.dta"]
	f2 <- ff[basename(ff) == "Bean sale transactions.dta"]
	f3 <- ff[basename(ff) == "Bean seed sources for varieties planted.dta"]
	f4 <- ff[basename(ff) == "Bean variety identification.dta"]
	f5 <- ff[basename(ff) == "chemical fertilizer use.dta"]
	f6 <- ff[basename(ff) == "Cropped area-all crops.dta"]
	f7 <- ff[basename(ff) == "Crops grown.dta"]
	f8 <- ff[basename(ff) == "HH roster_an.dta"]
	f9 <- ff[basename(ff) == "Input use on bean fields.dta"]
	f10 <- ff[basename(ff) == "IPM&ICM management strategies.dta"]
	f11 <- ff[basename(ff) == "Labour on bean fields.dta"]
	f12 <- ff[basename(ff) == "Land ownership.dta"]
	f13 <- ff[basename(ff) == "Off-farm Income.dta"]
	f14 <- ff[basename(ff) == "Part E- plot level varieties data.dta"]
	f15 <- ff[basename(ff) == "Post harvest handling.dta"]
	f16 <- ff[basename(ff) == "Seed types and sources.dta"]
	f17 <- ff[basename(ff) == "Trainings attended.dta"]
	f18 <- ff[basename(ff) == "ZIMBABWE-2018-HH-IMPACT-SURVEY_an.dta"]

	r1 <- haven::read_dta(f1) |> carobiner:::unlabel()
	#r2 <- haven::read_dta(f2) |> carobiner:::unlabel()
	#r3 <- haven::read_dta(f3)|> carobiner:::unlabel()
	r4 <- haven::read_dta(f4) |> carobiner:::unlabel()
	r5 <- haven::read_dta(f5)|> carobiner:::unlabel()
	r6 <- haven::read_dta(f6, .name_repair = "universal")|> carobiner:::unlabel()
	r7 <- haven::read_dta(f7, encoding = "latin1")|> carobiner:::unlabel()
	r8 <- haven::read_dta(f8)|> carobiner:::unlabel()
	r9 <- haven::read_dta(f9)|> carobiner:::unlabel()
	#r10 <- haven::read_dta(f10)|> carobiner:::unlabel()
	r11 <- haven::read_dta(f11) |> carobiner:::unlabel()
	r12 <- haven::read_dta(f12)|> carobiner:::unlabel()
	r13 <- haven::read_dta(f13)|> carobiner:::unlabel()
	r14 <- haven::read_dta(f14)
	#r15 <- haven::read_dta(f15)
	#r16 <- haven::read_dta(f16)
	#r17 <- haven::read_dta(f17)
	#r18 <- haven::read_dta(f18)|> carobiner:::unlabel()

#### process files
	
	d1 <- data.frame(
		hhid = as.character(r1$hhid),
		field_id = as.character(r1$e5),
		field_name = r1$e6,
		field_size = ifelse(grepl("Acre", r1$e81unit), r1$e810*0.4047, r1$e81) ,
		plot_slope = r1$e91,
		soil_texture = ifelse(is.na(r1$e92), r1$oth_e92, r1$e92),
		farmland_owned = ifelse(grepl("Owned", ifelse(is.na(r1$e92), r1$oth_e93, r1$e93)), "TRUE", ifelse(is.na(r1$e92), r1$oth_e93, r1$e93)),
		irrigated = grepl("Yes", r1$e106),
		variety_code = as.character(r1$variety_n),
		#harvest_pct = r1$h02,
		dmy_total = ifelse(grepl("90", r1$h04_unit), r1$h04*90, 
		           ifelse(grepl("50", r1$h04_unit), r1$h04*50, 
		           ifelse(grepl("Ton", r1$h04_unit), r1$h04*1000, r1$h04))),
		adm2 = trimws(r1$dist),
		adm1 = trimws(r1$prov),
		#key = r1$key,
		parent = r1$parent_key,
		ward_num = r1$ward_num
	)

	
	####
	d2 <- data.frame(
	   hhid = as.character(r4$hhid),
	   field_id = as.character(r4$field_id),
	   variety_code = as.character(r4$variety_id),
	   variety = r4$variety_name1,
	   #variety_trait = r4$var_color,
	   adm1 = trimws(r4$prov),
	   adm2 = trimws(r4$dist),
	   parent = r4$parent_key,
	   ward_num = r4$ward_num
	)
	
	### merge d and d2
	
	d <- merge(d1, d2, by = intersect(names(d1), names(d2)), all = TRUE)
	
	###
	d3 <- data.frame(
	   hhid = as.character(r5$hhid),
	   field_id = as.character(r5$field_id),
	   fertilizer_amount = ifelse(grepl("50",  r5$e17_unit), r5$e17_qty*50,
	                       ifelse(grepl("20", r5$e17_unit), r5$e17_qty*20, 
	                       ifelse(grepl("90", r5$e17_unit), r5$e17_qty*90,
	                       ifelse(grepl("Millitre|Gram", r5$e17_unit), r5$e17_qty/1000, 
	                       ifelse(grepl("Ton", r5$e17_unit), r5$e17_qty*1000,
	                       ifelse(grepl("5 litres", r5$e17_unit), r5$e17_qty*5, r5$e17_qty)))))) ,
	   fertilizer_price = r5$e17_cost,
	   fertilizer_type = ifelse(grepl("manure|dressing|999|0|O|Manure", r5$e16_name), "none", r5$e16_name) ,
	   field_name = r5$field_name,
	   adm1 = trimws(r5$prov),
	   adm2 =  trimws(r5$dist),
	   #key = r5$input_key,
	   ward_num = r5$ward_num,
	   parent = r5$parent_key
	)
	
	# merge d and d3
	d3$fertilizer_price <- as.character(d3$fertilizer_price/d3$fertilizer_amount)
	d <- merge(d, d3, by = intersect(names(d), names(d3)), all = TRUE)
	
	######
	d4 <- data.frame(
	   hhid = as.character(r7$hhid),
	   season = trimws(r7$season_name),
	   crop = tolower(r7$crop_name),
	   plot_area =ifelse(grepl("Acre", r7$d_areaunit), r7$d_area*0.4047, r7$d_area)  ,
	   yield = ifelse(grepl("50", r7$d_unit), r7$d_qty*50, 
	           ifelse(grepl("90", r7$d_unit), r7$d_qty*90, 
	           ifelse(grepl("Ton", r7$d_unit), r7$d_qty*1000, r7$d_qty))),
	  
	   yield_marketable = ifelse(grepl("50", r7$d_unit), r7$d_qtysold*50, 
	                     ifelse(grepl("90", r7$d_unit), r7$d_qtysold*90, 
	                     ifelse(grepl("Ton", r7$d_unit), r7$d_qtysold*1000, r7$d_qtysold))) ,
	   crop_price = r7$d_inc,
	   currency = "USD",
	   adm1 = trimws(r7$prov),
	   adm2 = trimws(r7$dist),
	   #key = r7$crops_key,
	   ward_num = r7$ward_num,
	   parent = r7$parent_key,
	   id = as.integer(1: nrow(r7))
	)
	
	d4$crop_price <- d4$crop_price/d4$yield
	# merge d and d4
	d <- merge(d, d4, by = intersect(names(d), names(d4)), all = TRUE)
	
	#### 
	d5 <- data.frame(
	   hhid = as.character(r6$hhid),
	   season = trimws(r6$season),
	   farm_id = r6$parcelnum,
	   plot_area = rowSums(r6[, c("d02_1_1", "d02_1_2", "d02_1_3", "d02_1_4", "d02_1_5", "d02_1_6", "d02_1_7", "d02_1_8")], na.rm = TRUE),
	   parent = r6$parent_key,
	   adm1 = trimws(r6$prov),
	   adm2 = trimws(r6$dist),
	   ward_num = r6$ward_num,
	   unit = r6$d02_1unit
	)
	
	d5$plot_area <- ifelse(grepl("Acre", d5$unit), d5$plot_area*0.4047, d5$plot_area) 
	d5$unit <- NULL
	
	### merge d and d5
	d <- merge(d, d5, by = intersect(names(d), names(d5)), all = TRUE)
	
	
	d6 <- data.frame(
	   hhid = as.character(r8$hhid),
	   farmer_gender = r8$b11,
	   farmer_age = r8$b14,
	   farmer_civil_status = r8$b13,
	   farmer_education = r8$b15,
	   #key = r8$key,
	   parent = r8$parent_key,
	   ward_num = r8$ward_num,
	   adm1 = trimws(r8$prov),
	   adm2 = trimws(r8$dist)
	)
	
	# merge d and d6
	
	d <- merge(d, d6, by = intersect(names(d), names(d6)), all = TRUE)
	
	#####
	d7 <- data.frame(
	   hhid = as.character(r9$hhid),
	   field_id = as.character(r9$field_id),
	   field_name = r9$field_name,
	   OM_used = grepl("Yes", r9$e181),
	   OM_amount = ifelse(grepl("20", r9$e18_unit), r9$e18_qty*20,
	               ifelse(grepl("50", r9$e18_unit), r9$e18_qty*50,
	               ifelse(grepl("90", r9$e18_unit), r9$e18_qty*90,
	               ifelse(grepl("Ton", r9$e18_unit), r9$e18_qty*1000, r9$e18_qty)))),
	   OM_price = r9$e18_cost,
	   #herbicide_used = r9$e19_0,
	   #herbicide_names = r9$e19_0,
	   herbicide_amount1 = ifelse(grepl("Millitre|Gram", r9$e19_unit1), r9$e19_qty1/1000, r9$e19_qty1),
	   herbicide_amount2 = ifelse(grepl("Millitre|Gram", r9$e19_unit2), r9$e19_qty2/1000, r9$e19_qty2),
	   fungicide_amount = r9$e20_qty,
	   insecticide_amount = r9$e20_2_qty,
	   pesticide_amount = r9$e20_3_qty,
	   #key = r9$key,
	   parent = r9$parent_key,
	   ward_num = r9$ward_num,
	   adm1 = trimws(r9$prov),
	   adm2 = trimws(r9$dist)
	)
	
	d7$herbicide_amount1 <- rowSums(d7[, c("herbicide_amount1", "herbicide_amount2")], na.rm = TRUE)
	d7$herbicide_amount1 <- d7$herbicide_amount2 <- NULL
	
	# merge d and d7
	d <- merge(d, d7, by = intersect(names(d), names(d7)), all = TRUE)
	
	
	#########
	d8 <- data.frame(
	   hhid =as.character(r12$hhid),
	   farm_id = r12$d_num,
	   farmland = ifelse(grepl("Acre", r12$d01_2unit),r12$d01_2*0.4047, r12$d01_2),
	   #key = r12$key,
	   parent = r12$parent_key,
	   ward_num = r12$ward_num,
	   adm1 = trimws(r12$prov),
	   adm2 = trimws(r12$dist)
	)
	
	# merge d and d8
	d <- merge(d, d8, by = intersect(names(d), names(d8)), all = TRUE)
	
	### remove duplicate id 
	d <- d[!duplicated(d$id), ]
	d$id <- d$parent <- d$ward_num <- d$farm_id <- NULL
	
	d$planting_date <- ifelse(grepl("2017", d$season), "2017", "2018")
	d$season <- NULL
	d$farmland_owned <- ifelse(grepl("TRUE", d$farmland_owned), d$farmland, NA)
	
	### Fixing yield 
	
	d$yield <- d$yield/d$plot_area # kg/ha
	d$yield_marketable <- d$yield_marketable/d$plot_area
	d$dmy_total <- d$dmy_total/d$plot_area
	d$fertilizer_amount <- d$fertilizer_amount/d$plot_area # kg/ha
	d$plot_area <- d$plot_area*10000 #m2
	
	### adding Lon and lat coordinate
	
	geo <- data.frame(
	   adm2 = c("Nyanga", "Mberengwa", "Chipinge", "Chimanimani", "Guruve", "Centenary", "Marondera", "Uzumba Maramba Pfungwe", "Mwenezi", "Lupane", "Binga", "Insiza", "Gokwe South", "Kwekwe", "Chirumhanzu", "Shurugwi"),
	   longitude = c(32.7403, 29.918, 32.6195, 32.873, 30.7033, 31.1163, 31.5461, 31.9100, 30.727, 27.7591, 27.339, 29.189, 28.922, 29.822, 29.830, 29.9761),
	   latitude = c(-18.218, -20.478, -20.1936, -19.8033, -16.6608, -16.729, -18.1879, -17.227, -21.419, -18.930, -17.622, -19.781, -18.204, -18.924, -18.918, -19.6767),
	   geo_from_source = FALSE
	)
	
	### merge geo and d
	
	d <- merge(d, geo, by ="adm2", all.x = TRUE)
	
	### Fixing soil_texture
	P <- carobiner::fix_name(d$soil_texture)
	P <- gsub("Sandy-loam", "sandy loam", P)
	P <- gsub("Sand", "sand", P)
	P <- gsub("Clay \\(clay loam\\)", "clay loam", P)
	P <- gsub("Other, specify", NA, P)
	d$soil_texture <- P
	
	#### Fixing fertilizer type 
	
	P <- carobiner::fix_name(d$fertilizer_type)
	P <- gsub("Compound D \\(Basal\\)", "D-compound", P)
	P <- gsub("Lime", "lime", P)
	P <- gsub("Urea", "urea", P)
	P <- gsub("Compound S", "S-compound", P)
	P <- gsub("Compound L|coumpound L", "L-compound", P)
	P <- gsub("None", "none", P)
	d$fertilizer_type <- P
	
	### Fixing crop names
	
	P <- carobiner::fix_name(d$crop)
	P <- gsub("bambara nuts", "bambara groundnut", P)
	P <- gsub("beans", "common bean", P)
	P <- gsub("buternut|butternuts|butternuts and covo", "winter squash", P)
	P <- gsub("carrots", "carrot", P)
	P <- gsub("^chilli$|^chilly$", "chili pepper", P)
	P <- gsub("^covo$|covo leaf veg|leaf vegetables covo", "vegetable", P)
	P <- gsub("groundnuts", "groundnut", P)
	P <- gsub("leaf veg", "vegetable", P)
	P <- gsub("leaf vegetables|leafy vegetabbles|leafy vegetable|leafy vegetables", "vegetable", P)
	P <- gsub("melons", "melon", P)
	P <- gsub("^onions$|king onions", "onion", P)
	P <- gsub("paprika", "bell pepper", P)
	P <- gsub("peas|peas", "pea", P)
	P <- gsub("^potatoes$|irish potatoes", "potato", P)
	P <- gsub("^rape$|rape and covo", "rapeseed", P)
	P <- gsub("soya bean", "soybean", P)
	P <- gsub("sweet cabbage|cabbages", "cabbage", P)
	P <- gsub("sweet potatoes", "sweetpotato", P)
	P <- gsub("tabasco chilli", "chili pepper", P)
	P <- gsub("tomatoe|tomatoes", "tomato", P)
	P <- gsub("^vegetables$", "vegetable", P)
	P <- gsub("water melon|water melons", "watermelon", P)
	P <- gsub("vegetableetables", "vegetable", P)
	P <- gsub("winter squash, carrot", "winter squash", P)
	P <- gsub("pea, carrot", "pea", P)
	d$crop <- P
	
	d$country <-  "Zimbabwe"
	d$trial_id <- d$field_name
	d$on_farm <- FALSE
	d$is_survey <- TRUE
	d$yield_part <- "none"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	### remove duplicate data
	d <- unique(d)
	
carobiner::write_files(path, meta, d)

}

