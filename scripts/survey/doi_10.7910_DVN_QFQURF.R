# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Nature+ Quantitative Baseline Household & Worker Survey, Kenya

In 2023, the Nature Positive Solutions (Nature+) baseline survey was conducted in Kenya, focusing on the Counties of Kisumu, Vihiga, and Kajiado. The study aimed to describe the socio-economic conditions and agricultural systems in these areas, providing a baseline assessment to inform ongoing Nature+ interventions. The survey covered 1,502 smallholder farmer households (752 treated and 750 control) across 25 villages. Data collection employed a two-stage sampling technique and assessed various variables, including socio-economic characteristics, agricultural practices, land use, nutrition, and adoption of Nature+ practices. This data will support the evaluation of Nature+'s impacts on inclusion, poverty reduction, food security, livelihoods, and environmental sustainability. Additionally, the survey included interviews with 1056 workers, covering socio-demographic characteristics, contract types, forced labor, harassment, workplace health and safety, wages, and overtime. All monetary variables are expressed in Kenyan Shilling (KSH).
"

	uri <- "doi:10.7910/DVN/QFQURF"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group, recursive=TRUE)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IFPRI",
		publication = NA,
		project = NA,
		carob_date = "2026-03-11",
		design = "unitOfAnalysis",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		carob_contributor = "Cedric Ngakou",
		completion = 70,	
		notes = "We only process file with useful information for Carob"
	)
	

	#f1 <- ff[basename(ff) == "Data"]
	#r1 <- haven::read_dta(paste(f1, "_CONVFAC_area.dta", sep = "/"))|> carobiner:::unlabel()
	#r2 <- haven::read_dta(paste(f1, "_CONVFAC_crop.dta", sep = "/")) |> carobiner:::unlabel()

	r3 <- haven::read_dta(ff[basename(ff) == "_Cover.dta"]) |> carobiner:::unlabel()
	r4 <- haven::read_dta(ff[basename(ff) == "A_hhroster_employment.dta"]) |> carobiner:::unlabel()
	r5 <- haven::read_dta(ff[basename(ff) == "C_parcelid.dta"]) |> carobiner:::unlabel()
	r6 <- haven::read_dta(ff[basename(ff) == "D1_crop_main.dta"]) |> carobiner:::unlabel()
	r7 <- haven::read_dta(ff[basename(ff) == "D2_crop_minor.dta"]) |> carobiner:::unlabel()
	r8 <- haven::read_dta(ff[basename(ff) == "D3_crop_12m.dta"]) |> carobiner:::unlabel()
	r9 <- haven::read_dta(ff[basename(ff) == "F2_livestockprod.dta"]) |> carobiner:::unlabel()
	r10 <- haven::read_dta(ff[basename(ff) == "G_NPS.dta"]) |> carobiner:::unlabel()



	#### process

	d1 <- data.frame(
	  hhid = as.character(r3$hhid),
	  adm1 = r3$county,
	  location = r3$sublocation
	)

	###########
	d2 <- data.frame(
	  hhid = as.character(r4$hhid),
	  farmer_gender = r4$sex,
	  farmer_age = r4$age,
	  farmer_education_level = tolower(r4$education),
	  weeding_done = grepl("yes", r4$manual_weeding),
	  weeding_method = ifelse(grepl("yes", r4$manual_weeding), "manual", "none"),
	  irrigated = grepl("yes", r4$bucket_irrigation)
	  #irrigation_method = ifelse(grepl("yes", r4$bucket_irrigation), "bucket", "none")
	   
	)

	### merge d1 and d2

	d <- merge(d1, d2, by= intersect(names(d1), names(d2)), all = TRUE)

	######

	d3 <- data.frame(
	  hhid = as.character(r5$hhid),
	  plot_id = as.character(r5$parcelid),
	  farmland = r5$areaha
	  #soil_erosion = r5$soil_erosion
	)
	### merge d and d3

	d <- merge(d, d3[!duplicated(d3$hhid),], by=intersect(names(d), names(d3)), all.x = TRUE)


	d4 <- data.frame(
		hhid = as.character(r6$hhid),
		plot_id = as.character(r6$parcelid),
		crop = tolower(r6$cropid),
		variety_code = as.character(r6$varietyid),
		variety = r6$crop_variety,
		intercrops = r6$intercrop,
		plot_area = r6$cropareaha,
		seed_type = r6$seedtype,
		#seed_source = trimws(r6$seed_source),
		seed_rate = rowSums(r6[, c("seed_pur_kg", "seed_free_kg")], na.rm = TRUE),
		seed_price = r6$seed_value,
		OM_used = grepl("yes", r6$organicfert_use),
		OM_type = r6$organic_fertilizer,
		OM_amount = r6$orgfert_kg,
		OM_price = r6$organicfert_value,
		fertilizer_used = grepl("yes", r6$inorganicfert_use),
		fertilizer_type = paste(r6$inorganic_fert1, r6$inorganic_fert2, r6$inorganic_fert3, sep = ";"),
		fertilizer_amount = r6$inorgfert_kg,
		fertilizer_price = r6$inorganic_value,
		insecticide_used = grepl("yes", r6$pesticide_use),
		insecticide_product = paste(r6$pesticide1, r6$pesticide2,r6$pesticide3,r6$pesticide4, sep = ";"),
		insecticide_amount = r6$pest_kg,
		insecticide_price = r6$pesticide_value,
		herbicide_used = grepl("yes", r6$herbicide_use),
		herbicide_product = paste(r6$herbicide1,r6$herbicide2, sep = ";"),
		herbicide_amount = r6$herb_kg,
		herbicide_price = r6$herbicide_value,
		#r6$herbicide_trend,
		yield = r6$harvest_kgs,
		#r6$produce_value,
		yield_marketable = r6$harvest_sold_kg,
		crop_price = r6$unitprice_kg,
		#yield_lost = r6$lost_harvest,
		record_id = as.numeric(1:nrow(r6))
	)


	## merge d and d4
	d <- merge(d, d4, by= intersect(names(d), names(d4)), all = TRUE)
	d <- d[!duplicated(d$record_id),]
	d$record_id <- NULL
	###
	d5 <- data.frame(
		hhid = as.character(r7$hhid),
		plot_id = as.character(r7$parcelid),
		crop = tolower(r7$cropid),
		variety_code = as.character(r7$varietyid),
		variety = r7$crop_variety_minor,
		plot_area = r7$cropareaha,
		seed_type = r7$seedtype_minor,
		#seed_source = trimws(r7$seed_source_minor),
		seed_rate = rowSums(r7[, c("seed_pur_kg", "seed_free_kg")], na.rm = TRUE),
		seed_price = r7$seed_value,
		OM_used = grepl("yes", r7$organicfert_use_minor),
		OM_type =  r7$organic_fertilizer_minor,
		OM_amount = r7$orgfert_kg,
		OM_price = r7$organicfert_value,
		fertilizer_used = grepl("yes", r7$inorganicfert_use),
		fertilizer_type = paste(r7$inorganic_fert1, r7$inorganic_fert2, r7$inorganic_fert3,r7$inorganic_fert4, sep = ";"),
		fertilizer_amount = r7$inorgfert_kg,
		fertilizer_price = r7$inorganic_value,
		insecticide_used = grepl("yes", r7$pesticide_use),
		insecticide_price = r7$pesticide_value,
		insecticide_product = paste(r7$pesticide1, r7$pesticide2,r7$pesticide3,r7$pesticide4, sep = ";"),
		insecticide_amount = r7$pest_kg,
		herbicide_used = grepl("yes", r7$herbicide_use),
		herbicide_price = r7$herbicide_value,
		herbicide_product = r7$herbicide1,
		herbicide_amount = r7$herb_kg,
		yield = r7$harvest_kgs_minor,
		yield_marketable = r7$harvest_sold_kg,
		crop_price = r7$unitprice_kg
	)

### merge d and d5

	d <- merge(d, d5, by= intersect(names(d), names(d5)), all = TRUE)

	### 
	d6 <- data.frame(
		hhid = as.character(r8$hhid),
		plot_id = as.character(r8$parcelid),
		crop = tolower(r8$cropid),
		variety_code = as.character(r8$varietyid),
		variety = r8$crop_variety_12m,
		plot_area = r8$cropareaha,
		seed_type = r8$seedtype_12m,
		#seed_source = trimws(r8$seed_source_12m),
		seed_rate = rowSums(r8[, c("seed_pur_kg", "seed_free_kg")], na.rm = TRUE),
		seed_price = r8$seed_value,
		OM_used = grepl("yes", r8$organicfert_use_12m),
		OM_type =  r8$organic_fertilizer_12m,
		OM_amount = r8$orgfert_kg,
		OM_price = r8$organicfert_value,
		fertilizer_used = grepl("yes", r8$inorganicfert_use),
		fertilizer_type = r8$inorganic_fert1,
		fertilizer_amount = r8$inorgfert_kg,
		fertilizer_price = r8$inorganic_value,
		insecticide_used = grepl("yes", r8$pesticide_use),
		insecticide_price = r8$pesticide_value,
		insecticide_product = r8$pesticide1,
		insecticide_amount = r8$pest_kg,
		herbicide_used = grepl("yes", r8$herbicide_use),
		yield = r8$harvest_kgs_12m,
		yield_marketable = r8$harvest_sold_kg,
		crop_price = r8$unitprice_kg
	)

	d <- merge(d, d6, by= intersect(names(d), names(d6)), all = TRUE)

	### Adding Long and lat coordinate 
	geo <- data.frame(
		location = c("Vigulu", "Emanda", "Ebunangwe", "Awach", "Emmaloba", "Upper Kadiang'a", "Jimo West", "Mambai", "Agoro East", "Olwalo", "Masana", "Mwitubwi", "Entonet", "Enkariak Ronkena", "Kimana", "Ilmedoti", "Loolopon", "Wanondi", "Olchorro", "Itumbu", "Essunza", "Jimo East", "Imisigiyo"),
		longitude = c(34.647, 34.7138, 34.663, 34.973, 34.5494, 34.8835, 34.894, 34.7845,  35.0352, 34.91789, 34.7075, 34.5740, 37.2939, 37.4799, 37.536, 37.49673, 37.52705, 34.6838, 35.99598, 37.6325, 34.6012, 35.00870, 36.7889),
		latitude = c(0.0160, 0.0336, 0.1029, -0.26731, -0.00232, -0.3382, -0.3228, 0.09409, -0.293, -0.32915, -0.000543, 0.01542, -2.5289, -2.9098, -2.7978, -2.5112, -2.93335, 0.1127, -0.8323, -1.5143, 0.07216, -0.3066, -1.2807)
	)

	d <- merge(d, geo, by= "location", all = TRUE)

	
	d$yield <- d$yield/(d$plot_area*0.4047) ## kg/ha
	d$yield_marketable <- d$yield_marketable/(d$plot_area*0.4047)
	d$seed_price <- d$seed_price/d$seed_rate ## KES/kg
	d$seed_rate <- d$seed_rate/(d$plot_area*0.4047) #kg/ha
	d$fertilizer_price <- as.character(d$fertilizer_price/d$fertilizer_amount)
	d$fertilizer_amount <- d$fertilizer_amount/d$plot_area ## kg/ha
	d$OM_price <- d$OM_price/d$OM_amount 
	d$OM_amount <- d$OM_amount/(d$plot_area *0.4047)
	d$herbicide_price <- d$herbicide_price/d$herbicide_amount
	d$herbicide_amount <- d$herbicide_amount/(d$plot_area*0.4047)
	d$insecticide_price <- d$insecticide_price/d$insecticide_amount
	d$insecticide_amount <- d$insecticide_amount/(d$plot_area*0.4047)
	d$plot_area <- d$plot_area*4046.86  # m2
	### Fixing crop names 

	P <- carobiner::fix_name(d$crop)
	P <- gsub("banana \\(ripe\\)|banana \\(cooking\\)|plantain", "banana", P)
	P <- gsub("groundnut/peanut", "groundnut", P)
	P <- gsub("^bean$|french beans|bambara bean", "common bean", P)
	P <- gsub("indigenous vegetables", "vegetable", P)
	P <- gsub("sweet potato", "sweetpotato", P)
	P <- gsub("cowpea leaves", "cowpea", P)
	P <- gsub("lime/lemon", "lemon", P)
	P <- gsub("beetroots", "beetroot", P)
	P <- gsub("pepper \\(bell\\)", "pepper", P)
	P <- gsub("brinjals \\(biringanya\\)", "eggplant", P)
	P <- gsub("butter nuts", "squash", P)
	P <- gsub("capsicum \\(hoho\\)", "bell pepper", P)
	P <- gsub("arrow roots", "unknown", P)
	P <- gsub("cocoyam", "taro", P)
	P <- gsub("commercial grass/pasture", "pasture", P)
	P <- gsub("corriander", "unknown", P)
	P <- gsub("custard apple \\(matomoko\\)", "apple", P)
	P <- gsub("dolicos \\(njahi\\)", "lablab", P)
	P <- gsub("green grams", "mung bean", P)
	P <- gsub("guavas", "guava", P)
	P <- gsub("irish potato", "potato", P)
	P <- gsub("jack fruit", "jackfruit", P)
	P <- gsub("macademia", "macadamia nut", P)
	P <- gsub("maize fodder|maize green", "maize", P)
	P <- gsub("mangoes", "mango", P)
	P <- gsub("nappier grass", "napier grass", P)
	P <- gsub("orange/tangerine", "orange", P)
	P <- gsub("other perennial crops|other pulses|other tree crops", "unknown", P)
	P <- gsub("^nuts$", "groundnut", P)
	P <- gsub("pawpaw/papaya", "pawpaw", P)
	P <- gsub("pearl millet \\(mawele\\)", "pearl millet", P)
	P <- gsub("peas green", "pea", P)
	P <- gsub("pigeon pea \\(mbaazi\\)", "pigeon pea", P)
	P <- gsub("pumpkin leaves", "pumpkin", P)
	P <- gsub("sesame seeds", "sesame", P)
	P <- gsub("sorghum/guinea corn", "sorghum", P)
	P <- gsub("tomatoes|tree tomato", "tomato", P)
	P <- gsub("unknown, nuts", "groundnut", P)
	d$crop <- P

	### Fixing fertilizer type 
	P <- carobiner::fix_name(d$fertilizer_type)
	P <- gsub("NA;|;NA", NA, P)
	P <- gsub("Foliar feeds", "foliar", P)
	P <- gsub("D.compound", "D-compound", P)
	P <- gsub("S.compound", "S-compound", P)
	P <- gsub("Super D", "DSP", P)
	P <- gsub("Yara and other blended brands", "unknown", P)
	P <- gsub("Urea", "urea", P)
	d$fertilizer_type <- P

	d$OM_type <- ifelse(grepl("Animal", d$OM_type), "animal dung", "unknown")

	### Fixing herbicide product 

	P <- carobiner::fix_name(d$herbicide_product)
	P <- gsub("NA;|;NA", "", P)
	P <- gsub("Kausha 480SL", "glyphosate", P)
	P <- gsub("GUARDIAN MAX", "", P)
	P <- gsub("ROUNDUP TURBO SL 450|Roundup Pro", "glyphosate", P)
	P <- gsub("Vanguish", "dicamba", P)
	P <- gsub("Confront", "triclopyr choline", P)
	P <- gsub("ATLANTIS MAX OD 37.5", "mesosulfuron-methyl;iodosulfuron-methyl-sodium", P)
	P <- gsub("Maguguma", "atrazine", P)
	P <- gsub("Topsite", "diuron;imazapyr", P)
	P <- gsub("Roundup \\(18% conc\\)", "glyphosate", P)
	P <- gsub("Kamex", "diuron", P)
	P <- gsub("weedar 64", "2,4-D", P)
	P <- gsub("Plateau", "imazapic", P)
	P <- gsub("Maize pro", "atrazine;s-metolachlor", P)
	P <- gsub("SENCOR SC 480", "metribuzin", P)
	P <- gsub("ARSENAL", "imazapyr", P)
	P <- gsub("Envy 2,4-D", "2,4-D", P)
	P <- gsub("Escort", "metsulfuron", P)
	P[P==""| P=="NA"] <- NA
	d$herbicide_product <- trimws(P)
	
	P <- carobiner::fix_name(d$insecticide_product)
	P <- gsub("NA;|;NA", "", P)
	P[P=="NA"] <- NA
	d$insecticide_product <- tolower(P)


	d$country <- "Kenya"
	d$currency = "KES"
	d$on_farm <- FALSE
	d$is_survey <- TRUE
	d$geo_from_source <- FALSE
	d$yield_part <- "none"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA
	d$planting_date <- as.character(NA) 
	d$trial_id <- paste(d$location, d$hhid, d$intercrops, sep="-")
	d$intercrops <- NULL
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

	### drop rows with missing  value in adm1 and location, 
	d <- d[!is.na(d$location),]

	### drop the duplicate records
	d <- unique(d)

	carobiner::write_files(path, meta, d)
}


