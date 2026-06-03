# R script for "carob"
# license: GPL (>=3)

## ISSUES
### rate of NPK fertilizer type applied is missing 
## The cultivated area is extremely large compared to the weight of the harvested crop

carob_script <- function(path) {

"
Landscape diagnostic survey data of wheat production practices and yield of 2018, 2019, 2020, 2021 from Nepal's Terai

The objective of the Landscape Diagnostic Survey (LDS) for wheat is to bridge the existing data-gap around current production practices of wheat, and also to help in evidence-based planning. The LDS is designed in a way that data is collected from randomly selected farmers spread uniformly within the Feed the Future (FtF) districs. The data has been collected from farmers largest wheat plot for 2018, 2019, 2020 and 2021. The surveys for the  year 2020, and 2021 were done via phone surveys due to COVID and can be used as panel data with the hhid. Survey questionnaire captures all production practices applied by farmers from land preparation to harvesting, including detailed sections on fertilizer use, weed control and irrigation application. Data is captured through electronically enabled Open Data Kit (ODK) tool on mobile phone or tablet. The sample size for this survey in 2018 is 1577 households, in 2019 is 1207 households, in 2020 is 1028 households, and 953 households for 2021. The inputs use were asked for largest rice grown plots as farms may have multiple plots and inputs use might be different in different plots.
"


	uri <- "hdl:11529/10549036"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT; IRRI",
		publication = NA,
		project = NA,
		carob_date = "2026-06-01",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "wheat_codebook.xlsx"]
	f2 <- ff[basename(ff) == "Nepal_wheat_LDS_datasets_2018_2019_2020_2021_anonymized.csv"]
	#f3 <- ff[basename(ff) == "tablet_map_wheat.png"]

	r1 <- carobiner::read.excel(f1)
	r2 <- read.csv(f2, na="")
	
	d <- data.frame(
		country = r2$country,
		adm1 = carobiner::fix_name(r2$adm_1, "title"),
		adm2 = carobiner::fix_name(r2$adm_2, "title"),
		adm3 = carobiner::fix_name(r2$adm_3, "title"),
		farmer_gender = r2$sex,
		farmer_age = r2$age,
		farmer_education = r2$edu,
		crop_cut = grepl("yes", tolower(r2$cropcut_done)),
		cropland_total = r2$total_cultivated_land,
		plot_area = r2$total_crop_cult_area,
		plot_id = as.character(r2$surveyed_plot),
		soil_texture = r2$soil_texture,
		previous_crop = ifelse(grepl("Other", r2$previous_crop), tolower(r2$name_of_other_previous_crop), tolower(r2$previous_crop)),
		previous_crop_residue_perc = r2$prev_crop_residue_perc,
		variety = ifelse(grepl("Other", r2$variety_name), 	r2$variety_name_other, r2$variety_name),
		variety_type = r2$variety_type,
		land_prep_method = r2$tillage_method,
		planting_method = r2$ce_method,
		planting_date = gsub(", ", " ", r2$sowing_date),
		seed_rate = r2$seed_amount/r2$total_crop_cult_area,
		seed_price = r2$seed_cost,
		seed_source = ifelse(grepl("Other", r2$seed_source), 	r2$seed_source_other, r2$seed_source) ,
		OM_type = r2$organic_fert_applied,
		OM_amount = (rowSums(r2[, c("fym_applied_qty", "qty_compost", "qty_cowdung", "green_manure_amount")], na.rm = TRUE))/r2$total_crop_cult_area,
		OM_price =  rowSums(r2[, c("fym_cost", "cost_compost", "cowdung_cost")], na.rm = TRUE),
		fertilizer_used = grepl("yes", tolower(r2$apply_minfert)),
		fertilizer_type = gsub(" ", ";", r2$chem_fert_applied),
		fertilizer_price = rowSums(r2[, c("urea_price", "npk_price", "dap_price", "mop_price", "ssp_price", "tsp_price", "npks_price", "boron_price", "znso4_price", "gypsum_price")], na.rm = TRUE),
		fertilizer_amount = rowSums(r2[c("amt_urea_basal", "amt_boron_basal", "amt_gypsum_basal", "amt_mop_basal","amt_npk_basal","amt_npks_basal","amt_ssp_basal","amt_tsp_basal", "amt_znso4_basal")], na.rm = TRUE)/r2$total_crop_cult_area,
		N_fertilizer = r2$amt_urea_basal*0.46  + r2$amt_dap_basal*0.18 + ifelse(!is.na(r2$amt_npks_basal), r2$amt_npks_basal*0.08, 0)  ,
		P_fertilizer = r2$amt_dap_basal*0.201 + ifelse(!is.na(r2$amt_npks_basal), r2$amt_npks_basal*0.21, 0) + ifelse(!is.na(r2$amt_ssp_basal), r2$amt_ssp_basal*0.0874, 0)  ,
		K_fertilizer = ifelse(!is.na(r2$amt_mop_basal), r2$amt_mop_basal*0.498, 0) +ifelse(!is.na(r2$amt_npks_basal),  r2$amt_npks_basal*0.07, 0)   ,
		gypsum = r2$amt_gypsum_basal,
		S_fertilizer = ifelse(!is.na(r2$amt_npks_basal), r2$amt_npks_basal*0.04, 0) + ifelse(!is.na(r2$amt_ssp_basal), r2$amt_ssp_basal*0.12, 0),
		B_fertilizer = r2$amt_boron_basal,
		irrigated = grepl("yes", tolower(r2$irrigation_done)),
		irrigation_number = r2$irrigation_times,
		irrigation_source = ifelse(grepl("Other", r2$source_of_irrigation), r2$source_irrigation_others, r2$source_of_irrigation) ,
		weeding_done = as.logical(ifelse(!is.na(r2$weed_control), grepl("Yes", r2$weed_control), r2$weed_control)) ,
		weeding_method = r2$weed_control_method,
		herbicide_times = r2$herbicide_times_applied,
		herbicide_amount = r2$herbicide_amount,
		herbicide_product = r2$herbicide_name,
		herbicide_price = r2$herbicide_total_cost,
		insecticide_amount = r2$insecticide_amount,
		insecticide_used = grepl("yes", tolower(r2$insecticide_applied)),
		insecticide_product = r2$insecticide_name,
		fungicide_amount = r2$fungicide_amount,
		fungicide_used = grepl("Yes",r2$fungicide_applied),
		fungicide_product = r2$fungicide_name,
		fungicide_price = r2$cost_of_fungicide,
		harvest_date = gsub(", ", " ",r2$harvest_date),
		previous_crop_burnt = as.logical(ifelse(!is.na(r2$crop_residue_burnt), grepl("yes",tolower(r2$crop_residue_burnt)), r2$crop_residue_burnt)),
		latitude = r2$latitude,
		longitude = r2$longitude,
		elevation = r2$altitude,
		yield = r2$total_production_farm/r2$total_crop_cult_area,
		disease_severity = r2$disease_severity,
		crop_price = r2$current_crop_market_price/100,
		trial_id = paste(r2$UID, r2$name, sep = "-"), 
		on_farm = FALSE, 
		is_survey = TRUE, 
		crop = "wheat", 
		yield_part = "none", 
		yield_moisture = as.numeric(NA), 
		geo_from_source = TRUE,
		yield_isfresh = TRUE,
		currency = "NPR"
	)
	
	### Fixing date
	i <- grepl("^\\d{1,2}-[A-Za-z]{3}-\\d{2}$", d$harvest_date)
	d$harvest_date[i] <- as.character(as.Date(carobiner::eng_months_to_nr(d$harvest_date[i]), format = "%d-%m-%y"))
	i <- grepl("^\\d{1,2}-[A-Za-z]{3}-\\d{2}$", d$planting_date)
	d$planting_date[i] <- as.character(as.Date(carobiner::eng_months_to_nr(d$planting_date[i]), format = "%d-%m-%y"))
	
	i <- grepl("^[A-Za-z]{3} \\d{1,2} \\d{4}$", d$harvest_date)
	d$harvest_date[i] <- as.character(as.Date(carobiner::eng_months_to_nr(d$harvest_date[i]), format = "%m %d %Y"))
	
	i <- grepl("^[A-Za-z]{3} \\d{1,2} \\d{4}$", d$planting_date)
	d$planting_date[i] <- as.character(as.Date(carobiner::eng_months_to_nr(d$planting_date[i]), format = "%m %d %Y"))
	
	i <- grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", d$harvest_date)
	d$harvest_date[i] <- as.character(as.Date(d$harvest_date[i], "%d/%m/%Y"))
	
	i <- grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", d$planting_date)
	d$planting_date[i] <- as.character(as.Date(d$planting_date[i], "%d/%m/%Y"))
	
	i <- grepl("00:00", d$harvest_date)
	d$harvest_date[i] <- as.character(as.Date(d$harvest_date[i], "%d/%m/%Y %H:%M"))
	
	i <- grepl("00:00", d$planting_date)
	d$planting_date[i] <- as.character(as.Date(d$planting_date[i], "%d/%m/%Y %H:%M"))
	
	d$harvest_date[which(d$harvest_date=="2032-03-27")] <- "2021-03-27"
	
	### harvest days is more than a year (no make sens for crop wheat)
	d$harvest_date[which(as.numeric(as.Date(d$harvest_date) - as.Date(d$planting_date)) >= 366)] <- NA
	## harvest date is less than planting date 
	d$harvest_date[which(as.Date(d$planting_date) >= as.Date(d$harvest_date))] <- NA
	### two rows where harvest_days are 4 and 10 respectively 
	d$harvest_date[which(as.numeric((as.Date(d$planting_date)- as.Date(d$harvest_date))) <= 10)] <- NA
	
	### Fixing herbicide product
	d$herbicide_product <- ifelse(grepl("gold|nomimee|hold|gokd|nomine|nominogol", d$herbicide_product), "bispyribac-sodium", d$herbicide_product)
	d$herbicide_product <- ifelse(grepl("2,4 d|24D|24 D|2,4-D|2,4d|2,4 -D|2 4 D|2,4-D|2,4-D|2-4D|2 4 D|2 4  D|2-4-D|2, 4 D|2.4D|2,4,D|2,4 D|2 4D|2,4D|2 4 D", d$herbicide_product), "2,4-D", d$herbicide_product)
	unknowns <- c(
	  "adora","^1$","algo","all mix","allmix","angkush","ankush","aspirit","atlantas","bayer","bidmar","blast","bomb","caler","chief","chips",
	  "chit","cholora","clear","cobra","collin","crimjin","cross","crush","don't know","donot know name","dont know","dont know name",
	  "dont know soil treatment","dontknowname","farmer have forgotten","farmer have forgotten the name","fighter","forget","forgetten",
	  "forgot the name","forgotten","goldcyps, rugo","heera","ifinity","image","indian","isoguard classic","kaal","kaiser mix surfactants","kamaal","kesar","keshar","keshari","kingfasher","kirathin",
	  "klever","kross","liqiid","liquid","liquid type","macho","marker","milkwhat","nit known","no name","not knoen","not known",
	  "not specific","not specific name","powder","powder\\(forgotten\\)","prodetion","rim jhim","rimjhim","rinser","scrit","snatch-58",
	  "snatch 58","spider","spirit","sprad","superphosphate","suprem","surya","surya gold","suryabisham","t 40","takashi","takila",
	  "target","tata","thiaimine","tiger","tonic","^u$","^unkown$","unkown from india","unkown indian"
	)
	
	
	P <- tolower(carobiner::fix_name(trimws(d$herbicide_product)))
	P <- gsub("24d|24 d|2 4 d|2 4d|2,4- d|25d|245|w4d|2,4-D, other", "2,4-D", P)
	P <- gsub("altrazine", "atrazine", P)
	P <- gsub("glyphot|glyphosate \\(pre\\)|total killer|total","glyphosate", P)
	P <- gsub("pendimethaline|pendimathaline|pendimetalin|pednymethylin|pendimethalin 30%ec","pendimethalin",P)
	P <- gsub("tilachlor 50% ec","butachlor",P)
	P <- gsub( "refix\\( pretilachlor\\)|pretilachlor \\(refix\\)|pretilachlor \\( refix\\)","pretilachlor",P)
	P <- gsub("sulphosulfuran|sulpho-sulfuran|sulpho-sulphuran|sulfo-sulfuron|sulphosulpharan|sulphosulphurin|sulphushulphurin|sulphosulphurampn|sulphoshulphuran|sulphoshulphurun|sulphoshulphrin 24d|sulphosulpharane|sulpho sulfate|sulpho sulfuran|sulpho sulpharan|sulfofuran|sukphosukpharan|sulphosulfant|sulfosulfuran","sulfosulfuron",P)
	P <- gsub("metsulpharan|metsulpharane|metasulphuran","metsulfuron",P	)
	P <- gsub("isoproturan", "isoproturon", P)
	P <- gsub("nominee gold|nominee|nominigold|nimoni gold|knonimy gold","bispyribac-sodium",P)
	P <- gsub("^2$|^24 d$|^25d$|^2 4 d$|^2,4-d$|^w4d$", "2,4-D", P)
	P <- gsub("sulfosulfourine|sulfosulphuran|sulphosulphuran|sulfoxofuran|sulfoxofudan", "sulfosulfuron", P)
	P <- gsub("tata fateh \\(sulfosulphoron\\)", "sulfosulfuron", P)
	P <- gsub("tattafate and leader", "sulfosulfuron", P)
	P <- gsub("sulfoxpfuran \\+ 24,d", "sulfosulfuron;2,4-D", P)
	P <- gsub("sulphoshulphrin 2,4-D", "sulfosulfuron;2,4-D", P)
	P <- gsub("glyphot", "glyphosate",P)
	P <- gsub("total killer", "glyphosate", P)
	P <- gsub("pretilachlor 50%ec", "pretilachlor", P)
	P <- gsub("pretilachlor \\( refix\\)", "pretilachlor", P)
	P <- gsub("pretilachlor \\(refix\\)", "pretilachlor", P)
	P <- gsub("refix\\( pretilachlor\\)", "pretilachlor", P)
	P <- gsub("tilachlor 50% ec", "pretilachlor", P)
	P <- gsub("pednymethylin|pendimathaline|pendimethaline|pendimetalin", "pendimethalin", P)
	P <- gsub("pendimethalin 30%ec", "pendimethalin", P)
	P <- gsub("nominee gold", "bispyribac-sodium", P)
	P <- gsub("nominee", "bispyribac-sodium", P)
	P <- gsub("nimoni gold", "bispyribac-sodium", P)
	P <- gsub("knonimy gold", "bispyribac-sodium", P)
	P <- gsub("altrazine", "atrazine", P)
	P <- gsub("sulfosulfuron \\(leader\\)|sulfosulfuron \\(lehar\\)|sulfosulfuron \\(brentis\\)", "sulfosulfuron", P)
	P <- gsub("^leader$|^lehar$|^brentis$", "sulfosulfuron", P)
	P <- gsub("sulfosulfuron, 2,4-D|2,4-D, sulfosulfuron", "sulfosulfuron;2,4-D", P)
	P <- gsub(paste(unknowns, collapse = "|"), "unknown", P)
	P <- gsub("bispyribac-sodium \\+ unknown|bispyribac-sodium unknown", "bispyribac-sodium", P)
	P[P=="2,4-D, other"] <- "2,4-D"
	d$herbicide_product <- P
	
	### Fixing insecticide product
	# Unknowns
	unknowns <- c(
	  "anti aphid insecticide","astra","astra 4","^bullet$","bullet 440",
	  "current","don't know","dont know","dont know name",
	  "farsha","fauji","^forgot$","forgotten","katar","ketop","kitok",
	  "lara 909","magic","nomanigold","not known",
	  "not known \\(got in grant\\)","rhino","selcon","silkon",
	  "superkiller","^tiger$","tiger 505"
	)
	
	P <- tolower(carobiner::fix_name(trimws(d$insecticide_product)))
	P <- gsub("amida chlorobide|amidachlorophide|amidachoropride|imida","imidacloprid", P)
	P <- gsub("^chloropyriphos$", "chlorpyrifos", P)
	P <- gsub("chloropyriphos cypermethrin","chlorpyrifos + cypermethrin", P)
	P <- gsub("^cyper$", "cypermethrin", P)
	P <- gsub("cypermetgrin|cypermethrine","cypermethrin", P)
	P <- gsub("thimade|thymade|thymate|thymax","phorate", P)
	P <- gsub("roger|roget|rogor","dimethoate", P)
	P <- gsub("feradon","carbofuran", P)
	P <- gsub(paste(unknowns, collapse = "|"), "unknown", P)
	P <- gsub("chlorpyrifos \\+ cypermethrin", "chlorpyrifos;cypermethrin", P)
	P <- gsub("furadan \\(carbofuran\\)", "carbofuran", P)
	P <- gsub("imidaclopridcloprid", "unknown", P)
	P <- gsub("metacil", "lambda-cyhalothrin", P)
	P <- gsub("metasystox", "unknown", P)
	P <- gsub("monocil", "monocrotophos", P)
	P <- gsub("thiram", NA, P)## fungicide
	d$insecticide_product <- P
	d$fungicide_product <- gsub("Dont know|Don't know|Ttt","unknown", d$fungicide_product)
	d$fungicide_product <- gsub("Thionate","thiophanate-methyl", d$fungicide_product)
	
	
	
	### Fixing fertilizer type
	d$fertilizer_type <- gsub("Urea", "urea", d$fertilizer_type)
	d$fertilizer_type <- gsub("MOP", "KCl", d$fertilizer_type)
	d$fertilizer_type <- gsub("Gypsum", "gypsum", d$fertilizer_type)
	d$fertilizer_type <- gsub("Boron", "borax", d$fertilizer_type)
	
	#### OM_type 
	P <- carobiner::fix_name(d$OM_type)
	P <- gsub("^Farm_Yard_Manure$|Farm_Yard_Manure Manure|Manure Farm_Yard_Manure|Manure", "farmyard manure", P)
	P <- gsub("^Cowdung Manure$|^Cowdung$|^Manure Cowdung$", "animal dung", P)
	P <- gsub("Vermicompost", "vermicompost", P)
	P <- gsub("Green_manure", "green manure", P)
	P <- gsub("Cowdung Farm_Yard_Manure", "farmyard manure", P)
	P <- gsub("Compost", "compost", P)
	P <- gsub("farmyard manure Cowdung|Cowdung farmyard manure|Cowdung Farm_Yard_farmyard manure", "farmyard manure", P)
	d$OM_type <- P
	
	### Fixing planting method
	
	P <- carobiner::fix_name(d$planting_method)
	P <- gsub("Broadcasting", "broadcasting", P)
	P <- gsub("SurfaceSeeding", "direct seeding", P)
	P <- gsub("Zero tillage|Minimum Tillage|ZeroTillage", "none", P)
	P <- gsub("Line sowing after tillage|LineSowingAfterTillage", "line sowing", P)
	d$planting_method <- P
	
	### Fixing land_prep_method 
	d$land_prep_method = ifelse(grepl("rotavator", d$land_prep_method), "rotovating", d$land_prep_method)
	d$land_prep_method = ifelse(grepl("plough", d$land_prep_method), "ploughing", d$land_prep_method)
	P <- carobiner::fix_name(d$land_prep_method)
	P <- gsub("No_tillage", "none", P)
	P <- gsub("^Tyne_cultivator$|^Disc_harrow$|Disc_harrow Tyne_cultivator|Tyne_cultivator Disc_harrow", "harrowing", P)
	d$land_prep_method <- P
	
	### fixing crops name
	
	P <- carobiner::fix_name(d$previous_crop)
	P <- gsub("fallow","none", P)
	P <- gsub("^grass$|jai grass","unknown", P)
	P <- gsub("groundnut \\+ urad","groundnut", P)
	P <- gsub("indianmustard","mustard", P)
	P <- gsub("jai grass","", P)
	P <- gsub("masuro","lentil", P)
	P <- gsub("mungbean","mung bean", P)
	P <- gsub("pigeonpea","pigeon pea", P)
	P <- gsub("vegetables","vegetable", P)
	d$previous_crop <- P
	
	### Fixing soil texture
	P <- carobiner::fix_name(d$soil_texture)
	P <- gsub("Medium/ Loamy","medium", P)
	P <- gsub("Light/ Sandy","sand", P)
	P <- gsub("Heavy/ Clayey","clay", P)
	P <- gsub("Heavy","clay", P)
	P <- gsub("Light","sand", P)
	P <- gsub("Medium","medium", P)
	d$soil_texture <- P
	
	
	### Fixing lon and lat coordinate
	
	geo <- data.frame(
	  adm3 = c("Adarsha Kotwal", "Gulariya","Gauriganga", "Ghodaghodi", "Kailari", "Lamkichuha", "Khajura",  "Subarna","Suwarna","Barbardiya", "Rajapur", "Geruwa","Bhimdutta","Bhimdatta","Belauri", "Shuklaphanta","Lamahi","Gadhawa", "Baijanath-5", "Duduwa-6","Pheta","Punarbas", "Krishnapur"  ,"Adarshkotwal","Duduwa", "Janaki","Joshipur","Badhaiyatal","Bardghat", "Bhajani", "Gaidhawa", "Hanumannagar Kankalini", "Kapilvastu", "Kotahimai", "Madhuwan", "Madhuwangeruwa", "Mayadevi", "Pratappur", "Ramgram", "Rohini", "Sarawal", "Shuddhodhan", "Yasodhara", NA),
	  long = c(85.155, 81.3324,  80.765, 80.962, 80.786,  81.1498, 81.533,  84.712, 85.043, 81.352, 81.130, 81.246, 80.161, 80.178, 80.358, 80.378, 82.570,  82.533, 81.611, 81.643, 84.947, 80.473, 80.464, 85.1477, 81.707, 81.090, 81.0123, 81.491, 83.7954, 80.980, 83.2761, 86.880, 83.0459, 83.352, 83.301,  83.301, 83.4093,  81.079, 83.664,  83.526, 83.7290, 83.373, 83.489,  81.392),
	  lat = c(26.939, 28.2279, 28.7451, 28.634, 28.606, 28.643, 28.125, 27.235, 26.926, 28.376, 28.421, 28.524, 28.984, 28.984, 28.692, 28.937, 27.881, 27.761, 28.207, 28.008, 27.0052, 28.656, 28.912, 26.963, 28.046, 28.587, 28.572, 28.192, 27.541, 28.497, 27.587, 26.5139, 27.548, 27.441, 27.479, 27.482, 27.6873, 28.608, 27.534, 27.507, 27.502, 27.615, 27.5101, 28.413),
	  geo_from = FALSE
	)
	
	d <- merge(d, geo, by="adm3", all.x = TRUE)
	d$longitude[!is.na(d$long)] <- d$long[!is.na(d$long)]
	d$latitude[!is.na(d$lat)] <- d$lat[!is.na(d$lat)]
	d$geo_from_source[!is.na(d$geo_from)] <- d$geo_from[!is.na(d$geo_from)]
	
	d$long <-  d$lat <-  d$geo_from <-  NULL
	
	
	carobiner::write_files(path, meta, d)
}

