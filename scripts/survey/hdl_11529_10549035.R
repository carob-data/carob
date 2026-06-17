# R script for "carob"
# license: GPL (>=3)

## ISSUES
### The cultivated area is extremely large compared to the weight of the harvested crop.


carob_script <- function(path) {

"
Landscape diagnostic survey data of rice production practices and yield of 2019 and 2020 from Nepal's Terai

The objective of the Landscape Diagnostic Survey (LDS) for rice is to bridge the existing data-gap around current production practices of rice, and also to help in evidence-based planning. The LDS is designed in a way that data is collected from randomly selected farmers spread uniformly within the Feed the Future (FtF) districs. Data has been collected from farmers largest rice plot for 2019, 2020 main (kharif) season . Survey questionnaire captures all production practices applied by farmers from land preparation to harvesting, including detailed sections on fertilizer use, weed control and irrigation application. Data is captured through electronically enabled Open Data Kit (ODK) tool on mobile phone or tablet.

The sample size for this survey in 2019 is 2917 households, and 2122 households for 2020. The inputs use were asked for largest rice grown plots as farms may have multiple plots and inputs use might be different in different plots.
"

  uri <- "hdl:11529/10549035"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "CIMMYT; IRRI",
		publication = NA,
		project = NA,
		carob_date = "2026-05-31",
		carob_effort = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		carob_completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "rice_codebook.xlsx"]
	f2 <- ff[basename(ff) == "Nepal Rice LDS dataset (2019,2020) anonymized.csv"]
	#f3 <- ff[basename(ff) == "summary_stats.html"]
	#f4 <- ff[basename(ff) == "summary_stats.Rmd"]
	#f5 <- ff[basename(ff) == "tablet_map.png"]

	r1 <- carobiner::read.excel(f1)
	r2 <- read.csv(f2, na= "")
	


	d <- data.frame(
	  hhid = as.character(r2$UID),
		adm1 = carobiner::fix_name(r2$adm_1, "title"),
		adm2 = carobiner::fix_name(r2$adm_2, "title"),
		adm3 = carobiner::fix_name(r2$adm_3, "title"),
		farmer_gender = r2$sex,
		farmer_age = r2$age,
		season = r2$survey_season,
		crop_cut = grepl("yes", r2$cropcut_done),
		farmer_education = r2$edu,
		farmland = r2$total_landholding,
		cropland_total = r2$total_cultivated_land,
		plot_area = r2$total_crop_cult_area,
		#r2$surveyed_plot,
		previous_crop = ifelse(is.na(r2$previous_crop), r2$name_of_other_previous_crop, r2$previous_crop),
		#r2$residue_retained,
		soil_texture = tolower(r2$soil_texture),
		variety = ifelse(is.na(r2$variety_name), 	r2$variety_name_other, r2$variety_name),
		variety_type = r2$variety_type,
		land_prep_method =gsub("conventional tillage", "conventional", tolower(r2$tillage_method)),
		planting_method = r2$ce_method,
		planting_date = as.character(as.Date(ifelse(is.na(r2$planting_date_dsr), r2$date_nursery_estd , r2$planting_date_dsr), "%d/%m/%Y")) ,
		transplanting_days = r2$transplanting_day,
		seed_rate = r2$seed_amount/r2$total_crop_cult_area,
		seed_price = r2$seed_cost,
		seed_source = r2$seed_source,
		seed_treatment = r2$seed_treatment,
		OM_type = tolower(r2$organic_fert_applied),
		OM_amount = rowSums(r2[, c("fym_applied_qty", "qty_compost")], na.rm = TRUE)/(r2$total_crop_cult_area),
		OM_price = rowSums(r2[, c("fym_cost", "cost_compost")], na.rm = TRUE),
		lime = r2$qty_lime,
		fertilizer_used = grepl("yes", r2$apply_minfert),
		fertilizer_type = gsub(" ", ";", r2$chem_fert_applied),
		fertilizer_price = rowSums(r2[, c("urea_price", "dap_price", "mop_price", "znso4_price")]),
		fertilizer_amount = (rowSums(r2[, c("cost_lime","total_urea_applied", "total_dap_applied", "total_mop_applied", "total_znso4_applied")]))/(r2$total_crop_cult_area),
		irrigated = grepl("yes",r2$irrigation_done),
		irrigation_number = r2$irrigation_times,
		weeding_method = r2$weed_control_method,
		herbicide_times = r2$herbicide_times_applied,
		herbicide_product = tolower(ifelse(is.na(r2$herbicide_name_1) & !is.na(r2$herbicide_name_2), r2$herbicide_name_2,
		                           ifelse(is.na(r2$herbicide_name_1) & !is.na(r2$herbicide_name_3), r2$herbicide_name_3, r2$herbicide_name_1))),
		herbicide_price = r2$herbicide_total_cost,
		insecticide_used = grepl("yes", r2$insecticide_applied),
		insecticide_price = r2$insecticide_cost,
	  insecticide_times = 	r2$insecticide_times_applied,
		insecticide_product = tolower(r2$insecticide_name_1),
		fungicide_used = grepl("yes",r2$fungicide_applied),
		fungicide_product =  tolower(ifelse(is.na(r2$fungicide_name_1),r2$fungicide_name_2, r2$fungicide_name_1)),
		fungicide_times = r2$fungicide_times_applied,
		previous_crop_burnt = as.logical(ifelse(!is.na(r2$crop_residue_burnt), grepl("yes", r2$crop_residue_burnt), r2$crop_residue_burnt)) ,
		latitude = r2$latitude,
		longitude = r2$longitude,
		fungicide_price = r2$cost_of_fungicide,
		harvest_date = as.character(as.Date(r2$harvest_date, "%d/%m/%Y")),
		yield = r2$total_production_farm/(r2$total_crop_cult_area),
		#yield = r2$total_rice_area_yield,
		Zn_fertilizer = r2$znso4_kg_ha,
		N_fertilizer = r2$Nitrogen_kg_ha,
		P_fertilizer = r2$Phosphorus_kg_ha,
		K_fertilizer = r2$Potassium_kg_ha,
		country = "Nepal",
		crop = "rice",
		trial_id = paste(r2$UID, r2$adm_1), 
		on_farm = FALSE, 
		is_survey = TRUE, 
		yield_part = "grain", 
		yield_moisture = as.numeric(NA), 
		geo_from_source = TRUE,
		yield_isfresh = TRUE
	)
	
	### Fixing fertilizer type
	
	d$fertilizer_type <- gsub("Urea", "urea", d$fertilizer_type)
	d$fertilizer_type <- gsub("Others", "unknown", d$fertilizer_type)
	d$fertilizer_type <- gsub("Zinc", "ZnSO4", d$fertilizer_type)
	d$fertilizer_type <- gsub("Potash", "KCl", d$fertilizer_type)
	
	### Fixing herbicide product
	d$herbicide_product <- ifelse(grepl("gold|nomimee|hold|gokd|nomine|nominogol", d$herbicide_product), "bispyribac-sodium", d$herbicide_product)
	d$herbicide_product <- ifelse(grepl("2,4-d|24d|24 d|2 ,4 -d|2,4 d|2 ,4 -d|2,4-d|2,4d|2 4 d|2 4d|2,4- d|2,4 - d|24, d|2,4-d|2 4  d| 2|2-4,d|2-4d|23d|24", d$herbicide_product), "2,4-D", d$herbicide_product)
	d$herbicide_product <- ifelse(grepl("pendameth|pendemith|pendimeth|pendemethy", d$herbicide_product), "pendimethalin", d$herbicide_product)

	P <- carobiner::fix_name(d$herbicide_product)
	P <- gsub("not known|unknown|unknosn|unknow|unknowin|unkown|unknwon|unknowm|unlnown|unknowb|umknown|don't know|dont know|don't known|don't no|dont known|do not know|donot know|rft|ddndnfn|aaaaa|ggggg|\\?+", "unknown", P)
	P <- gsub("killer|hunter|round up|hunder", "glyphosate", P)
	P <- gsub("^paraquat$", "paraquat dichloride", P)
	P <- gsub("butachor|butac$|buta$|butahlor|butacholor|butacholr|betachlor|betqchlor|butqchlor|butachl9r|butachlot|butachlor50|butachlore|butaclor|butaclore|butacolor|butacolore|butacoloue|pbutachlor|anuchlor|anup|dhanucolor|suryachlor|agni|agnipath|machete|macheti|machheti|maechete|mechete|mecheti|mechit|metiche|machaiti","butachlor",P	)
	P <- gsub("racer.*|razor|saffire|alfit|allfit|hifit.*|preetilachlor|preetichlor|pretichlor|preticlor|pretichloelr|protichlor|protilachlor|petri|petriclor|petrichlor|petrilachlor|petrilaclor|petilaclor|pectichlor|pectilachlor|pectilaclor|pertilachlor|craze.*|dawn.*|shivalik.*","pretilachlor", P	)
	P <- gsub("atrazin|atrzine|atrazine and twister", "atrazine", P)
	P <- gsub("pendamithalin", "pendimethalin", P)
	P <- gsub("king clean.*", "simetryn", P)
	P <- gsub("thimet|thimate", "phorate", P)
	P <- gsub("furadon", "carbofuran", P)
	P <- gsub("^2$|aunknown|don't|don't kno|dont|dont 3|unknown- khar|unknown \\+ vitamin|unknwo|delet|delete|particular|^un$","unknown", P)
	P <- gsub("atrazinee", "atrazine", P)
	P <- gsub("buta hlor|butachlor \\(butachlor\\)|butachlor \\+dap|butachlor%|butachlor,dhanachlor|bytavhkor|paitalcholor","butachlor", P)
	P <- gsub("preti|pretilachlor 50% ec|pretilachlor 50%ec \\(pretilachlor\\)","pretilachlor",P)
	P <- gsub("oryzostar, bispyribac sodium","bispyribac-sodium",P)
	P <- gsub("adora", "bispyribac-sodium", P)
	P <- gsub("basalin|besalin", "fluchloralin", P)
	P <- gsub("glavoside|dolphine", "glyphosate", P)
	P <- gsub("hactor|hectar", "nicosulfuron", P)
	P <- gsub("heptachlor", NA, P) ## insecticide
	P <- gsub("hira|heera", "2,4-D, 2-ethylhexyl ester", P)
	P <- gsub("lurma|kaal", "2,4-D, diethanolamine salt", P)
	P <- gsub("niko", "nicosulfuron", P)
	P <- gsub("rager", "carfentrazone-ethyl", P)
	P <- gsub("ram band", "picloram", P)
	P <- gsub("agent|anu poison|chips|green label|greenlevel|hirs|india|jorak|lungra|machette from panjab|multiplex|nora|pad|priyanka|weed mar|weedmar|weed lethal|trisul chhap|suuth|roger|rocket|rezer", "unknown", P)
	P <- gsub("taghit", "pretilachlor", P)
	P <- gsub("tarzan", "atrazine", P)
	P <- gsub("pretilachlorlachlor", "pretilachlor", P)
	d$herbicide_product <- trimws(P)
	
	### Fixing insecticide products
	P <- carobiner::fix_name(d$insecticide_product)
	P <- gsub("not known|don't know|dont know|don know|don't  know|dont know for gabaro|dont know \\+ vitamin|don't no|unk|unknown|unkown|unknowm|unknowwn|unkniwn|unkbown|unknosn|unknoen|ukown|rtff|xxx|aaaaaaa|cccc|ccccc|yu|farsaaa","unknown", P)
	P <- gsub("chloropyriphos|chloripyriphos|chloropyrifos|chloropyriphous|chloro pyrifos|chloro|cholripyriphos|choripyriphos|choloropyriphos", "chlorpyrifos", P)
	P <- gsub("cypermethirin|cypermethene|cypermethin|cypermethrim|cypermethrine|cyper methrin|cypemethrin|alphamethrin|alphamethrine","cypermethrin", P)
	P <- gsub("chloropyriphos 50% \\+cypermethrin 5%|chloropyriphos\\+ cypermethrin|chloropyriphos 50%\\+ cypermethrin 5%|chloropyriphos 50ec 50%\\+ cypermethrin 5%|cyber methrin chloro pyrifos|cypermethrin and chloropyriphus|chloropyriphous and cypermethrin|chloropyrifos 50% \\+ cypermethin 5% ec|surya neural|surya neurol|surya neurel|ki-top|keetop|ketop|kitop|kit up|kit  up|kitup|kitoph|kitopth|kicyper|ki cyper","chlorpyrifos;cypermethrin", P)
	P <- gsub("imidachloprid|imidachloropid|imidachorpid|imidachlorpid","imidacloprid", P	)
	P <- gsub( "cartap hydrochloride 4% gr|cartap hydrochloride|cartap|kartap|kartop|kirtap","cartab", P)
	P <- gsub("thimet|thimate|thiamate|thymate|thyamte|thimet z|phorate","phorate", P)
	P <- gsub("furadon|furadan|firadon|feradon|ferrodon|ferradon|feuraden|frradon|phridon|pheradone","carbofuran", P)
	P <- gsub("rogor|roger|rogen|rogar|tafgor dimethoate","dimethoate", P)
	P <- gsub("thiodan|thiodon","endosulfan", P)
	P <- gsub( "enrin|indrin|indirin", "dimethoate", P)
	P <- gsub("lambdacyhalothrin|lambda cyhalothrin 5%ec","lambda-cyhalothrin", P)
	P <- gsub( "flash \\(quinalphos 25% ec\\)|flash", "quinalphos", P)
	P <- gsub("corajen|corazin|quorazin|foragen","emamectin benzoate",P)
	P <- gsub("thiamethoxam a.i. 25%","thiamethoxam",P)
	P <- gsub("profenofos \\+ cypermethrin|profex super|super profax","profenofos;cypermethrin",P)
	P <- gsub("karate|tiger|tiger-50|tiger505|tiger 505|rhino505|rhino 505|allfighter|alfighter|fighter|commando|blaster|bullet|katar","cypermethrin", P	)
	P <- gsub("organic jhol|cow urine", "unknwon", P	)
	P[grepl("cypermethrin \\@cypermethrin|cypermethrin / cholopryiphus|chlorpyrifos.*cypermethrin|cypermethrin.*chlorpyrifos|cobra|surya|cyber methrin",P)] <- "chlorpyrifos;cypermethrin"
	P[grepl("cypermethrin\\(cypermethrin\\)|cyper 10|cypermethrin 10|cypermethrin 25|dollar cypermethrin|super cypermethrin|hitler",P)] <- "cypermethrin"
	P[grepl("^4g$|hydrochloride 4g|cartap hydrochloride|kitap|kitok|kitnasak|kritab|trishul",P)] <- "cartab"
	P[grepl("hunter|imatin|imidachlor",P)] <- "imidacloprid"
	P[grepl("kala sona|kalasona|hara sona|harasona|harya sona|killer|killdan|killdon|master killer|super killer|superkiller",P)] <- "carbofuran"
	P[grepl("metasystox|metacid|metacide|metacil|metacin|metazed|madasid|medasid",P)] <- "oxydemeton-methyl"
	P[grepl("nuvan|noorant|nurani",P)] <- "monocrotophos"
	P <- gsub("endocell|endrin", "dimethoate", P)
	P <- gsub("thiram",NA, P) ## fungicide
	P <- gsub("ampligo", "chlorantraniliprole;lambda-cyhalothrin",P )
	P[P %in% c("nominee gold","nomine gold","nomnigold")] <- NA # herbicide
	P[P == "butachlor"] <- NA ## herbicide
	P <- gsub("gammaxine|gammaxone", "lindane", P)
	## perhaps these are the local names of insecticide
	P <- gsub("505|55%nagraj|555|alkora|allmicro|androxyl|anth 505|anth505|anumayat|anuwhite|barud|bio z plus|borer ko|carbail|cobrq|current|dollar|doller|etno|farisdal|farsa|feratex|forca|forret|g-sunami|g sunami|gabaro|gsubami|gsunami|hilter|karmain|khanjar|killesker|king clear|kingclear|kingstar|kriship|lara|lethal|marshall|max power|meeraquaran|metaxy|multi plus|multipluz|naag|naag 505|naag 555|naag raj|naagraj|nag plush|nag555|nagmani|nagplush|nagrag|nagraj|nagraj \\(safex\\)|nagraj 50|nagraj 50%|nagraj 55|nagraj 55%|perfect super|pharsa|profenofos|rager|razor|safax|sap|satisfy|setisfy|sonapayal|spot|sticker, 500 bn|stop|sunami|super 505|super clear|super d|super ki ller|super perfect|superclear|superkiler|superkillar|superkillef|suposh d|theron|unknown - liquid \\+ powder|unknwon","unknown", P)
	P[P=="unknownl"] <- "unknown"
	d$insecticide_product <- P
	
	### Fixing fungicide 
	
	P <- carobiner::fix_name(d$fungicide_product)
	P <- gsub("bavistin|bebistin|bevistin|bevistine|karbendazim","carbendazim", P)
	P <- gsub("diathane|diathane-45|dm 45|mencozeb|menkozeb","mancozeb", P)
	P <- gsub("hexaconazol|hexaconazole 5% sc|hexaconazole 5%ec|hexagonazole","hexaconazole", P)
	P <- gsub("kasugamaisin|kasugamysin|kasukamycin|kasukamycine","kasugamycin", P)
	P <- gsub("hexaconazolee", "hexaconazole", P)
	P <- gsub("don't know|dont know|don't  know|n/a|saaff|saf|saff|shitmar|truf|tuflex|xxx|hegjha|hexja|hexjachlonajol|hexacholonajol","unknown", P)
	P[P =="mancozeb carbedazium"|P=="mancozeb, csrbanazim"] <- "mancozeb;carbendazim"
	d$fungicide_product <- P
	
	### previous crop
	d$previous_crop <- gsub("fallow", "none", d$previous_crop)
	d$previous_crop <- gsub("Others", "unknown", d$previous_crop)
	## soil
	d$soil_texture <- gsub("heavy", "clay", d$soil_texture)
	d$soil_texture <- gsub("light", "sand", d$soil_texture)
	
	### Fixing planting method
	
	P <- carobiner::fix_name(d$planting_method)
	P <- gsub("Random transplanted", "transplanted", P)
	P <- gsub("DSR broadcasted", "broadcasting", P)
	P <- gsub("Machine transplanted puddled", "mechanized", P)
	P <- gsub("Manual line transplanted", "transplanted", P)
	P <- gsub("dsr_line_sowing", "line sowing", P)
	d$planting_method <- P
### OM type 
	P <- carobiner::fix_name(d$OM_type)
	P <- gsub("compost fym|fym compost", "compost", P)
	P <- gsub("compost market_available_organic_manure", "compost", P)
	P <- gsub("^fym$", "farmyard manure", P)
	P <- gsub("fym market_available_organic_manure", "farmyard manure", P)
	P <- gsub("market_available_organic_manure", "farmyard manure", P)
	P <- gsub("not_applied", "none", P)
	d$OM_type <- P
	
	d$transplanting_days[ which(d$transplanting_days==3500)] <- 35
	### 
	d$latitude[grepl("Duduwa", d$adm3)] <- 28.01 
	d$longitude[grepl("Duduwa", d$adm3)] <- 81.67
	
	
	carobiner::write_files(path, meta, d)
}

