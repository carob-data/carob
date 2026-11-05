# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
## The location is mixed up in d2 and confusing. Some places were referred as adm2 yet were supposed to be in adm3
# Some plot records (VBAA names) have no match in the technology evaluation file; those have only the
#   district and are georeferenced at the district level with a large geo_uncertainty.
# plant_height ranges to 160 cm; high but possible for climbing beans. Left as recorded.
# The village Kikombwe and its ward (Lyamugugwe) and the ward Hasanga (Ichenjezya) and the ward
#   Bonde la Usangu (Lusungo) were not found in GeoNames or GADM; coarser fallbacks are used.


carob_script <- function(path) {

"
Technology and bean performance assessment

These datasets contained technology and plant performance assessment for mother demos established in 2017 in Southern highlands of Tanzania. The assessment was done before grain harvest, so farmers had a good experience on the performance of each technology tested and so the researchers from bean department of TARI Uyole.
"
	uri <- "doi:10.7910/DVN/DVHHTU"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIAT; KSU",
		publication = NA,
		project = NA,
		carob_date = "2026-09-17",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "fertilizer_used;variety",
		response_vars = "yield;germination_rate;plant_height", 
		carob_contributor = "Illiana Kwenda",
		carob_completion =90,	
		carob_effort = 3
	)
	
	
	f1 <- ff[basename(ff) == "Mother_Demos_ Bean_Assessment in 2017.xlsx"]
	f2 <- ff[basename(ff) == "Technology evaluation for the VBAAs in 2017.xlsx"]

	r1 <- carobiner::read.excel(f1, skip = 5, na = c("IT", "SG"))
	r2 <- carobiner::read.excel(f2, skip = 3)[-1, ]

	d1 <- data.frame(
	  adm2 = r1$District,
	  plot_id = as.character(r1$`Plot number`),
	  #p_asp = r1$Vig.,
	  plant_height = as.numeric(r1$PH.),
	  treatment = r1$Treatment,
	  variety = r1$Variety,
	  VB = r1$`VBAA Name`,
	  germination_rate = as.numeric(r1$Gc.),
	  yield = NA, ## crop assessment was done before harvest as described in the data discription
	  crop = "common bean" ## not specified whether its common bean or what type of bean is that
	)
	
	d1$plant_height[d1$plant_height == 0] <- NA
	d1$variety[d1$variety %in% c("V1= Njano Uyole")] <- "Njano Uyole"
	d1$variety[d1$variety %in% c("V1= Had not planted")] <- NA
	d1$variety[d1$variety %in% c("V2= Uyole 96")] <- "Uyole 96"
	d1$variety[d1$variety %in% c("V3= Wanja")] <- "Wanja"
	d1$variety[d1$variety %in% c("V3= Calima Uyole")] <- "Calima Uyole"
	d1$variety[d1$variety %in% c("V3= Uyole 03")] <- "Uyole 03"
	
	fv <- grepl("^Farmer Variety", d1$variety)
	
	farmer_names <- d1$variety[fv]
	start <- regexpr("(", farmer_names, fixed = TRUE) + 1
	end <- regexpr(")", farmer_names, fixed = TRUE) - 1
	d1$variety[fv] <- substring(farmer_names, start, end)
	
	d1$fertilizer_used <- d1$treatment %in% c("Fertilizer", "Apron Star +Fert")
	

	
	d2 <- data.frame(
	   adm2 = r2$Distrct,
	   adm3 = r2$Ward,
	   location = r2$Village,
	   VB = r2$`VBAA Name`,
	   #fertilizer_used = r2$`Fertilizer Source`, this variable only once source apron star
	   #cropping_system = r2$`Technology Selected`, this is similar to fertilizer used
	   farmer_gender = r2$Gender,
	   field_size = as.numeric(r2$`Farm Size (m2)`)
	 )
	
	 
	d2$adm3[d2$adm3 == "Matambula"] <- "Mtambula"
	d2$adm3[d2$adm3 == "Nyororo"] <- "Nyololo"
	d2$adm3[d2$adm3 == "litisha"] <- "Litisha"
	
	d <- merge(d1, d2, by = c("VB", "adm2"), all.x = TRUE)
	d <- unique(d)
	#d$VB <- NULL
	
	#### georeferencing, by village (location) within district (adm2).
	# Village coordinates are from GeoNames (geo_uncertainty 3000 m); where the village could not be
	# found, the centroid of the ward (adm3) given in the source is used (GADM 4.1 or the GeoNames
	# ADM3 record; geo_uncertainty from GADM or 10000 m); as a last resort the district centroid.
	# Derived offline from the GeoNames TZ dump (download.geonames.org/export/dump/TZ.zip) and GADM,
	# matching on the district/ward given in the source files.
	geo <- data.frame(rbind(
		# adm2, location, lon, lat, uncertainty, source
		c("Iringa", "Magulilwa", 35.7381, -7.9701, 3000, "GeoNames, village"),
		c("Iringa", "Ng'enza", 35.6785, -7.9618, 3000, "GeoNames, village"),
		c("Iringa", "Kikombwe", 35.5693, -8.0514, 13908, "GADM 4.1, adm3 Mgama"),  # village and its ward (Lyamugugwe) not found; placed in neighbouring Mgama as in the source v1
		c("Iringa", "Ihemi", 35.4868, -8.0163, 3000, "GeoNames, village"),
		c("Iringa", "Mseke", 35.5635, -7.9453, 3000, "GeoNames, village"),
		c("Iringa", "Tanangozi", 35.5833, -7.9167, 3000, "GeoNames, village"),  # GeoNames PPL; the earlier -8.9487 was a typo
		c("Iringa", "Wenda", 35.5951, -7.9094, 3000, "GeoNames, village"),
		c("Iringa", "Mangalali", 35.5678, -7.7735, 3000, "GeoNames, village"),
		c("Iringa", "Uhominyi", 35.8091, -7.5175, 3000, "GeoNames, village"),
		c("Iringa", "Mkungugu", 35.7765, -7.5377, 3000, "GeoNames, village"),
		c("Iringa", "Kalenga", 35.6031, -7.8013, 3000, "GeoNames, village"),
		c("Iringa", "Itamba", 35.6800, -7.7581, 3000, "GeoNames, village"),
		c("Mufindi", "Nyakipambo", 35.0091, -8.7068, 10633, "GADM 4.1, adm3 Mtambula"),
		c("Mufindi", "Lwing'ulo", 34.9463, -8.4550, 18495, "GADM 4.1, adm3 Nyololo"),
		c("Mufindi", "Ikangamwane", 34.8079, -8.6484, 12047, "GADM 4.1, adm3 Idunda"),
		c("Mufindi", "Isalavanu", 35.2579, -8.2021, 12695, "GADM 4.1, adm3 Isalavanu"),
		c("Mufindi", "Njojo", 34.9463, -8.4550, 18495, "GADM 4.1, adm3 Nyololo"),
		c("Makambako", "Igomba", 34.7645, -8.7293, 10000, "GeoNames, adm3 Saja"),
		c("Makambako", "Litundu", 34.6665, -8.8360, 17014, "GADM 4.1, adm3 Wanging'ombe"),
		c("Makambako", "Uhambule", 34.7007, -8.8794, 10000, "GeoNames, adm3 Uhambule"),
		c("Makambako", "Ikingula", 34.6665, -8.8360, 17014, "GADM 4.1, adm3 Wanging'ombe"),
		c("Njombe", "Saja", 34.7667, -8.6833, 3000, "GeoNames, village"),
		c("Njombe", "Utiga", 34.6665, -8.8360, 17014, "GADM 4.1, adm3 Wanging'ombe"),
		c("Njombe", "Mawende", 34.7848, -8.9666, 10000, "GeoNames, adm3 Utengule"),
		c("Njombe", "Mawindi", 34.7413, -9.1900, 15206, "GADM 4.1, adm3 Igima"),
		c("Njombe", "Itulahumba", 34.7480, -9.0860, 3000, "GeoNames, village"),
		c("Njombe", "Masaulwa", 34.5990, -9.1300, 3000, "GeoNames, village"),
		c("Njombe", "Wanging'ombe", 34.6665, -8.8360, 17014, "GADM 4.1, adm3 Wanging'ombe"),
		c("Songea", "Nguvu Moja", 35.4693, -10.6101, 8122, "GADM 4.1, adm3 Peramiho"),
		c("Songea", "Morogoro A", 35.2952, -10.6271, 16705, "GADM 4.1, adm3 Litisha"),
		c("Songea", "Peramiho (A)", 35.4500, -10.6500, 3000, "GeoNames, village"),
		c("Songea", "Litisha", 35.3833, -10.6500, 3000, "GeoNames, village"),
		c("Songea", "Likalangilo", 35.6689, -10.3974, 11982, "GADM 4.1, adm3 Mtyangimbole"),
		c("Songea", "Mkongotema", 35.5385, -10.0666, 45073, "GADM 4.1, adm3 Mkongotema"),
		c("Songea", "Lutukira", 35.5385, -10.0666, 45073, "GADM 4.1, adm3 Mkongotema"),
		c("Songea", "Ndelenyuma", 35.5000, -10.0667, 3000, "GeoNames, village"),
		c("Mbozi", "Lumbila", 33.1128, -8.9829, 10000, "GeoNames, adm3 Ruanda (Mbozi)"),
		c("Mbozi", "Ipyana", 33.1756, -9.0961, 15261, "GADM 4.1, adm3 Iyula"),
		c("Mbozi", "Lungwa", 32.9779, -8.8192, 10000, "GeoNames, adm3 Isansa"),
		c("Mbozi", "Idibira", 33.1128, -8.9829, 10000, "GeoNames, adm3 Ruanda (Mbozi)"),
		c("Mbozi", "Ukwire", 32.8969, -9.2150, 11564, "GADM 4.1, adm3 Isandula"),
		c("Mbozi", "Ichenjezya", 32.9261, -8.9457, 40000, "GeoNames, adm2 Mbozi"),  # village and its ward (Hasanga) not found
		c("Mbozi", "Ilembo", 32.9463, -9.1187, 10976, "GADM 4.1, adm3 Vwawa"),
		c("Mbozi", "Masangula", 33.0170, -9.1330, 3000, "GeoNames, village"),
		c("Mbozi", "Mantengu (B)", 32.9463, -9.1187, 10976, "GADM 4.1, adm3 Vwawa"),
		c("Mbeya", "Mwasoma", 33.5913, -8.9233, 10000, "GeoNames, adm3 Ijombe"),
		c("Mbeya", "Tembela", 33.6252, -8.9745, 4820, "GADM 4.1, adm3 Tembela"),
		c("Mbeya", "Iwala", 33.3317, -8.8843, 10000, "GeoNames, adm3 Utengule Usongwe"),
		c("Mbeya", "Mshewe", 33.1586, -8.7592, 19131, "GADM 4.1, adm3 Mshewe"),
		c("Mbeya", "Njelenje", 33.1586, -8.7592, 19131, "GADM 4.1, adm3 Mshewe"),
		c("Mbeya", "Nsambia", 33.1586, -8.7592, 19131, "GADM 4.1, adm3 Mshewe"),
		c("Mbeya", "Lusungo", 33.3566, -8.9617, 40000, "GeoNames, adm2 Mbeya"),  # village and its ward (Bonde la Usangu) not found
		c("Mbeya", "Idiwili", 33.2560, -9.0734, 10000, "GeoNames, adm3 Isuto"),
		c("Mbeya", "Galijembe", 33.6000, -8.9670, 3000, "GeoNames, village")
	), stringsAsFactors = FALSE)
	names(geo) <- c("adm2", "location", "longitude", "latitude", "geo_uncertainty", "geo_source")
	geo$longitude <- as.numeric(geo$longitude)
	geo$latitude <- as.numeric(geo$latitude)
	geo$geo_uncertainty <- as.numeric(geo$geo_uncertainty)

	d <- merge(d, geo, by=c("adm2", "location"), all.x=TRUE)

	# records whose VBAA has no match in the technology file have only the district;
	# georeference those at the district level (GeoNames ADM2 centroids)
	geod <- data.frame(
		adm2 = c("Iringa", "Makambako", "Mbeya", "Mbozi", "Mufindi", "Njombe", "Songea"),
		lond = c(34.8816, 34.9065, 33.3566, 32.9261, 35.2082, 35.1325, 35.4000),
		latd = c(-7.5640, -8.8708, -8.9617, -8.9457, -8.5953, -9.2091, -10.3906),
		uncd = c(60000, 15000, 40000, 40000, 40000, 40000, 50000)
	)
	d <- merge(d, geod, by="adm2", all.x=TRUE)
	i <- is.na(d$latitude)
	d$latitude[i] <- d$latd[i]
	d$longitude[i] <- d$lond[i]
	d$geo_uncertainty[i] <- d$uncd[i]
	d$geo_source[i] <- "GeoNames, adm2"
	d$latd <- d$lond <- d$uncd <- NULL

	# region (adm1) for each district
	adm1 <- c(Iringa="Iringa", Mufindi="Iringa", Makambako="Njombe", Njombe="Njombe",
		Mbeya="Mbeya", Mbozi="Songwe", Songea="Ruvuma")
	d$adm1 <- unname(adm1[d$adm2])

	d$geo_from_source <- FALSE

	
	d$country = "Tanzania" ### experiment was conducted the in Southern highlands of Tanzania.
	# one trial = one site (village demo); unmatched records are grouped by district
	d$trial_id <- as.character(as.integer(as.factor(paste(d$adm2, ifelse(is.na(d$location), "unknown", d$location)))))
	d$VB <- NULL  # VBAA (farmer adviser) personal name; only needed as the merge key
	d$on_farm <- TRUE      #based on data description
	d$is_survey <- FALSE
	d$irrigated <- NA
	
	
	d$planting_date <- "2017"  #The demos were planted in 2017-based on the description
	d$harvest_date  <- NA
	
	d$fertilizer_type[d$fertilizer_used == TRUE] <- "NPK"### fertilizer source used is Starter pack
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- NA
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA

	
	carobiner::write_files(path, meta, d)
}
