# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


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

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	
	names(r1) <- as.character(unlist(r1[5, ]))
	r1 <- r1[-(1:5), ]
	rownames(r1) <- NULL

	r1$PH.[r1$PH. %in% c("SG", "IT")] <- NA
	r1$Gc.[r1$Gc. == "IT"] <- NA
	
	
	names(r2) <- as.character(unlist(r2[3, ]))
	r2 <- r2[-c(1:4), ]
	rownames(r2) <- NULL
	
	d1 <- data.frame(
	  adm2 = r1$District,
	  plot_id = as.character(r1$`Plot number`),
	  #p_asp = r1$Vig.,
	  plant_height = as.numeric(r1$PH.),
	  treatment = r1$Treatment,
	  variety = r1$Variety,
	  germination_rate = as.numeric(r1$Gc.),
	  yield = NA, ## crop assessment was done before harvest as described in the data discription
	  crop = "common bean" ## not specified whether its common bean or what type of bean is that
	)
	
	
	d1$plant_height[d1$plant_height == 0] <- NA
	d1$variety[d1$variety %in% c("V1= Njano Uyole", "V1= Had not planted")] <- "V1"
	d1$variety[d1$variety %in% c("V2= Uyole 96")] <- "V2"
	d1$variety[d1$variety %in% c("V3= Wanja", "V3= Calima Uyole", "V3= Uyole 03")] <- "V3"
	d1$variety[grepl("^Farmer Variety", d1$variety)] <- "farmer Variety"
	d1$fertilizer_used <- d1$treatment %in% c("Fertilizer", "Apron Star +Fert")
	
	d2 <- data.frame(
	   adm2 = r2$Distrct,
	   adm3 = r2$Ward,
	   location = r2$Village,
	   #fertilizer_used = r2$`Fertilizer Source`, this variable only once source apron star
	   #cropping_system = r2$`Technology Selected`, this is similar to fertilizer used
	   farmer_gender = r2$Gender,
	   field_size = as.numeric(r2$`Farm Size (m2)`)
	 )
	 
	d2$adm3[d2$adm3 == "Matambula"] <- "Mtambula"
	d2$adm3[d2$adm3 == "Nyororo"] <- "Nyololo"
	d2$adm3[d2$adm3 == "litisha"] <- "Litisha"
	
	d <- merge(d1, d2, by = "adm2", all.x = TRUE)
	d <- unique(d)
	
#### geo-coordinates for location were obtained from geonames	
	geo4 <- data.frame(
	  adm1 = c("Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Njombe", "Njombe", "Njombe", "Njombe", "Njombe", "Njombe", "Njombe", "Njombe", "Njombe", "Njombe", "Ruvuma", "Ruvuma", "Ruvuma", "Ruvuma", "Ruvuma", "Ruvuma", "Ruvuma", "Ruvuma", "Songwe", "Songwe", "Songwe", "Songwe", "Songwe", "Songwe", "Songwe", "Songwe", "Songwe", "Mbeya", "Mbeya", "Mbeya", "Mbeya", "Mbeya", "Mbeya", "Songwe", "Songwe", "Mbeya"),
	  adm2 = c("Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Mufindi", "Mufindi", "Mufindi", "Mufindi", "Mufindi", "Mufindi", "Wanging'ombe", "Wanging'ombe", "Wanging'ombe", "Wanging'ombe", "Wanging'ombe", "Wanging'ombe", "Wanging'ombe", "Wanging'ombe", "Wanging'ombe", "Wanging'ombe", "Songea", "Songea", "Songea", "Songea", "Madaba", "Madaba", "Madaba", "Madaba", "Mbozi", "Mbozi", "Mbozi", "Mbozi", "Mbozi", "Mbozi", "Mbozi", "Mbozi", "Mbozi", "Mbeya", "Mbeya", "Mbeya", "Mbeya", "Mbeya", "Mbeya", "Mbozi", "Mbozi", "Mbeya"),
	  adm3 = c("Magulilwa", "Magulilwa", "Mgama", "Mgama", "Mseke", "Mseke", "Mseke", "Ulanda", "Kihorogota", "Nduli", "Kalenga", "Ulanda", "Mtambula", "Nyololo", "Idunda", "Isalavanu", "Nyololo", "Sadani", "Saja", "Wanging'ombe", "Wanging'ombe", "Wanging'ombe", "Wanging'ombe", "Wanging'ombe", "Igima", "Itulahumba", "Imalinyi", "Wanging'ombe", "Peramiho", "Litisha", "Maposeni", "Litisha", "Mtyangimbole", "Mkongotema", "Mkongotema", "Mkongotema", "Ruanda", "Iyula", "Ruanda", "Ruanda", "Isandula", "Vwawa", "Vwawa", "Nyimbili", "Vwawa", "Tembela", "Tembela", "Tembela", "Mshewe", "Mshewe", "Mshewe", "Ruanda", "Iyula", "Tembela"),
	  location = c("Magulilwa", "Ng'enza", "Kikombwe", "Ihemi", "Mseke", "Tanangozi", "Wenda", "Mangalali", "Uhominyi", "Mkungugu", "Kalenga", "Itamba", "Nyakipambo", "Lwing'ulo", "Ikangamwane", "Isalavanu", "Njojo", "Igomba", "Saja", "Litundu", "Uhambule", "Ikingula", "Utiga", "Mawende", "Mawindi", "Itulahumba", "Masaulwa", "Wanging'ombe", "Nguvu Moja", "Morogoro A", "Peramiho (A)", "Litisha", "Likalangilo", "Mkongotema", "Lutukira", "Ndelenyuma", "Lumbila", "Ipyana", "Lungwa", "Idibira", "Ukwire", "Ichenjezya", "Ilembo", "Masangula", "Mantengu (B)", "Mwasoma", "Tembela", "Iwala", "Mshewe", "Njelenje", "Nsambia", "Lusungo", "Idiwili", "Galijembe"),
	  longitude = c(35.7381, 35.6785, NA, 35.4868, 35.5635, 35.5654, 35.5951, 35.5678, 35.8091, 35.7765, 35.6031, 35.68, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 34.748, 34.599, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 33.017, NA, 33.488, 33.488, 33.488, NA, NA, NA, NA, NA, 33.6),
	  latitude = c(-7.9701, -7.9618, NA, -8.0163, -7.9453, -8.9487, -7.9094, -7.7735, -7.5175, -7.5377, -7.8013, -7.7581, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, -9.086, -9.130, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, -9.133, NA, -8.941, -8.941, -8.941, NA, NA, NA, NA, NA, -8.967)
	)
	
	##### geo_cordinates for adm3 were obtained from is from GADM # if we cannot find "adm4" we fall back to "adm3"
	geo3 <- data.frame(
	  adm3 = c("Kalenga", "Kihorogota", "Mgama", "Mseke", "Nduli", "Ulanda", "Idunda", "Isalavanu", "Mtambula", "Nyololo", "Sadani", "Mshewe", "Tembela", "Ruanda", "Igima", "Imalinyi", "Saja", "Wanging'ombe", "Litisha", "Maposeni", "Mkongotema", "Mtyangimbole", "Peramiho", "Isandula", "Iyula", "Vwawa"),
	  #adm2 = c("Iringa Rural", "Iringa Rural", "Iringa Rural", "Iringa Rural", "Iringa Rural", "Iringa Rural", "Iringa Urban", "Mufindi", "Mufindi", "Mufindi", "Mufindi", "Mufindi", "Mbeya Rural", "Mbeya Rural", "Mbeya Urban", "Mbeya Urban", "Wanging'ombe", "Wanging'ombe", "Wanging'ombe", "Wanging'ombe", "Mbinga", "Songea Rural", "Songea Rural", "Songea Rural", "Songea Rural", "Songea Rural", "Mbozi", "Mbozi", "Mbozi", "Mbozi"),
	  #adm1 = c("Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Iringa", "Mbeya", "Mbeya", "Mbeya", "Mbeya", "Njombe", "Njombe", "Njombe", "Njombe", "Ruvuma", "Ruvuma", "Ruvuma", "Ruvuma", "Ruvuma", "Ruvuma", "Songwe", "Songwe", "Songwe", "Songwe"),
	  long3 = c(35.6019, 35.8542, 35.5693, 35.5479, 35.7962, 35.5286, 34.8079, 35.2579, 35.0091, 34.9463, 34.9661, 33.1586, 33.6252, 33.4679, 34.7413, 34.6062, 34.7653, 34.6665, 35.2952, 35.4496, 35.5385, 35.6689, 35.4693, 32.8969, 33.1756, 32.9463),
	  lat3 = c(-7.8314, -7.4797, -8.0514, -7.9185, -7.5868, -7.8190, -8.6484, -8.2021, -8.7068, -8.4550, -8.2075, -8.7592, -8.9745, -8.9102, -9.1900, -9.1917, -8.7312, -8.8360, -10.6271, -10.4874, -10.0666, -10.3974, -10.6101, -9.2150, -9.0961, -9.1187),
	  unc3 = c(8986, 11274, 13908, 16540, 11795, 13891, 12047, 12695, 10633, 18495, 16455, 19131, 4820, 1085, 15206, 10522, 14377, 17014, 16705, 18046, 45073, 11982, 8122, 11564, 15261, 10976),
	  src3 = "GADM 4.1, adm3"
	)
	
	# replace the missing lon/lat in geo4 with geo3
	geo <- merge(geo4, geo3, all.x=TRUE)
	i <- is.na(geo$latitude) 
	geo$latitude[i] <- geo$lat3[i]
	geo$longitude[i] <- geo$long3[i]
	geo$geo_uncertainty[i] <- geo$unc3[i]
	geo$geo_source[i] <- geo$src3[i]
	geo$lat3 <- geo$long3 <- geo$unc3 <- geo$src3 <- NULL
	

	# merge by adm4 is unique within the other adm levels
	d <- merge(d, geo[c("location", "longitude", "latitude", "geo_uncertainty", "geo_source")], by="location", all.x=TRUE)
	d$geo_from_source <- FALSE

	
	d$country = "Tanzania" ### experiment was conducted the in Southern highlands of Tanzania.
	d$trial_id <- "1"
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
