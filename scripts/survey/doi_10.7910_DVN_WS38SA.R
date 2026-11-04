# R script for "carob"

## ISSUES

carob_script <- function(path) { 
  
"Here we release the Rural Household Multi-Indicator Survey (RHoMIS) dataset, derived from the open-source RHoMIS toolkit. RHoMIS is designed to simplify the collection, analysis, and dissemination of interoperable data from farm-household surveys. This release harmonizes 54,873 household observations spanning 35 countries in Latin America, Africa, and Asia into a single dataset, collected between 2015 and 2023. The data supports the investigation of system dynamics relating to food production, food security, and pathways out of poverty for smallholder farmers. This dataset is an update and revision of the previous RHoMIS dataset, published in 2020. In this new release we present a database quadruple the size, with observations of 54,873 households from 35 countries and 119 unique projects, collected from 2015 until 2023. These data are accompanied with radically improved analytical routines to process the data and calculate common performance indicators. We report 1599 variables and 41 farm household level indicators, as well as crop-level information (production, area planted, use of the produce, and sale value). We also report gendered decision-making information over the consumption of produced foodstuffs and over the incomes derived from on- and off-farm activities. (2025-01-28)"

	uri <- "doi:10.7910/DVN/WS38SA"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		project="RHoMIS",
		publication= "doi:10.1038/s41597-020-0388-8",
		data_organization = "UBR;ILRI;FRB;WUR;CIMMYT;CIRAD;ABC;TAID;ABIO;USM;SNV;IER;UED;ICRAF;COLSU;FVAL;CU;RUA;UCBU;SLU;IITA;UFS;ROTH;Yanapai;STATS4SD;CIP;BAU;GIZ;KALRO;OAF;TUAF;ZALF",
		data_type="survey", 
		response_vars = "none",
		treatment_vars = "none",
		carob_contributor="Robert Hijmans",
		carob_date="2025-05-30",
		carob_effort = NA,
		carob_completion = 1  #1%, a lot more to be done
	)
  
  
	f1 <- ff[basename(ff) == "full_survey_data.csv"]
	f2 <- ff[basename(ff) == "indicator_data.csv"]
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r <- merge(r1, r2, by="id_unique")
	
	d <- data.frame(
		trial_id= r$id_unique,
		planting_date=as.character(NA),
		longitude = r$gps_lon_rounded,
		latitude = r$gps_lat_rounded,
		geo_from_source = TRUE,
		elevation = r$gps_alt,
		adm1 = carobiner::fix_name(r$region, case="title"),
		location = carobiner::fix_name(r$sublocation, case="title"),
		farmland_owned = r$land_owned_ha,
		on_farm = TRUE,
		is_survey = TRUE
	)

	cc <- geodata::country_codes()
	d$country <- cc$NAME[match(r$iso_country_code.x, cc$ISO2)]
	
# dates
	d1 <- r$starttime_auto
	d2 <- rep(as.Date(NA), length(d1))
	i <- grep("/", d1)
	d2[i] <- as.Date(substr(d1[i], 1, 10), "%d/%m/%Y")
	i <- grep("-", d1)
	d2[i] <- as.Date(substr(d1[i], 1, 10), "%Y-%m-%d")
	i <- is.na(d2) & (!is.na(d1))
	x <- do.call(rbind, strsplit(d1[i], ", "))
	x[,2] <- sapply(strsplit(x[,2], " "), \(i) i[1])
	y <- do.call(rbind, strsplit(x[,1], " "))
	x <- cbind(x[,2], y)
	x[,2] <- carobiner::eng_months_to_nr(x[,2])
	d2[i] <- as.Date(apply(x, 1, \(i) paste(i, collapse="-")))
	d2 <- as.character(d2)
	i <- is.na(d2)
	d2[i] <- r$year.x[i]
	d$date <- d2
# end dates

### fixing geo coordinates errors 
	i <- grepl("Zinder", d$adm1)
	d$country[i] <- "Niger"
	
	i <- grepl("Sikass|Sokasso", d$adm1)
	d$country[i] <- "Mali"
	i <- grepl("Yatenga", d$location)
	d$country[i] <- "Burkina Faso"
	
	i <- grepl("Ile_anjouan", d$adm1) & grepl("Comoros", d$country)
	d$longitude[i] <- 44.4380
	d$latitude[i] <- -12.212
	d$geo_from_source[i] <- FALSE
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Cascades", d$adm1) & grepl("Burkina Faso", d$country)
	d$longitude[i] <-  -4.5704
	d$latitude[i] <- 10.3517
	d$geo_from_source[i] <- FALSE
	d$geo_source[i] <- "GADM 4.1, adm1"
	
	i <- grepl("Nord", d$adm1) & grepl("Burkina Faso", d$country)
	d$longitude[i] <-  -2.2833
	d$latitude[i] <- 	13.4594
	d$geo_from_source[i] <- FALSE
	d$geo_source[i] <- "GADM 4.1, adm1"
	
	i <- grepl("Centre-Nord", d$adm1) & grepl("Burkina Faso", d$country)
	d$longitude[i] <- -0.9745	
	d$latitude[i] <- 	13.2688
	d$geo_from_source[i] <- FALSE
	d$geo_source[i] <- "GADM 4.1, adm1"
	
	i <- grepl("Cibitoke", d$adm1) & grepl("Burundi", d$country)
	d$longitude[i] <- 29.2293	
	d$latitude[i] <- 	-2.8191
	d$geo_from_source[i] <- FALSE
	d$geo_source[i] <- "GADM 4.1, adm1"
	
	i <- grepl("Amhara", d$adm1) & grepl("Ethiopia", d$country)
	d$longitude[i] <- 38.0473	
	d$latitude[i] <- 	11.5625
	d$geo_from_source[i] <- FALSE
	d$geo_source[i] <- "GADM 4.1, adm1"
	
	i <- grepl("Uppereast", d$adm1) & grepl("Ghana", d$country)
	d$longitude[i] <- -0.8033	
	d$latitude[i] <- 10.7806
	d$geo_from_source[i] <- FALSE
	d$geo_source[i] <- "GADM 4.1, adm1"
	
	i <- grepl("Kassena_nakana_west", d$adm1) & grepl("Ghana", d$country)
	d$longitude[i] <- -1.2341
	d$latitude[i] <- 10.9170
	d$geo_from_source[i] <- FALSE
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Kassena_nakana_east", d$adm1) & grepl("Ghana", d$country)
	d$longitude[i] <- -1.0589
	d$latitude[i] <- 10.7945
	d$geo_from_source[i] <- FALSE
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Lambussie_karni", d$adm1) & grepl("Ghana", d$country)
	d$longitude[i] <- -2.6127
	d$latitude[i] <- 10.852
	d$geo_from_source[i] <- FALSE
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Kamanyola", d$adm1) & grepl("Democratic Republic of the Congo", d$country)
	d$longitude[i] <- 29.0064	
	d$latitude[i] <- 	-2.7406
	d$geo_from_source[i] <- FALSE
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Miti", d$adm1) & grepl("Democratic Republic of the Congo", d$country)
	d$longitude[i] <- 28.7934	
	d$latitude[i] <- 	-2.3553
	d$geo_from_source[i] <- FALSE
	d$geo_source[i] <- "Google Maps"
	
	
	i <- grepl("Zanzibar", d$adm1) & grepl("Tanzania", d$country)
	  d$longitude[i] <-  39.225
	  d$latitude[i] <- -6.1611
	  d$geo_from_source[i] <- FALSE
	  d$geo_source[i] <- "Google Maps"
	  
	i <- grepl("Tanga", d$adm1) & grepl("Tanzania", d$country)
	  d$longitude[i] <- 38.2819	
	  d$latitude[i] <- -5.2180
	  d$geo_from_source[i] <- FALSE
	  d$geo_source[i] <- "GADM 4.1, adm1"
	  
	  i <- grepl("Arusha", d$adm1) & grepl("Tanzania", d$country)
	  d$longitude[i] <-  35.9229	
	  d$latitude[i] <- -2.9700
	  d$geo_from_source[i] <- FALSE
	  d$geo_source[i] <- "Google Maps"
	 
#	farmland_rentedin = r$landrentin,
#	farmland_rentedout= r$landrentout,
#	cropland_used = r$landcultivated,
#	land_irrigated = r$land_irrigated,
#	land_tenure = r$land_tenure

	
	geo <- data.frame(
	  location =  c("Koumbri", "Sourou", "Yorosso", "Yatenga", "Gassan", "Sabra", "Bachicorepoun", "Diela", "Dassa", "Bilanga-Yanga-Peulh", "Doh", "Nebia", "Farba", "Rugombo", "Giteranyi", "Muyinga", "Kiffosso1", "Damy", "Tara", "Kouara", "Hanekuy", "Sadien", "Debere", "Sanekuy", "Lenekuy", "Sienso_marka_bobo", "Bougoudala", "Turula", "Bolimasso", "Tion", "Boissoni", "Tominian", "Bouani", "N'gorokan", "Feguesso", "Dabani", "Terekoungo", "Bogossoni", "Yasso", "Wara", "Bora", "Kibi", "Mirriah", "Falmey", "Doli", "Bushekeri", "Giheke", "Rwimbogo", "Nkanka", "Nzahaha", "Bushenge", "Ruharambuga", "Nyabitekeri", "Bugarama", "Muganza", "Mururu", "Gahara", "Nongowa", "Mbare", "Simango", "Benhourgou", "Divolet", "Endiizi", "Kiffosso1", "Kolonkomi", "Lorrain", "Mariah", "Miriah", "Mirriah", "Mirriqh", "O","Rotonak_mondol", "Soultenga", "Tindane", "Tobou-Peulh", "Tohogodou", "Yatenga", "Doullou_1_2", "Kabezi", "Kikagati", "Mpanda", "Quyã¡âºâ¾t_thã¡âºâ®ng", "Bakelo", "Basona_worena", "Goshebado", "Zendegur"),
	  country = c(rep("Burkina Faso", 13), rep("Burundi", 3), rep("Mali", 26), rep("Niger", 3), rep("Rwanda", 12), "Sierra Leone", "Uganda", "Zambia", rep("Burkina Faso", 2), "Uganda", "Mali", "Burkina Faso", rep("Niger", 5), "Mali", "Cambodia", rep("Burkina Faso", 5), "Niger", "Burundi", "Uganda", "Burundi", "Vietnam", rep("Ethiopia", 4)),
	  lon = c(-2.4098243, -2.9621810, -5.0631575, -2.5249513, -3.2864443, 0.3330871, -2.7312881, 1.4303899, -2.6972208, -0.0830948, -2.7373982, -2.7042139, 0.2355030, 29.0821647, 30.4674065, 30.3171441, -4.9020078, -4.6231539, -4.7186938, -4.6920231, -4.4960533, -4.5136567, -4.6790365, -4.6143787, -4.5455295, -4.8814335, -4.9478267, -4.5946856, -4.6213771, -4.6717732, -4.6492000, -4.5902405, -4.4755798, -4.9950857, -4.8613471, -7.9017497, -4.9188700, -4.8652121, -4.6745330, -5.2293655, -4.5622195, -4.7354752, 9.1585561, 2.8527150, 8.8636524, 29.0916907, 28.9561118, 28.9528284, 28.9448286, 28.9775486, 28.9986178, 29.0289350, 29.0269677, 29.0072671, 29.4324736, 28.9082953, 30.5042740, -11.4729787, 31.0367526, 25.8960276, 0.48, -2.7031,31.140, -4.8897, -0.08063, 8.9856, 8.9856, 8.9856, 8.9856, 8.9856, -5.6687, 102.8253, 0.03333, -0.0666, 0.0895, -0.20790, -2.4948725, 7.217, 29.351, 30.662, 29.4037, 105.776, 39.0145, 39.5071, 39.5071, 39.5071),
	  lat =  c(13.8457316, 13.2311675, 12.1909956, 13.5942939, 12.7974483, 12.7410286, 12.4234432, 11.9358832, 12.4158999, 12.4599708, 12.4110007, 12.4073648, 14.3304130, -2.8130179, -2.4760120, -2.7335984, 12.4995837, 13.1792733, 13.1584740, 13.1050529, 13.2421558, 13.2536019, 13.0291990, 13.0708972, 13.0394759, 13.2311408, 13.2286113, 13.1067736, 13.2873635, 13.2781039, 13.3140000, 13.2868042, 13.6377079, 13.0046578, 13.0851233, 12.9138084, 13.2826792, 13.2466113, 13.1361076, 12.6663455, 13.4422360, 13.2129765, 13.7145507, 12.5944720, 14.8542897, -2.4323270, -2.4569761, -2.6413423, -2.4019779, -2.6583514, -2.4241642, -2.4671816, -2.3365666, -2.6948888, -2.6767373, -2.5238707, -2.3352057, 7.4448227, -0.9220095, -17.4296783, 12.38, 12.415556, -0.869, 12.5303, 12.5135, 13.8018, 13.8018, 13.8018, 13.8018, 13.8018, 11.3161, 12.9066,11.85, 12.4333, 12.5345, 12.5561, 13.6478, 13.417, -3.5383, -1.0384, -3.170, 22.0180, 10.518, 9.7606, 9.7606, 9.7606),
	  geo_s = "Google Maps",
	  geo_from = FALSE
	)
	
	geo$lat <- round(geo$lat, 3)
	geo$lon <- round(geo$lon, 3)
	d <- merge(d, geo, by = c("location", "country"), all.x = TRUE)
	i <- !is.na(d$lon)
	d$longitude[i] <- d$lon[i]
	d$latitude[i] <- d$lat[i]
	d$geo_from_source[i] <- d$geo_from[i]
	d$geo_source[i] <- d$geo_s[i]
	d$lon <- d$lat <-d$geo_from <- d$geo_s <- NULL
	
	### set rows with incorrect coordinates that cannot be corrected because both location and adm1 are missing into NA.
 idx <- which(is.na(d$location) & (d$latitude %in% c(9.7, 9.5, 14.3, 13.5, 13.2, 13.6, 13.3, 13.8, 11.0, 14.5, 14.7, -3.2) | d$longitude %in% c(-82.8, -82.6, -82.7, 4.0, -16.5, -16.2, -15.2, -15.9, -15.5, -15.6, -14.9, -16.1, -14.0, -16.3, -14.3, -14.4, -16.4, -16.0, -14.6, -15.8, -1.2, -1.0, -89.1, 37.7)))
 d$longitude[idx] <- NA
 d$latitude[idx] <- NA
 
 ### Drop rows with missing longitude and latitude that cannot be filled in because both location and adm1 are missing.
	#d <- d[!is.na(d$longitude),]
	
	
	carobiner::write_files(meta, d, path=path)
}

