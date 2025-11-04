# R script for "carob"


carob_script <- function(path){

"Title: Non-responsiveness of crops to fertilizers under some soils in sub-Saharan Africa
 
Low productivity of agriculture observed in different parts of sub-Saharan Africa is threatening food security in the region. Decades of production with mostly application of small amounts of inorganic fertilizers (mostly macronutrients) and scarce organic resources in the context of integrated soil fertility management (ISFM) result in nutrient mining of secondary and micronutrients in majority of smallholder farms. With the last decade, crop non-responsiveness to nutrient application has become an important issue requiring scientific understanding. We provide data focused on identifying the extent of non-responsiveness of crops to nutrient application and the associated factors. Data contains crop yield response to secondary and micronutrient (SMN), manure and lime application relative to yields of only NP/K application. (2020-02-25)"
  
	uri <- "doi:10.7910/DVN/GXUNAZ"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)
  
	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
		project=NA,
		publication= NA,
		data_organization = "CIAT",
		carob_contributor="Rachel Mukami",
		carob_date="2023-05-29",
		data_type="compilation",
		response_vars = "yield",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer"		
	)
	
	# reading the data.csv data
	f <- ff[basename(ff) == "Non responsiveness of crop to fertiliser dat V2.xlsx"]
	r <- carobiner::read.excel(f) |> unique()
	
	d <- data.frame(
		variety = carobiner::fix_name(r$Var_Type,"title"),
		location = carobiner::fix_name(r$Site, "title"),
		crop = tolower(r$Crop_Type),
		variety_type = tolower(r$Var_Type),
		trial_id = r$`Field ID`,
		rep = as.integer(r$Replications),
		N_fertilizer = r$N,
		P_fertilizer = r$P...9,
		K_fertilizer = r$K,
		soil_pH = r$pH,
		soil_SOC = r$SOC,
		soil_P_total = r$P...28,
		soil_clay = r$Clay,
		soil_type = r$SoilType,
		rain = r$Rainfall,
		reference = r$Dataset
	)
	d$SiteID = carobiner::fix_name(r$`Site ID`, "title")
	d <- cbind(d,  r[,  c("Abs_Control",  "NK", "NP", "NPK", "NPK+S+Ca+Mg+Zn+B", "PK", "NPK+Lime", "NPK+Manure")])


	# country sites based on publication and coordinates
	cntrlocs <- rbind(
		data.frame(country="Nigeria", 
			location=c("Bakori", "Bunkure", "Dandume", "Doguwa", "Faskari", "Funtua", "Giwa", "Ikara", "Kauru", "Lere", "Makarfi", "Soba", "T/wada", "Tofa", "Tudun Wada", "Bauchi", "Calabar", "Ibadan", "Ikenne", "Ikole", "Ikoyi", "Ilora", "Iwo", "Kishi", "Mokwa", "Ogbomosho", "Oyo", "Pamp",  "Yola", "Sepeteri", "Samaru", "Ilorin", "Kafin-Maiyaki", "Yandev", "Dengi", "Tumu")), 
		data.frame(country="Kenya",  
			location = c("Kand", "Sidi", "Nai Farm", "Strong", "Cox", "Davidson", "Hulme", "Kiminini", "Leys", "Russell", "Sabwani",  "Menengai")), 
		data.frame(country="Malawi",  
			location = c("Kasu", "Nkha", "Thuc", "Balaka", "Bembeke", "Chitedze", "Kanyama", "Lilongwe", "Mlomba", "Mzuzu", "Manjawira", "Tsangano", "Salima")), 
		data.frame(country="Mali",  location = c("Kolo", "Kont", "Sourou Valley")), 
		data.frame(country="Tanzania",  location = c("Kibe", "Mbin", "Mpangala")), 
		data.frame(country="Ghana",  location = c("Kade", "Kpong", "Nyankpala")), 
		data.frame(country="Mozambique",  location = c("Nampula", "Sussundenga")), 
		data.frame(country="Benin",  location=c("Niaouli", "Cana", "Warda")), 
		data.frame(country="Togo",  location=c("Affem", "Sessaro")), 
		data.frame(country="Côte d'Ivoire",  location=c("Bouake",  "Dabou",  "Guessihio")), 
		data.frame(country="Ethiopia",  location=c("Aykel",  "Gewane College of Agriculture of the Afar Region",  "Tigray",  "Chilga (wujiraba)",  "Mekelle University",  "Imla")), 
		data.frame(country="Zimbabwe",  location=c("Dendenyore",  "Hwedza")), 

		data.frame(country=c("Sudan",  "Zambia",  "Cameroon"),  
			location=c("Gezira Research Station Farm",  "Lusaka",  "Minna"))
	)

	i <- match(d$location, cntrlocs$location)
	d$country <- cntrlocs$country[i]
	j <- is.na(d$country)
	d$country[j] <- r$COUNTRY[j]
	d$country[d$country == "Cote d'Ivoire"] <- "Côte d'Ivoire"
	
	# swapping back coordinates that are wrongly swapped
	d$latitude <- NA
	d$longitude <- NA
	swapped_coords <- 
		c("Affem",  "Balaka",  "Bembeke",  "Bouake",  "Cana",  "Chitedze",  "Dabou",  "Ibadan",  "Gewane College of Agriculture of the Afar Region", "Gezira Research Station Farm", "Kanyama", "Ikenne",  "Ikoyi", "Ilora", "Kade", "Kishi", "Kpong", "Lilongwe", "Lusaka", "Mlomba", "Mpangala", "Mzuzu", "Nai Farm", "Nampula",  "Niaouli", "Nyankpala", "Ogbomosho", "Oyo", "Tigray", "Warda", "Tsangano", "Tigray", "Sussundenga", "Strong",  "Cox", "Davidson", "Hulme", "Kiminini", "Leys", "Sessaro", "Sourou Valley", "Sepeteri", "Salima", "Russell", "Sabwani",  "Hwedza", "Dendenyore", "Imla")
		
	d$latitude <- ifelse(d$location %in% swapped_coords,  r$Lon,  r$Lat)
	d$longitude <- ifelse(d$location %in% swapped_coords,  r$Lat,  r$Lon)
	d$geo_from_source <- !(d$location %in% swapped_coords)
	d$geo_from_source[is.na(d$latitude)] <- FALSE

	i <- grep("N",  r$Lat)
	latlon <- t(sapply(strsplit(r$Lat[i],  " "),  \(i) 
		c(as.numeric(i[1]) + as.numeric(i[2])/60,  as.numeric(i[4]) + as.numeric(i[5])/60)))
	d[i,  c("latitude",  "longitude")] <- latlon
	d$latitude <- as.numeric(d$latitude)
	d$longitude[grep(" and ",  d$longitude)] <- NA
	d$longitude <- as.numeric(d$longitude)

	d$latitude[d$location == "Calabar"] <- 	4.9795999
	d$longitude[d$location == "Calabar"] <- 8.3373597	
	d$longitude[d$location == "Affem"] <- 1.5
	d$latitude[d$location == "Affem"] <- 9.15
	d$longitude[d$location == "Ojeniyi and Kayode"] <- 3.867
	d$latitude[d$location == "Manjawira"] <- -14.9994263
	d$longitude[d$location == "Manjawira"] <- 34.8533877
	d$longitude[d$location == "Sessaro"] <- 1.16700
	d$latitude[d$location == "Sessaro"] <- 8.6329999999999991
	d$longitude[d$location == "Samaru"] <- 11.183
	d$latitude[d$location == "Samaru"] <- 7.63300
	# fixing NA coordinates
	d$latitude[d$location == "Dengi"] <- 9.3674857
	d$longitude[d$location == "Dengi"] <- 9.9627315
	d$latitude[d$location == "Ilorin"] <- 10.8818885
	d$longitude[d$location == "Ilorin"] <- 4.007165
	d$latitude[d$location == "Kafin-Maiyaki"] <- 11.4585101
	d$longitude[d$location == "Kafin-Maiyaki"] <- 8.2022946
	d$latitude[d$location == "Menengai"] <- -0.517362
	d$longitude[d$location == "Menengai"] <- 36.0787746
	d$latitude[d$location == "Tumu"] <- 10.1121061
	d$longitude[d$location == "Tumu"] <- 11.1076295
	d$latitude[d$location == "Yandev"] <- 7.36308
	d$longitude[d$location == "Yandev"] <- 9.04235
	d$latitude[d$SiteID == "Rhodes and Kpaka,  1982"] <- 8.64003498
	d$longitude[d$SiteID == "Rhodes and Kpaka,  1982"] <- -11.8400269
	d$latitude[d$Dataset == "Kihara_Wkenya"] <- 0.4994716
	d$longitude[d$Dataset == "Kihara_Wkenya"] <- 34.5698326 # western Kenya
	d$latitude[d$location == "Guessihio"] <- 6.1090571
	d$longitude[d$location == "Guessihio"] <- -6.0018931
	d$latitude <- ifelse(d$location == "Sidi" & is.na(d$latitude), -0.17265, 
					ifelse(d$location == "Kand" & is.na(d$latitude), -0.9227613, 
					ifelse(d$location == "Thuc" & is.na(d$latitude), -15.90298,  d$latitude)))
	
	d$longitude <- ifelse(d$location == "Sidi" & is.na(d$longitude),  34.44384, 
					ifelse(d$location == "Kand" & is.na(d$longitude),  37.0055, 
					ifelse(d$location == "Thuc" & is.na(d$longitude),  35.29974,  d$longitude)))
	
	i <- which(d$country == "Nigeria" & d$location == "Mokwa")
	d$longitude[i] <- 5.0548
	d$latitude[i] <- 9.2979

	d$longitude[d$location == "Ibadan" & d$longitude == 34.960000] <- 3.867000



	d$id <- 1:nrow(d)
	# converting dataset from wide to long
	d <- reshape(d,  direction = "long", 
			varying = c("Abs_Control",  "NK", "NP", "NPK", "NPK+S+Ca+Mg+Zn+B", "PK", "NPK+Lime", "NPK+Manure"), 
			v.names = "yield", 
			times = c("Abs_Control",  "NK", "NP", "NPK", "NPK+S+Ca+Mg+Zn+B", "PK", "NPK+Lime", "NPK+Manure"), 
			idvar = "id")
	# dropping row names indices
	rownames(d) <- NULL
	colnames(d)[colnames(d) == "time"] <- "treatment"
	d$id <- NULL
	
	i <- d$treatment == "Abs_Control"
	d$N_fertilizer[i] <- 0
	d$P_fertilizer[i] <- 0
	d$K_fertilizer[i] <- 0
		
	# drop yield with NAs
	d <- d[!is.na(d$yield), ]
		
	d$lime <- 0	
	# this number (500) needs to be checked!
	d$lime[d$treatment == "NPK+Lime"] <- NA
	d$OM_used <- FALSE
	d$OM_used[d$treatment == "NPK+Manure"] <- TRUE
	
	d$S_fertilizer <- 0
	d$Ca_fertilizer <- 0
	d$Mg_fertilizer <- 0
	d$Zn_fertilizer <- 0
	d$B_fertilizer <- 0
	i <- d$treatment == "NPK+S+Ca+Mg+Zn+B"
	d$S_fertilizer[i] <- NA
	d$Ca_fertilizer[i] <- NA
	d$Mg_fertilizer[i] <- NA
	d$Zn_fertilizer[i] <- NA
	d$B_fertilizer[i] <- NA
	
	d <- d[!is.na(d$N_fertilizer),  ]
	
	
	d$planting_date <- as.character(NA)
	d$on_farm <- TRUE
	d$yield_part <- "grain"
	d$yield_part[d$crop %in% c("cowpea",  "faba bean",  "soybean")] <- "seed"
	d$SiteID <- NULL
	
	# removing records without coordinates from Kihara_Wkenya
	
	d$is_survey <- NA
	d$irrigated <- NA
	d <- unique(d)
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path)
}

