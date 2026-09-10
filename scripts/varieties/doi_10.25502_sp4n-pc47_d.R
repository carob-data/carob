# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Validation trial of improved yam varieties in Nigeria in 2017

The average yam yields of local varieties is less than 25% of the yield of improved released varieties, which range from 30 to 40 tons/ha. Absence of a formal seed system has not encouraged the promotion and adoption of released improved yam varieties. To sensitize farmers on the superiority of improved varieties, seed tubers of selected released varietiesproduced by IITA were planted in comparison with local checks in 80 on-farm validation/demonstration trials in six states of Nigeria - Enugu, Benue, Nasarawa, Federal Capital Territory (FCT), Niger and Oyo. Three improved yam varieties [two Dioscorea rotundata (TDr 89/02665 and TDr 95/19177) and one D. alata (TDa 98/01176)], and one location specific farmers’ best variety were used to quantify the superiority of the improved varieties over the locals.
"

	uri <- "doi:10.25502/sp4n-pc47/d"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-09-09",
		carob_completion = 100,	
		carob_effort = 3
	)
	

	f1 <- ff[basename(ff) == "seed-company-2017-data.csv"]
	#f2 <- ff[basename(ff) == "metadata_seed_company_2017.csv"]

	r1 <- read.csv(f1)
	#r2 <- read.csv(f2)

#####
	d <- data.frame(
		adm1 = carobiner::fix_name(r1$State, "title"),
		location = r1$Village,
		variety = r1$Variety,
		yield = r1$Yield_t_ha*1000,
		farmer_gender = c("male", "female")[r1$Gender],
		plot_area = r1$VarPlotSize,
		Nema_incid = as.integer(r1$NemaIncid),
		Nema_sev = as.character(r1$NemaSev),
		Mb_incid = as.integer(r1$MBIncid),
		Mb_sev = as.character(r1$MBSev),
		inid_scale = as.character(r1$ScaleIncid),
		sev_scale = as.character(r1$ScaleSev),
		Beet_incid = as.integer(r1$BeetleIncid),
		Beet_sev = as.character(r1$BeetleSev),
		record_id = seq_len(nrow(r1)),
		crop = "yam",
		country = "Nigeria",
		planting_date = "2017", 
		harvest_date = NA,
		trial_id = paste(r1$LGA, r1$Village, sep = "-")
	)


	### Adding lon and lat coordinate
	
	geo <- data.frame(
	  location = c("Rafin Zurfi", "Angwar Dodo", "Shazhi", "Yaba", "Yanbabu", "Kpaduma", "Guto", "Yaupe", "Dorowa", "Sabongida", "Angida Gida", "Angwaan Kadaura", "Akunza Maralaba", "Obi", "Tundun Adabu", "Doma", "Kadarko", "Sarkin Loma", "Bakinrijiya", "Adoyi", "Nassarawa Eggon", "Agunji", "Kpanga", "Egbanasara", "Maali", "Paiko Lugodan", "Popoi", "Badna", "Kurmin Sheyi", "Gabi", "Ganamadi", "Cheche", "Nami", "Boku", "Lambata/Gaiji", "Ugba", "Adum East", "Ochodu Upka", "Lube", "Igboho", "Yemere", "Oloje", "Tokunbo", "Kanko", "Agunrege", "Araromi", "NgeneUgbo", "Ugbo", "EziokaMgbowo"),
	  longitude = c(9.7856, 6.568, 7.7081, 3.371, 7.402, 7.5329, 6.534, 4.463, 7.855, 9.714, 8.5176, 8.4763, 7.6068, 7.557, 8.724, 8.358,  8.575, 7.1105, 8.571, 7.591, 8.543, 8.318, 4.644, 6.20005, 4.4927, 6.635, 6.366, 7.6028, 7.514, 6.633, 6.6059, 6.606, 6.548, 6.4841, 6.4730, 9.350,  8.334, 8.47185, 3.3119, 3.757, 6.6915, 4.506, 3.3485, 6.0138, 3.393, 4.489, 7.4674, 7.46907, 7.4561),
	  latitude = c(10.259, 11.288, 8.9160, 6.509, 10.512, 9.0129, 9.492, 8.4597, 8.7846, 8.735, 8.5142, 8.2858, 9.0286, 9.0067, 8.403, 8.4009, 8.229, 10.0156, 8.532, 9.0501, 8.743, 8.680, 10.2641, 9.258, 12.654, 9.4349, 9.6357, 8.927, 6.447, 9.098, 9.0367, 9.0365, 9.1478, 9.040, 9.0825, 7.506, 6.995, 6.925, 6.509, 8.834, 8.9152, 8.507, 6.597, 9.573, 8.397, 6.6019, 6.1637, 6.1627, 6.1248),
	  geo_uncertainty = NA,
	  geo_source = "Google Maps"
	) 
	
	d <- merge(d, geo, by = "location", all.x = TRUE)
	
	### For records where the location is unknown, we fill in the longitude and latitude using the corresponding adm1.
	
	geo1 <- data.frame(
	  adm1 = c("Benue", "Enugu", "Niger","Oyo",  "Fct", "Nassarawa"),
	  lon = c(8.7581, 7.4210, 5.6012, 3.6188, 7.1417, 8.1873),
	  lat = c(7.3244, 6.5341, 9.9360, 8.1437, 8.8614, 8.4989),
	  geo_unc = c(143060, 73973, 245255, 124749, 72818, 158136),
	  geo_s = "GADM 4.1, adm1",
	  geo_from_source = FALSE
	  
	)
	
	d <- merge(d, geo1, by = "adm1", all.x = TRUE)
	
	i <- is.na(d$longitude)|is.na(d$latitude)
	d$longitude[i] <- d$lon[i]
	d$latitude[i] <- d$lat[i]
	d$geo_uncertainty[i] <- d$geo_unc[i]
	d$geo_source[i] <- d$geo_s[i]
	
	d$geo_s <- d$geo_unc <- d$lon <- d$lat <- NULL
	
	### 
	Nm <- names(d)[grepl("Nema|Mb|scale|Beet", names(d))]
	long <- d[, Nm]
	long$record_id <- d$record_id
	
	long <- reshape(long, varying = list(c("Nema_incid", "Mb_incid", "Beet_incid", "inid_scale"), c("Nema_sev", "Mb_sev", "Beet_sev", "sev_scale")), 
	                v.names = c("pest_incidence", "pest_severity"),
	                timevar = "pest_species",
	                times = c("nematode", "mealybug", "beetle", "scale insect"),
	                direction = "long")
	long$id <- NULL
	long <- long[!is.na(long$pest_severity),]
	row.names(long) <- NULL
	
	col <- grep("Nema|Mb|scale|Beet", names(d))
	d <- d[, -col]
	
	d$is_survey <- FALSE
	d$on_farm <- TRUE
	d$yield_moisture <- NA
	d$yield_isfresh <- TRUE
	d$yield_part <- "tubers"
	d$irrigated <- NA
	d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA)
	
	
	carobiner::write_files(path, meta, d, long = long)
}


