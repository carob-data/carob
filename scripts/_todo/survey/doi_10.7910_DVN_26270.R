# R script for "carob"
# license: GPL (>=3)

## NOTES
# NCAP/IRRI/OUAT survey "Socio-economic dynamics of rice production systems in eastern India", Orissa (Odisha) component: 200 farmers in 4 villages of Kalahandi and Mayurbhanj districts, 1999 and 2000.
# Farmers are numbered within district, village and farm-size class (Type); hhid = district-village-type-farmer.
# Land use (Landuse.XLS, LandUse): one row per year x season x parcel x plot; crop, variety, area (ha), establishment method, production (qtl). yield = Prod * 100 / Area (kg/ha).
# Parcel topography, soil, irrigation source and tenure from Landhold.XLS (LandDescrip) merged by year, farmer and parcel.
# Fertilizer from RiceInp.xls (Material), for selected rice plots: up to 4 products, all in kg (unit code 1). Grades from the code description: urea 46% N, Super 16% P, DAP 18-46-0, potash (MOP) 58% K, Grommer 10-26-26, Shymala 28-28-0, gypsum. "P" and "K" were taken as P2O5 and K2O; P = P2O5/2.29, K = K2O/1.2051. FYM (kg) mapped to OM_amount.
# Villages and blocks from the code description.

## ISSUES
# No coordinates; georeference villages Sergad (Narla block) and Pasticudi (Kesinga block) in Kalahandi; Masinakati (Betonati block) and Tulasichaura (Suliapada block) in Mayurbhanj.
# No planting or harvest dates; only the survey year (1999, 2000) is used as planting_date.
# Variety codes 1000 and 2000 are "unknown modern/traditional variety" (variety NA). Some variety entries are names ("Asu", "Bhutia") instead of codes; these are kept.
# Fertilizer data only for the selected rice plots in RiceInp (804 of 1713 rice records).
# FYM quantities (header says kg, unit code 1 = kg) are very low (median 15 kg/ha); the unit may be qtl or cartloads. Check before use.
# Crop missing for 36 land use records (probably fallow); kept as NA.
# write_files: invalid crops "horse gram", "sabai grass", "vegetables"; fertilizer type "10-26-26"; yield_part "bulbs" (onion); NA yield for 2 records.
# Not processed: access, assets, changes, constraints, credit, crop changes, demography, disposal, employment, feeds, income, migration, non-rice inputs, labor, rice features/preferences, rice production changes.

## NEW VARIABLES
# farm_size_class_: sampling stratum (marginal, small, medium, large)
# topography_: parcel topography (upland bunded, upland unbunded, medium, lowland, very lowland)
# land_quality_: land quality class of the Revenue Department (very good to very poor)


carob_script <- function(path) {

"
Socio-economic dynamics of rice production system in inland areas of Orissa

The objectives of the project are: (1) to analyze the spatial patterns of rice productivity growth in the context of broader changes taking place in agriculture; (2) to analyze the patterns of rice technology adoption, constraints to adoption and importance of rice in rural livelihoods; and (3) to draw implications for technology design and policy reforms needed to accelerate rice productivity growth.
"

	uri <- "doi:10.7910/DVN/26270"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 40,
		carob_effort = 0.5
	)

	f1 <- ff[basename(ff) == "Landuse.XLS"]
	f2 <- ff[basename(ff) == "Landhold.xls"]
	f3 <- ff[basename(ff) == "RiceInp.xls"]
	r1 <- carobiner::read.excel(f1, sheet="LandUse", skip=7, col_types="text")
	r2 <- carobiner::read.excel(f2, sheet="LandDescrip", skip=5)
	r3 <- carobiner::read.excel(f3, sheet="Material", skip=5)

	num <- c("District", "Year", "Village", "Type", "Farmer", "Season", "Parcel", "Plot", "Crop", "Cropping", "MVTV", "Area", "CEMethod", "Prod", "Byprod")
	r1[num] <- lapply(r1[num], as.numeric)

	vars <- c("1"="Swarna", "2"="Parijat", "3"="Hira", "4"="Lalat", "5"="Rudra", "6"="Culture-1", "7"="Jhau", "8"="Utkal Prava", "9"="Pathara", "10"="Khandagiri", "11"="No.51", "12"="Sarada", "13"="Moti", "14"="Annapurna", "15"="Gitanjali", "16"="Mugai", "17"="Patharasala", "18"="CR 1030", "19"="Kalinga", 
		"1001"="Chetka", "1002"="Mugudhi", "1003"="Gurian", "1004"="Sarian", "1005"="Mahipal", "1006"="Kusuma", "1007"="Karni", "1008"="Badamahipal", "1009"="Assamchudi", "1010"="Burei", "1011"="Malpatri", "1012"="Biramani", "1013"="Locha", "1014"="Sela", "1015"="Gelei", "1016"="Jabda", "1017"="Jangalijata", "1018"="Jatakalam", "1019"="Burian", "1020"="Jhari", "1021"="Kankani", "1022"="Selaburi", "1023"="Puagi", "1024"="Hiran", "1025"="Bundei", "1026"="Kharkal", "1027"="Kainch", "1028"="Paniruhi", "1029"="Bhutia", "1030"="Asu", "1031"="Kakhuria", "1032"="Jaladhan", "1033"="Goda", "1034"="Jiramani", "1035"="Rangi", "1036"="Raishari", "1037"="Raichini", "1038"="Khakra", "1039"="Lal", "1040"="Maljhinga", "1041"="Dani", "1042"="Bhuska", "1043"="Kalia", "1044"="Karanga", "1045"="Baigan Bhija", "1046"="Nardi", "1047"="Sunga", "1048"="Katamudi", "1049"="Agnisal", "1050"="Kerakhandi", "1051"="Khajur Jhumpa", "1052"="Saljhati", "1053"="Kulichor", "1054"="Jhinga", "1055"="Kalama", "1056"="Baidyaraj", "1057"="Putiashakar", "1058"="Nilamani", "1059"="Karlabhuta", "1060"="Ganjeikali", "1061"="Bhajana", "1062"="Uddkurkur", "1063"="Chaina", "1064"="Harishankar", "1065"="Bidera")
	crops <- c("rice", "mung bean", "horse gram", "vegetables", "wheat", "chickpea", "grass pea", "cotton", "black gram", "pigeon pea", "sugarcane", "groundnut", "sabai grass", "bamboo", "onion", "finger millet", "pea", "castor", "mustard", "chili pepper", "vegetables", "eggplant", "sesame")
	villages <- c("Sergad", "Pasticudi", "Masinakati", "Tulasichaura")
	blocks <- c("Narla", "Kesinga", "Betonati", "Suliapada")

	d <- data.frame(
		district = r1$District,
		year = r1$Year,
		village = r1$Village,
		type = r1$Type,
		farmer = r1$Farmer,
		season = c("pre-kharif", "kharif", "rabi", "zaid")[r1$Season],
		parcel = r1$Parcel,
		plot = r1$Plot,
		crop = crops[r1$Crop],
		intercropped = r1$Cropping == 2,
		field_size = r1$Area,
		planting_method = c("direct seeding", "direct seeding", "transplanting")[r1$CEMethod],
		establishment_method_ = c("DSR", "WSR", "TPR")[r1$CEMethod],
		yield = r1$Prod * 100 / r1$Area,
		fwy_residue = r1$Byprod * 100 / r1$Area
	)
	isrice <- d$crop %in% "rice"
	v <- r1$Variety
	i <- v %in% names(vars)
	v[i] <- vars[v[i]]
	v[v %in% c("1000", "2000")] <- NA
	d$variety <- ifelse(isrice, v, NA)
	d$variety_type <- c("modern", "traditional")[r1$MVTV]

	## parcels
	lh <- data.frame(
		district = r2$District, year = r2$Year, village = r2$Village, type = r2$Type, farmer = r2$Farmer, parcel = r2$Parcel,
		topography_ = c("upland (bunded)", "upland (unbunded)", "medium", "lowland", "very lowland")[r2$Topog],
		soil_texture = c("clay", "clay loam", "sandy clay loam", "sandy loam", "loamy sand", "loam", "sand")[r2$Soiltype],
		land_quality_ = c("very good", "good", "average", "poor", "very poor")[r2$LandQlty],
		irrigation_source = c("canal", "deep tube well", "shallow tube well", "none", "cross bund", "dug well")[r2$IrrigSrc],
		land_tenure = c("owned", "sharecropped", "rented", "rented out")[r2$Tenure]
	)
	lh$irrigated <- !(lh$irrigation_source %in% "none")
	lh$irrigated[is.na(lh$irrigation_source)] <- NA
	pkey <- c("district", "year", "village", "type", "farmer", "parcel")
	lh <- lh[!duplicated(lh[, pkey]), ]
	d <- merge(d, lh, by=pkey, all.x=TRUE)

	## fertilizer
	grades <- data.frame(
		code = c(1:6, 10),
		name = c("urea", "SSP", "DAP", "KCl", "10-26-26", "28-28-0", "gypsum"),
		N = c(46, 0, 18, 0, 10, 28, 0),
		P = c(0, 16, 46, 0, 26, 28, 0),
		K = c(0, 0, 0, 58, 26, 0, 0)
	)
	r3$rec <- 1:nrow(r3)
	fl <- do.call(rbind, lapply(1:4, \(j) data.frame(rec=r3$rec, code=r3[[paste0("Fert", j, "Type")]], qty=r3[[paste0("Fert", j, "Qty")]])))
	fl <- fl[!is.na(fl$code) & !is.na(fl$qty) & fl$qty > 0, ]
	fl <- merge(fl, grades, by="code", all.x=TRUE)
	fl$N <- fl$qty * fl$N / 100
	fl$P <- fl$qty * fl$P / 100 / 2.29
	fl$K <- fl$qty * fl$K / 100 / 1.2051
	fn <- aggregate(fl[, c("N", "P", "K")], fl["rec"], sum)
	ft <- aggregate(fl["name"], fl["rec"], \(x) paste(unique(x), collapse="; "))
	fert <- data.frame(
		rec = r3$rec, district = r3$District, year = r3$Year, village = r3$Village, type = r3$Type, farmer = r3$Farmer,
		season = c("pre-kharif", "kharif", "rabi", "zaid")[r3$Season], parcel = r3$Parcel, plot = r3$Plot,
		ha = r3$Area, seed_rate = r3$SeedQty / r3$Area, OM_amount = ifelse(is.na(r3$FYMQUA), 0, r3$FYMQUA) / r3$Area
	)
	fert <- merge(fert, merge(fn, ft, by="rec"), by="rec", all.x=TRUE)
	nofert <- !fert$rec %in% fl$rec
	fert[nofert, c("N", "P", "K")] <- 0
	fert$name[nofert] <- "none"
	fert$N_fertilizer <- fert$N / fert$ha
	fert$P_fertilizer <- fert$P / fert$ha
	fert$K_fertilizer <- fert$K / fert$ha
	fert$fertilizer_type <- fert$name
	fert$OM_type <- ifelse(fert$OM_amount > 0, "farmyard manure", "none")
	fert$rec <- fert$ha <- fert$N <- fert$P <- fert$K <- fert$name <- NULL
	fkey <- c(pkey, "season", "plot")
	fert <- fert[!duplicated(fert[, fkey]), ]
	fert$crop <- "rice"
	d <- merge(d, fert, by=c(fkey, "crop"), all.x=TRUE)

	d$hhid <- paste(d$district, d$village, d$type, d$farmer, sep="-")
	d$farm_size_class_ <- c("marginal", "small", "medium", "large")[d$type]
	d$adm2 <- c("Kalahandi", "Mayurbhanj")[d$district]
	d$adm3 <- blocks[d$village]
	d$location <- villages[d$village]
	d$plot_id <- paste(d$parcel, d$plot, sep="-")
	d$trial_id <- paste(d$hhid, d$year, sep="-")
	d$planting_date <- as.character(d$year)
	d$district <- d$village <- d$type <- d$farmer <- d$parcel <- d$plot <- d$year <- NULL

	d$country <- "India"
	d$adm1 <- "Odisha"
	d$currency <- "INR"
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$yield_part <- "grain"
	d$yield_part[d$crop %in% c("mung bean", "horse gram", "chickpea", "grass pea", "black gram", "pigeon pea", "pea", "castor", "mustard", "sesame")] <- "seed"
	d$yield_part[d$crop %in% c("vegetables")] <- "leaves"
	d$yield_part[d$crop %in% c("chili pepper", "eggplant")] <- "fruit"
	d$yield_part[d$crop %in% c("sugarcane", "bamboo", "sabai grass")] <- "stems"
	d$yield_part[d$crop == "cotton"] <- "lint"
	d$yield_part[d$crop == "onion"] <- "bulbs"
	d$yield_part[d$crop == "groundnut"] <- "pod"

	carobiner::write_files(path, meta, d)
}
