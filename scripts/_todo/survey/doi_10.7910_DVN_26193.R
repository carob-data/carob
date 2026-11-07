# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI / Bogor Agricultural University survey of upland rice farmers (2002), districts Sukabumi, Tasikmalaya, Garut (West Java) and Lampung Selatan (Lampung), Indonesia; 220 farmers.
# Two production tables:
#  1. "Upland Rice Production.xls": up to 2 upland fields x 2 seasons x up to 5 crops (mixed cropping). Reshaped to one row per farmer x field x season x crop.
#  2. "Wetland.xls": up to 4 wetland (sawah) rice parcels x 3 seasons. One row per farmer x parcel x season.
# Areas are m2 (header) and converted to ha; yield = Prod (kg) / area. For mixed crops each crop's yield is on the full field area (intercropped = TRUE when the field has more than one crop).
# Seasons are kept as "first", "second" (and "third" for wetland); the planting/harvest months of season 1 and 2 of the first upland field (CP1Plant etc.) are not mapped.
# Fertilizer for the upland field selected for the input module ("Agric input of Upland Rice.xls", per season) is merged to the rice rows of that field and season by farmer, parcel and season.
# Nutrient contents from the code description: urea 46% N, TSP 38%, KCl 48%, NPK 15-15-15; TSP and KCl taken as % P2O5 and % K2O; P = P2O5/2.29, K = K2O/1.2051.

## ISSUES
# Kecamatan coordinates are GADM point-radius centroids. Panengahan corrected to Penengahan. Rows with no subdistrict code use the kabupaten centroid.
# Cidaun is a kecamatan of Cianjur in GADM, not of Garut, so those rows are left without coordinates. Rows with no district code are also left without coordinates. Village codes are only partly documented; not mapped.
# TSP 38% and KCl 48% are unusual grades (SP-36 is 36% P2O5; KCl is ~60% K2O); verify the nutrient contents.
# Wetland input use (variety, urea/TSP/KCl/NPK, manure, pesticide, columns 86-119) refers to one parcel without season; not merged. Wetland columns 120-189 have no names in the header (labor?); not processed.
# Upland fertilizer is only for the field selected for the input module; other fields have NA N/P/K.
# "red bean" (kidney bean? adzuki?) mapped to "common bean"; "leaf for silkworm" to mulberry; "vegetalbe-hiris" (pigeon pea? "hiris" is Sundanese for pigeon pea) kept as given; check.
# Production disposal (seed kept, consumed, payment, sold, price) not processed except price.
# Not processed: Farmer Profile (age, education, household size), Household consumption, Land ownership, Other Assets, Others Income, Others.
# Wetland.xls contains a note in Area1 ("For farmer 52, the land is just bought, so still not produce"); set to NA.
# Upland fertilizer matched to 146 of 241 upland rice rows (the input module covers one field per farmer).
# write_files: invalid term season "third" (wetland third season), invalid crop "vegetalbe-hiris", unknown variable land_type_; NA detected: latitude, longitude (Cidaun, and rows with no district code).

## NEW VARIABLES
# land_type_: upland or wetland (sawah) field



## NOT PROCESSED
# Household Consumption.xls: not read. sheet Household consumption: Filename:Household consumption. Observation records are missing from d until this file is standardized.
# Project details.xlsx: not read. sheet PROJECT: Title of the Study, ...2, Economics of upland rice production in Indonesia. Observation records are missing from d until this file is standardized.

carob_script <- function(path) {

"
Economics of Upland Rice Production in Indonesia

The objective of the study is to review government policies on upland rice, evaluate the relationship between upland rice activities and poverty level, and identify scenarios to improve the role of upland areas in rice production in Indonesia.
"

	uri <- "doi:10.7910/DVN/26193"
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
		carob_completion = 30,
		carob_effort = 0.6
	)

	f1 <- ff[basename(ff) == "Upland Rice Production.xls"]
	f2 <- ff[basename(ff) == "Wetland.xls"]
	f3 <- ff[basename(ff) == "Agric input of Upland Rice.xls"]
	r1 <- carobiner::read.excel(f1, sheet="Upland rice production", skip=5)
	r2 <- carobiner::read.excel(f2, sheet="Wetland", skip=9)
	r3 <- carobiner::read.excel(f3, sheet="Agric input of upland rice", skip=4)

	districts <- c("1"="Sukabumi", "2"="Tasikmalaya", "3"="Garut", "5"="Lampung Selatan")
	subdistricts <- c("1"="Jampang Tengah", "2"="Cibadak", "3"="Kadipaten", "4"="Salawu", "5"="Malangbong", "6"="Cidaun", "9"="Panengahan")
	crops <- c("upland rice"="rice", "chili"="chili pepper", "sweet potato"="sweetpotato", "peanut"="groundnut",
		"long bean"="asparagus bean", "red bean"="common bean", "leaf for silkworm"="mulberry")

	## upland fields: field x season x crop slots
	slots <- data.frame(
		field = c(1, 1, 2, 2),
		season = c(1, 2, 1, 2),
		parcel = c("Parcel1", "Parcel1", "Parcel 2", "Parcel 2"),
		area = c("Area...15", "Area...53", "Area2...92", "Area2...123"),
		comb = c("Crop11", "Crop12", "Crop 21", "Crop22"),
		ncrop = c(5, 5, 4, 4)
	)
	d1 <- lapply(1:nrow(slots), function(i) {
		s <- slots[i, ]
		lapply(1:s$ncrop, function(j) {
			p <- paste0(s$field, s$season, j)
			data.frame(
				farmer = r1$Farmer,
				subdistrict = r1$Subdistrict,
				district = r1$District,
				field_id = as.character(r1[[s$parcel]]),
				season = c("first", "second")[s$season],
				field_size = r1[[s$area]] / 10000,
				intercrops = r1[[s$comb]],
				crop = r1[[paste0("Crop", p)]],
				yield = r1[[paste0("Prod", p)]] / (r1[[s$area]] / 10000),
				crop_price = r1[[paste0("Price", p)]],
				land_type_ = "upland"
			)
		})
	})
	d1 <- do.call(rbind, unlist(d1, recursive=FALSE))
	d1 <- d1[!is.na(d1$crop), ]
	d1$intercropped <- grepl(",", d1$intercrops)
	d1$intercrops <- NULL

	## upland fertilizer (selected field, 2 seasons)
	ucols <- list(
		c("Season...4", "Parcel...5", "Area...6", "UreaQty...36", "TSPQty...40", "KClQty...44", "NPKQty...48", "ManQty...51", "Variety...9", "MV/TV"),
		c("Season...61", "Parcel...62", "Area...63", "UreaQty...93", "TSPQty...97", "KClQty...101", "NPKQty...105", "ManQty...108", "Variety...66", "MVTV")
	)
	fert <- lapply(ucols, function(v) {
		ha <- r3[[v[3]]] / 10000
		q <- lapply(v[4:8], function(x) { y <- r3[[x]]; y[is.na(y)] <- 0; y })
		data.frame(
			key = paste(r3$Farmer, r3[[v[2]]], c("first", "second")[r3[[v[1]]]]),
			N_fertilizer = (0.46 * q[[1]] + 0.15 * q[[4]]) / ha,
			P_fertilizer = (0.38 * q[[2]] + 0.15 * q[[4]]) / 2.29 / ha,
			K_fertilizer = (0.48 * q[[3]] + 0.15 * q[[4]]) / 1.2051 / ha,
			OM_amount = q[[5]] / ha,
			variety = r3[[v[9]]],
			variety_type = c(MV="modern", TV="traditional")[r3[[v[10]]]]
		)
	})
	fert <- do.call(rbind, fert)
	fert <- fert[!is.na(fert$N_fertilizer), ]
	d1$key <- paste(d1$farmer, d1$field_id, d1$season)
	d1$key[d1$crop != "upland rice"] <- NA
	d1 <- merge(d1, fert, by="key", all.x=TRUE)
	d1$key <- NULL

	## wetland rice parcels x 3 seasons
	r2$Area1[grepl("^NOTE", r2$Area1)] <- NA
	pcols <- c("Parcel 1", "Parcel 2", "Parcel3", "Parcel 4")
	d2 <- lapply(1:4, function(i) {
		lapply(1:3, function(j) {
			ha <- as.numeric(r2[[paste0("Area", i)]]) / 10000
			data.frame(
				farmer = r2$Farmer,
				subdistrict = r2$Subdistrict,
				district = r2$District,
				field_id = as.character(r2[[pcols[i]]]),
				season = c("first", "second", "third")[j],
				field_size = ha,
				crop = "rice",
				yield = r2[[paste0("Prod", i, j)]] / ha,
				crop_price = r2[[paste0("Price", i, j)]],
				land_type_ = "wetland",
				intercropped = FALSE
			)
		})
	})
	d2 <- do.call(rbind, unlist(d2, recursive=FALSE))
	d2 <- d2[!is.na(d2$yield), ]

	d <- carobiner::bindr(d1, d2)
	i <- d$crop %in% names(crops)
	d$crop[i] <- crops[d$crop[i]]
	d$hhid <- as.character(d$farmer)
	d$adm2 <- districts[as.character(d$district)]
	d$adm3 <- subdistricts[as.character(d$subdistrict)]
	d$adm1 <- ifelse(d$district == 5, "Lampung", "Jawa Barat")
	d$farmer <- d$district <- d$subdistrict <- NULL

	d$country <- "Indonesia"
	d$currency <- "IDR"
	d$trial_id <- d$hhid
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$irrigated <- NA
	d$yield_part <- "grain"
	d$yield_part[d$crop %in% c("cassava", "sweetpotato", "radish")] <- "roots"
	d$yield_part[d$crop %in% c("groundnut")] <- "pod"
	d$yield_part[d$crop %in% c("common bean", "asparagus bean")] <- "seed"
	d$yield_part[d$crop %in% c("banana", "chili pepper", "cucumber", "pumpkin", "watermelon")] <- "fruit"
	d$yield_part[d$crop %in% c("tea", "mulberry")] <- "leaves"

	d$adm3[which(d$adm3 == "Panengahan")] <- "Penengahan" # GADM spelling

	# Coordinates are the GADM point-radius centroid of the kecamatan.
	# Cibadak and Kadipaten also occur in other kabupaten, so the parent names stay in the merge key.
	geo <- data.frame(
		adm1 = c("Jawa Barat", "Jawa Barat", "Jawa Barat", "Jawa Barat", "Jawa Barat", "Lampung"),
		adm2 = c("Sukabumi", "Sukabumi", "Tasikmalaya", "Tasikmalaya", "Garut", "Lampung Selatan"),
		adm3 = c("Jampang Tengah", "Cibadak", "Kadipaten", "Salawu", "Malangbong", "Penengahan"),
		longitude = c(106.7904, 106.7629, 108.1161, 107.9842, 108.082, 105.6878),
		latitude = c(-7.0681, -6.8947, -7.1191, -7.3728, -7.0572, -5.7498),
		geo_uncertainty = c(12597, 8590, 5603, 9387, 8503, 8996),
		geo_source = "GADM 4.1, adm3"
	)
	d <- merge(d, geo, by=c("adm1", "adm2", "adm3"), all.x=TRUE, sort=FALSE)
	# Rows with no kecamatan name: centroid of the kabupaten. A named kecamatan that did not match is not filled.
	geo2 <- data.frame(
		adm1 = c("Jawa Barat", "Jawa Barat", "Jawa Barat", "Lampung"),
		adm2 = c("Garut", "Sukabumi", "Tasikmalaya", "Lampung Selatan"),
		longitude = c(107.7889, 106.7076, 108.1413, 105.5005),
		latitude = c(-7.3596, -7.0762, -7.4969, -5.5328),
		geo_uncertainty = c(53416, 47555, 50656, 70540),
		geo_source = "GADM 4.1, adm2"
	)
	names(geo2)[3:6] <- c("lon2", "lat2", "unc2", "src2")
	d <- merge(d, geo2, by=c("adm1", "adm2"), all.x=TRUE, sort=FALSE)
	fill <- is.na(d$longitude) & is.na(d$adm3) & !is.na(d$lon2)
	d$longitude[fill] <- d$lon2[fill]
	d$latitude[fill] <- d$lat2[fill]
	d$geo_uncertainty[fill] <- d$unc2[fill]
	d$geo_source[fill] <- d$src2[fill]
	d$lon2 <- d$lat2 <- d$unc2 <- d$src2 <- NULL
	d$geo_from_source <- ifelse(is.na(d$longitude), NA, FALSE)

	carobiner::write_files(path, meta, d)
}
