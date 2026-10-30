# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI / Khon Kaen University farm survey on rice establishment methods (TPR, DSR, WSR), Khon Kaen, northeast Thailand; wet season 1998 and dry season 1998/99; rainfed (Grp 0) and irrigated (Grp 1) groups.
# Main records: "FormA.xls" sheet "LandUse": per farmer x parcel x plot x subplot, three crop slots: 1 = wet season 1998, 2 = dry season, 3 = second dry-season crop. Reshaped to one row per slot.
# Areas are in rai; converted to ha (6.25 rai = 1 ha, code description). yield = Prod (kg) / Areal (ha). Rice production is paddy.
# Dates: week numbers (CEDate, HarDate) with code description "Date of farm activities": week 1 = 1st week of Jan 1998, 4 weeks per month, 49-64 = Jan-Apr 1999. Converted to "yyyy-mm".
# Village codes (FormA "Village" sheet) decoded with the code description (village, tambon, district).
# Fertilizer from "FormB.XLS" sheet "Fert" (rice only; up to 3 products, kg per subplot; Area in rai), merged by group, season, farmer, parcel, plot, subplot. Grades from the code description, taken as N-P2O5-K2O; P = P2O5/2.29, K = K2O/1.2051.

## ISSUES
# No coordinates. Georeference the 6 villages (Ban Pumoonlbao, Ban Nongkaunchang in Tambon Non Tong, Nong Ruea district; Ban Sinemoon in Tambon Sinemoon, Nam Phong district; Khon Kaen). Check district/tambon spelling against GADM.
# Crop codes 1 and 2 are glutinous and non-glutinous rice; mapped to crop "rice" with variety_traits. Other crop codes from the "Crop Price" list.
# Fertilizer type 9 "Polymer" has unknown nutrient content: N/P/K set to NA for those records.
# FormB (inputs) covers only 242 rice subplots (all match land-use records); other rice records have NA N/P/K.
# One record has CEDate1 = -11 (CEMon1 = 10), giving planting_date 1997-10; check.
# write_files: missing coordinates; invalid crops "eucalyptus", "mango; tamarind"; fertilizer_type grades (16-20-0, 16-16-8, ...) and "polymer" not in terminag; NA yield for empty Prod.
# Crop slot 3 has no establishment method (CE3 all empty).
# Not processed: HHInfo (members), Origin, Migrate, LandHold (soil, topography, tenure, irrigation source per parcel), CEHist (1996-98 establishment history), Switch, Income, FormB Seedbed/Plow/Harrow/OtherLP/Cropest (seed rate, labor)/Insect/Fungi/Weed/Irrig/Harvest (labor and costs), FormC (decision process, perceptions).
# Publication: Pandey S, Velasco LE, Suphanchaimat N (2000) Economics of direct seeding in Northeast Thailand. Workshop on Direct Seeding in Asian Rice Systems, Bangkok (no DOI).

## NEW VARIABLES
# establishment_method_: rice establishment method (TPR, DSR = dry seeded, WSR = wet seeded)
# crop_slot_: crop slot in the land use form (1 = wet season, 2 = dry season, 3 = second dry-season crop)


carob_script <- function(path) {

"
Economics of Direct Seeding in Northeast Thailand

The objective of the study is to conduct an economic assessment of direct seeding practices in Khon Kaen, northeast Thailand. Specifically, the study aims to compare the economics of direct seeding methods with transplanting methods in rainfed and irrigated environments. It also aims to identify factors that determine adoption of alternative methods of rice establishment.
"

	uri <- "doi:10.7910/DVN/26171"
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
		carob_completion = 35,
		carob_effort = 0.5
	)

	fA <- ff[basename(ff) == "FormA.xls"]
	fB <- ff[basename(ff) == "FormB.XLS"]
	r1 <- carobiner::read.excel(fA, sheet="LandUse", skip=7)
	r2 <- carobiner::read.excel(fA, sheet="Village")
	r3 <- carobiner::read.excel(fB, sheet="Fert", skip=8)

	crops <- c("rice", "rice", "sugarcane", "cassava", "groundnut", "soybean", "maize", "sesame", "mango; tamarind",
		"banana", "rubber", "tomato", "melon", "flower", "mushrooms", "cucumber", "pasture", "mulberry", "eucalyptus",
		"lemon", "cabbage", "pomelo", "areca nut", "vegetable")
	varieties <- c("1"="KDML", "2"="Local Mali", "3"="Chainat 1", "4"="Chainat 6", "5"="Suphan 60", "6"="Suphan 90",
		"7"="Tor Chor Dor", "8"="Daeng", "11"="RD6", "12"="RD8", "13"="Mali light")
	villages <- data.frame(
		village = 1:6,
		location = c("Ban Pumoonlbao mu.4", "Ban Pumoonlbao mu.16", "Ban Nongkaunchang mu.6", "Ban Nongkaunchang mu.17",
			"Ban Sinemoon mu.1", "Ban Sinemoon mu.2"),
		adm3 = rep(c("Non Tong", "Sinemoon"), c(4, 2)),
		adm2 = rep(c("Nong Ruea", "Nam Phong"), c(4, 2))
	)
	rai <- 6.25
	# week number -> "yyyy-mm"
	wdate <- function(w) {
		m <- ((w - 1) %/% 4) %% 12 + 1
		y <- 1998 + (w - 1) %/% 48
		ifelse(is.na(w), NA, paste0(y, "-", formatC(m, width=2, flag="0")))
	}

	area <- list(r1$Areal, r1$Area2, r1$Area3)
	d <- lapply(1:3, function(i) {
		data.frame(
			grp = r1$Grp,
			farmer = r1$Farmer,
			parcel = r1$Parcel,
			plot = r1$Plot,
			subplot = r1$Subplot,
			crop_slot_ = i,
			season = c("wet", "dry", "dry")[i],
			field_size = area[[i]] / rai,
			crop_code = r1[[paste0("Crop", i)]],
			planting_date = wdate(r1[[paste0("CEDate", i)]]),
			harvest_date = wdate(r1[[paste0("HarDate", i)]]),
			yield = r1[[paste0("Prod", i)]] / (area[[i]] / rai),
			crop_price = r1[[paste0("Price", i)]],
			variety = varieties[as.character(r1[[paste0("Variety", i)]])],
			land_prep_times = as.integer(r1[[paste0("NPlow", i)]] + r1[[paste0("NHarrow", i)]]),
			establishment_method_ = c("TPR", "DSR", "WSR")[r1[[paste0("CE", i)]]]
		)
	})
	d <- do.call(rbind, d)
	d <- d[!is.na(d$crop_code), ]
	d$crop <- crops[d$crop_code]
	d$variety_traits <- c("glutinous", "non-glutinous")[d$crop_code]
	d$planting_method <- c(TPR="transplanting", DSR="direct seeding", WSR="direct seeding")[d$establishment_method_]
	d$irrigated <- d$grp == 1
	d$currency <- "THB"

	grade <- data.frame(
		code = c(1:8, 10:13),
		fertilizer_type = c("16-20-0", "16-16-8", "16-8-8", "urea", "NPK", "12-3-6", "12-36-0", "16-6-3", "20-0-0", "16-3-2", "18-12-6", "16-16-0"),
		N = c(16, 16, 16, 46, 15, 12, 12, 16, 20, 16, 18, 16),
		P = c(20, 16, 8, 0, 15, 3, 36, 6, 0, 3, 12, 16) / 2.29,
		K = c(0, 8, 8, 0, 15, 6, 0, 3, 0, 2, 6, 0) / 1.2051
	)
	r3$key <- paste(r3$Grp, r3$Season, r3$Farmer, r3$Parcel, r3$Plot, r3$Subplot)
	fl <- lapply(1:3, function(i) data.frame(key=r3$key, area=r3$Area / rai, code=r3[[paste0("FType", i)]], amount=r3[[paste0("FAmt", i)]]))
	fl <- do.call(rbind, fl)
	fl <- fl[!is.na(fl$code), ]
	fl <- merge(fl, grade, by="code", all.x=TRUE)
	fl$fertilizer_type[fl$code == 9] <- "polymer"
	fl$N <- fl$amount * fl$N / 100 / fl$area
	fl$P <- fl$amount * fl$P / 100 / fl$area
	fl$K <- fl$amount * fl$K / 100 / fl$area
	fert <- data.frame(
		key = unique(r3$key)
	)
	fert$N_fertilizer <- as.vector(tapply(fl$N, fl$key, sum)[fert$key])
	fert$P_fertilizer <- as.vector(tapply(fl$P, fl$key, sum)[fert$key])
	fert$K_fertilizer <- as.vector(tapply(fl$K, fl$key, sum)[fert$key])
	fert$fertilizer_type <- as.vector(tapply(fl$fertilizer_type, fl$key, function(x) paste(unique(x), collapse="; "))[fert$key])
	# Fert records without any product
	i <- is.na(fert$fertilizer_type)
	fert$fertilizer_type[i] <- "none"
	fert$N_fertilizer[i] <- fert$P_fertilizer[i] <- fert$K_fertilizer[i] <- 0

	d$key <- paste(d$grp, c(1, 2, 2)[d$crop_slot_], d$farmer, d$parcel, d$plot, d$subplot)
	d$key[d$crop != "rice"] <- NA
	d <- merge(d, fert, by="key", all.x=TRUE)

	d$village <- r2$Village[match(d$farmer, r2$Farmer)]
	d <- merge(d, villages, by="village", all.x=TRUE)
	d$hhid <- paste(d$grp, d$farmer, sep="-")
	d$field_id <- paste(d$parcel, d$plot, d$subplot, sep="-")
	d$key <- d$grp <- d$farmer <- d$parcel <- d$plot <- d$subplot <- d$crop_code <- d$village <- NULL

	d$country <- "Thailand"
	d$adm1 <- "Khon Kaen"
	d$trial_id <- d$hhid
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$yield_part <- "grain"
	d$yield_part[d$crop %in% c("sugarcane")] <- "stems"
	d$yield_part[d$crop %in% c("cassava")] <- "roots"
	d$yield_part[d$crop %in% c("groundnut")] <- "pod"
	d$yield_part[d$crop %in% c("soybean", "sesame")] <- "seed"
	d$yield_part[d$crop %in% c("mango; tamarind", "banana", "tomato", "melon", "cucumber", "lemon", "pomelo", "areca nut")] <- "fruit"
	d$yield_part[d$crop %in% c("cabbage", "mulberry", "pasture", "vegetable")] <- "leaves"
	d$yield_part[d$crop %in% c("eucalyptus")] <- "wood"

	carobiner::write_files(path, meta, d)
}
