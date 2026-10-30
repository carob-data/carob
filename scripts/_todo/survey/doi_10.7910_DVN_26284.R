# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI/NAFRI benchmark survey (2002) of upland rice farmers in Pak Ou district, Louangphrabang, Laos: 8 villages, farmers grouped by well-being (well-off, average, poor).
# Farmers are numbered within village; hhid = village-farmer.
# Upland land use ("Landuse-Upland"): crops grown per year (1999-2002) and parcel, with crop area (Areapro = parcel area x crop area %), seed amount and production (kg, mostly only for recent years). yield = Prod / Areapro (kg/ha).
# Lowland land use ("Landuse-Lowland"): rice in wet/dry season 2001, area (ha) and production (kg); irrigation status from "Landhold-Lowland".
# Year is used as planting_date (upland rice is grown in the wet season of that year).

## ISSUES
# No coordinates; georeference villages Hatzoa, Houaylueng, Pakcheak, Latthahea, Houaytham, Houaykha, Houaythoum and Mokmueng (Pak Ou district, Louangphrabang).
# Production is only available for 2001 (193 of 764 records); 1999, 2000 and 2002 records have NA yield (Yieldnew = 0 for those; not used).
# Crop code 30 "Large ground" is unclear (large gourd?); kept as "large ground".
# write_files: unknown variables (NEW VARIABLES below), invalid crops "job's tears", "large ground", "paper mulberry"; NA yield.
# Upland crop codes include text entries "Fallow", "improfallow", "improve fallow", "Improffalow crop"; mapped to "none" with fallow_ = "fallow" or "improved fallow".
# Parcel numbers in Landuse-Upland are decimals like 4.1 (parcel 4, sub-plot 1?); kept as plot_id.
# Seed amounts in "trees" (perennials) not mapped to seed_rate.
# No fertilizer or other input quantities by plot (Cost-Crops has only total cash costs per household); not processed.
# Not processed: demography, upland/lowland landholding details, garden, plantation, other resources, livestock, livelihood matrix, changes, food security, costs, use of products, income, credit.
# Remarks fields in Lao script are in a legacy font encoding and unreadable.

## NEW VARIABLES
# wellbeing_: household well-being class (well-off, average, poor)
# land_type_: upland or lowland
# fallow_: fallow or improved fallow (for crop = "none")


carob_script <- function(path) {

"
Benchmark survey of upland rice farmers in Pak Ou district, Luang Prabang, Laos

The Integrated Upland Agriculture and Forestry Research Project (IUARP) is a multidisciplinary research project implemented by NAFRI. IUARP is designed to support the upland agricultural development strategy of the government of Laos. The major components of the strategy are to improve food security, alleviate poverty, and stabilize the environment. The project aims to collect farm-level data on household resource endowment, land use, resource allocation, and livelihood strategies for use in fine-tuning the research, delineating the target domain and establishing a benchmark for future monitoring of project impact.
"

	uri <- "doi:10.7910/DVN/26284"
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
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "Upland-1.xls"]
	r1 <- carobiner::read.excel(f, sheet="Landuse-Upland", skip=5)
	r2 <- carobiner::read.excel(f, sheet="Landuse-Lowland", skip=5)
	r3 <- carobiner::read.excel(f, sheet="Landhold-Lowland", skip=6)

	crops <- c("rice", "job's tears", "chili pepper", "eggplant", "sesame", "cassava", "maize", "sugarcane", "banana", "jackfruit", "mango", "taro", "tamarind", "pineapple", "lychee", "lemon", "custard apple", "avocado", "pomelo", "cantaloupe", "paper mulberry", "teak", "pumpkin", "onion", "coriander", "garlic", "lettuce", "common bean", "kale", "large ground", "sweetpotato", "groundnut", "rambutan", "longan", "papaya", "cotton", "rattan", "cabbage", "pigeon pea")
	villages <- c("Hatzoa", "Houaylueng", "Pakcheak", "Latthahea", "Houaytham", "Houaykha", "Houaythoum", "Mokmueng")
	wellbeing <- c(w="well-off", a="average", p="poor")

	cc <- suppressWarnings(as.integer(r1$Crop))
	d1 <- data.frame(
		village = r1$Village,
		farmer = r1$Farmer,
		wellbeing_ = wellbeing[tolower(r1$Wellbeing)],
		planting_date = as.character(r1$Year),
		plot_id = as.character(r1$Parcel),
		crop = crops[cc],
		intercropped = toupper(r1$Cropping) == "I",
		field_size = r1$Areapro,
		yield = ifelse(r1$ProdUnit %in% "kg", r1$Prod / r1$Areapro, NA),
		seed_rate = ifelse(r1$SeedUnit %in% "kg", r1$Seed / r1$Areapro, NA),
		land_type_ = "upland",
		irrigated = FALSE
	)
	fal <- grepl("fal", r1$Crop, ignore.case=TRUE)
	d1$crop[fal] <- "none"
	d1$fallow_ <- ifelse(fal, ifelse(grepl("^impro", r1$Crop, ignore.case=TRUE), "improved fallow", "fallow"), NA)
	d1 <- d1[!is.na(d1$crop), ]

	lh <- data.frame(village = r3$Village, farmer = r3$Farmer, parcel = r3$Parcel, 
		irrigated = r3$Irrig == "Irrigated", irrigation_source = tolower(r3$IrrSource))
	lh <- lh[!is.na(lh$parcel) & !duplicated(lh[, 1:3]), ]
	r2 <- r2[!is.na(r2$Crop), ]
	d2 <- data.frame(
		village = r2$Village,
		farmer = r2$Farmer,
		parcel = r2$Parcel,
		wellbeing_ = wellbeing[tolower(r2$Wellbeing)],
		season = c(WS="wet", DS="dry")[r2$Season],
		planting_date = as.character(r2$Year),
		plot_id = as.character(r2$Parcel),
		crop = crops[r2$Crop],
		field_size = r2$Areaha,
		yield = ifelse(r2$ProdUnit %in% "kg", r2$Prod / r2$Areaha, NA),
		land_type_ = "lowland"
	)
	d2 <- merge(d2, lh, by=c("village", "farmer", "parcel"), all.x=TRUE)
	d2$parcel <- NULL

	d <- carobiner::bindr(d1, d2)
	d$hhid <- paste(d$village, d$farmer, sep="-")
	d$location <- villages[d$village]
	d$village <- d$farmer <- NULL
	d$trial_id <- d$hhid

	d$country <- "Laos"
	d$adm1 <- "Louangphrabang"
	d$adm2 <- "Pak Ou"
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$yield_part <- "grain"
	d$yield_part[d$crop %in% c("sesame", "common bean", "pigeon pea", "cotton")] <- "seed"
	d$yield_part[d$crop %in% c("cassava", "sweetpotato", "taro")] <- "roots"
	d$yield_part[d$crop %in% c("chili pepper", "eggplant", "banana", "jackfruit", "mango", "tamarind", "pineapple", "lychee", "lemon", "custard apple", "avocado", "pomelo", "cantaloupe", "pumpkin", "rambutan", "longan", "papaya")] <- "fruit"
	d$yield_part[d$crop %in% c("lettuce", "kale", "cabbage", "coriander", "onion", "garlic")] <- "leaves"
	d$yield_part[d$crop %in% c("sugarcane", "paper mulberry", "rattan")] <- "stems"
	d$yield_part[d$crop == "teak"] <- "wood"
	d$yield_part[d$crop == "groundnut"] <- "pod"
	d$yield_part[d$crop == "none"] <- "none"

	carobiner::write_files(path, meta, d)
}
