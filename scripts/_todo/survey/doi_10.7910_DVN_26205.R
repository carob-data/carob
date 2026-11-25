# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI/BIDS "Livelihood Systems in Bangladesh" panel, 2004 round (62 villages; sam#20 links to the 2000 households).
# This dataset only contains the household file HHDataY2004 (5 parts). The Parcel and Cost&Return files described in the codebook are not included.
# HHDataY2004 parts 1-4 are Excel 2.1 (BIFF2) worksheets, read via carobiner::read.excel() BIFF2 fallback.
# Crop production is only available as household totals: part 3 has area (ha) and production (tons) per season (Aus, Aman, Boro) and crop group (MV rice, TV rice, jute, sugarcane, oilseeds, pulses, potato, onion, spices, vegetables, mixed, other).
# One row per household x season x crop group with area > 0; yield = production * 1000 / area. MV/TV = modern/traditional rice varieties (variety_type).
# Wheat is only given as an annual household total in part 2 (Whetland, WhetProd); added with season NA.
# zila = adm2 (spellings adjusted to GADM), upazila = adm3, union = adm4, village = location; names from the codebook "Genral_val" sheet.

## ISSUES
# Household-level aggregates only (field_size is the total area of that crop group in the season, not a plot). No varieties, inputs or dates.
# No coordinates; 62 villages (location) and upazila/zila need georeferencing; upazila/union spellings not checked against GADM.
# Crop groups "other", "mixed" (mixed rabi crops) have no single crop: crop NA (group in crop_label_). Oilseeds/pulses/spices/vegetables mapped to generic terms.
# Non-rice yields cluster on round constants (sugarcane 25 or 70 t/ha, vegetables 4 or 15 t/ha, oilseeds 0.4 t/ha, potato 10 t/ha): production was probably imputed as area x standard yield, not measured. Check before use.
# Sugarcane appears in Aus, Aman and Boro seasons (likely the same annual crop counted per season).
# Seasonal columns do not include wheat; wheat rows use the annual totals and have no season. Totals in part 2 (TVRLand, MVRLand, PotaLand, ...) duplicate part 3 and are not used.
# Unmapped: part 1 and 2 demography/education/labour, irrigated and rainfed area (irglnd, rfdlndo), land types, part 3 capital/livestock, part 4 housing/loans, part 5 income.
# write_files: no coordinates; season "aus" not in vocabulary; yield_part NA for jute, vegetables, onion, spices, mixed/other.

## NEW VARIABLES
# crop_label_: crop group label (e.g. "MV rice", "TV rice", "Oilseeds", "Mixed crop")
# hhid_2000_: household sample number in the 2000 round



## NOT PROCESSED
# HHDataY2004_part 4.xls: not read. could not read the header. Observation records are missing from d until this file is standardized.
# HHDataY2004_part 5.xls: not read. sheet HHDataY2004_part 5: Filename:    HHDataY2004_part 5. Observation records are missing from d until this file is standardized.
# Project details.xlsx: not read. sheet PROJECT: Title of the Study, ...2, Livelihood Systems in Bangladesh --for each year of study and the panel households created for all the years of study. Observation records are missing from d until this file is standardized.

carob_script <- function(path) {

"Livelihood Systems in Bangladesh (2004)

a)  Understand the operation of the Bangladesh rural household economy;  b) Analyze the drivers of change in income distribution and poverty;  c)  Prepare technical background papers to facilitating policy dialogues under the project 'Poverty Elimination Through Rice Research Assistance (PETRRA)' in Bangladesh;  d) Assess the impact of rice research on poverty reduction in Bangladesh using the Sustainable Livelihood Framework; and  e)  Estimating parameters on the determinants of rural household incomes for use in poverty mapping for Bangladesh."

	uri <- "doi:10.7910/DVN/26205"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = "PETRRA",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 30,
		carob_effort = 0.6
	)

	f1 <- ff[basename(ff) == "Documentation of 2004_Bangladesh.xls"]
	# value labels from the codebook: variable name (filled down), code, label
	codes <- function(sheet, var) {
		r <- carobiner::read.excel(f1, sheet=sheet, col_names=FALSE)
		nm <- r[[1]]
		nm <- nm[!is.na(nm)][cumsum(!is.na(nm))]
		k <- nm %in% var & !is.na(r[[4]])
		data.frame(code=as.numeric(r[[2]][k]), label=trimws(r[[4]][k]))
	}
	lab <- function(x, cds) {
		cds$label[match(x, cds$code)]
	}

	r1 <- carobiner::read.excel(ff[basename(ff) == "HHDataY2004_part 1.xls"])
	r2 <- carobiner::read.excel(ff[basename(ff) == "HHDataY2004_part 2.xls"])
	r3 <- carobiner::read.excel(ff[basename(ff) == "HHDataY2004_part 3.xls"])
	# column names differ in case (e.g. JutAsprd, SgAsLnd)
	names(r2) <- tolower(names(r2))
	names(r3) <- tolower(names(r3))

	# season x crop group columns of part 3 (codebook HHData_Var); "wheat" from the annual totals in part 2
	spec <- data.frame(
		prefix = c("mvas", "tvas", "jutas", "sgas", "osas", "vgas", "otas",
			"mvam", "tvam", "sgam", "plam", "spam", "vgam", "otam",
			"mvbr", "tvbr", "sgbr", "osbr", "plbr", "ptbr", "onbr", "spbr", "vgbr", "mxbr", "otbr"),
		season = rep(c("early wet", "aman", "boro"), c(7, 7, 11)),
		label = c("MV rice", "TV rice", "Jute", "Sugarcane", "Oilseeds", "Vegetable", "Other crop",
			"MV rice", "TV rice", "Sugarcane", "Pulses", "Spices", "Vegetable", "Other crop",
			"MV rice", "TV rice", "Sugarcane", "Oilseeds", "Pulses", "Potato", "Onion", "Spices", "Vegetable", "Mixed crop", "Other crop")
	)
	crops <- data.frame(
		label = c("MV rice", "TV rice", "Jute", "Sugarcane", "Oilseeds", "Pulses", "Potato", "Onion", "Spices", "Vegetable", "Mixed crop", "Other crop", "Wheat"),
		crop = c("rice", "rice", "jute", "sugarcane", "oilseed", "pulse", "potato", "onion", "spices", "vegetable", NA, NA, "wheat"),
		variety_type = c("modern", "traditional", rep(NA, 11)),
		yield_part = c("grain", "grain", NA, "stems", "seed", "seed", "tubers", NA, NA, NA, NA, NA, "grain")
	)

	d <- lapply(1:nrow(spec), function(i) {
		data.frame(
			sample = r3$sample,
			season = spec$season[i],
			crop_label_ = spec$label[i],
			field_size = r3[[paste0(spec$prefix[i], "lnd")]],
			production = r3[[paste0(spec$prefix[i], "prd")]]
		)
	})
	d <- do.call(rbind, d)
	dw <- data.frame(
		sample = r2$sample,
		season = NA,
		crop_label_ = "Wheat",
		field_size = r2$whetland,
		production = r2$whetprod
	)
	d <- rbind(d, dw)
	d <- d[!is.na(d$field_size) & d$field_size > 0, ]

	i <- match(d$crop_label_, crops$label)
	d$crop <- crops$crop[i]
	d$variety_type <- crops$variety_type[i]
	d$yield_part <- crops$yield_part[i]
	# production in tons, area in ha
	d$yield <- d$production * 1000 / d$field_size

	i <- match(d$sample, r1$sample)
	d$hhid <- as.character(d$sample)
	d$hhid_2000_ <- as.character(r1[["sam#20"]][i])
	d$hh_size <- r1$hhsize[i]
	d$farmland_owned <- r1$ownland[i]
	d$cropland <- r1$clvt[i]
	d$location <- lab(r1$village[i], codes("Genral_val", "village"))
	d$adm4 <- lab(r1$union[i], codes("Genral_val", "union"))
	d$adm3 <- lab(r1$upazila[i], codes("Genral_val", "upazila"))
	d$adm2 <- lab(r1$zila[i], codes("Genral_val", "zila"))
	d$adm2[is.na(d$adm2) & r1$zila[i] == 2] <- "Barisal"
	gadm <- c("B. Baria"="Brahamanbaria", "B.Baria"="Brahamanbaria", "Mouvi-Bazar"="Maulvibazar", "Moulovi-Bazar"="Maulvibazar", "Lakhmipur"="Lakshmipur", "Jhalakhati"="Jhalokati", "Perojpur"="Pirojpur", "Jhenaidaha"="Jhenaidah", "Kustia"="Kushtia", "Nababganj"="Nawabganj", "Panchaghar"="Panchagarh", "Netrokona"="Netrakona", "Munsiganj"="Munshiganj")
	i <- d$adm2 %in% names(gadm)
	d$adm2[i] <- gadm[d$adm2[i]]
	d$country <- "Bangladesh"
	geo <- data.frame(
		adm2 = c("Barisal", "Bhola", "Jhalokati", "Patuakhali", "Pirojpur", "Brahamanbaria", "Chandpur", "Chittagong", "Comilla", "Cox's Bazar", "Feni", "Lakshmipur", "Noakhali", "Habiganj", "Maulvibazar", "Sunamganj", "Sylhet", "Dhaka", "Faridpur", "Gopalganj", "Jamalpur", "Kishoreganj", "Madaripur", "Manikganj", "Munshiganj", "Mymensingh", "Narsingdi", "Netrakona", "Rajbari", "Shariatpur", "Sherpur", "Tangail", "Bagerhat", "Chuadanga", "Jessore", "Jhenaidah", "Khulna", "Kushtia", "Magura", "Meherpur", "Narail", "Satkhira", "Bogra", "Dinajpur", "Gaibandha", "Joypurhat", "Kurigram", "Lalmonirhat", "Naogaon", "Natore", "Nawabganj", "Nilphamari", "Pabna", "Panchagarh", "Rajshahi", "Rangpur", "Sirajganj", "Thakurgaon"),
		latitude = c(22.70, 22.69, 22.64, 22.36, 22.58, 23.96, 23.23, 22.36, 23.46, 21.43, 23.02, 22.94, 22.87, 24.38, 24.48, 25.07, 24.90, 23.81, 23.60, 23.01, 24.92, 24.44, 23.16, 23.86, 23.54, 24.75, 23.93, 24.88, 23.76, 23.24, 25.02, 24.25, 22.65, 23.64, 23.17, 23.54, 22.82, 23.90, 23.49, 23.76, 23.17, 22.72, 24.85, 25.63, 25.33, 25.10, 25.81, 25.92, 24.81, 24.42, 24.60, 25.93, 24.01, 26.34, 24.37, 25.75, 24.45, 26.03),
		longitude = c(90.37, 90.65, 90.20, 90.33, 89.97, 91.11, 90.66, 91.81, 91.18, 92.00, 91.40, 90.83, 91.10, 91.41, 91.77, 91.40, 91.87, 90.41, 89.84, 89.83, 89.95, 90.78, 90.20, 90.00, 90.53, 90.40, 90.72, 90.73, 89.64, 90.35, 90.02, 89.92, 89.79, 88.84, 89.21, 89.17, 89.54, 89.12, 89.42, 88.63, 89.50, 89.07, 89.37, 88.64, 89.53, 89.02, 89.65, 89.45, 88.94, 88.99, 88.27, 88.85, 89.24, 88.55, 88.60, 89.25, 89.70, 88.46)
	)
	d <- merge(d, geo, by="adm2", all.x=TRUE)
	d$geo_from_source <- FALSE

	d$sample <- d$production <- NULL

	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
