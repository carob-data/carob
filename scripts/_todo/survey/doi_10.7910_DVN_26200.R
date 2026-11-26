# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI/BIDS "Livelihood Systems in Bangladesh" panel, 1987 round (62 villages, ~1230 households).
# Parcel87.xls is an Excel 2.1 (BIFF2) worksheet (exported from SPSS), read via carobiner::read.excel() BIFF2 fallback.
# Parcel87 has one row per household parcel with crop, variety code and production for the Boro, Aus and Aman seasons. Reshaped to one row per parcel x season (seasons without a crop dropped).
# Parcel area "land": the codebook says "decimal", but values are hectares (= decimals / 247; parcel sums equal household clvt in ha).
# Production prdbr/prdas/prdam: tonnes (values are multiples of 1 maund = 0.03732 t; household totals elsewhere are labelled "tons"). yield = production * 1000 / land.
# Crop codes (Parcel_val) hard-coded below; TV/MV in the crop label = traditional/modern variety (variety_type). B-Aus/B-Aman = broadcast, T-Aman = transplanted.
# Variety and village/union/upazila/zila names are decoded from the codebook "Documentation of 1987_Bangladesh.xls".
# zila = district (adm2), upazila = adm3, union = adm4, village = location. Zila spellings adjusted to GADM.

## ISSUES
# No coordinates. 62 villages (location) with upazila/zila names need georeferencing; upazila/union spellings not checked against GADM.
# Survey year 1987 but crop year/season dates not given (Boro 1986/87?); planting_date not set.
# Production unit for non-rice crops (jute, potato, vegetables, ...) assumed tonnes like rice; not documented.
# Variety code 999 (and any code not in the codebook) has no label; left as NA. Code 0 = not mentioned.
# "irg" is the parcel's irrigation facility, not necessarily used in every season; irrigated is only set to FALSE for "No irrigation".
# "B-Aus & Aman" (mixed broadcast aus + aman rice) mapped to rice; original crop label kept in crop_label_. Variety is given for few parcels.
# yield_part left NA for jute, vegetables, onion, garlic, spices (fibre/bulb/mixed products; unit and part unclear).
# Unmapped: HHDataY1987 part 1/2 (household aggregates and income), Member87 (demography), Emp4days87 (time use), most household background columns repeated in Parcel87.
# write_files: no coordinates; season "aus" is not in the season vocabulary; yield_part NA; high yields on tiny parcels (e.g. vegetable 276 t/ha on 0.008 ha, lentil 24.9 t/ha) left as is, check.

## NEW VARIABLES
# land_type_: flooding land type of the parcel (high, medium high, low, very low land)
# crop_label_: original crop/season label from the codebook (e.g. "MV Boro", "B-Aman", "Kaon etc")



## NOT PROCESSED
# HHDataY1987_part 1.xls: not read. sheet Ecosystem (of 2): zila...1, ecosystem, ...3, sample, survey, splithh, village, union, upazila, ctrl, zila...11, Ecosystem. Observation records are missing from d until this file is standardized.
# HHDataY1987_part 2.xls: not read. sheet HHDataY1987_part 2 (of 2): sample, survey, splithh, village, union, upazila, ctrl, zila, Ecosystem, incpc, inchh, incagr, .... Observation records are missing from d until this file is standardized.
# Project details.xlsx: not read. sheet PROJECT: Title of the Study, ...2, Livelihood Systems in Bangladesh --for each year of study and the panel households created for all the years of study. Observation records are missing from d until this file is standardized.

carob_script <- function(path) {

"Livelihood Systems in Bangladesh (1987)

a)  Understand the operation of the Bangladesh rural household economy;  b) Analyze the drivers of change in income distribution and poverty;  c)  Prepare technical background papers to facilitating policy dialogues under the project 'Poverty Elimination Through Rice Research Assistance (PETRRA)' in Bangladesh;  d) Assess the impact of rice research on poverty reduction in Bangladesh using the Sustainable Livelihood Framework; and  e)  Estimating parameters on the determinants of rural household incomes for use in poverty mapping for Bangladesh."

	uri <- "doi:10.7910/DVN/26200"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
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
		carob_completion = 40,
		carob_effort = 0
	)

	f1 <- ff[basename(ff) == "Documentation of 1987_Bangladesh.xls"]
	# value labels from the codebook: variable name (filled down), code, label
	codes <- function(sheet, var) {
		r1 <- carobiner::read.excel(f1, sheet=sheet, col_names=FALSE)
		nm <- r1[[1]]
		nm <- nm[!is.na(nm)][cumsum(!is.na(nm))]
		k <- nm == var & !is.na(r1[[4]])
		data.frame(code=as.numeric(r1[[2]][k]), label=trimws(r1[[4]][k]))
	}
	lab <- function(x, cds) {
		cds$label[match(x, cds$code)]
	}

	r <- carobiner::read.excel(ff[basename(ff) == "Parcel87.xls"])

	vrty <- codes("Parcel_val", "vrtybr/vrtyam/vrtyas")
	tanr <- codes("Parcel_val", "tanr")
	fld <- codes("Parcel_val", "fld")
	irg <- codes("Parcel_val", "irg")

	# crop codes from sheet Parcel_val
	crops <- data.frame(code=1:41, 
		label = c("TV Aus", "MV Aus", "B-Aus & Aman", "B-Aman", "TV T-Aman", "MV Aman", "Pajam Aman", "TV Boro", "MV Boro", "TV Wheat", "MV Wheat", "Kaon etc", "Jute", "Sugarcane", "Tobacco", "Potato", "Chilli", "Vegetables", "Mustard", "Til", "Other oilseeds", "Masur", "Kheshari", "Kalai", "Mung", "Gram", "Other pulses", "Betel leaves", "Betel nut", "Ginger", "Turmaric", "Onion", "Garlic", "Other spices", "Banana", "Jackfruit", "Pineapple", "Other fruits", "Seedbed", "Bamboo", "Fish"),
		crop = c(rep("rice", 9), "wheat", "wheat", "foxtail millet", "jute", "sugarcane", "tobacco", "potato", "chili pepper", "vegetable", "mustard", "sesame", "oilseed", "lentil", "grass pea", "black gram", "mung bean", "chickpea", "pulse", "betel", "areca nut", "ginger", "tumeric", "onion", "garlic", "spices", "banana", "jackfruit", "pineapple", "fruit", NA, "bamboo", NA),
		variety_type = c("traditional", "modern", NA, NA, "traditional", "modern", NA, "traditional", "modern", "traditional", "modern", rep(NA, 30)),
		planting_method = c(NA, NA, "broadcasting", "broadcasting", "transplanting", rep(NA, 36)),
		yield_part = c(rep("grain", 12), NA, "stems", "leaves", "tubers", "fruit", NA, rep("seed", 9), "leaves", NA, "rhizome", "rhizome", "bulbs", "bulbs", NA, rep("fruit", 4), NA, NA, NA)
	)

	# one row per parcel x season
	seasons <- data.frame(season=c("boro", "early wet", "aman"), sfx=c("br", "as", "am"))
	d <- lapply(1:3, function(i) {
		s <- seasons$sfx[i]
		data.frame(
			hhid = as.character(r$sample),
			field_id = as.character(r[["plot#"]]),
			season = seasons$season[i],
			crop_code = r[[paste0("crp", s)]],
			variety_code = r[[paste0("vrty", s)]],
			production = r[[paste0("prd", s)]],
			field_size = r$land,
			land_tenure = lab(r$tanr, tanr),
			land_type_ = lab(r$fld, fld),
			irrigation_source = lab(r$irg, irg),
			village = r$village,
			union = r$union,
			upazila = r$upazila,
			zila = r$zila,
			hh_size = r$hhsize,
			farmland_owned = r$ownland,
			cropland = r$clvt
		)
	})
	d <- do.call(rbind, d)
	d <- d[d$crop_code > 0, ]

	i <- match(d$crop_code, crops$code)
	d$crop <- crops$crop[i]
	d$crop_label_ <- crops$label[i]
	d$variety_type <- crops$variety_type[i]
	d$planting_method <- crops$planting_method[i]
	d$yield_part <- crops$yield_part[i]
	d$variety <- lab(d$variety_code, vrty)
	d$variety[d$crop_label_ == "Pajam Aman" & is.na(d$variety)] <- "Pajam"
	# production in tonnes, area in ha
	d$yield <- d$production * 1000 / d$field_size

	d$irrigated <- ifelse(d$irrigation_source == "No irrigation", FALSE, NA)
	irmap <- c("No irrigation"="none", "DTW"="groundwater", "STW"="groundwater", "LLP"="surface water", "WDB channel"="canal", "Traditional"="unknown")
	irlift <- c("DTW"="motorized pump", "STW"="motorized pump", "LLP"="motorized pump", "Traditional"="manual")
	d$irrigation_lifting <- unname(irlift[d$irrigation_source])
	d$irrigation_source <- unname(irmap[d$irrigation_source])

	d$location <- lab(d$village, codes("Genral_val", "village"))
	d$adm4 <- lab(d$union, codes("Genral_val", "union"))
	d$adm3 <- lab(d$upazila, codes("Genral_val", "upazila"))
	d$adm2 <- lab(d$zila, codes("Genral_val", "zila"))
	gadm <- c("B. Baria"="Brahamanbaria", "Mouvi-Bazar"="Maulvibazar", "Lakhmipur"="Lakshmipur", "Jhalakhati"="Jhalokati", "Perojpur"="Pirojpur", "Jhenaidaha"="Jhenaidah", "Kustia"="Kushtia", "Nababganj"="Nawabganj", "Panchaghar"="Panchagarh", "Netrokona"="Netrakona", "Munsiganj"="Munshiganj")
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

	d$crop_code <- d$variety_code <- d$production <- NULL
	d$village <- d$union <- d$upazila <- d$zila <- NULL

	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
