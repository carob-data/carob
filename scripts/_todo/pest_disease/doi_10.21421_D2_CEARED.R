# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household survey of groundnut farmers in Anantapur district, Andhra Pradesh. 280 households. There is no aflatoxin measurement.
# Names and mobile numbers are not copied.
# Land holdings are in acres. 1 acre = 0.4046863 ha. farmland is the total holding. land_irrigated is the irrigated holding. rainfed_area_ is the rain-fed holding (one household does not add up).
# The crop sheet lists crops by season for 2011-2013. Those rows are in the long table. Paddy is rice. Pigeonpea is pigeon pea. Chilli is chili pepper. Castor is castor bean. Water melon is watermelon. Grpoundnut is groundnut.
# Field pests are ranks 1-4 (the 5th pest column is empty). Storage pests are a separate long row. Diseases are the farmer's own words, in rank order.
# Livestock counts are long rows: cows as cattle, plus buffalo, sheep and goat.
# Everybody answered "No" (one cell is "N0") to having heard of aflatoxin.
# Income uses the Indian comma style (2,00,000). The column does not name the currency.

## ISSUES
# No coordinates for the 12 villages (Gangalakunta, Palabhavi, Krishnapuram, Muddulapuram, Ippaeru, Ramachandrapuram, Uppanaesanapalli, Vasanthapuram, Chintalapalli, Kurlapally, Vaddipalli, Pampanur) or the mandals Rapthadu, Kudaeru, Dharmavaram and Atmakur. The sheet spells the district Ananthapuram. The Dataverse keyword is Anantapur.
# No yield or fertilizer. The GAP sheet was not parsed, so N, P and K are missing.
# One storage-pest answer is truncated ("Groundnut bru"). One disease answer is the pest name Helicoverpa.
# chini, korra, kurbuja and vegetables are not in the crop vocabulary. Pest names and the farmers' disease phrases are not in the vocabularies either.
# write_files: missing carob_contributor, yield, yield_moisture, yield_part, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date; unknown rainfed_area_ (rain?), drought_timing_ (timing?), aflatoxin_aware_, kernels_seen_, kernel_appearance_, abiotic_constraint_, drought_management_, pest_control_, sale_place_, sale_month_, pest_where_; invalid crop chini, korra, kurbuja, vegetables; invalid pest_species and disease terms as recorded.

## NOT PROCESSED
# Personal information: Name of the Farmer, Mobile number. No.Children mixes numbers with "NoChildren", "N0" and "Nil", and ages are not given.
# Agricultural information, column Other (specify): empty.
# Biotic constraints, 5th pest and Others: empty.
# GAP's sheet, all question columns: soil testing, crop rotation, seed choice, farmyard manure rate and timing, cereal residues, chemical fertilizer product and rate, gypsum or lime, harvest method, stripping, drying place and days, moisture check, storage method and days, consumption split, and willingness to pay for aflatoxin-free nuts. Free text, mixed products and units.
# Market: who sells (Male farmer / Husband), and whether sale is immediate. The immediate-sale answer is Yes, with the reason "Lack of money in hand".

## NEW VARIABLES
# rainfed_area_: rain-fed land holding, ha.
# aflatoxin_aware_: heard of aflatoxin (yes/no). All no.
# kernels_seen_: farmer has seen contaminated kernels.
# kernel_appearance_: how those kernels look, as written.
# abiotic_constraint_: major abiotic constraint, as written (Drought or Rainfall).
# drought_timing_: growth stage the farmer said was most affected in a drought year.
# drought_management_: how the farmer manages drought.
# pest_control_: chemical pesticides or traditional practices.
# pest_where_: field or storage.
# sale_place_: where the groundnuts are sold.
# sale_month_: month of sale.

carob_script <- function(path) {

"
Aflatoxin contamination problem in Anantapur district

Aflatoxin contamination problem studies  in Anantapur district of Andhra Pradesh of South India
"

	uri <- "doi:10.21421/D2/CEARED"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 1.2
	)

	f <- ff[basename(ff) == "Aflatoxin problem Anantapur AP India.xlsx"]
	p <- carobiner::read.excel(f, sheet="Personal information", skip=1, na=c("NA", ""))
	ag <- carobiner::read.excel(f, sheet="Agricultural information", skip=4, col_names=FALSE, na=c("NA", ""))
	bi <- carobiner::read.excel(f, sheet="Biotic constraints", skip=1, na=c("NA", ""))
	ab <- carobiner::read.excel(f, sheet="Abiotic constraints ", skip=1, na=c("NA", ""))
	aw <- carobiner::read.excel(f, sheet="Awareness on aflatoxin conta", skip=2, na=c("NA", ""))
	mk <- carobiner::read.excel(f, sheet="Market ", skip=1, na=c("NA", ""))

	align <- function(x) {
		id <- as.character(x[[1]])
		ok <- grepl("^[0-9]+$", id)
		x <- x[ok, ]
		x[match(hhid, as.character(x[[1]])), ]
	}

	acre <- 0.4046863
	hhid <- as.character(p[["S.No"]])
	ag <- align(ag)
	bi <- align(bi)
	ab <- align(ab)
	aw <- align(aw)
	mk <- align(mk)

	sex <- tolower(trimws(p$Gender))
	inc <- gsub(",", "", as.character(p[["Yearly Income"]]))

	heard <- tolower(trimws(aw[[2]]))
	heard[heard == "n0"] <- "no"
	seen <- tolower(trimws(aw[[4]]))

	major <- trimws(ab[[2]])
	zinc <- tolower(trimws(ab[[5]])) == "zinc"
	zinc[is.na(zinc)] <- FALSE
	stress <- ifelse(tolower(major) == "drought", "drought", NA_character_)
	stress[zinc & !is.na(stress)] <- paste(stress[zinc & !is.na(stress)], "zinc deficiency", sep=";")
	stress[zinc & is.na(stress)] <- "zinc deficiency"

	d <- data.frame(
		hhid = hhid,
		trial_id = hhid,
		country = "India",
		adm1 = "Andhra Pradesh",
		adm2 = "Ananthapuram",
		adm3 = carobiner::fix_name(trimws(p[["Name of the mandal"]]), "title"),
		location = carobiner::fix_name(trimws(p[["Name of the village"]]), "title"),
		sex = sex,
		age = as.numeric(p$Age),
		education = trimws(p[["Educational level"]]),
		civil_status = tolower(trimws(p[["Mariatal status"]])),
		hh_income = as.numeric(inc),
		farmland = as.numeric(ag[[4]]) * acre,
		land_irrigated = as.numeric(ag[[2]]) * acre,
		rainfed_area_ = as.numeric(ag[[3]]) * acre,
		aflatoxin_aware_ = heard == "yes",
		kernels_seen_ = seen == "yes",
		kernel_appearance_ = trimws(gsub("\\s+", " ", aw[[5]])),
		abiotic_constraint_ = major,
		drought_timing_ = trimws(ab[[3]]),
		drought_management_ = trimws(ab[[4]]),
		stress = stress,
		pest_control_ = trimws(bi[[12]]),
		sale_place_ = trimws(mk[[2]]),
		sale_month_ = trimws(mk[[7]]),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE,
		stringsAsFactors = FALSE
	)
	d$kernel_appearance_[is.na(d$kernel_appearance_) | !nzchar(d$kernel_appearance_)] <- NA_character_
	d$aflatoxin_aware_[!(heard %in% c("no", "yes"))] <- NA
	d$kernels_seen_[!(seen %in% c("no", "yes"))] <- NA

	map_crop <- function(z) {
		z0 <- tolower(trimws(z))
		z0 <- gsub("\\s+", " ", z0)
		out <- z0
		out[z0 %in% c("groundnut", "grpoundnut")] <- "groundnut"
		out[z0 == "paddy"] <- "rice"
		out[grepl("pigeon", z0)] <- "pigeon pea"
		out[z0 == "mango"] <- "mango"
		out[z0 %in% c("chilli", "chili")] <- "chili pepper"
		out[z0 == "banana"] <- "banana"
		out[z0 == "tomato"] <- "tomato"
		out[z0 == "wheat"] <- "wheat"
		out[z0 == "castor"] <- "castor bean"
		out[z0 %in% c("water melon", "watermelon")] <- "watermelon"
		out
	}
	split_crops <- function(x) {
		x <- gsub("&", ",", x)
		x <- gsub(" and ", ",", x, ignore.case=TRUE)
		strsplit(x, ",")
	}

	crop_rows <- function(col, year, season) {
		parts <- split_crops(as.character(ag[[col]]))
		h <- rep(hhid, lengths(parts))
		crops <- trimws(unlist(parts))
		if (is.null(crops)) crops <- character()
		keep <- !is.na(crops) & nzchar(crops) & !is.na(h)
		if (!any(keep)) {
			return(data.frame(hhid=character(), crop=character(), season=character(), planting_date=character(), stringsAsFactors=FALSE))
		}
		data.frame(
			hhid = h[keep],
			crop = map_crop(crops[keep]),
			season = rep(season, sum(keep)),
			planting_date = rep(year, sum(keep)),
			stringsAsFactors = FALSE
		)
	}
	crops <- rbind(
		crop_rows(5, "2013", "kharif"),
		crop_rows(6, "2013", "rabi"),
		crop_rows(7, "2012", "kharif"),
		crop_rows(8, "2012", "rabi"),
		crop_rows(9, "2011", "kharif"),
		crop_rows(10, "2011", "rabi")
	)

	clean_pest <- function(x) {
		x <- trimws(x)
		x[!nzchar(x)] <- NA_character_
		z <- tolower(gsub("\\s+", " ", x))
		x[z %in% c("leaf folder", "leaf floder")] <- "Leaf folder"
		x[z %in% c("helicoverpa", "helico verpa")] <- "Helicoverpa"
		x[z == "spodoptera"] <- "Spodoptera"
		x[grepl("red ?hairy|redhiary|red hairy", z)] <- "Red hairy caterpillar"
		x[z == "jassids"] <- "Jassids"
		x[z == "redweevils"] <- "Red weevils"
		x
	}
	pest_rows <- function(x, where) {
		x <- clean_pest(as.character(x))
		x <- gsub("&", ";", x)
		keep <- !is.na(x)
		x <- x[keep]
		id <- hhid[keep]
		parts <- lapply(strsplit(x, ";"), function(p) {
			p <- trimws(p)
			p[nzchar(p)]
		})
		data.frame(
			hhid = rep(id, lengths(parts)),
			pest_species = unlist(parts),
			pest_where_ = where,
			stringsAsFactors = FALSE
		)
	}
	pests <- rbind(
		pest_rows(bi[[2]], "field"),
		pest_rows(bi[[3]], "field"),
		pest_rows(bi[[4]], "field"),
		pest_rows(bi[[5]], "field"),
		pest_rows(bi[[11]], "storage")
	)

	dis_rows <- function(x) {
		x <- trimws(gsub("\\s+", " ", as.character(x)))
		x[!nzchar(x)] <- NA_character_
		keep <- !is.na(x)
		data.frame(hhid = hhid[keep], disease = x[keep], stringsAsFactors = FALSE)
	}
	diseases <- rbind(dis_rows(bi[[7]]), dis_rows(bi[[8]]), dis_rows(bi[[9]]))

	animal_rows <- function(x, animal) {
		nhead <- as.numeric(x)
		keep <- !is.na(nhead)
		data.frame(hhid = hhid[keep], animal = animal, heads = nhead[keep], stringsAsFactors = FALSE)
	}
	livestock <- rbind(
		animal_rows(ag[[11]], "cattle"),
		animal_rows(ag[[12]], "buffalo"),
		animal_rows(ag[[13]], "sheep"),
		animal_rows(ag[[14]], "goat")
	)

	long <- rbind(
		data.frame(hhid=crops$hhid, crop=crops$crop, season=crops$season, planting_date=crops$planting_date,
			pest_species=NA_character_, pest_where_=NA_character_, disease=NA_character_, animal=NA_character_, heads=NA_real_, stringsAsFactors=FALSE),
		data.frame(hhid=pests$hhid, crop=NA_character_, season=NA_character_, planting_date=NA_character_,
			pest_species=pests$pest_species, pest_where_=pests$pest_where_, disease=NA_character_, animal=NA_character_, heads=NA_real_, stringsAsFactors=FALSE),
		data.frame(hhid=diseases$hhid, crop=NA_character_, season=NA_character_, planting_date=NA_character_,
			pest_species=NA_character_, pest_where_=NA_character_, disease=diseases$disease, animal=NA_character_, heads=NA_real_, stringsAsFactors=FALSE),
		data.frame(hhid=livestock$hhid, crop=NA_character_, season=NA_character_, planting_date=NA_character_,
			pest_species=NA_character_, pest_where_=NA_character_, disease=NA_character_, animal=livestock$animal, heads=livestock$heads, stringsAsFactors=FALSE)
	)
	long$order <- as.integer(ave(seq_len(nrow(long)), long$hhid, FUN=seq_along))

	carobiner::write_files(path, meta, d, long=long)
}
