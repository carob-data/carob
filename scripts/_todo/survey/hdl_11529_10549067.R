# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household seed-security survey in Nepal. Geographic coverage is Nepal. Every GPS coordinate in Main Data is empty.
# Khet and bari areas are in ropani. One ropani is taken as 508.74 m2 (5476 square feet).
# Maize and wheat seed quantities are in kg. Production cells mix kg, muri, and pathi. The code book does not define muri or pathi, so only quantities already marked kg are used for yield.
# Variety is the first named variety of the respondent whose crop selection matches maize or wheat.

## ISSUES
# write_files: missing variables: latitude, longitude NA detected: yield contributor:

## NOT PROCESSED
# Seed-source, gender-role, diet, training, and perception modules.
# Production recorded as muri or pathi.
# Respondent names and phone numbers.
# Livestock counts and household assets.


carob_script <- function(path) {

"
Gender and Household Seed Security in Nepal

The study of women's involvement and responsibilities within seed systems in Nepal is scarce, despite the widely held belief that women possess extensive knowledge regarding seed selection and preservation, especially concerning maize and wheat, which are vital staple grains in remote mountain communities. Women play various crucial roles in maize and wheat farming, yet major decisions regarding crops management, such as variety selection, are predominantly made by men.
"

	uri <- "hdl:11529/10549067"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 35,
		carob_effort = 0.5
	)

	f <- ff[grepl("Seed Security Nepal", basename(ff))]
	r <- carobiner::read.excel(f, sheet="Main Data", na=c("", "NA", "n/a"))
	num <- function(x) {
		if (is.null(x)) return(NA_real_)
		if (is.numeric(x)) return(x)
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "n/a")] <- NA_character_
		as.numeric(x)
	}
	blank <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x) | x %in% c("NA", "n/a")] <- NA_character_
		x
	}
	kg_only <- function(x) {
		z <- tolower(gsub(" ", "", blank(x)))
		unit <- rep(NA_character_, length(z))
		unit[!is.na(z) & grepl("kg", z)] <- "kg"
		unit[!is.na(z) & grepl("muri", z)] <- "muri"
		unit[!is.na(z) & grepl("pathi", z)] <- "pathi"
		unit[!is.na(z) & grepl("muri", z) & grepl("kg", z)] <- NA_character_
		numc <- z
		numc <- gsub("kg", "", numc)
		numc <- gsub("muri", "", numc)
		numc <- gsub("pathi", "", numc)
		numc[!is.na(numc) & grepl("[A-Za-z]", numc)] <- NA_character_
		qty <- as.numeric(numc)
		out <- rep(NA_real_, length(z))
		use <- !is.na(unit) & unit == "kg" & !is.na(qty)
		out[use] <- qty[use]
		out
	}
	ropani_ha <- 508.74 / 10000
	area_ha <- function(a, b) {
		a <- num(a)
		b <- num(b)
		out <- ifelse(is.na(a), 0, a) + ifelse(is.na(b), 0, b)
		out[is.na(a) & is.na(b)] <- NA_real_
		out * ropani_ha
	}
	variety_for <- function(crop) {
		r1 <- toupper(blank(r$R1_Crop_Select))
		r2 <- toupper(blank(r$R2_Crop_Select))
		v <- rep(NA_character_, nrow(r))
		hit1 <- !is.na(r1) & r1 == crop
		v[hit1] <- blank(r$R1_VarietyA_Name)[hit1]
		hit2 <- is.na(v) & !is.na(r2) & r2 == crop
		v[hit2] <- blank(r$R2_VarietyA_Name)[hit2]
		v
	}

	hh <- blank(r$HH_ID)
	loc <- trimws(paste(blank(r$Village_Name), blank(r$Municipal_Name)))
	loc[!nzchar(loc)] <- NA_character_
	loc <- carobiner::fix_name(loc, "title")
	one <- function(crop, prod, land_a, land_b, seed) {
		fs <- area_ha(land_a, land_b)
		kg <- kg_only(prod)
		y <- rep(NA_real_, length(kg))
		use <- !is.na(kg) & !is.na(fs) & fs > 0
		y[use] <- kg[use] / fs[use]
		sr <- rep(NA_real_, length(kg))
		seed_kg <- num(seed)
		use_s <- !is.na(seed_kg) & !is.na(fs) & fs > 0
		sr[use_s] <- seed_kg[use_s] / fs[use_s]
		keep <- !is.na(kg) | !is.na(fs)
		data.frame(
			country = "Nepal",
			location = loc[keep],
			hhid = hh[keep],
			trial_id = paste(hh[keep], crop),
			crop = crop,
			variety = variety_for(toupper(crop))[keep],
			yield = y[keep],
			yield_part = "grain",
			field_size = fs[keep],
			seed_rate = sr[keep],
			on_farm = TRUE,
			is_survey = TRUE,
			geo_from_source = FALSE
		)
	}

	d1 <- one("maize", r$Tot_MaizePrdn, r$KhetLand_Maize, r$BariLand_Maize, r$Tot_MaizeSeed_Used)
	d2 <- one("wheat", r$Tot_WheatPrdn, r$KhetLand_Wheat, r$BariLand_Wheat, r$Tot_WheatSeed_Used)
	d <- carobiner::bindr(d1, d2)
	d <- d[!is.na(d$hhid), ]
	carobiner::write_files(path, meta, d)
}
