# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rothamsted long-term liming experiment, 1962-1996. Plot and sub-plot yields are in crop_data.csv. The factor files give P, K, Mg, Mn and S rates as kg of the element per ha, and lime totals as t CaCO3 per ha.
# The yield column has no unit. Grain values stay below 10 and fresh potato values reach about 62, so yield is stored as those values times 1000 (kg/ha).
# Percent dry matter is the dry-matter content of the harvested product. yield_moisture is 100 minus that value. Potato dry matter is not recorded.
# N, and the basal K and Mg rates, sit in the same table. Their unit is not written on the column. They are stored as kg/ha of the element, which is the unit the factor files use for P, K, Mg and S.
# Rothamsted Experimental Farm is in the United Kingdom.

## ISSUES
# The yield column has no unit. It is stored times 1000 because grain values are below 10 and fresh potato values reach about 62.
# Winter lupins sown 1992-10-07 were harvested 1993-10-10, 368 days later.
# write_files: , latitude, longitude, irrigated. unknown variable: lime_CaCO3_t_ha_ (lime?). datespan: 64 harvest_date more than 366 days after planting_date. NA detected: yield.

## NEW VARIABLES
# lime_CaCO3_t_ha_: annual lime, t CaCO3/ha. lime in the vocabulary is kg/ha and stops at 10000, so a 15 t application does not fit there.


## NOT PROCESSED
# notes_data.csv, the factor description tables, and the cumulative lime and P columns.
# The Excel workbook repeats the csv files.


carob_script <- function(path) {

"
Rothamsted long-term liming experiment crop yields 1962-1996

The experiment tests the effects of different amounts of lime (and thus soil pH), P and K on the yields of a sequence of arable crops, on a silty clay loam soil at Rothamsted Experimental Farm, from 1962-1996. Mg, Mn and S were tested on split plots at different times. Target soil pH was 4.5, 5.5, 6.5 and 7.5. The dataset contains plot and sub-plot yields for all plots and years, 1962-1996, details of all treatments applied (lime, P, K, Mg, Mn and S), and agronomic details (crop variety, sowing and harvest date, basal fertilizer).
"

	uri <- "doi:10.23637/rcs10-rltlyields-01"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	# The Rothamsted download does not save a metadata json that get_metadata can read.
	# Title, version, license, publisher and authors are the README.txt values.
	meta <- data.frame(
		uri = uri,
		dataset_id = "doi_10.23637_rcs10-rltlyields-01",
		title = "Rothamsted long-term liming experiment crop yields 1962-1996.",
		authors = "Margaret Glendining; Jonathan Holland; Steve McGrath; Keith Goulding",
		authors_email = NA_character_,
		date_published = "2021",
		publisher = "Electronic Rothamsted Archive, Rothamsted Research",
		version = "1.0",
		license = "CC-BY-4.0",
		description = "The experiment tests the effects of different amounts of lime (and thus soil pH), P and K on the yields of a sequence of arable crops, on a silty clay loam soil at Rothamsted Experimental Farm, from 1962-1996.",
		data_citation = "Margaret Glendining; Jonathan Holland; Steve McGrath; Keith Goulding (2021). Rothamsted long-term liming experiment crop yields 1962-1996. Electronic Rothamsted Archive, Rothamsted Research. Version 1.0. doi:10.23637/rcs10-rltlyields-01",
		carob_group = group,
		data_organization = "ROTH",
		publication = "doi:10.1016/j.eja.2019.02.016",
		project = NA_character_,
		design = NA_character_,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer",
		response_vars = "yield",
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "crop_data.csv"]
	r <- read.csv(f, check.names=FALSE)
	num <- function(x) {
		x <- trimws(as.character(x))
		x[x == "-"] <- "0"
		x[x %in% c("", "*", "NA")] <- NA
		as.numeric(x)
	}
	add <- function(a, b) ifelse(is.na(a) & is.na(b), NA_real_, ifelse(is.na(a), 0, a) + ifelse(is.na(b), 0, b))

	crop <- c(
		"spring beans" = "faba bean", "spring barley" = "barley", "potatoes" = "potato",
		"fallow" = "none", "spring oats" = "oats", "spring oilseed rape" = "rapeseed",
		"winter triticale" = "triticale", "spring lupins" = "white lupin",
		"spring linseed" = "flax", "winter oilseed rape" = "rapeseed",
		"winter lupins" = "white lupin", "winter wheat" = "wheat"
	)
	cn <- crop[r$crop]
	if (any(is.na(cn))) stop("crop not mapped")
	part <- c(
		"faba bean" = "seed", barley = "grain", potato = "tubers", none = "none",
		oats = "grain", rapeseed = "seed", triticale = "grain", "white lupin" = "seed",
		flax = "seed", wheat = "grain"
	)

	sp <- trimws(as.character(r[["sub-plot"]]))
	pid <- as.character(r$plot)
	use <- !(is.na(sp) | sp %in% c("", "-", "NA"))
	pid[use] <- paste(pid[use], sp[use], sep="-")
	dm <- num(r[["%_dry_matter"]])

	d <- data.frame(
		trial_id = as.character(r$harvest_year),
		country = "United Kingdom", # Rothamsted Experimental Farm
		location = r$site,
		plot_id = pid,
		rep = r$block,
		crop = unname(cn),
		variety = r$variety,
		planting_date = trimws(r$sow_date),
		harvest_date = trimws(r$harvest_date),
		yield = num(r$yield) * 1000,
		yield_moisture = 100 - dm,
		yield_part = unname(part[cn]),
		lime_CaCO3_t_ha_ = num(r$annual_lime),
		N_fertilizer = num(r$n_basal_rate),
		P_fertilizer = num(r$p_rate_annual),
		K_fertilizer = add(num(r$k_rate_annual), num(r$k_basal_rate)),
		Mg_fertilizer = add(num(r$mg_rate_annual), num(r$mg_basal_rate)),
		Mn_fertilizer = num(r$mn_rate_annual),
		S_fertilizer = num(r$s_rate_annual),
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d$variety[d$variety %in% c("", "*")] <- NA
	d$planting_date[d$planting_date %in% c("", "*")] <- NA
	d$harvest_date[d$harvest_date %in% c("", "*")] <- NA

	carobiner::write_files(path, meta, d)
}
