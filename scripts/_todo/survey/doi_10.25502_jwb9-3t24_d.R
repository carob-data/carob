# R script for "carob"
# license: GPL (>=3)

## NOTES
# USAID ERAAL / IITA-NARO annual outcome survey, Uganda, interviewed in 2022 (n=531). File eraal-annual-outcome-survey_2022.csv.
# The CSV repeats column names. Columns are taken by data-dictionary description, matched on position to the CSV (the dictionary's non-empty Column rows are in the same order as the CSV).
# Reshaped to household x crop x season for coffee, common bean, and maize. Seasons in the questions are July-December 2021 and January-May 2022.
# Coffee has one area for both seasons (area_covers_both_seasons_ = TRUE). Variety is the improved-variety name. Inorganic fertilizer, manure, and herbicide are Yes/No. Coffee irrigation is bottle, drip, or sprinkler; maize is drip or sprinkler.
# Area and yield units are not in the dictionary, so the numbers stay in area_reported_ and yield_reported_.

## ISSUES
# Do not treat area_reported_ as hectares or yield_reported_ as kg/ha. Units are not stated. Coffee area is repeated on both season rows.
# land_owned_ and land_cultivated_ are household totals repeated on every crop row.
# This file and the endline file (doi:10.25502/2fxj-4240/d) have the same 531 households in the same order (identical coordinates and districts). Coffee yield for July-December 2021 and several coffee practice-area columns differ. Decide which file is authoritative before using both.
# adm1 is the survey region and adm2 is the district as spelled (Luweero). Confirm GADM. Sub-county and parish are not adm3/adm4.
# Season labels are the questionnaire windows, not terminag season names. Variety text is free text.
# Altitude 0 occurs and was kept.
# write_files: missing carob_contributor; unknown variables area_reported_, yield_reported_, land_owned_, land_cultivated_, area_covers_both_seasons_, subcounty_, parish_; invalid season values.

## NOT PROCESSED
# Demographics (sex, age, education, phone), income, and the shock / food-security block.
# Practice-area and "who decided" columns (traps, fungicides, weeding, pruning, cover crops, intercrops, stumping, trenches, bunds, shade trees, water harvesting, post-harvest).
# Summary scores at the end of the file (Crop_ha, Cultural_ha, Pests_ha, Irrigation_ha, SFM, CA, WM, PHM).
# GPS precision and Actual_landsize_using_GPS (empty).

## NEW VARIABLES
# area_reported_: crop area as recorded; unit not stated
# yield_reported_: crop harvest as recorded; unit not stated
# area_covers_both_seasons_: TRUE for coffee, where one area is repeated on both season rows
# land_owned_: household total land owned, as recorded
# land_cultivated_: household total land under crops, as recorded
# subcounty_: sub-county name
# parish_: parish name


carob_script <- function(path) {

"
Enhancing Resilient and Adaptive Agricultural Livelihoods In Uganda—Scaling of Successful Technologies - Annual Outcome Survey Dataset

The Activity aims to improve smallholder farmers’ livelihoods through increased incomes resulting from increased yields, better organization, and coordination of farmers, improved post-harvest handling, and improved business models. The scaling will encompass the targeting of a broader geographical coverage, a more significant number of beneficiaries adopting and utilizing the successful technologies, tools, approaches, and lessons, and in engaging with more private and public stakeholders, thereby enhancing the resilience of agricultural livelihoods. The activity covers 18 districts in six regions of Uganda, which were selected based on the USAID GFSS ZOI.These include districts in Central Uganda (Luweero and Nakaseke); Northern Uganda (Gulu, Oyam, Lira, and Apac); Eastern Uganda (Mbale, Manafwa, Sironko, and Iganga); Karamoja (Abim); Southwestern Uganda (Rakai, Isingiro, Ntungamo, and Bushenyi); and Western Uganda (Kabarole, Kamwenge, and Bundibugyo).
"

	uri <- "doi:10.25502/jwb9-3t24/d"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA; NARO",
		publication = NA,
		project = "ERAAL",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 30,
		carob_effort = 0.7
	)

	f <- ff[basename(ff) == "eraal-annual-outcome-survey_2022.csv"]
	fd <- ff[basename(ff) == "data_dictionary_eraal-annual-outcome-survey.csv"]
	r <- read.csv(f, check.names=FALSE, stringsAsFactors=FALSE)
	dict <- read.csv(fd, check.names=FALSE, stringsAsFactors=FALSE)
	dict <- dict[nzchar(dict$Column), ]

	g <- function(desc) {
		i <- which(dict$description_abstract == desc)
		if (length(i) != 1) stop("description matched ", length(i), ": ", desc)
		r[[i]]
	}

	yn <- function(x) {
		x <- trimws(tolower(as.character(x)))
		out <- rep(NA, length(x))
		out[x == "yes"] <- TRUE
		out[x == "no"] <- FALSE
		out
	}

	place <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "N/A")] <- NA_character_
		carobiner::fix_name(x, "title")
	}

	clean_var <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "N/A")] <- NA_character_
		x <- gsub("\\s*,\\s*", "; ", x)
		x <- gsub("\\s*;\\s*", "; ", x)
		x <- gsub("(;\\s*)+$", "", x)
		x <- trimws(x)
		x[x == ""] <- NA_character_
		x
	}

	any_yes <- function(...) {
		m <- cbind(...)
		hasT <- rowSums(m == TRUE, na.rm=TRUE) > 0
		hasF <- rowSums(m == FALSE, na.rm=TRUE) > 0
		out <- rep(NA, nrow(m))
		out[hasT] <- TRUE
		out[!hasT & hasF] <- FALSE
		out
	}

	s1 <- "July-December 2021"
	s2 <- "January-May 2022"

	base <- data.frame(
		hhid = as.character(r$ID),
		country = "Uganda",
		adm1 = place(r$Region),
		adm2 = place(r$District),
		location = place(r$Village),
		subcounty_ = place(r$Subcounty),
		parish_ = place(r$Parish),
		latitude = as.numeric(r$GPS_latitude),
		longitude = as.numeric(r$GPS_longitude),
		elevation = as.numeric(r$GPS_altitude),
		land_owned_ = as.numeric(g("Total_land_owned")),
		land_cultivated_ = as.numeric(g("Total_land_cultivated")),
		stringsAsFactors = FALSE
	)

	one <- function(crop, season, area, yield, variety, om, fert, herb, irr, area_both) {
		area <- as.numeric(area)
		yield <- as.numeric(yield)
		keep <- !is.na(area) | !is.na(yield)
		if (!any(keep)) return(NULL)
		d <- base[keep, ]
		d$crop <- crop
		d$season <- season
		d$variety <- clean_var(variety)[keep]
		d$area_reported_ <- area[keep]
		d$yield_reported_ <- yield[keep]
		d$OM_used <- yn(om)[keep]
		d$fertilizer_used <- yn(fert)[keep]
		d$herbicide_used <- yn(herb)[keep]
		d$irrigated <- irr[keep]
		d$area_covers_both_seasons_ <- area_both
		d
	}

	coffee_area <- g("Area_cultivated_with_coffee")
	coffee_var <- g("Which_improved_coffee_varieties_do_you_grow_on_your_farm")
	coffee_om <- g("Did_you_apply_manure_in_coffee")
	coffee_fert <- g("Did_you_apply_inorganic_fertilizers_in_coffee")
	coffee_herb <- g("Did_you_apply_chemical_weed_control_herbicide_application_in_coffee")
	coffee_irr <- any_yes(
		yn(g("Did_you_apply_bottle_irrigation_in_coffee")),
		yn(g("Did_you_apply_drip_irrigation_in_coffee")),
		yn(g("Did_you_apply_sprinkler_irrigation_in_coffee"))
	)
	no_irr <- rep(NA, nrow(r))

	parts <- list(
		one("coffee", s1, coffee_area,
			g("What_was_your_coffee_yield_from_the_season_of_July_to_December_2021"),
			coffee_var, coffee_om, coffee_fert, coffee_herb, coffee_irr, TRUE),
		one("coffee", s2, coffee_area,
			g("What_was_your_coffee_yield_from_the_season_of_Janaury_to_May_2022"),
			coffee_var, coffee_om, coffee_fert, coffee_herb, coffee_irr, TRUE),
		one("common bean", s1,
			g("What_area_of_land_that_was_cultivated_with_beans_from_the_season_of_July_to_December_2021"),
			g("What_was_your_bean_yield_from_the_season_of_July_to_December_2021"),
			g("Which_improved_bean_varieties_did_you_grow_on_your_farm_in_the_season_of_July_to_December_2021"),
			g("Did_you_apply_manure_in_beans_in_the_season_of_July_to_Decemeber_2021"),
			g("Did_you_apply_inorganic_fertilizers_in_beans_in_the_season_of_July_to_December_2021"),
			g("Did_you_apply_herbicides_in_beans_in_the_season_of_July_to_December_2021"),
			no_irr, FALSE),
		one("common bean", s2,
			g("What_area_of_land_that_was_cultivated_with_beans_from_the_season_of_January_to_May_2022"),
			g("What_was_your_bean_yield_from_the_season_of_January_to_May_2022"),
			g("Which_improved_bean_varieties_did_you_grow_on_your_farm_in_the_season_of_January_to_May_2022"),
			g("Did_you_apply_manure_in_beans_in_the_season_of_January_to_May_2022"),
			g("Did_you_apply_inorganic_fertilizers_in_beans_in_the_season_of_January_to_May_2022"),
			g("Did_you_apply_herbicides_in_beans_in_the_season_of_January_to_May_2022"),
			no_irr, FALSE),
		one("maize", s1,
			g("What_area_of_land_that_was_cultivated_with_maize_from_the_season_of_July_to_December_2021"),
			g("What_was_your_maize_yield_from_the_season_of_July_to_December_2021"),
			g("Which_improved_maize_varieties_Did_you_grow_on_your_farm_from_the_season_of_July_to_December_2021"),
			g("Did_you_apply_manure_in_maize_from_the_season_of_July_to_December_2021"),
			g("Did_you_apply_inorganic_fertilizers_in_maize_from_the_season_of_July_to_December_2021"),
			g("Did_you_apply_herbicides_in_maize_from_the_season_of_July_to_December_2021"),
			any_yes(
				yn(g("Did_you_apply_drip_irrigation_in_maize_from_the_season_of_July_to_December_2021")),
				yn(g("Did_you_apply_sprinkler_irrigation_in_maize_from_the_season_of_July_to_December_2021"))
			), FALSE),
		one("maize", s2,
			g("What_area_of_land_that_was_cultivated_with_maize_from_the_season_of_January_to_May_2022"),
			g("What_was_your_maize_yield_from_the_season_of_January_to_May_2022"),
			g("Which_improved_maize_varieties_Did_you_grow_on_your_farm_from_the_season_of_January_to_May2022"),
			g("Did_you_apply_manure_in_maize_from_the_season_of_January_to_May_2022"),
			g("Did_you_apply_inorganic_fertilizers_in_maize_from_the_season_of_January_to_May_2022"),
			g("Did_you_apply_herbicides_in_maize_from_the_season_of_January_to_May_2022"),
			any_yes(
				yn(g("Did_you_apply_drip_irrigation_in_maize_from_the_season_of_January_to_May_2022")),
				yn(g("Did_you_apply_sprinkler_irrigation_in_maize_from_the_season_of_January_to_May_2022"))
			), FALSE)
	)
	parts <- parts[!vapply(parts, is.null, logical(1))]
	d <- do.call(rbind, parts)
	rownames(d) <- NULL

	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- as.logical(NA)
	has_xy <- !is.na(d$latitude) & !is.na(d$longitude)
	d$geo_from_source[has_xy] <- TRUE
	d$trial_id <- as.character(seq_len(nrow(d)))

	carobiner::write_files(path, meta, d)
}
