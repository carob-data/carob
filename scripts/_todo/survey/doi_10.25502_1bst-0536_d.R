# R script for "carob"
# license: GPL (>=3)

## NOTES
# USAID ERAAL / IITA-NARO annual outcome survey, Uganda, 2024 (n=756). One household row in usaid_eraal_activity_iita-naro_annual_outcome_survey.csv.
# Reshaped to household x crop x season for coffee, common bean, and maize. Seasons are July-December 2023 and January-May 2024.
# Coffee has one area for both seasons (area_covers_both_seasons_ = TRUE); bean and maize areas are asked per season.
# Variety is the improved-variety name only. Inorganic fertilizer, manure, and herbicide are Yes/No. Coffee irrigation is bottle, drip, or sprinkler; maize is drip or sprinkler. Beans have no irrigation question.
# The data dictionary and the questionnaire PDF do not state units for area or yield, so those numbers are kept as recorded (area_reported_, yield_reported_) and are not converted to ha or kg/ha.

## ISSUES
# Area and yield units are not in the dictionary. The PDF questionnaire is not text-extractable. Do not treat area_reported_ as hectares or yield_reported_ as kg/ha. Coffee yields reach 6e6 and maize yields reach 1.25e5, so the column is not a clean kg/ha series.
# Coffee area is repeated on both season rows. Do not sum it across seasons.
# land_owned_ and land_cultivated_ are household totals, repeated on every crop row, same unknown unit.
# Three households have no GPS. Two recorded points are outside Uganda and were kept: household 407 (Kabarole, 0.444 N, 3.028 E) and household 471 (Bushenyi, -0.598 N, 20.288 E). Altitude 0 was kept. No geocoding or coordinate repair.
# adm1 is the survey region (Central, Eastern, Northern, Western) and adm2 is the district as spelled (Luweero, not Luwero). Confirm against GADM. Sub-county and parish are not assigned to adm3/adm4.
# Season text is the questionnaire window, not a terminag season name. Improved-variety text is free text (several names in one cell, joined with "; ").
# write_files: missing carob_contributor; unknown variables area_reported_, yield_reported_, land_owned_, land_cultivated_, area_covers_both_seasons_, subcounty_, parish_; invalid season values; NA latitude/longitude (3 households); coordinates not on land; coordinates in the wrong country (the Bushenyi point falls in DRC).

## NOT PROCESSED
# Demographics ERAAL27-ERAAL2124 (sex, age, education, marital status, phone) and enumerator/consent fields.
# Income ERAAL36-ERAAL39 (on-farm and off-farm, by season).
# Practice-area and "who decided" columns for traps, phytosanitary removal, fungicides, weeding, pruning, cover crops, gap filling, intercrops, stumping, mulching, trenches, bunds, shade trees, and water harvesting (for example ERAAL4153, ERAAL41152, ERAAL41172, ERAAL41232). Same undocumented area unit as area_reported_; not crop yield.
# Free-text pesticide, soil-management, cultural-practice, and post-harvest answers.
# Shock and food-security block ERAAL51-ERAAL55.
# GPS precision.

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
Enhancing Resilient and Adaptive Agricultural Livelihoods In Uganda—Scaling of Successful Technologies - 2024 Annual Outcome Survey Dataset.

The Activity aims to improve smallholder farmers’ livelihoods through increased incomes resulting from increased yields, better organization, and coordination of farmers, improved post-harvest handling, and improved business models. The scaling will encompass the targeting of a broader geographical coverage, a more significant number of beneficiaries adopting and utilizing the successful technologies, tools, approaches, and lessons, and in engaging with more private and public stakeholders, thereby enhancing the resilience of agricultural livelihoods. The activity covers 18 districts in six regions of Uganda, which were selected based on the USAID GFSS ZOI.These include districts in Central Uganda (Luweero and Nakaseke); Northern Uganda (Gulu, Oyam, Lira, and Apac); Eastern Uganda (Mbale, Manafwa, Sironko, and Iganga); Karamoja (Abim); Southwestern Uganda (Rakai, Isingiro, Ntungamo, and Bushenyi); and Western Uganda (Kabarole, Kamwenge, and Bundibugyo). v
"

	uri <- "doi:10.25502/1bst-0536/d"
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
		carob_effort = 1
	)

	f <- ff[basename(ff) == "usaid_eraal_activity_iita-naro_annual_outcome_survey.csv"]
	r <- read.csv(f)

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

	base <- data.frame(
		hhid = as.character(r$ID),
		country = "Uganda",
		adm1 = place(r$ERAAL21),
		adm2 = place(r$ERAAL22),
		location = place(r$ERAAL25),
		subcounty_ = place(r$ERAAL23),
		parish_ = place(r$ERAAL24),
		latitude = as.numeric(r$latitude),
		longitude = as.numeric(r$longitude),
		elevation = as.numeric(r$altitude),
		land_owned_ = as.numeric(r$ERAAL31),
		land_cultivated_ = as.numeric(r$ERAAL34)
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

	coffee_irr <- any_yes(yn(r$ERAAL4120), yn(r$ERAAL4121), yn(r$ERAAL4122))
	maize_irr_a <- any_yes(yn(r$ERAAL4323), yn(r$ERAAL4325))
	maize_irr_b <- any_yes(yn(r$ERAAL4324), yn(r$ERAAL4327))
	no_irr <- rep(NA, nrow(r))

	parts <- list(
		one("coffee", "July-December 2023", r$ERAAL411, r$ERAAL412, r$ERAAL4141, r$ERAAL4111, r$ERAAL4112, r$ERAAL4110, coffee_irr, TRUE),
		one("coffee", "January-May 2024", r$ERAAL411, r$ERAAL413, r$ERAAL4141, r$ERAAL4111, r$ERAAL4112, r$ERAAL4110, coffee_irr, TRUE),
		one("common bean", "July-December 2023", r$ERAAL421, r$ERAAL423, r$ERAAL4251, r$ERAAL4213, r$ERAAL4215, r$ERAAL4211, no_irr, FALSE),
		one("common bean", "January-May 2024", r$ERAAL422, r$ERAAL424, r$ERAAL4261, r$ERAAL4214, r$ERAAL4216, r$ERAAL4212, no_irr, FALSE),
		one("maize", "July-December 2023", r$ERAAL431, r$ERAAL433, r$ERAAL4351, r$ERAAL4313, r$ERAAL4315, r$ERAAL4311, maize_irr_a, FALSE),
		one("maize", "January-May 2024", r$ERAAL432, r$ERAAL434, r$ERAAL4361, r$ERAAL4314, r$ERAAL4317, r$ERAAL4312, maize_irr_b, FALSE)
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
