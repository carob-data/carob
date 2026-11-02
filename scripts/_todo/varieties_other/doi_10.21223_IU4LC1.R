# R script for "carob"
# license: GPL (>=3)

## NOTES
# Consumer tests of sweetpotato in Kamwenge and Lira and of potato in Rakai and Kabale. Country is the country-of-residence column, not the file name.
# Sweetpotato color, sweetness, firmness, and mealiness are reshaped to one row per consumer and genotype. The check-all-that-apply columns were not reshaped.
# The potato workbooks record which product the consumer liked or disliked. They do not contain the same hedonic scores.

## ISSUES
# No field yield, fertilizer, or coordinates. Towns need georeferencing.
# Some potato country-of-residence cells are numeric codes. Those were not used as country.
# write_files: missing carob_contributor, yield, yield_moisture, yield_part, geo_from_source, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: consumer_, residence_country_, nationality_, color_, sweetness_, firmness_, mealiness_, trial_place_, liked_, disliked_. NA detected: country.

## NOT PROCESSED
# Check-all-that-apply columns in the sweetpotato workbooks (attractive, fibrous, orange, and the other yes/no descriptors).
# The two dictionary workbooks.

## NEW VARIABLES
# color_, sweetness_, firmness_, mealiness_: hedonic scores as recorded.
# liked_, disliked_, nationality_, residence_country_.


carob_script <- function(path) {

"
Dataset for: Sensory attributes and consumer acceptance of sweetpotato and potato genotypes in East Africa

Sensory attributes and consumer acceptance of sweetpotato and potato genotypes in East Africa
"

	uri <- "doi:10.21223/IU4LC1"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.45
	)

	chr <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x) | x == "NA"] <- NA_character_
		x
	}
	num <- function(x) {
		x <- trimws(as.character(x))
		ok <- grepl("^-?[0-9.]+$", x)
		x[!ok] <- NA_character_
		as.numeric(x)
	}
	pick <- function(r, pattern) {
		i <- grep(pattern, names(r))
		if (!length(i)) return(rep(NA_character_, nrow(r)))
		chr(r[[i[1]]])
	}
	sweet <- function(file, place) {
		r <- carobiner::read.excel(file)
		color <- grep("^Color-", names(r), value=TRUE)
		parts <- lapply(color, function(cn) {
			suffix <- sub("^Color-", "", cn)
			sw <- grep(paste0("^Sweetness-", suffix), names(r), value=TRUE, fixed=TRUE)
			fi <- grep(paste0("^Firmness-", suffix), names(r), value=TRUE, fixed=TRUE)
			me <- grep(paste0("^Mealiness-", suffix), names(r), value=TRUE, fixed=TRUE)
			data.frame(
				consumer_ = chr(r[[1]]),
				location = pick(r, "^Location/"),
				farmer_gender = pick(r, "^Gender$"),
				residence_country_ = pick(r, "^Country of residence$"),
				nationality_ = pick(r, "^Nationality$"),
				variety = trimws(suffix),
				color_ = if (length(cn)) num(r[[cn]]) else NA_real_,
				sweetness_ = if (length(sw)) num(r[[sw[1]]]) else NA_real_,
				firmness_ = if (length(fi)) num(r[[fi[1]]]) else NA_real_,
				mealiness_ = if (length(me)) num(r[[me[1]]]) else NA_real_,
				crop = "sweetpotato"
			)
		})
		d <- do.call(rbind, parts)
		d$trial_place_ <- place
		d
	}
	potato <- function(file, place) {
		r <- carobiner::read.excel(file)
		data.frame(
			consumer_ = chr(r[["Respondent ID"]]),
			location = pick(r, "^Location/"),
			farmer_gender = pick(r, "gender"),
			residence_country_ = pick(r, "Country of residence"),
			nationality_ = pick(r, "Nationality"),
			liked_ = pick(r, "like the most"),
			disliked_ = pick(r, "dislike the most"),
			crop = "potato",
			trial_place_ = place
		)
	}
	bybase <- function(pattern) ff[grepl(pattern, basename(ff))]
	d <- carobiner::bindr(
		sweet(bybase("^01_"), "Kamwenge"),
		sweet(bybase("^02_"), "Lira"),
		potato(bybase("^03_"), "Rakai"),
		potato(bybase("^04_"), "Kabale")
	)
	okc <- !is.na(d$residence_country_) & grepl("^[A-Za-z][A-Za-z .'-]+$", d$residence_country_)
	d$country <- NA_character_
	d$country[okc] <- tools::toTitleCase(tolower(d$residence_country_[okc]))
	d$is_survey <- TRUE
	d$on_farm <- FALSE
	d$trial_id <- paste(d$trial_place_, d$consumer_, d$variety, seq_len(nrow(d)), sep="_")

	carobiner::write_files(path, meta, d)
}
