# R script for "carob"
# license: GPL (>=3)

## NOTES
# Okra germplasm evaluated for aphids and other insects at Buea, Evodoula, Foumbot and Maroua, Cameroon.
# Plants in a plot on one observation date are averaged. There is no yield. The two Camponotus columns stay separate.
# Observation dates are day/month/year (14/05/2014 is unambiguous). Many ObsDate values are missing; ObsDay (1-7) is kept separately.
# Season 1/2 mapped to terminag first/second. Country is Cameroon from the dictionary.
# Aphid columns are described as counts. The values only reach 0-6, so they may be a score rather than a raw count. Stored as pest_incidence because the dictionary says "number".
# Leaf length and width by stratum have no unit (values about 0-48).

## ISSUES
# No yield, plot area, fertilizer, irrigation, or coordinates. Georeference Buea, Evodoula, Foumbot, Maroua.
# on_farm is not stated.
# Two unlabeled Num/Dam column pairs sit in front of the named natural-enemy columns. The dictionary only says "Number" and "Damages", so they were not mapped.
# Aphid "counts" of 0-6 need a human check against the field sheet (score vs count).
# Dataset metadata names CORAF/WECARD with IITA. That string is not in the organization vocabulary, so data_organization is IITA only.
# write_files: missing yield, N/P/K, irrigated, planting_date, harvest_date, longitude, latitude. Invalid pest_species (kept as recorded).

## NOT PROCESSED
# Num, Dam, Num.1, Dam.1: unlabeled number and damage columns.
# ID: row number.
# data dictionary: column definitions used above, not a data table.
# data_dictionary_okra_performance_and_effect_of_agroecology.csv: documentation or a codebook, not an observation table.

## NEW VARIABLES
# stratum_: bottom, middle, top or apex of the plant for insect counts and leaf size.
# obs_day_: ObsDay, 1-7, not a calendar date.
# bot_leaf_length_, bot_leaf_width_, mid_leaf_length_, mid_leaf_width_, top_leaf_length_, top_leaf_width_: leaf size, unit not stated.


carob_script <- function(path) {

"
Okra performance and effect of agroecology on resistance of various germplasms to aphids

IPM of aphid pests of vegetables
"

	uri <- "doi:10.25502/9qkm-qf24/d"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "pest_incidence",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.7,
		notes = NA
	)

	f <- ff[basename(ff) == "okra_performance_and_effect_of_agroecology.csv"]
	r <- read.csv(f, na.strings=c("", "NA"), check.names=TRUE)

	d <- data.frame(
		country = "Cameroon",
		location = r$Loc,
		trial_id = paste(r$Loc, r$Season, sep="_"),
		season = c("Season 1"="first", "Season 2"="second")[r$Season],
		date = as.character(as.Date(r$ObsDate, "%d/%m/%Y")),
		obs_day_ = as.integer(r$ObsDay),
		rep = as.integer(r$Rep),
		plot_id = as.character(r$Plot_No),
		plant_number_ = as.integer(r$Plant_Number),
		variety = r$Accession,
		treatment = r$Accession,
		bot_leaf_length_ = r$BotLength,
		bot_leaf_width_ = r$BotWidth,
		mid_leaf_length_ = r$MidLength,
		mid_leaf_width_ = r$MidWidth,
		top_leaf_length_ = r$TopLength,
		top_leaf_width_ = r$TopWidth,
		crop = "okra",
		yield_part = "fruit",
		on_farm = NA,
		is_survey = FALSE,
		geo_from_source = FALSE,
		record_id = seq_len(nrow(r))
	)

	add_pest <- function(value, species, stratum, severity=NULL, tag=1L) {
		out <- data.frame(
			record_id = d$record_id,
			pest_species = species,
			stratum_ = stratum,
			pest_incidence = as.integer(round(value)),
			pest_severity = if (is.null(severity)) NA_character_ else as.character(severity),
			tag_ = tag
		)
		out[!is.na(value), ]
	}

	long <- rbind(
		add_pest(r$AphidCountAtBot, "aphid", "bottom"),
		add_pest(r$AphidCountAtMid, "aphid", "middle"),
		add_pest(r$AphidCountAtTop, "aphid", "top"),
		add_pest(r$AphidCountAtApex, "aphid", "apex"),
		add_pest(r$WhiteflyAtBot, "whitefly", "bottom"),
		add_pest(r$WhiteflyAtMid, "whitefly", "middle"),
		add_pest(r$WhiteflyAtTop, "whitefly", "top"),
		add_pest(r$LeafBeetleNum, "leaf beetle", NA, r$LeafBeetleDam),
		add_pest(r$Syrph, "syrphid", NA),
		add_pest(r$Lbb, "ladybird beetle", NA),
		add_pest(r$Par, "parasitoid", NA),
		add_pest(r$Mum, "aphid mummy", NA),
		add_pest(r$Mil, "mealybug", NA),
		add_pest(r$Ph, "Pheidole", NA),
		add_pest(r$Camp1, "Camponotus", NA),
		add_pest(r$Camp2, "Camponotus", NA, tag=2L),
		add_pest(r$Myr, "Myrmicaria", NA),
		add_pest(r$SmallRed, "small red ant", NA)
	)
	# Aphid damage is one score per plant, not per stratum.
	aph <- data.frame(
		record_id = d$record_id,
		pest_species = "aphid",
		stratum_ = NA_character_,
		pest_incidence = NA_integer_,
		pest_severity = as.character(r$AphidDamSev),
		tag_ = 1L
	)
	aph <- aph[!is.na(r$AphidDamSev), ]
	long <- rbind(long, aph)
	long$date <- d$date[match(long$record_id, d$record_id)]

	# Average one row per plant to one row per group in `by`.
	# Numbers are means. A yes/no column stays yes/no when the plants agree, otherwise it is the proportion of plants.
	# Text that is all numbers is averaged. Other text that differs is joined with "; ". Full dates are averaged.
	mean_by <- function(x, by) {
		if (!nrow(x)) return(x)
		if ("plant_number_" %in% names(x)) x$plant_number_ <- NULL
		if ("record_id" %in% names(x)) x$record_id <- NULL
		by <- intersect(by, names(x))
		key <- do.call(paste, c(lapply(by, function(v) ifelse(is.na(x[[v]]), "", as.character(x[[v]]))), list(sep = "\r")))
		mean_na <- function(z) {
			z <- as.numeric(z)
			if (all(is.na(z))) NA_real_ else mean(z, na.rm = TRUE)
		}
		keep <- !duplicated(key)
		out <- x[keep, , drop = FALSE]
		num <- vapply(x, is.numeric, TRUE)
		logi <- vapply(x, is.logical, TRUE)
		num[names(num) %in% by] <- FALSE
		logi[names(logi) %in% by] <- FALSE
		for (v in names(x)[num]) {
			m <- tapply(x[[v]], key, mean_na)
			m <- unname(m[key[keep]])
			if (is.integer(x[[v]]) && all(is.na(m) | abs(m - round(m)) < 1e-8)) {
				m <- as.integer(round(m))
			}
			out[[v]] <- m
		}
		for (v in names(x)[logi]) {
			same <- tapply(x[[v]], key, function(z) {
				z <- z[!is.na(z)]
				!length(z) || all(z == z[1])
			})
			if (isTRUE(all(same))) {
				m <- tapply(x[[v]], key, function(z) {
					z <- z[!is.na(z)]
					if (!length(z)) NA else z[1]
				})
				out[[v]] <- unname(m[key[keep]])
			} else {
				m <- tapply(as.numeric(x[[v]]), key, mean_na)
				out[[v]] <- unname(m[key[keep]])
			}
		}
		for (v in names(x)) {
			if (v %in% by || num[v] || logi[v]) next
			u <- tapply(as.character(x[[v]]), key, function(z) {
				z <- z[!is.na(z)]
				z <- trimws(z)
				z <- z[nzchar(z)]
				if (!length(z)) return(NA_character_)
				if (all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", z))) {
					return(as.character(mean(as.Date(z))))
				}
				isn <- grepl("^-?[0-9]+(\\.[0-9]+)?$", z)
				parts <- character()
				if (any(isn)) parts <- c(parts, as.character(mean(as.numeric(z[isn]))))
				codes <- unique(z[!isn])
				parts <- c(parts, codes)
				if (length(parts) == 1) parts else paste(parts, collapse = "; ")
			})
			out[[v]] <- unname(u[key[keep]])
		}
		rownames(out) <- NULL
		out
	}

	# The long table is linked by record_id. After plants are averaged, that link is rebuilt.
	# long_by keeps species, date, and stage apart. It does not identify the plant.
	collapse_plants <- function(d, by, long, long_by) {
		key_of <- function(x) {
			do.call(paste, c(lapply(by, function(v) ifelse(is.na(x[[v]]), "", as.character(x[[v]]))), list(sep = "\r")))
		}
		long$plot_key <- key_of(d)[match(long$record_id, d$record_id)]
		d <- mean_by(d, by)
		d$plot_key <- key_of(d)
		d$record_id <- seq_len(nrow(d))
		long <- mean_by(long, c("plot_key", long_by))
		long$record_id <- d$record_id[match(long$plot_key, d$plot_key)]
		stopifnot(!any(is.na(long$record_id)))
		long$plot_key <- NULL
		d$plot_key <- NULL
		list(d = d, long = long)
	}

	cl <- collapse_plants(
		d,
		c("location", "trial_id", "season", "date", "obs_day_", "rep", "plot_id", "variety", "treatment"),
		long,
		c("pest_species", "stratum_", "date", "tag_")
	)
	d <- cl$d
	long <- cl$long
	long$tag_ <- NULL

	carobiner::write_files(path, meta, d, long=long)
}
