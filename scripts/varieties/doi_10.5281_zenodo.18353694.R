# R script for "carob"
# license: GPL (>=3)


## NON STANDARD VARIABLES
# package_id_: tricot package / block id (farmer × assigned variety set) from the source

# yield_problem_: semicolon-separated yield QC flags (e.g. "no mass unit"; "high yield")
# plot_area_problem_: semicolon-separated plot-size QC flags (e.g. "improbably large"; "likely mislabeled unit")
# location_problem_: semicolon-separated location QC flags (e.g. "outside country"; "missing coordinates")
# plot_area_unit_: original plot-size unit string from the source
# yield_mass_unit_: original yield mass unit string from the source

# plackett_luce_estimate_: Plackett–Luce worth parameter from rank_analysis (long; model output, included by exception)
# plackett_luce_se_: standard error of plackett_luce_estimate_
# plackett_luce_quasi_se_: quasi-SE from rank_analysis
# plackett_luce_quasi_var_: quasi-variance from rank_analysis


## ISSUES
# yield is only converted to kg/ha when plot area can be expressed in m2 and the mass
# unit is grams or kilograms. Other cases keep yield as NA and set yield_problem_ 

# Plot-size unit "Hectare"/"Acre" with large numeric sizes often looks mislabeled (tricot plots are
# typically a few–tens of m2); converted area is kept and flagged via plot_area_problem_.

# Location outliers (outside a country bounding box) keep their coordinates and are flagged in
# location_problem_; records with missing coordinates are also kept and flagged.

# Rank-analysis (Plackett–Luce) estimates are model outputs, included by exception in the long table
# (keyed by trial_id × variety × timing × rank_variable). Worth and SE are embedded in
# uncertainty_type (and SE in uncertainty) 

# ranks (all traits) are in the long table as rank_variable + rank_score.
# Long table also has variety (needed for genotype); 

# On-farm tricot trials. 
# Farmer ranks in long table (rank_variable + rank_score); Plackett–Luce estimates also in long.
# QC flags in yield_problem_/plot_area_problem_/location_problem_.
# "forages" mapped to "forage crop".


carob_script <- function(path) {

"
Global multi-crop agricultural trial data supported by citizen science

The triadic comparison of technologies (tricot) is a citizen science approach for testing technology options in their target environments, which has been applied to on-farm testing of crop varieties. ‘Triadic’ refers to the sets of three technology options that are compared by each participant. In the approach, participants are invited to test an anonymous set of three technologies (out of a larger number, generally between 5 to 20) randomly assigned. Between 2011 and 2025 the tricot approach was applied in more than 25 countries across Africa, Asia, Europe and Latin America with more than 30 crops.

tricot data v1 This release consolidates standardized trial outputs from participatory on-farm evaluations implemented through the tricot (triadic comparison of technology options) approach. This dataset supports re-use for variety evaluation, genotype-by-environment research, target population of environments (TPE) analyses, farmer preference modeling, and broader work on data-driven crop improvement and climate adaptation. Number of new studies: 41 Countries covered: 5 (BJ, ML, TZ, NG, RW) Crops covered: 10 Total participant observations added: ~12,355 participants
"

	uri <- "doi:10.5281/zenodo.18353694"
	group <- "varieties"
	ff <- carobiner::get_data(uri, path, group, filter=FALSE)

	meta <- carobiner::get_metadata(uri, path, group, major=14, minor=NA,
		data_organization = "BIOV; ABC",
		publication = NA,
		project = NA,
		design = "tricot",
		data_type = "compilation",
		treatment_vars = "variety",
		response_vars = "yield",
		notes = NA,
		carob_contributor = "Robert Hijmans",
		carob_LLM = "Composer",
		carob_date = "2026-08-06",
		carob_completion = 70,
		carob_effort = 3.5
	)

	jmeta <- tolower(c(paste0(yuri::simpleURI(uri), ".json"), ".zenodo.json", "metadata.json"))
	json_files <- ff[grepl("\\.json$", ff, ignore.case=TRUE)]
	json_files <- json_files[!tolower(basename(json_files)) %in% jmeta]
	json_files <- json_files[grepl("^[A-Za-z]+-[a-f0-9]+\\.json$", basename(json_files))]

	country_map <- c(BJ = "Benin", ML = "Mali", TZ = "Tanzania", NG = "Nigeria", RW = "Rwanda", ET = "Ethiopia")
	crop_map <- c(forages = "forage crop", commonbean = "common bean", chilipepper = "chili pepper", jutemallow="jute mallow")
	yield_part_map <- c(amaranth = "leaves", `mallow jute` = "leaves", okra = "fruit", groundnut = "seed", potato = "tubers",
			cowpea = "seed", `forage crop` = "aboveground biomass", maize = "grain",	soybean = "seed", `common bean` = "seed", 
			`chili pepper` = "fruit", sorghum = "grain", tomato = "fruit", wheat = "grain")


	first_col <- function(nms, patterns, exclude = "reason|loss|priorit|method|density|damag|freq") {
		for (p in patterns) {
			hit <- grep(p, nms, ignore.case=TRUE, value=TRUE)
			if (!is.null(exclude) && nzchar(exclude)) {
				hit <- hit[!grepl(exclude, hit, ignore.case=TRUE)]
			}
			if (length(hit) > 0) {
				return(hit[1])
			}
		}
		NA_character_
	}

	normalize_gender <- function(x) {
		x <- tolower(trimws(as.character(x)))
		x[x %in% c("m", "male", "man", "homme", "boy")] <- "male"
		x[x %in% c("f", "female", "woman", "femme", "girl")] <- "female"
		#x[!(x %in% c("male", "female"))] <- NA_character_
		x
	}

	# missing-value tokens seen in this dataset (set to NA before any coercion)
	char_na <- function(x, extra = character()) {
		x <- trimws(as.character(x))
		miss <- c("", "NA", "NaN", "N/A", "n/a", "null", "NULL", "None", "none",
			"No information provided", "Not specified", "not specified", extra)
		x[x %in% miss | tolower(x) %in% tolower(miss)] <- NA_character_
		x
	}

	# coerce after mapping missing tokens / comma decimals; no suppressWarnings
	to_number <- function(x, extra_na = character()) {
		if (is.numeric(x)) return(as.numeric(x))
		x <- char_na(x, extra_na)
		# European decimal comma on an otherwise plain number: "7,2" -> "7.2"
		x <- sub("^([+-]?[0-9]+),([0-9]+)$", "\\1.\\2", x)
		as.numeric(x)
	}

	to_integer <- function(x, extra_na = character()) {
		if (is.integer(x)) return(x)
		if (is.numeric(x)) return(as.integer(round(x)))
		x <- char_na(x, extra_na)
		as.integer(x)
	}

	# repair free-text plot sizes to a numeric area (m2) when the string encodes m2/dimensions;
	# otherwise a plain size number for use with the separate unit field.
	# returns list(value, already_m2, unparsed)
	repair_plot_size <- function(x) {
		n <- length(x)
		value <- rep(NA_real_, n)
		already_m2 <- rep(FALSE, n)
		unparsed <- rep(FALSE, n)
		if (is.numeric(x)) {
			return(list(value = as.numeric(x), already_m2 = already_m2, unparsed = unparsed))
		}

		raw <- char_na(x, c("Moyen", "Ras", "RAS", "Dense", "Xxx", "Xxxx", "O", "VARIE",
			"Laitue", "Tomate", "Vernonia", "Tchayo", "Crincrin", "Géant", "Geant", "Gant", "100%", "90%"))
		# normalize separators / encoding variants of ×
		s <- gsub("\u00d7|Ã\u0097", "x", raw)
		s <- gsub("\\*", "x", s)
		s <- tolower(trimws(s))
		s[s == ""] <- NA_character_

		# plain number (optional European comma)
		plain <- grepl("^[+-]?[0-9]+([.,][0-9]+)?$", s)
		plain[is.na(s)] <- FALSE
		if (any(plain)) {
			tmp <- sub(",", ".", s[plain], fixed = TRUE)
			value[plain] <- as.numeric(tmp)
		}

		# N m2 / N m² / N metres carrés / N metre carres / N mettre carré
		m2_pat <- grepl("^[+-]?[0-9]+([.,][0-9]+)?\\s*(m2|m²|m\\s*2|m\\s*²|metres?\\s*carres?|m[eè]tres?\\s*carr[eé]s?|mettre\\s*carr[eé]|m\\s*carr)", s)
		m2_pat[is.na(s)] <- FALSE
		m2_pat <- m2_pat & is.na(value)
		if (any(m2_pat)) {
			tmp <- sub("^([+-]?[0-9]+([.,][0-9]+)?).*", "\\1", s[m2_pat])
			tmp <- sub(",", ".", tmp, fixed = TRUE)
			value[m2_pat] <- as.numeric(tmp)
			already_m2[m2_pat] <- TRUE
		}

		# single length in meters without area (e.g. "10m") — keep length, not area
		m_only <- grepl("^[+-]?[0-9]+([.,][0-9]+)?\\s*m\\s*$", s)
		m_only[is.na(s)] <- FALSE
		m_only <- m_only & is.na(value)
		if (any(m_only)) {
			tmp <- sub("^([+-]?[0-9]+([.,][0-9]+)?).*", "\\1", s[m_only])
			tmp <- sub(",", ".", tmp, fixed = TRUE)
			value[m_only] <- as.numeric(tmp)
			unparsed[m_only] <- TRUE  # not a clear area; flag via caller
		}

		# dimensions: 4m by 1m, 5x2, 20mx6m, 20m sur 20m, 15/7, 1 sur 6, 4m*1m
		dim_pat <- grepl("([0-9]+([.,][0-9]+)?)\\s*m?\\s*(by|x|sur|/)\\s*([0-9]+([.,][0-9]+)?)\\s*m?", s)
		dim_pat[is.na(s)] <- FALSE
		dim_pat <- dim_pat & is.na(value)
		if (any(dim_pat)) {
			a <- sub("^.*?([0-9]+([.,][0-9]+)?)\\s*m?\\s*(by|x|sur|/).*", "\\1", s[dim_pat])
			b <- sub("^.*?([0-9]+([.,][0-9]+)?)\\s*m?\\s*(by|x|sur|/)\\s*([0-9]+([.,][0-9]+)?).*", "\\4", s[dim_pat])
			a <- sub(",", ".", a, fixed = TRUE)
			b <- sub(",", ".", b, fixed = TRUE)
			value[dim_pat] <- as.numeric(a) * as.numeric(b)
			already_m2[dim_pat] <- TRUE
		}

		# still unparsed text
		left <- !is.na(s) & is.na(value)
		unparsed[left] <- TRUE
		# leave value as NA for those

		list(value = value, already_m2 = already_m2, unparsed = unparsed)
	}

	# returns list(m2, unit_raw, flags character vector per row)
	parse_plot_area <- function(size, unit) {
		rep_sz <- repair_plot_size(size)
		s <- rep_sz$value
		u_raw <- trimws(as.character(unit))
		u <- tolower(u_raw)
		n <- length(s)
		m2 <- rep(NA_real_, n)
		flags <- rep(list(character()), n)

		miss_u <- is.na(u_raw) | !nzchar(u_raw) | u %in% c("na", "no information provided")
		for (i in which(miss_u)) flags[[i]] <- c(flags[[i]], "missing unit")
		for (i in which(rep_sz$unparsed)) flags[[i]] <- c(flags[[i]], "unparsed plot size text")

		is_m2 <- u %in% c("squared meters", "meter square", "m2", "sqm", "sq m")
		is_ac <- u %in% c("acre", "acres")
		is_ha <- u %in% c("hectare", "hectares", "ha")
		is_other <- !miss_u & !is_m2 & !is_ac & !is_ha

		# size string already encoded m2 / L×W
		m2[rep_sz$already_m2] <- s[rep_sz$already_m2]

		use_unit <- !rep_sz$already_m2 & !is.na(s)
		m2[use_unit & is_m2] <- s[use_unit & is_m2]
		m2[use_unit & is_ac] <- s[use_unit & is_ac] * 4046.8564224
		m2[use_unit & is_ha] <- s[use_unit & is_ha] * 10000
		for (i in which(is_other & use_unit)) flags[[i]] <- c(flags[[i]], "unknown unit")

		# tricot plots are typically small; ha/acre with size>>1 often means mislabeled m2
		mis_ha <- use_unit & is_ha & !is.na(s) & s > 1
		mis_ac <- use_unit & is_ac & !is.na(s) & s > 0.1
		for (i in which(mis_ha | mis_ac)) {
			flags[[i]] <- c(flags[[i]], "likely mislabeled unit")
		}

		large <- !is.na(m2) & m2 > 350
		for (i in which(large)) flags[[i]] <- c(flags[[i]], "improbably large")
		tiny <- !is.na(m2) & m2 > 0 & m2 < 1
		for (i in which(tiny)) flags[[i]] <- c(flags[[i]], "improbably small")
		miss_s <- is.na(s) & !miss_u & !rep_sz$unparsed
		for (i in which(miss_s)) flags[[i]] <- c(flags[[i]], "missing size")

		list(m2 = m2, unit_raw = u_raw, flags = flags)
	}

	mass_to_kg <- function(value, unit) {
		# value already numeric from to_number()
		v <- as.numeric(value)
		u_raw <- trimws(as.character(unit))
		u <- tolower(gsub("\\.$", "", u_raw))
		out <- rep(NA_real_, length(v))
		is_g <- u %in% c("g", "gram", "grams", "gra", "grans")
		is_kg <- u %in% c("kg", "kgs", "kilo", "kilos")
		out[is_g] <- v[is_g] / 1000
		out[is_kg] <- v[is_kg]
		out
	}

	mass_unit_flag <- function(unit) {
		u_raw <- trimws(as.character(unit))
		u <- tolower(gsub("\\.$", "", u_raw))
		n <- length(u)
		flags <- rep(list(character()), n)
		miss <- is.na(u_raw) | !nzchar(u_raw) | u %in% c("na", "no unit used", "no information provided")
		for (i in which(miss)) flags[[i]] <- c(flags[[i]], "no mass unit")
		ok <- u %in% c("g", "gram", "grams", "gra", "grans", "kg", "kgs", "kilo", "kilos")
		local <- !miss & !ok
		for (i in which(local)) flags[[i]] <- c(flags[[i]], "local mass unit")
		flags
	}

	collapse_flags <- function(flag_list) {
		vapply(flag_list, function(z) {
			z <- unique(z)
			if (length(z) == 0) NA_character_ else paste(z, collapse = "; ")
		}, character(1))
	}

	add_flag <- function(existing, add) {
		# existing: character NA/"a; b"; add: logical or character vector of new flags
		out <- existing
		for (i in seq_along(out)) {
			if (is.na(add[i]) || !nzchar(add[i]) || isFALSE(add[i])) next
			if (isTRUE(add[i])) next
			newf <- as.character(add[i])
			if (is.na(out[i]) || !nzchar(out[i])) {
				out[i] <- newf
			} else if (!grepl(newf, out[i], fixed=TRUE)) {
				out[i] <- paste(out[i], newf, sep = "; ")
			}
		}
		out
	}

	moment_priority <- function(moment) {
		m <- tolower(as.character(moment))
		pri <- rep(50L, length(m))
		pri[grepl("harvest|thresh|postharvest|breeder|physiolog|maturity", m)] <- 10L
		pri[grepl("vegetative|nursery|transplant|reproduct|flower", m)] <- 80L
		pri
	}

	# overall-type traits -> "preference"; others keep source trait name
	normalize_rank_variable <- function(trait) {
		t <- tolower(trimws(as.character(trait)))
		t[t %in% c("overall", "overallperf", "generalappreciation", "overallchar")] <- "preference"
		t
	}

	to_iso_date <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "NaN", "No information provided")] <- NA_character_
		out <- rep(NA_character_, length(x))
		ok <- grepl("^\\d{4}-\\d{2}-\\d{2}", x)
		out[ok] <- substr(x[ok], 1, 10)
		slash <- grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", x) & !ok
		if (any(slash)) {
			p <- strsplit(x[slash], "/", fixed=TRUE)
			out[slash] <- vapply(p, function(z) {
				sprintf("%s-%02d-%02d", z[3], as.integer(z[2]), as.integer(z[1]))
			}, character(1))
		}
		out
	}

	process_trial <- function(f) {
		x <- jsonlite::fromJSON(f)
		study <- x$metadata$study
		crop_raw <- tolower(trimws(x$metadata$crop$name))
		crop <- crop_raw
		country <- toupper(study$country)
		study_start <- to_iso_date(x$metadata$date$start)

		blocks <- x$block_data
		plots <- x$plot_data
		plots$genotype_name <- trimws(as.character(plots$genotype_name))
		plots$block_id <- as.character(plots$block_id)
		blocks$block_id <- as.character(blocks$block_id)

		bn <- names(blocks)
		size_col <- first_col(bn, c("trialplotsize$", "_plotsize$", "plotsurface$"))
		unit_col <- first_col(bn, c("plotsizeunit$", "plotsurfaceunit$"))
		yunit_col <- first_col(bn, "yieldunit$", exclude = NULL)
		plant_col <- first_col(bn, c(
			"plantingdatephone100_plantingdate$",
			"_plantingdate$",
			"_transplantingdate$"
		), exclude = "density|method|damag")
		gender_col <- first_col(bn, "_gender$", exclude = NULL)
		age_col <- first_col(bn, c("registration_age$", "socioeconomic_age$", "_age$"), exclude = NULL)

		if (!is.na(size_col) && !is.na(unit_col)) {
			pa <- parse_plot_area(blocks[[size_col]], blocks[[unit_col]])
		} else if (!is.na(size_col)) {
			pa <- parse_plot_area(blocks[[size_col]], rep(NA_character_, nrow(blocks)))
		} else {
			pa <- list(
				m2 = rep(NA_real_, nrow(blocks)),
				unit_raw = rep(NA_character_, nrow(blocks)),
				flags = rep(list(character()), nrow(blocks))
			)
			for (i in seq_len(nrow(blocks))) pa$flags[[i]] <- "missing size"
		}

		blocks$._plot_m2 <- pa$m2
		blocks$._plot_unit <- pa$unit_raw
		blocks$._plot_flags <- collapse_flags(pa$flags)
		# m2 usable for yield conversion: clear m2 unit and not improbably large
		u_plot <- tolower(trimws(as.character(pa$unit_raw)))
		blocks$._plot_m2_yield <- ifelse(
			u_plot %in% c("squared meters", "meter square", "m2", "sqm", "sq m") &
				!is.na(pa$m2) & pa$m2 >= 1 & pa$m2 <= 350,
			pa$m2, NA_real_
		)

		blocks$._yield_unit <- if (!is.na(yunit_col)) {
			as.character(blocks[[yunit_col]])
		} else {
			rep(NA_character_, nrow(blocks))
		}
		blocks$._mass_flags <- collapse_flags(mass_unit_flag(blocks$._yield_unit))
		blocks$._planting <- if (!is.na(plant_col)) {
			as.character(blocks[[plant_col]])
		} else {
			rep(NA_character_, nrow(blocks))
		}
		blocks$._gender <- if (!is.na(gender_col)) {
			normalize_gender(blocks[[gender_col]])
		} else {
			rep(NA_character_, nrow(blocks))
		}
		blocks$._age <- if (!is.na(age_col)) {
			to_number(blocks[[age_col]])
		} else {
			rep(NA_real_, nrow(blocks))
		}

		keys <- unique(plots[, c("block_id", "genotype_name", "plot"), drop=FALSE])
		keys <- keys[!is.na(keys$genotype_name) & nzchar(keys$genotype_name), , drop=FALSE]
		keys <- keys[!duplicated(paste(keys$block_id, keys$genotype_name)), , drop=FALSE]

		bkeep <- c(
			"block_id", "longitude", "latitude", "._plot_m2", "._plot_m2_yield",
			"._plot_unit", "._plot_flags", "._yield_unit", "._mass_flags",
			"._planting", "._gender", "._age"
		)
		bkeep <- bkeep[bkeep %in% names(blocks)]
		d <- merge(keys, blocks[, bkeep, drop=FALSE], by="block_id", all.x=TRUE)
		if (!("longitude" %in% names(d))) d$longitude <- NA_real_
		if (!("latitude" %in% names(d))) d$latitude <- NA_real_
		if (!("._plot_m2" %in% names(d))) d$._plot_m2 <- NA_real_
		if (!("._plot_m2_yield" %in% names(d))) d$._plot_m2_yield <- NA_real_
		if (!("._plot_unit" %in% names(d))) d$._plot_unit <- NA_character_
		if (!("._plot_flags" %in% names(d))) d$._plot_flags <- NA_character_
		if (!("._yield_unit" %in% names(d))) d$._yield_unit <- NA_character_
		if (!("._mass_flags" %in% names(d))) d$._mass_flags <- NA_character_
		if (!("._planting" %in% names(d))) d$._planting <- NA_character_
		if (!("._gender" %in% names(d))) d$._gender <- NA_character_
		if (!("._age" %in% names(d))) d$._age <- NA_real_

		d$yield <- NA_real_
		d$yield_moisture <- NA_real_
		d$yield_problem_ <- d$._mass_flags
		d$yield_problem_ <- add_flag(
			d$yield_problem_,
			ifelse(is.na(d$._plot_m2_yield), "no usable plot area", NA_character_)
		)

		yp <- plots[plots$trait == "yieldperse" & plots$value_type == "decimal", , drop=FALSE]
		if (nrow(yp) > 0) {
			yp$value_num <- to_number(yp$value)
			yp <- merge(
				yp,
				blocks[, c("block_id", "._plot_m2_yield", "._yield_unit"), drop=FALSE],
				by="block_id", all.x=TRUE
			)
			yp$kg <- mass_to_kg(yp$value_num, yp$._yield_unit)
			yp <- yp[!is.na(yp$kg) & !is.na(yp$._plot_m2_yield) & yp$._plot_m2_yield > 0, , drop=FALSE]
			if (nrow(yp) > 0) {
				yp$yha <- yp$kg / (yp$._plot_m2_yield / 10000)
				agg <- stats::aggregate(yha ~ block_id + genotype_name, data=yp, FUN=sum, na.rm=TRUE)
				names(agg)[3] <- "yield_yp"
				d <- merge(d, agg, by=c("block_id", "genotype_name"), all.x=TRUE)
				d$yield <- d$yield_yp
				d$yield_yp <- NULL
			}
		}

		gw <- plots[plots$trait == "grainyieldweight" & plots$value_type == "decimal", , drop=FALSE]
		if (nrow(gw) > 0) {
			gw$value_num <- to_number(gw$value)
			gw <- merge(gw, blocks[, c("block_id", "._plot_m2_yield"), drop=FALSE], by="block_id", all.x=TRUE)
			gw <- gw[!is.na(gw$value_num) & !is.na(gw$._plot_m2_yield) & gw$._plot_m2_yield > 0, , drop=FALSE]
			if (nrow(gw) > 0) {
				gw$yha <- gw$value_num / (gw$._plot_m2_yield / 10000)
				agg <- stats::aggregate(yha ~ block_id + genotype_name, data=gw, FUN=function(z) z[1])
				names(agg)[3] <- "yield_gw"
				d <- merge(d, agg, by=c("block_id", "genotype_name"), all.x=TRUE)
				fill <- is.na(d$yield) & !is.na(d$yield_gw)
				d$yield[fill] <- d$yield_gw[fill]
				d$yield_problem_[fill] <- add_flag(d$yield_problem_[fill], "grainyieldweight assumed kg")
				d$yield_gw <- NULL
			}
		}

		# yield QC flags (keep values)
		d$yield_problem_ <- add_flag(
			d$yield_problem_,
			ifelse(!is.na(d$yield) & d$yield > 25000, "high yield", NA_character_)
		)
		d$yield_problem_ <- add_flag(
			d$yield_problem_,
			ifelse(!is.na(d$yield) & d$yield > 150000, "yield above terminag max", NA_character_)
		)
		d$yield_problem_ <- add_flag(
			d$yield_problem_,
			ifelse(!is.na(d$yield) & d$yield < 0, "negative yield", NA_character_)
		)
		d$yield_problem_ <- add_flag(
			d$yield_problem_,
			ifelse(is.na(d$yield), "no yield", NA_character_)
		)

		mo <- plots[plots$trait %in% c("moisture", "moisturecontent") & plots$value_type == "decimal",
			, drop=FALSE]
		if (nrow(mo) > 0) {
			mo$value_num <- to_number(mo$value)
			mo <- mo[!is.na(mo$value_num), , drop=FALSE]
			mo <- mo[order(mo$block_id, mo$genotype_name), , drop=FALSE]
			mo <- mo[!duplicated(paste(mo$block_id, mo$genotype_name)), , drop=FALSE]
			mo <- mo[, c("block_id", "genotype_name", "value_num"), drop=FALSE]
			names(mo)[3] <- "ym"
			d <- merge(d, mo, by=c("block_id", "genotype_name"), all.x=TRUE)
			d$yield_moisture <- d$ym
			d$ym <- NULL
			d$yield_problem_ <- add_flag(
				d$yield_problem_,
				ifelse(
					!is.na(d$yield_moisture) & (d$yield_moisture < 0 | d$yield_moisture > 90),
					"moisture out of range",
					NA_character_
				)
			)
		}

		hd <- plots[plots$value_type == "date" & grepl("harvest", plots$trait, ignore.case=TRUE),
			, drop=FALSE]
		harvest_date <- rep(NA_character_, nrow(d))
		if (nrow(hd) > 0) {
			hd$value <- as.character(hd$value)
			hd$pri <- moment_priority(hd$collection_moment)
			hd <- hd[order(hd$block_id, hd$genotype_name, hd$pri), , drop=FALSE]
			hd <- hd[!duplicated(paste(hd$block_id, hd$genotype_name)), , drop=FALSE]
			i <- match(paste(d$block_id, d$genotype_name), paste(hd$block_id, hd$genotype_name))
			harvest_date <- hd$value[i]
		}

		variety_release_year <- rep(NA_integer_, nrow(d))
		variety_type <- rep(NA_character_, nrow(d))
		geno <- x$metadata$genotypes
		if (!is.null(geno) && is.data.frame(geno) && nrow(geno) > 0 && "genotype_name" %in% names(geno)) {
			gname <- trimws(as.character(geno$genotype_name))
			ry <- rep(NA_integer_, length(gname))
			if ("release_year" %in% names(geno)) {
				ry_chr <- char_na(geno$release_year)
				ry <- to_integer(ry_chr)
			}
			vt <- rep(NA_character_, length(gname))
			if ("role" %in% names(geno)) {
				vt <- trimws(as.character(geno$role))
				vt[is.na(vt) | vt %in% c("No information provided", "")] <- NA_character_
			}
			g2 <- data.frame(
				genotype_name = gname,
				variety_release_year = ry,
				variety_type = vt,
				stringsAsFactors = FALSE
			)
			g2 <- g2[!duplicated(g2$genotype_name), , drop=FALSE]
			i <- match(d$genotype_name, g2$genotype_name)
			variety_release_year <- g2$variety_release_year[i]
			variety_type <- g2$variety_type[i]
		}

		wide <- data.frame(
			trial_id = as.character(study$id),
			block_id = d$block_id,
			country = country,
			crop = crop,
			variety = d$genotype_name,
			treatment = d$genotype_name,
			plot_id = as.character(d$plot),
			longitude = to_number(d$longitude),
			latitude = to_number(d$latitude),
			planting_date = to_iso_date(d$._planting),
			harvest_date = to_iso_date(harvest_date),
			farmer_gender = d$._gender,
			age = to_number(d$._age),
			plot_area = to_number(d$._plot_m2),
			plot_area_unit_ = as.character(d$._plot_unit),
			plot_area_problem_ = as.character(d$._plot_flags),
			yield_mass_unit_ = as.character(d$._yield_unit),
			yield = to_number(d$yield),
			yield_moisture = to_number(d$yield_moisture),
			yield_problem_ = as.character(d$yield_problem_),
			variety_release_year = variety_release_year,
			variety_type = variety_type,
			stringsAsFactors = FALSE
		)
		wide$geo_from_source <- TRUE
		wide$on_farm <- TRUE
		wide$is_survey <- FALSE
		wide$irrigated <- as.logical(NA)
		wide$N_fertilizer <- as.numeric(NA)
		wide$P_fertilizer <- as.numeric(NA)
		wide$K_fertilizer <- as.numeric(NA)
		wide$yield_isfresh <- as.logical(NA)

		# observed ranks -> long (rank_variable + rank_score); all traits / moments kept
		rk <- plots[plots$value_type == "rank", , drop=FALSE]
		d_rk <- NULL
		if (nrow(rk) > 0) {
			d_rk <- data.frame(
				trial_id = as.character(study$id),
				package_id_ = as.character(rk$block_id),
				variety = trimws(as.character(rk$genotype_name)),
				timing = as.character(rk$collection_moment),
				rank_variable = normalize_rank_variable(rk$trait),
				rank_score = to_integer(rk$value),
				stringsAsFactors = FALSE
			)
			d_rk <- d_rk[!is.na(d_rk$rank_score) & nzchar(d_rk$variety), , drop=FALSE]
		}

		# Plackett–Luce rank_analysis (model estimates; included by exception) -> long
		ra <- x$rank_analysis
		d_pl <- NULL
		if (!is.null(ra) && is.data.frame(ra) && nrow(ra) > 0) {
			trait <- as.character(ra$trait)
			moment <- as.character(ra$collection_moment)
			est <- to_number(ra$estimate)
			se <- to_number(ra$SE)
			d_pl <- data.frame(
				trial_id = as.character(study$id),
				package_id_ = NA_character_,
				variety = trimws(as.character(ra$genotype_name)),
				timing = moment,
				rank_variable = normalize_rank_variable(trait),
				rank_score = NA_integer_,
				# SE in uncertainty; worth+context in uncertainty_type so clean long keeps PL results
				uncertainty = se,
				uncertainty_type = sprintf(
					"Plackett-Luce worth=%s; SE=%s [%s / %s]",
					format(est, scientific=FALSE, trim=TRUE),
					format(se, scientific=FALSE, trim=TRUE),
					moment, trait
				),
				plackett_luce_estimate_ = est,
				plackett_luce_se_ = se,
				plackett_luce_quasi_se_ = to_number(ra$quasiSE),
				plackett_luce_quasi_var_ = to_number(ra$quasiVar),
				stringsAsFactors = FALSE
			)
		}

		list(wide = wide, ranks = d_rk, pl = d_pl, study_start = study_start)
	}

	parts <- lapply(json_files, function(f) {
		tryCatch(
			process_trial(f),
			error = function(e) {
				stop(paste0(basename(f), ": ", conditionMessage(e)), call. = FALSE)
			}
		)
	})
	d <- do.call(rbind, lapply(parts, `[[`, "wide"))
	dlong_rk <- do.call(rbind, lapply(parts, `[[`, "ranks"))
	dlong_pl <- do.call(rbind, lapply(parts, `[[`, "pl"))

	d$crop <- carobiner::replace_values(d$crop, crop_map)
	d$country <- carobiner::replace_values(d$country, country_map)
	d$yield_part <- unname(yield_part_map[d$crop])

	# location flags (keep coordinates); after country names are standardized
	#miss_geo <- is.na(d$longitude) | is.na(d$latitude)
	#ok_box <- bbox_ok(d$country, d$longitude, d$latitude)
	#d$location_problem_ <- NA_character_
	#d$location_problem_[miss_geo] <- "missing coordinates"
	#d$location_problem_[!miss_geo & !ok_box] <- "outside country"

	d$record_id <- as.integer(seq_len(nrow(d)))

	# planting/harvest span QC (flag only; keep dates)
	ph <- !is.na(d$planting_date) & !is.na(d$harvest_date)
	if (any(ph)) {
		delta <- as.numeric(as.Date(d$harvest_date[ph]) - as.Date(d$planting_date[ph]))
		bad_short <- rep(FALSE, nrow(d))
		bad_long <- rep(FALSE, nrow(d))
		bad_short[ph] <- !is.na(delta) & delta < 45
		bad_long[ph] <- !is.na(delta) & delta > 366
		d$yield_problem_ <- add_flag(
			d$yield_problem_,
			ifelse(bad_short, "harvest within 45d of planting", NA_character_)
		)
		d$yield_problem_ <- add_flag(
			d$yield_problem_,
			ifelse(bad_long, "harvest >366d after planting", NA_character_)
		)
	}

	names(d)[names(d) == "block_id"] <- "package_id_"

	# long: observed ranks + Plackett–Luce (trial_id key; package_id_ links ranks to wide)
	starts <- vapply(parts, function(z) {
		s <- z$study_start
		if (length(s) != 1 || is.na(s)) NA_character_ else as.character(s)
	}, character(1))
	tids <- vapply(parts, function(z) as.character(unique(z$wide$trial_id)[1]), character(1))
	names(starts) <- tids

	dlong <- NULL
	if (!is.null(dlong_rk) && nrow(dlong_rk) > 0) {
		j <- match(
			paste(dlong_rk$trial_id, dlong_rk$package_id_, dlong_rk$variety, sep = "\r"),
			paste(d$trial_id, d$package_id_, d$variety, sep = "\r")
		)
		dlong_rk$date <- d$planting_date[j]
		miss <- is.na(dlong_rk$date) | !nzchar(as.character(dlong_rk$date))
		dlong_rk$date[miss] <- d$harvest_date[j][miss]
		still <- is.na(dlong_rk$date) | !nzchar(as.character(dlong_rk$date))
		dlong_rk$date[still] <- unname(starts[dlong_rk$trial_id[still]])
		dlong_rk$uncertainty <- NA_real_
		dlong_rk$uncertainty_type <- NA_character_
		dlong_rk$plackett_luce_estimate_ <- NA_real_
		dlong_rk$plackett_luce_se_ <- NA_real_
		dlong_rk$plackett_luce_quasi_se_ <- NA_real_
		dlong_rk$plackett_luce_quasi_var_ <- NA_real_
		dlong <- dlong_rk
	}
	if (!is.null(dlong_pl) && nrow(dlong_pl) > 0) {
		dlong_pl$date <- unname(starts[dlong_pl$trial_id])
		dlong <- if (is.null(dlong)) dlong_pl else rbind(dlong, dlong_pl)
	}

	carobiner::write_files(path, meta, d, long = dlong)
}
