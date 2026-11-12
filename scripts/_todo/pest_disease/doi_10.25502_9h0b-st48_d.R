# R script for "carob"
# license: GPL (>=3)

## NOTES
# effect-of-neem-seed-oil csv, read as Latin-1. Weekly counts on cabbage (chou) and kale, Cameroon, 22 Nov 2016 to 18 Feb 2017, 4 blocks, 5 plants.
# The five plants in a block are averaged on each week. Long-table counts are the mean for that block, week, leaf position, and stage.
# Treatments in the file are dose 1, dose 2, dose 3, and témoin (stored as control). The dose amounts are not in the table.
# engrais is fiente on every row, stored as poultry manure. No rate, so no N/P/K.
# Outer-leaf and inner-leaf counts are in the long table. Damage scores (dégats_*) are pest_severity in that same table.
# CKAN coverage_x 3.84803 and coverage_y 11.50208 are used as latitude and longitude (southern Cameroon). The site name is not in the files.
# Total* columns match the sum of the corresponding inner and outer counts where both are present. 0/1 incidence columns repeat whether a count is positive. Those columns are not stored.
# Growth stage "format,tete" is stored as head formation (the comma replaces a corrupted character in formation de tete).

## ISSUES
# Neem dose 1, 2, and 3 have no concentration or rate. No yield.
# Aphid outer/inner columns only take values 1-5 (never 0; 29 rows missing). They may be scores rather than counts. Stored as pest_incidence as the dictionary describes them as aphids on leaves.
# Entomopathogen counts include 1000, 1001, 1100, and 120. Left as recorded.
# The processionary-caterpillar column is "spodoptera" because the dictionary says spodopterans. Confirm that reading.
# No site name, planting date, or irrigation. Coverage start 2016-09-10 is not used as a planting date.
# write_files: missing yield, yield_moisture, N/P/K, irrigated, planting_date, harvest_date; pest names not in the vocabulary; carob_contributor empty.

## NOT PROCESSED
# Columns TotalHelula, TotPlutel, TotMchBlch, TotArrreign, TotSyrph, TotChenille, TotChP, TotEsc, TotEntomoP, IncTotTurnAphids, InTotCabAph, IncTotHelul, IncTotPlut. Sums of the inner and outer counts.
# Columns IncTurnAphids, InCabAph, IncHelul, IncPlut. 0/1 incidence flags.
# SN. Row number.
# Dictionary legend rows Cab, Turn, Aphi, Inc, Tot, Entomopath. Not observations.
# effect-of-neem-seed-oil-at-different-dos_dictionary.csv: documentation or a codebook, not an observation table.

## NEW VARIABLES
# leaf_number_: mean leaves on the sampled plants (nbre_de_feuilles).
# leaf_position_: outer or inner leaves.
# insect_stage_: apterous, alate, larva, pupa, or damage.
# organism_role_: pest, predator, or pathogen.


carob_script <- function(path) {

"
Effect of neem seed oil at different doses on insect pests of cabbage

Prelinimary testing of plant extracts as insecticides in organic cultivation
"

	uri <- "doi:10.25502/9h0b-st48/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = "4 blocks; neem seed oil doses and an untreated control; cabbage and kale",
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "pest_incidence;pest_severity",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 65,
		carob_effort = 0.8
	)

	f <- ff[basename(ff) == "effect-of-neem-seed-oil-at-different-doses-on-insect-pests-of-cabbage.csv"]
	r <- read.csv(f, check.names=FALSE, fileEncoding="latin1")
	stopifnot(names(r)[4] == "culture", names(r)[8] == "bloc", ncol(r) == 56)

	crop <- trimws(r$culture)
	crop[crop == "chou"] <- "cabbage"
	stage <- tolower(trimws(r$stade_dvlpt))
	growth <- rep(NA_character_, length(stage))
	growth[grepl("^v", stage)] <- "vegetative"
	growth[grepl("^m", stage)] <- "mature"
	growth[grepl("format", stage)] <- "head formation"
	pest <- tolower(trimws(r$pesticides))
	pest <- gsub("[^a-z0-9 ]", "", pest)
	pest <- trimws(gsub(" +", " ", pest))
	pest[pest == "tmoin"] <- "control"

	d <- data.frame(
		record_id = seq_len(nrow(r)),
		date = as.character(as.Date(r[["date_d'obs"]], format="%d/%m/%Y")),
		order = as.integer(r$semaine),
		crop = crop,
		growth_stage = growth,
		treatment = pest,
		block_id = as.character(r$bloc),
		plant_number_ = as.integer(r$no_de_plante),
		leaf_number_ = as.integer(r$nbre_de_feuilles),
		yield_part = "leaves",
		insecticide_used = pest != "control",
		OM_used = TRUE,
		OM_type = "poultry manure"
	)
	d$country <- "Cameroon"
	d$latitude <- 3.84803
	d$longitude <- 11.50208
	d$geo_from_source <- TRUE
	d$trial_id <- "1"
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$stress <- "pests"

	num <- function(i) as.integer(r[[i]])
	piece <- function(i, species, position, stage, role, what) {
		v <- num(i)
		data.frame(
			record_id = d$record_id,
			pest_species = species,
			leaf_position_ = position,
			insect_stage_ = stage,
			organism_role_ = role,
			pest_incidence = if (what == "count") v else NA_integer_,
			pest_severity = if (what == "severity") as.character(v) else NA_character_
		)
	}
	# Column positions follow the file header. See NOTES.
	dd <- rbind(
		piece(11, "Hellula", NA, "damage", "pest", "severity"),
		piece(12, "Plutella", NA, "damage", "pest", "severity"),
		piece(13, "aphids", NA, "damage", "pest", "severity"),
		piece(14, "turnip aphid", "outer", "apterous", "pest", "count"),
		piece(15, "turnip aphid", "outer", "alate", "pest", "count"),
		piece(16, "cabbage aphid", "outer", "apterous", "pest", "count"),
		piece(17, "cabbage aphid", "outer", "alate", "pest", "count"),
		piece(18, "Plutella", "outer", "larva", "pest", "count"),
		piece(19, "Plutella", "outer", "pupa", "pest", "count"),
		piece(20, "Hellula", "outer", "larva", "pest", "count"),
		piece(21, "whitefly", "outer", NA, "pest", "count"),
		piece(22, "spider", "outer", NA, "predator", "count"),
		piece(23, "syrphid", "outer", "larva", "predator", "count"),
		piece(24, "spodoptera", "outer", "larva", "pest", "count"),
		piece(25, "snail", "outer", NA, "pest", "count"),
		piece(26, "looper", "outer", "larva", "pest", "count"),
		piece(27, "entomopathogen", "outer", NA, "pathogen", "count"),
		piece(28, "turnip aphid", "inner", "apterous", "pest", "count"),
		piece(29, "turnip aphid", "inner", "alate", "pest", "count"),
		piece(30, "cabbage aphid", "inner", "apterous", "pest", "count"),
		piece(31, "cabbage aphid", "inner", "alate", "pest", "count"),
		piece(32, "Plutella", "inner", "larva", "pest", "count"),
		piece(33, "Plutella", "inner", "pupa", "pest", "count"),
		piece(34, "Hellula", "inner", "larva", "pest", "count"),
		piece(35, "whitefly", "inner", NA, "pest", "count"),
		piece(36, "spider", "inner", NA, "predator", "count"),
		piece(37, "syrphid", "inner", "larva", "predator", "count"),
		piece(38, "looper", "inner", "larva", "pest", "count"),
		piece(39, "entomopathogen", "inner", NA, "pathogen", "count")
	)
	dd <- dd[!(is.na(dd$pest_incidence) & is.na(dd$pest_severity)), ]

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
		c("date", "order", "crop", "growth_stage", "treatment", "block_id", "trial_id"),
		dd,
		c("pest_species", "leaf_position_", "insect_stage_", "organism_role_")
	)
	d <- cl$d
	dd <- cl$long

	carobiner::write_files(path, meta, d, dd)
}
