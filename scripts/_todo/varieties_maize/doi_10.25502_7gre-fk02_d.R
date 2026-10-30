# R script for "carob"
# license: GPL (>=3)

## NOTES
# Field evaluation of maize varieties in Sao Tome (COMPRAN / IITA). The CKAN notes field is a parasitoid bioassay sentence and does not describe this file.
# Used maize-evaluation-in-sao-tome.csv. Dropped 751 rows that were empty across every column.
# YIELD is t/ha from the grain-count method in the dictionary (kernel size 90000, 25.4 kg/bushel, harvest area 1 m2) and was multiplied by 1000 to kg/ha.
# BLOCK is the replicate (dictionary). RL and STEM_LODGING are yes/no, not counts. Disease and pest columns are 1-5 scores (dictionary) and were kept as long records.
# Cob row and grain counts are plot measurements. Average grains per row and grains per cob are computed from those counts and were not stored again.

## ISSUES
# No site name, coordinates, planting date, or fertilizer rates in the file. CKAN coverage_x/coverage_y are 243721 and 377878 (not degrees). Coverage dates 2025-05-27 to 2025-07-01 are not labeled as planting or harvest.
# Yield moisture is not reported. Do not assume a moisture.
# On-farm vs station is not stated.
# PL_HARV is only 0 or 1. E_HT includes 0. PLASP and EASP include 0 (dictionary scales start at 1). Left as recorded.
# About 135 rows have a variety but no yield. Striga / FAW / termite / borer / ear-rot scores are mostly a single value (1) where present; the dictionary mixes "number of plants" and a 1-5 score.
# Variety names repeat with and without hyphens (for example "TZE W" vs "TZE-W"); not merged.
# Pest names "termites" and "Spodoptera frugiperda" are not in the pest vocabulary.
# write_files will still report missing longitude, latitude, geo_from_source, planting_date, harvest_date, N/P/K fertilizer, irrigated, yield_moisture, on_farm, and NA yield.

## NOT PROCESSED
# maize-evaluation-in-sao-tome.csv, LAND_SIZE: constant 10000, the hectare used in the yield formula.
# maize-evaluation-in-sao-tome.csv, HARVEST_AREA: constant 1 m2 harvest area used in the yield formula.
# maize-evaluation-in-sao-tome.csv, N_EARS_HARVEST_AREA: constant 8, ears assumed in that 1 m2.
# maize-evaluation-in-sao-tome.csv, KERNEL_SIZE: constant 90000, medium-grain factor in the yield formula.
# maize-evaluation-in-sao-tome.csv, RATE_KG: constant 25.4, kg per bushel in the yield formula.
# maize-evaluation-in-sao-tome.csv, RATE_TONS: constant 1000, g per tonne in the yield formula.
# maize-evaluation-in-sao-tome.csv, AVG_N_GRAINS_PER_ROW: mean of the longest and shortest grain counts.
# maize-evaluation-in-sao-tome.csv, N_GRAIN_PER_COB: kernel rows times average grains per row.
# maize-evaluation-in-sao-tome.csv, trailing blank columns: empty.

## NEW VARIABLES
# plants_harvested_: plants per plot at harvest (PL_HARV). Unit: plants/plot.
# ears_harvested_: ears harvested per plot (N_EARS). Unit: ears/plot.
# kernel_rows_: grain rows on the cob (N_ROW_PER_COB). Unit: rows/cob.
# grains_longest_row_: grains on the longest cob row. Unit: grains/row.
# grains_shortest_row_: grains on the shortest cob row. Unit: grains/row.


carob_script <- function(path) {

"
Evaluation of improved varieties of maize for the enhancement of agricultural productivity and food security in São Tomé

Bioassay of parasitoids (natural enemies) of armyworms host range for biological control.
"

	uri <- "doi:10.25502/7gre-fk02/d"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = NA, minor = NA,
		data_organization = "IITA",
		publication = NA,
		project = "Commercialization, Agricultural Production Nutrition Project (COMPRAN)",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;plant_height;ear_height",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 68,
		carob_effort = 0.7
	)

	f <- ff[basename(ff) == "maize-evaluation-in-sao-tome.csv"]
	r <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
	r <- r[, nzchar(names(r)), drop = FALSE]
	filled_col <- vapply(r, function(z) {
		if (is.numeric(z)) return(any(!is.na(z)))
		z <- trimws(as.character(z))
		any(!is.na(z) & z != "")
	}, TRUE)
	r <- r[, filled_col, drop = FALSE]
	keep <- rep(FALSE, nrow(r))
	for (nm in names(r)) {
		z <- r[[nm]]
		if (is.numeric(z)) {
			keep <- keep | !is.na(z)
		} else {
			z <- trimws(as.character(z))
			keep <- keep | (!is.na(z) & z != "")
		}
	}
	r <- r[keep, ]

	yn <- function(x) {
		z <- tolower(trimws(as.character(x)))
		z[is.na(x) | z %in% c("", "na")] <- NA_character_
		z
	}

	d <- data.frame(
		trial_id = "1",
		plot_id = as.character(r$ID),
		rep = as.integer(r$BLOCK),
		variety = trimws(r$VARIETY),
		treatment = trimws(r$VARIETY),
		variety_type = "improved",
		plant_height = r$PLHT,
		ear_height = r$E_HT,
		root_lodging = yn(r$RL),
		stalk_lodging = yn(r$STEM_LODGING),
		husk = as.character(r$HC),
		aspect = r$PLASP,
		ear_aspect = r$EASP,
		# dictionary: YIELD in t/ha
		yield = r$YIELD * 1000,
		plants_harvested_ = r$PL_HARV,
		ears_harvested_ = r$N_EARS,
		kernel_rows_ = r$N_ROW_PER_COB,
		grains_longest_row_ = r$N_GRAINS_LONGEST_ROW,
		grains_shortest_row_ = r$N_GRAINS_SHORTEST_ROW,
		crop = "maize",
		yield_part = "grain",
		country = "Sao Tome and Principe",
		is_survey = FALSE,
		stringsAsFactors = FALSE
	)
	d$record_id <- as.integer(seq_len(nrow(d)))

	# scores are 1-5 in the dictionary (0 also occurs). Species names follow the dictionary.
	P <- local({
		long <- NULL
		add <- function(keep, disease = NA_character_, disease_severity = NA_character_,
			pest_species = NA_character_, pest_severity = NA_character_) {
			if (!any(keep, na.rm = TRUE)) return(invisible(NULL))
			n <- sum(keep)
			chr1 <- function(x) {
				if (length(x) == 1L) {
					if (is.na(x)) return(rep(NA_character_, n))
					return(rep(as.character(x), n))
				}
				as.character(x[keep])
			}
			x <- data.frame(
				record_id = d$record_id[keep],
				disease = chr1(disease),
				disease_severity = chr1(disease_severity),
				disease_incidence = rep(NA_character_, n),
				pest_species = chr1(pest_species),
				pest_severity = chr1(pest_severity),
				pest_incidence = rep(NA_integer_, n),
				DAP = rep(NA_integer_, n),
				order = rep(NA_integer_, n),
				stringsAsFactors = FALSE
			)
			if (is.null(long)) long <<- x else long <<- rbind(long, x)
		}
		environment()
	})
	score <- function(col, disease = NA_character_, pest = NA_character_) {
		v <- r[[col]]
		keep <- !is.na(v)
		if (!is.na(disease)) {
			P$add(keep, disease = disease, disease_severity = v)
		} else {
			P$add(keep, pest_species = pest, pest_severity = v)
		}
	}
	# Puccinia polysora is the species named in the column; vocabulary term is rust
	score("RUST_PUCCINIA_POLYSORA", disease = "rust")
	score("MAIZE_STREAK_VIRUS", disease = "maize streak virus")
	score("CURVILARIA_LUNATA", disease = "curvularia leaf spot")
	score("BLIGHT", disease = "maize leaf blight")
	score("GLS", disease = "gray leaf spot")
	score("E_ROT", disease = "ear rot")
	score("TERMITES", pest = "termites")
	score("WEED_STRIGA_HERMONTHICA", pest = "Striga")
	score("FAW", pest = "Spodoptera frugiperda")
	score("STEM_BORER", pest = "stem borer")
	score("MUSSIDIA", pest = "Mussidia nigrivenella")

	carobiner::write_files(path, meta, d, long = P$long)
}
