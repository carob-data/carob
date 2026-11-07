# R script for "carob"
# license: GPL (>=3)

## NOTES
# Each data row is one clone in one set. A second header near the bottom repeats the checks, with a control-class column inserted. That block is read on its own columns.
# Natural field infection is the percent of plants infected. The header states that unit. The three trials are the long table. Zero is kept. Pend is treated as not recorded. The check block has no first or second trial percent, so only its third trial is stored.
# Reaction type is the word on the sheet. The dictionary lists those words on a 1 to 7 scale (extreme resistance through hypersensitive). The sheet does not store the numbers.
# The extreme-resistance columns are words (Positive, Negative, Mosaic, Necrosis, and combinations). The abstract says that test is done in a greenhouse. The spellings are kept as written, including Mosaico and the spacing variants.
# The title and the sheet do not name a place. The Dataverse box is La Molina, Lima, Peru. Country and coordinates are not stored.

## ISSUES
# No yield, fertilizer, irrigation, planting date, or coordinates.
# Reaction type uses both "Extreme Resistance" and "Extreme resistance".
# The collection window in the Dataverse record is 1999 to 2017. The sheet has set numbers, not years.
# write_files: missing yield, yield_moisture, yield_part, country, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: primary_trait_, check_role_, pvx_test_1_, pvx_test_2_, infected_plants_pct_. The long table has no date or depth column.

## NOT PROCESSED
# Data_Dictionary.xls, other than the reaction categories and the statement that the crop is potato.
# The per-trial infection columns after they are stacked in the long table.


## NEW VARIABLES
# primary_trait_: breeding group on the clone row (TPS, LB, BW, and the other sheet labels).
# check_role_: control class on the check block (Resistant, Susceptible, Resistant2).
# pvx_test_1_: first extreme-resistance observation, as written.
# pvx_test_2_: second extreme-resistance observation, as written.
# infected_plants_pct_: percent of plants infected in that trial. order is 1, 2, or 3.


carob_script <- function(path) {

"
Dataset for: Standard Evaluation Trial for Potato Virus X

Potato virus X (PVX) although less important alone, is commonly found in combined infections with other viruses thereby increasing the disease effect. PVX is easily transmitted by contact through knives; secateurs or movement of equipment, people and animals across a field. As PVY three different types of resistance to PVX have recognize: Extreme resistance, hypersensitive and relative resistance. Plants infected by PVX symptoms include mild leaf mottle or yellow mosaic between veins.
Field evaluation trials for relative resistance requires the testing material to be exposed to virus infection during two seasons in a location and season of high aphid population and virus incidence. The extreme resistance test is conducted under greenhouse conditions to complement the whole set of evaluations. A total of 319 advanced clones divided into 6 Subset was evaluated until now showing the percentage of field infection and extreme resistance results turning out on six levels of resistance: Hypersensitive, highly susceptible, susceptible, resistant and extreme resistance. The interpretation of these evaluations provides important information to guide potato breeding and selection programs, as well as for the recommendation of varieties for specific uses.
"

	uri <- "doi:10.21223/P3/R0TADB"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "infected_plants_pct_;virus_severity",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.55
	)

	f <- ff[basename(ff) == "Potato Virus X.xls"]
	r <- carobiner::read.excel(f, sheet="Potato Virus X", col_names=FALSE, col_types="text")
	clean <- function(x) {
		x <- gsub("\u00a0", " ", as.character(x), fixed=TRUE)
		x <- trimws(x)
		x[!nzchar(x)] <- NA_character_
		x
	}
	num <- function(x) {
		x <- clean(x)
		x[x %in% c("NA", "Pend", ".")] <- NA_character_
		as.numeric(x)
	}
	txt <- function(x) {
		x <- clean(x)
		x[x %in% c("NA", "Pend")] <- NA_character_
		x
	}
	pct_long <- function(record_id, mat, cols) {
		parts <- lapply(seq_along(cols), function(t) {
			val <- num(mat[, cols[t]])
			keep <- !is.na(val)
			if (!any(keep)) return(NULL)
			data.frame(
				record_id = record_id[keep],
				order = t,
				infected_plants_pct_ = val[keep]
			)
		})
		parts <- parts[!vapply(parts, is.null, logical(1))]
		if (!length(parts)) return(NULL)
		do.call(rbind, parts)
	}

	main_i <- which(grepl("^CIP[0-9]", clean(r[[2]])))
	ctrl_i <- which(grepl("^CIP[0-9]", clean(r[[3]])))
	m <- r[main_i, ]
	ctrl <- r[ctrl_i, ]
	variety_m <- clean(m[[2]])
	variety_c <- clean(ctrl[[4]])
	code_c <- clean(ctrl[[3]])

	d1 <- data.frame(
		trial_id = paste0("PVX", clean(m[[1]])),
		crop = "potato",
		variety = variety_m,
		variety_code = variety_m,
		treatment = variety_m,
		primary_trait_ = txt(m[[3]]),
		check_role_ = NA_character_,
		pvx_test_1_ = txt(m[[7]]),
		pvx_test_2_ = txt(m[[8]]),
		virus_severity = txt(m[[9]]),
		plot_id = paste(clean(m[[1]]), variety_m, sep="-"),
		stress = "disease",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d2 <- data.frame(
		trial_id = paste0("PVX", clean(ctrl[[1]])),
		crop = "potato",
		variety = variety_c,
		variety_code = code_c,
		treatment = variety_c,
		primary_trait_ = NA_character_,
		check_role_ = txt(ctrl[[2]]),
		pvx_test_1_ = txt(ctrl[[8]]),
		pvx_test_2_ = txt(ctrl[[9]]),
		virus_severity = txt(ctrl[[10]]),
		plot_id = paste(clean(ctrl[[1]]), code_c, sep="-"),
		stress = "disease",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d1$record_id <- seq_len(nrow(d1))
	d2$record_id <- seq_len(nrow(d2)) + nrow(d1)
	long <- rbind(
		pct_long(d1$record_id, m, 4:6),
		pct_long(d2$record_id, ctrl, 5:7)
	)
	d <- rbind(d1, d2)
	rownames(d) <- NULL
	rownames(long) <- NULL

	carobiner::write_files(path, meta, d, long=long)
}
