# R script for "carob"
# license: GPL (>=3)

## NOTES
# Each data row is one clone in one set. A second header at the bottom repeats the checks, with a control-class column. That block is read on its own columns.
# The three evaluations in each of three trials are the long table. The sheet values are already means across plants. Pend is treated as not recorded. Trial averages, the last mean and max, the count of evaluations, and the mean of all evaluations are the summaries of those scores and are not stored again.
# The dictionary calls the scale an SES 5 point scale and then lists six classes, from 1 (extreme resistant, 0 percent) to 6 (highly susceptible). The abstract describes a 1 to 3 plant scale. Scores on the sheet reach 5, so they are not on that 3-point scale. They are stored as recorded.
# Percent symptomatic tubers is the first-trial column. Tuber latent infection is the separate percent column.
# The title and the sheet do not name a place. The Dataverse record lists Carhuaz and La Molina. Country and coordinates are not stored.
# One check code is stored as 380389.09999999998.

## ISSUES
# No yield, fertilizer, irrigation, planting date, or coordinates.
# Reaction type uses both "Moderately susceptible" and "Moderately Susceptible".
# The collection window in the Dataverse record is 1998 to 2001. The sheet has set numbers, not years.
# The abstract's 3-point wilt scale does not match the scores.
# write_files: missing yield, yield_moisture, yield_part, country, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: primary_trait_, check_role_, tuber_infection_pct_, tuber_latent_pct_, bw_trial_, eval_, wilt_score_. The long table has no date or depth column.

## NOT PROCESSED
# The dictionary, other than the scale description and the statement that the crop is potato.
# Derived wilt averages listed in the notes.
# Standard Evaluation Trial for Bacterial Wilt Resistance Dictionary.xls: documentation or a codebook, not an observation table.


## NEW VARIABLES
# primary_trait_: breeding group on the clone row.
# check_role_: control class on the check block.
# tuber_infection_pct_: percent symptomatic tubers on the first-trial block.
# tuber_latent_pct_: percent latent tuber infection.
# bw_trial_: 1, 2, or 3.
# eval_: 1, 2, or 3 within that trial.
# order on the long table is (trial - 1) * 3 + eval.
# wilt_score_: mean wilt score at that evaluation.


carob_script <- function(path) {

"
Dataset for: Standard evaluation trial for bacterial wilt (BW) resistance documentation of CIP advanced breeding clones

Bacterial wilt (BW) or brown rot is caused by the bacteria Ralstonia solanacearum (Smith), previously called Pseudomonas solanacearum. The disease is a major constraint to potato production in most tropical and subtropical regions and a threat to the potato seed industry in European countries (EPPO/CABI 1997; Elphinstone 2005). Latent infection is responsible for spread of the disease and overcoming of resistance. Disease symptoms include wilting, stunting and yellowing of the foliage and external symptoms on the tuber are visible at the harvest when infection is severe. Tuber symptoms are often described as brown rot. Clones or varieties are evaluated in fields naturally infested by R. solanacearum or inoculated with the bacteria. Wilt severity is evaluated using a three point scale (1=healthy plant, 2= below or equal to 50% plan wilted, 3= above 50% plant wilted and eventually dead). Al harvest, the percentage of symptomatic tubers is computed by recording the number of symptomatic tubers and the number of the healthy looking tubers in all plants of the experimental unit. To measure latent infections of tubers only asymptomatic tubers of selected clones are analyzed infecting with bacteria. For each clone and over all replications, a minimum of 30 tubers of any size above 30mm and without visible symptoms is selected and random in the field. Individual asymptomatic tuber are analyzed with the ELISA test or NCM- ELISA.
"

	uri <- "doi:10.21223/P3/U0WIYZ"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "wilt_score_;disease_severity",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "Standard Evaluation Trial for Bacterial Wilt Resistance Data.xls"]
	r <- carobiner::read.excel(f, sheet="Bacterial Wilt", col_names=FALSE, col_types="text")
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
	wilt_long <- function(record_id, mat, cols) {
		parts <- list()
		for (t in seq_along(cols)) {
			for (e in seq_along(cols[[t]])) {
				val <- num(mat[, cols[[t]][e]])
				keep <- !is.na(val)
				if (!any(keep)) next
				parts[[length(parts) + 1]] <- data.frame(
					record_id = record_id[keep],
					order = (t - 1L) * 3L + e,
					bw_trial_ = t,
					eval_ = e,
					wilt_score_ = val[keep]
				)
			}
		}
		do.call(rbind, parts)
	}

	main_i <- which(grepl("^CIP", clean(r[[2]])))
	header <- which(clean(r[[2]]) == "Control")
	ctrl_i <- which(seq_len(nrow(r)) > header & !is.na(clean(r[[4]])))
	m <- r[main_i, ]
	c <- r[ctrl_i, ]
	variety_m <- clean(m[[2]])
	variety_c <- clean(c[[4]])
	code_c <- clean(c[[3]])

	d1 <- data.frame(
		trial_id = paste0("BW", clean(m[[1]])),
		crop = "potato",
		variety = variety_m,
		variety_code = variety_m,
		treatment = variety_m,
		primary_trait_ = txt(m[[3]]),
		check_role_ = NA_character_,
		tuber_infection_pct_ = num(m[[8]]),
		tuber_latent_pct_ = num(m[[21]]),
		disease_severity = txt(m[[22]]),
		plot_id = paste(clean(m[[1]]), variety_m, sep="-"),
		stress = "disease",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d2 <- data.frame(
		trial_id = paste0("BW", clean(c[[1]])),
		crop = "potato",
		variety = variety_c,
		variety_code = code_c,
		treatment = variety_c,
		primary_trait_ = NA_character_,
		check_role_ = txt(c[[2]]),
		tuber_infection_pct_ = num(c[[9]]),
		tuber_latent_pct_ = num(c[[21]]),
		disease_severity = txt(c[[22]]),
		plot_id = paste(clean(c[[1]]), code_c, sep="-"),
		stress = "disease",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d1$record_id <- seq_len(nrow(d1))
	d2$record_id <- seq_len(nrow(d2)) + nrow(d1)
	long <- rbind(
		wilt_long(d1$record_id, m, list(4:6, 9:11, 13:15)),
		wilt_long(d2$record_id, c, list(5:7, 10:12, 14:16))
	)
	d <- rbind(d1, d2)
	rownames(d) <- NULL
	rownames(long) <- NULL

	carobiner::write_files(path, meta, d, long=long)
}
