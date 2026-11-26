# R script for "carob"
# license: GPL (>=3)

## NOTES
# Each data row is one clone in one set. A second header at the bottom repeats the checks, with a control-class column. That block is read on its own columns.
# The three evaluations in each of three trials are the long table (disease = "bacterial wilt", disease_severity, order = 1..9).
# Percent symptomatic tubers is stored in tuber_infection_ (%), and latent infection in tuber_latent_ (%).
# Dataverse record lists Peru (Carhuaz and La Molina); country = "Peru", geo_from_source = FALSE (specific site per set not stated).

## ISSUES
# No yield, fertilizer, irrigation, planting date (collection window 1998-2001), or per-row site coordinates.
# Reaction type uses both "Moderately susceptible" and "Moderately Susceptible".
# The abstract's 3-point wilt scale does not match the 1-5 scores in the sheet.

## NOT PROCESSED
# The dictionary, other than the scale description and the statement that the crop is potato.
# Derived wilt averages listed in the notes.
# Standard Evaluation Trial for Bacterial Wilt Resistance Dictionary.xls: documentation or a codebook, not an observation table.


## NEW VARIABLES
# primary_trait_: breeding group on the clone row.
# check_role_: control class on the check block.
# tuber_infection_: percent symptomatic tubers on the first-trial block (%).
# tuber_latent_: percent latent tuber infection (%).
# bw_trial_: trial number (1, 2, or 3).
# eval_: evaluation number (1, 2, or 3) within that trial.


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
		response_vars = "disease_severity;tuber_infection_;tuber_latent_",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 55,
		carob_effort = 0
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
					disease = "bacterial wilt",
					disease_severity = as.character(val[keep])
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
		tuber_infection_ = num(m[[8]]),
		tuber_latent_ = num(m[[21]]),
		reaction_ = txt(m[[22]]),
		plot_id = paste(clean(m[[1]]), variety_m, sep="-"),
		pathogen = "Ralstonia solanacearum",
		stress = "disease",
		country = "Peru",
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
		tuber_infection_ = num(c[[9]]),
		tuber_latent_ = num(c[[21]]),
		reaction_ = txt(c[[22]]),
		plot_id = paste(clean(c[[1]]), code_c, sep="-"),
		pathogen = "Ralstonia solanacearum",
		stress = "disease",
		country = "Peru",
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
