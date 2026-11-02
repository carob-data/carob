# R script for "carob"
# license: GPL (>=3)

## NOTES
# Root-knot nematode evaluation. The abstract names Meloidogyne incognita and two kinds of test: field infection and a greenhouse pot test.
# The sheet has one subheader row (replicate labels). Field scores are Rep I, Rep II, Rep III, and the average. Greenhouse scores are the first and second trial. The scale of those numbers is not printed.
# Reaction type is the class recorded for the clone.

## ISSUES
# The dataset coverage gives incomplete degree-and-minute positions for San Ramon and Ica, not one trial coordinate. Sites need georeferencing.
# No fertilizer, yield, planting date, or harvest date.
# Many field-infection cells are empty. The sheet also writes Pend where a score was not taken. Those cells are missing.
# The control block at the bottom of the sheet uses a different column order (cultivar name, then field replicates, then greenhouse trials).
# Meloidogyne incognita is not in the pathogen vocabulary, so the name is kept in the notes.
# Rows 210-212 name a reaction class and have no cultivar or CIP number. They are not plot records.
# missing variables: yield, yield_moisture, yield_part, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date
# unknown variables: set_, primary_trait_, field_rep1_, field_rep2_, field_rep3_, field_avg_, greenhouse_trial1_, greenhouse_trial2_, reaction_, record_type_

## NOT PROCESSED
# Data_Dictionary.xls.


carob_script <- function(path) {

"
Dataset for: Standard Evaluation Trial for Root Knot Nematode

Plant aboveground symptoms are not specific. Weak to growth and small, chlorotic leaves that wilt quickly in warm weather is typical. Infected roots show knots or galls of variable size depending on the extent of the damage. Tubers may also become infected and show galls, deformations, or internal symptoms of nematode disease. Strong attacks cause the early death of infected plants (CIP, 1996). Root Knot Nematode (Meloidogyne incognita) resistance trials were conducted for 205 clones at two endemic places. The set experiments include natural field exposure and greenhouse pot test. Different reactions type was recording: highly susceptible, moderately susceptible, susceptible and moderately resistant.
"

	uri <- "doi:10.21223/P3_U7PHJX"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "reaction_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "Root Knot Nematode.xls"]
	r <- carobiner::read.excel(f, na=c("", "NA"))
	names(r) <- trimws(names(r))
	num <- function(x) {
		x <- trimws(as.character(x))
		out <- rep(NA_real_, length(x))
		ok <- !is.na(x) & grepl("^-?[0-9]+(\\.[0-9]+)?$", x)
		out[ok] <- as.numeric(x[ok])
		out
	}
	txt <- function(x) trimws(as.character(x))
	blank <- function(x) {
		x <- txt(x)
		x[!nzchar(x) | is.na(x)] <- NA_character_
		x
	}

	clone <- blank(r$Clone)
	main <- grepl("^CIP", clone)
	m <- r[main, ]
	d1 <- data.frame(
		trial_id = "RKN_standard_evaluation",
		country = "Peru",
		crop = "potato",
		variety = blank(m$Clone),
		variety_code = blank(m$Clone),
		treatment = blank(m$Clone),
		set_ = blank(m[[1]]),
		primary_trait_ = blank(m[[3]]),
		field_rep1_ = num(m[[4]]),
		field_rep2_ = num(m[[5]]),
		field_rep3_ = num(m[[6]]),
		field_avg_ = num(m[[7]]),
		greenhouse_trial1_ = num(m[[8]]),
		greenhouse_trial2_ = num(m[[9]]),
		reaction_ = blank(m[[10]]),
		record_type_ = "clone",
		stress = "pests",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	# Control block: cultivar name is column 4, CIP number is column 3, scores start at column 5.
	cult <- blank(r[[4]])
	code <- blank(r[[3]])
	ctrl <- !is.na(cult) & !cult %in% c("Cultivar Name", "Rep. I", "Field Infection") &
		!is.na(code) & grepl("^[0-9]", code)
	c <- r[ctrl, ]
	d2 <- data.frame(
		trial_id = "RKN_standard_evaluation",
		country = "Peru",
		crop = "potato",
		variety = blank(c[[4]]),
		variety_code = blank(c[[3]]),
		treatment = blank(c[[4]]),
		set_ = blank(c[[1]]),
		primary_trait_ = NA_character_,
		field_rep1_ = num(c[[5]]),
		field_rep2_ = num(c[[6]]),
		field_rep3_ = num(c[[7]]),
		field_avg_ = num(c[[8]]),
		greenhouse_trial1_ = num(c[[9]]),
		greenhouse_trial2_ = num(c[[10]]),
		reaction_ = blank(c[[11]]),
		record_type_ = "control",
		stress = "pests",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d <- rbind(d1, d2)
	d$record_id <- seq_len(nrow(d))
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
