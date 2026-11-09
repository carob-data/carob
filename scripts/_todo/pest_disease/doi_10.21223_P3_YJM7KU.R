# R script for "carob"
# license: GPL (>=3)

## NOTES
# Pest and disease counts at farmer fields in Patna and Nalanda, and at CPRS Patna, for potato seasons 2022-23, 2023-24, and 2024-25.
# Farmer disease columns EB, LB, LC, BS, and CS are early blight, late blight, leaf curl, black scurf, and common scab severity in percent, evaluations 1 to 10.
# Farmer insect columns are counts at the same ten evaluations: aphid, whitefly, leafminer, potato tuber moth, cutworm, lady beetle, pentatomid bug, and spider. The dictionary unit is count. Values are not all integers, so they are not stored as pest_incidence.
# The station disease sheet has three replicates at vegetative, reproductive, and harvesting stages for early blight, late blight, and leaf curl.
# The station insect sheet uses the same stages and three replicates. Cutworm is labeled Agrotis ipsilon there. Lady beetle is spelled "beetal" on that sheet. That sheet also counts spider mites and ants.
# Phuleapur is stored as Phulelpur, the village name in the description. Kurkuri, Faridpur, Maner, and Danapur are the same villages with and without a Patna suffix.
# Kufri Pukhraj is the variety stated for the station treatments. The farmer sheets have no variety.
# Pooled three-year rows and the mean and standard-error columns are summaries of the plot values and are not stored.
# CT, ZT, MT, and PBs are kept as codes. The dictionary does not define them. The description says the station trial combined tillage, establishment, and residue, and that one treatment included maize.

## ISSUES
# Treatment codes are not defined in the files, so tillage and mulch are not assigned.
# The station design is described as RCBD with four replicates. The station sheets have three replicate columns. The farmer-field trials are not described as RCBD.
# No coordinates for the villages or for CPRS Patna.
# No fertilizer, irrigation, planting date, or yield.
# Lady beetle, spider, and spider mite are natural enemies or other arthropods counted with the pests.
# Early blight, leaf curl, black scurf, and common scab are not in the disease list. Late blight is stored as potato late blight.
# missing variables: yield, yield_moisture, yield_part, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date
# unknown variable: season_ (season?)
# unknown variables: evaluation_, pest_count_
# invalid terms: disease: black scurf, common scab, early blight, leaf curl
# invalid terms: pest_species: aphid, cutworm, lady beetle, leafminer, pentatomid bug, potato tuber moth, spider, spider mite, whitefly

## NOT PROCESSED
# Pooled (3 years) rows in the farmer and station sheets.
# Mean and standard-error columns.
# 02_Analysis_average and 04 average sheets. They repeat the raw means.
# Maize in one station treatment. The code for that treatment is not given.
# 03_data_dictionary_pest_population.xlsx: documentation or a codebook, not an observation table.


## NEW VARIABLES
# season_: potato season label, 2022-23, 2023-24, or 2024-25.
# evaluation_: farmer-field evaluation number, 1 to 10. No date is given.
# pest_count_: arthropod count as recorded. Not an integer and not converted per hectare.


carob_script <- function(path) {

"
Dataset for: Pest Dynamics in Conservation Agriculture-Based Zero Tillage Potato Systems: A Three-Season Study in Bihar, India (2022-2025)

As part of the GIZ–PZTM project, the trial was established in 2021, while the pest dynamics study was conducted over three consecutive potato-growing seasons (2022–23, 2023–24, and 2024–25) at the ICAR–Central Potato Research Institute, Regional Station, Patna, Bihar, India, along with complementary farmer-field trials in Patna and Nalanda districts. The experiment was laid out in a randomized complete block design (RCBD) with four treatments combining different tillage, crop establishment, and residue mulch practices, each replicated four times. The potato variety Kufri Pukhraj was used across treatments, with maize included in one treatment. Complementary farmer-field trials were carried out in six villages of Patna district (Kurkuri, Faridpur, Korji, Maner, Danapur, Phulelpur) and two villages of Nalanda district (Kari, Meyar).
"

	uri <- "doi:10.21223/P3/YJM7KU"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = "GIZ-PZTM",
		design = "Randomized Complete Block Design (RCBD)",
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "disease_severity;pest_count_",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.8
	)

	f1 <- ff[basename(ff) == "01_PZTM_Disease_Dynamics_2025.xlsx"]
	r1 <- carobiner::read.excel(f1, sheet="Disease field data_raw")
	f2 <- ff[basename(ff) == "02_PZTM_Disease_Dynamics_2026.xlsx"]
	r2 <- carobiner::read.excel(f2, sheet="Insect farmers field data_raw")
	f3 <- ff[basename(ff) == "02_Analysis_average_PZTM_Disease_Dynamics_2025.xlsx"]
	r3 <- carobiner::read.excel(f3, sheet="Agronomic trial_CPRS")
	f4 <- ff[basename(ff) == "04_Analysis_and_average_PZTM_Insect-Pest_2026.xlsx"]
	r4 <- carobiner::read.excel(f4, sheet="Agronomic trial_CPRS")

	place <- function(site) {
		s <- trimws(as.character(site))
		s <- sub(",$", "", s)
		s <- trimws(s)
		loc <- s
		adm2 <- rep("Patna", length(s))
		loc[s %in% c("Kurkuri, Patna", "Kurkuri")] <- "Kurkuri"
		loc[s %in% c("Faridpur, Patna", "Faridpur")] <- "Faridpur"
		loc[s %in% c("Maner, Patna", "Maner")] <- "Maner"
		loc[s %in% c("Danapur, Patna", "Danapur")] <- "Danapur"
		loc[s %in% c("Phulelpur, Patna", "Phulelpur", "Phuleapur")] <- "Phulelpur"
		loc[s == "Korji"] <- "Korji"
		kari <- grepl("^Kari", s)
		meyar <- grepl("^Meyar", s)
		loc[kari] <- "Kari"
		loc[meyar] <- "Meyar"
		adm2[kari | meyar] <- "Nalanda"
		data.frame(location=loc, adm2=adm2, stringsAsFactors=FALSE)
	}
	keep_year <- function(v) {
		y <- trimws(as.character(v))
		!is.na(y) & y != "Pooled (3 years)"
	}

	r1 <- r1[keep_year(r1$years), ]
	r2 <- r2[keep_year(r2$years), ]
	r3 <- r3[keep_year(r3$years) & !is.na(r3$Treatment), ]
	pf <- place(r1$site)
	pi <- place(r2$site)
	fkeys <- unique(data.frame(
		location = c(pf$location, pi$location),
		adm2 = c(pf$adm2, pi$adm2),
		treatment = c(as.character(r1$Treatment), as.character(r2$Treatment)),
		season_ = c(as.character(r1$years), as.character(r2$years)),
		stringsAsFactors = FALSE
	))
	fkeys <- fkeys[!duplicated(paste(fkeys$location, fkeys$treatment, fkeys$season_, sep="|")), ]

	hdr <- names(r4)
	sub <- trimws(as.character(unlist(r4[1, ])))
	pestn <- hdr
	for (i in 2:length(pestn)) {
		if (grepl("^\\.\\.\\.", pestn[i])) pestn[i] <- pestn[i - 1]
	}
	xi <- r4[-1, ]
	names(xi) <- make.unique(ifelse(is.na(sub) | !nzchar(sub), paste0("blank", seq_along(sub)), sub))
	xi <- xi[keep_year(xi$years) & !is.na(xi$Treatment), ]
	skeys <- rbind(
		data.frame(
			treatment = rep(as.character(r3$Treatment), times=3),
			season_ = rep(as.character(r3$years), times=3),
			rep = rep(1:3, each=nrow(r3)),
			stringsAsFactors = FALSE
		),
		data.frame(
			treatment = rep(as.character(xi$Treatment), times=3),
			season_ = rep(as.character(xi$years), times=3),
			rep = rep(1:3, each=nrow(xi)),
			stringsAsFactors = FALSE
		)
	)
	skeys <- skeys[!duplicated(paste(skeys$treatment, skeys$season_, skeys$rep, sep="|")), ]

	df <- data.frame(
		trial_id = paste(fkeys$location, fkeys$season_, sep="_"),
		country = "India",
		adm1 = "Bihar",
		adm2 = fkeys$adm2,
		location = fkeys$location,
		crop = "potato",
		variety = NA_character_,
		treatment = fkeys$treatment,
		season_ = fkeys$season_,
		rep = NA_integer_,
		stress = "pests;disease",
		on_farm = TRUE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	ds <- data.frame(
		trial_id = paste("CPRS Patna", skeys$season_, sep="_"),
		country = "India",
		adm1 = "Bihar",
		adm2 = "Patna",
		location = "CPRS Patna",
		crop = "potato",
		variety = "Kufri Pukhraj",
		treatment = skeys$treatment,
		season_ = skeys$season_,
		rep = as.integer(skeys$rep),
		stress = "pests;disease",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d <- rbind(df, ds)
	d$record_id <- seq_len(nrow(d))
	rownames(d) <- NULL
	fkey <- paste(d$location, d$treatment, d$season_, sep="|")
	fkey[d$on_farm == FALSE] <- NA_character_
	skey <- paste(d$treatment, d$season_, d$rep, sep="|")
	skey[d$on_farm == TRUE] <- NA_character_

	fid <- match(paste(pf$location, r1$Treatment, r1$years, sep="|"), fkey)
	iid <- match(paste(pi$location, r2$Treatment, r2$years, sep="|"), fkey)

	blank_row <- function(id, n) {
		data.frame(
			record_id = id,
			evaluation_ = rep(NA_integer_, n),
			growth_stage = rep(NA_character_, n),
			disease = rep(NA_character_, n),
			disease_severity = rep(NA_character_, n),
			pest_species = rep(NA_character_, n),
			pest_count_ = rep(NA_real_, n)
		)
	}
	align <- function(x, n) {
		if (length(x) == 1) x <- rep(x, n)
		x
	}
	add_sev <- function(id, values, evaluation, stage, disease) {
		v <- as.numeric(values)
		ok <- !is.na(v) & !is.na(id)
		if (!any(ok)) return(blank_row(integer(0), 0))
		data.frame(
			record_id = id[ok],
			evaluation_ = as.integer(align(evaluation, length(id)))[ok],
			growth_stage = as.character(align(stage, length(id)))[ok],
			disease = disease,
			disease_severity = as.character(v[ok]),
			pest_species = NA_character_,
			pest_count_ = NA_real_
		)
	}
	add_pest <- function(id, values, evaluation, stage, species) {
		v <- as.numeric(values)
		ok <- !is.na(v) & !is.na(id)
		if (!any(ok)) return(blank_row(integer(0), 0))
		data.frame(
			record_id = id[ok],
			evaluation_ = as.integer(align(evaluation, length(id)))[ok],
			growth_stage = as.character(align(stage, length(id)))[ok],
			disease = NA_character_,
			disease_severity = NA_character_,
			pest_species = species,
			pest_count_ = v[ok]
		)
	}

	parts <- list()
	k <- 0
	diseases <- c(EB="early blight", LB="potato late blight", LC="leaf curl", BS="black scurf", CS="common scab")
	for (p in names(diseases)) {
		for (i in 1:10) {
			k <- k + 1
			parts[[k]] <- add_sev(fid, r1[[paste0(p, i)]], i, NA_character_, diseases[[p]])
		}
	}
	pests <- c(ap="aphid", wf="whitefly", lm="leafminer", ptm="potato tuber moth", cp="cutworm", lbp="lady beetle", pbp="pentatomid bug", sp="spider")
	for (p in names(pests)) {
		for (i in 1:10) {
			k <- k + 1
			parts[[k]] <- add_pest(iid, r2[[paste0(p, i)]], i, as.character(r2$Stage), pests[[p]])
		}
	}

	sblocks <- list(
		list("early blight", c("Early blight (Severity%)-R1", "Early blight (Severity%)-R2", "Early blight (Severity%)-R3")),
		list("potato late blight", c("Late blight (Severity %) R1", "Late blight (Severity %) R2", "Late blight (Severity %) R3")),
		list("leaf curl", c("Leaf curl (severity %) R1", "Leaf curl (severity %) R2", "Leaf curl (severity %) R3"))
	)
	for (b in sblocks) {
		for (r in 1:3) {
			sid <- match(paste(r3$Treatment, r3$years, r, sep="|"), skey)
			k <- k + 1
			parts[[k]] <- add_sev(sid, r3[[b[[2]][r]]], NA_integer_, as.character(r3$Stage), b[[1]])
		}
	}

	lab <- function(h) {
		h <- tolower(h)
		if (grepl("aphid", h)) return("aphid")
		if (grepl("white", h)) return("whitefly")
		if (grepl("leaf", h)) return("leafminer")
		if (grepl("cut", h)) return("cutworm")
		if (grepl("lady|beetal|beetle", h)) return("lady beetle")
		if (grepl("pentatom", h)) return("pentatomid bug")
		if (grepl("spider mite", h)) return("spider mite")
		if (grepl("spider", h)) return("spider")
		if (grepl("^ant", h)) return("ant")
		h
	}
	for (r in 1:3) {
		cols <- which(sub == paste("Rep", r))
		for (col in cols) {
			sid <- match(paste(xi$Treatment, xi$years, r, sep="|"), skey)
			k <- k + 1
			parts[[k]] <- add_pest(sid, xi[[col]], NA_integer_, as.character(xi$Stage), lab(pestn[col]))
		}
	}

	x <- do.call(rbind, parts)
	rownames(x) <- NULL

	carobiner::write_files(path, meta, d, long=x)
}
