# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Each raw sheet has a pooled 3-year summary block at the bottom where some rows (Kari/Meyar in farmer sheets; MT/PBs at Reproductive in CPRS sheets) were mislabeled "2024-25" instead of "Pooled (3 years)"; everything from the first "Pooled (3 years)" row onward is excluded to avoid duplicating or mixing pooled averages with 2024-25 records.
# Treatment codes (CT, ZT, MT, PBs) map to land_prep_method (conventional, none, minimum tillage, permanent beds), but the files do not specify mulch rates or which on-station treatment included maize, so mulch_type and intercrops are not assigned.
# The abstract describes the CPRS station trial as RCBD with four replicates, whereas the data sheets contain three replicate columns (R1-R3).
# Pest and arthropod counts contain non-integer decimals (likely means per plant/unit), so they are kept in pest_count_ (numeric) and insect_abundance (character) rather than pest_incidence (integer).
# Coordinates were georeferenced from village/station names in Patna and Nalanda districts (geo_from_source = FALSE); verify village locations (especially Kurkuri, Faridpur, Korji, Phulelpur, Kari, Meyar) if higher spatial precision is needed.

## NEW
# disease: early blight, leaf curl, common scab (potato diseases not in values_disease)
# pest_species: leafminer, cutworm, pentatomid bug, spider mite (not in values_pest; station sheet specifies cutworm as Agrotis ipsilon)
# pest_count_: pest count as recorded (numeric, non-integer counts per sampling unit)

## MISSING
# yield, yield_moisture, yield_part, irrigated, N_fertilizer, P_fertilizer, K_fertilizer

## NOTES
# Three rabi potato seasons (2022-23, 2023-24, 2024-25) in Bihar, India: on-farm trials (CT vs ZT) in 6 villages of Patna district and 2 villages of Nalanda district (2024-25 only), plus on-station RCBD trial (CT, ZT, MT, PBs; variety Kufri Pukhraj) at ICAR-CPRI Regional Station, Patna.
# Disease severity (%) and arthropod counts across 10 evaluations (order 1-10, farmer fields) or 3 replicates x 3 growth stages (vegetative, reproductive, harvesting; CPRS Patna) are stored in long, including 2024-25 ant counts from columns 40-42 of the station insect sheet whose Rep 1-3 subheaders were blank.
# Natural enemies / non-pest arthropods (lady beetle, spider, ant) are stored in insect_species and insect_abundance; pests are stored in pest_species and pest_count_.

## NOT PROCESSED
# Pooled (3 years) summary blocks and Average / Standard Error columns in all four raw sheets.
# Summary-only sheets: disease average data, Average Agronomic trial_CPRS., Insect average data, Agronomic trial CPRS-Average (repeat means/SEs; Cut worm and Tuber moth damaged-tuber columns on the last sheet are entirely empty).
# 03_data_dictionary_pest_population.xlsx: variable codebook.

carob_script <- function(path) {

"
Dataset for: Pest Dynamics in Conservation Agriculture-Based Zero Tillage Potato Systems: A Three-Season Study in Bihar, India (2022-2025)

As part of the GIZ–PZTM project, the trial was established in 2021, while the pest dynamics study was conducted over three consecutive potato-growing seasons (2022–23, 2023–24, and 2024–25) at the ICAR–Central Potato Research Institute, Regional Station, Patna, Bihar, India, along with complementary farmer-field trials in Patna and Nalanda districts. The experiment was laid out in a randomized complete block design (RCBD) with four treatments combining different tillage, crop establishment, and residue mulch practices, each replicated four times. The potato variety Kufri Pukhraj was used across treatments, with maize included in one treatment. Complementary farmer-field trials were carried out in six villages of Patna district (Kurkuri, Faridpur, Korji, Maner, Danapur, Phulelpur) and two villages of Nalanda district (Kari, Meyar).
"

	uri <- "doi:10.21223/P3/YJM7KU"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP;ICAR",
		publication = NA,
		project = "GIZ-PZTM",
		design = "Randomized Complete Block Design (RCBD)",
		data_type = "experiment",
		treatment_vars = "land_prep_method",
		response_vars = "disease_severity;pest_count_;insect_abundance",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 85,
		carob_effort = 0
	)

	f1 <- ff[basename(ff) == "01_PZTM_Disease_Dynamics_2025.xlsx"]
	r1 <- carobiner::read.excel(f1, sheet="Disease field data_raw")
	f2 <- ff[basename(ff) == "02_PZTM_Disease_Dynamics_2026.xlsx"]
	r2 <- carobiner::read.excel(f2, sheet="Insect farmers field data_raw")
	f3 <- ff[basename(ff) == "02_Analysis_average_PZTM_Disease_Dynamics_2025.xlsx"]
	r3 <- carobiner::read.excel(f3, sheet="Agronomic trial_CPRS")
	f4 <- ff[basename(ff) == "04_Analysis_and_average_PZTM_Insect-Pest_2026.xlsx"]
	r4 <- carobiner::read.excel(f4, sheet="Agronomic trial_CPRS")

	# Drop the pooled 3-year summary block at the bottom of each sheet (including mislabeled "2024-25" rows inside it)
	before_pooled <- function(df, ycol) {
		y <- trimws(as.character(df[[ycol]]))
		idx <- which(y == "Pooled (3 years)")
		if (length(idx)) df <- df[seq_len(idx[1] - 1), ]
		df[!is.na(df[[ycol]]) & !is.na(df$Treatment), ]
	}

	r1 <- before_pooled(r1, "years")
	r2 <- before_pooled(r2, "years")
	r3 <- before_pooled(r3, "years")

	hdr <- names(r4)
	sub <- trimws(as.character(unlist(r4[1, ])))
	pestn <- hdr
	for (i in 2:length(pestn)) {
		if (grepl("^\\.\\.\\.", pestn[i])) pestn[i] <- pestn[i - 1]
	}
	# Columns 40-42 are Ant population Rep 1-3 (blank in row 1)
	ant_cols <- which(grepl("^Ant population", pestn))[1:3]
	sub[ant_cols] <- c("Rep 1", "Rep 2", "Rep 3")
	xi <- r4[-1, ]
	names(xi) <- make.unique(ifelse(is.na(sub) | !nzchar(sub), paste0("blank", seq_along(sub)), sub))
	xi <- before_pooled(xi, "years")

	place <- function(site) {
		s <- trimws(sub(",$", "", trimws(as.character(site))))
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

	pf <- place(r1$site)
	pi <- place(r2$site)
	fkeys <- data.frame(
		location = c(pf$location, pi$location),
		adm2 = c(pf$adm2, pi$adm2),
		treatment = c(trimws(as.character(r1$Treatment)), trimws(as.character(r2$Treatment))),
		yr = c(trimws(as.character(r1$years)), trimws(as.character(r2$years))),
		stringsAsFactors = FALSE
	)
	fkeys <- fkeys[!duplicated(paste(fkeys$location, fkeys$treatment, fkeys$yr, sep="|")), ]

	skeys <- rbind(
		data.frame(
			treatment = rep(trimws(as.character(r3$Treatment)), times=3),
			yr = rep(trimws(as.character(r3$years)), times=3),
			rep = rep(1:3, each=nrow(r3)),
			stringsAsFactors = FALSE
		),
		data.frame(
			treatment = rep(trimws(as.character(xi$Treatment)), times=3),
			yr = rep(trimws(as.character(xi$years)), times=3),
			rep = rep(1:3, each=nrow(xi)),
			stringsAsFactors = FALSE
		)
	)
	skeys <- skeys[!duplicated(paste(skeys$treatment, skeys$yr, skeys$rep, sep="|")), ]

	lp_map <- c(CT="conventional", ZT="none", MT="minimum tillage", PBs="permanent beds")

	df <- data.frame(
		trial_id = paste(fkeys$location, fkeys$yr, sep="_"),
		country = "India",
		adm1 = "Bihar",
		adm2 = fkeys$adm2,
		location = fkeys$location,
		planting_date = substr(fkeys$yr, 1, 4),
		harvest_date = paste0("20", substr(fkeys$yr, 6, 7)),
		season = "rabi",
		crop = "potato",
		variety = NA_character_,
		treatment = fkeys$treatment,
		land_prep_method = unname(lp_map[fkeys$treatment]),
		rep = NA_integer_,
		severity_scale = "0-100",
		stress = "pests;disease",
		on_farm = TRUE,
		is_survey = FALSE,
		yr_ = fkeys$yr
	)
	ds <- data.frame(
		trial_id = paste("CPRS_Patna", skeys$yr, sep="_"),
		country = "India",
		adm1 = "Bihar",
		adm2 = "Patna",
		location = "CPRS Patna",
		planting_date = substr(skeys$yr, 1, 4),
		harvest_date = paste0("20", substr(skeys$yr, 6, 7)),
		season = "rabi",
		crop = "potato",
		variety = "Kufri Pukhraj",
		treatment = skeys$treatment,
		land_prep_method = unname(lp_map[skeys$treatment]),
		rep = as.integer(skeys$rep),
		severity_scale = "0-100",
		stress = "pests;disease",
		on_farm = FALSE,
		is_survey = FALSE,
		yr_ = skeys$yr
	)
	d <- rbind(df, ds)
	d$record_id <- seq_len(nrow(d))
	rownames(d) <- NULL

	geo <- data.frame(
		location = c("CPRS Patna", "Kurkuri", "Faridpur", "Korji", "Maner", "Danapur", "Phulelpur", "Kari", "Meyar"),
		latitude = c(25.5922, 25.5657, 25.6601, 25.5583, 25.6460, 25.6358, 25.4334, 25.2520, 25.0515),
		longitude = c(85.0747, 85.0831, 84.9158, 85.0150, 84.8729, 85.0465, 85.6315, 85.4817, 85.3720),
		geo_uncertainty = c(1000, 3000, 5000, 5000, 5000, 5000, 5000, 5000, 5000),
		stringsAsFactors = FALSE
	)
	gi <- match(d$location, geo$location)
	d$latitude <- geo$latitude[gi]
	d$longitude <- geo$longitude[gi]
	d$geo_uncertainty <- geo$geo_uncertainty[gi]
	d$geo_from_source <- FALSE
	d$geo_source <- "georeferenced from station/village names"

	fkey <- paste(d$location, d$treatment, d$yr_, sep="|")
	fkey[!d$on_farm] <- NA_character_
	skey <- paste(d$treatment, d$yr_, d$rep, sep="|")
	skey[d$on_farm] <- NA_character_
	d$yr_ <- NULL

	fid <- match(paste(pf$location, trimws(as.character(r1$Treatment)), trimws(as.character(r1$years)), sep="|"), fkey)
	iid <- match(paste(pi$location, trimws(as.character(r2$Treatment)), trimws(as.character(r2$years)), sep="|"), fkey)

	align <- function(x, n) {
		if (length(x) == 1) rep(x, n) else x
	}
	empty_long <- function() {
		data.frame(
			record_id = integer(0),
			order = integer(0),
			growth_stage = character(0),
			disease = character(0),
			disease_severity = character(0),
			pest_species = character(0),
			pest_count_ = numeric(0),
			insect_species = character(0),
			insect_abundance = character(0)
		)
	}
	add_sev <- function(id, values, ord, stage, disease) {
		v <- as.numeric(values)
		ok <- !is.na(v) & !is.na(id)
		if (!any(ok)) return(empty_long())
		st <- tolower(trimws(as.character(align(stage, length(id)))))
		data.frame(
			record_id = id[ok],
			order = as.integer(align(ord, length(id)))[ok],
			growth_stage = st[ok],
			disease = disease,
			disease_severity = as.character(v[ok]),
			pest_species = NA_character_,
			pest_count_ = NA_real_,
			insect_species = NA_character_,
			insect_abundance = NA_character_
		)
	}
	add_arthropod <- function(id, values, ord, stage, species, is_pest) {
		v <- as.numeric(values)
		ok <- !is.na(v) & !is.na(id)
		if (!any(ok)) return(empty_long())
		st <- tolower(trimws(as.character(align(stage, length(id)))))
		data.frame(
			record_id = id[ok],
			order = as.integer(align(ord, length(id)))[ok],
			growth_stage = st[ok],
			disease = NA_character_,
			disease_severity = NA_character_,
			pest_species = if (is_pest) species else NA_character_,
			pest_count_ = if (is_pest) v[ok] else NA_real_,
			insect_species = if (is_pest) NA_character_ else species,
			insect_abundance = if (is_pest) NA_character_ else as.character(v[ok])
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

	pests <- c(ap="aphid", wf="whitefly", lm="leafminer", ptm="potato tuber moth", cp="cutworm", pbp="pentatomid bug")
	for (p in names(pests)) {
		for (i in 1:10) {
			k <- k + 1
			parts[[k]] <- add_arthropod(iid, r2[[paste0(p, i)]], i, r2$Stage, pests[[p]], TRUE)
		}
	}
	beneficials <- c(lbp="lady beetle", sp="spider")
	for (p in names(beneficials)) {
		for (i in 1:10) {
			k <- k + 1
			parts[[k]] <- add_arthropod(iid, r2[[paste0(p, i)]], i, r2$Stage, beneficials[[p]], FALSE)
		}
	}

	sblocks <- list(
		list("early blight", c("Early blight (Severity%)-R1", "Early blight (Severity%)-R2", "Early blight (Severity%)-R3")),
		list("potato late blight", c("Late blight (Severity %) R1", "Late blight (Severity %) R2", "Late blight (Severity %) R3")),
		list("leaf curl", c("Leaf curl (severity %) R1", "Leaf curl (severity %) R2", "Leaf curl (severity %) R3"))
	)
	for (b in sblocks) {
		for (r in 1:3) {
			sid <- match(paste(trimws(as.character(r3$Treatment)), trimws(as.character(r3$years)), r, sep="|"), skey)
			k <- k + 1
			parts[[k]] <- add_sev(sid, r3[[b[[2]][r]]], NA_integer_, r3$Stage, b[[1]])
		}
	}

	lab <- function(h) {
		h <- tolower(h)
		if (grepl("aphid", h)) return(list(name="aphid", pest=TRUE))
		if (grepl("white", h)) return(list(name="whitefly", pest=TRUE))
		if (grepl("leaf", h)) return(list(name="leafminer", pest=TRUE))
		if (grepl("cut", h)) return(list(name="cutworm", pest=TRUE))
		if (grepl("pentatom", h)) return(list(name="pentatomid bug", pest=TRUE))
		if (grepl("spider mite", h)) return(list(name="spider mite", pest=TRUE))
		if (grepl("lady|beetal|beetle", h)) return(list(name="lady beetle", pest=FALSE))
		if (grepl("spider", h)) return(list(name="spider", pest=FALSE))
		if (grepl("^ant", h)) return(list(name="ant", pest=FALSE))
		list(name=h, pest=TRUE)
	}
	for (r in 1:3) {
		cols <- which(sub == paste("Rep", r))
		for (col in cols) {
			sid <- match(paste(trimws(as.character(xi$Treatment)), trimws(as.character(xi$years)), r, sep="|"), skey)
			info <- lab(pestn[col])
			k <- k + 1
			parts[[k]] <- add_arthropod(sid, xi[[col]], NA_integer_, xi$Stage, info$name, info$pest)
		}
	}

	dlong <- do.call(rbind, parts)
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
