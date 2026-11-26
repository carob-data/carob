# R script for "carob"
# license: GPL (>=3)

## NOTES
# Potato pest and disease records from Peru. The dictionary describes departments, provinces, and districts as the administrative divisions of Peru, and the title states the crop was grown in Peru.
# The screening sheet records foliage late blight severity (%) on 14 dates. Dates are Excel serials and DAP are printed on the same columns. Planting date is the evaluation date minus DAP. AUDPC is divided by 100 to convert %-days to fraction-days (terminag standard, matching process_cip_lbvars).
# The incidence sheet is one row per sampled plant. Place codes do not match the place names on the geographic sheet, so those coordinates are not copied onto the plants.

## ISSUES
# Geographic sheet places are Quinua, Quichas, Candelaria, and Gochachupan, with DMS and no hemisphere letter. Incidence place codes are CC, KL1, KL2, KPP, KT, QA, QC, QE, and QJ. There is no crosswalk, so coordinates are omitted.
# SFLate Blight on the field_incidence sheet has some values exceeding 100 (kept as recorded in disease_severity).

## NEW VARIABLES
# place_code_: incidence locality code.
# tuber_number_: tuber number harvested per plant (count, not yield).

## NOT PROCESSED
# Early blight, Andean potato weevil, flea beetle, leaf beetle, Rhizoctonia, virus columns, and tuber-rot proportions. The dictionary says visual estimation and does not give a unit.
# Fields_Geographic_locations coordinates, because they cannot be matched to the incidence codes.
# Total plot area in square metres on that sheet, for the same reason.
# Data_dictionary.xls: documentation or a codebook, not an observation table.


carob_script <- function(path) {

"
Replication data for: Pests and diseases affecting potato landraces and bred varieties grown in Peru under indigenous farming system

The major pests and diseases were identified and quantified on thirteen potato landraces and three bred varieties cultivated in Peru. Late blight (Phytophthora infestans) was the primary biotic constraint affecting plants before flowering with an average severity of 24%. No other pathogens caused severe foliar disease, but black scurf (Rhizoctonia solani) was relatively common on tubers of some genotypes with incidence ranging from 4.30 to 33.33%. The viruses most generally considered important in potato seed degeneration, PVY and PLRV, were extremely rare, with 1.11 and 0.12 % incidence, respectively. Other viruses considered mild, such as PVX and PVS, were more common, with incidence of 28.23 and 22.29 %, respectively. Potato flea beetle (Epitrix spp.), potato leaf beetle (Diabrotica spp.) and Andean potato weevil (Premnotrypes spp.) were common, with incidence of 28.14, 18.75 and 13.61%, respectively. Potato landraces known as Ishkupuru, Lengua de vaca, Chaulina, Chaulina Tajacaja and Negro cayash were identified as potentially resistant to P. infestans.
"

	uri <- "doi:10.21223/P3/B6ACYT"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "compilation",
		treatment_vars = "variety",
		response_vars = "AUDPC;rAUDPC;disease_severity;disease_incidence",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 50,
		carob_effort = 0.55
	)

	f <- ff[basename(ff) == "Data_pests_and_diseases_incidence.xls"]
	sh <- readxl::excel_sheets(f)
	r1 <- as.data.frame(carobiner::read.excel(f, sheet=sh[3], col_names=FALSE))
	txt <- function(x) {
		z <- trimws(as.character(x))
		z[!nzchar(z) | z == "NA"] <- NA_character_
		z
	}
	num <- function(x) {
		z <- txt(x)
		ok <- is.na(z) | grepl("^-?[0-9]+(\\.[0-9]+)?$", z)
		z[!ok] <- NA_character_
		as.numeric(z)
	}

	lab <- txt(unlist(r1[2, ]))
	ev <- which(grepl("^[0-9]+ Ev$", lab))
	dap <- as.integer(num(unlist(r1[4, ev])))
	when <- as.Date(num(unlist(r1[3, ev])), origin="1899-12-30")
	plant <- format(when - dap, "%Y-%m-%d")
	planting <- if (length(unique(plant)) == 1) plant[1] else NA_character_
	audpc_col <- which(lab == "AUDPC")[1]
	raudpc_col <- which(lab == "rAUDPC")[1]

	body <- r1[6:nrow(r1), ]
	variety <- txt(body[[2]])
	keep <- !is.na(variety)
	body <- body[keep, ]
	variety <- variety[keep]
	d1 <- data.frame(
		variety = variety,
		rep = as.integer(num(body[[1]])),
		treatment = variety,
		trial_id = "late_blight_screening",
		planting_date = planting,
		AUDPC = num(body[[audpc_col]]) / 100,
		rAUDPC = num(body[[raudpc_col]]),
		country = "Peru",
		crop = "potato",
		pathogen = "Phytophthora infestans",
		geo_from_source = FALSE,
		on_farm = NA,
		is_survey = FALSE
	)
	d1$record_id <- seq_len(nrow(d1))
	longs <- lapply(seq_along(ev), function(i) {
		j <- ev[i]
		sev <- num(body[[j]])
		keep <- !is.na(sev)
		data.frame(
			record_id = d1$record_id[keep],
			order = i,
			date = format(when[i], "%Y-%m-%d"),
			DAP = dap[i],
			disease = "potato late blight",
			disease_severity = as.character(sev[keep]),
			severity_scale = "0-100",
			disease_incidence = NA_character_
		)
	})
	dlong1 <- do.call(rbind, longs)
	dlong1$record_id <- as.integer(dlong1$record_id)
	dlong1$DAP <- as.integer(dlong1$DAP)

	r2 <- as.data.frame(carobiner::read.excel(f, sheet=sh[2]))
	affected <- num(r2[["IFLate Blight"]])
	sf_sev <- num(r2[["SFLate Blight"]])
	d2 <- data.frame(
		variety = txt(r2$Variety),
		rep = NA_integer_,
		treatment = txt(r2$Variety),
		trial_id = "field_incidence",
		planting_date = NA_character_,
		AUDPC = NA_real_,
		rAUDPC = NA_real_,
		country = "Peru",
		crop = "potato",
		pathogen = "Phytophthora infestans",
		geo_from_source = FALSE,
		on_farm = TRUE,
		is_survey = FALSE,
		place_code_ = txt(r2[["Place code"]]),
		tuber_number_ = num(r2[["Tuber number harvested"]])
	)
	keep2 <- !is.na(d2$variety)
	d2 <- d2[keep2, ]
	affected <- affected[keep2]
	sf_sev <- sf_sev[keep2]
	d1$place_code_ <- NA_character_
	d1$tuber_number_ <- NA_real_
	d2$record_id <- seq_len(nrow(d2)) + nrow(d1)
	dlong2 <- data.frame(
		record_id = as.integer(d2$record_id),
		order = 1L,
		date = NA_character_,
		DAP = NA_integer_,
		disease = "potato late blight",
		disease_severity = ifelse(is.na(sf_sev), NA_character_, as.character(sf_sev)),
		severity_scale = NA_character_,
		disease_incidence = ifelse(is.na(affected), NA_character_, ifelse(affected == 1, "100", ifelse(affected == 0, "0", NA_character_)))
	)
	dlong2 <- dlong2[!is.na(dlong2$disease_severity) | !is.na(dlong2$disease_incidence), ]
	dlong <- rbind(dlong1, dlong2)
	d <- rbind(d1, d2)
	rownames(d) <- NULL
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
