# R script for "carob"
# license: GPL (>=3)

## NOTES
# Greenhouse late-blight screen of potato landraces at CIP-Lima, May to December 2019.
# The description says severity was read 4 and 6 days after inoculation (stored with order 1, 2 and days_after_inoculation_ = 4, 6).
# AUDPC in percent-days is divided by 100 to match the terminag fraction-days standard (0-150).
# CIP-Lima coordinates (-12.078, -76.946, geo_from_source = FALSE) added from description.

## ISSUES
# Greenhouse screen: no yield, fertilizer, irrigation, or harvest date.

## NEW VARIABLES
# isolate_: Phytophthora infestans isolate identifier.
# saudpc_: standardized susceptibility scale (SAUDPC) as recorded.
# days_after_inoculation_: days after inoculation (4 or 6).

## NOT PROCESSED
# Experiment, Taxon code, Family, Genus, Taxon full name, SAUDPC_mean: taxonomy or a mean of SAUDPC.
# Material list parents and biological status, except the accession name used as variety_alt.
# SAUDPC is kept.
# 03_Datadictionary_Screening for resistance to late blight in traditional landraces under greenhouse conditions.xlsx: documentation or a codebook, not an observation table.


carob_script <- function(path) {

"
Dataset for: Screening for resistance to late blight in traditional landraces under greenhouse conditions - 2019

This data set includes information of late blight severity readings at 4 and 6 days after inoculation under greenhouse conditions, calculated values of Area Under Disease Progress Curve (AUDPC) and Relative Area Under Disease Progress Curve (rAUDPC) from experiments carried out in CIP-Lima during May to December 2019. Vegetal material tested was potato landraces accessions hold in CIP's potato germplasm.
"

	uri <- "doi:10.21223/66XOT9"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "AUDPC;rAUDPC",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 70,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "01_Data_Screening for resistance to late blight in traditional landraces under greenhouse conditions.xlsx"]
	r <- carobiner::read.excel(f)
	f1 <- ff[basename(ff) == "02_Material_list_Screening for resistance to late blight in traditional landraces under greenhouse conditions.xlsx"]
	r1 <- carobiner::read.excel(f1)

	id <- seq_len(nrow(r))
	d <- data.frame(
		record_id = id,
		variety = trimws(as.character(r$Accesion_number)),
		treatment = trimws(as.character(r$Accesion_number)),
		rep = as.integer(r$Rep),
		isolate_ = trimws(as.character(r$Isolate)),
		AUDPC = as.numeric(r$AUDPC) / 100,
		rAUDPC = as.numeric(r$rAUDPC),
		saudpc_ = as.numeric(r$SAUDPC)
	)
	alias <- data.frame(
		variety = trimws(as.character(r1[["Accession number"]])),
		variety_alt = trimws(as.character(r1[["Accession name"]]))
	)
	alias <- alias[!duplicated(alias$variety), ]
	d <- merge(d, alias, by="variety", all.x=TRUE)

	d$crop <- "potato"
	d$country <- "Peru"
	d$adm1 <- "Lima"
	d$location <- "CIP-Lima"
	d$latitude <- -12.078
	d$longitude <- -76.946
	d$geo_from_source <- FALSE
	d$planting_date <- "2019"
	d$trial_id <- paste("CIP_Lima_2019", d$isolate_, sep="_")
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$stress <- "disease"
	d$pathogen <- "Phytophthora infestans"

	pieces <- list(
		data.frame(record_id=id, order=1L, DAI=4L, disease_severity=as.character(r$LB1)),
		data.frame(record_id=id, order=2L, DAI=6L, disease_severity=as.character(r$LB2))
	)
	dlong <- do.call(rbind, pieces)
	dlong <- dlong[!is.na(dlong$disease_severity) & nzchar(dlong$disease_severity), ]
	dlong$disease <- "potato late blight"
	dlong$severity_scale <- "0-100"

	carobiner::write_files(path, meta, d, long=dlong)
}
