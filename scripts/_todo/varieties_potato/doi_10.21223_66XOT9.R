# R script for "carob"
# license: GPL (>=3)

## NOTES
# Greenhouse late-blight screen of potato landraces at CIP-Lima, May to December 2019.
# The description says severity was read 4 and 6 days after inoculation. LB1 is stored at 4 days and LB2 at 6 days.
# AUDPC and rAUDPC are used as recorded. The dictionary does not say to rescale them.

## ISSUES
# No yield, fertilizer, or coordinates. CIP-Lima still needs a georeference.
# AUDPC reaches 180, above the terminag maximum of 150. Values are left as recorded.
# write_files: missing carob_contributor, yield, yield_moisture, yield_part, geo_from_source, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date. unknown variables: saudpc_, isolate_. out of bounds: AUDPC (0, 180).

## NOT PROCESSED
# Experiment, Taxon code, Family, Genus, Taxon full name, SAUDPC_mean: taxonomy or a mean of SAUDPC.
# Material list parents and biological status, except the accession name used as variety_alt.
# SAUDPC is kept.


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
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 65,
		carob_effort = 0.35
	)

	f <- ff[grepl("^01_Data_Screening", basename(ff))]
	r <- carobiner::read.excel(f)
	ml <- carobiner::read.excel(ff[grepl("^02_Material_list", basename(ff))])

	id <- seq_len(nrow(r))
	d <- data.frame(
		record_id = id,
		variety = trimws(as.character(r$Accesion_number)),
		treatment = trimws(as.character(r$Accesion_number)),
		rep = as.integer(r$Rep),
		isolate_ = trimws(as.character(r$Isolate)),
		AUDPC = as.numeric(r$AUDPC),
		rAUDPC = as.numeric(r$rAUDPC),
		saudpc_ = as.numeric(r$SAUDPC),
		stringsAsFactors = FALSE
	)
	alias <- data.frame(
		variety = trimws(as.character(ml[["Accession number"]])),
		variety_alt = trimws(as.character(ml[["Accession name"]])),
		stringsAsFactors = FALSE
	)
	alias <- alias[!duplicated(alias$variety), ]
	d <- merge(d, alias, by="variety", all.x=TRUE)

	d$crop <- "potato"
	d$country <- "Peru"
	d$location <- "CIP Lima"
	d$planting_date <- "2019"
	d$trial_id <- paste("CIP_Lima_2019", d$isolate_, sep="_")
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$stress <- "disease"
	d$pathogen <- "Phytophthora infestans"

	pieces <- list(
		data.frame(record_id=id, DAP=4L, disease_severity=as.character(r$LB1), stringsAsFactors=FALSE),
		data.frame(record_id=id, DAP=6L, disease_severity=as.character(r$LB2), stringsAsFactors=FALSE)
	)
	dlong <- do.call(rbind, pieces)
	dlong <- dlong[!is.na(dlong$disease_severity) & nzchar(dlong$disease_severity), ]
	dlong$disease <- "potato late blight"
	dlong$severity_scale <- "0-100"

	carobiner::write_files(path, meta, d, long=dlong)
}
