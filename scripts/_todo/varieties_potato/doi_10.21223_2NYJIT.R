# R script for "carob"
# license: GPL (>=3)

## NOTES
# Greenhouse inoculations at CIP-Lima, June to December 2019. The description says severity was read 4 and 6 days after inoculation. Those are LB1 and LB2, kept in the long table.
# Genotype is a within-accession index (1 to 180) reused across accessions. It is stored as genotype_, not as a field replicate. The accession number is the variety. Four experiment-accession-genotype combinations occur twice. record_id keeps those rows.
# Country Peru and location CIP-Lima are in the description. The files have no coordinates.

## ISSUES
# Greenhouse variety screen. No yield, fertilizer, or field plot.
# CIP-Lima needs a coordinate if one is added later. None is in the files.
# AUDPC scale is not stated. Values reach 190 and are stored as recorded. LB readings reach 90. The unit is not stated.
# SAUDPC reaches 11.8, so it is not the 0-1 sAUDPC term and not a 0-9 scale. It is kept as late_blight_saudpc_.
# write_files: missing carob_contributor, yield, yield_part, yield_moisture, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date. unknown variables: late_blight_saudpc_, genotype_, days_after_inoculation_. out of bounds: AUDPC (0, 190).

## NEW VARIABLES
# genotype_: genotype index within an accession.
# days_after_inoculation_: days after inoculation for a severity reading.
# lb_severity_: late blight severity reading. Unit is not stated.
# late_blight_saudpc_: SAUDPC column as recorded.

## NOT PROCESSED
# 02_Material_list and 03_Data_dictionary: definitions and passport fields not joined, because the accession number is already the variety key.
# Taxon code, family, and genus. The full taxon name is variety_type.


carob_script <- function(path) {

"
Dataset for: Screening for resistance to late blight in Peruvian wild potato species - 2019

This data set includes information of late blight severity readings at 4 and 6 days after inoculation under greenhouse conditions, calculated values of Area Under Disease Progress Curve (AUDPC) and Relative Area Under Disease Progress Curve (rAUDPC) from experiments carried out in CIP-Lima during Jun to December 2019.
"

	uri <- "doi:10.21223/2NYJIT"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
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
		carob_completion = 70,
		carob_effort = 0.35
	)

	f <- ff[grepl("01_Data_Screening", basename(ff))]
	r <- carobiner::read.excel(f, na=c("", "NA"))

	variety <- trimws(as.character(r[["Accesion number"]]))
	rep <- as.integer(r$Genotype)
	experiment <- trimws(as.character(r$Experiment))
	d <- data.frame(
		record_id = seq_len(nrow(r)),
		crop = "potato",
		variety = variety,
		variety_code = variety,
		variety_type = trimws(as.character(r[["Taxon full name"]])),
		treatment = variety,
		genotype_ = rep,
		country = "Peru",
		location = "CIP-Lima",
		planting_date = "2019",
		trial_id = paste("CIP-Lima", experiment, "2019"),
		AUDPC = as.numeric(r$AUDPC),
		rAUDPC = as.numeric(r$rAUDPC),
		late_blight_saudpc_ = as.numeric(r$SAUDPC),
		pathogen = "Phytophthora infestans",
		stress = "disease",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	one <- function(day, order, values) {
		data.frame(
			record_id = d$record_id,
			order = as.integer(order),
			days_after_inoculation_ = day,
			disease_severity = as.character(values),
			disease = "potato late blight"
		)
	}
	dlong <- rbind(one(4, 1, r$LB1), one(6, 2, r$LB2))
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
