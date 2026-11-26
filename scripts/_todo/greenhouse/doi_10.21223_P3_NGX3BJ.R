# R script for "carob"
# license: GPL (>=3)

## NOTES
# Greenhouse late blight screening of potato landraces at CIP-Lima, Peru (-12.078, -76.946, geo_from_source = FALSE), May to December 2018.
# LB1 and LB2 are percent severity at 3 and 5 days after inoculation, stored in the long table under disease = "potato late blight", disease_severity, severity_scale = "0-100", order = 1..2.
# AUDPC in percent-days is divided by 100 to match the terminag fraction-days standard (0-150).
# rAUDPC is the relative value (0-1). Saudpc is the 0 to 9 susceptibility scale (stored in saudpc_ rather than 0-1 sAUDPC).

## ISSUES
# Greenhouse screen: no yield, fertilizer, irrigation, or harvest date.

## NEW VARIABLES
# saudpc_: 0-9 susceptibility scale (Saudpc) as recorded.
# days_after_inoculation_: days after inoculation (3 or 5).

## NOT PROCESSED
# Material_list: taxon names for the accessions.
# Data_dictionary.xls: the definitions used above.


carob_script <- function(path) {

"
Dataset for: Screening for resistance to late blight in traditional landraces under greenhouse conditions

This data set includes information of late blight severity readings at 3 and 5 days after inoculation under greenhouse conditions, calculated values of Area Under Disease Progress Curve (AUDPC) and Relative Area Under Disease Progress Curve (rAUDPC)  from experiments carried out in CIP-Lima during May to December 2018. Vegetal material tested were potato landraces accessions hold in CIP's potato germplasm.
"

	uri <- "doi:10.21223/P3/NGX3BJ"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "AUDPC;rAUDPC;disease_severity",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 70,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "Fieldbook _V1.xlsx"]
	r <- carobiner::read.excel(f, sheet="datos", na=c("", "NA"))
	names(r) <- trimws(names(r))
	variety <- trimws(as.character(r$INSTN))

	d <- data.frame(
		record_id = seq_len(nrow(r)),
		trial_id = "CIP_Lima_LB_2018",
		country = "Peru",
		adm1 = "Lima",
		location = "CIP-Lima",
		latitude = -12.078,
		longitude = -76.946,
		planting_date = "2018",
		crop = "potato",
		variety = variety,
		variety_code = variety,
		treatment = variety,
		rep = as.integer(r$REP),
		plot_id = paste(variety, r$REP, sep="_"),
		AUDPC = as.numeric(r$AUDPC) / 100,
		rAUDPC = as.numeric(r$rAUDPC),
		saudpc_ = as.numeric(r$Saudpc),
		pathogen = "Phytophthora infestans",
		stress = "disease",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	dlong <- rbind(
		data.frame(record_id = d$record_id, order = 1L, DAI = 3L, disease = "potato late blight", disease_severity = as.character(as.numeric(r$LB1)), severity_scale = "0-100"),
		data.frame(record_id = d$record_id, order = 2L, DAI = 5L, disease = "potato late blight", disease_severity = as.character(as.numeric(r$LB2)), severity_scale = "0-100")
	)
	dlong <- dlong[!is.na(dlong$disease_severity), ]

	carobiner::write_files(path, meta, d, long=dlong)
}
