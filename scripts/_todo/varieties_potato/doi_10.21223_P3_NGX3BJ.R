# R script for "carob"
# license: GPL (>=3)

## NOTES
# Greenhouse late blight screening of potato landraces at CIP-Lima, May to December 2018.
# The dictionary says LB1 and LB2 are percent severity at 3 and 5 days after inoculation. Those two readings are the long table.
# AUDPC is in percent-days. That is not the fraction-based AUDPC term, so it is stored as audpc_pct_days_.
# rAUDPC is the relative value, a decimal. The susceptibility scale Saudpc is the 0 to 9 scale described in the dictionary. It is not the sAUDPC term, which runs from 0 to 1.

## ISSUES
# The abstract names CIP-Lima and does not name a country, coordinates, fertilizer, irrigation, or planting and harvest dates. May to December 2018 is the experiment period.
# No yield.
# write_files: missing variables: carob_contributor, yield, yield_moisture, yield_part, country, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variable: audpc_pct_days_ (AUDPC?). unknown variable: saudpc_ (sAUDPC?). time/depth: no time/depth variables in long records? unknown variables: dai_, severity_percent_.

## NOT PROCESSED
# Material_list: taxon names for the accessions.
# Data_dictionary.xls: the definitions used above.


carob_script <- function(path) {

"
Dataset for: Screening for resistance to late blight in traditional landraces under greenhouse conditions

This data set includes information of late blight severity readings at 3 and 5 days after inoculation under greenhouse conditions, calculated values of Area Under Disease Progress Curve (AUDPC) and Relative Area Under Disease Progress Curve (rAUDPC)  from experiments carried out in CIP-Lima during May to December 2018. Vegetal material tested were potato landraces accessions hold in CIP's potato germplasm.
"

	uri <- "doi:10.21223/P3_NGX3BJ"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "severity_percent_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.35
	)

	f <- ff[grepl("Fieldbook", basename(ff))]
	r <- carobiner::read.excel(f, sheet="datos", na=c("", "NA"))
	names(r) <- trimws(names(r))
	variety <- trimws(as.character(r$INSTN))

	d <- data.frame(
		trial_id = "CIP_Lima_LB_2018",
		location = "Lima",
		crop = "potato",
		variety = variety,
		variety_code = variety,
		treatment = variety,
		rep = as.integer(r$REP),
		plot_id = paste(variety, r$REP, sep="_"),
		audpc_pct_days_ = as.numeric(r$AUDPC),
		rAUDPC = as.numeric(r$rAUDPC),
		saudpc_ = as.numeric(r$Saudpc),
		stress = "disease",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE,
		stringsAsFactors = FALSE
	)

	dlong <- rbind(
		data.frame(plot_id = d$plot_id, trial_id = d$trial_id, dai_ = 3, severity_percent_ = as.numeric(r$LB1), stringsAsFactors = FALSE),
		data.frame(plot_id = d$plot_id, trial_id = d$trial_id, dai_ = 5, severity_percent_ = as.numeric(r$LB2), stringsAsFactors = FALSE)
	)

	carobiner::write_files(path, meta, d, long=dlong)
}
