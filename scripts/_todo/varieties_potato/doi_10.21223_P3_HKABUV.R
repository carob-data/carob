# R script for "carob"
# license: GPL (>=3)

## NOTES
# Greenhouse inoculations at CIP-Lima, May to November 2018. The dictionary says LB1 and LB2 are percent severity at 3 and 5 days after inoculation. Those readings are in the long table. severity_scale is 0-100 because the unit is Percentage.
# AUDPC unit is percent-days. It is not divided by 100. rAUDPC is a decimal. Two values are 85 and 90 and are left as recorded. SAUDPC is the Yuen and Forbes susceptibility scale. The dictionary describes 0 to 9, but the column reaches 16.17, so it is kept as late_blight_saudpc_ and not mapped to sAUDPC.
# The fieldbook has two identical Experiment columns. One is used. Genotype is an index within the accession, stored as genotype_. The accession (INSTN) is the variety. Taxon full name is joined from the material list.
# Country Peru and location CIP-Lima are in the description. The files have no coordinates.

## ISSUES
# Greenhouse variety screen. No yield, fertilizer, or field plot.
# CIP-Lima needs a coordinate. None is in the files.
# write_files: missing carob_contributor, yield, yield_part, yield_moisture, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date. unknown variables: genotype_, late_blight_saudpc_, days_after_inoculation_. out of bounds: AUDPC (1, 200), rAUDPC (0.005, 90).

## NEW VARIABLES
# genotype_: genotype index within an accession.
# days_after_inoculation_: days after inoculation.
# late_blight_saudpc_: SAUDPC column as recorded.

## NOT PROCESSED
# The second Experiment column duplicates the first.
# Material-list taxon code, family, and genus. The full taxon name is variety_type.


carob_script <- function(path) {

"
Dataset for: Screening for resistance to late blight in Peruvian wild potato species

This data set includes information of late blight severity readings at 3 and 5 days after inoculation under greenhouse conditions, calculated values of Area Under Disease Progress Curve (AUDPC) and Relative Area Under Disease Progress Curve (rAUDPC)  from experiments carried out in CIP-Lima during May to November 2018. Vegetal material tested belonged to genotypes from wild potato landraces accessions hold in CIP's potato germplasm.
"

	uri <- "doi:10.21223/P3/HKABUV"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4,
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

	f <- ff[basename(ff) == "Fieldbook.xlsx"]
	r <- carobiner::read.excel(f, sheet="Fieldbook")
	ml <- carobiner::read.excel(f, sheet="Material_list")

	variety <- trimws(as.character(r$INSTN))
	alias <- data.frame(
		variety = trimws(as.character(ml$Accession)),
		variety_type = trimws(as.character(ml[["Taxon full name"]])),
		stringsAsFactors = FALSE
	)
	alias <- alias[!duplicated(alias$variety) & nzchar(alias$variety), ]

	d <- data.frame(
		record_id = seq_len(nrow(r)),
		crop = "potato",
		variety = variety,
		variety_code = variety,
		treatment = variety,
		genotype_ = as.integer(r$Genotype),
		country = "Peru",
		location = "CIP-Lima",
		planting_date = "2018",
		trial_id = paste("CIP-Lima", as.integer(r[[1]]), "2018"),
		AUDPC = as.numeric(r$AUDPC),
		rAUDPC = as.numeric(r$rAUDPC),
		late_blight_saudpc_ = as.numeric(r$SAUDPC),
		pathogen = "Phytophthora infestans",
		stress = "disease",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE,
		stringsAsFactors = FALSE
	)
	one <- function(day, order, values) {
		data.frame(
			record_id = d$record_id,
			order = as.integer(order),
			days_after_inoculation_ = day,
			disease_severity = as.character(values),
			severity_scale = "0-100",
			disease = "potato late blight",
			stringsAsFactors = FALSE
		)
	}
	dlong <- rbind(one(3, 1, r$LB1), one(5, 2, r$LB2))
	d <- merge(d, alias, by="variety", all.x=TRUE, sort=FALSE)
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
