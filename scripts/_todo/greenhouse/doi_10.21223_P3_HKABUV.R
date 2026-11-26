# R script for "carob"
# license: GPL (>=3)

## NOTES
# Greenhouse inoculations at CIP-Lima, May to November 2018. LB1 and LB2 are percent severity at 3 and 5 days after inoculation, stored in the long table (severity_scale = "0-100").
# AUDPC in percent-days is divided by 100 to match the terminag fraction-days standard (0-150). rAUDPC is a decimal (two rows have 85 and 90, left as recorded). SAUDPC reaches 16.17 and is kept as late_blight_saudpc_.
# The fieldbook has two identical Experiment columns; one is used. Genotype is an index within the accession (genotype_). Taxon full name is joined from Material_list into variety_type.
# CIP-Lima coordinates (-12.078, -76.946, geo_from_source = FALSE) added from description.

## ISSUES
# Greenhouse variety screen. No yield, fertilizer, or field plot.
# Two rAUDPC values are 85 and 90 (likely percentages instead of 0-1 proportions); left as recorded.

## NEW VARIABLES
# genotype_: genotype index within an accession.
# days_after_inoculation_: days after inoculation.
# late_blight_saudpc_: SAUDPC column as recorded.

## NOT PROCESSED
# The second Experiment column duplicates the first.
# Material-list taxon code, family, and genus. The full taxon name is variety_type.
# Data_dictionary.xls: documentation or a codebook, not an observation table.


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
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 75,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "Fieldbook.xlsx"]
	r <- carobiner::read.excel(f, sheet="Fieldbook")
	r1 <- carobiner::read.excel(f, sheet="Material_list")

	variety <- trimws(as.character(r$INSTN))
	alias <- data.frame(
		variety = trimws(as.character(r1$Accession)),
		variety_type = trimws(as.character(r1[["Taxon full name"]]))
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
		adm1 = "Lima",
		location = "CIP-Lima",
		latitude = -12.078,
		longitude = -76.946,
		planting_date = "2018",
		trial_id = paste("CIP-Lima", as.integer(r[[1]]), "2018"),
		AUDPC = as.numeric(r$AUDPC) / 100,
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
			DAI = as.integer(day),
			disease_severity = as.character(values),
			severity_scale = "0-100",
			disease = "potato late blight"
		)
	}
	dlong <- rbind(one(3, 1, r$LB1), one(5, 2, r$LB2))
	d <- merge(d, alias, by="variety", all.x=TRUE, sort=FALSE)
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
