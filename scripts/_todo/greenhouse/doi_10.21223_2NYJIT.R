# R script for "carob"
# license: GPL (>=3)

## NOTES
# Greenhouse inoculations at CIP-Lima, June to December 2019. The description says severity was read 4 and 6 days after inoculation. Those are LB1 and LB2, kept in the long table (severity_scale = "0-100").
# AUDPC in percent-days is divided by 100 to match the terminag fraction-days standard (0-150).
# Genotype is a within-accession index (1 to 180) reused across accessions. It is stored as genotype_, not as a field replicate. The accession number is the variety. Four experiment-accession-genotype combinations occur twice. record_id keeps those rows.
# Country Peru and CIP-Lima coordinates (-12.078, -76.946, geo_from_source = FALSE) added from description.

## ISSUES
# Greenhouse variety screen. No yield, fertilizer, or field plot.
# SAUDPC reaches 11.8, so it is not the 0-1 sAUDPC term and not a 0-9 scale. It is kept as late_blight_saudpc_.

## NEW VARIABLES
# genotype_: genotype index within an accession.
# days_after_inoculation_: days after inoculation for a severity reading.
# lb_severity_: late blight severity reading. Unit is not stated.
# late_blight_saudpc_: SAUDPC column as recorded.

## NOT PROCESSED
# 02_Material_list and 03_Data_dictionary: definitions and passport fields not joined, because the accession number is already the variety key.
# Taxon code, family, and genus. The full taxon name is variety_type.
# 02_Material_list_Screening for resistance to late blight in Peruvian wild potato species  2019.xlsx: not read. sheet Material_list: Accession number, Collecting number/Breeder code/Biosafety code, Accession name, Population Group, Crop Name CIP, Genus, Taxon full name, Biological Status CIP, Parent Female, Parent Male. Observation records are missing from d until this file is standardized.
# 03_Data_dictionary_Screening for resistance to late blight in Peruvian wild potato species 2019.xlsx: documentation or a codebook, not an observation table.


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
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 75,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "01_Data_Screening for resistance to late blight in Peruvian wild potato species  2019.xlsx"]
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
		adm1 = "Lima",
		location = "CIP-Lima",
		latitude = -12.078,
		longitude = -76.946,
		planting_date = "2019",
		trial_id = paste("CIP-Lima", experiment, "2019"),
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
	dlong <- rbind(one(4, 1, r$LB1), one(6, 2, r$LB2))
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
