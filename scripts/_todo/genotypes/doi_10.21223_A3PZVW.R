# R script for "carob"
# license: GPL (>=3)

## NOTES
# Accession rows are S1.Table_journal.pone.0232173.s006.xlsx (name, country, code).
# Crop is sweetpotato, from the dataset description.
# S1.Data is a marker-by-accession matrix (about 9670 marker rows). It is not expanded into one row per call.

## ISSUES
# This is a marker dataset, not a yield trial. Yield and fertilizer are not in the files.
# The table has a country for each accession and no coordinates.
# write_files: missing variables: carob_contributor; yield, yield_moisture, yield_part, geo_from_source, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.

## NOT PROCESSED
# S1.Data_journal.pone.0232173.s001.csv: SNP marker rows (AlleleID, sequence, chromosome position, and one column per accession). Not reshaped.

carob_script <- function(path) {

"
Dataset for: DArTSeq data generated from 662 parents of the global breeding program of the International Potato Center

Molecular markers dataset of 662 sweetpotato parents from CIP Peru Breeding platform genotyped using DArTSeq. The single nucleotide polymorphism (SNP) markers were analyzed to develop a low-density highly informative marker set for quality assurance and control in sweetpotato breeding.   The dataset was included in the article: Development of diagnostic SNP markers for quality assurance and control in sweetpotato. S1.Data and S1.Table
"

	uri <- "doi:10.21223/A3PZVW"
	group <- "genotypes"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "compilation",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 25,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "S1.Table_journal.pone.0232173.s006.xlsx"]
	r <- carobiner::read.excel(f, sheet="Sheet1")
	country <- trimws(as.character(r$Country))
	country[country == "Viet Nam"] <- "Vietnam"

	d <- data.frame(
		variety = trimws(as.character(r$Names)),
		variety_code = trimws(as.character(r$Code)),
		country = country,
		crop = "sweetpotato",
		trial_id = "CIP_sweetpotato_parents",
		on_farm = NA,
		is_survey = FALSE,
		irrigated = NA
	)
	d$plot_id <- as.character(seq_len(nrow(d)))

	carobiner::write_files(path, meta, d)
}
