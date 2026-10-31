# R script for "carob"
# license: GPL (>=3)

## NOTES
# CIPVIR_2018_2020.csv has no header. Each line is one quoted record of semicolon-separated fields.
# Positions used here: record number, accession, plant, name, species, then the following result fields.
# Tokens POS, NEG, and SS are kept as recorded. Crop is sweetpotato because the assay is sweetpotato virus disease on crop wild relatives.
# The file does not name a country or coordinates.

## ISSUES
# No yield or fertilizer. This is a screenhouse ELISA assay.
# Column roles are positions, because the file has no header.
# write_files: ; yield, yield_moisture, yield_part, country, geo_from_source, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.
# write_files: unknown variables: species_recorded_, elisa_1_, elisa_2_.

## NEW VARIABLES
# species_recorded_: species text, for example I. leucantha
# elisa_1_: first result token
# elisa_2_: second result token

carob_script <- function(path) {

"
Dataset for: Screenhouse SPVD evaluations of 53 unique accessions of sweetpotato crop wild relative's species

A total of 53 accessions were evaluated for sweetpotato virus disease resistance. Virus carriers with SPVD were grafted on plants of parental accessions used for the crossing study. After four weeks of growth, plants were cut below the grafted node removing the virus carrier. Subsequently, plants were left to re-grow for another four weeks. On completion of the re-growth period plant samples were subjected to ELISA screening. Genotypes that were negative (no virus detected) for SPCSV or both virus were re-grafted with infected material and tested again for resistance. A negative control consisting of five I. trifida plants were grafted with healthy I. batatas (cv. Beauregard) scions which were tested negative. Scions of plants of those accessions that remained resistant were grafted on I. setosa as indicator plant to validate results.
"

	uri <- "doi:10.21223/MCB28H"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "elisa_1_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 30,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "CIPVIR_2018_2020.csv"]
	lines <- readLines(f, encoding="UTF-8", warn=FALSE)
	lines <- gsub('^"|"$', "", lines)
	lines <- lines[nzchar(lines)]
	parts <- strsplit(lines, ";", fixed=TRUE)
	n <- max(lengths(parts))
	m <- t(vapply(parts, function(z) { length(z) <- n; z }, character(n)))
	txt <- function(j) trimws(m[, j])
	d <- data.frame(
		crop = "sweetpotato",
		plot_id = paste(txt(1), txt(3), sep="_"),
		variety_code = txt(2),
		variety = txt(4),
		species_recorded_ = txt(5),
		elisa_1_ = txt(6),
		elisa_2_ = txt(7),
		trial_id = "screenhouse_SPVD",
		stress = "disease",
		on_farm = FALSE,
		is_survey = FALSE,
		irrigated = NA
	)
	d$treatment <- d$variety
	carobiner::write_files(path, meta, d)
}
