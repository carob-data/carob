# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The observation workbooks are marked restricted in the Dataverse file list and were not in the raw folder.
# Files present: 8_Data_Dictionary.xlsx.
# No data table was written.

## NOT PROCESSED
# The files above are dictionaries or documentation, not plot or household records.

carob_script <- function(path) {

"
Dataset for: Potato production under conservation agriculture: A trial in the locality of Pampa del Condor, Chugay district-La Libertad, Peru

An experimental potato trial based on conservation agriculture was conducted November 30th, 2023 to May 9th, 2024 with a total area of 783 m2 under support farmer Mr. Francisco Castillo Alvarado of the locality of Pampa del Condor, district of Chugay, province of Sánchez Carrión, La Libertad-Peru. For this trial, the improved potato variety INIA-325 Poderosa was used with a split-split plot design considering three factors: Types of tillage (minimum and zero tillage), residue management (incorporation and removal), and thickness of quinoa/barley of mulching (10, 20 and 30 cm). Combining these factors generated 12 treatments; and an additional control treatment under local conventional practical. Four replicates were established, with 52 plots of 3x3.6 m2 each with distance between plant and furrow of 0.3 m and 0.9 m respectively. The variables evaluated were: fresh tuber yield (t ha-1), benefit-cost ratio, carbon footprint, and soil analysis at different times during the vegetative period of the crop.
"

	uri <- "doi:10.21223/24VKN1"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = NA,
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 0,
		carob_effort = 0.15
	)

	# The Dataverse file list marks the observation workbooks restricted.
	# The raw folder only has documentation, so there is no table to standardize.
	stop("No observation file in data/raw/agronomy/doi_10.21223_24VKN1. Files present: 8_Data_Dictionary.xlsx")
}

