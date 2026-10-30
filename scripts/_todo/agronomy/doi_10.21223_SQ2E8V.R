# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The local folder has the data dictionary only. The conservation-agriculture measurement files are marked restricted and were not extracted.
# Files on disk: 8_Data_Dictionary.xlsx, doi_10.21223_SQ2E8V.zip.
# No data table was written.

carob_script <- function(path) {

"
Dataset for: Potato production under conservation agriculture: A trial in the locality of Santa Fe, Chugay district-La Libertad, Peru

An experimental potato trial based on conservation agriculture was conducted November 28th, 2023 to May 7th, 2024 with a total area of 783 m2 under support farmer Mr. Faustino Marquina Ruiz of the locality of Santa Fe, district of Chugay, province of Sánchez Carrión, La Libertad-Peru. For this trial, the improved potato variety INIA-325 Poderosa was used with a split-split plot design considering three factors: Types of tillage (minimum and zero tillage), residue management (incorporation and removal), and thickness of quinoa/wheat of mulching (10, 20 and 30 cm). Combining these factors generated 12 treatments; and an additional control treatment under local conventional practical. Four replicates were established, with 52 plots of 3x3.6 m2 each with distance between plant and furrow of 0.3 m and 0.9 m respectively. The variables evaluated were: fresh tuber yield (t ha-1), benefit-cost ratio, carbon footprint, and soil analysis at different times during the vegetative period of the crop.
"

	uri <- "doi:10.21223/SQ2E8V"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "split-split plot",
		data_type = NA,
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 0,
		carob_effort = 0.1
	)

	message("No measurement table in the local raw files for ", uri)
	invisible(meta)
}
