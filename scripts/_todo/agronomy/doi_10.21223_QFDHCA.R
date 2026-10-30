# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Measurement files are restricted on CIP Dataverse and were not downloaded. Only unrestricted documentation is in the raw folder, so no data table was built.
# Restricted files not on disk: 1_Emergency-Plant uniformity.tab; 2_Senescence.tab; 4_Harvest.tab; 5_Dry matter.tab; 6_FTY-MTY-BCR-CF.tab; 7_Soil analysis.tab; Design-Pishauli trial-GIZ.tab; Pishauli trial-GIZ.jpg.

carob_script <- function(path) {

"
Dataset for: Potato production under conservation agriculture: A trial in the locality of Pishauli, Chugay district-La Libertad, Peru

An experimental potato trial based on conservation agriculture was conducted December 1st, 2023 to May 8th, 2024 with a total area of 783 m2 with support of farmers of the locality of Pishauli , district of Chugay, province of Sánchez Carrión, La Libertad-Peru. For this trial, the improved potato variety INIA-325 Poderosa was used with a split-split plot design considering three factors: Types of tillage (minimum and zero tillage), residue management (incorporation and removal), and thickness of quinoa/barley of mulching (10, 20 and 30 cm). Combining these factors generated 12 treatments; and an additional control treatment under local conventional practical. Four replicates were established, with 52 plots of 3x3.6 m2 each with distance between plant and furrow of 0.3 m and 0.9 m respectively. The variables evaluated were: fresh tuber yield (t ha-1), benefit-cost ratio, carbon footprint, and soil analysis at different times during the vegetative period of the crop.
"

	uri <- "doi:10.21223/QFDHCA"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 0,
		carob_effort = 0.15
	)

	# No measurement table is available in the raw folder.
	return(FALSE)
}

