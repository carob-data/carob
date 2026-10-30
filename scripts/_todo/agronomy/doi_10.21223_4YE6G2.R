# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Measurement files are restricted on CIP Dataverse and were not downloaded. Only unrestricted documentation is in the raw folder, so no data table was built.
# Restricted files not on disk: 1_Gravimetric soil moisture LTT.tab; 2_Soil temperature LTT.tab; 3.1_Remote sensing_Reference tray.tab; 3_Remote sensing LTT.tab; 4_Harvest LTT.tab; 5_Soil analysis LTT.tab; Design Schematic LTT.tab; Image LTT S1.png; LTT Potato S1.jpeg.

carob_script <- function(path) {

"
Dataset for: Conservation Agriculture trial at CIP-La Molina experimental station (Season 2023-2024)

A potato trial under Conservation Agriculture with drip irrigation was carried out at the International Potato Center (CIP) La Molina experimental station, from October 4th, 2023 to February 7th, 2024. This trial, a split-plit plot design was used to test three factors: Tillage technique (minimum and conventional tillage), residue management (incorporation and removal) and the application of organic amendments (1, 5 and 10 t ha-1 of sheep manure). The combination of these factors generated a total of 12 treatments; and an additional control treatment under conventional management. Five replicates were established, with a total of 65 plots of 4x6 m2 each (distance between plant and furrow of 0.3 m and 1 m, respectively). The variables evaluated were: Total foliage weight, total tuber weight per plant, gravimetric soil moisture, canopy cover, multiespectral band, canopy temperature and soil analysis.
"

	uri <- "doi:10.21223/4YE6G2"
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

