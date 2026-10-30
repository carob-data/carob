# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Measurement files are restricted on CIP Dataverse and were not downloaded. Only unrestricted documentation is in the raw folder, so no data table was built.
# Restricted files not on disk: 1_Emergence-SPAD.tab; 2_Canopy Cover.tab; 3_Harvest.tab; 4_Yield-BCR-CF-1.tab; 5_Soil analysis.tab; Design Schematic_OA.tab; Photo_La Soledad trial_1.JPG.

carob_script <- function(path) {

"
Dataset for: Organic agriculture trial in the locality of La Soledad, Chugay district, La Libertad - Peru (First season 2022-2023)

An experimental potato trial based on organic agriculture in the 2022-23 season was conducted in a total area of 987 m2 with the support of the 'Asociación  de Productores Agropecuarios y Semilleristas La Poderosa' of the locality of La Soledad, Chugay district, province of Sánchez Carrión, La Libertad. For this trial, the improved potato variety INIA-325 Poderosa was used with a split-plit plot design considering three factors: crop intercropping (intercropping with faba bean, variety Señorita and no-intercropping), use of a decision support system for fungicide application or 'Juego de Ruedas' (Disc and no-disc) and the application of organic amendments (1, 2 and 4 t ha-1 of poultry manure). The combination of these factors generated a total of 12 treatments; and an additional control treatment (conventional practices in the study zone). In adition, 4 replicates were established, with a total of 52 plots of 3x4 m2 each (distance between plant and furrow of 0.3 m and 1 m, respectively). The variables evaluated were: emergence (%), canopy cover (%), chlorophyll content (SPAD units), plant height (cm), fresh tuber yield (t ha-1), cost-benefit ratio and carbon footprint (kg CO2e Total) at different times during the vegetative period of the crop.
"

	uri <- "doi:10.21223/PP7DMK"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
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

