# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Measurement files are restricted on CIP Dataverse and were not downloaded. Only unrestricted documentation is in the raw folder, so no data table was built.
# Restricted files not on disk: 01_Biomass_data.tab; 02_Canopy_cover_data.tab; 03_Weather_data.tab; El Progreso YG.png.

carob_script <- function(path) {

"
Dataset for: Temporal data of biomass and canopy cover for five potato varieties in El Progreso-Chugay, Peru (2023-2024): Calibration dataset for the SOLANUM Model

This dataset includes temporal data of biomass (fresh and dried) disaggregated into each organ (leaf, stem, root, and tubers) and canopy cover for 5 potato varieties: Yungay, Huevo de Indio, Bretaña, Poderosa, and Amarillis. This data was collected from a field trial conducted in El-Progreso locality with the support of the 'Asociación de Productores Agropecuarios Mi Licame Unido' of the locality of Licame, district of Chugay, province of Sánchez Carrión, La Libertad., between October, 24th 2023 and April, 18th 2024. Potatoes were managed under optimum growing conditions (no biotic or abiotic stress) to test their potential yield. This dataset also includes weather data (minimum, mean, and maximum temperatures, relative humidity, solar radiation, precipitation, and wind velocity) recorded during the growing season. This dataset aims to calibrate the SOLANUM model for those 5 potato varieties in Chugay.
"

	uri <- "doi:10.21223/JPA3NZ"
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

