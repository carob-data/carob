# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Measurement files are restricted on CIP Dataverse and were not downloaded. Only unrestricted documentation is in the raw folder, so no data table was built.
# Restricted files not on disk: 1_Emergency-CC-Hg.tab; 2_Harvest.tab; 3_Yield-BCR-CF.tab; 4_Descomposition rate.tab; 5_Soil temperature.xlsx; 6_Soil analysis.tab; Design Schematic.tab; Licame S1.JPG.

carob_script <- function(path) {

"
Dataset for: Conservation agriculture trial in the locality of Licame, Chugay district, La Libertad - Peru (First season 2022-2023)

An experimental potato trial based on conservation agriculture in the 2022-23 season was conducted in a total area of 987 m2 with the support of the 'Asociación de Productores Agropecuarios Mi Licame Unido' of the locality of Licame, district of Chugay, province of Sánchez Carrión, La Libertad. For this trial, the improved potato variety INIA-325 Poderosa was used with a split-plit plot design considering three factors: tillage (conventional and zero tillage), plastic barriers (with and without barrier) and amount of oat/barley mulching (10, 20 and 30 cm thick). The combination of these factors generated a total of 12 treatments; and an additional control treatment (conventional practices in the area). In adition, 4 replicates were established, with a total of 52 plots of 3x4 m2 each (distance between plant and furrow of 0.3 m and 1 m, respectively). The variables evaluated were: emergence (%), coverage (%), soil moisture (%), chlorophyll content (SPAD units), plant height (cm), soil temperature (°C), descompositio rate, fresh tuber yield (t ha-1), cost-benefit ratio and carbon footprint (kg CO2e Total) at different times during the vegetative period of the crop.
"

	uri <- "doi:10.21223/XNXOG3"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=0,
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

