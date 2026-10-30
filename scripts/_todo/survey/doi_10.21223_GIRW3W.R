# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Measurement files are restricted on CIP Dataverse and were not downloaded. Only unrestricted documentation is in the raw folder, so no data table was built.
# Restricted files not on disk: Modulo A_informacion_preliminar_y_hogar.tab; Modulo B_Cultivos_de_shagra.xlsx; Modulo C_ Cultivos_anuales.xlsx; Modulo D_Cultivos_perennes.xlsx; Modulo E1_Agrobiodiversidad y redes de intercambio.xlsx; Modulo E2_Conservacion_y_tendencias_de_agrobiodiversidad.xlsx; Modulo F_Ganaderia.xlsx; Modulo G_Colecta y Caza.xlsx; Modulo H_Soluciones_basadas_en_la_naturaleza.xlsx; Modulo I_Crisis y Cambio climatico.xlsx; Modulo J_Manejo del suelo.xlsx; Modulo K_Manejo y uso agua.xlsx; Modulo L_Calidad de aire y ruido ambiental.xlsx; Modulo M_Valoracion_paisaje.xlsx; Modulo N_Residuos agricolas.tab.

carob_script <- function(path) {

"
Dataset for: Household and agrobiodiversity characterization in Cumbal Colombia

Agrobiodiversity is currently on the decline, attributed to various factors such as climate change and the global market. Recognizing the critical need to monitor agrobiodiversity status for effective conservation strategies, this study aimed to characterize households and agrobiodiversity within farmers' fields. A total of 206 farmers in Cumbal, Colombia, were surveyed using ARCGIS Survey 123. The surveyed farmers were specifically those with shagras (homegardens), as they were anticipated to exhibit the highest agrobiodiversity. Additionally, seed custodians, individuals acknowledged by communities for possessing extensive diversity and providing seeds for variety recovery, were included in the survey. As a result, we obtained a comprehensive database covering the following modules: household characteristics (Module A), shagras (Module B), annual (Module C) and perennial crops (Module D), agrobiodiversity and seed exchange (Module E1), conservation trends (Module E2), livestock (Module F), food collection and hunting (Module G), local nature-based solutions (Module H), crises and climate change (Module I), soil (Module J) and water management (Module K), air quality and noise contamination (Module L), landscape value (Module M), and agricultural residues (Module N). These modules have been individualized, anonymized, and standardized using the same 'Global ID' to facilitate the aggregation of the different modules at a household level. We also include here two exploratory databases about the management of the shagras and perennial crops that are called 'Datos_exploratorios_shagras' and 'Datos_exploratorios_diversidad_pastos_cultivados' respectively. These could be integrated with the modules using 'Global ID'. The dataset resulting from this survey is expected to be instrumental in designing more effective conservation initiatives in Cumbal, Colombia.
"

	uri <- "doi:10.21223/GIRW3W"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 0,
		carob_effort = 0.15
	)

	# No measurement table is available in the raw folder.
	return(FALSE)
}

