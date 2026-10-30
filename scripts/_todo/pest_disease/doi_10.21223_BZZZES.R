# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Measurement files are restricted on CIP Dataverse and were not downloaded. Only unrestricted documentation is in the raw folder, so no data table was built.
# Restricted files not on disk: 01_evaluation.tab.

carob_script <- function(path) {

"
Dataset for: Optimal pesticide regimes determined for managing B. cockerelli in Northern Peru

The potato psyllid (Bactericera cockerelli) is causing severe losses in potato production, ranging from 60% to 100% when combined with phytoplasmas and bacteria in the zebra chip complex. This leads to low-quality, deformed tubers and economic losses. Farmers often misuse insecticides, resulting in pest resistance, crop abandonment, or fallow fields. The pest, which primarily affects potatoes and other solanaceous crops like tomatoes and peppers, is spreading rapidly across regions of Peru, including Piura, Cajamarca, La Libertad, and Áncash, triggering a phytosanitary alert.

B. cockerelli causes direct damage by feeding on sap and secreting toxins, and indirect damage as a vector for pathogens like phytoplasmas and Candidatus Liberibacter solanacearum (CLso), the cause of zebra chip. Strengthening farmers' knowledge and pest management practices is critical. Initial control strategies recommended by Ecuador's INIAP were unsuccessful, but recent trials with stage-specific insecticides of B. cockerelli show promise for developing an effective Integrated Pest Management (IPM) strategy to support small-scale farmers.
"

	uri <- "doi:10.21223/BZZZES"
	group <- "pest_disease"
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

