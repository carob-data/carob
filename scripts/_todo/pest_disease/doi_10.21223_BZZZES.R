# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Optimal pesticide regimes determined for managing B. cockerelli in Northern Peru

The potato psyllid (Bactericera cockerelli) is causing severe losses in potato production, ranging from 60% to 100% when combined with phytoplasmas and bacteria in the zebra chip complex. This leads to low-quality, deformed tubers and economic losses. Farmers often misuse insecticides, resulting in pest resistance, crop abandonment, or fallow fields. The pest, which primarily affects potatoes and other solanaceous crops like tomatoes and peppers, is spreading rapidly across regions of Peru, including Piura, Cajamarca, La Libertad, and Áncash, triggering a phytosanitary alert.

B. cockerelli causes direct damage by feeding on sap and secreting toxins, and indirect damage as a vector for pathogens like phytoplasmas and Candidatus Liberibacter solanacearum (CLso), the cause of zebra chip. Strengthening farmers' knowledge and pest management practices is critical. Initial control strategies recommended by Ecuador's INIAP were unsuccessful, but recent trials with stage-specific insecticides of B. cockerelli show promise for developing an effective Integrated Pest Management (IPM) strategy to support small-scale farmers.
"

	uri <- "doi:10.21223/BZZZES"
	group <- "pest_disease"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = "",
		project = NA,
		carob_date = "2026-10-30",
		design = NA,
		data_type = NA,
		treatment_vars = "",
		response_vars = "", 
		carob_contributor = "Your Name",
		completion = 0,	
		notes = "",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "02_Data_dictionary.xlsx"]

	r1 <- carobiner::read.excel(f1)
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
