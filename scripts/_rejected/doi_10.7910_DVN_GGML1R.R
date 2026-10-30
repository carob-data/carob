# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rejected. Geospatial table of drought in inland valleys (one row per valley code), not a farm, plot, or experiment table.

## ISSUES
# No farm, plot, or experimental crop measurements. dryoccu, dryoccfre, lossfre and redfre are drought and harvest-loss frequencies (ordinal codes), not a yield. area and avwidt describe the valley. var, fert and irrig are valley-level yes/no or category codes. Coordinates are present, but there is nothing to attach to a crop record.

## NOT PROCESSED
# A geospatial database of drought occurrence in inland valleys in Mali, Burkina Faso and Nigeria.csv (300 inland valleys).
# Dictionary.xlsx (variable definitions for that table).


carob_script <- function(path) {

"
Replication Data for: A geospatial database of drought occurrence in inland valleys in Mali, Burkina Faso and Nigeria

In this study, we assessed the predictors for occurrence of drought in inland valley rice-based production systems and enabling factors for small scale farmers to mitigate its effects in three West African countries located in the Sudan-Sahel zone viz. Nigeria, Burkina Faso and Mali. We used both biophysical and socio-economic data, farmers' community surveys and secondary data from remote sensing on soil characteristics and water demand. We then applied regression models to assess the predictors of drought and the factors affecting farmers' adaptation strategies.
"

	uri <- "doi:10.7910/DVN/GGML1R"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "AfricaRice",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 0,
		carob_effort = 0.2
	)

	NULL
}
