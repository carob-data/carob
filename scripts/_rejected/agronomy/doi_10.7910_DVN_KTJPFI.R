# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Harvest tables (02. Cowpeas harvest; 04. Maize harvest) are restricted on Harvard Dataverse.
# Only variable-description CSVs downloaded. Guestbook/access request needed (s.nyawira@cgiar.org).


carob_script <- function(path) {

"
Harvest data for 6 cowpeas and 6 maize farms in sandy soils in Makueni County, Kenya

The  data shows the harvested yield in 6 maize farms and 6 cowpeas farms in Mtito Andei- Makueni County. The data was collected in 3 plots in each farm to compare crop productivity in sandy soils plots installed with the sub-surface water retaining technology (SWRT) against plots without this technology. 2 of the plots are installed with SWRT (plot 1 and 2) while the third plot is treated as the control plot. All the plots received equal amounts of manure and fertilizer during planting. The maize plots were also top-dressed.
"

	uri <- "doi:10.7910/DVN/KTJPFI"
	group <- "rejected"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 0,
		carob_effort = 0.2
	)

	return(FALSE)
}
