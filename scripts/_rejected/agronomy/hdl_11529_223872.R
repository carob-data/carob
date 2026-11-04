# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Data file NG_VT_NGS_2016 (1).xlsx is listed with filesize 0; download from data.cimmyt.org returns HTTP 500.
# Duplicate to-do.csv row titled "Long term trial on winter wheat / CIAT soils" uses this same URI; that title does not match this TAMASA Nigeria dataset.


carob_script <- function(path) {

"
TAMASA Nigeria. Variety validation trials dataset for Northern Guinea Savanna, Nigeria. 2016.

This dataset contains information from on-farm validation trials conducted across 30 farmer fields in Local government areas of the northern guinea savanna AEZ in Kano. In each farmer field 10 plots (30 meter square each) were planted with a different combination of early maturing maize (10 varieties) under 3 different sowing densities. Each plot is a different combination of variety and sowing data. Data was collected on tops weight at anthesis, cob yield and stover yield.
"

	uri <- "hdl:11529/223872"
	group <- "rejected"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		publication = NA,
		project = "TAMASA",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 0,
		carob_effort = 0.1
	)

	return(FALSE)
}
