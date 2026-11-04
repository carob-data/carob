# R script for "carob"
# license: GPL (>=3)

## ISSUES
# This is a SNP marker table for a spring-wheat crossing block (about 12 to 14 thousand
# SNPs per panel after the filters described in the dataset). It was not mapped to a
# plot or genotype table. The previous script stopped after get_metadata.


carob_script <- function(path) {

"
The historic crossing block (HCB) is a collection of the parents of the spring wheat pre-breeding crossing block, including landraces, synthetics, synthetic derivatives, elite lines, high biomass lines, lines with rust resistance, and some varieties. The HCB has three panels: yield potential (CB-YP), drought (CB-D), and heat stress (CB-H).
"

	uri <- "hdl:11529/10548509"
	group <- "genetics"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "",
		response_vars = "",
		carob_completion = 0,
		carob_effort = 0,
		carob_contributor = "Your Name",
		carob_date = "2026-10-30",
		notes = "",
		
	)

	## ff holds the SNP files. They still need a reader.
	ff
	meta
}
