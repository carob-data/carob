# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Seems like an incomplete ODK export: 3 observations across ~700 columns,
# only 366 of which have any non-NA value, and none of these is a yield
# measurement (harvest section entirely empty; harvest date shows the
# placeholder "1-Jan-99" for all 3 records). Crop type is also not clear from
# the data. Flagging for rejection.

carob_script <- function(path) {

"
N2Africa agronomy trials harvest - Uganda, 2016, I

N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"

	uri <- "doi:10.25502/2788-9j45"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = NA,
		treatment_vars = "",
		response_vars = "",
		notes = "",
		carob_contributor = "Stella Muthoni",
		carob_date = "2026-07-14",
		carob_completion = 100,	
		carob_effort = 0
	)

	f1 <- ff[basename(ff) == "data_table.csv"]
	f2 <- ff[basename(ff) == "variable_definitions.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	
	
# all scripts must end like this
	carobiner::write_files(path, meta, d)
}


