
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Automatically rejected by carobiner::draft
# This file is a bean breeding-line pedigree registry (linea, cruza,
# Pedigri, extended genealogy notation, registration year), not trial/
# evaluation data. The dataset's own description states yield (kg/ha) and
# grain Fe/Zn content (mg/kg) were evaluated, but no such columns - or any
# location, treatment, or trial structure - exist anywhere in this file.
# This appears to be a line-naming/ancestry catalog produced in preparation
# for the trials the description refers to, not the trial results themselves.


carob_script <- function(path) {

"
Replication Data for: Bean experimental lines selected for tolerance to Drought, high temperatures, low P in the soil, high aluminum in the soil and high content of Fe / Zn in grain.

Bean experimental lines selected for tolerance to Drought, high temperatures, low P in the soil, high aluminum in the soil and high content of Fe / Zn in grain. These lines will be sent to collaborators in Central America and Africa to be evaluated under their local conditions. Yield in Kg / Ha and Fe / Zn content in mg / Kg were evaluated.
"

	uri <- "doi:10.7910/DVN/CDBHQW"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIAT",
		publication = "",
		project = NA,
		carob_date = "2026-07-14",
		design = NA,
		data_type = NA,
		treatment_vars = "",
		response_vars = "", 
		carob_contributor = "Stella Muthoni",
		completion = 0,	
		notes = ""
	)
	
	f1 <- ff[basename(ff) == "Code Lines_2019..xlsx"]

	r1 <- carobiner::read.excel(f1)
	## r1: "linea" (line code), "cruza" (cross code), "Pedigri" (short pedigree),
	## "Pedigri + genealogia - opcion1" (full genealogy notation), "Ano Linea"
	## (registration year, 2019 throughout), "comentario" (free text, e.g. bean
	## market class - "linea mesos" for all rows seen). No yield, Fe/Zn, location,
	## or treatment data anywhere in the file - see ISSUES.
	
	# all scripts must end like this
	carobiner::write_files(path, meta, d)
}
