# R script for "carob"
# license: GPL (>=3)

## NOTES
# The measurement files for this record are restricted on Dataverse. The raw folder only has the unrestricted documentation (dictionary and, in some cases, a questionnaire). There are no observation rows to standardize.

## ISSUES
# No data table. The restricted measurement files were not downloaded.
# The script stops before write_files because there is no table.

## NOT PROCESSED
# The dictionary or questionnaire that is on disk describes columns that are not in the downloaded files.


carob_script <- function(path) {

"
Replication Data for: Baseline survey report of orange-fleshed sweet potato knowledge, farming and consumption, and dietary practices among households with children aged 6-59 months in selected districts of Morogoro, Iringa and Mbeya regions of Tanzania

Orange-fleshed sweetpotato (OFSP) as a staple food besides providing necessary body energy in human diet provides significant amounts of Vitamin A addressing both Vitamin A deficiency (VAD) and body energy needs. The Viable Sweetpotato Technologies in Africa (VISTA) – Tanzania project was designed to expand the production and utilization of nutritious OFSP into seven districts in Mbeya, Iringa and Morogoro regions which were part of USAID's Feed the Future (FTF) zones of influence (ZOI). As part of initial project activities, a baseline survey was implemented to provide a better understanding of the prevailing OFSP knowledge, farming and consumption and dietary practices of caregivers of households with children aged between 6 and 59 months in the project intervention districts. This provided up-to-date data on key indicators to better understand the context and to be able to use these as a basis for progress monitoring over the three year project period.
"

	uri <- "doi:10.21223/P3/WWESEI"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 0,
		carob_effort = 0.15
	)

	stop("Measurement files are restricted and are not in the raw folder. Only documentation was available, so no data table was built.")
}

