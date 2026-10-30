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
Dataset for: Baseline Survey for Scaling Up Sweetpotato Through Agriculture and Nutrition  (SUSTAIN) Project, Kenya

Scaling Up Sweetpotato Through Agriculture and Nutrition (SUSTAIN) was a 5-year project of the International Potato Center (CIP). The project aimed at improving production and consumption of vitamin A-rich orange-fleshed sweetpotato (OFSP) by households with children under five years of age in Kenya. A study was conducted to provide baseline information about the status of the households in terms of sweetpotato production, consumption and sale of OFSP. It was also intended to obtain information on dietary diversity, feeding practices, household food security status, vitamin A and acceptance of OFSP among the households. Data was collected from 600 households randomly selected from five counties where SUSTAIN project was implemented, namely, Siaya, Kisumu, Homa bay, Nyamira and Migori. In each household, the head responded to the agriculture modules of the tool while the reference woman (defined as a caregiver of child less than 5 years of age or lactating/pregnant woman) responded to the questions on knowledge, household nutrition, food security and feeding practices. Household's diet quality was assessed using the standard child dietary diversity (CDDS) and minimum dietary diversity for women of reproductive age (MDD-W) as well as frequency of consumption of different foods based on 24-hour recall. The vitamin consumption frequency was assessed using scores based on the Helen Keller International (HKI) approach based on 7-day recall.
"

	uri <- "doi:10.21223/P3/TCBLPT"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
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

