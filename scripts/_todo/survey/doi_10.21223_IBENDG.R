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
Dataset for: Baseline Survey of the Project 'Group-Based ICT Extension Systems: Evidence from Rural Areas of Peru' in the Provinces of Sanchez Carrion and Pataz, La Libertad Region 2024

We will investigate how to make group-based ICT extension work better among potato farmers in Peru. Farmers will be organized in groups and collectively select focal points, who act as liaisons between group members and a team of agricultural experts. In this demand-driven support system, focal points submit queries (including pictures, text, and voice messages) on behalf of group members to experts through WhatsApp and relay back advice to farmers. A common feature in the literature and in previous pilot studies has been the low level of commitment of focal points. Based on a cluster randomized controlled trial, we will assess the effectiveness of private (with individual monetary awards for focal points) and social (that garners them recognition in their communities) incentives to foster more active participation of focal points and group members in the digital support system, taking into consideration pre-existing differences in agricultural knowledge between communities.

Baseline Survey: In our baseline data, we will collect information about household's socio-economic characteristics, plots, past production, participation in previous extension projects, etc. This will provide us with information to further analyze heterogeneous effects of the treatment amongst specific subgroups (i.e., by age, gender, etc.). Importantly, we will also administer questionnaires to gauge farmers' baseline agricultural knowledge and will use this data to group communities into different tournament brackets (see above). The baseline survey was conducted from October 14 to 25, 2024, covering the districts of Huamachuco, Curgos, Chugay, and Cochorco in the province of Sánchez Carrión, as well as the districts of Tayabamba, Pataz, and Pias in the province of Pataz. A total of 1,389 potato producers were surveyed.
"

	uri <- "doi:10.21223/IBENDG"
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

