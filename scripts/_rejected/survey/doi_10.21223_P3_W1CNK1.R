# R script for "carob"
# license: GPL (>=3)

## REJECTED
# The deposit is a questionnaire PDF (11 Baseline Survey.pdf) and the zip. There is no data table.

carob_script <- function(path) {

"
Dataset for: Endline survey of the Project 'Group-Based ICT Extension Systems: Evidence from Rural Areas of Peru' in the Provinces of Sanchez Carrion and Pataz, La Libertad Region - 2025.

We will investigate how to make group-based ICT extension work better among potato farmers in Peru. Farmers will be organized in groups and collectively select focal points, who act as liaisons between group members and a team of agricultural experts. In this demand-driven support system, focal points submit queries (including pictures, text, and voice messages) on behalf of group members to experts through WhatsApp and relay back advice to farmers. A common feature in the literature and in previous pilot studies has been the low level of commitment of focal points. Based on a cluster randomized controlled trial, we will assess the effectiveness of private (with individual monetary awards for focal points) and social (that garners them recognition in their communities) incentives to foster more active participation of focal points and group members in the digital support system, taking into consideration pre-existing differences in agricultural knowledge between communities. Endline Survey: In our endline data, we will collect information about household's socio-economic characteristics, plots, past production, participation in previous extension projects, etc. This will provide us with information to further analyze heterogeneous effects of the treatment amongst specific subgroups (i.e., by age, gender, etc.). Importantly, we will also administer questionnaires to gauge farmers' endline agricultural knowledge and will use this data to group communities into different tournament brackets (see above). The endline survey was administered also by CEDEP between 20 and 31 of July, 2025 covering the districts of Huamachuco, Curgos, Chugay, and Cochorco in the province of Sánchez Carrión, as well as the districts of Tayabamba, Pataz, and Pias in the province of Pataz. The endline survey covered a total of 1,107 farmers, since some farmers had already abandoned the project or could not be found for the endline survey.
"

	uri <- "doi:10.21223/P3/W1CNK1"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = NA,
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 0,
		carob_effort = 0.1
	)

	message("No measurement table in the local raw files for ", uri)
	invisible(meta)
}
