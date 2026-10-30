# REJECTED 
# Reason: get_data() returned no files
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Endline survey of the Project 'Group-Based ICT Extension Systems: Evidence from Rural Areas of Peru' in the Provinces of Sanchez Carrion and Pataz, La Libertad Region - 2025.

We will investigate how to make group-based ICT extension work better among potato farmers in Peru. Farmers will be organized in groups and collectively select focal points, who act as liaisons between group members and a team of agricultural experts. In this demand-driven support system, focal points submit queries (including pictures, text, and voice messages) on behalf of group members to experts through WhatsApp and relay back advice to farmers. A common feature in the literature and in previous pilot studies has been the low level of commitment of focal points. Based on a cluster randomized controlled trial, we will assess the effectiveness of private (with individual monetary awards for focal points) and social (that garners them recognition in their communities) incentives to foster more active participation of focal points and group members in the digital support system, taking into consideration pre-existing differences in agricultural knowledge between communities. Endline Survey: In our endline data, we will collect information about household's socio-economic characteristics, plots, past production, participation in previous extension projects, etc. This will provide us with information to further analyze heterogeneous effects of the treatment amongst specific subgroups (i.e., by age, gender, etc.). Importantly, we will also administer questionnaires to gauge farmers' endline agricultural knowledge and will use this data to group communities into different tournament brackets (see above). The endline survey was administered also by CEDEP between 20 and 31 of July, 2025 covering the districts of Huamachuco, Curgos, Chugay, and Cochorco in the province of Sánchez Carrión, as well as the districts of Tayabamba, Pataz, and Pias in the province of Pataz. The endline survey covered a total of 1,107 farmers, since some farmers had already abandoned the project or could not be found for the endline survey.
"

	uri <- "doi:10.21223/P3/W1CNK1"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP; Pataz Assosiation",
		publication = "",
		project = NA,
		carob_date = "2026-10-30",
		design = "unitOfAnalysis: Social and Nutritional Sciences DivisionMetadata; universe: Total 102 communities in the highlands of the Provinces of Sanchez Carrión, and Pataz (La Libertad region, northern Peru); timeMethod: Between 20 and 31 of July, 2025; samplingProcedure: To test the incentives, between September 2024 and August 2025 we organized groups of 10-15 farmers in 102 communities in the highlands of the Provinces of Sanchez Carrión and Pataz (Department of La Libertad, northern Peru), involving a total of 1,242 potato growing households. We facilitated discussions about whom to designate as their focal point, but ultimately this was a participatory process. Then communities were randomly assigned to one of three treatment arms (40 communities each of the incentives treatments and 21 for the control treatment arm) with: (a) individual incentives for focal points (40 groups), (b) social incentives for the community (41 groups), and (c) no incentive (21 groups); collectionMode: Survey Endline.; researchInstrument: Endline Survey - The endline survey was administered also by CEDEP between 20 and 31 of July, 2025.",
		data_type = NA,
		treatment_vars = "",
		response_vars = "", 
		carob_contributor = "Your Name",
		completion = 0,	
		notes = "",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	




	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
