# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The published files are dictionaries only (bacterial wilt and potato leaf roll virus standard evaluation trials). There is no measurement table, so no data table was built.
# Files on disk: Standard Evaluation Trial for Bacterial Wilt Resistance Dictionary.xls; Standard Evaluation Trial for Potato Leaf Roll Virus Resistance Dictionary.xls.

carob_script <- function(path) {

"
Dataset for: Standard evaluation trial for potato leaf roll virus resistance documentation of CIP advanced breeding clones

Potato leaf roll virus (PLRV) is one of the most important virus diseases of potato, responsible for serious yield losses worldwide. PLRV is transmitted by several potato-colonizing aphid species, but Myzus persicae is regarded as the most efficient vector. Both larval and adult stages of winged and nonwinged forms transmit PLRVs. Symptoms of PLRV include blockage of the food movement channels of the plant. Upper leaves in primary infected plants become pale, upright and rolled, showing some reddening of the  tissue around the leaf edges. The lower leaves may or  may not have symptoms. Secondary  infected plants develop severely rolled lower leaves which feel like leather. The plant frequently has an overall stunted, upright, chlorotic appearance. The oldest leaves may show reddening on the margins or chlorosis The upper leaves may not have obvious symptoms. Standard evaluation trials measure PLRV resistance in advanced clones by subjecting plants to three consecutive intentional  field exposure trials, monitoring aphid population levels, and releasing aphids on infected plants of a susceptible variety planted as header plants on each plot. Individual plants of  each tested clone growing  in experimental units of ten hill plots replicated three times in a RCBD are tested by  serological ELISA test after the third exposure trial in order to determine the level of resistance to infection of the clone using a scale from highly resistance to highly susceptible based on the percentage number of infected plants.
"

	uri <- "doi:10.21223/P3/VRX2HW"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 0,
		carob_effort = 0.15
	)

	# No measurement table is available in the raw folder.
	return(FALSE)
}

