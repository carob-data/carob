# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Measurement files are restricted on CIP Dataverse and were not downloaded. Only unrestricted documentation is in the raw folder, so no data table was built.
# Restricted files not on disk: 01_Data_Yield_CHONT_Quitaco_FC_Aya_2006.tab; 02_Data_minerals_CHONT_Quitaco_FC_Aya_2006.tab; 03_Data_VitaminC_CHONT_Quitaco_FC_Aya_2006.tab; 04_Material_list_CHONT_Quitaco_FC_Aya_2006.tab; PTYL200612_CHONT_Quitaco_FC_Aya_Field.xls; TMIN200611_CHONT_Quitaco_FC_Aya_Field.xls; TVIT200611_CHONT_Quitaco_FC_Aya_Field.xls.

carob_script <- function(path) {

"
Dataset for: Evaluation of Micronutrient Concentrations in Advanced and Promising Potato Clones in Ayacucho - 2006

In 2006, 82 advanced and promising potato clones, characterized by their colored flesh, were evaluated in the locality of San Pedro de Quicato, in the community of Chontaca, Huamanga province, Ayacucho. The study focused on assessing micronutrient concentrations. A completely randomized design with two replications was employed to ensure the robustness of the results. Micronutrient concentrations were measured using spectrophotometry, providing detailed insights into the nutritional properties of these promising clones.
"

	uri <- "doi:10.21223/ITV1AA"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
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

