# R script for "carob"
# license: GPL (>=3)

## NOTES
# Vitamin C and dry matter of promising clones at La Victoria, Huancayo. The tidy table is 01_Data_VitaminC_Huancayo_Set5_2007.xlsx.
# The fieldbook sheet "2008_Sets 5_Hyo" names the units: ascorbic acid mg/100 g dry weight, mg/100 g fresh weight, and percentage dry matter.
# Installation: planted 2007-12-12, harvested 2008-04-23, plot 2.7 m2, 0.30 m between plants, 0.90 m between rows. Spacing is stored in cm.
# Coordinates and elevation are on the Identification sheet.

## ISSUES
# The abstract says a completely randomized design with two replications. The Installation sheet says a randomized complete block design with three blocks. The data file has two replications. The design stored here is the Installation sheet.
# Crop management lists fertilizing, weed, pest, disease, and irrigation with no product and no rate.
# No tuber yield in the files.
# The laboratory year in the data file is 2008. The trial was planted in 2007.
# write_files: missing variables: carob_contributor, yield, yield_moisture, yield_part, irrigated, K_fertilizer, N_fertilizer, P_fertilizer. unknown variables: ascorbic_acid_mg_100g_dw_, ascorbic_acid_mg_100g_fw_, dry_matter_percent_.

## NOT PROCESSED
# 02_Material_list and the fieldbook Material List: identifier crosswalk. Variety is the accession name when it is filled, otherwise the institutional number.
# 03_Data_dictionary.xlsx and Lists, Notes, Summary by clone: definitions and summaries.
# VitaminC sheet: the same results in a layout that is not one row per plot. The tidy workbook is used.
# Breeder Code or Collecting Number, Code laboratory, Numeration.


carob_script <- function(path) {

"
Dataset for: Vitamin C Analysis in Promising Potato Clones from Junin, Peru (2007)

An analysis of vitamin C concentration was performed on 29 promising potato clones in the Victoria region of Junin in 2007. The study employed a completely randomized design (CRD) with two replications. These clones were part of Set 5, identified as promising clones for further agricultural development.
"

	uri <- "doi:10.21223/0XGN5Q"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "Randomized Complete Block Design (RCBD)",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "ascorbic_acid_mg_100g_dw_;ascorbic_acid_mg_100g_fw_;dry_matter_percent_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "01_Data_VitaminC_Huancayo_Set5_2007.xlsx"]
	r <- carobiner::read.excel(f, na=c("", "NA", "-"))
	names(r) <- trimws(names(r))
	acc <- trimws(as.character(r[["Accession name"]]))
	acc[!nzchar(acc)] <- NA_character_
	instn <- trimws(as.character(r$INSTN))
	variety <- ifelse(is.na(acc), instn, acc)

	d <- data.frame(
		trial_id = "LaVictoria_2007",
		crop = "potato",
		variety = variety,
		variety_code = instn,
		treatment = variety,
		rep = as.integer(r$REP),
		country = "Peru",
		adm1 = "Junin",
		adm2 = "Huancayo",
		adm3 = "San Agustin",
		location = "La Victoria",
		elevation = 3216,
		# Identification sheet of TVIT200712_LAVICT_Set_5.xls
		latitude = -12.0264,
		longitude = -75.2449,
		geo_from_source = TRUE,
		planting_date = "2007-12-12",
		harvest_date = "2008-04-23",
		plot_area = 2.7,
		plant_spacing = 0.30 * 100,
		row_spacing = 0.90 * 100,
		plant_density = 37037,
		# units from the fieldbook sheet 2008_Sets 5_Hyo
		ascorbic_acid_mg_100g_dw_ = as.numeric(r$ASC_DW),
		ascorbic_acid_mg_100g_fw_ = as.numeric(r$ASC_FW),
		dry_matter_percent_ = as.numeric(r$PDM),
		on_farm = FALSE,
		is_survey = FALSE
	)

	carobiner::write_files(path, meta, d)
}
