# R script for "carob"
# license: GPL (>=3)

## NOTES
# Mineral concentration of four bitter-potato varieties at Puno, measured fresh and as chuño.
# The dictionary gives dry-matter as a percentage and the minerals as mg/kg dry weight or mg/100 g fresh weight. K and P here are tuber contents, not fertilizer.
# Installation: two-way factorial, plot 3 m2, 0.30 m between plants, 1 m between rows. Spacing is stored in cm.
# Coordinates and elevation 3856 m are on the Identification sheet.
# Every row is recorded at Puno. Treatment labels that say Huancavelica name the chuño process, not a second trial site.

## ISSUES
# The abstract says a completely randomized design. The Installation sheet says a two-way factorial design. The design stored here is the Installation sheet.
# Installation leaves planting and harvest blank. The Minimal begin and end dates are not labeled as planting and harvest, so they are not used.
# No fertilizer rate and no irrigation statement.
# No tuber yield. The measurements are laboratory mineral concentrations.
# The material list has twelve accession names. The data file has four variety names in INSTN, so the material list was not joined.
# write_files: missing variables: carob_contributor, yield, yield_moisture, yield_part, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: processing_ and the tuber mineral and dry-matter columns named with a trailing underscore.

## NOT PROCESSED
# 02_Material_list and the fieldbook Material List: the names do not match the four varieties in the data file.
# 03_Data_dictionary.xlsx, Lists, Notes, Summary by clone, Minerals, data ok: definitions or a layout that is not one row per observation.
# Numeration and laboratory code.


carob_script <- function(path) {

"
Dataset for: Micronutrient Concentration in Bitter Potatoes from Puno - 2007

Four native varieties of bitter potatoes were used to analyze mineral concentration in fresh and cooked tubers. Mineral concentrations in these tubers were determined using the spectrophotometry method. Treatments included white and black chuño from Huancavelica, as well as tubers from Puno. The experiment was conducted under a completely randomized design.
"

	uri <- "doi:10.21223/3UT8HI"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "Two-way Factorial Design",
		data_type = "experiment",
		treatment_vars = "variety;processing_",
		response_vars = "dry_matter_percent_;tuber_Fe_mg_kg_dw_;tuber_Zn_mg_kg_dw_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 65,
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "01_Data_minerals_Puno_Fresh_Bitter_2007.xlsx"]
	r <- carobiner::read.excel(f, na=c("", "NA", "-"))
	names(r) <- trimws(names(r))
	variety <- trimws(as.character(r$INSTN))
	processing <- trimws(as.character(r$Treatment))

	d <- data.frame(
		trial_id = "Puno_bitter_2007",
		crop = "potato",
		variety = variety,
		treatment = paste(variety, processing),
		processing_ = processing,
		rep = as.integer(r$REP),
		country = "Peru",
		adm1 = "Puno",
		adm2 = "Puno",
		adm3 = "Puno",
		location = "Puno",
		elevation = 3856,
		# Identification sheet of TMIN200712_PUNO_Fresh_Bitter_Potatoes.xls
		latitude = -15.8392,
		longitude = -70.0292,
		geo_from_source = TRUE,
		plot_area = 3,
		plant_spacing = 0.30 * 100,
		row_spacing = 1 * 100,
		plant_density = 33333,
		dry_matter_percent_ = as.numeric(r$PDM),
		tuber_Fe_mg_kg_dw_ = as.numeric(r$FeDW),
		tuber_Fe_mg_100g_fw_ = as.numeric(r$FeFW),
		tuber_Mn_mg_kg_dw_ = as.numeric(r$MnDW),
		tuber_Mn_mg_100g_fw_ = as.numeric(r$MnFW),
		tuber_B_mg_kg_dw_ = as.numeric(r$BDW),
		tuber_B_mg_100g_fw_ = as.numeric(r$BFW),
		tuber_Cu_mg_kg_dw_ = as.numeric(r$CUDW),
		tuber_Cu_mg_100g_fw_ = as.numeric(r$CUFW),
		tuber_Zn_mg_kg_dw_ = as.numeric(r$ZNDW),
		tuber_Zn_mg_100g_fw_ = as.numeric(r$ZnFW),
		tuber_Ca_mg_kg_dw_ = as.numeric(r$CADW),
		tuber_Ca_mg_100g_fw_ = as.numeric(r$CAFW),
		tuber_Mg_mg_kg_dw_ = as.numeric(r$MgDW),
		tuber_Mg_mg_100g_fw_ = as.numeric(r$MgFW),
		tuber_K_mg_kg_dw_ = as.numeric(r$KDW),
		tuber_K_mg_100g_fw_ = as.numeric(r$KFW),
		tuber_P_mg_kg_dw_ = as.numeric(r$PDW),
		tuber_P_mg_100g_fw_ = as.numeric(r$PFW),
		tuber_S_mg_kg_dw_ = as.numeric(r$SDW),
		tuber_S_mg_100g_fw_ = as.numeric(r$SFW),
		tuber_Na_mg_kg_dw_ = as.numeric(r$NADW),
		tuber_Na_mg_100g_fw_ = as.numeric(r$NAFW),
		tuber_Al_mg_kg_dw_ = as.numeric(r$AlDW),
		tuber_Al_mg_100g_fw_ = as.numeric(r$AlFW),
		tuber_Ti_mg_kg_dw_ = as.numeric(r$TIDW),
		tuber_Cr_mg_kg_dw_ = as.numeric(r$CRDW),
		tuber_Se_mg_kg_dw_ = as.numeric(r$SEDW),
		tuber_Pb_mg_kg_dw_ = as.numeric(r$PBDW),
		tuber_Cd_mg_kg_dw_ = as.numeric(r$CDDW),
		tuber_Cd_mg_100g_fw_ = as.numeric(r$CDFW),
		tuber_Mo_mg_kg_dw_ = as.numeric(r$MODW),
		tuber_Mo_mg_100g_fw_ = as.numeric(r$MOFW),
		tuber_Co_mg_kg_dw_ = as.numeric(r$CODW),
		tuber_Ni_mg_kg_dw_ = as.numeric(r$NIDW),
		on_farm = FALSE,
		is_survey = FALSE,
		stringsAsFactors = FALSE
	)

	carobiner::write_files(path, meta, d)
}
