# R script for "carob"
# license: GPL (>=3)

## NOTES
# Mineral concentration of four bitter-potato varieties at Puno, measured fresh and as chuño.
# The dictionary gives dry-matter as a percentage and the minerals as mg/kg dry weight or mg/100 g fresh weight. yield_moisture is 100 minus the dry-matter percentage. K and P here are tuber contents, not fertilizer.
# Installation: two-way factorial, plot 3 m2, 0.30 m between plants, 1 m between rows. Spacing is stored in cm.
# Coordinates and elevation 3856 m are on the Identification sheet.
# Every row is recorded at Puno. Treatment labels that say Huancavelica name the chuño process, not a second trial site.

## ISSUES
# The abstract says a completely randomized design. The Installation sheet says a two-way factorial design. The design stored here is the Installation sheet.
# Installation leaves planting and harvest blank. The Minimal begin and end dates are not labeled as planting and harvest, so they are not used.
# No fertilizer rate, irrigation statement, or tuber yield.
# The material list has twelve accession names. The data file has four variety names in INSTN, so the material list was not joined.

## NEW VARIABLES
# processing_: fresh tubers vs chuño processing treatment.
# tuber_<element>_fw_: tuber mineral concentration (mg/100g fresh weight). (Dry-weight minerals are stored in standard tuber_<element> columns in mg/g DW.)

## NOT PROCESSED
# 02_Material_list and the fieldbook Material List: the names do not match the four varieties in the data file.
# 03_Data_dictionary.xlsx, Lists, Notes, Summary by clone, Minerals, data ok: definitions or a layout that is not one row per observation.
# Numeration and laboratory code.
# 02_Material_list_Puno_Fresh_Bitter_2007.xlsx: not read. sheet Sheet1: Numeration, Accession name. Observation records are missing from d until this file is standardized.


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
		response_vars = "yield_moisture;tuber_Fe;tuber_Zn",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 80,
		carob_effort = 0.5
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
		yield_moisture = 100 - as.numeric(r$PDM),
		# Dry-weight minerals converted from mg/kg to mg/g (terminag standard)
		tuber_Fe = as.numeric(r$FeDW) / 1000,
		tuber_Fe_fw_ = as.numeric(r$FeFW),
		tuber_Mn = as.numeric(r$MnDW) / 1000,
		tuber_Mn_fw_ = as.numeric(r$MnFW),
		tuber_B = as.numeric(r$BDW) / 1000,
		tuber_B_fw_ = as.numeric(r$BFW),
		tuber_Cu = as.numeric(r$CUDW) / 1000,
		tuber_Cu_fw_ = as.numeric(r$CUFW),
		tuber_Zn = as.numeric(r$ZNDW) / 1000,
		tuber_Zn_fw_ = as.numeric(r$ZnFW),
		tuber_Ca = as.numeric(r$CADW) / 1000,
		tuber_Ca_fw_ = as.numeric(r$CAFW),
		tuber_Mg = as.numeric(r$MgDW) / 1000,
		tuber_Mg_fw_ = as.numeric(r$MgFW),
		tuber_K = as.numeric(r$KDW) / 1000,
		tuber_K_fw_ = as.numeric(r$KFW),
		tuber_P = as.numeric(r$PDW) / 1000,
		tuber_P_fw_ = as.numeric(r$PFW),
		tuber_S = as.numeric(r$SDW) / 1000,
		tuber_S_fw_ = as.numeric(r$SFW),
		tuber_Na = as.numeric(r$NADW) / 1000,
		tuber_Na_fw_ = as.numeric(r$NAFW),
		tuber_Al = as.numeric(r$AlDW) / 1000,
		tuber_Al_fw_ = as.numeric(r$AlFW),
		tuber_Ti = as.numeric(r$TIDW) / 1000,
		tuber_Cr = as.numeric(r$CRDW) / 1000,
		tuber_Se = as.numeric(r$SEDW) / 1000,
		tuber_Pb = as.numeric(r$PBDW) / 1000,
		tuber_Cd = as.numeric(r$CDDW) / 1000,
		tuber_Cd_fw_ = as.numeric(r$CDFW),
		tuber_Mo = as.numeric(r$MODW) / 1000,
		tuber_Mo_fw_ = as.numeric(r$MOFW),
		tuber_Co = as.numeric(r$CODW) / 1000,
		tuber_Ni = as.numeric(r$NIDW) / 1000,
		on_farm = FALSE,
		is_survey = FALSE
	)

	carobiner::write_files(path, meta, d)
}
