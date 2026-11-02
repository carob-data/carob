# R script for "carob"
# license: GPL (>=3)

## NOTES
# Alpha-lattice variety trial of the sorghum RIL population 296 B x PVK 801 at ICRISAT, Patancheru. The file and title are postrainy 2013-14 (360 entries, 36 blocks). Moved from agronomy to varieties_sorghum because the treatment is genotype.
# The description paragraph still says 2012-2013; planting_date is 2013 from the title and file name. Coordinates in that paragraph (17.53 N, 78.27 E, 545 m) are the Patancheru site.
# Plot is 2 rows x 2 m x 0.75 m (3 m2), rows 75 cm, plants 15 cm.
# Grain yield "weight/m2" was read as kg/m2 and multiplied by 10000. Hundred-grain weight (g) times 10 is the 1000-grain weight.
# Fe and Zn are mg/kg and were left in mg/kg. geo_uncertainty 600 m reflects 0.01 degree precision.

## ISSUES
# The description year (2012-13) disagrees with the title and file (2013-14). 2013 was used as the planting year.
# Confirm kg/m2 against Phuke et al. 2017 (doi:10.3389/fpls.2017.00712). grain_Zn is out of range because the unit is mg/kg.
# Parents are not flagged. Fertilizer and irrigation are not reported.
# write_files: missing carob_contributor; missing irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date; out of bounds grain_Zn (10.1-32.9); NA detected in yield.

## NOT PROCESSED
# Genotype: numeric entry code, stored as variety_code.

## NEW VARIABLES
# None.

carob_script <- function(path) {

"
Phenotypic and micronutrients data of Sorghum RIL populations trial during 2013-14 postrainy season at ICRISAT, Patancheru

The low grain iron and zinc densities are well documented problems in food crops, affecting crop nutritional quality especially in cereals. Sorghum is a major source of energy and micronutrients for majority of population in Africa and central India. The database includes the investigation was carried out principally to study the variation for grain iron (Fe) and zinc (Zn) concentration, G × E interaction and association between these two traits and agronomic traits in RIL (Recombinant Inbred Line) population consisting of 334 individuals of generation developed using two contrasting parents 296 B and PVK 801 along with parents (296 B and PVK 801). The experiment was conducted during postrainy 2012-2013 season in International Crops Research Institute for the Semi-Arid Tropics (ICRISAT) located at an altitude of 545 m above mean sea level with latitude of 17.53◦N and longitude of 78.27◦E to obtain best quality grain for assessing grain Fe and Zn concentration. The trial was conducted using Alpha Lattice design with three replications. Seeds of each entry were distributed equally to seed packets, representing number of rows of each plot size; and then randomized plot numbers were assigned to each plot seed packets and arranged according to planned ﬁeld layout. Each entry was planted in two rows of 2 m length with a spacing of 75 cm between rows and 15 cm between plants in a row. The data on parameters like days to 50% flowering, plant height (cm), 100 seed weight (g), Fe (mg kg-1), Zn (mg kg-1), grain weight per meter square were recorded. The RIL population study showed good variability and high heritabilities for Fe and Zn and other traits studied indicating its suitability to map QTL for iron and zinc.        Experiment location on Google Map
"

	uri <- "doi:10.21421/D2/DVPP9L"
	group <- "varieties_sorghum"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "ICRISAT",
		publication = "doi:10.3389/fpls.2017.00712",
		project = NA,
		design = "alpha-lattice",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;grain_Fe;grain_Zn;plant_height;flowering_days;seed_weight",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 80,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "Sorghum RIL population trial 2013-14 ICRISAT.xlsx"]
	r <- carobiner::read.excel(f, na="NA")

	variety <- gsub("^ICSL\\s*", "ICSL ", trimws(r$`ICSL No`))
	d <- data.frame(
		variety = variety,
		variety_code = as.character(r$Genotype),
		variety_pedigree = "296 B x PVK 801",
		variety_type = "RIL",
		treatment = variety,
		plot_id = as.character(r$`Plot No`),
		rep = as.integer(r$`Replication number`),
		block_id = as.character(r$Block),
		flowering_days = as.numeric(r$`Days to flowering`),
		plant_height = as.numeric(r$`Plant height`),
		seed_weight = as.numeric(r$`Hundred grain weight`) * 10,
		yield = as.numeric(r$`Grain yield`) * 10000,
		yield_moisture = NA_real_,
		yield_isfresh = NA,
		grain_Fe = as.numeric(r$Iron),
		grain_Zn = as.numeric(r$Zinc),
		crop = "sorghum",
		yield_part = "grain",
		country = "India",
		adm1 = "Telangana",
		adm2 = "Sangareddy",
		location = "Patancheru",
		season = "rabi",
		planting_date = "2013",
		trial_id = "Patancheru_2013",
		plot_area = 3,
		plot_length = 2,
		row_spacing = 75,
		plant_spacing = 15,
		longitude = 78.27,
		latitude = 17.53,
		elevation = 545,
		geo_uncertainty = 600,
		geo_from_source = TRUE,
		on_farm = FALSE,
		is_survey = FALSE
	)

	carobiner::write_files(path, meta, d)
}
