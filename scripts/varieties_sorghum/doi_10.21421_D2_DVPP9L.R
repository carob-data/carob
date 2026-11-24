# R script for "carob"
# license: GPL (>=3)

## NOTES
# Alpha-lattice variety trial of the sorghum RIL population 296 B x PVK 801 at ICRISAT, Patancheru. The file and title are postrainy 2013-14 (360 entries, 36 blocks). Moved from agronomy to varieties_sorghum because the treatment is genotype.
# The description paragraph still says 2012-2013; planting_date is 2013 from the title and file name. Coordinates in that paragraph (17.53 N, 78.27 E, 545 m) are the Patancheru site.
# Plot is 2 rows x 2 m x 0.75 m (3 m2), rows 75 cm, plants 15 cm.
# Grain yield is kg/m2. Phuke et al. 2017 Table 3 (ICRISAT 2013-14) is in t/ha: raw x 10 matches 296 B (2.80 t/ha, 1.4-3.8), PVK 801 (4.16, 2.2-6.2) and the RIL mean (3.10). The file maximum is 5.9 t/ha against the published 5.4. Multiplied by 10000 to kg/ha. 
#  geo_uncertainty 600 m reflects 0.01 degree precision.

## ISSUES
# The description year (2012-13) disagrees with the title and file (2013-14). 2013 was used as the planting year.
# Parents are not flagged. Fertilizer and irrigation are not reported.
# write_files: missing irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date; NA detected in yield.

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
		carob_contributor = "Robert Hijmans",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 80,
		carob_effort = 1/12
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
		# mg/kg to mg/g
		grain_Fe = as.numeric(r$Iron) / 1000,
		grain_Zn = as.numeric(r$Zinc) / 1000,
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
