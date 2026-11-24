# R script for "carob"
# license: GPL (>=3)

## NOTES
# Alpha-lattice variety trial of the sorghum RIL population 296 B x PVK 801, postrainy 2012-13 at VNMKV, Parbhani. Moved from agronomy to varieties_sorghum because the treatment is genotype.
# The description says 3 replications and 36 blocks per replication. In this file "Block" is 1-3 and "Replication number" is 1-36, so those two columns were swapped when mapped to rep and block_id.
# Plot is 2 rows x 2 m x 0.75 m (3 m2), rows 75 cm apart, plants 15 cm apart.
# Grain yield "weight/m2" was read as kg/m2 and multiplied by 10000. Hundred-grain weight (g) times 10 is the 1000-grain weight.
# Fe and Zn are mg/kg and were left in mg/kg.
# Coordinates 18.45 N, 76.13 E and elevation 357 m are from the dataset description. geo_uncertainty 600 m reflects 0.01 degree precision.

## ISSUES
# Confirm the replication/block swap and the kg/m2 yield unit against the field book and Phuke et al. 2017 (doi:10.3389/fpls.2017.00712).
# grain_Zn is out of the terminag range because the values are mg/kg.
# Parents are not flagged. Fertilizer and irrigation are not reported. Rows missing yield or Fe/Zn were kept.
# write_files: missing carob_contributor; missing irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date; out of bounds grain_Zn (17.3-58.7); NA detected in yield.

## NOT PROCESSED
# Genotype: numeric entry code, stored as variety_code.

## NEW VARIABLES
# None.

carob_script <- function(path) {

"
Phenotypic and micronutrients data of Sorghum RIL populations trial during 2012-13 postrainy season at VNMKV, Parbhani

The low grain iron and zinc densities are well documented problems in food crops, affecting crop nutritional quality especially in cereals. Sorghum is a major source of energy and micronutrients for majority of population in Africa and central India. The database includes the investigation was carried out principally to study the variation for grain iron (Fe) and zinc (Zn) concentration, G × E interaction and association between these two traits and agronomic traits in RIL (Recombinant Inbred Line) population consisting of 309 individuals of generation developed using two contrasting parents 296 B and PVK 801 along with parents (296 B and PVK 801). The experiment was conducted during postrainy 2012-2013 season in Vasantrao Naik Marathwada Krishi Vidhyapeeth (VNMKV)-Parbhani located at an altitude of 357 m above mean sea level with latitude of 18.45◦N and longitude of 76.13◦E to obtain best quality grain for assessing grain Fe and Zn concentration. The trial was conducted using Alpha Lattice design with three replications. Seeds of each entry were distributed equally to seed packets, representing number of rows of each plot size; and then randomized plot numbers were assigned to each plot seed packets and arranged according to planned ﬁeld layout. Each entry was planted in two rows of 2 m length with a spacing of 75 cm between rows and 15 cm between plants in a row. The data on parameters like days to 50% flowering, plant height (cm), 100 seed weight (g), Fe (mg kg-1), Zn (mg kg-1), grain weight per meter square were recorded. The RIL population study showed good variability and high heritabilities for Fe and Zn and other traits studied indicating its suitability to map QTL for iron and zinc.        Experiment location on Google Map
"

	uri <- "doi:10.21421/D2/EXXKOX"
	group <- "varieties_sorghum"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
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

	f <- ff[basename(ff) == "Sorghum RIL populations  2012-13 VNMKV.xlsx"]
	r <- carobiner::read.excel(f, na="NA")

	variety <- gsub("^ICSL\\s*", "ICSL ", trimws(r$`ICSL No`))
	d <- data.frame(
		variety = variety,
		variety_code = as.character(r$Genotype),
		variety_pedigree = "296 B x PVK 801",
		variety_type = "RIL",
		treatment = variety,
		plot_id = as.character(r$`Plot No`),
		# File labels are reversed: Block is 1-3 and Replication number is 1-36
		rep = as.integer(r$Block),
		block_id = as.character(r$`Replication number`),
		flowering_days = as.numeric(r$`Days to flower`),
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
		adm1 = "Maharashtra",
		adm2 = "Parbhani",
		location = "Parbhani",
		season = "rabi",
		planting_date = "2012",
		trial_id = "Parbhani_2012",
		plot_area = 3,
		plot_length = 2,
		row_spacing = 75,
		plant_spacing = 15,
		longitude = 76.13,
		latitude = 18.45,
		elevation = 357,
		geo_uncertainty = 600,
		geo_from_source = TRUE,
		on_farm = FALSE,
		is_survey = FALSE
	)

	carobiner::write_files(path, meta, d)
}
