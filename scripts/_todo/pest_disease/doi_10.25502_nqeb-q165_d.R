# R script for "carob"
# license: GPL (>=3)

## ISSUES
# - Coordinates (-6.52471, 35.78784) are a single country-level coverage point from dataset metadata, not farm GPS; no district/village names are provided.
# - Confirm whether this dataset corresponds to the on-farm component of Amour et al. (2025, Frontiers in Agronomy, doi:10.3389/fagro.2025.1623632) and verify MandiPlus active ingredients (thiamethoxam + mefenoxam + fludioxonil) and Lecatech active ingredient (Lecanicillium lecanii).
# - Root weight (Total_Yield) is fresh root weight in grams per plant (stored in root_weight_g_) and cannot be converted to kg/ha without plant spacing or harvested plot area.
# - Data-entry anomalies in yield-data.csv (kept as recorded): row 134 (Farmer BL-1, Plant 14, Imidacloprid + Lecatech) has Total_Infected_Roots = 180 despite Total_Root_Number = 4, Total_Healthy_Roots = 4, and CBSD_Incidence_infected = 0 (likely copied from Stem_Height = 180); row 276 (Farmer AL-2, Plant 6) has Stem_Height = 2 cm.
# - Disease and mite severity scales are not defined in the data dictionary (likely the standard IITA 1-5 cassava scale, where 1 = no symptoms; CMD severity is 1 on all 10,000 rows).
# - parasitism-data.csv identifies blocks 1-10 without farmer codes (BL-1 to MS-10) and is therefore kept as separate records rather than joined to the farmer plots.

## NEW
# plant_number_: sequential plant number sampled within each plot (1-50 in pest/yield data, 1-100 in parasitism data)
# months_after_planting_: months after planting at assessment (1, 3, 6, 10 from Period; 3 from parasitism Month)
# stage_: whitefly life stage ("adult" or "nymph")
# root_weight_g_: fresh root weight per plant (g)
# root_number_: total storage roots per plant (count)
# root_healthy_: healthy storage roots per plant (count)
# root_infected_: CBSD-infected storage roots per plant (count)
# stem_number_: stems per plant (count)
# nymphs_healthy_: unparasitized whitefly nymphs per plant (count)
# nymphs_parasitized_: parasitized whitefly nymphs per plant (count)
# pest_species: Mononychellus tanajoa
# insecticide_product: lecanicillium lecanii

## MISSING
# yield, yield_moisture, irrigated, N_fertilizer, P_fertilizer, K_fertilizer, planting_date, harvest_date

## NOTES
# Farmer participatory cassava whitefly (Bemisia tabaci) IPM trial in Tanzania across 10 farmers (BL-1 to MS-10) testing 5 cutting-dip / biopesticide treatments (Control, Mandiplus, Imidacloprid, Mandiplus + Lecatech, Imidacloprid + Lecatech).
# Wide table d preserves individual plant-level records without aggregation: 2,500 plants (10 farmers x 5 treatments x 50 plants, with yield/root/stem measurements merged for plants 1-30) plus 5,000 plants from parasitism-data.csv (10 blocks x 5 treatments x 100 plants; hhid set to "block_<n>").
# Long table dd holds all pest, disease, and parasitism observations linked by record_id: 1, 3, 6, and 10 MAP assessments for the 2,500 plants (whitefly adults/nymphs, cassava green mite Mononychellus tanajoa, CMD, CBSD) plus 3-month nymph parasitism counts for the 5,000 parasitism plants.
# Abbreviated treatment labels in parasitism-data.csv (MandiLeca, ImidaLeca) and spacing typo in yield-data.csv ("Imidacloprid+ Lecatech") are standardized to match pest-and-disease-data.csv.

## NOT PROCESSED
# pest-and-disease-data.csv, yield-data.csv, parasitism-data.csv: ID (row number).
# data_dictionary.csv: codebook only.


carob_script <- function(path) {

"
IPM of cassava whiteflies and whitefly-transmitted viruses through farmer participatory trials using insecticidal cutting

RTB crops are of strategic importance to the future food security of Africa under predicted climate change scenarios. Cassava is the most important RTB crop in Africa, and its production and consumption are projected to increase due to its adaptability to climate change. Currently cassava is threatened by two viral diseases, cassava mosaic (CMD) and cassava brown streak (CBSD) disease. To boost and sustain cassava production there is a need to develop and deploy strategies that will halt/slow the spread of pandemics of these viruses. Approaches to achieve this include participatory surveillance based on digital tools as well as the promotion of IPM solutions such as use of clean planting materials and control of virus vectors. This project will seek to scale out the use of digital tools for diagnosis of pests and diseases (PlantVillage NURU), access to modern agronomic practices (AKILIMO) and access to clean seed planting material (SeedTracker). This study will strengthen surveillance methods for monitoring virus disease spread and develop early warning strategies that will facilitate tracking of disease fronts. IPM packages for control of cassava whiteflies will be developed through the evaluation and validation of insecticidal cutting dip treatments and spraying with entomopathogens.
"

	uri <- "doi:10.25502/nqeb-q165/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = NA, minor = NA,
		data_organization = "IITA",
		publication = NA,
		project = "Mitigating the Impacts of Cassava Virus Epidemics and their Whitefly Vectors in East, Central and Southern Africa through Surveillance, Scaling of Digital Extension Tools, and IPM",
		design = "on-farm participatory trial; 10 farmers; 5 insecticidal cutting-dip and biopesticide treatments",
		data_type = "on-farm experiment",
		treatment_vars = "insecticide_used;insecticide_product;fungicide_used;fungicide_product",
		response_vars = "pest_incidence;pest_severity;disease_severity;root_infection;plant_height",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	f1 <- ff[basename(ff) == "pest-and-disease-data.csv"]
	f2 <- ff[basename(ff) == "yield-data.csv"]
	f3 <- ff[basename(ff) == "parasitism-data.csv"]
	r1 <- read.csv(f1, check.names = FALSE)
	r2 <- read.csv(f2, check.names = FALSE)
	r3 <- read.csv(f3, check.names = FALSE)

	fix_trt <- function(x) {
		x <- trimws(x)
		x[x == "Imidacloprid+ Lecatech"] <- "Imidacloprid + Lecatech"
		x[x == "MandiLeca"] <- "Mandiplus + Lecatech"
		x[x == "ImidaLeca"] <- "Imidacloprid + Lecatech"
		x
	}
	trt1 <- fix_trt(r1$Treatment)
	trt2 <- fix_trt(r2$Treatment)
	trt3 <- fix_trt(r3$Treatment)

	# Helper to map treatment names to explicit active-ingredient columns
	# MandiPlus: thiamethoxam + mefenoxam + fludioxonil; Lecatech: Lecanicillium lecanii (Amour et al. 2025)
	ins_prod <- function(trt) {
		ifelse(trt == "Control", "none",
		ifelse(trt == "Imidacloprid", "imidacloprid",
		ifelse(trt == "Mandiplus", "thiamethoxam",
		ifelse(trt == "Imidacloprid + Lecatech", "imidacloprid; lecanicillium lecanii",
		ifelse(trt == "Mandiplus + Lecatech", "thiamethoxam; lecanicillium lecanii", NA_character_)))))
	}
	seed_trt <- function(trt) {
		ifelse(trt == "Control", "none",
		ifelse(grepl("Mandiplus", trt), "Mandiplus", "imidacloprid"))
	}

	# Wide table part 1: 2,500 individual plants (10 farmers x 5 treatments x 50 plants)
	id1 <- unique(data.frame(
		hhid = trimws(r1$Farmer),
		treatment = trt1,
		plant_number_ = as.integer(r1$Plant)
	))
	y <- data.frame(
		hhid = trimws(r2$Farmer),
		treatment = trt2,
		plant_number_ = as.integer(r2$Plant),
		root_weight_g_ = as.numeric(r2$Total_Yield),
		root_number_ = as.integer(r2$Total_Root_Number),
		root_healthy_ = as.integer(r2$Total_Healthy_Roots),
		root_infected_ = as.integer(r2$Total_Infected_Roots),
		root_infection = as.numeric(r2$CBSD_Incidence_infected),
		plant_height = as.numeric(r2$Stem_Height),
		stem_number_ = as.integer(r2$Stem_Numbers)
	)
	d1 <- merge(id1, y, by = c("hhid", "treatment", "plant_number_"), all = TRUE)
	d1 <- d1[order(d1$hhid, d1$treatment, d1$plant_number_), ]
	rownames(d1) <- NULL
	d1$record_id <- seq_len(nrow(d1))
	d1$trial_id <- d1$hhid
	d1$plot_id <- paste(d1$hhid, d1$treatment, sep = "_")

	# Long table: repeated observations at 1, 3, 6, 10 MAP for the 2,500 plants in d1
	map_num <- as.integer(sub("MAP", "", trimws(r1$Period)))
	key1 <- paste(trimws(r1$Farmer), trt1, as.integer(r1$Plant), sep = "|")
	key_d1 <- paste(d1$hhid, d1$treatment, d1$plant_number_, sep = "|")
	rec_id <- d1$record_id[match(key1, key_d1)]
	stopifnot(!any(is.na(rec_id)))

	one <- function(species, stage, incidence, severity, disease, dsev) {
		data.frame(
			record_id = rec_id,
			months_after_planting_ = map_num,
			pest_species = species,
			stage_ = stage,
			pest_incidence = incidence,
			pest_severity = severity,
			disease = disease,
			disease_severity = dsev
		)
	}
	dd <- rbind(
		one("whitefly", "adult", as.integer(r1$Whitefly_adults), NA_character_, NA_character_, NA_character_),
		one("whitefly", "nymph", as.integer(r1$Whitefly_nymphs), NA_character_, NA_character_, NA_character_),
		one("Mononychellus tanajoa", NA_character_, NA_integer_, as.character(r1$Cassava_green_mite_severity), NA_character_, NA_character_),
		one(NA_character_, NA_character_, NA_integer_, NA_character_, "cassava mosaic", as.character(r1$Cassava_mosaic_disease_severity)),
		one(NA_character_, NA_character_, NA_integer_, NA_character_, "cassava brown streak", as.character(r1$Cassava_brown_streak_severity))
	)

	# Wide table part 2: 5,000 individual plants from parasitism-data.csv (10 blocks x 5 treatments x 100 plants)
	d3 <- data.frame(
		record_id = seq_len(nrow(r3)) + nrow(d1),
		hhid = paste0("block_", r3$Block),
		trial_id = paste0("parasitism_block_", r3$Block),
		block_id = as.character(r3$Block),
		plot_id = paste("parasitism_block", r3$Block, trt3, sep = "_"),
		treatment = trt3,
		plant_number_ = as.integer(r3$Plant)
	)

	# Keep parasitism counts in the long table alongside pest/disease counts to avoid duplicate wide/long columns
	dd3 <- data.frame(
		record_id = d3$record_id,
		months_after_planting_ = as.integer(r3$Month),
		pest_species = "whitefly",
		stage_ = "nymph",
		pest_incidence = as.integer(r3$Total_nymphs),
		nymphs_healthy_ = as.integer(r3$Healthy_nymphs),
		nymphs_parasitized_ = as.integer(r3$Parasitized_nymphs)
	)
	dd <- carobiner::bindr(dd, dd3)

	d <- carobiner::bindr(d1, d3)
	d$crop <- "cassava"
	d$yield_part <- "roots"
	d$insecticide_used <- d$treatment != "Control"
	d$insecticide_product <- ins_prod(d$treatment)
	d$fungicide_used <- grepl("Mandiplus", d$treatment)
	d$fungicide_product <- ifelse(d$fungicide_used, "mefenoxam; fludioxonil", "none")
	d$seed_treatment <- seed_trt(d$treatment)
	d$country <- "Tanzania"
	# Single country-level coverage coordinate from dataset metadata (no farm GPS)
	d$latitude <- -6.52471
	d$longitude <- 35.78784
	d$geo_from_source <- TRUE
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$stress <- "pests;disease"

	carobiner::write_files(path, meta, d, long = dd)
}
