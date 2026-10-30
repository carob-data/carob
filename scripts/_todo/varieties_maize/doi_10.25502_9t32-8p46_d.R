# R script for "carob"
# license: GPL (>=3)

## NOTES
# Diallel of early pro-vitamin A maize and six hybrid checks, Nigeria, 2016-2017.
# Conditions in the file: Optimum, Low N, Drought, and Striga-infested, at Ikenne, Kadawa, Mokwa, Abuja, and Bagauda.
# Each location x year x condition is a trial. Variety (the pedigree) is the treatment.
# Grain_yield_ha_Kgha is kg/ha. MOIST is grain moisture at harvest (%).
# Days to 50% tasseling (ND50PerTas) are tassling_days. The file has no separate anthesis-day column. ASI is kept as recorded.
# Lodging percentages (RL_PERC, SL_PERC) are used. Striga ratings and emerged counts at 8 and 10 weeks (raw counts, not the STRCO transforms) go to the long table, as do ear-rot counts.
# Stress is none, nitrogen deficiency, drought, or striga, matching Condition.

## ISSUES
# The dataset abstract also lists Ile-Ife. That site is not in the data file.
# No coordinates. Metadata coverage point was not used. Georeference Ikenne, Kadawa, Mokwa, Abuja, and Bagauda (all Nigeria).
# No fertilizer rates. Low-N plots are not given a numeric N rate. Irrigation is not stated, including for the managed-drought rows at Ikenne, so irrigated was omitted.
# No day of planting or harvest.
# Male and Female are integer diallel codes, not parent names. The pedigree already has the cross. They were not mapped.
# Ear aspect includes 0. Stay-green includes 0 and is missing on most rows. Ear height stays within range. Left as recorded.
# Field weight is described as grams but the values are about 0-4, which does not match a plot of ears without a plot area. Not converted.
# Plant aspect is missing for part of the data.
# Expect missing longitude, latitude, geo_from_source, irrigated, N/P/K, harvest_date, and unknown-variable warnings.

## NOT PROCESSED
# gene-action-and-heterotic-pettern-and-inter-trait-relationship.csv, ID: row number.
# gene-action-and-heterotic-pettern-and-inter-trait-relationship.csv, Env: environment index. trial_id is location, year, and condition.
# gene-action-and-heterotic-pettern-and-inter-trait-relationship.csv, Male, Female: integer parent codes. No name key in the file.
# gene-action-and-heterotic-pettern-and-inter-trait-relationship.csv, PLST: plants per plot.
# gene-action-and-heterotic-pettern-and-inter-trait-relationship.csv, RL, SL: lodging counts. Percentage columns were used.
# gene-action-and-heterotic-pettern-and-inter-trait-relationship.csv, RLSTAT, SLSTAT: transformed lodging percentages.
# gene-action-and-heterotic-pettern-and-inter-trait-relationship.csv, PL_HARV, E_HARV: plants and ears at harvest per plot.
# gene-action-and-heterotic-pettern-and-inter-trait-relationship.csv, FWT: field weight. Dictionary says grams; values are about 0-4. Not converted.
# gene-action-and-heterotic-pettern-and-inter-trait-relationship.csv, STRCO1, STRCO2: transformed Striga counts. The untransformed count columns were used.
# data_dictionary_gene_action.csv: definitions used to map columns.

## NEW VARIABLES
# ears_per_plant_: ears harvested divided by plants in the plot (dimensionless).
# stay_green_: score at maturity, 1 = completely green and 9 = completely dried, as described in the dictionary. Zeros also occur.


carob_script <- function(path) {

"
Phenotypic data of Gene Action, Heterotic Patterns, and Inter-Trait Relationships of Early Maturing Pro-Vitamin a Maize Inbred Lines and Performance of Testcrosses under Contrasting Environments

Vitamin A deficiency is the leading cause of night blindness, total blindness, maternal and childhood mortality in developing countries. Drought, low soil nitrogen and Striga hermonthica parasitism are major constraints to maize production in sub-Saharan Africa (SSA). Thus, the development of multiple stress tolerant maize varieties with elevated levels of PVA is an economically feasible approach to simultaneously tackle malnutrition and food insecurity in SSA. The objectives of this study were to determine the gene action modulating the inheritance of grain yield and other traits, group the inbred lines, investigate inter-trait relationships among grain yield and other traits and assess the performance and stability of single-cross hybrids derived from a set of inbred lines under stress and non-stress environments. One hundred and ninety diallel crosses plus six hybrid checks were evaluated under managed drought at Ikenne during the 2016/17 and 2017/18 dry seasons, low soil N conditions at Mokwa and Ile-Ife, Striga infestation at Abuja and Mokwa, and optimal management conditions at Ikenne, Kadawa, Abuja, Bagauda and Mokwa during the 2016 and 2017 growing seasons. Both additive and non-additive gene actions were prominent in the inheritance of grain yield and other measured traits under stress and optimal management conditions. However, additive gene action was preponderant over the non-additive. The PVA inbreds were classified into three heterotic groups with TZEI 25 and TZEIOR 164 identified as inbred testers for heterotic groups 2 and 3, respectively. Plant and ear heights, ears per plant, plant and ear aspects were identified as reliable secondary traits for genetic enhancement of grain yield under both stress and non-stress conditions. Hybrids TZEIOR 4 x TZEIOR 158 and TZEIOR 119 × TZEIOR 158 were outstanding in performance and should be tested extensively for possible commercialization to combat malnutrition and food insecurity in SSA.
"

	uri <- "doi:10.25502/9t32-8p46/d"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "Stress Tolerant Maize for Africa (STMA)",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;tassling_days;silking_days;asi;plant_height;ear_height;aspect;ear_aspect",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 76,
		carob_effort = 0.7,
		notes = NA
	)

	f <- ff[basename(ff) == "gene-action-and-heterotic-pettern-and-inter-trait-relationship.csv"]
	r <- read.csv(f)

	variety <- trimws(as.character(r$Pedigree))
	cond <- trimws(as.character(r$Condition))
	stress <- ifelse(cond == "Optimum", "none",
		ifelse(cond == "Low N", "nitrogen deficiency",
			ifelse(cond == "Drought", "drought",
				ifelse(cond == "Striga-infested", "striga", cond))))
	striga <- cond == "Striga-infested"

	d <- data.frame(
		trial_name = trimws(as.character(r$Trial)),
		location = trimws(as.character(r$Loc)),
		country = "Nigeria",
		planting_date = as.character(r$Year),
		plot_id = as.character(r$Plot),
		rep = as.integer(r$Rep),
		block_id = as.character(r$Block),
		variety_code = as.character(r$Entry),
		variety = variety,
		variety_pedigree = variety,
		variety_type = "hybrid",
		treatment = variety,
		yield = as.numeric(r$Grain_yield_ha_Kgha),
		yield_moisture = as.numeric(r$MOIST),
		tassling_days = as.numeric(r$ND50PerTas),
		silking_days = as.numeric(r$DY_SK),
		asi = as.numeric(r$ASI),
		plant_height = as.numeric(r$PLHT),
		ear_height = as.numeric(r$E_HT),
		root_lodging = ifelse(is.na(r$RL_PERC), NA_character_, paste0(r$RL_PERC, "%")),
		stalk_lodging = ifelse(is.na(r$SL_PERC), NA_character_, paste0(r$SL_PERC, "%")),
		husk = trimws(as.character(r$HC)),
		aspect = as.numeric(r$PL_ASP),
		ear_aspect = as.numeric(r$E_ASP),
		e_rot = as.numeric(r$E_rot),
		stay_green_ = as.numeric(r$Stay_green_characteristics),
		ears_per_plant_ = as.numeric(r$EPP),
		str_rat1 = as.numeric(r$RAT1),
		str_rat2 = as.numeric(r$RAT2),
		str_co1 = as.numeric(r$Stria_emergence_count_at_8WAP),
		str_co2 = as.numeric(r$Stria_emergence_count_at_10WAP),
		stress = stress,
		striga_trial = striga,
		striga_infected = striga,
		crop = "maize",
		yield_part = "grain",
		borer_trial = FALSE,
		on_farm = FALSE,
		is_survey = FALSE
	)

	d$trial_id <- paste(d$location, d$planting_date, cond, sep = "_")
	d$record_id <- as.integer(seq_len(nrow(d)))
	mz_pd_long <- carobiner::get_function("mz_pd_long", path, group)
	long <- mz_pd_long(d)
	d[intersect(c("e_rot", "str_co1", "str_co2", "str_rat1", "str_rat2"), names(d))] <- NULL
	carobiner::write_files(path, meta, d, long = long)
}
