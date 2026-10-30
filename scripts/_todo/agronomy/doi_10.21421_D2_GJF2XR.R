# R script for "carob"
# license: GPL (>=3)

## NOTES
# Station chickpea (ICCV 2) trial of four actinomycete seed inoculants (SAI-13, SAI-29, VAI-7, VAI-40) plus an uninoculated control. ICRISAT BP-13B, 2013-14 post-rainy season.
# Agronomic file: stover and grain are t/ha (x1000 = kg/ha). The docx says dry matter is g/m2, but on every row it equals stover + grain in t/ha, so it is treated as t/ha and yield is the dry component.
# Nodules, leaf area, leaf weight, pods, shoot weight and plant height sit on the same row. The abstract says those scores were taken at 60 DAS. Weights are g/plant. No plant density, so they stay per plant.
# Seed number is per plant. The column "Seed weight" is 194-204. The docx says g/plant, which cannot be, with 64-90 seeds per plant; those numbers are the 1000-seed weight range used in the sister chickpea files. Mapped to seed_weight.
# Soil at 60 and 100 DAS (two replicates) is the long table. Organic carbon is %. Nitrogen and phosphorus are ppm.

## ISSUES
# No coordinates. Georeference ICRISAT BP-13B, Patancheru, Telangana, India.
# Fertilizer and irrigation are not in the files. Plot size is "4 m x 3 ridges", not m2.
# Confirm seed_weight: docx says g/plant, values are 194-204.
# Nitrogen is not labeled total vs extractable. Mapped to soil_N_total because it is 650-790 ppm, the same range as related files that call it total N. Confirm.
# 100 DAS is described as crop maturity, so harvest_days is 100. The harvest file itself has no date column.
# Design is not named.
# In vitro antagonism and enzyme scores are not crop measurements.
# write_files: missing carob_contributor, yield_moisture, latitude, longitude, irrigated, N/P/K, harvest_date; unknown per-plant variables; rep and seed_treatment also sit on the soil long table.

## NOT PROCESSED
# Antagonistic potential of the four actinomycetes.xlsx: Method, FOC, Rhizoctonia bataticola 6, Rhizoctonia bataticola 24, Rhizoctonia bataticola 115, S.rolfsii. Dual-culture scores.
# Production of extracellular enzymes and plant growth.xlsx: Lipase, Protaese, Chitinase, Sid, Hydrocyanic acid, Cellulase, Glu, IAA. In vitro ratings and concentrations.
# Metadata docx files: unit notes used above.

## NEW VARIABLES
# leaf_area_cm2_: leaf area (cm2); docx writes cm−2
# leaf_weight_g_: leaf weight (g/plant)
# pod_number_plant_: pods per plant
# shoot_weight_g_plant_: shoot weight (g/plant)
# seed_number_plant_: seeds per plant


carob_script <- function(path) {

"
Data on exploring plant growth-promotion actinomycetes from vermicompost and rhizosphere soil for yield enhancement in Chickpea

A total of 89 actinomycetes were screened for their antago-nistic activity against S. rolfsii, M. phaseolina (three strains viz.MP-6, MP-24 and MP-115) and FOC by dual culture assay asper the protocols on glucose casamino acid yeast extract agar plates. The culture filtrates of the promising isolates, based on the dual culture assay, were extracted by partitioning against ethyl acetate and the resultant organic and aqueous fractions were evaporated on a rotary evaporator and collected in a minimal volume of methanol. Both the fractions were evaluated for their antagonistic potential against the three fungal pathogens of Chickpea (M. phaseolina, FOC and S. rolfsii). For this, a fungal disc of 6 mm diameter was bored and kept at center of the potato dextrose agar plate amended with either organic or aqueous fractions.  The selected actinomycetes were evaluated for PGP potential on chickpea under on station field conditions in 2013–14 post-rainy season at Chickpea variety ICCV 2 was used for this trial. The actinomycete cultures were grown in SCB at 28 ± 2◦C for 5 days. Chickpea seeds were treated with the actinomycete cultures (108CFU per mL) for 50 min and sown. At 60 DAS, the nodule number, stem weight, pod number, pod weight, leaf weight and leaf area were noted and compared with un-inoculated control. The crop was harvested manually, stover yield, grain yield and total dry matter were noted.          Experiment location on Google Map
"

	uri <- "doi:10.21421/D2/GJF2XR"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "ICRISAT",
		publication = "doi:10.1016/j.bjm.2015.11.030",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "seed_treatment",
		response_vars = "yield;dmy_residue;dmy_total;plant_height;seed_weight",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.7
	)

	f <- ff[basename(ff) == "Effect of the four actinomycetes on agronomic performance.xlsx"]
	r <- carobiner::read.excel(f, na="NA")
	trt <- trimws(r$Treatments)

	d <- data.frame(
		trial_id = "Patancheru_2013",
		country = "India",
		adm1 = "Telangana",
		location = "Patancheru",
		site = "ICRISAT field BP-13B",
		crop = "chickpea",
		variety = "ICCV 2",
		treatment = trt,
		seed_treatment = trt,
		inoculant = ifelse(trt == "Control", NA_character_, trt),
		inoculated = trt != "Control",
		rep = as.integer(r$`Replication number`),
		planting_date = "2013",
		harvest_days = 100,
		nodule_plant = as.numeric(r$`Nod number per plant`),
		leaf_area_cm2_ = as.numeric(r$`Leaf area`),
		leaf_weight_g_ = as.numeric(r$`Leaf weight`),
		pod_number_plant_ = as.numeric(r$`Pod number per plant`),
		shoot_weight_g_plant_ = as.numeric(r$`Shoot weight`),
		plant_height = as.numeric(r$`Plant height`),
		seed_weight = as.numeric(r$`Seed weight`),
		seed_number_plant_ = as.numeric(r$`Number of seeds`),
		yield = as.numeric(r$`Grain yield`) * 1000,
		dmy_residue = as.numeric(r$`Stover yield`) * 1000,
		dmy_total = as.numeric(r$`Dry matter`) * 1000,
		yield_part = "seed",
		yield_isfresh = FALSE,
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	fs <- ff[basename(ff) == "Effect of the four actinomycetes on rhizosphere.xlsx"]
	rs <- carobiner::read.excel(fs, na="NA")
	long <- data.frame(
		trial_id = "Patancheru_2013",
		seed_treatment = trimws(rs$Treatments),
		rep = as.integer(rs$`Replication number`),
		DAP = as.integer(sub(" days", "", rs$`Days after sowing`)),
		soil_N_total = as.numeric(rs$Nitrogen),
		soil_P = as.numeric(rs$Phosphorus),
		soil_SOC = as.numeric(rs$`Organic Carbon`)
	)

	carobiner::write_files(path, meta, d, long=long)
}
