# R script for "carob"
# license: GPL (>=3)

## NOTES
# Station chickpea (ICCV 2) trial of four actinomycete seed inoculants (SAI-13, SAI-29, VAI-7, VAI-40) plus an uninoculated control at ICRISAT BP-13B, Patancheru, 2013-14 post-rainy season.
# Stover, grain, and total dry matter are in t/ha (x1000 -> kg/ha). On every row, Dry matter equals Stover + Grain in t/ha (dry basis -> yield_isfresh = FALSE, yield_moisture = 0).
# Nodules, leaf area, leaf weight, pods, shoot weight, and plant height were recorded at 60 DAS (abstract) on the same agronomic table; per-plant weights remain per plant because plant density is not reported.
# "Seed weight" values (194-204) match 1000-seed weight (g) as in sister chickpea files (mapped to seed_weight; docx states g/plant, which is inconsistent with 64-90 seeds/plant).
# Rhizosphere soil at 60 and 100 DAS (0-15 cm, two replicates) is stored in the long table linked via record_id: organic carbon (%), total N and available P (ppm = mg/kg).
# Estimated ICRISAT Patancheru coordinates (17.511, 78.275) added with geo_from_source = FALSE and geo_uncertainty = 2000 m.

## ISSUES
# Fertilizer rates, irrigation statement, experimental design, and calendar harvest date are not in the files. Plot size is "4 m x 3 ridges" (area in m2 not stated).
# Confirm seed_weight (194-204 g per 1000 seeds vs docx label g/plant) and soil_N_total (650-790 ppm, matching total N in sister ICRISAT files).
# write_files: missing carob_contributor, irrigated, K/N/P_fertilizer, harvest_date; unknown per-plant variables.

## NOT PROCESSED
# Antagonistic potential of the four actinomycetes.xlsx: in vitro dual-culture ratings.
# Production of extracellular enzymes and plant growth.xlsx: in vitro enzyme/metabolite ratings.
# Metadata docx files: documentation used to verify units.

## NEW VARIABLES
# leaf_area_cm2_: leaf area at 60 DAS (cm2/plant).
# leaf_weight_g_: leaf weight at 60 DAS (g/plant).
# pod_number_plant_: pods per plant at 60 DAS.
# shoot_weight_g_plant_: shoot weight at 60 DAS (g/plant).
# seed_number_plant_: seeds per plant at harvest.


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
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 85,
		carob_effort = 0.2
	)

	f <- ff[basename(ff) == "Effect of the four actinomycetes on agronomic performance.xlsx"]
	r <- carobiner::read.excel(f, na="NA")
	trt <- trimws(as.character(r$Treatments))

	d <- data.frame(
		record_id = seq_len(nrow(r)),
		trial_id = "Patancheru_2013",
		country = "India",
		adm1 = "Telangana",
		adm2 = "Sangareddy",
		location = "Patancheru",
		site = "ICRISAT field BP-13B",
		latitude = 17.511,
		longitude = 78.275,
		geo_from_source = FALSE,
		geo_uncertainty = 2000,
		crop = "chickpea",
		variety = "ICCV 2",
		treatment = trt,
		seed_treatment = trt,
		inoculant = ifelse(trt == "Control", NA_character_, trt),
		inoculated = trt != "Control",
		rep = as.integer(r[["Replication number"]]),
		planting_date = "2013",
		harvest_days = 100,
		nodule_plant = as.numeric(r[["Nod number per plant"]]),
		leaf_area_cm2_ = as.numeric(r[["Leaf area"]]),
		leaf_weight_g_ = as.numeric(r[["Leaf weight"]]),
		pod_number_plant_ = as.numeric(r[["Pod number per plant"]]),
		shoot_weight_g_plant_ = as.numeric(r[["Shoot weight"]]),
		plant_height = as.numeric(r[["Plant height"]]),
		seed_weight = as.numeric(r[["Seed weight"]]),
		seed_number_plant_ = as.numeric(r[["Number of seeds"]]),
		yield = as.numeric(r[["Grain yield"]]) * 1000,
		dmy_residue = as.numeric(r[["Stover yield"]]) * 1000,
		dmy_total = as.numeric(r[["Dry matter"]]) * 1000,
		yield_part = "seed",
		yield_isfresh = FALSE,
		yield_moisture = 0,
		on_farm = FALSE,
		is_survey = FALSE
	)

	f1 <- ff[basename(ff) == "Effect of the four actinomycetes on rhizosphere.xlsx"]
	r1 <- carobiner::read.excel(f1, na="NA")
	dkey <- paste(d$seed_treatment, d$rep, sep="|")
	skey <- paste(trimws(as.character(r1$Treatments)), as.integer(r1[["Replication number"]]), sep="|")
	dlong <- data.frame(
		record_id = d$record_id[match(skey, dkey)],
		DAP = as.integer(sub(" days", "", as.character(r1[["Days after sowing"]]))),
		depth_top = 0,
		depth_bottom = 15,
		soil_N_total = as.numeric(r1$Nitrogen),
		soil_P = as.numeric(r1$Phosphorus),
		soil_SOC = as.numeric(r1[["Organic Carbon"]])
	)

	carobiner::write_files(path, meta, d, long=dlong)
}
