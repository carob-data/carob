# R script for "carob"
# license: GPL (>=3)

## NOTES
# Station RCBD on Vertisol at ICRISAT field BP 11, Patancheru, rabi 2012-13. Five Streptomyces strains (CAI-17, CAI-68, CAI-78, KAI-26, KAI-27) plus an uninoculated control (sister trial to 2013-14 doi:10.21421/D2/LFIFZF).
# Grain, stover, and total dry matter are in t/ha (x1000 -> kg/ha) on a dry basis (Grain + Stover = Dry matter -> yield_isfresh = FALSE, yield_moisture = 0).
# Coordinates (17.508398, 78.267686) are the midpoint of the Dataverse bounding box for BP 11 (geo_uncertainty = 110 m).
# Repeated in-season plant measurements (30 DAS and 60 DAS) and rhizosphere soil chemistry/biology (flowering and harvest, 0-15 cm, two replicates) are stored cleanly in the long table linked via record_id, avoiding duplicated per-date column suffixes.

## ISSUES
# Fertilizer rates, harvest date, and irrigation are not in the files.
# 30 DAS metadata docx says nodule dry weight is grams, but values (21-77) far exceed root dry weight (0.17-0.19 g) and match mg/plant in 2013-14; kept as recorded under nodule_dry_weight_.
# Plot numbers differ across files (and repeat in harvest), so records are linked by treatment and replication.
# One harvest row (CAI-17, rep 2) has NA grain/stover/dry matter; kept as recorded.
# Plant density is not reported and available-P method is not stated.
# write_files: missing carob_contributor, irrigated, K/N/P_fertilizer, harvest_date; NA yield on 1 row; unknown per-plant variables and dehydrogenase_.

## NOT PROCESSED
# All sheets, Treatment no: numeric treatment code.
# 30 DAS, 60 DAS, and soil sheets, Plot no / Code no: inconsistent plot ids and lab sample ids.

## NEW VARIABLES
# pod_weight_harvest_g_plant_: pod dry weight at harvest (g/plant).
# pod_number_harvest_: pods per plant at harvest.
# seed_mass_g_plant_: seed weight per plant at harvest (g/plant; distinct from 1000-seed weight).
# seed_number_plant_: seeds per plant at harvest.
# branch_number_: branches per plant at 60 DAS.
# pod_number_plant_: pods per plant at 60 DAS.
# pod_weight_g_plant_: pod weight at 60 DAS (g/plant).
# leaf_weight_g_: leaf weight at 60 DAS (g/plant).
# stem_weight_g_plant_: stem weight at 60 DAS (g/plant).
# root_weight_g_plant_: root dry weight (g/plant, at 30 and 60 DAS in long).
# leaf_area_cm2_: leaf area at 60 DAS (cm2/plant).
# nodule_dry_weight_: nodule dry weight at 30 DAS as recorded (docx says g, values 21-77 likely mg/plant).
# shoot_weight_g_plant_: shoot dry weight at 30 DAS (g/plant).
# dehydrogenase_: soil dehydrogenase activity at harvest (ug TPF/g soil/24 h).


carob_script <- function(path) {

"
Data on plant growth-promoting broad-spectrum Streptomyces sp. in chickpea and its effect on Grain yield 2012-13

This data set contain replicated observations on PGP capabilities of five strains (CAI-17, CAI-68, CAI-78, KAI-26 and KAI-27) under field conditions in chickpea in the 2012-13, post-rainy seasons on Vertisols. All experiments were carried out at the International Crops Research Institute for the Semi-Arid Tropics.  The experiment were laid out in a randomized complete block design (RCBD) with three replicates and subplot sizes of 4 m × 3 ridges (rows). In both the 2012-13 seasons, the nodule number, nodule weight, root weight and shoot weight were recorded at 30 days after sowing (DAS), and at 60 DAS, the plant height, pod number, pod weight, leaf area and stem weight were recorded. At both harvests, stover yield, grain yield, total dry matter, 1000-seed weight, pod weight, seed number and seed weight were recorded.   Soil samples were collected from the top 15 cm of the soil profile at the harvest stage and analysed for total nitrogen, available phosphorous and % organic carbon, microbial biomass carbon and dehydrogenase activity.  Data sets were analysed and published. Publication details can be found below in publication section.

  Experiment location on Google Map
"

	uri <- "doi:10.21421/D2/VR7CIY"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=3,
		data_organization = "ICRISAT",
		publication = "doi:10.1186/s40064-015-0811-3",
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "treatment;inoculated",
		response_vars = "yield;dmy_residue;dmy_total;nodule_plant;plant_height",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 90,
		carob_effort = 0.25
	)

	f1 <- ff[basename(ff) == "Final Harvest for chickpea on plant growth-promoting broad-spectrum Streptomyces sp.xlsx"]
	f2 <- ff[basename(ff) == "Morphological observation at 60 DAS plant growth-promoting broad-spectrum Streptomyces sp. in chickpea.xlsx"]
	f3 <- ff[basename(ff) == "Nodulation at 30 DAS on plant growth-promoting broad-spectrum Streptomyces sp. in chickpea..xlsx"]
	f4 <- ff[basename(ff) == "Soil biological propertiee of plant growth-promoting broad-spectrum Streptomyces sp. in chickpea.xlsx"]
	f5 <- ff[basename(ff) == "Soil chemical analysis of flowering stage on plant growth-promoting broad-spectrum Streptomyces sp. in chickpea.xlsx"]
	f6 <- ff[basename(ff) == "Soil chemical analysis of Harvest stage on plant growth-promoting broad-spectrum Streptomyces sp. in chickpea.xlsx"]

	r1 <- carobiner::read.excel(f1, na="NA")
	r2 <- carobiner::read.excel(f2, na="NA")
	r3 <- carobiner::read.excel(f3, na="NA")
	r4 <- carobiner::read.excel(f4, na="NA")
	r5 <- carobiner::read.excel(f5, na="NA")
	r6 <- carobiner::read.excel(f6, na="NA")

	trt <- trimws(as.character(r1$Treatment))
	d <- data.frame(
		record_id = seq_len(nrow(r1)),
		treatment = trt,
		rep = as.integer(r1[["Replication number"]]),
		plot_id = as.character(r1[["Plot no"]]),
		yield = as.numeric(r1[["Grain yield"]]) * 1000,
		dmy_residue = as.numeric(r1[["Stover yield"]]) * 1000,
		dmy_total = as.numeric(r1[["Dry matter"]]) * 1000,
		seed_weight = as.numeric(r1[["Thousand seed weight"]]),
		pod_weight_harvest_g_plant_ = as.numeric(r1[["Pod weight"]]),
		pod_number_harvest_ = as.numeric(r1[["Pod number"]]),
		seed_mass_g_plant_ = as.numeric(r1[["Seed weight"]]),
		seed_number_plant_ = as.numeric(r1[["Seed number"]]),
		inoculated = trt != "Control",
		inoculant = ifelse(trt == "Control", NA_character_, trt),
		crop = "chickpea",
		yield_part = "seed",
		country = "India",
		adm1 = "Telangana",
		adm2 = "Sangareddy",
		location = "Patancheru",
		site = "Research field BP 11",
		season = "rabi",
		planting_date = "2012",
		soil_type = "Vertisol",
		trial_id = "Patancheru_2012",
		on_farm = FALSE,
		is_survey = FALSE,
		yield_moisture = 0,
		yield_isfresh = FALSE,
		longitude = 78.267686,
		latitude = 17.508398,
		geo_uncertainty = 110,
		geo_from_source = TRUE
	)

	dkey <- paste(d$treatment, d$rep, sep="|")
	rid <- function(tvec, rvec) d$record_id[match(paste(trimws(as.character(tvec)), as.integer(rvec), sep="|"), dkey)]

	l30 <- data.frame(
		record_id = rid(r3$Treatments, r3[["Replication number"]]),
		DAP = 30L,
		nodule_plant = as.numeric(r3[["No of nodules per plant"]]),
		nodule_dry_weight_ = as.numeric(r3[["Nodules dry weight"]]),
		root_weight_g_plant_ = as.numeric(r3[["Root dry weight"]]),
		shoot_weight_g_plant_ = as.numeric(r3[["Shoot dry weight"]])
	)
	l60 <- data.frame(
		record_id = rid(r2$Treatment, r2[["Replication number"]]),
		DAP = 60L,
		plant_height = as.numeric(r2[["Plant height"]]),
		branch_number_ = as.numeric(r2[["No of branches"]]),
		pod_number_plant_ = as.numeric(r2[["Pod number"]]),
		leaf_weight_g_ = as.numeric(r2[["Leaf weight"]]),
		pod_weight_g_plant_ = as.numeric(r2[["Pod weight"]]),
		stem_weight_g_plant_ = as.numeric(r2[["Stem weight"]]),
		root_weight_g_plant_ = as.numeric(r2[["Root weight"]]),
		leaf_area_cm2_ = as.numeric(r2[["Leaf area"]])
	)
	l_sf <- data.frame(
		record_id = rid(r5$Treatment, r5[["Replication number"]]),
		growth_stage = "flowering",
		depth_top = 0,
		depth_bottom = 15,
		soil_SOC = as.numeric(r5[["Organic carbon"]]),
		soil_N_total = as.numeric(r5$Nitrogen),
		soil_P = as.numeric(r5$Phosphorus)
	)
	l_sh <- data.frame(
		record_id = rid(r6$Treatment, r6[["Replication number"]]),
		growth_stage = "harvest",
		depth_top = 0,
		depth_bottom = 15,
		soil_SOC = as.numeric(r6[["Organic carbon"]]),
		soil_N_total = as.numeric(r6$Nitrogen),
		soil_P = as.numeric(r6$Phosphorus)
	)
	l_bio <- data.frame(
		record_id = rid(r4$Treatment, r4[["Replication number"]]),
		growth_stage = "harvest",
		depth_top = 0,
		depth_bottom = 15,
		soil_MBC = as.numeric(r4[["Microbial boimass C"]]),
		dehydrogenase_ = as.numeric(r4$Dehydrogenase)
	)
	l_sh_all <- merge(l_sh, l_bio, by=c("record_id", "growth_stage", "depth_top", "depth_bottom"), all=TRUE)

	dlong <- carobiner::bindr(l30, l60, l_sf, l_sh_all)

	carobiner::write_files(path, meta, d, long=dlong)
}
