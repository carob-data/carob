# R script for "carob"
# license: GPL (>=3)

## NOTES
# Station RCBD on Vertisol at ICRISAT field BP 11, Patancheru, rabi 2012-13. Five Streptomyces strains and an uninoculated control.
# Grain, stover and total dry matter are t/ha in the harvest metadata. Multiplied by 1000 to kg/ha. Grain + stover equals dry matter, so yield_moisture is 0.
# Soil organic carbon is percent. Total N and available P are ppm, mapped as mg/kg. Samples are 0-15 cm. Microbial biomass C is µg/g and dehydrogenase is µg TPF/g/24 h.
# Coordinates are the midpoint of the Dataverse bounding box for BP 11. geo_uncertainty is the distance from that midpoint to a corner of the box (about 110 m).
# 30 DAS and 60 DAS sheets were joined to harvest on treatment and replication. Plot numbers are not the same across sheets.
# Soil chemistry and biology were recorded for two replications only.

## ISSUES
# Fertilizer rates, harvest date and irrigation are not in the files.
# 30 DAS metadata says nodule dry weight is grams, but the values (21-77) are far above the root dry weight (0.17-0.19 g) and the 2013-14 file records the same trait in mg/plant. The number is kept as recorded and is not converted to kg/ha.
# Harvest plot numbers are repeated across some treatments, so plot_id is not a unique plot key.
# Available-P method is not stated. Flowering-stage soil is a second date and is kept under separate names.
# Per-plant weights and counts were not converted to per-ha because plant density is not reported. Harvested area is given as both 0.9 and 1.5 m2 without saying which plots.
# One harvest row (CAI-17, rep 2) has no grain, stover or dry matter. It was kept.
# write_files: missing carob_contributor; missing irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date; unknown variables pod_weight_, pod_number_, seed_mass_, seed_number_, branch_number_, pod_number_60_, leaf_weight_, pod_weight_60_, stem_weight_, root_weight_60_, leaf_area_, nodule_dry_weight_, root_weight_30_, shoot_weight_30_, soil_SOC_flowering_, soil_N_flowering_, soil_P_flowering_, dehydrogenase_; NA detected in yield.

## NOT PROCESSED
# All sheets, Treatment no: a numeric code for the strain name already in treatment.
# Harvest, plot numbers that collide across treatments: kept as plot_id but not used as a key.
# 30 DAS and 60 DAS, Plot no: not the harvest plot numbers.
# Soil sheets, Code no / code no: laboratory sample number.
# 30 DAS metadata lists plant height and branches, but those columns are not in the xlsx.

## NEW VARIABLES
# pod_weight_: pod dry weight at harvest, g/plant
# pod_number_: pods per plant at harvest
# seed_mass_: seed weight at harvest, g/plant
# seed_number_: seeds per plant at harvest
# branch_number_: branches per plant at 60 DAS
# pod_number_60_: pods per plant at 60 DAS
# leaf_weight_: leaf weight at 60 DAS, g/plant
# pod_weight_60_: pod weight at 60 DAS, g/plant
# stem_weight_: stem weight at 60 DAS, g/plant
# root_weight_60_: root weight at 60 DAS, g/plant
# leaf_area_: leaf area at 60 DAS, cm2/plant
# nodule_dry_weight_: nodule dry weight at 30 DAS, as recorded (see ISSUES)
# root_weight_30_: root dry weight at 30 DAS, g/plant
# shoot_weight_30_: shoot dry weight at 30 DAS, g/plant
# soil_SOC_flowering_: organic carbon at flowering, percent
# soil_N_flowering_: total N at flowering, mg/kg
# soil_P_flowering_: available P at flowering, mg/kg
# dehydrogenase_: dehydrogenase activity, µg TPF/g soil/24 h

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
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 75,
		carob_effort = 0.8
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

	trt <- trimws(r1$Treatment)
	d <- data.frame(
		treatment = trt,
		rep = as.integer(r1$`Replication number`),
		plot_id = as.character(r1$`Plot no`),
		yield = as.numeric(r1$`Grain yield`) * 1000,
		dmy_residue = as.numeric(r1$`Stover yield`) * 1000,
		dmy_total = as.numeric(r1$`Dry matter`) * 1000,
		seed_weight = as.numeric(r1$`Thousand seed weight`),
		pod_weight_ = as.numeric(r1$`Pod weight`),
		pod_number_ = as.numeric(r1$`Pod number`),
		seed_mass_ = as.numeric(r1$`Seed weight`),
		seed_number_ = as.numeric(r1$`Seed number`),
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
		# Dataverse bounding box for field BP 11
		longitude = 78.267686,
		latitude = 17.508398,
		geo_uncertainty = 110,
		geo_from_source = TRUE,
		depth_top = 0,
		depth_bottom = 15
	)

	m60 <- data.frame(
		treatment = trimws(r2$Treatment),
		rep = as.integer(r2$`Replication number`),
		plant_height = as.numeric(r2$`Plant height`),
		branch_number_ = as.numeric(r2$`No of branches`),
		pod_number_60_ = as.numeric(r2$`Pod number`),
		leaf_weight_ = as.numeric(r2$`Leaf weight`),
		pod_weight_60_ = as.numeric(r2$`Pod weight`),
		stem_weight_ = as.numeric(r2$`Stem weight`),
		root_weight_60_ = as.numeric(r2$`Root weight`),
		leaf_area_ = as.numeric(r2$`Leaf area`)
	)
	d <- merge(d, m60, by=c("treatment", "rep"), all.x=TRUE, sort=FALSE)

	m30 <- data.frame(
		treatment = trimws(r3$Treatments),
		rep = as.integer(r3$`Replication number`),
		nodule_plant = as.numeric(r3$`No of nodules per plant`),
		nodule_dry_weight_ = as.numeric(r3$`Nodules dry weight`),
		root_weight_30_ = as.numeric(r3$`Root dry weight`),
		shoot_weight_30_ = as.numeric(r3$`Shoot dry weight`)
	)
	d <- merge(d, m30, by=c("treatment", "rep"), all.x=TRUE, sort=FALSE)

	bio <- data.frame(
		treatment = trimws(r4$Treatment),
		rep = as.integer(r4$`Replication number`),
		soil_MBC = as.numeric(r4$`Microbial boimass C`),
		dehydrogenase_ = as.numeric(r4$Dehydrogenase)
	)
	d <- merge(d, bio, by=c("treatment", "rep"), all.x=TRUE, sort=FALSE)

	# ppm as reported for total N and available P
	sH <- data.frame(
		treatment = trimws(r6$Treatment),
		rep = as.integer(r6$`Replication number`),
		soil_SOC = as.numeric(r6$`Organic carbon`),
		soil_N_total = as.numeric(r6$Nitrogen),
		soil_P = as.numeric(r6$Phosphorus)
	)
	d <- merge(d, sH, by=c("treatment", "rep"), all.x=TRUE, sort=FALSE)

	sF <- data.frame(
		treatment = trimws(r5$Treatment),
		rep = as.integer(r5$`Replication number`),
		soil_SOC_flowering_ = as.numeric(r5$`Organic carbon`),
		soil_N_flowering_ = as.numeric(r5$Nitrogen),
		soil_P_flowering_ = as.numeric(r5$Phosphorus)
	)
	d <- merge(d, sF, by=c("treatment", "rep"), all.x=TRUE, sort=FALSE)

	carobiner::write_files(path, meta, d)
}
