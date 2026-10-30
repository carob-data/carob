# R script for "carob"
# license: GPL (>=3)

## NOTES
# Station RCBD on Vertisol at ICRISAT field BP 13, Patancheru, rabi 2013-14. Same five Streptomyces strains and an uninoculated control as doi:10.21421/D2/VR7CIY.
# Grain, stover and total dry matter are t/ha. Multiplied by 1000 to kg/ha. They are on a dry basis (dry matter is the total), so yield_moisture is 0.
# Soil organic carbon is percent. N and available P are ppm (mg/kg). Depth 0-15 cm. Microbial biomass C is µg/g. Dehydrogenase is µg TPF/g/24 h.
# Nodule dry weight is mg/plant at 30 and at 60 DAS. Root dry weight at 30 DAS is mg/plant; root weight at 60 DAS is grams.
# Coordinates are the midpoint of the Dataverse bounding box for BP 13. geo_uncertainty is about 80 m, the distance from the midpoint to a corner.
# Rows were joined on treatment and replication. Soil files have two replications; the crop files have three.

## ISSUES
# Fertilizer rates, harvest date and irrigation are not reported.
# The harvest-stage soil metadata text says "at flowering stage", which copies the flowering file. The file name says harvest, and that is how the harvest columns were assigned.
# Plot numbers differ between harvest, 30 DAS and 60 DAS, so they were not used as the join key.
# Available-P method is not stated. Plant density is not reported, so per-plant weights stay per plant. Harvest area is not given in this year's files.
# write_files: missing carob_contributor; missing irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date; unknown variables pod_weight_, pod_number_, seed_mass_, seed_number_, branch_number_, nodule_plant_60_, nodule_dry_mg_60_, root_weight_60_, stem_weight_, pod_number_60_, pod_weight_60_, leaf_weight_, leaf_area_, plant_height_30_, branch_number_30_, nodule_dry_mg_, root_dry_mg_, shoot_weight_, soil_SOC_flowering_, soil_N_flowering_, soil_P_flowering_, dehydrogenase_.

## NOT PROCESSED
# All sheets, Treatment no: numeric code for the strain already stored in treatment.
# 30 DAS, 60 DAS and soil sheets, Plot no: not consistent with the harvest plot numbers.
# Soil sheets, Code no: laboratory sample number.

## NEW VARIABLES
# pod_weight_: pod weight at harvest, g/plant
# pod_number_: filled pods per plant at harvest
# seed_mass_: seed weight at harvest, g/plant
# seed_number_: seeds per plant at harvest
# plant_height_30_: plant height at 30 DAS, cm
# branch_number_30_: branches per plant at 30 DAS
# branch_number_: branches per plant at 60 DAS
# nodule_plant_60_: nodules per plant at 60 DAS
# nodule_dry_mg_: nodule dry weight at 30 DAS, mg/plant
# nodule_dry_mg_60_: nodule dry weight at 60 DAS, mg/plant
# root_dry_mg_: root dry weight at 30 DAS, mg/plant
# shoot_weight_: shoot dry weight at 30 DAS, g/plant
# root_weight_60_: root weight at 60 DAS, g/plant
# stem_weight_: stem weight at 60 DAS, g/plant
# pod_number_60_: pods per plant at 60 DAS
# pod_weight_60_: pod weight at 60 DAS, g/plant
# leaf_weight_: leaf weight at 60 DAS, g/plant
# leaf_area_: leaf area at 60 DAS, cm2/plant
# soil_SOC_flowering_: organic carbon at flowering, percent
# soil_N_flowering_: total N at flowering, mg/kg
# soil_P_flowering_: available P at flowering, mg/kg
# dehydrogenase_: dehydrogenase activity, µg TPF/g soil/24 h

carob_script <- function(path) {

"
Data on plant growth-promoting broad-spectrum Streptomyces sp. in chickpea and its effect on grain yield 2013-14

This data set contain replicated observations on PGP capabilities of five strains (CAI-17, CAI-68, CAI-78, KAI-26 and KAI-27) under field conditions in chickpea in the 2013-14, post-rainy seasons on Vertisols. All experiments were carried out at the International Crops Research Institute for the Semi-Arid Tropics. The experiment were laid out in a randomized complete block design (RCBD) with three replicates and subplot sizes of 4 m × 3 ridges (rows). In both the 2013-14 seasons, the nodule number, nodule weight, root weight and shoot weight were recorded at 30 days after sowing (DAS), and at 60 DAS, the plant height, pod number, pod weight, leaf area and stem weight were recorded. At both harvests, stover yield, grain yield, total dry matter, 1000-seed weight, pod weight, seed number and seed weight were recorded. Soil samples were collected from the top 15 cm of the soil profile at the harvest stage and analysed for total nitrogen, available phosphorous and % organic carbon, microbial biomass carbon and dehydrogenase activity. Data sets were analysed and published. Publication details can be found below in publication section.         Experiment location on Google Map
"

	uri <- "doi:10.21421/D2/LFIFZF"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
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

	f1 <- ff[basename(ff) == "Data on plant growth-promoting broad-spectrum Streptomyces sp. in chickpea harvesting.xlsx"]
	f2 <- ff[basename(ff) == "Data on plant growth-promoting broad-spectrum Streptomyces sp. in chickpea soil biological propertiee.xlsx"]
	f3 <- ff[basename(ff) == "Data on plant growth-promoting broad-spectrum Streptomyces sp. in chickpea soil chemical flowering stage.xlsx"]
	f4 <- ff[basename(ff) == "Data on plant growth-promoting broad-spectrum Streptomyces sp. in chickpea soil chemical harvesting stage.xlsx"]
	f5 <- ff[basename(ff) == "Data on plant growth-promoting broad-spectrum Streptomyces sp. in vegetative growth sampling of chickpea at 60 DAS.xlsx"]
	f6 <- ff[basename(ff) == "Plant growth-promoting nodulation at 30 DAS broad-spectrum Streptomyces sp. in chickpea.xlsx"]

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
		pod_number_ = as.numeric(r1$`Filled pod number`),
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
		site = "Research field BP 13",
		season = "rabi",
		planting_date = "2013",
		soil_type = "Vertisol",
		trial_id = "Patancheru_2013",
		on_farm = FALSE,
		is_survey = FALSE,
		yield_moisture = 0,
		yield_isfresh = FALSE,
		longitude = 78.268106,
		latitude = 17.506482,
		geo_uncertainty = 80,
		geo_from_source = TRUE,
		depth_top = 0,
		depth_bottom = 15
	)

	m60 <- data.frame(
		treatment = trimws(r5$Treatment),
		rep = as.integer(r5$`Replication number`),
		plant_height = as.numeric(r5$`Plant height`),
		branch_number_ = as.numeric(r5$`No of branches`),
		nodule_plant_60_ = as.numeric(r5$`Nodule number per plant`),
		nodule_dry_mg_60_ = as.numeric(r5$`Nodule dry weight`),
		root_weight_60_ = as.numeric(r5$`Root weight`),
		stem_weight_ = as.numeric(r5$`Stem weight`),
		pod_number_60_ = as.numeric(r5$`Pod number`),
		pod_weight_60_ = as.numeric(r5$`Pod weight`),
		leaf_weight_ = as.numeric(r5$`Leaf weight`),
		leaf_area_ = as.numeric(r5$`Leaf area`)
	)
	d <- merge(d, m60, by=c("treatment", "rep"), all.x=TRUE, sort=FALSE)

	m30 <- data.frame(
		treatment = trimws(r6$Treatment),
		rep = as.integer(r6$`Replication number`),
		plant_height_30_ = as.numeric(r6$`Plant height`),
		branch_number_30_ = as.numeric(r6$`No of branches`),
		nodule_plant = as.numeric(r6$`No of nodules`),
		nodule_dry_mg_ = as.numeric(r6$`Nodules dry weight`),
		root_dry_mg_ = as.numeric(r6$`Root dry weight`),
		shoot_weight_ = as.numeric(r6$`shoot dry weight`)
	)
	d <- merge(d, m30, by=c("treatment", "rep"), all.x=TRUE, sort=FALSE)

	bio <- data.frame(
		treatment = trimws(r2$Treatment),
		rep = as.integer(r2$`Replication number`),
		soil_MBC = as.numeric(r2$`Microbial boimass Carbon`),
		dehydrogenase_ = as.numeric(r2$`Dehydrogenase activity`)
	)
	d <- merge(d, bio, by=c("treatment", "rep"), all.x=TRUE, sort=FALSE)

	sH <- data.frame(
		treatment = trimws(r4$Treatment),
		rep = as.integer(r4$`Replication number`),
		soil_SOC = as.numeric(r4$`Organic carbon`),
		soil_N_total = as.numeric(r4$Nitrogen),
		soil_P = as.numeric(r4$Phosphorus)
	)
	d <- merge(d, sH, by=c("treatment", "rep"), all.x=TRUE, sort=FALSE)

	sF <- data.frame(
		treatment = trimws(r3$Treatment),
		rep = as.integer(r3$`Replication number`),
		soil_SOC_flowering_ = as.numeric(r3$`Organic carbon`),
		soil_N_flowering_ = as.numeric(r3$Nitrogen),
		soil_P_flowering_ = as.numeric(r3$Phosphorus)
	)
	d <- merge(d, sF, by=c("treatment", "rep"), all.x=TRUE, sort=FALSE)

	carobiner::write_files(path, meta, d)
}
