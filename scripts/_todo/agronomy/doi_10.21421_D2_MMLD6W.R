# R script for "carob"
# license: GPL (>=3)

## NOTES
# Two irrigated rabi seasons (2012-13 and 2013-14) of chickpea ICCV 2 at ICRISAT, Patancheru. Six Streptomyces strains and an uninoculated control, RCBD, three replicates.
# Sowing was October 2012 and November 2013 (dataset description).
# Grain, stover and dry matter are t/ha, multiplied by 1000. Treated as dry weight because dry matter is reported with them. yield_moisture is 0.
# 30 DAS and 60 DAS are in one file and fill different columns, so both were joined onto the harvest row by year, treatment and replication.
# Nodule and root weights at 30 DAS are mg/plant. Shoot, stem and leaf weights are g/plant. Leaf area is cm2.
# Soil at harvest (two replicates): organic carbon in percent, N and available P in ppm (mg/kg), microbial biomass C in µg/g, dehydrogenase in µg TPF/g/24 h. Depth 0-15 cm from the description.
# Patancheru is named but this dataset has no coordinates.

## ISSUES
# Fertilizer product and rate are not reported. The 30 DAS metadata types "2031-14" for the second year; the data and the abstract say 2013-14.
# Soil biology and chemistry were not recorded for the third replicate.
# Plant density is not reported, so per-plant weights were not expanded to kg/ha.
# Available-P method is not stated.
# No coordinates in this dataset. A human should georeference Patancheru.
# write_files: missing carob_contributor; missing latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date; unknown variables pod_weight_, seed_number_, seed_mass_, nodule_dry_mg_, root_dry_mg_, shoot_weight_, stem_weight_, leaf_area_, leaf_weight_, pod_number_, dehydrogenase_.

## NOT PROCESSED
# Agronomic and soil sheets do not include plot numbers.
# 30 DAS columns that are empty at that date (plant height, stem, leaf, pods) and 60 DAS columns that are empty then (nodules, root, shoot): each trait was taken from the date that has values.

## NEW VARIABLES
# pod_weight_: pod weight at harvest, g/plant
# seed_number_: seeds per plant at harvest
# seed_mass_: seed weight at harvest, g/plant
# nodule_dry_mg_: nodule weight at 30 DAS, mg/plant
# root_dry_mg_: root weight at 30 DAS, mg/plant
# shoot_weight_: shoot weight at 30 DAS, g/plant
# stem_weight_: stem weight at 60 DAS, g/plant
# leaf_area_: leaf area at 60 DAS, cm2
# leaf_weight_: leaf weight at 60 DAS, g/plant
# pod_number_: pods per plant at 60 DAS
# dehydrogenase_: dehydrogenase at harvest, µg TPF/g soil/24 h

carob_script <- function(path) {

"
Effect of Plant growth-promoting Streptomyces sp. on growth promotion and grain yield in Chickpea (Cicer arietinum L)

This dataset contains, field trials were conducted for two cropping seasons in 2012–13 and 2013–14 at ICRISAT, Patancheru. The experiment was laid out with three replicates and sub-plot sizes of 4 x 3 m rows in ridges in a randomized complete block design (RCBD) under irrigated condition. The Streptomyces strains (CAI-13, CAI-85, CAI-93, CAI-140, CAI-155 and KAI-180) were grown individually on starch casein at 28°C for 4 to 5 days. Seeds of Chickpea variety ICCV 2 (duration 90 days) were treated individually with a Streptomyces strain (108 CFU per ml) for 45 min and hand sown on Oct 2012 in the first year and Nov 2013 in the second year Streptomyces strain was applied once in 15 days, from the sowing day, on the soil close to the plant until the flowering stage. The Chickpea crop was harvested manually. Samplings were done at 30 DAS, 60 DAS and at crop maturity. On the sampling day, plant underground and aerial parts were harvested separately with care to eliminate border effects in each plot. During both the seasons of 2012–13, 2013–14, Chickpea growth performance was measured in terms of nodule number, nodule weight, root weight and shoot weight at 30 DAS; plant height, pod number, pod weight, leaf area, leaf weight and stem weight at 60 DAS; and stover yield, grain yield, total dry matter (TDM), 1000 seed weight, pod weight, seed number and seed weight at crop maturity. Also, at crop maturity, rhizosphere soil samples were collected from top 0 to 15 cm soil and analysed for total nitrogen, available phosphorous, organic carbon, microbial biomass carbon and dehydrogenase activity.
"

	uri <- "doi:10.21421/D2/MMLD6W"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "ICRISAT",
		publication = "doi:10.1007/s13205-015-0283-8",
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "treatment;inoculated",
		response_vars = "yield;dmy_residue;plant_height;nodule_plant",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 75,
		carob_effort = 0.7
	)

	f1 <- ff[basename(ff) == "Effect of Plant growth-promoting Streptomyces sp 30 DAS, 60 DAS.xlsx"]
	f2 <- ff[basename(ff) == "Effect of the six Streptomyces sp microbial total N, P OC.xlsx"]
	f3 <- ff[basename(ff) == "Effect of the six Streptomyces sp. on agronomic.xlsx"]

	r1 <- carobiner::read.excel(f1, na="NA")
	r2 <- carobiner::read.excel(f2, na="NA")
	r3 <- carobiner::read.excel(f3, na="NA")

	trt <- trimws(r3$Treatment)
	yr <- trimws(r3$Year)
	d <- data.frame(
		treatment = trt,
		rep = as.integer(r3$`Replication number`),
		year = yr,
		variety = "ICCV 2",
		yield = as.numeric(r3$`Grain yield`) * 1000,
		dmy_residue = as.numeric(r3$`Stover yield`) * 1000,
		dmy_total = as.numeric(r3$`Dry matter`) * 1000,
		seed_weight = as.numeric(r3$`Thousand seed weight`),
		pod_weight_ = as.numeric(r3$`Pod weight`),
		seed_number_ = as.numeric(r3$`Seed number`),
		seed_mass_ = as.numeric(r3$`Seed weight`),
		inoculated = trt != "Control",
		inoculant = ifelse(trt == "Control", NA_character_, trt),
		planting_date = ifelse(yr == "2012/13", "2012-10", "2013-11"),
		trial_id = ifelse(yr == "2012/13", "Patancheru_2012", "Patancheru_2013"),
		crop = "chickpea",
		yield_part = "seed",
		country = "India",
		adm1 = "Telangana",
		adm2 = "Sangareddy",
		location = "Patancheru",
		season = "rabi",
		irrigated = TRUE,
		on_farm = FALSE,
		is_survey = FALSE,
		yield_moisture = 0,
		yield_isfresh = FALSE,
		geo_from_source = FALSE,
		depth_top = 0,
		depth_bottom = 15
	)

	g30 <- r1[r1$DAS == 30, ]
	m30 <- data.frame(
		treatment = trimws(g30$Treatment),
		rep = as.integer(g30$`Replication number`),
		year = trimws(g30$Year),
		nodule_plant = as.numeric(g30$`Nodule number`),
		nodule_dry_mg_ = as.numeric(g30$`Nodule weight`),
		root_dry_mg_ = as.numeric(g30$`Root weight`),
		shoot_weight_ = as.numeric(g30$`Shoot weight`)
	)
	d <- merge(d, m30, by=c("treatment", "rep", "year"), all.x=TRUE, sort=FALSE)

	g60 <- r1[r1$DAS == 60, ]
	m60 <- data.frame(
		treatment = trimws(g60$Treatment),
		rep = as.integer(g60$`Replication number`),
		year = trimws(g60$Year),
		plant_height = as.numeric(g60$`Plant height`),
		stem_weight_ = as.numeric(g60$`Stem weight`),
		leaf_area_ = as.numeric(g60$`Leaf area`),
		leaf_weight_ = as.numeric(g60$`Leaf weight`),
		pod_number_ = as.numeric(g60$`Pod number`)
	)
	d <- merge(d, m60, by=c("treatment", "rep", "year"), all.x=TRUE, sort=FALSE)

	s <- data.frame(
		treatment = trimws(r2$Treatments),
		rep = as.integer(r2$`Replication number`),
		year = trimws(r2$Year),
		soil_MBC = as.numeric(r2$`Microbial biomass C`),
		dehydrogenase_ = as.numeric(r2$Dehydrogenase),
		soil_N_total = as.numeric(r2$Nitrogen),
		soil_P = as.numeric(r2$Phosphorus),
		soil_SOC = as.numeric(r2$`Organic carbon`)
	)
	d <- merge(d, s, by=c("treatment", "rep", "year"), all.x=TRUE, sort=FALSE)
	d$year <- NULL

	carobiner::write_files(path, meta, d)
}
