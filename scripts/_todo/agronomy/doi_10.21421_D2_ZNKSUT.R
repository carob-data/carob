# R script for "carob"
# license: GPL (>=3)

## NOTES
# Station chickpea (ICCV2) seed-inoculation trial of Streptomyces CAI-21, CAI-26 and MMA-32 plus an uninoculated control. ICRISAT field BP-13B, post-rainy seasons 2012/13 and 2013/14.
# Harvest file: stover, grain and dry matter are t/ha (x1000 = kg/ha). On every row grain + stover = dry matter, so yield is the dry component. Thousand-seed weight is g (seed_weight). Pod weight, seed weight and seed number at harvest are per plant.
# 30 DAS and 60 DAS files are the long table (nodules, biomass, height, pods, leaves). 2013/14 leaf area and leaf weight are swapped relative to 2012/13 and to the stated units (cm2 vs g/plant); they are exchanged for that year.
# Soil at harvest (two replicates) is merged onto the harvest rows. Organic carbon is %. Nitrogen and phosphorus are ppm (mg/kg).

## ISSUES
# No coordinates. Georeference ICRISAT BP-13B, Patancheru, Telangana, India. The state was Andhra Pradesh during these seasons.
# The Dataverse citation (Philippine Agricultural Scientist 2015, http://oar.icrisat.org/9056/) has no DOI.
# Fertilizer, irrigation, and the harvest day count are not in the files. Plot size is "4 m x 3 ridges", not m2.
# Nitrogen is not labeled total vs extractable. It is mapped to soil_N_total because it is 631-807 ppm, the same range as the related ICRISAT files that call this analysis total N, beside available-P values of 7-12 ppm. Confirm.
# 30 DAS root weight is 160-213 while shoot weight is 1.3-2.9 g/plant and nodule weight is in mg/plant. The docx says root weight is g/plant, which is not credible next to the shoot. Left as recorded.
# One thousand-seed weight and one harvest pod weight are missing.
# Design is not stated (three replicates, but not named RCBD in this record).
# seed_weight_g_plant_ and nodule_weight_mg_plant_ must not be renamed to seed_weight or nodule_weight; those terms are 1000-seed weight (g) and nodule weight (kg/ha). Harvest pod weight and the 60 DAS pod weight share pod_weight_g_plant_.
# write_files: missing carob_contributor, yield_moisture, latitude, longitude, irrigated, N/P/K, harvest_date; unknown per-plant variables; rep and seed_treatment also sit on the 30/60 DAS long table.

## NOT PROCESSED
# None of the four xlsx files were skipped. Metadata docx files are the unit notes used above.
# Soil column Year is the season key, already used as planting_date.

## NEW VARIABLES
# pod_weight_g_plant_: pod weight (g/plant). Harvest table, and the 60 DAS long rows
# seed_weight_g_plant_: seed weight at harvest (g/plant), not the 1000-seed weight
# seed_number_plant_: seeds per plant at harvest
# nodule_weight_mg_plant_: nodule weight at 30 DAS (mg/plant)
# root_weight_g_plant_: root weight at 30 DAS, docx unit g/plant (see ISSUES)
# shoot_weight_g_plant_: shoot weight at 30 DAS (g/plant)
# pod_number_plant_: pods per plant at 60 DAS
# leaf_area_cm2_: leaf area at 60 DAS (cm2), with 2013/14 columns exchanged
# leaf_weight_g_: leaf weight at 60 DAS (g/plant), with 2013/14 columns exchanged
# stem_weight_g_plant_: stem weight at 60 DAS (g/plant)
# dehydrogenase_: dehydrogenase activity at harvest (TPF g-1 soil 24 h-1)


carob_script <- function(path) {

"
Data on evaluation of broad-spectrum Streptomyces sp. for plant growth promotion traits in Chickpea (Cicer arietinum L.)

The experiment was done for two consecutive post-rainy cropping seasons in 2012−13 and 2013–14 at the International Crops Research Institute for the Semi-arid Tropics (ICRISAT). The test Streptomyces (CAI-21, CAI-26 and MMA-32) were grown individually on a starch casein broth at 28 °C for 4–5 days. Seeds of Chickpea (variety ICCV2) were treated individually with a Streptomyces strain (108 CFU per ml) for 45 min and sown by hand planting. Samplings were done at 30 days after sowing (DAS), 60 DAS and at crop maturity. On the day of sampling, plant aerial and underground parts were harvested separately with care to eliminate border effects in each plot, and dried to constant weight in dryers equipped with high air draught at 45 °C for 72 h; total dry weights were recorded. At 30 DAS, nodule number, nodule weight, root weight and shoot weight and at 60 DAS, plant height, pod number, pod weight, leaf area, leaf weight and stem weight were recorded in both seasons (2012–2013 and 2013–2014). At crop maturity, stover yield, grain yield, total dry matter (TDM), 1000 seed weight, pod weight, seed number and seed weight were recorded in both seasons.        Experiment location on Google Map
"

	uri <- "doi:10.21421/D2/ZNKSUT"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "seed_treatment",
		response_vars = "yield;dmy_residue;dmy_total;seed_weight",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 65,
		carob_effort = 0.8
	)

	season <- function(x) {
		# "2012/13" is the post-rainy season that starts in 2012
		substr(x, 1, 4)
	}

	f <- ff[basename(ff) == "Effect of the three Streptomyces sp Chickpea.xlsx"]
	r <- carobiner::read.excel(f, na="NA")
	trt <- trimws(r$Treatment)

	d <- data.frame(
		country = "India",
		adm1 = "Telangana",
		location = "Patancheru",
		site = "ICRISAT field BP-13B",
		crop = "chickpea",
		variety = "ICCV2",
		treatment = trt,
		seed_treatment = trt,
		inoculant = ifelse(trt == "Control", NA_character_, trt),
		inoculated = trt != "Control",
		rep = as.integer(r$`Replication number`),
		planting_date = season(r$Year),
		pod_weight_g_plant_ = as.numeric(r$`Pod weight`),
		seed_weight_g_plant_ = as.numeric(r$`Seed weight`),
		seed_number_plant_ = as.numeric(r$`Seed no`),
		seed_weight = as.numeric(r$`Thousand seed weight`),
		yield = as.numeric(r$`Grain yield`) * 1000,
		dmy_residue = as.numeric(r$`Stover yield`) * 1000,
		dmy_total = as.numeric(r$`Dry matter`) * 1000,
		yield_part = "seed",
		yield_isfresh = FALSE,
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d$trial_id <- paste0("Patancheru_", d$planting_date)

	fs <- ff[basename(ff) == "Effect of the three sp on rhizosphere.xlsx"]
	rs <- carobiner::read.excel(fs, na="NA")
	s <- data.frame(
		seed_treatment = trimws(rs$Treatment),
		rep = as.integer(rs$`Replication number`),
		planting_date = season(rs$Year),
		soil_SOC = as.numeric(rs$OC),
		soil_N_total = as.numeric(rs$Nitrogen),
		soil_P = as.numeric(rs$Phosphorus),
		soil_MBC = as.numeric(rs$`Microbial carbon`),
		dehydrogenase_ = as.numeric(rs$Dehydrogenase)
	)
	d <- merge(d, s, by=c("seed_treatment", "rep", "planting_date"), all.x=TRUE)

	f30 <- ff[basename(ff) == "Effect of the three Streptomyces sp 30das.xlsx"]
	r30 <- carobiner::read.excel(f30, na="NA")
	t30 <- trimws(r30$Treatment)
	d30 <- data.frame(
		trial_id = paste0("Patancheru_", season(r30$Year)),
		seed_treatment = t30,
		rep = as.integer(r30$`Replication number`),
		DAP = 30L,
		nodule_plant = as.numeric(r30$`Nodules number`),
		nodule_weight_mg_plant_ = as.numeric(r30$`Nodule weight`),
		root_weight_g_plant_ = as.numeric(r30$`Root weight`),
		shoot_weight_g_plant_ = as.numeric(r30$`Shoot weight`)
	)

	f60 <- ff[basename(ff) == "Effect of the three Streptomyces sp 60das.xlsx"]
	r60 <- carobiner::read.excel(f60, na="NA")
	t60 <- trimws(r60$Treatment)
	# 2013/14: Leaf area holds the gram weights and Leaf weight holds the cm2 areas
	swap <- r60$Year == "2013/14"
	area <- as.numeric(r60$`Leaf area`)
	wt <- as.numeric(r60$`Leaf weight`)
	area[swap] <- as.numeric(r60$`Leaf weight`)[swap]
	wt[swap] <- as.numeric(r60$`Leaf area`)[swap]
	d60 <- data.frame(
		trial_id = paste0("Patancheru_", season(r60$Year)),
		seed_treatment = t60,
		rep = as.integer(r60$`Replication number`),
		DAP = 60L,
		plant_height = as.numeric(r60$`Plant height`),
		pod_number_plant_ = as.numeric(r60$`Pod number`),
		pod_weight_g_plant_ = as.numeric(r60$`Pod weight`),
		leaf_area_cm2_ = area,
		leaf_weight_g_ = wt,
		stem_weight_g_plant_ = as.numeric(r60$`Stem weight`)
	)

	long <- carobiner::bindr(d30, d60)

	carobiner::write_files(path, meta, d, long=long)
}
