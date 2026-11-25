# R script for "carob"
# license: GPL (>=3)

## NOTES
# Station chickpea (ICCV2) seed-inoculation trial of Streptomyces CAI-21, CAI-26, and MMA-32 plus an uninoculated control on ICRISAT field BP-13B, Patancheru, post-rainy seasons 2012/13 and 2013/14.
# Harvest file: stover, grain, and total dry matter are in t/ha (x1000 -> kg/ha) on a dry basis (Grain + Stover = Dry matter -> yield_isfresh = FALSE, yield_moisture = 0). Thousand-seed weight is mapped to seed_weight (g).
# Rhizosphere soil at harvest (0-15 cm, two replicates) is merged onto d: soil_SOC (%), soil_N_total and soil_P (ppm = mg/kg), soil_MBC (ug/g), dehydrogenase (ug TPF g-1 24 h-1).
# 30 DAS and 60 DAS plant observations are stored in the long table linked via record_id. In 2013/14, Leaf area and Leaf weight columns are transposed relative to 2012/13 and stated units (cm2 vs g/plant) and are exchanged for that year.
# Estimated ICRISAT Patancheru coordinates (17.511, 78.275) added with geo_from_source = FALSE and geo_uncertainty = 2000 m.

## ISSUES
# Dataverse citation (Philippine Agricultural Scientist 2015, http://oar.icrisat.org/9056/) has no DOI.
# Fertilizer rates, irrigation, experimental design, and harvest date are not in the files. Plot size is "4 m x 3 ridges" (area in m2 not stated).
# 30 DAS root weight values (160-213) are likely mg/plant despite the docx stating g/plant (shoot weight is 1.3-2.9 g/plant); kept as recorded under root_weight_30_das_.
# One 1000-seed weight and one harvest pod weight are NA.
# write_files: missing carob_contributor, irrigated, K/N/P_fertilizer, harvest_date; unknown per-plant variables and dehydrogenase_.

## NOT PROCESSED
# Metadata docx files: documentation used to verify units.

## NEW VARIABLES
# pod_weight_harvest_g_plant_: pod weight at harvest (g/plant).
# seed_weight_g_plant_: seed weight per plant at harvest (g/plant; distinct from 1000-seed weight).
# seed_number_plant_: seeds per plant at harvest.
# nodule_weight_mg_plant_: nodule dry weight at 30 DAS (mg/plant).
# root_weight_30_das_: root weight at 30 DAS as recorded (docx states g/plant, values 160-213 likely mg/plant).
# shoot_weight_g_plant_: shoot weight at 30 DAS (g/plant).
# pod_number_plant_: pods per plant at 60 DAS.
# pod_weight_g_plant_: pod weight at 60 DAS (g/plant).
# leaf_area_cm2_: leaf area at 60 DAS (cm2/plant).
# leaf_weight_g_: leaf weight at 60 DAS (g/plant).
# stem_weight_g_plant_: stem weight at 60 DAS (g/plant).
# dehydrogenase_: soil dehydrogenase activity at harvest (ug TPF g-1 soil 24 h-1).


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
		response_vars = "yield;dmy_residue;dmy_total;seed_weight;plant_height;nodule_plant",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 90,
		carob_effort = 0.2
	)

	season_yr <- function(x) substr(trimws(as.character(x)), 1, 4)

	f <- ff[basename(ff) == "Effect of the three Streptomyces sp Chickpea.xlsx"]
	r <- carobiner::read.excel(f, na="NA")
	trt <- trimws(as.character(r$Treatment))

	d <- data.frame(
		record_id = seq_len(nrow(r)),
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
		variety = "ICCV2",
		season = "rabi",
		treatment = trt,
		seed_treatment = trt,
		inoculant = ifelse(trt == "Control", NA_character_, trt),
		inoculated = trt != "Control",
		rep = as.integer(r[["Replication number"]]),
		planting_date = season_yr(r$Year),
		pod_weight_harvest_g_plant_ = as.numeric(r[["Pod weight"]]),
		seed_weight_g_plant_ = as.numeric(r[["Seed weight"]]),
		seed_number_plant_ = as.numeric(r[["Seed no"]]),
		seed_weight = as.numeric(r[["Thousand seed weight"]]),
		yield = as.numeric(r[["Grain yield"]]) * 1000,
		dmy_residue = as.numeric(r[["Stover yield"]]) * 1000,
		dmy_total = as.numeric(r[["Dry matter"]]) * 1000,
		yield_part = "seed",
		yield_isfresh = FALSE,
		yield_moisture = 0,
		depth_top = 0,
		depth_bottom = 15,
		on_farm = FALSE,
		is_survey = FALSE
	)
	d$trial_id <- paste0("Patancheru_", d$planting_date)

	f1 <- ff[basename(ff) == "Effect of the three sp on rhizosphere.xlsx"]
	r1 <- carobiner::read.excel(f1, na="NA")
	s <- data.frame(
		seed_treatment = trimws(as.character(r1$Treatment)),
		rep = as.integer(r1[["Replication number"]]),
		planting_date = season_yr(r1$Year),
		soil_SOC = as.numeric(r1$OC),
		soil_N_total = as.numeric(r1$Nitrogen),
		soil_P = as.numeric(r1$Phosphorus),
		soil_MBC = as.numeric(r1[["Microbial carbon"]]),
		dehydrogenase_ = as.numeric(r1$Dehydrogenase)
	)
	d <- merge(d, s, by=c("seed_treatment", "rep", "planting_date"), all.x=TRUE, sort=FALSE)
	d <- d[order(d$record_id), ]

	dkey <- paste(d$seed_treatment, d$rep, d$planting_date, sep="|")
	rid <- function(x) d$record_id[match(paste(trimws(as.character(x$Treatment)), as.integer(x[["Replication number"]]), season_yr(x$Year), sep="|"), dkey)]

	f30 <- ff[basename(ff) == "Effect of the three Streptomyces sp 30das.xlsx"]
	r30 <- carobiner::read.excel(f30, na="NA")
	d30 <- data.frame(
		record_id = rid(r30),
		DAP = 30L,
		nodule_plant = as.numeric(r30[["Nodules number"]]),
		nodule_weight_mg_plant_ = as.numeric(r30[["Nodule weight"]]),
		root_weight_30_das_ = as.numeric(r30[["Root weight"]]),
		shoot_weight_g_plant_ = as.numeric(r30[["Shoot weight"]])
	)

	f60 <- ff[basename(ff) == "Effect of the three Streptomyces sp 60das.xlsx"]
	r60 <- carobiner::read.excel(f60, na="NA")
	# 2013/14: Leaf area holds gram weights and Leaf weight holds cm2 areas
	swap <- trimws(as.character(r60$Year)) == "2013/14"
	area <- as.numeric(r60[["Leaf area"]])
	wt <- as.numeric(r60[["Leaf weight"]])
	area[swap] <- as.numeric(r60[["Leaf weight"]])[swap]
	wt[swap] <- as.numeric(r60[["Leaf area"]])[swap]
	d60 <- data.frame(
		record_id = rid(r60),
		DAP = 60L,
		plant_height = as.numeric(r60[["Plant height"]]),
		pod_number_plant_ = as.numeric(r60[["Pod number"]]),
		pod_weight_g_plant_ = as.numeric(r60[["Pod weight"]]),
		leaf_area_cm2_ = area,
		leaf_weight_g_ = wt,
		stem_weight_g_plant_ = as.numeric(r60[["Stem weight"]])
	)

	dlong <- carobiner::bindr(d30, d60)
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
