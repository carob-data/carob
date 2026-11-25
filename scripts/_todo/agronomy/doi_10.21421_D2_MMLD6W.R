# R script for "carob"
# license: GPL (>=3)

## NOTES
# Two irrigated rabi seasons (2012-13 and 2013-14) of chickpea ICCV 2 at ICRISAT Patancheru, India. Six Streptomyces strains (CAI-13, CAI-85, CAI-93, CAI-140, CAI-155, KAI-180) and an uninoculated control, RCBD, three replicates.
# Sowing was October 2012 and November 2013 (dataset description).
# Grain, stover, and total dry matter are in t/ha (x1000 -> kg/ha) on a dry basis (Dry matter = Grain + Stover -> yield_isfresh = FALSE, yield_moisture = 0).
# Harvest rhizosphere soil (0-15 cm, two replicates) is merged onto d: soil_SOC (%), soil_N_total and soil_P (ppm = mg/kg), soil_MBC (ug/g), dehydrogenase (ug TPF/g/24 h).
# Repeated in-season plant measurements at 30 DAS and 60 DAS are stored in the long table linked via record_id (nodule/root weights at 30 DAS are mg/plant; shoot/stem/leaf weights are g/plant; leaf area is cm2).
# Estimated ICRISAT Patancheru coordinates (17.511, 78.275) added with geo_from_source = FALSE and geo_uncertainty = 2000 m.

## ISSUES
# Fertilizer product/rate and harvest date are not reported. 30 DAS metadata docx has a typo "2031-14" for 2013-14.
# Soil biology and chemistry were recorded for two replicates only (rep 3 is NA).
# Plant density is not reported, so per-plant weights remain per plant. Available-P method is not stated.
# write_files: missing carob_contributor, K/N/P_fertilizer, harvest_date; unknown per-plant variables and dehydrogenase_.

## NOT PROCESSED
# 30 DAS and 60 DAS columns that are structurally empty at that sampling date.

## NEW VARIABLES
# pod_weight_harvest_g_plant_: pod weight at harvest (g/plant).
# seed_number_plant_: seeds per plant at harvest.
# seed_mass_g_plant_: seed weight per plant at harvest (g/plant; distinct from 1000-seed weight).
# nodule_dry_mg_: nodule dry weight at 30 DAS (mg/plant).
# root_dry_mg_: root dry weight at 30 DAS (mg/plant).
# shoot_weight_g_plant_: shoot dry weight at 30 DAS (g/plant).
# stem_weight_g_plant_: stem weight at 60 DAS (g/plant).
# leaf_area_cm2_: leaf area at 60 DAS (cm2/plant).
# leaf_weight_g_: leaf weight at 60 DAS (g/plant).
# pod_number_plant_: pods per plant at 60 DAS.
# dehydrogenase_: soil dehydrogenase activity at harvest (ug TPF/g soil/24 h).


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
		response_vars = "yield;dmy_residue;dmy_total;plant_height;nodule_plant",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 90,
		carob_effort = 0.2
	)

	f1 <- ff[basename(ff) == "Effect of Plant growth-promoting Streptomyces sp 30 DAS, 60 DAS.xlsx"]
	f2 <- ff[basename(ff) == "Effect of the six Streptomyces sp microbial total N, P OC.xlsx"]
	f3 <- ff[basename(ff) == "Effect of the six Streptomyces sp. on agronomic.xlsx"]

	r1 <- carobiner::read.excel(f1, na="NA")
	r2 <- carobiner::read.excel(f2, na="NA")
	r3 <- carobiner::read.excel(f3, na="NA")

	trt <- trimws(as.character(r3$Treatment))
	yr <- trimws(as.character(r3$Year))
	d <- data.frame(
		record_id = seq_len(nrow(r3)),
		treatment = trt,
		rep = as.integer(r3[["Replication number"]]),
		year = yr,
		variety = "ICCV 2",
		yield = as.numeric(r3[["Grain yield"]]) * 1000,
		dmy_residue = as.numeric(r3[["Stover yield"]]) * 1000,
		dmy_total = as.numeric(r3[["Dry matter"]]) * 1000,
		seed_weight = as.numeric(r3[["Thousand seed weight"]]),
		pod_weight_harvest_g_plant_ = as.numeric(r3[["Pod weight"]]),
		seed_number_plant_ = as.numeric(r3[["Seed number"]]),
		seed_mass_g_plant_ = as.numeric(r3[["Seed weight"]]),
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
		latitude = 17.511,
		longitude = 78.275,
		geo_from_source = FALSE,
		geo_uncertainty = 2000,
		season = "rabi",
		irrigated = TRUE,
		on_farm = FALSE,
		is_survey = FALSE,
		yield_moisture = 0,
		yield_isfresh = FALSE,
		depth_top = 0,
		depth_bottom = 15
	)

	s <- data.frame(
		treatment = trimws(as.character(r2$Treatments)),
		rep = as.integer(r2[["Replication number"]]),
		year = trimws(as.character(r2$Year)),
		soil_MBC = as.numeric(r2[["Microbial biomass C"]]),
		dehydrogenase_ = as.numeric(r2$Dehydrogenase),
		soil_N_total = as.numeric(r2$Nitrogen),
		soil_P = as.numeric(r2$Phosphorus),
		soil_SOC = as.numeric(r2[["Organic carbon"]])
	)
	d <- merge(d, s, by=c("treatment", "rep", "year"), all.x=TRUE, sort=FALSE)
	d <- d[order(d$record_id), ]

	dkey <- paste(d$treatment, d$rep, d$year, sep="|")
	rid <- function(x) d$record_id[match(paste(trimws(as.character(x$Treatment)), as.integer(x[["Replication number"]]), trimws(as.character(x$Year)), sep="|"), dkey)]

	g30 <- r1[r1$DAS == 30, ]
	l30 <- data.frame(
		record_id = rid(g30),
		DAP = 30L,
		nodule_plant = as.numeric(g30[["Nodule number"]]),
		nodule_dry_mg_ = as.numeric(g30[["Nodule weight"]]),
		root_dry_mg_ = as.numeric(g30[["Root weight"]]),
		shoot_weight_g_plant_ = as.numeric(g30[["Shoot weight"]])
	)
	g60 <- r1[r1$DAS == 60, ]
	l60 <- data.frame(
		record_id = rid(g60),
		DAP = 60L,
		plant_height = as.numeric(g60[["Plant height"]]),
		stem_weight_g_plant_ = as.numeric(g60[["Stem weight"]]),
		leaf_area_cm2_ = as.numeric(g60[["Leaf area"]]),
		leaf_weight_g_ = as.numeric(g60[["Leaf weight"]]),
		pod_number_plant_ = as.numeric(g60[["Pod number"]])
	)
	dlong <- carobiner::bindr(l30, l60)
	d$year <- NULL
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
