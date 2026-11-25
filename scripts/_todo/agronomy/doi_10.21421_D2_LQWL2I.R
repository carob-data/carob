# R script for "carob"
# license: GPL (>=3)

## NOTES
# Station chickpea trial of seven Streptomyces seed inoculants (BCA-546, BCA-659, BCA-667, BCA-689, BCA-698, CAI-133, CAI-8) plus an uninoculated control at ICRISAT Patancheru (field BP-13 and glasshouse), 2012 rabi season, RCBD with 3 replicates.
# Field agronomic file: grain, stover, and total dry matter are in t/ha (x1000 -> kg/ha). Grain + stover = dry matter on every row -> dry basis (yield_isfresh = FALSE, yield_moisture = 0). 60 DAS plant traits sit on the same field rows; plant density is not given, so g/plant weights stay per plant.
# Glasshouse repeated plant measurements (45 and 90 DAS) and field rhizosphere soil measurements (60 and 100 DAS, 0-15 cm, 2 replicates) are stored in the long table linked via record_id.
# Estimated ICRISAT Patancheru coordinates (17.511, 78.275) added with geo_from_source = FALSE and geo_uncertainty = 2000 m.

## ISSUES
# Fertilizer rates, irrigation, variety, and harvest date are not in the files. Publication is on OAR (http://oar.icrisat.org/id/eprint/10151) without a DOI.
# Plot size is "4 m x 3 ridges" (area in m2 not stated).
# Glasshouse root length is recorded in the thousands (docx: cm; total scanned length); root volume is labeled cm2.
# Glasshouse rows have no grain yield (yield is NA on those rows). One field plot is missing seed number.
# seed_weight_g_plant_ is grams per plant (distinct from terminag 1000-seed weight).
# write_files: missing carob_contributor, irrigated, K/N/P_fertilizer, harvest_date; NA yield on glasshouse plots; unknown per-plant variables.

## NOT PROCESSED
# Antagonistic potential of the seven actinomycetes.xlsx: in vitro dual-culture ratings.
# Effect of temperature of seven actinomycetes.xlsx: in vitro temperature/pH/salinity/fungicide/metabolite assays.
# Metadata docx files: unit notes used above.

## NEW VARIABLES
# leaf_area_cm2_: leaf area (cm2/plant).
# leaf_weight_g_: leaf weight (g/plant).
# shoot_weight_g_plant_: shoot weight (g/plant).
# root_weight_g_plant_: root weight (g/plant).
# pod_number_plant_: pods per plant.
# pod_weight_g_plant_: pod weight (g/plant).
# seed_weight_g_plant_: seed weight per plant (g/plant).
# seed_number_plant_: seeds per plant.
# root_length_cm_: scanned total root length (cm/plant).
# root_volume_cm2_: root volume as labeled in source (cm2/plant).


carob_script <- function(path) {

"
Data on biological control and plant growth promotion traits of Streptomyces species under greenhouse and field conditions in Chickpea

This dataset shows the seven most promising isolates of Streptomyces (BCA-546,BCA-659,BCA-667, BCA-689, BCA-698, CAI-133, and CAI-8) were evaluated for their physiological, plant growth-promoting traits and antagonistic activity under in vitro and in planta conditions and also performed their plant growth promoting activity under green house conditions. At 45 days after sowing, observations including the root length, root volume, leaf area, leaf weight, root weight, shoot weight and nodule weight were recorded, whereas at final harvest, the shoot weight, root weight, pod numbers and pod weight were recorded. The experiments were laid out in a randomized complete block design (RCBD) with three replicates and subplot sizes of 4 m × 3 ridges (rows) in 2012 Rabi season, nodule number, leaf area, leaf weight, pod number, shoot weight and root weight were recorded at 60 days. At final harvest, parameters including seed weight, seed number, stover yield, grain yield and total dry matter were recorded. soil samples were collected from the top 15 cm of the soil profile at the harvest stage and analysed for total nitrogen, available phosphorous and organic carbon %. Data sets were analysed and published. Publication details can be found below in publication section        Experiment location on Google Map
"

	uri <- "doi:10.21421/D2/LQWL2I"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "seed_treatment",
		response_vars = "yield;dmy_residue;dmy_total;nodule_plant",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 85,
		carob_effort = 0.25
	)

	f <- ff[basename(ff) == "Effect of the seven actinomycetes on agronomic.xlsx"]
	r <- carobiner::read.excel(f, na="NA")
	trt <- trimws(as.character(r$Treatment))

	dfield <- data.frame(
		trial_id = "Patancheru_2012",
		country = "India",
		adm1 = "Telangana",
		adm2 = "Sangareddy",
		location = "Patancheru",
		site = "ICRISAT field BP-13",
		latitude = 17.511,
		longitude = 78.275,
		geo_from_source = FALSE,
		geo_uncertainty = 2000,
		crop = "chickpea",
		treatment = trt,
		seed_treatment = trt,
		inoculant = ifelse(trt == "Control", NA_character_, trt),
		inoculated = trt != "Control",
		rep = as.integer(r[["Replication number"]]),
		planting_date = "2012",
		season = "rabi",
		seed_weight_g_plant_ = as.numeric(r[["Seed weight"]]),
		seed_number_plant_ = as.numeric(r[["Seed number"]]),
		yield = as.numeric(r[["Grain weight"]]) * 1000,
		dmy_residue = as.numeric(r[["Stover weight"]]) * 1000,
		dmy_total = as.numeric(r[["Dry matter"]]) * 1000,
		yield_part = "seed",
		yield_isfresh = FALSE,
		yield_moisture = 0,
		on_farm = FALSE,
		is_survey = FALSE
	)

	f1 <- ff[basename(ff) == "Biological control and plant growth-promotion traits of glasshouse.xlsx"]
	r1 <- carobiner::read.excel(f1, na="NA")
	tg <- trimws(as.character(r1$Treatment))
	r1_rep <- as.integer(r1[["Replication number"]])
	gh_plots <- unique(data.frame(treatment = tg, rep = r1_rep))
	dgh <- data.frame(
		trial_id = "glasshouse_2012",
		country = "India",
		adm1 = "Telangana",
		adm2 = "Sangareddy",
		location = "Patancheru",
		site = "ICRISAT glasshouse",
		latitude = 17.511,
		longitude = 78.275,
		geo_from_source = FALSE,
		geo_uncertainty = 2000,
		crop = "chickpea",
		treatment = gh_plots$treatment,
		seed_treatment = gh_plots$treatment,
		inoculant = ifelse(gh_plots$treatment == "Control", NA_character_, gh_plots$treatment),
		inoculated = gh_plots$treatment != "Control",
		rep = gh_plots$rep,
		planting_date = "2012",
		yield_part = "seed",
		yield_isfresh = FALSE,
		on_farm = FALSE,
		is_survey = FALSE
	)

	d <- carobiner::bindr(dfield, dgh)
	d$record_id <- seq_len(nrow(d))
	dkey <- paste(d$trial_id, d$seed_treatment, d$rep, sep="|")

	# Field 60 DAS plant observations in long
	lf60 <- data.frame(
		record_id = d$record_id[match(paste("Patancheru_2012", trt, as.integer(r[["Replication number"]]), sep="|"), dkey)],
		DAP = 60L,
		nodule_plant = as.numeric(r[["Nodule no plant"]]),
		leaf_area_cm2_ = as.numeric(r[["Leaf area"]]),
		leaf_weight_g_ = as.numeric(r[["Leaf weight"]]),
		pod_number_plant_ = as.numeric(r[["Pod number"]]),
		pod_weight_g_plant_ = as.numeric(r[["Pod weight"]]),
		shoot_weight_g_plant_ = as.numeric(r[["Shoot weight"]]),
		root_weight_g_plant_ = as.numeric(r[["Root weight"]])
	)
	# Glasshouse 45 and 90 DAS plant observations in long
	lgh <- data.frame(
		record_id = d$record_id[match(paste("glasshouse_2012", tg, r1_rep, sep="|"), dkey)],
		DAP = as.integer(sub(" days", "", as.character(r1$Time))),
		root_length_cm_ = as.numeric(r1[["Root length"]]),
		root_volume_cm2_ = as.numeric(r1[["Root volume"]]),
		leaf_area_cm2_ = as.numeric(r1[["Leaf area"]]),
		leaf_weight_g_ = as.numeric(r1[["Leaf weight"]]),
		root_weight_g_plant_ = as.numeric(r1[["Root weight"]]),
		shoot_weight_g_plant_ = as.numeric(r1[["Shoot weight"]]),
		nodule_plant = as.numeric(r1[["Nodule no plant"]]),
		pod_number_plant_ = as.numeric(r1[["Pod number"]]),
		pod_weight_g_plant_ = as.numeric(r1[["Pod weight"]])
	)
	# Field rhizosphere soil (60 and 100 DAS, 0-15 cm) in long
	f2 <- ff[basename(ff) == "Effect of the seven actinomycetes on the rhizosphere.xlsx"]
	r2 <- carobiner::read.excel(f2, na="NA")
	lsoil <- data.frame(
		record_id = d$record_id[match(paste("Patancheru_2012", trimws(as.character(r2$Treatment)), as.integer(r2[["Replication number"]]), sep="|"), dkey)],
		DAP = as.integer(r2$Days),
		depth_top = 0,
		depth_bottom = 15,
		soil_SOC = as.numeric(r2[["Organic Carbon"]]),
		soil_N_total = as.numeric(r2$Nitrogen),
		soil_P = as.numeric(r2$Phosphorus)
	)

	dlong <- carobiner::bindr(lf60, lgh, lsoil)
	rownames(d) <- NULL
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
