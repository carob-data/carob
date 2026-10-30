# R script for "carob"
# license: GPL (>=3)

## NOTES
# Station chickpea trial of seven Streptomyces seed inoculants plus an uninoculated control, ICRISAT Patancheru, 2012 rabi (field BP-13).
# Field file "Effect of the seven actinomycetes on agronomic.xlsx": one row per plot. Grain, stover and dry matter are t/ha (x1000 = kg/ha). On every row grain + stover = dry matter, so yield is the dry component (yield_isfresh FALSE). Moisture percent was not measured.
# The same row also holds the 60 DAS plant scores named in the abstract (nodules, leaf area, leaf and shoot and root weight, pods). Those weights are g/plant. Plot plant density is not given, so they are not converted to kg/ha.
# Soil file: organic carbon (%), total N and available P (ppm = mg/kg) at 60 and 100 DAS, two replicates, top 15 cm. Kept in the long table because replicate count and dates do not match the harvest table.
# Glasshouse plant scores (45 and 90 days) are extra rows (trial_id glasshouse_2012). That trial has no grain yield.

## ISSUES
# No coordinates in the files or the Dataverse notes. Georeference ICRISAT, Patancheru, Telangana, India (field BP-13). Telangana is the current state; in 2012 the site was in Andhra Pradesh.
# Fertilizer rates, irrigation, variety, and a numeric harvest date are not in the files. Publication is cited on OAR (http://oar.icrisat.org/id/eprint/10151) but the Dataverse record has no DOI.
# Plot size is "4 m x 3 ridges", not an area in m2.
# Glasshouse root length is recorded in the thousands (docx: cm). That can only be a scanned total length, not a taproot. Root volume is labeled cm2.
# Glasshouse rows have no grain yield, so yield is NA on those rows.
# Seed number is missing on one field plot.
# seed_weight_g_plant_ is grams per plant. Do not rename it to seed_weight (that term is 1000-seed weight).
# write_files: missing carob_contributor, yield_moisture, latitude, longitude, irrigated, N/P/K, harvest_date; unknown per-plant variables; NA yield on glasshouse rows; DAP, rep and seed_treatment also sit on the soil long table.

## NOT PROCESSED
# Antagonistic potential of the seven actinomycetes.xlsx: Method, Rhizoctonia bataticola6, Rhizoctonia bataticola24, Rhizoctonia bataticola115, FOC, Botr. In vitro dual-culture scores (0-3), not a crop measurement.
# Effect of temperature of seven actinomycetes.xlsx: Temperature 1-4 (20-50 C), pH 5/7/9/11, Salinity 1-6 (0-10%), Bavistin, Thiram, Benlet, Captan, Radonil, Benomyl, Indole acetic acid, Gluc. In vitro growth and metabolite assays of the isolates.
# Metadata docx files: column definitions used above, not data.

## NEW VARIABLES
# leaf_area_cm2_: leaf area (cm2)
# leaf_weight_g_: leaf weight (g)
# shoot_weight_g_plant_: shoot weight (g/plant)
# root_weight_g_plant_: root weight (g/plant)
# pod_number_plant_: pods per plant
# pod_weight_g_plant_: pod weight (g/plant)
# seed_weight_g_plant_: seed weight (g/plant), not 1000-seed weight
# seed_number_plant_: seeds per plant
# root_length_cm_: glasshouse root length as recorded (cm)
# root_volume_cm2_: glasshouse root volume as labeled (cm2)


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
		response_vars = "yield;dmy_residue;dmy_total",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.7
	)

	f <- ff[basename(ff) == "Effect of the seven actinomycetes on agronomic.xlsx"]
	r <- carobiner::read.excel(f, na="NA")
	trt <- trimws(r$Treatment)

	d <- data.frame(
		trial_id = "Patancheru_2012",
		country = "India",
		adm1 = "Telangana",
		location = "Patancheru",
		site = "ICRISAT field BP-13",
		crop = "chickpea",
		treatment = trt,
		seed_treatment = trt,
		inoculant = ifelse(trt == "Control", NA_character_, trt),
		inoculated = trt != "Control",
		rep = as.integer(r$`Replication number`),
		planting_date = "2012",
		nodule_plant = as.numeric(r$`Nodule no plant`),
		leaf_area_cm2_ = as.numeric(r$`Leaf area`),
		leaf_weight_g_ = as.numeric(r$`Leaf weight`),
		pod_number_plant_ = as.numeric(r$`Pod number`),
		pod_weight_g_plant_ = as.numeric(r$`Pod weight`),
		shoot_weight_g_plant_ = as.numeric(r$`Shoot weight`),
		root_weight_g_plant_ = as.numeric(r$`Root weight`),
		seed_weight_g_plant_ = as.numeric(r$`Seed weight`),
		seed_number_plant_ = as.numeric(r$`Seed number`),
		yield = as.numeric(r$`Grain weight`) * 1000,
		dmy_residue = as.numeric(r$`Stover weight`) * 1000,
		dmy_total = as.numeric(r$`Dry matter`) * 1000,
		yield_part = "seed",
		yield_isfresh = FALSE,
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	fg <- ff[basename(ff) == "Biological control and plant growth-promotion traits of glasshouse.xlsx"]
	rg <- carobiner::read.excel(fg, na="NA")
	tg <- trimws(rg$Treatment)
	dg <- data.frame(
		trial_id = "glasshouse_2012",
		country = "India",
		adm1 = "Telangana",
		location = "Patancheru",
		site = "ICRISAT glasshouse",
		crop = "chickpea",
		treatment = tg,
		seed_treatment = tg,
		inoculant = ifelse(tg == "Control", NA_character_, tg),
		inoculated = tg != "Control",
		rep = as.integer(rg$`Replication number`),
		planting_date = "2012",
		DAP = as.integer(sub(" days", "", rg$Time)),
		root_length_cm_ = as.numeric(rg$`Root length`),
		root_volume_cm2_ = as.numeric(rg$`Root volume`),
		leaf_area_cm2_ = as.numeric(rg$`Leaf area`),
		leaf_weight_g_ = as.numeric(rg$`Leaf weight`),
		root_weight_g_plant_ = as.numeric(rg$`Root weight`),
		shoot_weight_g_plant_ = as.numeric(rg$`Shoot weight`),
		nodule_plant = as.numeric(rg$`Nodule no plant`),
		pod_number_plant_ = as.numeric(rg$`Pod number`),
		pod_weight_g_plant_ = as.numeric(rg$`Pod weight`),
		yield_part = "seed",
		yield_isfresh = FALSE,
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d <- carobiner::bindr(d, dg)

	fs <- ff[basename(ff) == "Effect of the seven actinomycetes on the rhizosphere.xlsx"]
	rs <- carobiner::read.excel(fs, na="NA")
	dsoil <- data.frame(
		trial_id = "Patancheru_2012",
		seed_treatment = trimws(rs$Treatment),
		rep = as.integer(rs$`Replication number`),
		DAP = as.integer(rs$Days),
		depth_top = 0,
		depth_bottom = 15,
		soil_SOC = as.numeric(rs$`Organic Carbon`),
		soil_N_total = as.numeric(rs$Nitrogen),
		soil_P = as.numeric(rs$Phosphorus)
	)

	carobiner::write_files(path, meta, d, long=dsoil)
}
