# R script for "carob"
# license: GPL (>=3)

## NOTES
# Paper not linked in the metadata; found by search (doi:10.22161/ijeab/2.5.53). Its means match the data.
# Site, sowing dates, management, soil and plot size are from the paper's Methods.
# Split plot: mulch (main) x variety (sub), 4 reps, two dry seasons sown on 24 Dec 2014 and 2015.
# M = white polythene mulch on broad beds; WM = bare broad beds.
# Codebook gives "nmol/mg" for SCMR and LAI; both are unitless (codebook error).

## ISSUES
# Shelling ratio (9-51%) does not match the paper (67-72%); kept as shelling_ratio_. Ask the authors?
# dw_biomass: not stated if total of 5 plants or per plant, so not converted to kg/ha.
# seed_density only reported for mulched plots; NA for bare beds.
# soil_SOC 0.219 has no unit; assumed %.
# yield_moisture not reported (air-dried pods); left out, yield_isfresh = FALSE

## NEW VARIABLES
# first_flowering_days_: days from sowing to first flower
# shelling_ratio_: "Shelling ratio" as given in the data (%), meaning unclear

## NOT PROCESSED
# herbicide amount: only product rate given (3 L/ha), no active ingredient
# weeding: "where necessary", frequency unknown
# rainfall and temperature: annual totals and a figure only, not for the crop season


carob_script <- function(path) {

"
Response of irrigated Groundnut to polythene mulching on broad-bed and furrows during the temperature months in Nigeria

The objective of this work conceived an idea of using Polythene mulch 
on broad bed furrows to improve groundnut production and productivity during the low temperature months 
in Sudan savanna of Nigeria. The experiments were conducted in 2 seasons (2014 and 2015 dry seasons). 
The treatments used in the experiment consisted of Polythene mulch (Mulch and Without mulch) and 
Groundnut varieties ( Samnut 23, Samnut 24, Samnut 26 and Ex-Dakar) laid out in Split plot design with 4 replications. 
Experimental location on Google Maps
"

	uri <- "doi:10.21421/D2/3FUSXO"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = "doi:10.22161/ijeab/2.5.53",                  ## paper found by web search;
		project = "CGIAR Research Program on Grain Legumes",        ## paper acknowledgement
		design = "split plot",                                      ## from paper
		data_type = "experiment",
		treatment_vars = "mulch_type;variety",
		response_vars = "yield;dmy_residue;seed_weight;flowering_days;maturity_days;germination_rate;SPAD;LAI;dw_biomass",
		notes = NA, 
		carob_contributor = "Stella Muthoni",
		carob_LLM = "Claude Opus 5.5 (merged from 4 drafts)",
		carob_date = "2026-11-15",
		carob_completion = 95,
		carob_effort = 2
	)

	f <- ff[basename(ff) == "Data file of Irrigated groundnut to polythene mulching on broad-bed and furrows .xlsx"]

	r1 <- carobiner::read.excel(f, sheet="Sheet1")
	r2 <- carobiner::read.excel(f, sheet="Sheet2")
	
	# Sheet2: dry matter at 105 DAS, 120 DAS and harvest for the same 64 plots
	r <- merge(r1, r2, by=c("Plot no", "Year", "Replication number")) 

	d <- data.frame(
		trial_id = as.character(r$Year),
		plot_id = paste0(r$Year, "_", r[["Plot no"]]),
		rep = as.integer(r[["Replication number"]]),
		mulch_type = ifelse(r$Mulch == "M", "plastic", "none"),
		variety = gsub("Ex-dakar", "Ex-Dakar", r$Variety),
		treatment = paste0(r$Mulch, "_", r$Variety),
		planting_date = paste0(r$Year, "-12-24"),               # paper: sown on 24 December in both seasons
		first_flowering_days_ = r$DFF,
		flowering_days = r$DF,                                  # DF: days to 50% flowering
		maturity_days = r$DM,
		yield = r[["Pod Yield"]],
		dmy_residue = r[["Haulm Yield"]],                       # haulm weighed after 1 week of air-drying
		seed_weight = r$HSW * 10,                               # HSW is 100-seed weight; seed_weight is 1000-seed weight
		shelling_ratio_ = r[["Shelling ratio"]]                 # does not match the paper's shelling percentage (72% / 67%)
	)

	# paper: harvested when at least 80% of plants reached physiological maturity (days to maturity)
	d$harvest_date <- as.character(as.Date(d$planting_date) + d$maturity_days)

	d$crop <- "groundnut"
	d$yield_part <- "pod"
	d$yield_isfresh <- FALSE
	d$season <- "dry"

	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	# paper: irrigated twice a week for the first two weeks, weekly thereafter
	d$irrigation_scheduling <- "calendar"
	d$irrigation_interval <- 7L

	# paper: ICRISAT experimental station in Wasai, Minjibir LGA, Kano State; 12.14 N, 8.67 E, 441 m
	d$country <- "Nigeria"
	d$adm1 <- "Kano"
	d$adm2 <- "Minjibir"
	d$location <- "Wasai"
	d$latitude <- 12.14
	d$longitude <- 8.67
	d$elevation <- 441
	d$geo_from_source <- TRUE
	d$geo_uncertainty <- 1000           # coordinates given with 2 decimals (~1 km)

	# paper: loamy sand, pH 6.54, organic carbon 0.219 (unit not given; assumed %)
	d$soil_texture <- "loamy sand"
	d$soil_pH <- 6.54
	d$soil_SOC <- 0.219

	# paper: ploughed twice and planked; broad bed and furrow coded as "raised beds"
	d$land_prep_method <- "double ploughing;raised beds"
	
	# paper: 4 beds of 4 m x 1 m alternating with 0.25 m furrows (20 m2)
	d$plot_area <- 20
	d$plot_length <- 4
	d$plot_width <- 5
	
	# paper: dibbling, 2 seeds per hole, 5 cm deep
	d$planting_method <- "dibbling"
	d$planting_depth <- 5
	
	# paper (Discussion): 250,000 hills/ha for the mulched plots (holes 20 x 20 cm), 2 seeds per hill
	# spacing on the bare beds is not reported
	d$seed_density <- ifelse(d$mulch_type == "plastic", 250000 * 2, NA)
	
	# paper: seed treated with Apron Star at 10 g per 4 kg seed
	d$seed_treatment <- "Apron Star"
	
	# paper: pre-emergence pendimethalin, 3 L/ha (product)
	d$herbicide_used <- TRUE
	d$herbicide_product <- "pendimethalin"

	# paper: basal SSP 100 kg/ha and gypsum 400 kg/ha is the only fertilizer described, so N and K are 0
	# nutrient fractions from terminag values_fertilizer_type: SSP 8.74% P, 12% S; gypsum 19% S, 22% Ca
	d$fertilizer_type <- "SSP;gypsum"
	d$fertilizer_amount <- 100 + 400
	d$gypsum <- 400
	d$N_fertilizer <- 0
	d$P_fertilizer <- 100 * 0.0874
	d$K_fertilizer <- 0
	d$S_fertilizer <- 100 * 0.12 + 400 * 0.19
	d$Ca_fertilizer <- 400 * 0.22

	## long table: observations at days after sowing (DAP)
	n <- nrow(r)
	d1 <- data.frame(
		plot_id = rep(d$plot_id, 3),
		DAP = rep(c(8L, 15L, 30L), each=n),
		germination_rate = c(r[["Germination_8 DAS"]], r[["Germination_15 DAS"]], r[["Germination_30 DAS"]])
	)
	
	# paper: SCMR on the 3rd fully expanded tetrafoliate leaf of 5 plants; LAI with AccuPAR LP-80 ceptometer at noon
	d2 <- data.frame(
		plot_id = rep(d$plot_id, 3),
		DAP = rep(c(40L, 60L, 80L), each=n),
		SPAD = c(r[["SCMR_40 DAS"]], r[["SCMR_60 DAS"]], r[["SCMR_80 DAS"]]),
		LAI = c(r[["LAI_40 DAS"]], r[["LAI_60 DAS"]], r[["LAI_80 DAS"]])
	)
	# paper: 5 plants per plot, oven-dried 48 h at 70 C; per plant or per 5 plants not stated
	# DMW_Harvest has no DAS; DAP set to days to maturity (harvest at physiological maturity)
	d3 <- data.frame(
		plot_id = rep(d$plot_id, 8),
		DAP = c(rep(c(30L, 45L, 60L, 75L, 90L, 105L, 120L), each=n), as.integer(r$DM)),
		growth_stage = rep(c(rep(NA, 7), "harvest"), each=n),
		dw_biomass = c(r[["DMW_30 DAS"]], r[["DMW_45 DAS"]], r[["DMW_60 DAS"]], r[["DMW_75 DAS"]],
			r[["DMW_90 DAS"]], r[["DMW_105 DAS"]], r[["DMW_120 DAS"]], r$DMW_Harvest)
	)
	dlong <- merge(d1, d2, by=c("plot_id", "DAP"), all=TRUE)
	dlong <- merge(dlong, d3, by=c("plot_id", "DAP"), all=TRUE)

	carobiner::write_files(path, meta, d, long=dlong)
}
