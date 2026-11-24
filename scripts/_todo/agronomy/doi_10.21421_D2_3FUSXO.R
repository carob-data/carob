# R script for "carob"
# license: GPL (>=3)

## NOTES
# Associated paper (not linked in the dataset metadata, found by web search): Ajeigbe, Motagi & Abdulsalam (2017)
#   IJEAB 2(5):2698-2705, doi:10.22161/ijeab/2.5.53. Its means (pod yield M 3401 vs WM 2102 kg/ha; haulm 4775 vs 3505)
#   match the data. Site, sowing date, management, soil, plot size and design are from its Materials and Methods.
# Station trial at ICRISAT Wasai, Minjibir LGA, Kano. Split plot: mulch = main plot, variety = sub-plot, 4 replications.
# "Year" in the data (2014, 2015) is the sowing year; both dry seasons were sown on 24 December (paper).
# Mulch "M" = 7 micron white polythene on the broad bed; "WM" = without mulch (bare broad bed and furrow).
# Plot numbers repeat across years, so plot_id combines Year and plot number.
# Sheet2 holds the dry matter weights at 105 DAS, 120 DAS and harvest for the same 64 plots.
# Germination, SPAD (SCMR), LAI and plant dry matter at several days after sowing are in the long table (plot_id, DAP).
# Codebook gives "nmol/mg" for SCMR and LAI; both are unitless indices (codebook error). It also describes SCMR_80DAS
#   as "at 60 days after sowing" (typo).

## ISSUES
# "Shelling ratio" in the data (8.8-51.3%, mean 30% mulch / 26% control) does not match the paper (72% / 67%),
#   its complement, or normal groundnut values. Kept as new variable shelling_ratio_, not shelling_percentage.
# dw_biomass (g): 5 plants per plot were sampled and oven-dried 48 h at 70 C (paper); not stated whether the value is
#   the total of the 5 plants or a per-plant mean, so it is not converted to kg/ha.
# DMW_Harvest has no DAS; its DAP is set to the plot's days to maturity (DM), with growth_stage "harvest".
# harvest_date = planting_date + DM: the paper says plots were harvested when >= 80% of plants reached physiological
#   maturity (days to maturity); no harvest dates are given.
# Pod and haulm yields are air-dried weights (paper), moisture unknown (yield_isfresh = FALSE). Haulm yield (air-dried
#   1 week) mapped to dmy_residue; check if acceptable as dry matter.
# seed_density (250,000 hills/ha x 2 seeds) is from the paper's Discussion ("population in the polythene mulch trial"),
#   based on the 20 x 20 cm hexagonal holes in the film; spacing on the bare (no mulch) beds is not stated, assumed equal.
# irrigation_interval = 7 is the regime after the first two weeks (twice a week before that).
# soil_SOC 0.219: paper gives no unit, assumed %.
# Dataverse bounding box (lat ~9.1-9.2, lon ~10.0-10.6) and "time period covered" (to 2015-12-25) do not match the
#   paper; the paper's coordinates and sowing dates are used.
# write_files: "missing variables: yield_moisture" (moisture of air-dried pods unknown; not added as NA);
#   unknown variables first_flowering_days_ and shelling_ratio_ (new variables, see below).
# RIS for the paper (references/doi_10.22161_ijeab_2.5.53.ris) exists in the B_plain working copy.

## NEW VARIABLES
# first_flowering_days_: days from sowing to first flower (DFF), days
# shelling_ratio_: "Shelling ratio" as given in the data (%); meaning unclear, see ISSUES

## NOT PROCESSED
# paper: 20 x 20 cm hexagonal hole pattern in the film; not stored as row_spacing/plant_spacing (no rows in a hexagonal
#   pattern, and unknown for the bare beds). Used only for seed_density.
# paper: pendimethalin 3 L/ha of product; active ingredient concentration not given, so herbicide_amount not set.
# paper: irrigation method not stated (furrow likely with BBF), irrigation_method not set.
# paper: "weeding where necessary"; unclear if and how often weeding was done, weeding_done not set.
# paper: annual rainfall (862.8 mm 2014, 564.8 mm 2015) is calendar-year rain, not for the irrigated dry-season crop;
#   monthly temperatures only in a figure (Fig. 1).


carob_script <- function(path) {

"
Response of irrigated Groundnut to polythene mulching on broad-bed and furrows during the temperature months in Nigeria

The objective of this work conceived an idea of using Polythene mulch on broad bed furrows to improve groundnut production and productivity during the low temperature months in Sudan savanna of Nigeria. The experiments were conducted in 2 seasons (2014 and 2015 dry seasons). The treatments used in the experiment consisted of Polythene mulch (Mulch and Without mulch) and Groundnut varieties ( Samnut 23, Samnut 24, Samnut 26 and Ex-Dakar) laid out in Split plot design with 4 replications.         Experimental location on Google Maps
"

	uri <- "doi:10.21421/D2/3FUSXO"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		# paper found by web search; not linked in the dataset metadata
		publication = "doi:10.22161/ijeab/2.5.53",
		# paper acknowledgement
		project = "CGIAR Research Program on Grain Legumes",
		# paper: mulch = main plot, variety = sub-plot, 4 replications
		design = "split plot",
		data_type = "experiment",
		treatment_vars = "mulch_type;variety",
		response_vars = "yield;dmy_residue;seed_weight;flowering_days;maturity_days;germination_rate;SPAD;LAI;dw_biomass",
		notes = "Pod and haulm yields are air-dried weights. Germination, SPAD, LAI and 5-plant sample dry weight at several days after sowing are in the long table. The 'Shelling ratio' column does not match the associated paper and is not standardized.",
		carob_contributor = NA,
		carob_LLM = "Claude Opus 5.5 (claude-opus-5-5)",
		carob_date = "2026-11-15",
		carob_completion = 95,
		carob_effort = 1
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
		# spelled "Ex-Dakar" in the abstract and paper
		variety = gsub("Ex-dakar", "Ex-Dakar", r$Variety),
		treatment = paste0(r$Mulch, "_", r$Variety),
		# paper: sown on 24 December in both seasons
		planting_date = paste0(r$Year, "-12-24"),
		first_flowering_days_ = r$DFF,
		# DF: days to 50% flowering
		flowering_days = r$DF,
		maturity_days = r$DM,
		# air-dried pods, kg/ha
		yield = r[["Pod Yield"]],
		# haulms air-dried for 1 week, kg/ha
		dmy_residue = r[["Haulm Yield"]],
		# HSW is 100-seed weight; seed_weight is 1000-seed weight
		seed_weight = r$HSW * 10,
		# does not match the paper's shelling percentage (72% / 67%)
		shelling_ratio_ = r[["Shelling ratio"]]
	)

	# paper: harvest when >= 80% of plants reached physiological maturity, assumed at maturity_days
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
	# coordinates given with 2 decimals (~1 km)
	d$geo_uncertainty <- 1000

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
	# paper (Discussion): 250,000 hills/ha, 2 seeds per hill; assumed also for the bare beds
	d$seed_density <- 250000 * 2
	# paper: seed treated with Apron Star at 10 g per 4 kg seed
	d$seed_treatment <- "Apron Star"
	# paper: pre-emergence pendimethalin, 3 L/ha (product)
	d$herbicide_used <- TRUE
	d$herbicide_product <- "pendimethalin"

	# paper: basal SSP 100 kg/ha and gypsum 400 kg/ha, no other fertilizer reported
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
