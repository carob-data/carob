# R script for "carob"
# license: GPL (>=3)

## NOTES
# Paper: Ramirez et al. 2021 (doi:10.1111/jac.12565), "Trial B"; NDVI drop under LSD (-4.0%) matches the data.
# 120 clones x 2 water regimes (OI, LSD), 2 reps each, resolvable row-column design; 480 plots, 1 trial.
# Data are UAV imagery summaries per plot at one date (190 DAP); no yield or vine weight in this dataset.
# Raw images (Imagenes_Moz2.rar, 303 MB) not downloaded; coordinates from Image_description_Moz2.xlsx.

## ISSUES
# Yield, vine and root weights are in the paper (BLUPs only) but not in the data; yield and yield_moisture are omitted.
# Genotype spelling variants harmonised (10 names); check the mapping in the code.
# After that, 121 names (paper: 120); some clones appear in >4 or <4 plots; kept as reported.
# Urea rate not reported: N_fertilizer is NA.
# 5 plots have negative chlorophyll_index (NIR < red-edge reflectance); kept as reported (within valid_min = -1).
# Plot area: dataset says 25 m2, paper gives 5.1 x 4.5 m (22.95 m2); paper value used.
# Variety types (checks, parents, landraces) are in paper Table S2 (not available); only the checks and landraces named in the description are set.
# Flight date: sheet name gives 3 Oct 2019, but 190 DAP from planting (28 Mar, day 0) is 4 Oct; date from the sheet name.

## NEW VARIABLES
# irrigation_stop_: days after planting from which irrigation was withheld (LSD only), days

## NOT PROCESSED
# Within-plot pixel standard deviations (stdev) of canopy_temperature, NDVI, NDVI_green, and chlorophyll_index (UAV raster diagnostics).
# Image_description_Moz2.xlsx: flight altitude, image resolution and size (image metadata).
# Paper: BLUPs per genotype of TRW, CRW, VW, dT, PCI, RCI (model outputs, not plot data).
# Paper: irrigation "twice a week" (dataset description); amounts are in Table S3 (not available).
# Paper Table S1: weather during the trial (not available).


carob_script <- function(path) {

"
Dataset for: Remote sensing and agronomic data for drought trials evaluated at Umbeluzi Research Station in 2019 (2nd experiment).

The trial had 120 clones,  the majority begin parents used in crossing blocks at Umbeluzi and Gurue Research Stations. The check clones such as Resisto and 199062.1 are parents in crossing blocks as well and some checks were landraces such as Xitsekele, Xiadla xakau. Trial life span started on 28 March 2019 and ended on 18 October 2019. The experiment was laid in two treatments defined as follows;   (i) Optimum Irrigation (OI, as control): No water restriction was imposed.  (ii) Late-season drought (LSD): Irrigation was withheld at 110 DAP and no irrigation was applied thereafter. Plot size was 25m2. The plots were arranged as 8 rows × 15 columns per treatment and replicated twice in each treatment. The plots were fertilized with urea, 46N:0P:0K at three weeks after planting and irrigated twice a week. All the field plots were kept weed-free by hoe weeding. A Cypermethrin (Avansagro, Shangai, China) insecticide was applied at 20 and 35 days after planting (DAP) in all trials with doses of 10mL 100mL-1 water.
"

	uri <- "doi:10.21223/2GJMNA"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = "doi:10.1111/jac.12565",
		project = NA,
		design = "resolvable row-column; 2 reps per water regime",
		data_type = "experiment",
		treatment_vars = "variety;stress",
		response_vars = "canopy_temperature;NDVI;NDVI_green;chlorophyll_index;ground_cover",
		notes = NA,
		carob_contributor = "Stella Muthoni",
		carob_LLM = "Claude Opus 5.5 (merged from 2 drafts)",
		carob_date = "2026-11-24",
		carob_completion = 90,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "Umbeluzi_2019_Moz2.xlsx"]
	r <- carobiner::read.excel.hdr(f, skip=2, hdr=1)   # sheet "031019": plot design and UAV indices (two header rows)

	# genotype spelling variants; each fix fills a treatment x rep where the name was missing
	vfix <- c(
		"19906.099999999999" = "199062.1",   # Excel number, missing in OI rep 2
		"Atacana" = "ATACANA",
		"CN 1448-49" = "CN1448-49",
		"MGSG 12019-21" = "MGSG12019-21",
		"MUASG014015-12" = "MUSG014015-12",
		"MUASG15017-16" = "MUSG15017-16",
		"MUSG016043-15" = "MUSG16043-15",
		"MGSG12046-16" = "MUSG12046-16",     # MUSG spelling in 3 of 4 plots
		"MUSG151108-18" = "MUSG15108-18",    # doubled '1' typo in OI rep 1 (completes 2x4 = 8 plots)
		"MUSG1614-10" = "MUSG16114-10"       # OI plots; MUSG16114-10 only in the LSD plots
	)
	variety <- trimws(as.character(r$Genotype))
	tofix <- variety %in% names(vfix)
	variety[tofix] <- vfix[variety[tofix]]

	is_oi <- r$Treatment == "Optimum Irrigation"

	# measurements: sheet name "031019" (3 Oct 2019); image file says 190 DAP (= 4 Oct, as dictionary counts planting as day 0)
	d <- data.frame(
		trial_id = "Umbeluzi_2019_B",
		plot_id = paste0(ifelse(is_oi, "OI", "LSD"), "_", r$PLOT),
		plot_row = as.integer(r$Row),
		plot_columns = as.integer(r$Column),
		rep = as.integer(r$Rep),
		crop = "sweetpotato",
		variety = variety,
		# landraces and check clones named in the dataset description; other variety types are in paper Table S2
		variety_type = ifelse(variety %in% c("Xitsekele", "Xiadlaxakau"), "landrace",
			ifelse(variety %in% c("Resisto", "199062.1"), "check", NA_character_)),
		treatment = ifelse(is_oi, "OI", "LSD"),   # "Terminal drought" = LSD in the paper
		stress = ifelse(is_oi, "none", "drought"),
		irrigation_stop_ = ifelse(is_oi, NA_integer_, 110L),  # irrigation withheld from 110 DAP
		date = "2019-10-03",
		DAP = 190L,
		canopy_temperature = as.numeric(r$Canopy.Temperature.C._mean),   # C
		NDVI = as.numeric(r$NDVI_mean),
		NDVI_green = as.numeric(r$NDVIgreen_mean),
		chlorophyll_index = as.numeric(r$CI_mean),
		ground_cover = as.numeric(r$Coberture_.pct)                      # %, green pixels / plot pixels
	)

	d$country <- "Mozambique"
	d$adm1 <- "Maputo"
	d$adm2 <- "Boane"
	d$location <- "Umbeluzi Research Station"
	# image centre from Image_description_Moz2.xlsx (image 158 x 119 m)
	d$longitude <- 32.361856
	d$latitude <- -26.0518
	d$geo_from_source <- TRUE
	d$geo_uncertainty <- 200
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	d$irrigation_method <- "furrow"   # paper: Trial B established by furrow irrigation
	# paper: planting 28 March 2019, harvest 18 October 2019
	d$planting_date <- "2019-03-28"
	d$harvest_date <- "2019-10-18"
	# paper: 5 rows x 17 plants, 0.9 m x 0.3 m
	d$row_spacing <- 90
	d$plant_spacing <- 30
	d$plant_density <- 10000 / (0.9 * 0.3)
	d$plot_length <- 5.1
	d$plot_width <- 4.5
	d$plot_area <- 5.1 * 4.5
	# paper: urea (46N:0P:0K) at 3 weeks after planting, rate not given
	d$fertilizer_type <- "urea"
	d$fertilizer_dap <- "21"   # three weeks after planting
	d$N_fertilizer <- as.numeric(NA)
	d$P_fertilizer <- 0
	d$K_fertilizer <- 0
	# paper: cypermethrin at 20 and 35 DAP
	d$insecticide_used <- TRUE
	d$insecticide_product <- "cypermethrin"
	d$insecticide_times <- 2L
	d$insecticide_dates <- "2019-04-17;2019-05-02"
	d$weeding_done <- TRUE
	d$weeding_implement <- "hoe"
	d$yield_part <- "roots"

	carobiner::write_files(path, meta, d)
}
