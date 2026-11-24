# R script for "carob"
# license: GPL (>=3)

## NOTES
# Paper: Ramirez et al. 2021 (doi:10.1111/jac.12565), "Trial B"; NDVI drop under LSD (-4.0%) matches the data.
# 120 clones x 2 water regimes (OI, LSD), 2 reps each, resolvable row-column design; 480 plots, 1 trial.
# Data are UAV imagery summaries per plot at one date (190 DAP); no yield or vine weight in this dataset.
# Raw images (Imagenes_Moz2.rar, 303 MB) not downloaded; coordinates from Image_description_Moz2.xlsx.

## ISSUES
# Yield, vine and root weights are in the paper (BLUPs only) but not in the data; yield left NA.
# Genotype spelling variants harmonised (9 names); check the mapping in the code.
# After that, 122 names (paper: 120); 5 names in >4 plots, 7 in <4 plots; kept as reported.
# Urea rate not reported: N_fertilizer NA.
# 5 plots have negative CI red-edge (NIR < red-edge reflectance); kept as reported.
# Plot area: dataset says 25 m2, paper gives 5.1 x 4.5 m (22.95 m2); paper value used.
# Variety types (checks, parents, landraces) are in paper Table S2 (not available); only 2 landraces set.

## NEW VARIABLES
# canopy_temp: mean canopy temperature of the plot from UAV thermal image, C
# canopy_temp_sd: standard deviation of canopy temperature pixels in the plot, C
# NDVI_sd: standard deviation of NDVI pixels in the plot, unitless
# NDVI_green: mean green NDVI (NIR - green)/(NIR + green) of the plot, unitless
# NDVI_green_sd: standard deviation of green NDVI pixels in the plot, unitless
# CI_rededge: mean chlorophyll red-edge index (NIR/RedEdge - 1) of the plot, unitless
# CI_rededge_sd: standard deviation of CI red-edge pixels in the plot, unitless
# irrigation_stop: days after planting from which irrigation was withheld (LSD only), days

## NOT PROCESSED
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
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = "doi:10.1111/jac.12565",
		project = NA,
		design = "resolvable row-column; 2 reps per water regime",
		data_type = "experiment",
		treatment_vars = "variety;stress",
		response_vars = "canopy_temp;NDVI;NDVI_green;CI_rededge;ground_cover",
		notes = NA,
		carob_contributor = NA,
		carob_LLM = "Claude Opus 5.5",
		carob_date = "2026-11-24",
		carob_completion = 90,
		carob_effort = 0.4
	)

	f1 <- ff[basename(ff) == "Umbeluzi_2019_Moz2.xlsx"]

	r1 <- carobiner::read.excel.hdr(f1, skip=2, hdr=1)   # sheet "031019": plot design and UAV indices (two header rows)

	## d1 from r1
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
		"MUSG1614-10" = "MUSG16114-10"       # OI plots; MUSG16114-10 only in the LSD plots
	)
	variety <- r1$Genotype
	tofix <- variety %in% names(vfix)
	variety[tofix] <- vfix[variety[tofix]]
	# measurements: sheet name "031019" (3 Oct 2019); paper and image file say 190 DAP (planting day counted as day 1)
	d1 <- data.frame(
		plot_id = paste0(ifelse(r1$Treatment == "Optimum Irrigation", "OI", "LSD"), "_", r1$PLOT),
		plot_row = as.integer(r1$Row),
		plot_columns = as.integer(r1$Column),
		rep = as.integer(r1$Rep),
		variety = variety,
		treatment = ifelse(r1$Treatment == "Optimum Irrigation", "OI", "LSD"),   # "Terminal drought" = LSD in the paper
		stress = ifelse(r1$Treatment == "Optimum Irrigation", "none", "drought"),
		irrigation_stop = ifelse(r1$Treatment == "Optimum Irrigation", NA, 110L),  # irrigation withheld from 110 DAP
		date = "2019-10-03",
		DAP = 190L,
		canopy_temp = r1$Canopy.Temperature.C._mean,          # C
		canopy_temp_sd = r1$stdev,
		NDVI = r1$NDVI_mean,
		NDVI_sd = r1$stdev.1,
		NDVI_green = r1$NDVIgreen_mean,
		NDVI_green_sd = r1$stdev.2,
		CI_rededge = r1$CI_mean,
		CI_rededge_sd = r1$stdev.3,
		ground_cover = r1$Coberture_.pct                      # %, green pixels / plot pixels
	)

	d <- d1
	d$trial_id <- "Umbeluzi_2019_B"
	d$crop <- "sweetpotato"
	# landraces named in the dataset description; other variety types are in paper Table S2
	d$variety_type <- ifelse(d$variety %in% c("Xitsekele", "Xiadlaxakau"), "landrace", NA)
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
	d$fertilizer_date <- "2019-04"
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
	d$yield <- as.numeric(NA)            # not reported in the data
	d$yield_moisture <- as.numeric(NA)   # not reported
	d$yield_isfresh <- NA                # no yield in the data (paper yields are fresh weights)

	carobiner::write_files(path, meta, d)
}
