# R script for "carob"
# license: GPL (>=3)

## NOTES
# Plot-level data (3 reps) of ~60 rice genotypes under well-watered (C) and drought (S) treatments
# in 11 seasons (2008DS-2013DS) at IRRI, Los Banos, from sheet "All" of "Lowland-Sel_2014.xlsx".
# Management from Torres & Henry 2018 (Field Crops Res. 220:37-45, included pdf): transplanted 18-21 day
# old seedlings, 25 x 20 cm, alpha lattice with 3 reps; 40-40-40 kg NPK/ha basal + 50 kg N/ha as
# ammonium sulfate at max. tillering; DRT: irrigation withdrawn 4 weeks after transplanting and re-watered
# at -65 kPa (30 cm); WW kept flooded. GY at 14% moisture (kg/ha), TDMY oven-dry (kg/ha).
# HI is given as a fraction in 08DS, 09DS, 09WS, 10WS, 11DS, 11WS and as % in other seasons; converted to %.

## ISSUES
# 11WS records are an exact copy of the 11DS records (all 360 rows); one of them is probably wrong. Kept both.
# 08WS drought (S) rows: TDMY and HI columns are clearly swapped (TDMY ~20-40, HI ~5000-9000); swapped back here. Verify.
# 10DS HI has some 0 values; Plant height has 10 values of 410-676 cm (probably typos, e.g. 67.6), left as recorded (out of bounds).
# One record (12DS S Dular rep 2) has NA yield.
# for data from 2008 to 2013 (dry season only ) Planting/transplanting dates are not given ; only the year from the season code is used

## Not processed
 # - 13_Lowland Sel.xlsx (incomplete raw data)
 # Not captured
#   - NDVI: Not captured because the measurement dates are ambiguous.


carob_script <- function(path) {

"
Yield stability of selected rice breeding lines and donors acrossconditions of mild to moderately severe drought stress

Although mild to moderately severe drought stress may have less of an effect on rice grain yield thansevere drought stress during reproductive stage, its prevalence across rice farmers' fields at the globallevel may be more economically significant. In this study, field experiments were conducted on selectedgenotypes with known tolerance to severe reproductive-stage drought in order to identify those thatwould produce high and stable grain yield across seasons and soil moisture conditions varying fromwell-watered to mild and moderately severe drought stress. Mild stress generally occurred during wetseasons and moderate stress happened during dry seasons. The drought stress was mild enough suchthat the time to flowering was similar under drought stress and well-watered conditions in either sea-son. However, significant grain yield reductions were incurred even at mild drought levels. Using anAMMI1 biplot analysis, IR83142-B-7-B-B, Binuhangin, IR77298-14-1-2-13, IR70215-70-CPA-3-4-1-3 andIR77298-14-1-2 were identified as the genotypes with the highest and most stable grain yields in bothwell-watered and mild to moderately severe drought stress environments. In a characterization of traitsconferring drought tolerance among the highest yielding genotypes under mild to moderate droughtstress, genotypes Binuhangin and IR70215-70-CPA-3-4-1-3 stood out for multiple physiological traitsunder drought. However, no direct correlations among genotypes between stomatal conductance, nor-malized difference vegetation index (NDVI) or root dry weight with grain or total dry matter yield wereobserved under any soil moisture level. These results reflect the complex interaction of drought responsetraits contributing to grain yield. The genotypic variation and physiological responses observed in thisstudy point to the potential of developing varieties targeted to mild and moderate drought stress usingyield as the selection criterion.
"

	uri <- "doi:10.7910/DVN/0BM2ZH"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "IRRI",
		publication = "doi:10.1016/j.fcr.2016.09.011",
		project = NA,
		design = "alpha lattice; 3 replications",
		data_type = "experiment",
		treatment_vars = "variety;stress",
		response_vars = "yield;dmy_total;harvest_index",
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-11-20",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 70,
		carob_effort = 0.6 + 2
	)

	f1 <- ff[basename(ff) == "Lowland-Sel_2014.xlsx"]
	f2 <- ff[basename(ff) == "13_Lowland_Sel_Detailed.xls"]
	f3 <- ff[basename(ff) == "10_Lowland Sel Detailed.xlsx"]
	f4 <- ff[basename(ff) == "13_Lowland Sel.xlsx"]
	
	### process data from 2008 to 2013 (only dry season)
	r1 <- carobiner::read.excel(f1, sheet="All")
	
	### some value of harvest index and Total dry matter are swapped 
	swap <- r1$Season == "08WS" & r1$Trt == "S"
	hi <- ifelse(swap, r1$TDMY, r1$HI)
	hi_fraction <- r1$Season %in% c("08DS", "09DS", "09WS", "10WS", "11DS", "11WS")

	d1 <- data.frame(
		season = ifelse(grepl("DS", r1$Season), "dry", "wet"),
		planting_date = paste0("20", substr(r1$Season, 1, 2)),
		treatment = r1$Trt,
		stress = ifelse(r1$Trt == "S", "drought", "none"),
		variety = trimws(r1$Genotype),
		rep = as.integer(r1$Rep),
		flowering_days = r1$DTF,
		plant_height = r1$HT,
		yield = r1$GY,
		dmy_total = ifelse(swap, r1$HI, r1$TDMY),
		harvest_index = ifelse(hi_fraction, hi * 100, hi)
	)
	
	##### process 2013 Wet season
	r2 <- carobiner::read.excel(f2, sheet = "Harvest")
	r_fd <- carobiner::read.excel(f2, sheet = "DTF")
	#r_ndv <- carobiner::read.excel(f2, sheet = "NDVI")
	r_ph <- carobiner::read.excel(f2, sheet = "Plant ht")
	
	d2 <- data.frame(
	  plot_id = as.character(r2$`Plot #`),
	  variety = trimws(r2$Genotype),
	  treatment = r2$Trt,
	  harvest_date = as.character(r2$Harv.Date),
	  stress = ifelse(r2$Trt == "S", "drought", "none"),
	  yield = r2$GY13,
	  dmy_total = r2$`TDMY, kg/ha`,
	  harvest_index = r2$HI,
	  season = "wet",
	  planting_date = "2013-06-06", ## from data description sheet 
	  transplanting_date = "2013-06-25"
	)
	
	fd <- data.frame(
	  plot_id = as.character(r_fd$Plot),
	  variety = trimws(r_fd$Name),
	  treatment = r_fd$Trt,
	  flowering_days = r_fd$DTF
	)
	
	ph <- data.frame(
	  plot_id = as.character(r_ph$Plot),
	  variety = trimws(r_ph$Genotype),
	  treatment = r_ph$Trt,
	  plant_height = r_ph$Mean
	)

	### merge 
	d2 <- merge(d2, fd, by = c("variety", "treatment", "plot_id"), all.x = TRUE) 
	d2 <- merge(d2, ph, by = c("variety", "treatment", "plot_id"), all.x = TRUE) 
	
	## process 2014 wet season data
	
	r3 <- carobiner::read.excel(f3, sheet = "Harvest", skip = 1)
	r_fd1 <- carobiner::read.excel(f3,  sheet = "Flowering")
	#r_ndv1 <- carobiner::read.excel(f3, sheet = "NDVI")
	r_ph1 <- carobiner::read.excel(f3, sheet = "Plant ht")
	
	d3 <- data.frame(
	  plot_id = as.character(r3$Plot),
	  variety = trimws(r3$Genotype),
	  treatment = r3$Trt,
	  harvest_date = as.character(r3$Harvest),
	  stress = ifelse(r2$Trt == "S", "drought", "none"),
	  yield = r3$GY14,
	  dmy_total = r3$TDMY,
	  harvest_index = r3$HI,
	  season = "wet",
	  planting_date = as.character(as.Date(41807, origin = "1899-12-30")),
	  transplanting_date = as.character(as.Date(41824, origin = "1899-12-30")) ## from data description sheet 
	)
	
	fd1 <- data.frame(
	  plot_id = as.character(r_fd1$Plot),
	  variety = trimws(r_fd1$Name),
	  treatment = r_fd1$Trt,
	  flowering_days = r_fd1$DTF
	)
	
	ph1 <- data.frame(
	  plot_id = as.character(r_ph1$Plot),
	  variety = trimws(r_ph1$Name),
	  treatment = r_ph1$Trt,
	  plant_height = r_ph1$Mean
	)
	
	### merge 
	d3 <- merge(d3, fd1, by = c("variety", "treatment", "plot_id"), all.x = TRUE) 
	d3 <- merge(d3, ph1, by = c("variety", "treatment", "plot_id"), all.x = TRUE) 
	
	##### joint d1, d2 and d3
	d <- carobiner::bindr(d1, d2, d3)
	
	d$trial_id <- paste0(d$season, "_", d$treatment)
	d$treatment = c("WW" =  "well-watered", "C" = "well-watered", "S" = "drought")[d$treatment]

	d$crop <- "rice"
	d$yield_part <- "grain"
	d$yield_moisture <- 14
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	d$flooded <- d$stress == "none"
	d$planting_method <- "transplanting"
	d$row_spacing <- 25
	d$plant_spacing <- 20
	d$plot_length <- 3
	# 3 rows x 25 cm
	d$plot_width <- 0.75

	# Torres & Henry 2018: 40-40-40 kg NPK/ha basal + 50 kg N/ha ammonium sulfate  ( From publication)
	d$N_fertilizer <- 90
	d$P_fertilizer <- 40/2.20
	d$K_fertilizer <- 40/1.2051
	# S from 50 kg N as ammonium sulfate (21% N, 24% S)
	d$S_fertilizer <- 50 / 0.21 * 0.24
	d$fertilizer_type <- "NPK; DAS"

	d$country <- "Philippines"
	d$adm1 <- "Laguna"
	d$location <- "IRRI, Los Banos"
	# coordinates as printed in the paper
	d$latitude <- 14.5
	d$longitude <- 121.25
	d$geo_from_source <- TRUE

	carobiner::write_files(path, meta, d)
}
