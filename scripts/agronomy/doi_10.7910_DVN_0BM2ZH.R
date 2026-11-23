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
# "40-40-40 kg NPK" taken as elemental N, P, K as written in the paper; it may be N-P2O5-K2O (then P=17.5, K=33.2). Verify.
# Paper coordinates "14 30 N, 121 15 E" used (degree signs lost in pdf); IRRI farm is at ~14.17 N. Verify.
# Planting/transplanting dates are not given per season; only the year from the season code is used
#   (DS sowing may be in December of the previous year).
# Not mapped: physiology sub-study files "13_Lowland Sel.xlsx", "13_Lowland_Sel_Detailed.xls" (2013WS) and
#   "10_Lowland Sel Detailed.xlsx" (2014WS): canopy temperature, NDVI, porometer, leaf area, yield components,
#   root length/dry weight by depth, tensiometer readings. These could be added (plot level, 12 entries x 4 reps).
# Harvest dates not given.
# write_files remaining: missing harvest_date; NA detected yield (1 record); out of bounds plant_height.


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
		response_vars = "yield;dmy_total;harvest_index;flowering_days;plant_height",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 70,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "Lowland-Sel_2014.xlsx"]
	r <- carobiner::read.excel(f, sheet="All")

	swap <- r$Season == "08WS" & r$Trt == "S"
	hi <- ifelse(swap, r$TDMY, r$HI)
	hi_fraction <- r$Season %in% c("08DS", "09DS", "09WS", "10WS", "11DS", "11WS")

	d <- data.frame(
		season = ifelse(grepl("DS", r$Season), "dry", "wet"),
		planting_date = paste0("20", substr(r$Season, 1, 2)),
		treatment = r$Trt,
		stress = ifelse(r$Trt == "S", "drought", "none"),
		variety = trimws(r$Genotype),
		rep = as.integer(r$Rep),
		flowering_days = r$DTF,
		plant_height = r$HT,
		yield = r$GY,
		dmy_total = ifelse(swap, r$HI, r$TDMY),
		harvest_index = ifelse(hi_fraction, hi * 100, hi)
	)
	d$trial_id <- paste0(r$Season, "_", r$Trt)

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

	# Torres & Henry 2018: 40-40-40 kg NPK/ha basal + 50 kg N/ha ammonium sulfate
	d$N_fertilizer <- 90
	d$P_fertilizer <- 40
	d$K_fertilizer <- 40
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
