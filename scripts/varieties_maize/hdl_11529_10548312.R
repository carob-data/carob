# R script for "carob"
# license: GPL (>=3)

## NOTES
# Two 2016 trial sets: on-station (dry season, irrigated, 4 sites; Adnan et al. 2019) and on-farm in the Northern Guinea Savanna (Doguwa, Ikara, Lere; Adnan et al. 2020, 2022).
# On-farm: 10 varieties x 3 densities over 30 farmers' fields, 10 plots per field; trial_id is the field.
# The Sudan Savanna file (NG_VT_Validation_SS_2016.xlsx) is not processed here; it is already in hdl_11529_221663.

## ISSUES
# On-station: paper says 4 reps, 20 varieties and "120N:60P:60K"; data have 3 reps, 22 varieties. P and K taken as P2O5 and K2O, as in the on-farm papers.
# S_fertilizer includes the S in SSP (12%), not mentioned in the papers. Keep?
# "OBASUPER1" in the data vs "OBA SUPER 9" in the paper; which variety?
# Likely data errors, left as reported: grain yield > cob yield in 20 plots; harvest index differs from grain/tops weight in 123 plots; one 100-seed weight of 99.7 g.

## NEW VARIABLES
# cob_yield: cob yield after drying (kg/ha)
# biomass_tasseling: biomass without cobs at 50% tasselling (kg/ha; fresh or dry not stated)
# kernels_per_cob: kernels per cob (mean of 5 cobs)


## NOT PROCESSED
# herbicide rate: product only (Primextra 4 l/ha), no active ingredient
# farmer SAA class (A-E): in the papers, not in the data


carob_script <- function(path) {

"
Nigeria Variety Trial

Validation Trials Dataset for Nigeria
"

	uri <- "hdl:11529/10548312"
	group <- "varieties_maize"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "BUK;IITA;KUL",       # files: IITA, CDA BUK; dataset: University of Leuven
		publication = "doi:10.1371/journal.pone.0200118;doi:10.3389/fagro.2022.773012;doi:10.1017/S001447972000037X",
		project = "TAMASA",
		design = "on-station: RCBD; on-farm: incomplete blocks (farmers' fields)",
		data_type = "experiment",
		treatment_vars = "variety;plant_density",
		response_vars = "yield;cob_yield_;dmy_total;harvest_index;seed_weight;biomass_tasseling_;emergence_days;tassling_days;silking_days;maturity_days;kernels_per_cob_",
		notes = NA,
		carob_contributor = "Stella Muthoni",
		carob_LLM = "Claude Opus 5.5 (merged from 3 drafts)",
		carob_date = "2026-11-18",
		carob_completion = 95,
		carob_effort = 1.5
	)

	f1 <- ff[basename(ff) == "NG_VT_Calibration_2016.xlsx"]
	f2 <- ff[basename(ff) == "NG_VT_NGS_2016 (1).xlsx"]

	r1a <- carobiner::read.excel(f1, sheet="metadata")      # dataset description, site coordinates, dates
	r1b <- carobiner::read.excel(f1, sheet="plot layout")   # empty
	r1c <- carobiner::read.excel(f1, sheet="variables")     # codebook with units
	r1d <- carobiner::read.excel(f1, sheet="Data")          # calibration plot data
	r1e <- carobiner::read.excel(f1, sheet="Data_History")  # processing log
	r2a <- carobiner::read.excel(f2, sheet="Metadata")      # dataset description, LGA coordinates, dates
	r2b <- carobiner::read.excel(f2, sheet="variables")     # codebook with units
	r2c <- carobiner::read.excel(f2, sheet="Revised")       # NGS on-farm plot data
	r2d <- carobiner::read.excel(f2, sheet="Data history")  # processing log

## calibration trials, dry season 2016, irrigated (PLoS ONE 2019)
	# coordinates from the r1a metadata sheet: 
	# its Longitude/Latitude labels are swapped and the pairs are listed in the order Lere, BUK, Samaru, Dambatta
	d1d <- data.frame(
		location = unname(c(BUK="BUK", DBT="Dambatta", SMR="Samaru", LERE="Lere")[r1d$Location]),
		rep = as.integer(r1d$Rep),
		variety = trimws(r1d$Variety),
		yield = r1d$GY_ha,                       # kg/ha, after drying
		cob_yield_ = r1d$CY_ha,                  # kg/ha, after drying
		seed_weight = r1d$hun_SW * 10,           # 100-seed -> 1000-seed weight (g)
		biomass_tasseling = r1d$BM_Tasselling,  # kg/ha, without cobs
		emergence_days = r1d$DTE,
		tassling_days = r1d$DTT,                 # 50% of plants with tassels
		silking_days = r1d$DTS,
		maturity_days = r1d$DTPM,                # physiological maturity
		kernels_per_cob = r1d$No_Kernel,        # mean of 5 cobs
		trial_id = paste0("cal_", r1d$Location),
		plot_id = paste0("cal_", seq_len(nrow(r1d))),
		season = "dry",
		on_farm = FALSE,
		irrigated = TRUE,                        # irrigated when soil moisture fell below field capacity
		planting_date = unname(c(BUK="2016-03-16", DBT="2016-03-19", SMR="2016-03-22", LERE="2016-03-17")[r1d$Location]),  # PLoS Table 2
		plant_density = 53333,                   # 0.75 x 0.25 m, thinned to 1 plant per hill
		row_spacing = 75,
		plant_spacing = 25,
		plot_area = 13.5,                        # 6 ridges, 0.75 m x 3 m
		sampled_plot_area = 4.5,                 # 2 inner ridges
		OM_used = TRUE,
		OM_type = "poultry manure",
		OM_amount = 5000,                        # "approximately" 5 t/ha
		herbicide_used = TRUE,
		herbicide_product = "atrazine;metolachlor",  # Primextra, pre-emergence
		latitude = unname(c(BUK=11.97978, DBT=12.35444, SMR=11.18686, LERE=10.39217)[r1d$Location]),
		longitude = unname(c(BUK=8.41923, DBT=8.64945, SMR=7.61470, LERE=8.58265)[r1d$Location]),
		adm1 = unname(c(BUK="Kano", DBT="Kano", SMR="Kaduna", LERE="Kaduna")[r1d$Location]),
		geo_from_source = TRUE,
		geo_uncertainty = 1000                   # research farm extent
	)
	# these 12 plots (3 varieties x 3 reps at DBT, 1 at SMR) have no data in any column
	d1d <- d1d[!is.na(d1d$yield), ]
	## harvest_date extimation from plating_date and maturity days
    d1d$harvest_date = as.character(as.Date(d1d$planting_date)+ d1d$maturity_days)

## on-farm trials, Northern Guinea Savanna, rainy season 2016 (Front. Agron. 2022; Exp. Agric. 2020)
	# one point per LGA from the r2a metadata sheet; Exp. Agric. gives points 0.3-6 km away
	d2c <- data.frame(
		adm1 = unname(c(Doguwa="Kano", Ikara="Kaduna", Lere="Kaduna")[r2c$LGA]),
		adm2 = r2c$LGA,
		variety = trimws(r2c$Variety),
		plant_density = r2c$Density,             # plants/ha, after thinning to 1 plant per hill
		cob_yield = r2c$CY_ha,                  # kg/ha, after drying
		yield = r2c$GY_ha,                       # kg/ha, after drying
		dmy_total = r2c$TW_Ha,                   # tops weight with cobs at harvest; papers oven-dried stover and cob subsamples
		harvest_index = r2c$HI * 100,            # ratio -> %
		trial_id = paste0("ngs_", r2c$LGA, "_", r2c$`Farm ID`),  # farmer field
		plot_id = paste0("ngs_", seq_len(nrow(r2c))),
		season = "wet",
		on_farm = TRUE,
		irrigated = FALSE,                       # sown "as soon as the rains were established"
		planting_date = unname(c(Doguwa="2016-06-20", Ikara="2016-06-21", Lere="2016-06-24")[r2c$LGA]),
		row_spacing = 75,
		plant_spacing = unname(c("26666"=50, "53333"=25, "66666"=20)[as.character(r2c$Density)]),
		plot_area = 30,                          # 8 ridges, 0.75 m x 5 m
		sampled_plot_area = 12,                  # 4 inner ridges x 4 m
		latitude = unname(c(Doguwa=10.77841, Ikara=11.14099, Lere=10.54297)[r2c$LGA]),
		longitude = unname(c(Doguwa=8.60288, Ikara=8.23207, Lere=8.45094)[r2c$LGA]),
		geo_from_source = TRUE,
		geo_uncertainty = 10000                  # 10 fields per LGA share one point
	)
	# these 15 plots (4 with Farm ID ".") have no data in any column
	d2c <- d2c[!is.na(d2c$yield), ]

	d <- carobiner::bindr(d1d, d2c)
	d$treatment <- paste0(d$variety, "_", d$plant_density)
	d$crop <- "maize"
	d$country <- "Nigeria"
	d$is_survey <- FALSE
	d$yield_part <- "grain"
	d$yield_isfresh <- FALSE                     # "after drying", moisture not reported
	d$yield_moisture <- NA     # not reported
	
	# 120 N : 60 P2O5 : 60 K2O kg/ha as urea, SSP and KCl; P and K at sowing, half N at sowing, half 21 days later
	d$N_fertilizer <- 120
	d$P_fertilizer <- 60 / 2.29
	d$K_fertilizer <- 60 / 1.2051
	d$S_fertilizer <- d$P_fertilizer / 0.0874 * 0.12  # SSP: 8.74% P, 12% S
	d$fertilizer_type <- "urea;SSP;KCl"
	d$N_splits <- 2L
	

	# PLoS 2019 Table 3 (calibration names) and Front. Agron. 2022 Table 1 (NGS names, and calibration names that are the same without spaces)
	vt <- data.frame(
		variety = c("2011TZEWDTSTRSYN", "2013TZEEWPOPDTSTR", "EVDTW99STR", "EVDTY2000STR", "OBASUPER1", "M0926-8",
			"TZE24 x TZE25", "TZEEI29 x TZEEI21", "TZEEWPOPSTRC5 x TZEE16", "TZEYPOPDTSTRC4 x TZEEI13",
			"M1026-10", "IWD C2 SYN", "OBA98", "DT STR-Y SYN-2", "TZE COMP3 DT C3", "TZECOMP3DTC3", "2009EVDT-WSTR",
			"DTSTRW", "TZBSR", "TZL COMP4C3 DT C2", "TZLCOMP1SYN"),
		variety_type = c("OPV", "OPV", "OPV", "OPV", "hybrid", "hybrid",
			"hybrid", "hybrid", "hybrid", "hybrid",
			"hybrid", "OPV", "hybrid", "OPV", "OPV", "OPV", "OPV",
			"OPV", "OPV", "OPV", "OPV"),
		maturity_group_ = c("early", "extra early", "early", "early", "late", "late",
			"early", "extra early", "extra early", "extra early",
			"intermediate", "intermediate", "late", "late", "early", "early", "early",
			"late", "late", "late", "late")
	)
	i <- match(d$variety, vt$variety)
	d$variety_type <- vt$variety_type[i]
	variety_traits <- ifelse(!is.na(vt$maturity_group_[i]),  paste0(vt$maturity_group_[i], " maturity"), vt$maturity_group_[i])
	
	carobiner::write_files(path, meta, d)
}
