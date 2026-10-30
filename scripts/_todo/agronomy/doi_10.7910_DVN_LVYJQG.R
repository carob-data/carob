# R script for "carob"
# license: GPL (>=3)

## NOTES
# 16 field experiments at IRRI Los Banos (2016DS-2017DS): 17 genotypes x establishment (puddled transplanted, PTR;
#   dry direct-seeded, DSR) x water (W well-watered, S reproductive-stage drought) x N rate (CN, LN, HN).
# Main data: "XX for slope.csv" (GY, Biomass, HI, root length density by depth, per plot).
# "Expt names.xlsx" links experiment codes to environment codes and sowing dates.
# Long table: NDVI (NDVI.csv, with DAS) and canopy temperature (CT.csv, dd-Mon dates; year taken from experiment code).
# Leaf osmotic potential (LOP.csv, LOP2 in MPa) merged per plot.
# Methods from Xie et al. 2021 (doi:10.1093/jxb/erab214, included as PDF):
#   coordinates 14 10'11.81"N 121 15'39.22"E; GY and biomass in g/m2 from 1.5 m2 harvest area; GY at 14% moisture;
#   N: CN 100; LN 60 (DSR) / 75 (PTR); HN 120 (DSR) / 150 (PTR) kg N/ha;
#   P2O5/K2O 50 (expts 1-8), 75 (9-12), 60 (13-16) kg/ha -> elemental P and K;
#   PTR: 25 cm rows, 20 cm hills, transplanted 17-18 d seedlings; DSR: 25 cm rows, 10 cm hills, sprinkler irrigated.
# RLD_A-D are taken as root length density at 0-15, 15-30, 30-45 and 45-60 cm (cores to 60 cm in 15 cm increments).

## ISSUES
# RLD units not stated in the data (probably cm/cm3); depth assignment of A-D assumed from Methods.
# Shoot N uptake block in "XX for slope.csv" (cols Env..NUEb; shoot dry weight, N content, N uptake, NUEg, NUEb by stage)
#   is not mapped: N content and dry weight units unclear (sample of 1 or 4 hills).
# Greenhouse lysimeter study (BG02 workbook, "BG02 water uptake.csv") is not mapped.
# The supplementary PDF (soil characteristics) is not used.
# Genotype names contain trailing non-breaking spaces in the source; removed.
# Some plots (16DS_23S plots 46-51 etc., 16WS_12S) have NDVI/CT but no harvest record; added with NA yield.
# One LOP record (16WS_12W plot 21) has no matching plot in the harvest file and is not used.
# Some GY are 0 (severe drought, failed to flower); kept.
# Harvest dates not given. write_files: missing harvest_date; NA yield (plots without harvest record); new variables.

## NEW VARIABLES
# rld_0_15_, rld_15_30_, rld_30_45_, rld_45_60_: root length density at the given soil depth (cm) (units probably cm/cm3)
# leaf_osmotic_potential_: flag-leaf osmotic potential (MPa)
# canopy_temp_: canopy temperature (degrees C), long table
# variety_set_: genotype group as given in the data (e.g. other breeding lines)
# DAS_: days after sowing (long table; for PTR not equal to days after transplanting)


carob_script <- function(path) {

"
Establishment method affects rice root plasticity in response to drought and its relationship with grain yield stability

By responding to the variable soil environments in which they are grown, the roots of rice crops are likely to contribute to yield stability across a range of soil moistures, nutrient levels, and establishment methods. In this study, we explored different approaches to quantification of root plasticity and characterization of its relationship with yield stability. Using four different statistical approaches (plasticity index, slope, AMMI, and factor analytic) on a set of 17 genotypes including several recently-developed breeding lines targeted to dry direct-seeding, we identified only very few direct relationships between root plasticity and yield stability. However, genotypes identified as having combined yield stability and root plasticity showed higher grain yields across trials. Furthermore, root plasticity was expressed to a greater degree in puddled transplanted trials rather than under dry direct-seeding. Significant interactions between nitrogen and water resulted in contrasting relationships between nitrogen-use efficiency and biomass stability between puddled-transplanted and direct-seeded conditions. These results reflect the complex interaction between nitrogen, drought, and even different types of drought (as a result of the establishment method) on rice root growth, and suggest that although rice root plasticity may confer stable yield across a range of environments, it might be necessary to more narrowly define the targeted environments to which it will be most beneficial.
"

	uri <- "doi:10.7910/DVN/LVYJQG"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IRRI",
		publication = "doi:10.1093/jxb/erab214",
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety;planting_method;N_fertilizer;drought_stress",
		response_vars = "yield;dmy_total;harvest_index", 
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 60,
		carob_effort = 1
	)

	f1 <- ff[basename(ff) == "XX for slope.csv"]
	f2 <- ff[basename(ff) == "Expt names.xlsx"]
	f3 <- ff[basename(ff) == "NDVI.csv"]
	f4 <- ff[basename(ff) == "CT.csv"]
	f5 <- ff[basename(ff) == "LOP.csv"]

	r1 <- read.csv(f1, check.names=FALSE, fileEncoding="latin1")
	r2 <- carobiner::read.excel(f2, col_names=FALSE, skip=1)
	r3 <- read.csv(f3, check.names=FALSE, fileEncoding="latin1")
	r4 <- read.csv(f4, check.names=FALSE, fileEncoding="latin1")
	r5 <- read.csv(f5, check.names=FALSE, fileEncoding="latin1")

	# the left block of "XX for slope.csv" (first 17 columns) has the harvest data
	r1 <- r1[, 1:17]
	d <- data.frame(
		trial_id = r1$Expt1,
		plot_id = as.character(r1$Plot),
		rep = r1$Rep,
		variety = trimws(r1$Genotype, whitespace="[\\h\\v]"),
		variety_set_ = r1$Set2,
		yield = r1$GY * 10,
		dmy_total = r1$Biomass * 10,
		harvest_index = r1$HI * 100,
		rld_0_15_ = r1$RLD_A,
		rld_15_30_ = r1$RLD_B,
		rld_30_45_ = r1$RLD_C,
		rld_45_60_ = r1$RLD_D
	)

	# some plots have NDVI/CT observations but no harvest data
	obs <- unique(rbind(r3[, c("Experiment", "Plot", "Rep", "Genotype")], r4[, c("Experiment", "Plot", "Rep", "Genotype")]))
	obs <- data.frame(
		trial_id = obs$Experiment,
		plot_id = as.character(obs$Plot),
		rep = obs$Rep,
		variety = trimws(obs$Genotype, whitespace="[\\h\\v]")
	)
	obs <- obs[!paste(obs$trial_id, obs$plot_id) %in% paste(d$trial_id, d$plot_id), ]
	d <- carobiner::bindr(d, obs)

	env <- data.frame(
		trial_id = r2[[1]],
		env = r2[[2]],
		planting_date = as.character(as.Date(r2[[3]]))
	)
	d <- merge(d, env, by="trial_id", all.x=TRUE)

	d$season <- ifelse(grepl("DS", d$env), "dry", "wet")
	d$planting_method <- ifelse(grepl("PTR", d$env), "transplanting", "direct seeding")
	d$land_prep_method <- ifelse(grepl("PTR", d$env), "puddled", "ploughing")
	d$drought_stress <- ifelse(grepl("\\(S\\)", d$env), "reproductive stage", "none")
	nlev <- substr(sub(".*_", "", d$env), 1, 2)
	ptr <- grepl("PTR", d$env)
	d$N_fertilizer <- ifelse(nlev == "CN", 100, 
		ifelse(nlev == "LN", ifelse(ptr, 75, 60), ifelse(ptr, 150, 120)))
	d$N_splits <- ifelse(nlev == "CN", 2L, ifelse(ptr, 3L, 4L))
	yr <- substr(d$env, 1, 6)
	P2O5 <- ifelse(yr %in% c("2016DS", "2016WS"), 50, ifelse(ptr, 75, 60))
	d$P_fertilizer <- P2O5 / 2.29
	d$K_fertilizer <- P2O5 / 1.2051
	d$transplanting_date <- ifelse(!ptr, NA, 
		ifelse(yr == "2016DS", "2016-01-05", ifelse(yr == "2016WS", "2016-07-19", "2017-01-10")))
	d$row_spacing <- 25
	d$plant_spacing <- ifelse(ptr, 20, 10)
	d$irrigated <- TRUE
	d$irrigation_method <- ifelse(ptr, NA, "sprinkler")
	d$treatment <- d$env
	d$env <- NULL

	lop <- data.frame(
		trial_id = r5$Experiment,
		plot_id = as.character(r5$Plot),
		leaf_osmotic_potential_ = r5$LOP2
	)
	d <- merge(d, lop, by=c("trial_id", "plot_id"), all.x=TRUE)

	d$crop <- "rice"
	d$yield_part <- "grain"
	d$yield_moisture <- 14
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$country <- "Philippines"
	d$adm1 <- "Laguna"
	d$location <- "IRRI, Los Banos"
	d$longitude <- 121.26089
	d$latitude <- 14.16995
	d$geo_from_source <- TRUE
	d$record_id <- 1:nrow(d)

	# dates are "dd-Mon" without year; the year is taken from the experiment code
	ndvi <- data.frame(
		trial_id = r3$Experiment,
		plot_id = as.character(r3$Plot),
		date = as.character(as.Date(paste0("20", substr(r3$Experiment, 1, 2), "-", r3$Date), "%Y-%d-%b")),
		DAS_ = r3$DAS,
		NDVI = r3$NDVI
	)
	ct <- data.frame(
		trial_id = r4$Experiment,
		plot_id = as.character(r4$Plot),
		date = as.character(as.Date(paste0("20", substr(r4$Experiment, 1, 2), "-", r4$Date), "%Y-%d-%b")),
		canopy_temp_ = r4$CT
	)
	dlong <- carobiner::bindr(ndvi, ct)
	dlong <- merge(dlong, d[, c("trial_id", "plot_id", "record_id")], by=c("trial_id", "plot_id"))
	dlong$trial_id <- dlong$plot_id <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
