# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. raw header names can't be trusted for cols 10-11 (2 cols are even both
#    named "Yield_t_ha", cols 11 & 14) so columns were renamed by position, not
#    by name: metadata_time_of_planting.csv has 22 description rows that align
#    1:1, left-to-right, with the raw file's 22 data columns (all but ID, which
#    has no metadata row). That positional match is what shows "Vigor" (row 9)
#    and the 1st "Yield_t_ha" (row 10) actually hold Leaf Area Index and Number
#    of Leaves. Confirmed col 14, not col 11, is the true yield: back-calculated
#    yield (plant density x tubers/plant x mean tuber weight) matches col 14
#    (corr 0.9999) but not col 11 (corr 0.54)
# 2. exact planting/harvest dates are not in the raw data (only Year and
#    Early/Mid/Late); dates were taken from the linked publication's Materials
#    and Methods (Aighewi et al. 2020)
# 3. plant vigor, number of leaves/vines and the five pest/disease severity
#    scores have no matching terminag term and are kept as non-standard,
#    domain-prefixed variables

carob_script <- function(path) {

"
The influence of minisett size and time of planting on the yield of seed yam (Dioscorea rotundata) in Abuja, Nigeria

The data is from an investigation of the influence of planting different minisett sizes at different periods on yield. The trial was carried out on the experimental field at IITA Abuja Station (9.164694 N, 7.345136 E) during the cropping seasons of 2015 and 2016. It was laid out in a Randomized Complete Block Design (RCBD) with three replications with nine treatment combinations: 30 g minisetts planted early (Early); 30 g minisetts planted 21 days after the early planting (Mid); 30 g minisetts planted 21 days after the mid planting (Late). Similarly, 60 g and 90 g minisetts were planted early, mid and late, respectively. The D. rotundata landrace 'Meccakusa' was used. Generated as part of the Yam Improvement for Income and Food Security in West Africa (YIIFSWA) project.
"

	uri <- "doi:10.25502/hmrd-ba78/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = "doi:10.1017/S0014479720000095",
		project = "YIIFSWA",
		design = "RCBD",
		data_type = "on-station experiment",
		treatment_vars = "minisett_size;planting_date",
		response_vars = "yield",
		notes = NA,
		carob_contributor = "AI agronomy writer agent",
		carob_date = "2026-07-23",
		carob_completion = 100,
		carob_effort = 0.2
	)

	f <- ff[basename(ff) == "time-of-planting-trials-yiifswa_abj_data.csv"]

	# raw file has trailing empty rows; nrows drops them at read time
	r <- read.csv(f, nrows = 54)

	# the raw header has duplicated/mislabeled names; metadata_time_of_planting.csv lists the
	# true column descriptions in file order, so columns are renamed here by position
	names(r) <- c("ID", "Plot", "Rep", "Year", "SettSize", "TimePlant", "Perc_sprout",
		"Day50_perc_Sprout", "STEM_LENGTH_m", "LAI", "Leaf_number", "Vine_number", "Plant_vigor",
		"Yield_t_ha", "CRACK", "M_BUG", "SCALE", "CRZROOT_Gall", "ROT", "Number_of_tuber_trt",
		"PLST", "Mean_number_of_tubers_plant", "MeanWt")

	# Year is coded 1/2 for the two cropping seasons (confirmed in the linked publication)
	year_lookup <- c("1" = 2015, "2" = 2016)
	r$year <- year_lookup[as.character(r$Year)]

	# planting time is inconsistently capitalized in the raw data (e.g. "late" vs "Late")
	r$TimePlant <- carobiner::fix_name(r$TimePlant, "title")

	# exact planting dates by year and planting period, from Aighewi et al. (2020) Materials and Methods
	planting_dates <- data.frame(
		year = c(2015, 2015, 2015, 2016, 2016, 2016),
		TimePlant = c("Early", "Mid", "Late", "Early", "Mid", "Late"),
		planting_date = c("2015-06-02", "2015-06-23", "2015-07-14",
			"2016-05-06", "2016-05-27", "2016-06-17")
	)
	# harvest was on a single date per year for all treatments (Aighewi et al. 2020)
	harvest_dates <- data.frame(
		year = c(2015, 2016),
		harvest_date = c("2015-12-17", "2016-12-20")
	)

	r <- merge(r, planting_dates, by = c("year", "TimePlant"), all.x = TRUE)
	r <- merge(r, harvest_dates, by = "year", all.x = TRUE)

	d <- data.frame(
		trial_id = as.character(r$year),
		plot_id = as.character(r$Plot),
		rep = as.integer(r$Rep),
		country = "Nigeria",
		# IITA Abuja Station falls in Bwari LGA, Federal Capital Territory (checked with GADM
		# level-2 boundaries at 9.164694 N, 7.345136 E, the coordinates given in the source metadata)
		adm1 = "Federal Capital Territory",
		adm2 = "Bwari",
		longitude = 7.345136,
		latitude = 9.164694,
		geo_from_source = TRUE,
		crop = "yam",
		variety = "Meccakusa",
		variety_type = "landrace",
		minisett_size = r$SettSize, # g
		treatment = paste0(r$SettSize, "g_", r$TimePlant),
		planting_date = r$planting_date,
		harvest_date = r$harvest_date
	)

	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- FALSE

	# single ridges 9 m long, 1 m apart, minisetts planted 0.3 m apart within a row (Aighewi et al. 2020)
	d$plot_area <- 9 * 1 # m2
	d$plant_density <- (r$PLST / d$plot_area) * 10000 # plants/ha

	d$yield <- r$Yield_t_ha * 1000 # t/ha to kg/ha
	d$yield_part <- "tubers"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE

	d$LAI <- r$LAI # leaf area index, terminag standard term

	# no fertilizer was applied (Aighewi et al. 2020)
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- 0
	d$fertilizer_used <- FALSE
	d$fertilizer_type <- "none"

	## non-standard, domain-prefixed variables with no matching terminag term
	d$sprout_percent <- r$Perc_sprout # %
	d$sprout_days50 <- r$Day50_perc_Sprout # days to 50% sprouting
	d$vine_length <- r$STEM_LENGTH_m # m
	d$leaf_number <- r$Leaf_number
	d$vine_number <- r$Vine_number
	d$plant_vigor <- r$Plant_vigor # scale 1-5

	# pest/disease severity scores, all on a scale 1-5 (1 = least severe), metadata_time_of_planting.csv
	d$nematode_severity <- r$CRACK # scale 1-5
	d$mealybug_severity <- r$M_BUG # scale 1-5
	d$scaleinsect_severity <- r$SCALE # scale 1-5
	d$crazyroot_severity <- r$CRZROOT_Gall # scale 1-5
	d$rot_severity <- r$ROT # scale 1-5

	d$tuber_number <- r$Number_of_tuber_trt # tubers per plot
	d$tuber_number_plant <- r$Mean_number_of_tubers_plant
	d$tuber_mean_weight <- r$MeanWt # kg

	carobiner::write_files(path, meta, d)
}
