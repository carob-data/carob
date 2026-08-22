# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. raw cols 10-11 mislabeled/duplicate-named ("Vigor" is actually LAI; two
#    cols named "Yield_t_ha", the 1st is leaf count, the 2nd (auto-deduped to
#    "Yield_t_ha.1") is true yield). Resolved via metadata_time_of_planting.csv,
#    whose 22 description rows align 1:1 with the raw file's data columns.
#    Confirmed yield column: back-calculated yield (plant_density x
#    tubers/plant x mean tuber weight) matches "Yield_t_ha.1" (corr 0.9999),
#    not "Yield_t_ha" (corr 0.54)
# 2. exact planting/harvest dates not in raw data (only Year, Early/Mid/Late);
#    taken from linked publication's Materials and Methods (Aighewi et al. 2020)
# 3. tuber/leaf/vine counts have no matching terminag term; kept non-standard,
#    normalized to per-ha density (leaf/vine counts are per-plant means, so
#    multiplied by plant_density; tubers are a plot total, so divided by
#    plot_area) so each stays recoverable as density / plant_density
# 4. tuber_mean_weight (raw MeanWt) dropped: recoverable as yield /
#    tuber_density (corr 0.9999)
# 5. pests (nematode/mealybug/scale insect/crazy root-gall) pivoted wide to
#    long into pest_species/pest_severity, 54 -> 216 rows; tuber rot is a
#    disease, not a pest, so it's kept separate as disease/disease_severity
#    ("tuber rot" not a listed disease value -> expected "invalid terms" warning)

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
		carob_contributor = "Oscar Bautista",
		carob_LLM = "Claude Sonnet 5",
		carob_date = "2026-07-23",
		carob_completion = 100,
		carob_effort = 0.2
	)

	f <- ff[basename(ff) == "time-of-planting-trials-yiifswa_abj_data.csv"]

	# raw file has trailing empty rows; nrows drops them at read time
	r <- read.csv(f, nrows = 54)

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
		adm1 = "Federal Capital Territory",
		adm2 = "Bwari",
		longitude = 7.3451,
		latitude = 9.1647,
		geo_from_source = TRUE,
		crop = "yam",
		variety = "Meccakusa",
		variety_type = "landrace",
		minisett_size = r$SettSize, # g
		treatment = paste0(r$SettSize, "g_", r$TimePlant),
		planting_date = r$planting_date,
		harvest_date = r$harvest_date,

		yield = r$Yield_t_ha.1 * 1000, # t/ha -> kg/ha; true yield col, see ISSUES 1
		LAI = r$Vigor, # raw col mislabeled "Vigor", see ISSUES 1

		## non-standard, domain-prefixed variables with no matching terminag term
		# % of planted minisetts that sprouted (metadata: "Percentage of Sprout")
		sprout_percent = r$Perc_sprout,
		# days after planting until 50% of minisetts had sprouted (metadata:
		# "Duration of the 50 Percentage Sprout (days)")
		sprout_days50 = r$Day50_perc_Sprout,
		vine_length = r$STEM_LENGTH_m, # m
		plant_vigor = r$PL_Vigor, # scale 1-5

		# pest severity, scale 1-5 (1=least severe); pivoted to long below, ISSUES 5
		nematode_severity = r$CRACK,
		mealybug_severity = r$M_BUG,
		scaleinsect_severity = r$SCALE,
		crazyroot_severity = r$CRZROOT_Gall,
		rot_severity = r$ROT # scale 1-5; disease not pest, see ISSUES 5
	)

	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- FALSE

	# ridges 9 m long, 1 m apart (Aighewi et al. 2020)
	d$plot_area <- 9 * 1 # m2
	d$plant_density <- (r$PLST / d$plot_area) * 10000 # plants/ha

	# tuber count is a plot total -> divide by plot_area, see ISSUES 3
	d$tuber_density <- (r$Number_of_tuber_trt / d$plot_area) * 10000 # tubers/ha
	# leaf/vine counts are per-plant means -> multiply by plant_density, see ISSUES 3
	d$leaf_density <- r$Yield_t_ha * d$plant_density
	d$vine_density <- r$NO_VINE * d$plant_density

	d$yield_part <- "tubers"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE

	# no fertilizer was applied (Aighewi et al. 2020)
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- 0
	d$fertilizer_used <- FALSE
	d$fertilizer_type <- "none"

	d$disease <- "tuber rot"
	d$disease_severity <- as.character(d$rot_severity)
	d$rot_severity <- NULL

	d <- reshape(d, direction = "long",
		varying = c("nematode_severity", "mealybug_severity",
			"scaleinsect_severity", "crazyroot_severity"),
		timevar = "pest_species",
		times = c("nematode", "mealybug", "scale insect", "crazy root/gall"),
		v.names = "pest_severity",
		idvar = c("trial_id", "plot_id"))
	rownames(d) <- NULL

	# terminag defines pest_severity as character (matches precedent in
	# doi_10.21223_P3_RBR0FG.R, doi_10.21421_D2_MHOUWW.R, ...)
	d$pest_severity <- as.character(d$pest_severity)
	d$severity_scale <- "1-5, 1=least severe" # constant, see metadata CSV

	carobiner::write_files(path, meta, d)
}
