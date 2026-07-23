# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. raw header names can't be trusted for cols 10-11 (2 cols are even both
#    named "Yield_t_ha", cols 11 & 14; read.csv auto-dedups the 2nd to
#    "Yield_t_ha.1"). metadata_time_of_planting.csv has 22 description rows
#    that align 1:1, left-to-right, with the raw file's 22 data columns (all
#    but ID, which has no metadata row). That positional match is what shows
#    "Vigor" (row 9) and the 1st "Yield_t_ha" (row 10) actually hold Leaf Area
#    Index and Number of Leaves - see the inline comments at each field below.
#    Confirmed "Yield_t_ha.1", not "Yield_t_ha", is the true yield:
#    back-calculated yield (plant density x tubers/plant x mean tuber weight)
#    matches "Yield_t_ha.1" (corr 0.9999) but not "Yield_t_ha" (corr 0.54)
# 2. exact planting/harvest dates are not in the raw data (only Year and
#    Early/Mid/Late); dates were taken from the linked publication's Materials
#    and Methods (Aighewi et al. 2020)
# 3. plant vigor and number of leaves/vines have no matching terminag term
#    and are kept as non-standard, domain-prefixed variables
# 4. the five pest/disease severity scores are pivoted from wide to long
#    (pest_species/pest_severity/severity_scale) via reshape(), which
#    multiplies the row count from 54 to 270 (5 rows per original plot);
#    tuber rot (ROT) is grouped with the other 4 as "pest" per the editor's
#    instruction, though it could arguably be classified as a disease instead
# 5. per editor feedback, Number_of_tuber_trt (raw count per 9 m2 plot) is
#    converted to tuber_density (tubers/ha), matching the plant_density
#    convention; Mean_number_of_tubers_plant is dropped, since it is now
#    fully recoverable as tuber_density / plant_density (both kept in d)

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
		harvest_date = r$harvest_date,

		# raw col "Yield_t_ha.1" (2nd dup-named col) confirmed true yield, ISSUES 1
		yield = r$Yield_t_ha.1 * 1000, # t/ha to kg/ha

		# raw col "Vigor" mislabeled; metadata confirms it is LAI, see ISSUES 1
		LAI = r$Vigor,

		## non-standard, domain-prefixed variables with no matching terminag term
		sprout_percent = r$Perc_sprout, # %
		sprout_days50 = r$Day50_perc_Sprout, # days to 50% sprouting
		vine_length = r$STEM_LENGTH_m, # m
		# raw col "Yield_t_ha" (1st dup-named col) mislabeled; metadata confirms
		# it is Number of Leaves, see ISSUES 1
		leaf_number = r$Yield_t_ha,
		vine_number = r$NO_VINE, # metadata: Number of Vines
		plant_vigor = r$PL_Vigor, # scale 1-5

		# pest/disease severity, scale 1-5 (1=least severe), see metadata CSV;
		# pivoted into long-format pest_severity/pest_species below, ISSUES 4
		nematode_severity = r$CRACK, # scale 1-5
		mealybug_severity = r$M_BUG, # scale 1-5
		scaleinsect_severity = r$SCALE, # scale 1-5
		crazyroot_severity = r$CRZROOT_Gall, # scale 1-5
		rot_severity = r$ROT, # scale 1-5

		# mean weight per tuber is an intensive (average) measure, not an
		# extensive per-plot/per-plant count, so the per-ha rule doesn't
		# apply; kept as-is, ISSUES 5
		tuber_mean_weight = r$MeanWt # kg
	)

	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- FALSE

	# single ridges 9 m long, 1 m apart, minisetts planted 0.3 m apart within a row (Aighewi et al. 2020)
	d$plot_area <- 9 * 1 # m2
	d$plant_density <- (r$PLST / d$plot_area) * 10000 # plants/ha

	# tubers counted per 9 m2 plot, converted to a per-ha basis the same way
	# as plant_density above; no exact terminag term for tubers, so named
	# following the existing <organ>_density (count/ha) family, ISSUES 5
	d$tuber_density <- (r$Number_of_tuber_trt / d$plot_area) * 10000 # tubers/ha

	d$yield_part <- "tubers"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE

	# no fertilizer was applied (Aighewi et al. 2020)
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- 0
	d$fertilizer_used <- FALSE
	d$fertilizer_type <- "none"

	# pivot the five wide pest/disease severity columns into shared long-format
	# fields (pest_species/pest_severity), following the reshape() pattern in
	# doi_10.25502_ec86-2t29.R; trial_id + plot_id already uniquely identify
	# each of the 54 original rows, so they double as the reshape idvar
	d <- reshape(d, direction = "long",
		varying = c("nematode_severity", "mealybug_severity",
			"scaleinsect_severity", "crazyroot_severity", "rot_severity"),
		timevar = "pest_species",
		times = c("nematode", "mealybug", "scale insect",
			"crazy root/gall", "tuber rot"),
		v.names = "pest_severity",
		idvar = c("trial_id", "plot_id"))
	rownames(d) <- NULL

	# terminag defines pest_severity as character (matches precedent in
	# doi_10.21223_P3_RBR0FG.R, doi_10.21421_D2_MHOUWW.R, ...)
	d$pest_severity <- as.character(d$pest_severity)

	# scale is constant across all pest/disease scores, see metadata CSV
	d$severity_scale <- "1-5, 1=least severe"

	carobiner::write_files(path, meta, d)
}
