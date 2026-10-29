# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Harvested plot area not recorded; grain weights are kg/plot so yield kg/ha cannot be computed.
# Fertilizer product amounts not recorded (only type); N/P/K omitted.
# GPS often 1 decimal degree and sometimes in the wrong country; 538 records missing coordinates.
# Plot 1/2/3 assumed n2africa / own_legume / control (variable_definitions plot index).
# farm_id is often NOT_KNOWN; trial_id uses farm_id_correction when present.
# write_files: missing yield, yield_moisture, N/P/K; unknown grain_weight_plot_ / seed_amount_plot_; NA fertilizer_type/country/lat/lon; coords in Uganda/Ethiopia.

## NEW VARIABLES
# grain_weight_plot_ (kg): shelled grain harvested from the plot (area unknown)
# seed_amount_plot_ (kg): seed planted in the plot (area unknown)


carob_script <- function(path) {

"
N2Africa focal adapt trial, 2015, II

N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"

	uri <- "doi:10.25502/ejjm-hq60"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA;ICRAF;WUR",
		publication = NA,
		project = "N2Africa",
		design = "on-farm demonstration: N2Africa plot, farmer legume plot, control",
		data_type = "on-farm experiment",
		treatment_vars = "treatment;inoculated;fertilizer_type",
		response_vars = "grain_weight_plot_",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 45,
		carob_effort = 0.7
	)

	f <- ff[basename(ff) == "data_table.csv"]
	r <- read.csv(f, na.strings = c("", "NA", "NOT_KNOWN"))

	to_date <- function(x) {
		x <- trimws(x)
		as.character(as.Date(x, "%d-%b-%y"))
	}

	crop <- trimws(tolower(r$xxx_1))
	crop <- carobiner::replace_values(crop,
		c("bush_bean", "climbing_bean", "soya_bean", "faba_bean"),
		c("common bean", "common bean", "soybean", "faba bean")
	)

	tid <- trimws(r$farm_id_correction)
	i <- is.na(tid) | tid == ""
	tid[i] <- trimws(r$farm_id[i])
	i <- is.na(tid) | tid == ""
	tid[i] <- r$id[i]

	base <- data.frame(
		country = carobiner::fix_name(r$country, "title"),
		location = r$place,
		latitude = r$gps_homestead_device_latitude_decimal_degrees,
		longitude = r$gps_homestead_device_longitude_decimal_degrees,
		elevation = r$gps_homestead_device_altitude_m,
		geo_from_source = !is.na(r$gps_homestead_device_latitude_decimal_degrees),
		trial_id = tid,
		crop = crop,
		planting_date = to_date(r$date_of_planting_1_date),
		harvest_date = to_date(r$date_of_planting_2_date),
		farmer_gender = ifelse(r$hh_head_gender == 0, "male",
			ifelse(r$hh_head_gender == 1, "female", as.character(NA)))
	)
	# date_of_planting_2 is a second calendar event, not harvest; drop it
	base$harvest_date <- as.character(NA)

	# Three experimental plots per farm
	p1 <- data.frame(
		treatment = "n2africa",
		fertilizer_type = r$type_of_fertilizer_n2africa_plot,
		inoculated = grepl("inoculant", r$inputs_n2a_plot),
		irrigated = grepl("irrigation", r$inputs_n2a_plot),
		grain_weight_plot_ = r$grain_weight_crop_1_plot_1_kg,
		seed_amount_plot_ = r$amount_of_seed_planted_crop_1_plot_1_kg
	)
	p2 <- data.frame(
		treatment = "own_legume",
		fertilizer_type = r$type_of_fertilizer_own_legume_plot,
		inoculated = grepl("inoculant", r$inputs_own_legume_plot),
		irrigated = grepl("irrigation", r$inputs_own_legume_plot),
		grain_weight_plot_ = r$grain_weight_crop_1_plot_2_kg,
		seed_amount_plot_ = r$amount_of_seed_planted_crop_1_plot_2_kg
	)
	p3 <- data.frame(
		treatment = "control",
		fertilizer_type = r$type_of_fertilizer_control_plot,
		inoculated = grepl("inoculant", r$inputs_control_plot),
		irrigated = grepl("irrigation", r$inputs_control_plot),
		grain_weight_plot_ = r$grain_weight_crop_1_plot_3_kg,
		seed_amount_plot_ = r$amount_of_seed_planted_crop_1_plot_3_kg
	)

	d <- rbind(
		cbind(base, p1),
		cbind(base, p2),
		cbind(base, p3)
	)

	# Fertilizer listed as space-separated products; drop "None"/"other"
	ft <- strsplit(trimws(d$fertilizer_type), " +")
	d$fertilizer_type <- vapply(ft, function(p) {
		if (length(p) == 1 && is.na(p)) return(NA_character_)
		p <- p[!is.na(p) & p != "" & !tolower(p) %in% c("none", "other")]
		p[p == "Urea"] <- "urea"
		if (length(p) == 0) return("none")
		paste(p, collapse = "; ")
	}, character(1))

	d$country[d$country == ""] <- NA
	d$location[d$location == ""] <- NA
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$yield_part <- ifelse(d$crop == "groundnut", "pod", "seed")
	d$yield_part[is.na(d$crop)] <- NA

	# Plots with neither grain nor seed were not planted
	keep <- !is.na(d$grain_weight_plot_) | !is.na(d$seed_amount_plot_)
	d <- d[keep, ]
	d <- unique(d)

	carobiner::write_files(path, meta, d)
}
