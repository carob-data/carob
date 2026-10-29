# R script for "carob"

## NOTES
# N2Africa demonstration trial ODK table (wide treatments 1-16). Reshaped to one row per farm x treatment plot.
# Grain weight is kg/plot; area from harvested width (m) x number of rows x row spacing (cm).
# GPS used when present (geo_from_source=TRUE). Remaining coordinates left missing.
# Control/no-fertilizer treatments coded as N/P/K = 0. Do not drop NA yields or clip outliers.

## ISSUES
# Many field GPS values are missing; human must georeference those farms.
# Placeholder dates 1-Jan-99 set to NA. Some planting/harvest dates fail to parse.
# Plot area formula can produce extreme yields; left as calculated (not clipped).
# Fertilizer rates inferred from treatment text (P, K, Zn, S, OM) following other N2Africa scripts; confirm vs protocol.
# Treatments 13-16 may lack harvest-area columns. Crop 2 plots not processed.
# Inoculation field is farm-level, applied to all plots of that farm.
# write_files: N_fertilizer has no variation (all 0); extreme/Inf yields from bad plot area; previous_crop terms gram/green/other/vegetables; many GPS off-land or wrong country; duplicate records; NA yield and coordinates; datespans implausible. Do not clip outliers.

carob_script <- function(path) {

"
N2Africa demonstration trial

N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"

	uri <- "doi:10.25502/vhf4-rn77"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA;ICRAF;WUR",
		publication = NA,
		project = "N2Africa",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;inoculated",
		response_vars = "yield",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 45,
		carob_effort = 2
	)

	f <- ff[basename(ff) == "data_table.csv"]
	r <- read.csv(f, fill=TRUE, stringsAsFactors=FALSE)

	lat1 <- r$gps_field_device_latitude_decimal_degrees
	lat2 <- r$gps_latitude_field_decimal_degrees
	lon1 <- r$gps_field_device_longitude_decimal_degrees
	lon2 <- r$gps_longitude_field_decimal_degrees
	lat1[is.na(lat1)] <- lat2[is.na(lat1)]
	lon1[is.na(lon1)] <- lon2[is.na(lon1)]

	farm <- data.frame(
		trial_id = as.character(r$id),
		country = r$country,
		location = r$lga_district_woreda,
		site = r$sector_ward,
		latitude = lat1,
		longitude = lon1,
		planting_date = r$date_of_planting_whole_n2africa_field_date,
		harvest_date = r$date_of_final_harvest_whole_n2a_field_date,
		crop = r$legume_planted_in_the_n2africa_trial,
		previous_crop = r$crop_1_previous_season,
		inoculated = r$inoculation_n2africa_field,
		stringsAsFactors = FALSE
	)

	oldnms <- c("id", "treatmentX", "row_spacing_crop_1_plot_X_cm",
		"plant_spacing_crop_1_plot_X_cm", "grain_weight_crop_1_plot_X_kg",
		"pod_weight_groundnut_crop_1_plot_X_kg", "above_ground_biomass_weight_crop_1_plot_X_kg",
		"width_of_harvested_plot_crop_1_plot_X_m", "number_of_rows_crop_1_plot_X_nr")
	newnms <- c("trial_id", "treatment", "row_spacing", "plant_spacing", "grain_kg",
		"pod_kg", "fwy_residue", "width_m", "number_row")

	# plot 3 row-count column is missing the plot number in the CSV
	if ("number_of_rows_crop_1_plot__nr" %in% names(r) &&
			!("number_of_rows_crop_1_plot_3_nr" %in% names(r))) {
		r$number_of_rows_crop_1_plot_3_nr <- r$number_of_rows_crop_1_plot__nr
	}

	lst <- list()
	for (i in 1:16) {
		inms <- gsub("X", i, oldnms)
		if (!all(inms[1:2] %in% names(r))) next
		keep <- inms %in% names(r)
		ri <- r[, inms[keep], drop=FALSE]
		colnames(ri) <- newnms[keep]
		# add missing harvest columns as NA
		for (nm in newnms) if (!nm %in% names(ri)) ri[[nm]] <- NA
		ri <- ri[, newnms]
		lst[[length(lst)+1]] <- ri
	}
	dd <- do.call(rbind, lst)
	dd$trial_id <- as.character(dd$trial_id)
	dd <- dd[!is.na(dd$treatment) & dd$treatment != "", ]

	dd$grain_kg <- as.numeric(dd$grain_kg)
	dd$pod_kg <- as.numeric(dd$pod_kg)
	dd$fwy_residue <- as.numeric(dd$fwy_residue)
	dd$width_m <- as.numeric(dd$width_m)
	dd$row_spacing <- as.numeric(dd$row_spacing)
	dd$number_row <- as.numeric(dd$number_row)
	dd$plant_spacing <- as.numeric(dd$plant_spacing)

	# harvested area m2: width (m) * n rows * row spacing (cm)/100
	dd$area <- dd$width_m * dd$number_row * dd$row_spacing / 100
	yld <- dd$grain_kg
	i <- is.na(yld) & !is.na(dd$pod_kg)
	yld[i] <- dd$pod_kg[i]
	dd$yield <- 10000 * yld / dd$area
	dd$fwy_residue <- 10000 * dd$fwy_residue / dd$area

	d <- merge(farm, dd, by="trial_id")

	pd <- as.Date(d$planting_date, format="%d-%b-%y")
	hd <- as.Date(d$harvest_date, format="%d-%b-%y")
	d$planting_date <- as.character(pd)
	d$harvest_date <- as.character(hd)
	d$planting_date[format(pd, "%Y") == "1999"] <- NA
	d$harvest_date[format(hd, "%Y") == "1999"] <- NA

	d$country <- carobiner::fix_name(d$country, "title")
	d$location <- carobiner::fix_name(d$location, "title")
	d$site <- carobiner::fix_name(d$site, "title")

	cr <- carobiner::fix_name(d$crop, "lower")
	cr <- gsub("soya_bean", "soybean", cr)
	cr <- gsub("bush_bean", "common bean", cr)
	cr <- gsub("climbing_bean", "common bean", cr)
	cr <- gsub("faba_bean", "faba bean", cr)
	cr <- gsub("pigeon_pea", "pigeon pea", cr)
	d$crop <- cr

	pc <- carobiner::fix_name(d$previous_crop, "lower")
	pc <- gsub("soyabean|soya_bean", "soybean", pc)
	pc <- gsub("bush_bean|climbing_bean", "common bean", pc)
	pc <- gsub("sweet_potato", "sweetpotato", pc)
	pc <- gsub("bambara_bean", "bambara groundnut", pc)
	pc <- gsub("irish_potato", "potato", pc)
	pc <- gsub("pigeon_pea", "pigeon pea", pc)
	pc <- gsub("fallow", "none", pc)
	d$previous_crop <- pc

	ino <- tolower(trimws(as.character(d$inoculated)))
	d$inoculated <- ifelse(ino %in% c("y", "yes", "true"), TRUE,
		ifelse(ino %in% c("n", "no", "false"), FALSE, NA))

	d$N_fertilizer <- 0
	d$P_fertilizer <- ifelse(grepl("\\bP\\b|\\+P|P\\+|P fertilizer|SSP|TSP|DAP", d$treatment, ignore.case=TRUE) &
		!grepl("Pendo", d$treatment), 30, 0)
	d$K_fertilizer <- ifelse(grepl("\\bK\\b|\\+K|K\\+", d$treatment, ignore.case=TRUE), 20, 0)
	d$OM_used <- grepl("OM|Manure|manure|FYM", d$treatment)

	d$yield_part <- "grain"
	d$yield_part[d$crop == "groundnut"] <- "pod"

	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- !is.na(d$latitude) & !is.na(d$longitude) & d$latitude != 0
	d$latitude[d$latitude == 0] <- NA
	d$longitude[d$longitude == 0] <- NA

	d$area <- NULL
	d$grain_kg <- NULL
	d$pod_kg <- NULL
	d$width_m <- NULL
	d$number_row <- NULL

	carobiner::write_files(path, meta, d)
}
