# R script for "carob"
# license: GPL (>=3)

## ISSUES
# GPS lat/lon empty; georeference villages in Burera and Gakenke.
# crop_2 empty; maize rows treated as sole maize (may be intercrops).
# Stover weights all 0.
# mineral_fert_amount assumed kg product per plot.
# Soil chemistry present but many Mehlich columns empty; pH/SOC/texture merged.
# write_files: missing longitude/latitude/yield_moisture; 2 harvest dates >366 days after planting; common bean yield to 12500 kg/ha left as recorded; NA yield_part when crop missing.


carob_script <- function(path) {

"
N2Africa farm monitoring - Rwanda, 2012, I

N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"

	uri <- "doi:10.25502/jn3v-pr28"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA;ICRAF;WUR",
		publication = NA,
		project = "N2Africa",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety;inoculated;fertilizer_type",
		response_vars = "yield",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 60,
		carob_effort = 0.8
	)

	f0 <- ff[basename(ff) == "a_general.csv"]
	f1 <- ff[basename(ff) == "c_use_of_package_1.csv"]
	f2 <- ff[basename(ff) == "c_use_of_package_2.csv"]
	f3 <- ff[basename(ff) == "c_use_of_package_4.csv"]
	f4 <- ff[basename(ff) == "d_cropping_calendar.csv"]
	f5 <- ff[basename(ff) == "e_harvest.csv"]
	f6 <- ff[basename(ff) == "h_soil_data.csv"]
	f7 <- ff[basename(ff) == "b_info_site_2.csv"]

	r0 <- read.csv(f0, na.strings = c("", "NA"))
	r1 <- read.csv(f1, na.strings = c("", "NA"))
	r2 <- read.csv(f2, na.strings = c("", "NA"))
	r3 <- read.csv(f3, na.strings = c("", "NA"))
	r4 <- read.csv(f4, na.strings = c("", "NA"))
	r5 <- read.csv(f5, na.strings = c("", "NA"))
	r6 <- read.csv(f6, na.strings = c("", "NA"))
	r7 <- read.csv(f7, na.strings = c("", "NA"))

	d0 <- data.frame(
		trial_id = r0$farm_id,
		country = "Rwanda",
		adm2 = carobiner::fix_name(r0$district, "title"),
		adm3 = carobiner::fix_name(r0$sector_ward, "title"),
		location = carobiner::fix_name(r0$vilage, "title"),
		elevation = r0$gps_altitude,
		farmer_gender = tolower(r0$gender_of_farmer)
	)
	d0$location[d0$location == ""] <- NA

	d1 <- data.frame(
		trial_id = r1$farm_id,
		plot_area = r1$plot_size
	)

	d2 <- data.frame(
		trial_id = r2$farm_id,
		plot_id = as.character(r2$plot_no),
		crop = tolower(trimws(r2$crop_1)),
		variety = trimws(r2$variety_1),
		inoculated = toupper(r2$inoculant_used) == "Y",
		OM_type = r2$organic_fert_type,
		OM_amount = r2$organic_fert_amount,
		fertilizer_type = r2$mineral_fert_type,
		fert_amount = r2$mineral_fert_amount
	)
	d2$crop[d2$crop %in% c("bush bean", "climbing bean")] <- "common bean"
	d2$OM_type <- trimws(tolower(d2$OM_type))
	d2$OM_type <- carobiner::replace_values(d2$OM_type,
		c("farm yard manure", "compost manure"),
		c("farmyard manure", "compost"), FALSE)
	d2$OM_type[d2$OM_type == ""] <- "none"
	d2$OM_used <- d2$OM_type != "none"
	d2$fertilizer_type <- trimws(d2$fertilizer_type)
	d2$fertilizer_type <- gsub("UREA", "urea", d2$fertilizer_type)
	d2$fertilizer_type <- gsub("Urea", "urea", d2$fertilizer_type)
	d2$fertilizer_type <- gsub(",", "; ", d2$fertilizer_type)
	d2$fertilizer_type[d2$fertilizer_type == ""] <- "none"

	d3 <- data.frame(
		trial_id = r3$farm_id,
		plot_id = as.character(r3$plot_no),
		row_spacing = r3$crop_1_spacing_row_to_row,
		plant_spacing = r3$crop_1_spacing_plant_to_plant
	)

	# Calendar is long (one row per activity)
	plant <- r4[r4$activity == "Date of planting", ]
	harv_b <- r4[r4$activity == "Date of harvest of bush beans", ]
	harv_m <- r4[r4$activity == "Date of harvest of maize", ]
	ymd <- function(dd, mm, yyyy) {
		oky <- !is.na(yyyy) & yyyy > 1900
		ok <- oky & !is.na(mm) & mm > 0 & mm <= 12 & !is.na(dd) & dd > 0 & dd <= 31
		out <- rep(NA_character_, length(yyyy))
		out[oky] <- as.character(yyyy[oky])
		out[ok] <- sprintf("%04d-%02d-%02d", yyyy[ok], mm[ok], dd[ok])
		out
	}
	dP <- data.frame(trial_id = plant$farm_id, planting_date = ymd(plant$dd, plant$mm, plant$yyyy))
	dHb <- data.frame(trial_id = harv_b$farm_id, harvest_bean = ymd(harv_b$dd, harv_b$mm, harv_b$yyyy))
	dHm <- data.frame(trial_id = harv_m$farm_id, harvest_maize = ymd(harv_m$dd, harv_m$mm, harv_m$yyyy))

	d5 <- data.frame(
		trial_id = r5$farm_id,
		plot_id = as.character(r5$plot_no),
		area_h = r5$crop_1_area_harvested,
		yield = r5$crop_1_weight_grain / r5$crop_1_area_harvested * 10000,
		plant_density = r5$crop_1_plants_no / r5$crop_1_area_harvested * 10000
	)
	d5$plant_density[d5$plant_density == 0] <- NA

	d6 <- data.frame(
		trial_id = r6$farm_id,
		soil_pH = r6$ph,
		soil_SOC = r6$oc.perc,
		soil_N = r6$n.perc * 10000,
		soil_sand = r6$sand.perc,
		soil_silt = r6$silt.perc,
		soil_clay = r6$clay.perc,
		soil_P = r6$p_olsen.ppm,
		soil_Mg_exch = r6$mg.cmol_per_kg
	)
	# n.perc is %; terminag soil_N is mg/kg (~ppm). % * 10000 = mg/kg. Check: 0.20% = 2000 mg/kg.

	d7 <- data.frame(
		trial_id = r7$farm_id,
		previous_crop = tolower(trimws(r7$main_crop_last_season))
	)
	d7$previous_crop <- carobiner::replace_values(d7$previous_crop,
		c("bean", "beans"), c("common bean", "common bean"), FALSE)

	d <- merge(d2, d5, by = c("trial_id", "plot_id"), all = TRUE)
	d <- merge(d, d3, by = c("trial_id", "plot_id"), all.x = TRUE)
	d <- merge(d, d1, by = "trial_id", all.x = TRUE)
	d <- merge(d, d0, by = "trial_id", all.x = TRUE)
	d <- merge(d, dP, by = "trial_id", all.x = TRUE)
	d <- merge(d, dHb, by = "trial_id", all.x = TRUE)
	d <- merge(d, dHm, by = "trial_id", all.x = TRUE)
	d$harvest_date <- d$harvest_bean
	i <- !is.na(d$crop) & d$crop == "maize"
	d$harvest_date[i] <- d$harvest_maize[i]
	d$harvest_bean <- NULL
	d$harvest_maize <- NULL
	d <- merge(d, d6, by = "trial_id", all.x = TRUE)
	d <- merge(d, d7, by = "trial_id", all.x = TRUE)

	area <- d$area_h
	area[is.na(area) | area == 0] <- d$plot_area[is.na(area) | area == 0]
	d$plot_area <- area

	d$N_fertilizer <- 0
	d$P_fertilizer <- 0
	d$K_fertilizer <- 0
	ft <- d$fertilizer_type
	am <- d$fert_amount / d$plot_area * 10000
	i <- grepl("DAP", ft) & !is.na(am)
	d$N_fertilizer[i] <- d$N_fertilizer[i] + am[i] * 0.18
	d$P_fertilizer[i] <- d$P_fertilizer[i] + am[i] * 0.201
	j <- grepl("urea", ft) & !is.na(am)
	d$N_fertilizer[j] <- d$N_fertilizer[j] + am[j] * 0.46
	d$OM_amount <- d$OM_amount / d$plot_area * 10000

	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$yield_part <- ifelse(d$crop == "maize", "grain", "seed")
	d$geo_from_source <- FALSE

	d$area_h <- NULL
	d$fert_amount <- NULL

	carobiner::write_files(path, meta, d)
}
