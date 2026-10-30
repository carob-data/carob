# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm legume plots in several Ethiopian woredas, 2013, with soil analyses for some farms.
# Bush bean is stored as common bean with variety_type "bush bean". Chick pea mapped to chickpea.
# area_harvested_m2 is m2. weight_kg is 0 throughout; weight_grain is used as kg (values such as 1-3 kg on a few m2). About 80 weights are hundreds to thousands on 4-8 m2 and give impossible yields if they are kg; they were not recoded as grams.
# DAP product kg/plot to elemental N (18%) and P (20.1%) using the harvested area. Compost amount is kg on the plot.
# Field GPS kept. Where latitude is above 20 and longitude is below 20 the pair is swapped (Ethiopia lon is about 33-48).
# Soil: pH, texture, Olsen P, exchangeable cations, CEC, Zn. n.perc treated as percent N and multiplied by 10000 to mg/kg. Total carbon (tc) is not soil organic carbon and was not mapped to soil_SOC.

## ISSUES
# Confirm weight_grain unit. Median yield is a few t/ha, but many records exceed 20 t/ha.
# plot_size is text (100m2, 10x10, 3m*4m) and was not used; harvested area was used for yield and fertilizer rate.
# Some GPS points are still outside Ethiopia after the swap, or are missing.
# Woreda was stored in adm2; GADM Ethiopia often has woreda at adm3. Action site was not assigned to an admin level.
# Season value 2013 is not in the terminag season list.
# "Field pea" was mapped to pea. Yield moisture and irrigation were not recorded.
# write_files: missing carob_contributor, yield_moisture, irrigated; variety and inoculated have NA; yield up to about 4e6; invalid season 2013; 8 harvests within 45 days of planting; high faba bean yield; duplicate coordinates; NA yield, latitude, longitude.

## NOT PROCESSED
# a_family_composition.csv, a_livestock.csv, a_technician.csv: household and enumerator fields.
# a_inputs.csv: household input purchases.
# a_general.csv: education, labour, farm size, legume flags, action_site (see ISSUES).
# b_conservation_structure.csv: conservation_method.
# b_info_site_1.csv: fertility scores.
# b_info_site_2.csv: previous-season products; previous crops that are not a single crop name.
# c_use_of_package_2.csv: inoculant storage.
# c_use_of_package_3.csv: germination. crop_2 is empty.
# d_cropping_calendar.csv: weeding and fertilizer dates.
# f_farmer_assessment.csv: relative scores.
# g_treatments_*.csv: training text (several files have no rows) and plot opinions.
# h_soil_data.csv: tc (total carbon), Cu, Mn, Fe, exchangeable acidity.
# variable_definitions.csv: dictionary.


carob_script <- function(path) {

"
N2Africa farm monitoring - Ethiopia, 2013

N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"

	uri <- "doi:10.25502/m5h6-cj32/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA;ICRAF;WUR",
		publication = NA,
		project = "N2Africa",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety;inoculated",
		response_vars = "yield",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 65,
		carob_effort = 0.9
	)

	ymd <- function(dd, mm, yyyy) {
		dd <- as.numeric(dd)
		mm <- as.numeric(mm)
		yyyy <- as.numeric(yyyy)
		dim <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
		leap <- !is.na(yyyy) & (yyyy %% 4 == 0) & ((yyyy %% 100 != 0) | (yyyy %% 400 == 0))
		ok_y <- !is.na(yyyy) & yyyy > 1900 & yyyy < 2100
		ok_m <- ok_y & !is.na(mm) & mm >= 1 & mm <= 12
		maxd <- rep(NA_real_, length(mm))
		maxd[ok_m] <- dim[mm[ok_m]]
		feb <- which(ok_m & mm == 2 & leap)
		maxd[feb] <- 29
		ok <- which(ok_m & !is.na(dd) & dd >= 1 & dd <= maxd)
		out <- rep(NA_character_, length(yyyy))
		out[which(ok_y)] <- as.character(yyyy[which(ok_y)])
		out[ok] <- sprintf("%04d-%02d-%02d", yyyy[ok], mm[ok], dd[ok])
		out
	}

	f0 <- ff[basename(ff) == "a_general.csv"]
	f1 <- ff[basename(ff) == "c_use_of_package_1.csv"]
	f3 <- ff[basename(ff) == "c_use_of_package_3.csv"]
	f4 <- ff[basename(ff) == "d_cropping_calendar.csv"]
	f5 <- ff[basename(ff) == "e_harvest.csv"]
	f7 <- ff[basename(ff) == "b_info_site_2.csv"]
	f8 <- ff[basename(ff) == "h_soil_data.csv"]

	r0 <- read.csv(f0, na.strings = c("", "NA"))
	r1 <- read.csv(f1, na.strings = c("", "NA"))
	r3 <- read.csv(f3, na.strings = c("", "NA"))
	r4 <- read.csv(f4, na.strings = c("", "NA"))
	r5 <- read.csv(f5, na.strings = c("", "NA"))
	r7 <- read.csv(f7, na.strings = c("", "NA"))
	r8 <- read.csv(f8, na.strings = c("", "NA"))

	lat <- r0$gps_latitude
	lon <- r0$gps_longitude
	swap <- which(!is.na(lat) & !is.na(lon) & lat > 20 & lon < 20)
	tmp <- lat[swap]
	lat[swap] <- lon[swap]
	lon[swap] <- tmp
	lat[which(lat == 0)] <- NA
	lon[which(lon == 0)] <- NA
	loc <- carobiner::fix_name(r0$vilage, "title")
	loc[which(loc == "")] <- NA
	d0 <- data.frame(
		trial_id = as.character(r0$farm_id),
		season = as.character(r0$season),
		country = "Ethiopia",
		adm2 = carobiner::fix_name(r0$district, "title"),
		location = loc,
		latitude = lat,
		longitude = lon,
		elevation = r0$gps_altitude,
		farmer_gender = tolower(trimws(r0$gender_of_farmer)),
		geo_from_source = !is.na(lat) & !is.na(lon)
	)

	prev <- tolower(trimws(r7$main_crop_last_season))
	prev <- carobiner::replace_values(prev,
		c("barley", "bread wheat", "common bean", "emmer wheat", "field pea", "finger millet", "maize", "potato", "sesame", "sorghum", "sweet potato", "tef", "teff", "wheat", "faba bean"),
		c("barley", "wheat", "common bean", "wheat", "pea", "finger millet", "maize", "potato", "sesame", "sorghum", "sweetpotato", "teff", "teff", "wheat", "faba bean"),
		FALSE)
	known <- c("barley", "wheat", "common bean", "pea", "finger millet", "maize", "potato", "sesame", "sorghum", "sweetpotato", "teff", "faba bean")
	prev[which(!prev %in% known)] <- NA
	d7 <- data.frame(trial_id = as.character(r7$farm_id), previous_crop = prev)

	area <- r5$area_harvested_m2
	yield <- r5$weight_grain / area * 10000
	yield[which(is.na(area) | area <= 0 | is.na(r5$weight_grain))] <- NA
	part <- ifelse(toupper(trimws(r5$crop_1_grain_unshelled)) == "Y", "pod", "seed")
	d5 <- data.frame(
		trial_id = as.character(r5$farm_id),
		plot_id = as.character(r5$plot_no),
		plot_area = area,
		yield = yield,
		yield_part = part
	)

	raw <- tolower(trimws(r1$crop))
	vtype <- ifelse(raw == "bush bean", "bush bean", NA_character_)
	crop <- raw
	crop[which(crop %in% c("bush bean", "common bean"))] <- "common bean"
	crop[which(crop == "chick pea")] <- "chickpea"
	crop[which(crop == "")] <- NA
	om <- tolower(trimws(r1$org_fertilizer_type))
	om[is.na(om)] <- "none"
	om[which(om == "compost")] <- "compost"
	om[which(om %in% c("manure", "no", "none"))] <- ifelse(om[which(om %in% c("manure", "no", "none"))] == "manure", "farmyard manure", "none")
	ft <- tolower(trimws(r1$min_fertilizer_type))
	ft[is.na(ft)] <- "none"
	ft[which(ft == "dap")] <- "DAP"
	ft[which(ft %in% c("no", "none", "nil"))] <- "none"
	inoc <- toupper(trimws(r1$inoculant_used))
	d1 <- data.frame(
		trial_id = as.character(r1$farm_id),
		plot_id = as.character(r1$plot_no),
		crop = crop,
		variety_type = vtype,
		variety = trimws(r1$variety),
		inoculated = ifelse(inoc == "Y", TRUE, ifelse(inoc == "N", FALSE, NA)),
		OM_type = om,
		OM_kg = as.numeric(r1$org_fertilizer_amount_kg),
		fertilizer_type = ft,
		fert_kg = as.numeric(r1$min_fertiliser_amount_kg)
	)
	d1$variety[which(d1$variety == "")] <- NA

	sp_r <- r3$crop_1_spacing_row_to_row
	sp_p <- r3$crop_1_spacing_plant_to_plant
	sp_r[which(sp_r == 0)] <- NA
	sp_p[which(sp_p == 0)] <- NA
	d3 <- data.frame(
		trial_id = as.character(r3$farm_id),
		plot_id = as.character(r3$plot_no),
		row_spacing = sp_r,
		plant_spacing = sp_p
	)

	r4 <- r4[!duplicated(r4$farm_id), ]
	d4 <- data.frame(
		trial_id = as.character(r4$farm_id),
		planting_date = ymd(r4$date_planting_dd, r4$date_planting_mm, r4$date_planting_yyyy),
		harvest_date = ymd(r4$date_harvest_dd, r4$date_harvest_mm, r4$date_harvest_yyyy)
	)

	d8 <- data.frame(
		trial_id = as.character(r8$farm_id),
		soil_pH = r8$ph,
		soil_sand = r8$sand.perc,
		soil_silt = r8$silt.perc,
		soil_clay = r8$clay.perc,
		soil_N = r8$n.perc * 10000,
		soil_P = r8$p_olsen.ppm,
		soil_K_exch = r8$k.cmol_per_kg,
		soil_Ca_exch = r8$ca.cmol_per_kg,
		soil_Mg_exch = r8$mg.cmol_per_kg,
		soil_Na_exch = r8$na.cmol_per_kg,
		soil_Zn = r8$zn.ppm,
		soil_CEC = r8$cec.cmol_per_kg
	)

	d <- merge(d1, d5, by = c("trial_id", "plot_id"), all = TRUE)
	d <- merge(d, d3, by = c("trial_id", "plot_id"), all.x = TRUE)
	d <- merge(d, d0, by = "trial_id", all.x = TRUE)
	d <- merge(d, d4, by = "trial_id", all.x = TRUE)
	d <- merge(d, d7, by = "trial_id", all.x = TRUE)
	d <- merge(d, d8, by = "trial_id", all.x = TRUE)

	rate <- d$fert_kg / d$plot_area * 10000
	d$N_fertilizer <- NA_real_
	d$P_fertilizer <- NA_real_
	d$K_fertilizer <- NA_real_
	dap <- which(d$fertilizer_type == "DAP")
	d$N_fertilizer[dap] <- rate[dap] * 0.18
	d$P_fertilizer[dap] <- rate[dap] * 0.201
	d$K_fertilizer[dap] <- 0
	none <- which(d$fertilizer_type == "none")
	d$N_fertilizer[none] <- 0
	d$P_fertilizer[none] <- 0
	d$K_fertilizer[none] <- 0

	d$OM_used <- !is.na(d$OM_type) & d$OM_type != "none"
	d$OM_amount <- d$OM_kg / d$plot_area * 10000
	d$OM_amount[which(d$OM_type == "none")] <- 0
	d$yield_part[is.na(d$yield_part)] <- "seed"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$fert_kg <- NULL
	d$OM_kg <- NULL

	carobiner::write_files(path, meta, d)
}
