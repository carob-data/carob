# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm legume plots in northern Nigeria, 2012. Treatments A-D are control, P fertilizer, inoculant, and P plus inoculant, where that column is filled.
# The crop column is empty. Crop is taken from the variety name: TGX/TXG soybean, Samnut groundnut, IT and related codes cowpea. Other names, including Local, stay without a crop.
# Yield uses weight_kg and area_harvested_m2. weight_grain mixes kg and mudu and was not converted.
# SSP product kg/plot to elemental P with terminag fraction 8.74%. NPK grade is unknown so nutrients stay NA when the amount is positive. "Phosphorus" is kept as recorded, without a guessed product fraction. None or amount 0 is 0 N, P and K. Groundnut yield_part is pod. A blank mineral type is left blank unless the amount is 0.
# Organic amounts with a "ton"/"tons" suffix are multiplied by 1000. SSP entered as an organic fertilizer is not treated as organic matter.
# GPS zeros set to NA. Soil n.perc treated as percent N (* 10000 = mg/kg). oc.perc is soil_SOC. Mehlich P (p_mehlich.ppm) is stored as soil_P. ecec is stored as soil_CEC.
# LGA names in action_site stored as adm2 (the district column is the word Nigeria).

## ISSUES
# Many varieties are not TGX, Samnut or IT, so crop is NA.
# Mudu grain weights were not converted (local measure, no factor in the file).
# Cow dung is sometimes in the mineral-fertilizer column; those rows are fertilizer_type none and the dung amount was not moved to OM.
# plot_size text such as "10 x 15" was not parsed; harvested m2 was used.
# Elevation includes values far above 5000 m. They were left as recorded.
# Wards in sector_ward were not mapped to adm3.
# Season 2012 is not in the terminag season list. Harvested area of 0 m2 was set to NA so fertilizer rates are not infinite.
# A few harvest rows have no match in a_general.csv. Country is Nigeria for those rows. Elevation includes 21611 m.
# SSP on a small harvested area gives P_fertilizer up to about 2331 kg/ha. That rate was left as calculated.
# Yield moisture and irrigation were not recorded.
# write_files: missing carob_contributor, yield_moisture, irrigated; treatment, variety and inoculated have NA; invalid fertilizer_type "phosphorus"; invalid season 2012; elevation and P_fertilizer out of bounds; datespan 7 harvests within 45 days of planting; high cowpea, groundnut and soybean yields; duplicate coordinates; NA yield, latitude, longitude.

## NOT PROCESSED
# a_family_composition.csv, a_livestock.csv, a_technician.csv, a_inputs.csv: household, livestock, enumerator, input purchases.
# a_general.csv: education, labour, farm size, the district value "Nigeria", sector_ward.
# b_conservation_structure.csv and b_info_site_1.csv: conservation and fertility scores.
# b_info_site_2.csv: previous-season management text; previous crops that are not a single crop name.
# c_use_of_package_2.csv: inoculant source and storage.
# c_use_of_package_3.csv: germination. crop_2 spacing is 0.
# d_cropping_calendar.csv: weeding dates and fertilizer comments.
# e_harvest.csv: weight_grain strings (mudu and kg). weight_kg was used.
# f_farmer_assessment.csv: relative scores.
# g_treatments_1.csv, g_treatments_2.csv, g_treatments_lead_farmer.csv, g_treatments_n2africa_plot.csv: training and opinions.
# h_soil_data.csv: total carbon, Cu, Mn, Fe, exchangeable acidity.
# variable_definitions.csv: dictionary.


carob_script <- function(path) {

"
N2Africa farm monitoring - Nigeria, 2012

N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries.
"

	uri <- "doi:10.25502/1zxn-yy75"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA;ICRAF;WUR",
		publication = NA,
		project = "N2Africa",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "treatment;variety;inoculated",
		response_vars = "yield",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 1
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

	parse_kg <- function(x) {
		x <- trimws(tolower(as.character(x)))
		tons <- grepl("ton", x)
		x2 <- gsub("kg", "", x)
		x2 <- gsub("tons", "", x2)
		x2 <- gsub("ton", "", x2)
		x2 <- trimws(x2)
		ok <- grepl("^[0-9.]+$", x2)
		v <- rep(NA_real_, length(x))
		v[ok] <- as.numeric(x2[ok])
		v[which(ok & tons)] <- v[which(ok & tons)] * 1000
		v
	}

	crop_of <- function(v) {
		x <- tolower(trimws(v))
		out <- rep(NA_character_, length(x))
		out[which(grepl("tgx|txg", x))] <- "soybean"
		out[which(is.na(out) & grepl("samnut", x))] <- "groundnut"
		hit <- is.na(out) & grepl("it\\s*\\d|it\\d|277-2|452-1|499-35|573-1|573-2|205-8", x)
		out[which(hit)] <- "cowpea"
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
	lat[which(lat == 0)] <- NA
	lon[which(lon == 0)] <- NA
	adm <- trimws(r0$action_site)
	adm <- gsub(" [Ll][Gg][Aa]$", "", adm)
	adm <- carobiner::fix_name(adm, "title")
	loc <- carobiner::fix_name(r0$vilage, "title")
	loc[which(loc == "")] <- NA
	d0 <- data.frame(
		trial_id = as.character(r0$farm_id),
		season = as.character(r0$season),
		country = "Nigeria",
		adm2 = adm,
		location = loc,
		latitude = lat,
		longitude = lon,
		elevation = r0$gps_altitude,
		farmer_gender = tolower(trimws(r0$gender_of_farmer)),
		geo_from_source = !is.na(lat) & !is.na(lon)
	)

	prev <- tolower(trimws(r7$main_crop_last_season))
	prev <- carobiner::replace_values(prev,
		c("cowpea", "groundnut", "maize", "soybean", "soyabean", "sorghum", "cassava", "millet", "rice"),
		c("cowpea", "groundnut", "maize", "soybean", "soybean", "sorghum", "cassava", "millet", "rice"),
		FALSE)
	known <- c("cowpea", "groundnut", "maize", "soybean", "sorghum", "cassava", "millet", "rice")
	prev[which(!prev %in% known)] <- NA
	d7 <- data.frame(trial_id = as.character(r7$farm_id), previous_crop = prev)

	ft <- tolower(trimws(r1$min_fertilizer_type))
	ft[which(ft %in% c("none", "nil", "no", "cow dung"))] <- "none"
	ft[which(ft %in% c("ssp", "sssp"))] <- "SSP"
	ft[which(ft == "npk")] <- "NPK"
	ft[which(ft == "phosphorus")] <- "phosphorus"
	om <- tolower(trimws(r1$org_fertilizer_type))
	om[is.na(om)] <- "none"
	om[which(om %in% c("ssp", "nil", "none", "no"))] <- "none"
	om[which(om %in% c("farm yard manure", "fym", "manure"))] <- "farmyard manure"
	om[which(om == "poultry manure")] <- "poultry manure"
	inoc <- toupper(trimws(r1$inoculant_used))
	d1 <- data.frame(
		trial_id = as.character(r1$farm_id),
		plot_id = as.character(r1$plot_no),
		treatment = trimws(r1$treatment),
		crop = crop_of(r1$variety),
		variety = trimws(r1$variety),
		inoculated = ifelse(inoc == "Y", TRUE, ifelse(inoc == "N", FALSE, NA)),
		OM_type = om,
		OM_kg = parse_kg(r1$org_fertilizer_amount_kg),
		fertilizer_type = ft,
		fert_kg = parse_kg(r1$min_fertiliser_amount_kg)
	)
	d1$variety[which(d1$variety == "")] <- NA
	d1$treatment[which(d1$treatment == "")] <- NA
	d1$fertilizer_type[which(!is.na(d1$fert_kg) & d1$fert_kg == 0)] <- "none"

	sr <- r3$crop_1_spacing_row_to_row
	sp <- r3$crop_1_spacing_plant_to_plant
	sr[which(sr == 0)] <- NA
	sp[which(sp == 0)] <- NA
	d3 <- data.frame(
		trial_id = as.character(r3$farm_id),
		plot_id = as.character(r3$plot_no),
		row_spacing = sr,
		plant_spacing = sp
	)

	area <- r5$area_harvested_m2
	area[which(!is.na(area) & area <= 0)] <- NA
	y <- r5$weight_kg / area * 10000
	y[which(is.na(area))] <- NA
	uns <- toupper(trimws(r5$crop_1_grain_unshelled))
	part <- ifelse(uns == "Y", "pod", "seed")
	d5h <- data.frame(
		trial_id = as.character(r5$farm_id),
		plot_id = as.character(r5$plot_no),
		plot_area = area,
		yield = y,
		yield_part = part
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
		soil_SOC = r8$oc.perc,
		soil_sand = r8$sand.perc,
		soil_silt = r8$silt.perc,
		soil_clay = r8$clay.perc,
		soil_N = r8$n.perc * 10000,
		soil_P = r8$p_mehlich.ppm,
		soil_K_exch = r8$k.cmol_per_kg,
		soil_Ca_exch = r8$ca.cmol_per_kg,
		soil_Mg_exch = r8$mg.cmol_per_kg,
		soil_Na_exch = r8$na.cmol_per_kg,
		soil_Zn = r8$zn.ppm,
		soil_CEC = r8$ecec.cmol_per_kg
	)

	d <- merge(d1, d5h, by = c("trial_id", "plot_id"), all = TRUE)
	d <- merge(d, d3, by = c("trial_id", "plot_id"), all.x = TRUE)
	d <- merge(d, d0, by = "trial_id", all.x = TRUE)
	d <- merge(d, d4, by = "trial_id", all.x = TRUE)
	d <- merge(d, d7, by = "trial_id", all.x = TRUE)
	d <- merge(d, d8, by = "trial_id", all.x = TRUE)
	d$country[which(is.na(d$country))] <- "Nigeria"

	rate <- rep(NA_real_, nrow(d))
	ok <- which(d$plot_area > 0)
	rate[ok] <- d$fert_kg[ok] / d$plot_area[ok] * 10000
	d$N_fertilizer <- NA_real_
	d$P_fertilizer <- NA_real_
	d$K_fertilizer <- NA_real_
	none <- which(d$fertilizer_type == "none")
	d$N_fertilizer[none] <- 0
	d$P_fertilizer[none] <- 0
	d$K_fertilizer[none] <- 0
	ssp <- which(d$fertilizer_type == "SSP")
	d$N_fertilizer[ssp] <- 0
	d$K_fertilizer[ssp] <- 0
	d$P_fertilizer[ssp] <- rate[ssp] * 0.0874

	d$OM_used <- !is.na(d$OM_type) & d$OM_type != "none"
	d$OM_amount <- NA_real_
	d$OM_amount[ok] <- d$OM_kg[ok] / d$plot_area[ok] * 10000
	d$OM_amount[which(d$OM_type == "none")] <- 0
	d$yield_part[which(is.na(d$yield_part) & d$crop == "groundnut")] <- "pod"
	d$yield_part[which(is.na(d$yield_part))] <- "seed"
	gn <- which(d$crop == "groundnut" & d$yield_part == "seed")
	d$yield_part[gn] <- "pod"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$fert_kg <- NULL
	d$OM_kg <- NULL

	carobiner::write_files(path, meta, d)
}
