# R script for "carob"
# license: GPL (>=3)

## NOTES
# Plot questionnaires for wheat fields in Punjab, India, rabi 2021 and rabi 2022. Country and state are the geographic coverage, and the title names Punjab.
# Plot size is in acres and is stored as hectares. 2021 uses the largest wheat plot. 2022 uses the plot size column.
# 2021 coordinates are the plot-centre GPS. 2022 coordinates are the GPS reading inside the plot. A stored 0 is treated as missing.
# Kharif crop codes follow the code book. Basmati and coarse rice are both rice. Codes 0 and 23 are not in the code book.
# 2022 tillage labels: CT is conventional, ZT is zero tillage (none). SS is a super seeder and is not a land-preparation term, so land_prep_method is left missing for those plots.
# 2022 wheat sowing is a day and a month. The covered season is October-November 2022.
# Soil codes are sand, loam, and clay. Irrigation count and source are on the 2021 sheet. Source code 0 (rain only) does not occur.

## ISSUES
# 2022 interviews have no farmer id, so hhid is missing on those rows.
# A stored latitude or longitude of 0 is missing.
# write_files: unknown variable: interview_date_ (date?). unknown variables: used_happy_seeder_, tillage_category_, tillage_number_. NA detected: hhid. contributor:

## NEW VARIABLES
# tillage_category_: 2022 wheat tillage and residue label, as recorded.
# tillage_number_: number of tillage passes recorded for wheat in 2022.
# used_happy_seeder_: 2021 plot used a turbo Happy Seeder (1 yes, 0 no).
# interview_date_: date of the interview.

## NOT PROCESSED
# Questionnaires and household fields (age, gender, education, religion, caste, household size).
# Farmer names, phone numbers, enumerator names, and comments.
# Household land totals, fallow area, rice area, number of wheat plots, and distance to the plot.
# Seed source, spring crops, road access, KVK, cooperatives, tube wells, and water-table depth.
# 2021 machinery flags other than the Happy Seeder, residue burning outside the plot, and laser levelling.
# 2022 recall of 2021 tillage, residue condition notes, and whether the plot looked ploughed.
# Corner GPS, altitude, and accuracy. The centre or inside-plot reading is used.


carob_script <- function(path) {

"
The intensive, irrigated rice-wheat systems of the northwestern Indo-Gangetic Plains are associated with the widespread burning of excess rice residue. Plot-level datasets were constructed for the 2021 and 2022 rabi seasons to identify tillage types and residue burning, as groundtruth for remote sensing. The two rounds are not the same farmers, and plot selection was not random.
"

	uri <- "hdl:11529/10548905"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "SPIA",
		design = NA,
		data_type = "survey",
		response_vars = "field_size",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.7
	)

	acre <- 0.4046863
	num <- function(z) {
		if (is.null(z)) return(NA_real_)
		if (is.numeric(z)) return(z)
		z <- trimws(as.character(z))
		z[z %in% c("", "NA", "n/a", "N/A")] <- NA_character_
		z[!is.na(z) & grepl("[[:alpha:]]", z)] <- NA_character_
		as.numeric(z)
	}
	chr <- function(z) {
		z <- trimws(as.character(z))
		z[!nzchar(z)] <- NA_character_
		z
	}
	as_date <- function(z) {
		if (inherits(z, "Date") || inherits(z, "POSIXt")) return(as.character(as.Date(z)))
		z <- trimws(as.character(z))
		z[!nzchar(z)] <- NA_character_
		out <- rep(NA_character_, length(z))
		iso <- !is.na(z) & nchar(z) >= 10 & substr(z, 5, 5) == "-"
		out[iso] <- substr(z[iso], 1, 10)
		out
	}
	place <- function(z) {
		z <- gsub("_", " ", chr(z))
		carobiner::fix_name(z, "title")
	}
	kharif <- function(z) {
		x <- as.integer(num(z))
		out <- rep(NA_character_, length(x))
		out[x %in% c(1, 2)] <- "rice"
		out[x == 3] <- "wheat"
		out[x == 4] <- "maize"
		out[x == 5] <- "sorghum"
		out[x == 8] <- "mustard"
		out[x == 9] <- "soybean"
		out[x == 10] <- "groundnut"
		out[x == 18] <- "potato"
		out
	}
	yesno <- function(z) {
		x <- num(z)
		out <- rep(NA, length(x))
		out[x == 1] <- TRUE
		out[x == 0] <- FALSE
		out
	}

	f1 <- ff[grepl("2021", basename(ff)) & grepl("xlsx$", basename(ff))]
	a <- carobiner::read.excel(f1, sheet="Main sheet")
	names(a) <- trimws(names(a))
	lat1 <- num(a$Plot_GPS_centre_gps_Lat)
	lon1 <- num(a$Plot_GPS_center_gps_Long)
	lat1[lat1 == 0] <- NA_real_
	lon1[lon1 == 0] <- NA_real_
	soil <- c("1"="sand", "2"="loam", "3"="clay")
	st <- soil[as.character(as.integer(num(a$Soil_type)))]
	d1 <- data.frame(
		country = "India",
		adm1 = "Punjab",
		adm2 = place(a$District_name),
		location = place(a$Village_name),
		hhid = chr(a$Farmer_id),
		trial_id = paste0("2021-", chr(a$Farmer_id)),
		crop = "wheat",
		variety = chr(a$Wheat_var_name),
		planting_date = as_date(a$Wheat_sowing_date),
		interview_date_ = as_date(a$Interview_date),
		field_size = num(a$Large_wheat_Plot) * acre,
		previous_crop = kharif(a$Kha_crop_name),
		previous_crop_burnt = yesno(a$Burnt_kha_resi_in_Plot),
		residue_prevcrop_used = yesno(a$Plot_crop_residue_mulching),
		used_happy_seeder_ = yesno(a$Plot_used_hs),
		irrigation_number = as.integer(num(a$Num_irri_wheat)),
		irrigated = TRUE,
		soil_texture = unname(st),
		latitude = lat1,
		longitude = lon1,
		geo_from_source = !is.na(lat1) & !is.na(lon1),
		on_farm = TRUE,
		is_survey = TRUE
	)

	f2 <- ff[grepl("2022", basename(ff)) & grepl("xlsx$", basename(ff))]
	b <- carobiner::read.excel(f2, sheet="Main_data")
	names(b) <- trimws(names(b))
	lat2 <- num(b$Inside_plot_GPS_Lat)
	lon2 <- num(b$Inside_plot_GPS_Long)
	lat2[lat2 == 0] <- NA_real_
	lon2[lon2 == 0] <- NA_real_
	day <- as.integer(num(b[["Wheat _Sowing_ Date"]]))
	mon <- c(Oct=10, Nov=11)
	mm <- mon[chr(b[["Wheat _Sowing_ Month"]])]
	when <- rep(NA_character_, nrow(b))
	ok <- !is.na(day) & !is.na(mm) & day >= 1 & day <= 31
	when[ok] <- as.character(as.Date(sprintf("2022-%02d-%02d", mm[ok], day[ok])))
	till <- chr(b[["Till_ Category _Wheat _2022"]])
	pref <- sub("_.*$", "", till)
	lp <- rep(NA_character_, length(till))
	lp[pref %in% "CT"] <- "conventional"
	lp[pref %in% "ZT"] <- "none"
	resi <- c(
		"cat-1_Most_(>75%)"="most (>75%)",
		"cat-2_Half"="half",
		"cat-3_Less_than_half"="less than half"
	)
	d2 <- data.frame(
		country = "India",
		adm1 = "Punjab",
		adm2 = place(b$District_name),
		location = place(b$Village_name),
		trial_id = paste0("2022-", chr(b$Interview_id_new)),
		crop = "wheat",
		planting_date = when,
		interview_date_ = as_date(b$Date_interview),
		field_size = num(b[["Plot_ size"]]) * acre,
		previous_crop = kharif(b[["Kharif_ Crop"]]),
		land_prep_method = lp,
		tillage_category_ = gsub("_", " ", till),
		tillage_number_ = num(b[["Num_ of _tillage_ 2022"]]),
		previous_crop_residue_management = unname(resi[chr(b[["Resi_ percentag_ Rabi _2022"]])]),
		latitude = lat2,
		longitude = lon2,
		geo_from_source = !is.na(lat2) & !is.na(lon2),
		on_farm = TRUE,
		is_survey = TRUE
	)
	blank <- is.na(d2$adm2) & is.na(d2$location) & is.na(d2$latitude) & is.na(d2$field_size)
	d2 <- d2[!blank, ]

	d <- carobiner::bindr(d1, d2)
	d$geo_from_source[is.na(d$latitude) | is.na(d$longitude)] <- FALSE

	carobiner::write_files(path, meta, d)
}
