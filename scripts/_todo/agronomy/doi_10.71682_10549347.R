# R script for "carob"
# license: GPL (>=3)

## NOTES
# Weather and SoilGrids columns in the clean tables are secondary/modelled; omitted.
# Planting/harvest stored as day-of-year; mapped to 2022/2023 (season 2022-2023).
# Two soybean uuids not in raw survey so GPS/variety missing (used means for the location).

## NOT PROCESSED
# EiA_2022_survey_2025-09-26_v0.rds: same table as the file of this name that is read.

carob_script <- function(path) {

"
Maize and soybean farming practices and yields during the 2022-2023 season in Malawi, Mozambique and Zambia

This dataset describes farming practices in soybean and maize monocrop fields in Malawi, Mozambique and Zambia for the 2022-2023 growing season. Crop cuts were carried out on the fields of farmers participating in the EiA use case, as well as neighbouring farms and farms in non-implementing communities for representativeness. Data are provided as Excel spreadsheets and an .rds file (compatible with R 4.4.1). There is one table with the raw data (as recorded through KoboCollect) and one with the cleaned data used for the related manuscript. Survey data are reproduced as closely as possible to how they were recorded. Secondary data were used as indicated in the codebook.
"

	uri <- "doi:10.71682/10549347"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
	 carob_contributor = "Royette Santos",
		data_organization = "CIMMYT;IITA;ICRAF;ZARI;IIAM",
		publication = "doi:10.1016/j.agsy.2026.104740",
		project = "Excellence in Agronomy",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "yield",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 75,
		carob_effort = 0.7
	)

	f <- ff[basename(ff) == "EiA_2022_survey_2025-09-26_v0.xlsx"]
	r1 <- carobiner::read.excel(f, sheet = "maize_clean_data")
	r2 <- carobiner::read.excel(f, sheet = "soybean_clean_data")
	r3 <- carobiner::read.excel(f, sheet = "raw_survey_data")

	# Codebook: planting/harvest are julian day. Season is 2022-2023.
	doy_date <- function(doy) {
		y <- ifelse(doy >= 200, 2022, 2023)
		as.character(as.Date(doy - 1, origin = paste0(y, "-01-01")))
	}

	geo <- data.frame(
		X_uuid = r3$X_uuid,
		latitude = r3[["_field_gps_latitude"]],
		longitude = r3[["_field_gps_longitude"]],
		elevation = r3[["_field_gps_altitude"]],
		variety = r3$variety_name,
		adm1 = carobiner::fix_name(r3$province, "title")
	)
	geo <- geo[!duplicated(geo$X_uuid), ]

	# Crop-cut protocol: Chiduwa et al. (2026), Section 2.2.
	# Three quadrats were sampled per field.
	# Maize quadrat size: 3 m x 2 m (6 m2 each).
	# Maize grain yield was expressed on a dry-matter basis.
	
	d1 <- data.frame(
		record_id = r1$X_uuid,
		country = r1$country,
		adm2 = carobiner::fix_name(r1$district, "title"),
		location = r1$epa,
		crop = "maize",
		field_size = r1$plot_geotraced_area_m2,
		yield = r1$maize_yield_kgha,
		plant_density = r1$maize_plantpop_plha,
		seed_rate = r1$seed_kgha,
		variety_type = r1$seed_type,
		N_fertilizer = r1$nfert_kgha,
		# codebook: P2O5 and K2O
		P_fertilizer = r1$pfert_kgha / 2.29,
		K_fertilizer = r1$kfert_kgha / 1.2051,
		OM_used = r1$orgfert_yn == "y",
		planting_date = doy_date(r1$date_planting),
		harvest_date = doy_date(r1$date_harvest),
		weeding_times = as.integer(rowSums(!is.na(cbind(r1$weeding_1st_das, r1$weeding_2nd_das)))),
		yield_part = "grain",
		yield_moisture = 0
	)
	
	# Publication: Chiduwa et al. (2026), Section 3.2, Table 1.
# No fertilizer application was reported for soybean.
# N, P, and K are provisionally coded as zero.
# Raw survey contains a few possible fertilizer inputs
# that require verification before submission.
	
	# Crop-cut protocol: Chiduwa et al. (2026), Section 2.2.
	# Three quadrats were sampled per field.
	# Soybean quadrat size: 2 m x 2 m (4 m2 each).
	# Soybean grain yield was based on air-dried grain weight,
	# without further moisture adjustment.

	# Chiduwa et al. (2026), Section 3.2, Table 1.
	# No fertilizer application was reported for soybean.
	# N, P, and K are provisionally coded as zero.
	# Some raw survey entries indicate possible fertilizer inputs
	# that require verification before submission.
	#
	# Crop-cut protocol: Three quadrats of 2 m x 2 m per field.
	# Soybean yield was based on air-dried grain without
	# further moisture adjustment.
	
	d2 <- data.frame(
	  record_id = r2$X_uuid,
	  country = r2$country,
	  adm2 = carobiner::fix_name(r2$district, "title"),
	  location = r2$epa,
	  crop = "soybean",
	  yield = r2$soy_yield_kgha,
	  dmy_total = r2$soy_biomass_kgha,
	  seed_rate = r2$seed_kgha,
	  variety_type = r2$seed_type,
	  N_fertilizer = 0,
	  P_fertilizer = 0,
	  K_fertilizer = 0,
	  OM_used = r2$orgfert_yn == "y",
	  planting_date = doy_date(r2$date_planting),
	  harvest_date = doy_date(r2$date_harvest),
	  weeding_times = as.integer(rowSums(!is.na(cbind(
	    r2$weeding_1st_das,
	    r2$weeding_2nd_das
	  )))),
	  yield_part = "seed",
	  yield_moisture = NA_real_, # Soybean yield based on air-dried grain weight.
	  # No further moisture adjustment was applied.
	  # Reported as fresh-matter yield in Chiduwa et al. (2026).
	  yield_isfresh = TRUE
	)
	
	d <- carobiner::bindr(d1, d2)
	d <- merge(d, geo, by.x = "record_id", by.y = "X_uuid", all.x = TRUE)
    d$location <- carobiner::fix_name(d$location, "title")
	## this reveals a lot of location spelling variants that ought to be fixed:
    g <- aggregate(d[, c("longitude", "latitude")], d[, c("country", "adm2", "location")], mean, na.rm=TRUE)
#For example, this is all probably the same location name:
#50 Mozambique  Angonia         Mangan  34.36000 -14.58000
#51 Mozambique  Angonia        Mangane  34.30905 -14.59381
#52 Mozambique  Angonia        Mangâne  34.28000 -14.60000
#53 Mozambique  Angonia        Mangani  34.28609 -14.57522
#54 Mozambique  Angonia        Mangano  34.36000 -14.58000
#55 Mozambique  Angonia        Mangany  34.36233 -14.61867
    i <- which(is.na(d$longitude))
    j <- match (d$location[i], g$location)
    d$longitude[i] <- g$longitude[j]
    d$latitude[i] <- g$latitude[j]
	d$geo_from_source <- TRUE
    d$geo_from_source[i] <- FALSE
	
	d$trial_id <- d$record_id
	d$record_id <- NULL
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$crop_cut <- TRUE
	d$irrigated <- NA
	d$variety[d$variety == ""] <- NA
	d$adm1[d$adm1 == ""] <- NA

	carobiner::write_files(path, meta, d)
}
