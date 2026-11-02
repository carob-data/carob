# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Soybean clean table has no mineral N/P/K columns; omitted (do not assume 0).
# Weather and SoilGrids columns in the clean tables are secondary/modelled; omitted.
# Planting/harvest stored as day-of-year; mapped to 2022/2023 (season 2022-2023).
# Variety names only in raw survey; merged by uuid.
# plot_geotraced_area_m2 is field-sized (114–33000 m2), outside plot_area bounds; omitted.
# Two soybean uuids not in raw survey so GPS/variety missing (write_files: NA lat/lon).


carob_script <- function(path) {

"
Maize and soybean farming practices and yields during the 2022-2023 season in Malawi, Mozambique and Zambia

This dataset describes farming practices in soybean and maize monocrop fields in Malawi, Mozambique and Zambia for the 2022-2023 growing season. Crop cuts were carried out on the fields of farmers participating in the EiA use case, as well as neighbouring farms and farms in non-implementing communities for representativeness. Data are provided as Excel spreadsheets and an .rds file (compatible with R 4.4.1). There is one table with the raw data (as recorded through KoboCollect) and one with the cleaned data used for the related manuscript. Survey data are reproduced as closely as possible to how they were recorded. Secondary data were used as indicated in the codebook.
"

	uri <- "doi:10.71682/10549347"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
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
	rmz <- carobiner::read.excel(f, sheet = "maize_clean_data")
	rsy <- carobiner::read.excel(f, sheet = "soybean_clean_data")
	rr <- carobiner::read.excel(f, sheet = "raw_survey_data")

	# Codebook: planting/harvest are julian day. Season is 2022-2023.
	doy_date <- function(doy) {
		y <- ifelse(doy >= 200, 2022, 2023)
		as.character(as.Date(doy - 1, origin = paste0(y, "-01-01")))
	}

	geo <- data.frame(
		X_uuid = rr$X_uuid,
		latitude = rr[["_field_gps_latitude"]],
		longitude = rr[["_field_gps_longitude"]],
		elevation = rr[["_field_gps_altitude"]],
		variety = rr$variety_name,
		adm1 = carobiner::fix_name(rr$province, "title")
	)
	geo <- geo[!duplicated(geo$X_uuid), ]

	d1 <- data.frame(
		record_id = rmz$X_uuid,
		country = rmz$country,
		adm2 = carobiner::fix_name(rmz$district, "title"),
		location = rmz$epa,
		crop = "maize",
		yield = rmz$maize_yield_kgha,
		plant_density = rmz$maize_plantpop_plha,
		seed_rate = rmz$seed_kgha,
		variety_type = rmz$seed_type,
		N_fertilizer = rmz$nfert_kgha,
		# codebook: P2O5 and K2O
		P_fertilizer = rmz$pfert_kgha / 2.29,
		K_fertilizer = rmz$kfert_kgha / 1.2051,
		OM_used = rmz$orgfert_yn == "y",
		planting_date = doy_date(rmz$date_planting),
		harvest_date = doy_date(rmz$date_harvest),
		weeding_times = as.integer(rowSums(!is.na(cbind(rmz$weeding_1st_das, rmz$weeding_2nd_das)))),
		yield_part = "grain",
		yield_moisture = 0
	)

	d2 <- data.frame(
		record_id = rsy$X_uuid,
		country = rsy$country,
		adm2 = carobiner::fix_name(rsy$district, "title"),
		location = rsy$epa,
		crop = "soybean",
		yield = rsy$soy_yield_kgha,
		dmy_total = rsy$soy_biomass_kgha,
		seed_rate = rsy$seed_kgha,
		variety_type = rsy$seed_type,
		OM_used = rsy$orgfert_yn == "y",
		planting_date = doy_date(rsy$date_planting),
		harvest_date = doy_date(rsy$date_harvest),
		weeding_times = as.integer(rowSums(!is.na(cbind(rsy$weeding_1st_das, rsy$weeding_2nd_das)))),
		yield_part = "seed",
		yield_moisture = 0
	)

	d <- carobiner::bindr(d1, d2)
	d <- merge(d, geo, by.x = "record_id", by.y = "X_uuid", all.x = TRUE)

	d$trial_id <- d$record_id
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$crop_cut <- TRUE
	d$irrigated <- FALSE
	d$geo_from_source <- TRUE
	d$record_id <- NULL
	d$variety[d$variety == ""] <- NA
	d$adm1[d$adm1 == ""] <- NA

	carobiner::write_files(path, meta, d)
}
