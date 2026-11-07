# R script for "carob"
# license: GPL (>=3)

## NOTES
# Yield and aboveground biomass from pLER_LER. The Legend sheet gives Mg DM/ha, so values are stored as kg/ha with yield_moisture 0.
# Each farmer-season row is expanded to maize and cowpea, sole and intercrop, when that yield or biomass is present.
# Coordinates, country, and elevation are the Sites sheet (farmer matched to id_site). Country in that sheet is Zimbabwe.
# Water-stress, nitrogen-stress, and LER columns are estimated or derived and are not mapped. The Fertilization sheet has no treatment key shared with pLER_LER, so N, P, and K are not joined.

## ISSUES
# Cowpea variety is not in the yield sheet. Crop_management has planting and harvest dates, but they differ by treatment and the yield sheet has no treatment code, so they were not joined. Day-of-year columns on the yield sheet have no calendar year.
# Solar radiation is labeled J/m2 but the values are about 20, so it is not mapped.
# write_files: missing variables: project. missing variables: irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.

## NOT PROCESSED
# Soil_results, N2-fixation, Soil_water_LAI, Fertilization, and the stress and LER columns.

## NEW VARIABLES
# None.


carob_script <- function(path) {

"
Data for \"Intercrop overyielding is maintained under estimated water and nitrogen stress in maize-cowpea on-farm trials in semi-arid Zimbabwe\"

These are the raw data of the paper \"Intercrop overyielding is maintained under estimated water and nitrogen stress in maize-cowpea on-farm trials in semi-arid Zimbabwe\" authored by Illiana W. Kwenda, Gatien N. Falconnier, Rémi Cardinael, François Affholder, Antoine Couëdel, Frederic Baudron, Angelinus C. Franke, Isaiah Nyagumbo, Stanford Mabasa, Mathilde de Freitas, Valentin Pret, Souleymane Diop, Eleanor F. Magwaza, Regis Chikowo
"

	uri <- "doi:10.18167/DVN1/G5TYUZ"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIRAD",
		publication = "doi:10.1016/j.fcr.2025.109890",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "intercrops",
		response_vars = "yield;dmy_total",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.8
	)

	f <- ff[basename(ff) == "Kwenda_et_al_data_vf.xlsx"]
	r1 <- carobiner::read.excel(f, sheet = "pLER_LER")
	r2 <- carobiner::read.excel(f, sheet = "Sites")
	r3 <- carobiner::read.excel(f, sheet = "Climate_data")

	num <- function(x) {
		if (is.numeric(x)) return(x)
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "na")] <- NA
		as.numeric(x)
	}

	piece <- function(crop, intercrops, yield_col, biom_col) {
		out <- data.frame(
			farmer = r1$farmer,
			season = r1[["growing season"]],
			variety = if (crop == "maize") r1[["maize variety"]] else NA_character_,
			crop = crop,
			intercrops = intercrops,
			yield = num(r1[[yield_col]]) * 1000,
			dmy_total = num(r1[[biom_col]]) * 1000
		)
		out <- out[!(is.na(out$yield) & is.na(out$dmy_total)), ]
		out
	}

	maize <- rbind(
		piece("maize", "cowpea", "Maize grain yield for intercropping", "Maize aboveground biomass for intercropping"),
		piece("maize", "none", "maize grain yield for sole cropping", "Maize aboveground biomass for sole cropping")
	)
	cow <- rbind(
		piece("cowpea", "maize", "cowpea grain yield for intercropping", "cowpea aboveground biomass for intercropping"),
		piece("cowpea", "none", "cowpea grain yield for sole cropping", "cowpea aboveground biomass for sole cropping")
	)
	# The yield sheet repeats cowpea results once per maize variety. Keep one row when the cowpea record is the same.
	cow <- cow[!duplicated(cow[, c("farmer", "season", "intercrops", "yield", "dmy_total")]), ]
	d <- rbind(maize, cow)

	r2 <- r2[, c("id_site", "country", "longitude", "latitude", "elevation")]
	d <- merge(d, r2, by.x = "farmer", by.y = "id_site", all.x = TRUE)

	d <- data.frame(
		trial_id = paste(d$farmer, gsub(" ", "", d$season), sep = "_"),
		location = d$farmer,
		country = d$country,
		longitude = d$longitude,
		latitude = d$latitude,
		elevation = d$elevation,
		geo_from_source = TRUE,
		crop = d$crop,
		variety = d$variety,
		treatment = ifelse(is.na(d$variety), d$crop, d$variety),
		intercrops = d$intercrops,
		intercropped = d$intercrops != "none",
		yield = d$yield,
		dmy_total = d$dmy_total,
		yield_part = ifelse(d$crop == "cowpea", "seed", "grain"),
		yield_moisture = 0,
		# Climate file station id is KWERE. Sites also lists KWRE, which is not a farmer.
		station_id = "KWERE",
		on_farm = TRUE,
		is_survey = FALSE
	)

	as_date <- function(x) {
		if (inherits(x, "POSIXt")) return(format(x, "%Y-%m-%d"))
		if (inherits(x, "Date")) return(as.character(x))
		as.character(x)
	}
	wth <- data.frame(
		station_id = as.character(r3$id_wst),
		date = as_date(r3$Date),
		tmax = as.numeric(r3[[grep("^max_temperature", names(r3))]]),
		tmin = as.numeric(r3[[grep("^min_temperture", names(r3))]]),
		prec = as.numeric(r3[[grep("^rainfall", names(r3))]]),
		wspd = as.numeric(r3[[grep("^wind_speed", names(r3))]]),
		rhum = as.numeric(r3[[grep("^relative_humidity", names(r3))]])
	)
	wth <- wth[!is.na(wth$date), ]

	carobiner::write_files(path, meta, d, wth = wth)
}
