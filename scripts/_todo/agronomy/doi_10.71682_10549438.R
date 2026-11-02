# R script for "carob"
# license: GPL (>=3)

## NOTES
# Seven years at the San Miguel Tlacamama research platform, Oaxaca, Mexico. The title names the country.
# Latitude and longitude are degrees, minutes, and seconds. Longitude is unsigned. It is stored west, because the platform is in Mexico.
# Yield_moist is the yield adjusted to 14% for maize and 12% for other grains, in kg/ha from the magnitude. yield_moisture is that basis.
# The Crop value Bean is cowpea. The variety on those rows is Cowpea, and the treatment table says maize-cowpea.
# Fert_N, Fert_P, and Fert_K are kilograms of the elements. They are 51, 12, and 15 on every row, including Supermagro-only treatments. The treatment table lists 51-12-15 only for the organic-mineral treatments.
# Zero tillage is land_prep_method none. Kept residue is residue_prevcrop_used TRUE. Water regime Temporal is rainfed.
# Row distance and intrarow spacing are both 1 and the legend gives no unit.
# Mat_days is a date. maturity_days is that date minus the sowing date.
# Heights below 10 are metres and are stored in centimetres. The cell 3.09.8 is stored as 309.8.

## ISSUES
# Fertilizer columns do not differ between Supermagro and Supermagro plus NPK.

## NOT PROCESSED
# Soil data, grain quality, and the yearly operations sheets.
# Cost, price, and profit.
# Flower_days, described as days between flowering and maturity.
# Count, about 120, which does not match plants per square meter.
# Row_dist and Intrarow, both 1, with no unit.
# Precip and Irrig are empty.


carob_script <- function(path) {

"
Results of a 7-Year Conservation Agriculture Experiment at the San Miguel Tlacamama Research Platform, Oaxaca, Mexico (2017–2023)

Rainfed maize production in southern Mexico is constrained by climate variability and soil fertility limitations. This dataset contains seven years of data from a long-term field experiment conducted at the San Miguel Tlacamama Research Platform, Oaxaca, Mexico, evaluating the effects of crop diversification (maize monoculture vs. maize–cowpea intercropping), stubble management (remove vs. keep), and fertilization (organic vs. organic–mineral fertilization).
"

	uri <- "doi:10.71682/10549438"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.5
	)

	dms <- function(x) {
		x <- as.character(x)
		x <- gsub("[°º]", " ", x)
		x <- gsub("[’'`]", " ", x)
		x <- gsub("[”\"″]", " ", x)
		sp <- strsplit(trimws(x), "[^0-9.]+")
		out <- rep(NA_real_, length(x))
		for (i in seq_along(sp)) {
			p <- sp[[i]]
			p <- p[nzchar(p)]
			if (!length(p)) next
			out[i] <- as.numeric(p[1])
			if (length(p) >= 2) out[i] <- out[i] + as.numeric(p[2]) / 60
			if (length(p) >= 3) out[i] <- out[i] + as.numeric(p[3]) / 3600
		}
		out
	}
	as_date <- function(x) {
		if (inherits(x, "POSIXt") || inherits(x, "Date")) return(as.character(as.Date(x)))
		x <- trimws(as.character(x))
		x[!nzchar(x)] <- NA_character_
		out <- rep(NA_character_, length(x))
		iso <- !is.na(x) & nchar(x) >= 10 & substr(x, 5, 5) == "-"
		out[iso] <- substr(x[iso], 1, 10)
		out
	}

	plain_num <- function(x) {
		if (is.numeric(x)) return(as.numeric(x))
		z <- trimws(as.character(x))
		z[z %in% c("", "NA", "N/A", "n/a", ".", "-")] <- NA_character_
		as.numeric(z)
	}
	height_cm <- function(x) {
		z <- trimws(as.character(x))
		z[z %in% c("", "NA", "N/A", "n/a", ".", "-")] <- NA_character_
		two <- !is.na(z) & grepl("\\..*\\.", z)
		z[two] <- sub("\\.", "", z[two])
		n <- as.numeric(z)
		n[!is.na(n) & n < 10] <- n[!is.na(n) & n < 10] * 100
		n
	}

	f <- ff[grepl("xlsx$", basename(ff), ignore.case=TRUE)]
	r <- carobiner::read.excel(f, sheet="San Miguel Tlacamama_OAX")
	crop <- rep(NA_character_, nrow(r))
	crop[r$Crop == "Maize"] <- "maize"
	crop[r$Crop == "Bean"] <- "cowpea"
	part <- ifelse(crop == "maize", "grain", "seed")
	moist <- ifelse(crop == "maize", 14, 12)
	lon <- dms(r$Longitud)
	# Unsigned longitude at this Oaxaca platform is west of Greenwich.
	lon <- -abs(lon)

	d <- data.frame(
		country = "Mexico",
		adm1 = "Oaxaca",
		location = "San Miguel Tlacamama",
		trial_id = paste("San Miguel Tlacamama", r$Year),
		plot_id = as.character(r$Parcela),
		rep = as.integer(r$Num_Rep),
		treatment = trimws(as.character(r$Name_tr)),
		crop = crop,
		variety = trimws(as.character(r$Variety)),
		yield = as.numeric(r$Yield_moist),
		yield_moisture = moist,
		yield_part = part,
		planting_date = as_date(r$Sowing_date),
		harvest_date = as_date(r$Harvest_date),
		maturity_days = as.numeric(as.Date(r$Mat_days) - as.Date(r$Sowing_date)),
		plant_height = height_cm(r$Height),
		plant_density = plain_num(r$Plants_m2) * 10000,
		seed_density = as.numeric(r$Seed_dens),
		plot_area = as.numeric(r$Area_plot),
		elevation = as.numeric(r$Altitud),
		latitude = dms(r$Latitud),
		longitude = lon,
		N_fertilizer = as.numeric(r$Fert_N),
		P_fertilizer = as.numeric(r$Fert_P),
		K_fertilizer = as.numeric(r$Fert_K),
		land_prep_method = "none",
		residue_prevcrop_used = r$Residue == "Kept",
		irrigated = FALSE,
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = TRUE
	)
	carobiner::write_files(path, meta, d)
}
