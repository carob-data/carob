# R script for "carob"
# license: GPL (>=3)

## NOTES
# TAMASA Tanzania baseline yield survey soil samples, 2015. Revised_Data is the data sheet. Raw_Data is empty.
# The dictionary: total C and total N are percent by weight, pH is in water, and Mehlich 3 nutrients are mg/kg.
# EC.S is labelled dS/m in the Variables sheet, but the values (33.5-240.5, median 69.8) are clearly uS/cm (consistent with other AfSIS/TAMASA EC.S datasets; 240 dS/m would be >4x seawater); converted to mS/cm (/ 1000).
# Total N percent is stored as soil_N_total in mg/kg (percent * 10000).
# Latitude and longitude are decimal degrees in the file. GPS_Precision is meters.
# Depth uses an en dash, for example 0-20 cm and 20-50 cm.
# The description also names the Northern zone. This table has Eastern and Southern Highlands only.

## ISSUES
# Some calcium values exceed terminag valid_max (up to 29959 mg/kg); left as recorded.
# write_files: unknown variable zone_; out of bounds soil_Ca (14.268, 29959.1).

## NEW VARIABLES
# zone_: survey zone named in the file (Eastern, Southern Highlands). It is not an administrative unit.

## NOT PROCESSED
# Farmer name.
# Laboratory ID, which is missing on some rows. sample_id is the sample QR code plus depth.
# Metadata, Variables, and Data_History sheets.


carob_script <- function(path) {

"
TAMASA Tanzania Baseline Yield Survey Soil data for 2015

This dataset consists of soil data from Baseline Yield Survey conducted in Southern Highlands, Northern and Eastern Zones in Tanznaia. These data are associated, by location, with the yield data collected under the same baseline survey for the same areas and time period.
"

	uri <- "hdl:11529/10548219"
	group <- "soil_samples"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "TAMASA",
		design = NA,
		data_type = "survey",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 80,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "TAMASA_BYS_2015_Soil_Data.xlsx"]
	r <- carobiner::read.excel(f, sheet="Revised_Data")

	num <- function(x) {
		if (is.numeric(x)) return(x)
		x <- trimws(as.character(x))
		x[!nzchar(x)] <- NA_character_
		as.numeric(x)
	}
	place <- function(x) {
		x <- trimws(as.character(x))
		x <- gsub("_", " ", x, fixed=TRUE)
		x[!nzchar(x)] <- NA_character_
		ok <- !is.na(x)
		if (any(ok)) x[ok] <- carobiner::fix_name(x[ok], "title")
		x
	}
	parts <- strsplit(as.character(r$Depth), "[^0-9.]+")
	top <- as.numeric(sapply(parts, function(z) z[1]))
	bot <- as.numeric(sapply(parts, function(z) z[2]))

	when <- carobiner::parse_dates(r$Date, order="dmy")

	ssid <- trimws(as.character(r$SSID))
	id <- paste(ssid, top, bot, sep="_")
	seqn <- ave(seq_len(length(id)), id, FUN=seq_along)
	dup <- duplicated(id) | duplicated(id, fromLast=TRUE)
	id[dup] <- paste(id[dup], seqn[dup], sep="_")

	d <- data.frame(
		sample_id = id,
		country = place(r$Country),
		zone_ = place(r$Zone),
		adm1 = place(r$Region),
		adm2 = place(r$District),
		adm3 = place(r$Ward),
		location = place(r$Village),
		latitude = num(r$Latitude),
		longitude = num(r$Longitude),
		elevation = num(r$Altitude),
		geo_uncertainty = num(r$GPS_Precision),
		depth_top = top,
		depth_bottom = bot,
		date = when,
		soil_sample_date = when,
		soil_C_total = num(r$C),
		soil_pH = num(r$pH),
		soil_Al = num(r$Al),
		soil_Ca = num(r$Ca),
		# EC.S values (33-241) are uS/cm despite "dS/m" typo in Variables sheet; / 1000 -> mS/cm
		soil_EC = num(r$EC.S) / 1000,
		soil_S = num(r$S),
		soil_Mn = num(r$Mn),
		soil_P = num(r$P),
		soil_P_method = "Mehlich 3",
		soil_Zn = num(r$Zn),
		soil_K = num(r$K),
		soil_Mg = num(r$Mg),
		soil_Na = num(r$Na),
		soil_Fe = num(r$Fe),
		soil_B = num(r$B),
		# Dictionary: total N is percent by weight. soil_N_total is mg/kg.
		soil_N_total = num(r$N) * 10000,
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = TRUE
	)

	carobiner::write_files(path, meta, d)
}
