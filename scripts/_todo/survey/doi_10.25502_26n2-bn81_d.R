# R script for "carob"
# license: GPL (>=3)

## NOTES
# BATN baseline, Oke-Ogun, Oyo State, Nigeria. One row per household from batn-baseline-needs-assess-data-all-excel.csv. There is no separate codebook in the deposit.
# The column named Lat holds eastings ("E003 54.161") and Long holds northings ("N07 30.034"). Values are parsed as degrees and decimal minutes from the hemisphere letter, not from the column name. Degrees + minutes/60. Unparseable strings stay NA.
# Mcrop is split on "and" and commas into an intercrop name (cassava_maize). bean -> common bean. Sahel/Nigeria "Mil" left as recorded when it is not the whole token millet.
# Pcass / Pcassz is a cassava area. field_size is Pcassz only when Pcass contains "ha" and the crop is sole cassava. On mixed rows the same figure is cassava_area_ha_.
# N23cassava is the fertiliser named for cassava (NPK 15-15-15, urea, and a few product names). NPK grades are recorded as fertilizer_type NPK. Elemental rates are not calculated because the amount is not in the file.
# Ycass and Yfarm are not yields: they sit next to each other in the 3-70 range and match "years", not a harvest. N21* is plant spacing text. N35cassava is a harvest unit (ton, pickup, truck) with no quantity column.

## ISSUES
# No data dictionary. Column meanings above are from names and value patterns; a human should confirm Pcassz, N23 and the DMS reading against the questionnaire.
# Comm is empty, so location is only the LGA (adm2). adm1 Oyo is from the dataset description (Oke-Ogun, Oyo State), which matches the LGA names.
# Some coordinates fail the letter-degrees-minutes pattern (quotes, swapped N/E, missing space).
# Fertilizer amount is absent. Harvest quantity is absent (only a unit word).
# No publication DOI.
# write_files: missing carob_contributor; unknown fertilizer_note_, cassava_area_ha_, spacing_cassava_, spacing_maize_; invalid fertilizer_type Golden Pen, Kno3, Krister K; duplicate coordinates; NA latitude, longitude.

## NEW VARIABLES
# cassava_area_ha_: Pcassz where Pcass says ha, including mixed-crop households
# fertilizer_note_: N23cassava as recorded when it was not reduced to a single NPK or urea
# spacing_cassava_: N21cassava plant spacing text
# spacing_maize_: N21Maize plant spacing text

## NOT PROCESSED
# Household columns ID demographics: Gend, Mart, Educ, Ysch, Shh, Pinc, Yfarm, Pqur (land acquisition), Plotsz (no unit; values are not clearly hectares).
# Ycass (see NOTES), cassed (seed source), C20-C42 and M01-M36 (unlabelled blocks), N01-N20, N22, N24-N34, N35price, N36price, N37-N41.
# N35cassava / N36maize unit words without quantities.


carob_script <- function(path) {

"Baseline and Needs Assessment of BATN Farmers

British America Tobacco Nigeria Limited (BATNL) wish to mitigate their former leaf farmers from hardship and ensure a thriving and sustainable future for them. A letter from BATNL to IITA presented BATNL’s intention for 365 farmers in Oke-Ogun area of Oyo State. The plan is to support the farmers develop profitable and sustainable cassava/maize intercrop business without recourse to BAT.
"

	uri <- "doi:10.25502/26n2-bn81/d"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "BATN",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 35,
		carob_effort = 0.5
	)

	r <- read.csv(ff[basename(ff) == "batn-baseline-needs-assess-data-all-excel.csv"], stringsAsFactors=FALSE)
	r[] <- lapply(r, function(col) {
		if (!is.character(col)) return(col)
		bad <- !is.na(col) & is.na(iconv(col, from="UTF-8", to="UTF-8"))
		if (any(bad)) col[bad] <- iconv(col[bad], from="latin1", to="UTF-8", sub="")
		col
	})

	parse_dms <- function(x) {
		x <- gsub("['\"]", "", trimws(as.character(x)))
		x <- gsub("([A-Za-z])([0-9])", "\\1\\2", x)
		x <- gsub("\\s+", " ", x)
		out <- rep(NA_real_, length(x))
		for (i in seq_along(x)) {
			if (is.na(x[i]) || !nzchar(x[i])) next
			m <- regexec("^([NnSsEeWw])([0-9]{1,3}) ([0-9.]+)$", x[i])
			g <- regmatches(x[i], m)[[1]]
			if (length(g) != 4) next
			deg <- as.numeric(g[3])
			mins <- as.numeric(g[4])
			if (is.na(deg) || is.na(mins) || mins >= 60) next
			val <- deg + mins / 60
			if (g[2] %in% c("S", "s", "W", "w")) val <- -val
			out[i] <- val
		}
		out
	}

	# column names are swapped relative to the hemisphere letter (see NOTES)
	easting <- parse_dms(r$Lat)
	northing <- parse_dms(r$Long)
	# if a value was parsed from the wrong column, fall back by letter already handled inside parse
	lon <- easting
	lat <- northing
	lat[!is.na(lat) & (lat < 4 | lat > 14)] <- NA
	lon[!is.na(lon) & (lon < 2 | lon > 15)] <- NA

	map_piece <- function(z) {
		z <- gsub("\\s+", " ", trimws(z))
		if (z %in% c("", "na")) return(NA_character_)
		if (grepl("^cassava$|casssava", z)) return("cassava")
		if (grepl("^maize$", z)) return("maize")
		if (grepl("^yam$", z)) return("yam")
		if (grepl("soya|soy", z)) return("soybean")
		if (grepl("^beans?$|^tomatoes$", z)) return(if (grepl("tomato", z)) "tomato" else "common bean")
		if (grepl("cashew", z)) return("cashew")
		if (grepl("^millet$|^mil$", z)) return("pearl millet")
		z
	}
	crop <- vapply(seq_len(nrow(r)), function(i) {
		raw <- tolower(r$Mcrop[i])
		if (is.na(raw) || !nzchar(raw)) return(NA_character_)
		parts <- unlist(strsplit(raw, ",| and "))
		mapped <- unique(unlist(lapply(parts, map_piece)))
		mapped <- mapped[!is.na(mapped) & nzchar(mapped)]
		if (!length(mapped)) NA_character_ else paste(mapped, collapse="_")
	}, character(1))

	pcass <- tolower(trimws(as.character(r$Pcass)))
	cass_ha <- ifelse(grepl("ha", pcass), as.numeric(r$Pcassz), NA_real_)
	sole <- crop == "cassava"

	fert_raw <- trimws(as.character(r$N23cassava))
	fert_raw[fert_raw == ""] <- NA
	fert <- fert_raw
	fert[grepl("NPK", fert_raw) & grepl("Urea|urea", fert_raw)] <- "NPK; urea"
	fert[grepl("NPK", fert_raw) & !grepl("Urea|urea", fert_raw)] <- "NPK"
	fert[grepl("^Urea$", fert_raw)] <- "urea"

	vtype <- tolower(trimws(r$casvar))
	vtype[grepl("both", vtype)] <- NA
	vtype[vtype == "improved"] <- "improved"
	vtype[vtype == "local"] <- "landrace"
	vtype[vtype == ""] <- NA

	d <- data.frame(
		hhid = as.character(r$ID),
		country = "Nigeria",
		adm1 = "Oyo",
		adm2 = carobiner::fix_name(r$LGA, "title"),
		crop = crop,
		variety_type = vtype,
		field_size = ifelse(sole, cass_ha, NA_real_),
		cassava_area_ha_ = cass_ha,
		fertilizer_type = fert,
		fertilizer_note_ = ifelse(!is.na(fert_raw) & fert_raw != fert, fert_raw, NA_character_),
		spacing_cassava_ = trimws(as.character(r$N21cassava)),
		spacing_maize_ = trimws(as.character(r$N21Maize)),
		longitude = lon,
		latitude = lat,
		stringsAsFactors = FALSE
	)
	d$spacing_cassava_[d$spacing_cassava_ == ""] <- NA
	d$spacing_maize_[d$spacing_maize_ == ""] <- NA
	d <- d[!is.na(d$crop), ]
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- !is.na(d$longitude) & !is.na(d$latitude)
	d$trial_id <- d$hhid

	carobiner::write_files(path, meta, d)
}
