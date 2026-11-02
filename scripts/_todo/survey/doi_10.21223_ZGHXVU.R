# R script for "carob"
# license: GPL (>=3)

## NOTES
# Farmer survey in Ulcumayo, Junin, Peru. Same questionnaire as the Paucartambo survey. Coordinates in the file are degrees, minutes and seconds (S and W) and were converted to decimal degrees.
# Potyield and Lbyield are counts of 70 kg bags, not kg/ha. Plot area is not in the file, so they were not converted.
# The farmer-name column was not copied. hhid is the data number.

## ISSUES
# The dictionary formula for Difyield does not match its description. The column is kept as recorded.
# Pest and spray-timing columns are the numeric codes in the file.
# Some GPS cells are empty. Some data-number cells are empty; those rows use rowN as hhid.
# write_files: missing carob_contributor. unknown variables are the questionnaire columns in NEW VARIABLES (farmer_age_, education_, cause_rain_, fungicide_humid_, and the code, cultivar, dose, and bag-count columns). NA detected: latitude, longitude.

## NOT PROCESSED
# Farmer name was not copied.

## NEW VARIABLES
# education_, farmer_gender: as recorded.
# lb_code_ and the other pest and spray-timing codes: numeric codes from the questionnaire.
# pot_yield_bags_, lb_yield_bags_, dif_yield_bags_: 70 kg bags.
# other_cultivars_, susceptible_cultivars_, resistant_cultivars_: names given by the farmer.
# fungicide_humid_, fungicide_early_: commercial names, in the order given.
# spray_interval_humid_days_, spray_interval_early_days_: days between sprays.
# dose_humid_1_ to dose_humid_5_ and dose_early_1_ to dose_early_4_: tablespoons per 20 L.
# n_fungicides_humid_, n_fungicides_early_, n_sprays_humid_, n_sprays_early_: counts as recorded.


carob_script <- function(path) {

"
Dataset for: Farmers' perception on late blight in Peruvian highlands - Ulcumayo (Junin, Peru)

A questionnaire with 25 questions, many with multiple answers, was used in the village of Ulcumayo (Junin) in the Central Peruvian highlands to learn more about the perceptions of farmers regarding potato production with particular emphasis on management practices for the disease late blight (LB), caused by the oomycete, Phytophthora infestans.
"

	uri <- "doi:10.21223/ZGHXVU"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 80,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "01_Data_survey_Ulcumayo.xlsx"]
	r <- carobiner::read.excel(f, sheet="Database Ulcumayo")

	num <- function(x) {
		x <- trimws(as.character(x))
		ok <- grepl("^-?[0-9.]+$", x)
		x[!ok] <- NA_character_
		as.numeric(x)
	}
	# Sexagesimal text such as S10°46'03.1" . Several prime characters occur in the file.
	dms <- function(x) {
		x <- gsub("[\u2019\u02B9\u2032\u00B4`]", "'", as.character(x), perl=TRUE)
		x <- gsub("[\u201c\u201d\u2033\"]", "", x)
		x <- gsub("[[:space:]]+", "", x)
		x <- gsub(",", ".", x)
		x <- sub("'+$", "", x)
		m <- regexec("^([NSEW])([0-9]+).([0-9]+)'([0-9.]+)$", x, perl=TRUE)
		g <- regmatches(x, m)
		out <- rep(NA_real_, length(x))
		ok <- lengths(g) == 5L
		if (!any(ok)) return(out)
		hemi <- vapply(g[ok], function(z) z[2], "")
		dec <- as.numeric(vapply(g[ok], function(z) z[3], "")) +
			as.numeric(vapply(g[ok], function(z) z[4], "")) / 60 +
			as.numeric(vapply(g[ok], function(z) z[5], "")) / 3600
		dec[hemi %in% c("S", "W")] <- -dec[hemi %in% c("S", "W")]
		out[ok] <- dec
		out
	}
	paste_cols <- function(cols) {
		m <- as.data.frame(lapply(cols, function(z) trimws(as.character(z))))
		apply(m, 1, function(z) {
			z <- z[nzchar(z) & !is.na(z) & z != "NA"]
			if (!length(z)) NA_character_ else paste(z, collapse="; ")
		})
	}

	d <- data.frame(
		hhid = {
			h <- trimws(as.character(r$Data))
			miss <- is.na(h) | !nzchar(h) | h == "NA"
			h[miss] <- paste0("row", seq_len(nrow(r))[miss])
			h
		},
		farmer_age_ = num(r$Age),
		education_ = trimws(as.character(r[["Education level"]])),
		farmer_gender = trimws(as.character(r$Gender)),
		location = tools::toTitleCase(tolower(trimws(as.character(r$Place)))),
		latitude = dms(r$Latitude),
		longitude = dms(r$Longitude),
		elevation = num(r$Altitude),
		lb_code_ = num(r$LB),
		apw_code_ = num(r$APW),
		black_scurf_code_ = num(r$BS),
		early_blight_code_ = num(r$EB),
		frost_code_ = num(r$F),
		ptm_code_ = num(r$PTM),
		pfb_code_ = num(r$PFB),
		tuber_rot_code_ = num(r$TR),
		leafminer_code_ = num(r$PLM),
		cutworm_code_ = num(r$CW),
		other_pest_code_ = num(r$Othr1),
		cause_sun_ = num(r$Sun),
		cause_rain_ = num(r$Rai),
		cause_mist_ = num(r$Mi),
		cause_mixed_ = num(r$SRM),
		cause_insects_ = num(r$Insc),
		cause_fungi_ = num(r$Fun),
		cause_other_ = num(r$Othr2),
		spray_planting_ = num(r$PD),
		spray_emergence_ = num(r$Eme),
		spray_20_30d_ = num(r[["30d"]]),
		spray_rainy_ = num(r$Rainy),
		spray_rain_sun_ = num(r[["Rain/sun"]]),
		spray_spots_ = num(r$Nspot),
		spray_neighbour_ = num(r$NeighLB),
		spray_other_ = num(r$Others3),
		advice_seller_ = num(r$AgSell),
		advice_agronomist_ = num(r$Agron),
		advice_neighbour_ = num(r$Neigh),
		advice_experience_ = num(r$Ownexp),
		advice_course_ = num(r$Course),
		advice_media_ = num(r$RTV),
		advice_other_ = num(r$Others4),
		variety = trimws(as.character(r$Cv1)),
		other_cultivars_ = paste_cols(r[, c("Cv2", "Cv3", "Cv4", "Cv5", "Cv6", "CV7")]),
		n_cultivars_ = num(r$TotCv),
		susceptible_cultivars_ = paste_cols(r[, c("S1", "S2", "S3", "S4", "S5")]),
		resistant_cultivars_ = paste_cols(r[, c("R1", "R2", "R3", "R4", "R5")]),
		fungicide_humid_ = paste_cols(r[, c("FHS1", "FHS2", "FHS3", "FHS4", "FHS5")]),
		n_fungicides_humid_ = num(r$TotF),
		spray_interval_humid_days_ = num(r$FfreqHS),
		dose_humid_1_ = num(r$DF1),
		dose_humid_2_ = num(r$DF2),
		dose_humid_3_ = num(r$DF3),
		dose_humid_4_ = num(r$DF4),
		dose_humid_5_ = num(r$DF5),
		n_sprays_humid_ = num(r$NFS),
		fungicide_early_ = paste_cols(r[, c("FES1", "FES2", "FES3", "FES4")]),
		n_fungicides_early_ = num(r$TFRF),
		spray_interval_early_days_ = num(r$FfreqES),
		dose_early_1_ = num(r$DFES1),
		dose_early_2_ = num(r$DFES2),
		dose_early_3_ = num(r$DFES3),
		dose_early_4_ = num(r$DFES4),
		n_sprays_early_ = num(r$NFEachS),
		pot_yield_bags_ = num(r$Potyield),
		lb_yield_bags_ = num(r$Lbyield),
		dif_yield_bags_ = num(r$Difyield)
	)
	d$variety[!nzchar(d$variety) | d$variety == "NA"] <- NA_character_
	d$crop <- "potato"
	d$country <- "Peru"
	d$adm1 <- "Junin"
	d$adm2 <- "Ulcumayo"
	d$trial_id <- paste0("Ulcumayo_", d$hhid)
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$stress <- "disease"
	d$pathogen <- "Phytophthora infestans"
	d$geo_from_source <- !is.na(d$latitude) & !is.na(d$longitude)

	carobiner::write_files(path, meta, d)
}
