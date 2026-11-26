# R script for "carob"
# license: GPL (>=3)

## NOTES
# Cleaned line means from ESWYT, HTWYT, IDYN, and SAWYT used in the 2021 Nature Plants paper (compilation of CIMMYT spring wheat nurseries).
# yld is in t/ha (converted to kg/ha). Negative yields (18 rows down to -0.074 t/ha) are set to NA.
# lat and lon are in the files. There is no country column and geographicCoverage does not name the trial countries.
# yearofsow and dayofsow give the sowing date; dayofhar is days from sowing to harvest (median 23 days after maturity), so harvest_date = planting_date + dayofhar (for plausible 45-365 days; corrupt values like -8 and 11087 are set to NA).
# IDYN rows are durum. The other nurseries are not given a variety type.
# Publication DOI is in the dataset metadata: doi:10.1038/s41477-021-00988-w

## ISSUES
# Compilation of ESWYT, HTWYT, IDYN, and SAWYT trials that may overlap with individual CIMMYT nursery scripts in scripts/varieties_wheat/.
# No country column in the CSVs (678 unique lat/lon sites worldwide).
# 18 negative yields and corrupt heading/maturity/harvest day values (<25, <45, or >365 days) were set to NA.

## NOT PROCESSED
# sim_head, sim_anth, sim_matu, matu1, and head1. The sim_ columns are model output.
# Seasonal, vegetative, reproductive, and grain-filling weather summaries. The file does not state their units.
# r_p.


carob_script <- function(path) {

"
Replication data for: Increased ranking change in wheat breeding under climate change

A standard quantitative genetic model was used to examine how genotype-environment interactions have changed over the past decades from four spring wheat trial data sets. The variability of cross interactions for yield from one year to another is explained in more than 70% by climatic factors.
"

	uri <- "hdl:11529/10548836"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication = "doi:10.1038/s41477-021-00988-w",
		project = "ESWYT, HTWYT, IDYN, and SAWYT",
		design = NA,
		data_type = "compilation",
		treatment_vars = "variety_code",
		response_vars = "yield;heading_days;anthesis_days;maturity_days;harvest_days",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 60,
		carob_effort = 0
	)

	doy_date <- function(year, doy) {
		year <- as.integer(year)
		doy <- as.integer(doy)
		out <- rep(NA_character_, length(year))
		ok <- !is.na(year) & !is.na(doy) & doy >= 1 & doy <= 366
		if (any(ok)) {
			origin <- as.Date(paste0(year[ok], "-01-01"))
			out[ok] <- as.character(origin + (doy[ok] - 1))
		}
		out
	}
	num <- function(x) as.numeric(x)

	one <- function(f) {
		r <- read.csv(f, check.names=FALSE, comment.char="", na.strings=c("NA", ""))
		bn <- basename(f)
		nursery <- "ESWYT"
		if (grepl("^HTWYT", bn)) nursery <- "HTWYT"
		if (grepl("^IDYN", bn)) nursery <- "IDYN"
		if (grepl("^SAWYT", bn)) nursery <- "SAWYT"
		g <- trimws(as.character(r[["G!"]]))
		g[!nzchar(g)] <- NA_character_
		e <- trimws(as.character(r[["E!"]]))
		variety <- sub("^[^_]+_", "", g)
		variety[variety == g] <- g[variety == g]
		vtype <- rep(NA_character_, length(g))
		if (nursery == "IDYN") vtype <- "durum"
		lat <- num(r$lat)
		lon <- num(r$lon)
		sow <- as.integer(num(r$dayofsow))
		har_days <- num(r$dayofhar)
		yr <- as.integer(num(r$yearofsow))
		sow[!is.na(sow) & (sow < 1 | sow > 366)] <- NA_integer_
		# dayofhar is days from sowing to harvest; drop corrupt values outside 45-365 days
		har_days[!is.na(har_days) & (har_days < 45 | har_days > 365)] <- NA_real_
		pdate <- doy_date(yr, sow)
		hdate <- rep(NA_character_, length(pdate))
		okh <- !is.na(pdate) & !is.na(har_days)
		if (any(okh)) hdate[okh] <- as.character(as.Date(pdate[okh]) + as.integer(round(har_days[okh])))
		yld <- num(r$yld) * 1000
		yld[!is.na(yld) & yld < 0] <- NA_real_
		hd <- num(r$head)
		hd[!is.na(hd) & hd < 25] <- NA_real_
		ad <- num(r$anth)
		ad[!is.na(ad) & ad < 30] <- NA_real_
		md <- num(r$matu)
		md[!is.na(md) & md < 45] <- NA_real_
		data.frame(
			crop = "wheat",
			variety_type = vtype,
			variety_code = g,
			variety = variety,
			treatment = g,
			trial_id = paste(nursery, e, r$year),
			yield = yld,
			yield_part = "grain",
			planting_date = pdate,
			harvest_date = hdate,
			heading_days = hd,
			anthesis_days = ad,
			maturity_days = md,
			harvest_days = har_days,
			latitude = lat,
			longitude = lon,
			geo_from_source = !is.na(lat) & !is.na(lon),
			on_farm = FALSE,
			is_survey = FALSE,
			irrigated = NA
		)
	}

	ff <- ff[basename(ff) %in% c(
		"ESWYT_ALL_LINES_clean_ARC5.csv",
		"HTWYT_ALL_LINES_clean_ARC5.csv",
		"IDYN_ALL_LINES_clean_ARC5.csv",
		"SAWYT_ALL_LINES_clean_ARC5.csv"
	)]
	d <- do.call(rbind, lapply(ff, one))
	keep <- !is.na(d$variety_code) | !is.na(d$yield)
	d <- d[keep, ]
	d <- unique(d)
	d$geo_from_source[is.na(d$latitude) | is.na(d$longitude)] <- FALSE
	carobiner::write_files(path, meta, d)
}
