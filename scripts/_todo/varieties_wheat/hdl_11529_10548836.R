# R script for "carob"
# license: GPL (>=3)

## NOTES
# Cleaned line means from ESWYT, HTWYT, IDYN, and SAWYT used in the 2021 Nature Plants paper.
# yld is between about 0.1 and 11, so it is stored as t/ha converted to kg/ha.
# lat and lon are in the files. There is no country column and geographicCoverage does not name the trial countries.
# dayofsow and dayofhar are day-of-year. When harvest day-of-year is earlier than sowing day-of-year, harvest is the next calendar year. Seasons that remain shorter than 45 days are stored as recorded. Values outside day 1-366 are not used as dates.
# IDYN rows are durum. The other nurseries are not given a variety type.
# Publication DOI is in the dataset metadata: doi:10.1038/s41477-021-00988-w

## ISSUES
# write_files: missing variables: yield_moisture, country, K_fertilizer, N_fertilizer, P_fertilizer out of bounds: yield (-74, 17866), heading_days (0, 278), anthesis_days (43, 230), maturity_days (1, 354) datespan: 15786 records with harvest_date within 45 days of planting_date location: variable 'country' is missing NA detected: yield contributor:

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
		data_type = "experiment",
		treatment_vars = "variety_code",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.4
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
		har <- as.integer(num(r$dayofhar))
		yr <- as.integer(num(r$year))
		sow[!is.na(sow) & (sow < 1 | sow > 366)] <- NA_integer_
		har[!is.na(har) & (har < 1 | har > 366)] <- NA_integer_
		hyr <- yr
		cross <- !is.na(sow) & !is.na(har) & har < sow
		hyr[cross] <- yr[cross] + 1
		data.frame(
			crop = "wheat",
			variety_type = vtype,
			variety_code = g,
			variety = variety,
			treatment = g,
			trial_id = paste(nursery, e, r$year),
			yield = num(r$yld) * 1000,
			yield_part = "grain",
			planting_date = doy_date(yr, sow),
			harvest_date = doy_date(hyr, har),
			heading_days = num(r$head),
			anthesis_days = num(r$anth),
			maturity_days = num(r$matu),
			latitude = lat,
			longitude = lon,
			geo_from_source = !is.na(lat) & !is.na(lon),
			on_farm = FALSE,
			is_survey = FALSE,
			irrigated = NA
		)
	}

	ff <- ff[grepl("_ALL_LINES_clean_ARC5\\.csv$", basename(ff))]
	d <- do.call(rbind, lapply(ff, one))
	keep <- !is.na(d$variety_code) | !is.na(d$yield)
	d <- d[keep, ]
	d$geo_from_source[is.na(d$latitude) | is.na(d$longitude)] <- FALSE
	carobiner::write_files(path, meta, d)
}
