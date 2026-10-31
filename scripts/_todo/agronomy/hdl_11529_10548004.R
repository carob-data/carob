# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm maize and vegetable intercropping at Purnea, rabi 2015-16. The column ctry contains the state Bihar, not a country. dist is Purnea. geographicCoverage is empty.
# gy and bm are t/ha. The Cost sheet names the companion column "Inter crop Yield (t/ha)". incy on the intercropping sheet is that yield. It is 0 on sole maize. Maize yield_part is grain. Coriander and red amaranth yield_part is leaves.
# Coordinates are on the Cost sheet, on the same row as the node and farmer name. A latitude of 0 is empty. geo_from_source is TRUE where both coordinates are present. Treatment codes on the Cost sheet are not the same labels as the intercropping sheet, so fertilizer rates on the Cost sheet are not joined.
# irri-cm is centimetres of irrigation, stored as mm. ds is an Excel date serial (origin 1899-12-30). The column ctry contains the state Bihar, not a country.

## ISSUES
# No country column. The field labeled Country contains Bihar.
# Plot size on the Cost sheet is often above 350 m2.
# Biomass is not labeled fresh or dry.
# write_files: missing variables: yield_moisture, country, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date. unknown variables: biomass_kg_ha_. low yield (tons not kg?): red amaranth. location: variable 'country' is missing. NA detected: latitude, longitude.

## NEW VARIABLES
# biomass_kg_ha_: bm in t/ha, stored as kg/ha, on the maize row. Fresh or dry is not stated.

## NOT PROCESSED
# Labor, fuel, costs, returns, BCR, energy, and emission columns.
# Farmer names.


carob_script <- function(path) {

"
5.4- Rabi (winter) crops-all nodes- Inter-cropping trial -Purnea-Bihar

Farmer participatory intercropping of leafy vegetables in maize, Purnea, Bihar, rabi 2015-16.
"

	uri <- "hdl:11529/10548004"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = "Intercropping trial, Purnea, Bihar",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.5
	)

	f <- ff[grepl("\\.xlsx$", basename(ff), ignore.case=TRUE)][1]
	r <- as.data.frame(carobiner::read.excel(f, sheet="intercropping"))
	r <- r[!is.na(r$crop) & !is.na(r$gy), ]
	raw <- carobiner::read.excel(f, sheet="Cost", col_names=FALSE)
	hdr <- which(apply(raw, 1, function(z) any(trimws(as.character(z)) == "Lat")))[1]
	cnms <- trimws(as.character(unlist(raw[hdr, ])))
	cnms[!nzchar(cnms)] <- paste0("blank", seq_along(cnms))[!nzchar(cnms)]
	cost <- as.data.frame(raw[(hdr + 1):nrow(raw), , drop=FALSE])
	names(cost) <- make.names(cnms, unique=TRUE)
	latn <- grep("^Lat$", names(cost), value=TRUE)[1]
	lonn <- grep("^Long$", names(cost), value=TRUE)[1]
	namen <- grep("^Farmer.name$", names(cost), value=TRUE)[1]
	clat <- as.numeric(cost[[latn]])
	clon <- as.numeric(cost[[lonn]])
	clat[clat == 0] <- NA_real_
	clon[clon == 0] <- NA_real_
	ck <- paste(tolower(trimws(as.character(cost$Node))), tolower(trimws(as.character(cost[[namen]]))))
	uk <- unique(ck)
	lat_u <- rep(NA_real_, length(uk))
	lon_u <- rep(NA_real_, length(uk))
	for (i in seq_along(uk)) {
		j <- which(ck == uk[i] & !is.na(clat) & !is.na(clon))
		if (length(j) && length(unique(paste(clat[j], clon[j]))) == 1) {
			lat_u[i] <- clat[j[1]]
			lon_u[i] <- clon[j[1]]
		}
	}
	r_key <- paste(tolower(trimws(as.character(r$node))), tolower(trimws(as.character(r$far))))
	hit <- match(r_key, uk)
	lat <- lat_u[hit]
	lon <- lon_u[hit]

	as_date <- function(x) {
		if (inherits(x, "POSIXt") || inherits(x, "Date")) return(as.character(as.Date(x)))
		if (is.numeric(x)) return(as.character(as.Date(x, origin="1899-12-30")))
		rep(NA_character_, length(x))
	}
	hi <- as.numeric(r$hi)
	hi <- ifelse(!is.na(hi) & hi <= 1.5, hi * 100, hi)
	water <- as.numeric(r[["irri-cm"]]) * 10
	loc <- carobiner::fix_name(tolower(trimws(r$node)), "title")
	base <- data.frame(
		adm1 = "Bihar",
		adm2 = "Purnea",
		location = loc,
		season = "rabi",
		trial_id = paste(loc, r$yr),
		treatment = r$trt,
		planting_date = as_date(r$ds),
		maturity_days = as.numeric(r[["crop-duration"]]),
		yield_maize = as.numeric(r$gy) * 1000,
		yield_other = as.numeric(r$incy) * 1000,
		system = trimws(r$crop),
		biomass_kg_ha_ = as.numeric(r$bm) * 1000,
		harvest_index = hi,
		irrigation_amount = water,
		latitude = lat,
		longitude = lon,
		far = r[["Far-id"]]
	)
	base$irrigated <- !is.na(base$irrigation_amount) & base$irrigation_amount > 0
	base$geo_from_source <- !is.na(base$latitude) & !is.na(base$longitude)

	maize <- base
	maize$crop <- "maize"
	maize$yield <- maize$yield_maize
	maize$yield_part <- "grain"
	other <- base[base$system != "Sole Maize", ]
	other$crop <- ifelse(other$system == "Maize+Coriander", "coriander", "red amaranth")
	other$yield <- other$yield_other
	other$yield_part <- "leaves"
	other$biomass_kg_ha_ <- NA_real_
	other$harvest_index <- NA_real_
	other$maturity_days <- NA_real_
	d <- rbind(maize, other)
	d$plot_id <- paste(d$location, d$far, d$treatment, d$crop)
	d$yield_maize <- NULL
	d$yield_other <- NULL
	d$system <- NULL
	d$far <- NULL
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$yield_isfresh <- FALSE
	if (all(is.na(d$latitude))) {
		d$latitude <- NULL
		d$longitude <- NULL
		d$geo_from_source <- FALSE
	}

	carobiner::write_files(path, meta, d)
}
