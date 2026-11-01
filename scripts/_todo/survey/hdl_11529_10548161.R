# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household survey of maize producers in Odisha, India. Country is India and the state is Odisha in the title and in geographic coverage.
# MaizeAgronomy reports dry grain sold and kept, in quintals, and area with a land-unit code. One quintal is 100 kg. An acre is 0.4046863 ha. A decimal is 1/100 acre. A guntha is 1/40 acre. Mana is not converted.
# Yield is (sold + consumed quintals) * 100 / area_ha for dry grain. Green-cob quantities are not grain.
# Household coordinates are SF__GPS_location in the sampling frame. geo_from_source is TRUE.

## ISSUES
# Rows with land unit Mana have no yield because that unit is not defined in the file.
# District and village names come from the sampling frame. Some GPS values may be missing.
# write_files: unknown variable: planting_method_ (planting_method?). duplicates: duplicate records detected. NA detected: yield, latitude, longitude.

## NEW VARIABLES
# planting_method_: manual or line-sown.

## NOT PROCESSED
# Green-cob sales and consumption.
# Wheat plots, food consumption, income, credit, and assets.
# Market transactions and zero-tillage service opinions.


carob_script <- function(path) {

"
Livelihood Impacts of Commercial Maize Production in the Plateau Ecology of Odisha, India.

Large tracts of land in the upland plateau ecology of Odisha are not suitable for rice production and are largely left fallow. In Mayurbhanj district of the plateau area, nearly half of the total cultivable area (43%) falls into the upland category that is not very suitable for growing rice. Since 2013, CSISA has been engaged in efforts to sustainably intensify maize production in Mayurbhanj district.
"

	uri <- "hdl:11529/10548161"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "CSISA",
		design = NA,
		data_type = "survey",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 25,
		carob_effort = 0.6
	)

	fa <- ff[basename(ff) == "MaizeAgronomy.dta"]
	fs <- ff[basename(ff) == "Sampling_Frame.dta"]
	a <- haven::read_dta(fa)
	s <- haven::read_dta(fs)
	num <- function(x) as.numeric(haven::zap_labels(x))
	chr <- function(x) {
		x <- trimws(as.character(haven::as_factor(x)))
		x[!nzchar(x)] <- NA_character_
		x
	}
	to_ha <- function(area, unit) {
		u <- chr(unit)
		u[is.na(u)] <- ""
		area <- num(area)
		ha <- rep(NA_real_, length(area))
		acre <- u == "Acre"
		dec <- u == "Decimal"
		gun <- u == "Guntha"
		ha[acre] <- area[acre] * 0.4046863
		ha[dec] <- area[dec] * 0.4046863 / 100
		ha[gun] <- area[gun] * 0.4046863 / 40
		ha
	}
	yes <- function(x) {
		z <- tolower(chr(x))
		z %in% c("yes", "y", "1")
	}
	block <- function(yn, area, unit, sold, cons, variety, method) {
		keep <- yes(a[[yn]]) | (!is.na(num(a[[area]])) & num(a[[area]]) > 0)
		ha <- to_ha(a[[area]], a[[unit]])
		soldq <- num(a[[sold]])
		consq <- num(a[[cons]])
		qty <- ifelse(is.na(soldq) & is.na(consq), NA_real_, ifelse(is.na(soldq), 0, soldq) + ifelse(is.na(consq), 0, consq))
		yld <- ifelse(!is.na(qty) & !is.na(ha) & ha > 0, qty * 100 / ha, NA_real_)
		data.frame(
			hhid = chr(a$Respondent_ID),
			variety = variety,
			planting_method_ = method,
			treatment = paste(method, variety),
			yield = yld,
			keep = keep
		)
	}
	d <- rbind(
		block("ManLocal_MzCultYN", "ManLocal_TotalMzArea", "ManLocal_LandUnit", "ManLocal_DryGrainSoldQty", "ManLocal_DryGrainConsQty", "local", "manual"),
		block("ManHyb_MzCultYN", "ManHyb_TotalMzArea", "ManHyb_LandUnit", "ManHyb_DryGrainSoldQty", "ManHyb_DryGrainConsQty", "hybrid", "manual"),
		block("LSLocal_MzCultYN", "LSLocal_TotalMzArea", "LSLocal_LandUnit", "LSLocal_DryGrainSoldQty", "LSLocal_DryGrainConsQty", "local", "line-sown"),
		block("LSHybrid_MzCultYN", "LSHybrid_TotalMzArea", "LSHybrid_LandUnit", "LSHybrid_DryGrainSoldQty", "LSHybrid_DryGrainConsQty", "hybrid", "line-sown")
	)
	d <- d[d$keep, ]
	d$keep <- NULL
	geo <- data.frame(
		hhid = chr(s$Respondent_ID),
		latitude = num(s$SF__GPS_location_latitude),
		longitude = num(s$SF__GPS_location_longitude),
		adm2 = chr(s$SF_district_surveybe),
		location = chr(s$SF_Name_of_Village)
	)
	geo <- geo[!duplicated(geo$hhid), ]
	d <- merge(d, geo, by="hhid", all.x=TRUE)
	d$country <- "India"
	d$adm1 <- "Odisha"
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$trial_id <- d$hhid
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- !is.na(d$latitude) & !is.na(d$longitude)
	d$adm2 <- carobiner::fix_name(d$adm2, "title")
	d$location <- carobiner::fix_name(d$location, "title")

	carobiner::write_files(path, meta, d)
}
