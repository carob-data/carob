# R script for "carob"
# license: GPL (>=3)

## NOTES
# 2013 Peru potato-adoption survey. The measurements are in STC_Survey_2017_Febrero09-2018.accdb, read with RODBC and the Access ACE driver.
# GPS strings have no hemisphere. The study is in Peru, so latitude is stored south and longitude west.
# P4ACosMed is the kilograms of one harvest unit, as stored (1 for KG, 1000 for TN, and the recorded sack or arroba weight). Production in kg is quantity times that factor. Status values such as NO COSECHO have no factor and are left missing.
# P3AMedida is the square meters of one land unit, as stored (10000 for HA, 1 for M2). Area in hectares is quantity times that factor, divided by 10000.

## ISSUES
# Yield is production divided by parcel area only when that parcel has a single potato row. A parcel with several varieties keeps production_kg_ and does not get the whole parcel area as its yield. One potato yield reaches 1,020,000 kg/ha; it is left as recorded.
# A few GPS strings fall outside Peru, including in Brazil, or not on land. They are left as parsed.
# CodigoGeog is a code, not a place name.
# Farmer names are not copied.
# write_files: missing carob_contributor. unknown variables: production_kg_. out of bounds: yield (17.25, 1020000). high crop yield: potato: 1020000. coordinates not on land: Peru. coordinates in wrong country: Peru/Brazil. duplicates. NA detected: yield, latitude, longitude.

## NOT PROCESSED
# Pag03 capital social, Pag06 adoption knowledge, Pag09 variety scores, Pag10 inputs, Pag11 labor and chemicals, Pag12-16 constraints, market, assets, extension.
# Seed quantity on the potato-production page.
# Data_dictionary_STC_Survey.xls.

## NEW VARIABLES
# production_kg_: harvested quantity times the recorded kilograms per unit.


carob_script <- function(path) {

"
Replication data for: Adoption and diffusion of potato varieties in Peru

For more than 40 years CIP has worked with the potato breeding program of the National Institute of Agriculture Innovation (INIA) in Peru to develop new genetic material. More than 34 improved varieties have been released in the country under this collaborative framework. Canchan (released in 1990) and Amarilis (1993) are amongst the most popular. Recently released varieties such as Unica (1998) and Serranita (2006) seem to be increasing their adoption rates. Besides meeting market needs, these varieties have higher yields and reduce production costs, increasing farmers' income and reducing the environmental footprint through lower pesticide use. Estimates of adoption of these varieties have been obtained in the past from expert opinions, or are only partial approximations based on local level studies. Lack of a nationally representative sample has constrained the validity of these estimations and the ability of drawing rigorous conclusions about farmers' drivers of adoption. These estimates and conclusions are a crucial feedback to adjust breeding strategies while developing new varieties. This database collected during the 2013 growing season were conducted from a nationally representative sample of 1098 potato farmers in 120 communities. The objectives of these database were to quantify adoption and diffusion of improved potato varieties in the most representative production regions of Peru; and to identify adoption determinants of improved potato varieties in Peru and estimate their impacts.
"

	uri <- "doi:10.21223/P3/MDNLI6"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "variety",
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.8
	)

	f <- ff[basename(ff) == "STC_Survey_2017_Febrero09-2018.accdb"]
	ch <- RODBC::odbcConnectAccess2007(f)
	on.exit(RODBC::odbcClose(ch))
	geo <- RODBC::sqlQuery(ch, "SELECT CodigoEnc, P0Latitud, P0Longitud, P0Altitud FROM [Pag02DatosGenerales]", stringsAsFactors=FALSE)
	land <- RODBC::sqlQuery(ch, "SELECT CodigoEnc, P3ANroParcela, P3ACantidad, P3AMedida FROM [Pag04Tenencia de Tierra]", stringsAsFactors=FALSE)
	pot <- RODBC::sqlQuery(ch, "SELECT CodigoEnc, P4AParcela, P4AVariedadNomb, P4ACosCant, P4ACosUnid, P4ACosMed FROM [Pag07-08ProduccionDePapa]", stringsAsFactors=FALSE)

	dms <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x) | x == "NA"] <- NA_character_
		out <- rep(NA_real_, length(x))
		ok <- !is.na(x)
		parts <- strsplit(gsub("[^0-9.]+", " ", x[ok]), " +")
		out[ok] <- vapply(parts, function(p) {
			p <- p[nzchar(p)]
			if (!length(p)) return(NA_real_)
			d <- as.numeric(p[1])
			m <- if (length(p) >= 2) as.numeric(p[2]) else 0
			s <- if (length(p) >= 3) as.numeric(p[3]) else 0
			d + m / 60 + s / 3600
		}, numeric(1))
		out
	}
	num <- function(x) {
		x <- trimws(as.character(x))
		ok <- grepl("^-?[0-9.]+$", x)
		x[!ok] <- NA_character_
		as.numeric(x)
	}

	pot$hhid <- trimws(as.character(pot$CodigoEnc))
	pot$parcel <- trimws(as.character(pot$P4AParcela))
	pot$production_kg_ <- num(pot$P4ACosCant) * num(pot$P4ACosMed)
	land$hhid <- trimws(as.character(land$CodigoEnc))
	land$parcel <- trimws(as.character(land$P3ANroParcela))
	land$area_ha <- num(land$P3ACantidad) * num(land$P3AMedida) / 10000
	land <- land[!is.na(land$hhid) & !is.na(land$parcel), c("hhid", "parcel", "area_ha")]
	land <- land[!duplicated(land[, c("hhid", "parcel")]), ]

	d <- data.frame(
		hhid = pot$hhid,
		parcel = pot$parcel,
		variety = trimws(as.character(pot$P4AVariedadNomb)),
		production_kg_ = pot$production_kg_,
		stringsAsFactors = FALSE
	)
	d$variety[!nzchar(d$variety) | d$variety == "NA"] <- NA_character_
	d <- merge(d, land, by=c("hhid", "parcel"), all.x=TRUE)
	n_on_parcel <- ave(d$hhid, d$hhid, d$parcel, FUN=length)
	one <- n_on_parcel == 1 & !is.na(d$area_ha) & d$area_ha > 0 & !is.na(d$production_kg_)
	d$yield <- NA_real_
	d$yield[one] <- d$production_kg_[one] / d$area_ha[one]
	d$area_ha[!one] <- NA_real_

	geo$hhid <- trimws(as.character(geo$CodigoEnc))
	geo <- geo[!duplicated(geo$hhid), ]
	d <- merge(d, geo[, c("hhid", "P0Latitud", "P0Longitud", "P0Altitud")], by="hhid", all.x=TRUE)

	d <- data.frame(
		country = "Peru",
		hhid = d$hhid,
		variety = d$variety,
		treatment = d$variety,
		crop = "potato",
		yield = d$yield,
		yield_part = "tubers",
		production_kg_ = d$production_kg_,
		latitude = -dms(d$P0Latitud),
		longitude = -dms(d$P0Longitud),
		elevation = num(d$P0Altitud),
		stringsAsFactors = FALSE
	)
	d$geo_from_source <- !is.na(d$latitude) & !is.na(d$longitude)
	d$is_survey <- TRUE
	d$on_farm <- TRUE
	d$trial_id <- d$hhid
	keep <- !is.na(d$variety) | !is.na(d$production_kg_)
	d <- d[keep, ]

	carobiner::write_files(path, meta, d)
}
