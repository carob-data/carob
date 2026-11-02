# R script for "carob"
# license: GPL (>=3)

## NOTES
# Hayami's 1974 benchmark (complete enumeration) survey of Tubuan village, Pila, Laguna ("Anatomy of Peasant Economy"). Same village as doi:10.7910/DVN/26317 (1976), 26322 (1995) and 26323 (1996).
# Records are household x season x parcel from sheets ProdnWS (wet season 1974) and ProdnDS (dry season "1974"). SEAS code 1 = wet, 0 = dry (coding manual).
# AREA is ha. PRODN (production) and the disposal columns are in cavans of palay (coding manual); kg per cavan is not given, so production is kept in cavans (production_) and yield is not computed.
# Current inputs (CURRWs) exist for the wet season only and are reported per household (not per parcel), in kg of product (FERTKG, SEEDKG). They were divided by the household wet-season rice area (sum of ProdnWS AREA, which matches CRetWS AREA) and assigned to every wet-season parcel of the household, i.e. a uniform rate across parcels is assumed.
# Fertilizer codes: UREA -> urea; SULP -> DAS (ammonium sulfate, assumed); COMP -> NPK (complete fertilizer, grade not given); T162 -> "16-20-0" (assumed from the code); GUANO -> OM. N_fertilizer is only computed for households that used only urea and/or ammonium sulfate.

## ISSUES
# No coordinates. Single village: Tubuan, Pila, Laguna (location/adm2/adm1 set); georeference Tubuan manually.
# kg per cavan of palay not documented; production_ is in cavans. The 1995 resurvey of the same village (doi:10.7910/DVN/26322) uses 45 kg/cavan in its summary rows; confirm this applies to 1974 before computing yield.
# The dry-season sheet is labelled "DRY Season 1974"; interviews were in Nov 1974, so this is probably the 1973/74 dry season. No planting or harvest dates.
# Fertilizer kind codes (SULP, COMP, T162, GUANO) and insecticide/herbicide codes (HYTO, ERED, TF-D, ...) are explained in an Appendix that is not in the dataset. NPK grade of COMP unknown, so N/P/K not computed for those households.
# Herbicide amounts are in ml of product (HERBML) or kg (HERBKG); insecticide in ml (INSTML) or kg (INSTKG): not converted to active ingredient; only product codes kept.
# Tenure codes (L, S, O) presumably leasehold, share tenant, owner (Landholdings sheet uses those words); kept as codes.
# Two parcels have no production (e.g. hh 541, 5 ha, wet season). Wet-season households without a CURRWs fertilizer row get NA (not 0) for fertilizer: unclear whether that means none used.
# Unmapped: HHINFO (members), Assets, Landholdings, CRetWS (costs/returns), HiredWs, HerbWs, PowerWs, WageWs, Off-Farm Income, Nrice income, and the disposal columns (sold, consumed, stored, landlord share) of ProdnWS/ProdnDS.
# write_files: missing longitude/latitude/geo_from_source; unknown variables farm_no_, production_, crop_price_cavan_, fertilizer_codes_, insecticide_codes_, herbicide_codes_; invalid fertilizer_type "16-20-0" (not in terminag; kept).

## NEW VARIABLES
# farm_no_: original farm number (FNO) of the household in the survey year
# production_: rice (palay) production of the parcel, cavans
# crop_price_cavan_: palay price received, pesos per cavan
# fertilizer_codes_: raw fertilizer kind codes reported by the household (wet season)
# insecticide_codes_: raw insecticide product codes (wet season)
# herbicide_codes_: raw herbicide product codes (wet season)


carob_script <- function(path) {

"
Preliminary Survey - Anatomy of Peasant Economy

To describe the major socio-economic characteristics of a typical rice village in Southern Luzon through a complete enumeration survey of the village.   This is a benchmark survey for the integrated farm household record-keeping project, titled 'Anatomy of Peasant Economy'.
"

	uri <- "doi:10.7910/DVN/26316"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = "Anatomy of Peasant Economy",
		design = "complete enumeration of the village",
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 35,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "Tubuan_1974.xls"]
	rW <- carobiner::read.excel(f, sheet="ProdnWS", skip=4, na=".")
	rD <- carobiner::read.excel(f, sheet="ProdnDS", skip=4, na=".")
	rI <- carobiner::read.excel(f, sheet="CURRWs", skip=4, na=".")

	prod <- function(r, seas) {
		data.frame(
			hhid = as.character(r$Hhcode),
			farm_no_ = as.character(r$FNO),
			field_id = as.character(r$PAR),
			season = seas,
			field_size = as.numeric(r$AREA),
			land_tenure = r[[7]],
			production_ = as.numeric(r$PRODN),
			crop_price_cavan_ = as.numeric(r$PRICE)
		)
	}
	d <- rbind(prod(rW, "wet"), prod(rD, "dry"))
	d <- d[!is.na(d$hhid), ]

	# household-level wet season inputs
	rI <- rI[!is.na(rI$Hhcode), ]
	rI$hhid <- as.character(rI$Hhcode)
	area <- tapply(d$field_size[d$season == "wet"], d$hhid[d$season == "wet"], sum)

	ftype <- c(UREA="urea", SULP="DAS", COMP="NPK", T162="16-20-0")
	rI$fert <- ftype[rI$FERTKD]
	rI$fkg <- as.numeric(rI$FERTKG)
	# NA for products with unknown grade, so that the household N total becomes NA
	rI$Nkg <- rI$fkg * c(urea=0.46, DAS=0.21, NPK=NA, "16-20-0"=NA)[rI$fert]
	inorg <- rI[!is.na(rI$fert), ]
	paste_u <- function(x) {
		x <- unique(x[!is.na(x)])
		if (length(x) == 0) NA else paste(x, collapse="; ")
	}
	sum_na <- function(x) {
		if (all(is.na(x))) NA else sum(x, na.rm=TRUE)
	}

	hh <- data.frame(hhid = unique(rI$hhid))
	agg <- function(x, by, fun) {
		as.vector(tapply(x, by, fun)[hh$hhid])
	}
	om <- rI$FERTKD %in% "GUANO"
	hh$fertilizer_type <- agg(rI$fert, rI$hhid, paste_u)
	hh$fertilizer_codes_ <- agg(rI$FERTKD, rI$hhid, paste_u)
	hh$fkg <- agg(inorg$fkg, inorg$hhid, sum)
	hh$Nkg <- agg(inorg$Nkg, inorg$hhid, sum)
	hh$OMkg <- agg(rI$fkg[om], rI$hhid[om], sum)
	hh$seed <- agg(as.numeric(rI$SEEDKG), rI$hhid, sum_na)
	hh$insecticide_codes_ <- agg(rI$INSTKD, rI$hhid, paste_u)
	hh$herbicide_codes_ <- agg(rI$HERBKD, rI$hhid, paste_u)
	hh$area <- as.vector(area[hh$hhid])

	i <- match(d$hhid, hh$hhid)
	i[d$season != "wet"] <- NA
	d$fertilizer_type <- hh$fertilizer_type[i]
	d$fertilizer_codes_ <- hh$fertilizer_codes_[i]
	d$fertilizer_amount <- as.numeric(hh$fkg[i] / hh$area[i])
	d$N_fertilizer <- as.numeric(hh$Nkg[i] / hh$area[i])
	d$OM_type <- ifelse(is.na(hh$OMkg[i]), NA, "guano")
	d$OM_amount <- as.numeric(hh$OMkg[i] / hh$area[i])
	d$seed_rate <- as.numeric(hh$seed[i] / hh$area[i])
	d$insecticide_codes_ <- hh$insecticide_codes_[i]
	d$herbicide_codes_ <- hh$herbicide_codes_[i]
	# wet-season households without any CURRWs record (or without fertilizer rows) are left NA, not 0
	d$insecticide_used <- ifelse(is.na(i), NA, !is.na(d$insecticide_codes_))
	d$herbicide_used <- ifelse(is.na(i), NA, !is.na(d$herbicide_codes_))

	d$currency <- "PHP"
	d$country <- "Philippines"
	d$adm1 <- "Laguna"
	d$adm2 <- "Pila"
	d$location <- "Tubuan"
	d$crop <- "rice"
	d$yield_part <- "grain"
	d$trial_id <- paste(d$hhid, d$season, sep="_")
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
