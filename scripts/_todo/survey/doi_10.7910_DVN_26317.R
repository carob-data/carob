# R script for "carob"
# license: GPL (>=3)

## NOTES
# Terminal (1976) survey of Tubuan village, Pila, Laguna ("Anatomy of Peasant Economy"). Same village as doi:10.7910/DVN/26316 (1974), 26322 (1995) and 26323 (1996); same file structure and codes as the 1974 survey.
# Records are household x season x parcel from sheets ProdnWs76 (wet season 1976) and ProdnDs76 (dry season 1976). SEAS 1 = wet, 0 = dry.
# AREA is ha. PRODN and the disposal columns are in cavans of palay; kg per cavan is not given, so production is kept in cavans (production_) and yield is not computed.
# Current inputs (CurrWs76) are for the wet season only and reported per household, in kg of product (FERT KG, SEED KG). They were divided by the household wet-season rice area (sum of ProdnWs76 AREA) and assigned to every wet-season parcel of the household (uniform rate assumed).
# Fertilizer codes: UREA -> urea; SULP -> DAS (ammonium sulfate, assumed); COMP -> NPK (complete fertilizer, grade not given); T162 -> "16-20-0" (assumed from the code). N_fertilizer only computed for households that used only urea and/or ammonium sulfate.
# DAMAGE text mapped to stress (leafhoppers -> pests, Tungro -> disease, flooded -> flood, rats -> rodents); raw text kept in damage_. "due to" and "infected" are incomplete and not mapped.

## ISSUES
# No coordinates. Single village: Tubuan, Pila, Laguna (location/adm2/adm1 set); georeference Tubuan manually.
# kg per cavan of palay not documented; production_ is in cavans. The 1995 resurvey of the same village (doi:10.7910/DVN/26322) uses 45 kg/cavan in its summary rows; confirm this applies to 1976 before computing yield.
# Unclear which calendar years the "dry season 1976" refers to (1975/76 or 1976/77). No planting or harvest dates.
# Fertilizer, insecticide and herbicide codes (SULP, COMP, T162, HYTO, BROD, FURA, TF-D, ...) are explained in an Appendix that is not in the dataset. NPK grade of COMP unknown.
# hh 261 reports 3500 kg urea (1842 kg/ha) but a fertilizer cost of 633.5 pesos (0.18 peso/kg vs ~1.2-1.7 for other households): probably 350 kg; set to NA. Other rates up to ~340 kg/ha product remain.
# Insecticide (ml or kg) and herbicide (ml) amounts are product amounts; not converted; only product codes kept.
# Households 71 and 95 have two wet-season records with PAR 1 (different tenure), so field_id is not unique within household-season.
# Variety names kept as reported ("IR26 + 32", "C4 green", "C4-37", "C4-137", "C-4-137", "glrice", "gl.rice", "Malagkit" = glutinous rice); harmonize if needed.
# Dropped: hh 398 wet season (no data) and hh 342 dry season ("no dry" season crop). Two parcels have no production.
# Tenure codes (L, S, O, M, SBR-L, SBR-S, L-S/SBR, ...) kept as codes; meaning of SBR/SBE not documented in the dataset.
# Unmapped: HHold76, ASSET76, Tenure76, labour sheets, PowerWs76, OwnCTr, CRetWs76 (costs/returns), WageWs76, OffarmInc76, NRiceInc76, Debt76 and others; disposal columns of the production sheets.
# write_files: missing longitude/latitude/geo_from_source; unknown variables farm_no_, production_, crop_price_cavan_, damage_, fertilizer_codes_, insecticide_codes_, herbicide_codes_; invalid fertilizer_type "16-20-0".

## NEW VARIABLES
# farm_no_: original farm number (FNO) of the household
# production_: rice (palay) production of the parcel, cavans
# crop_price_cavan_: palay price received, pesos per cavan
# damage_: crop damage as reported (text)
# fertilizer_codes_: raw fertilizer kind codes reported by the household (wet season)
# insecticide_codes_: raw insecticide product codes (wet season)
# herbicide_codes_: raw herbicide product codes (wet season)


carob_script <- function(path) {

"
Terminal Survey - Anatomy of Peasant Economy

The project 'Anatomy of Peasant Economy' seeks to document the production, income-expenditure, and saving-investment activities of rural households .
"

	uri <- "doi:10.7910/DVN/26317"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = "Anatomy of Peasant Economy",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 35,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "Tubuan_1976.xls"]
	rW <- carobiner::read.excel(f, sheet="ProdnWs76", skip=4, na=".")
	rD <- carobiner::read.excel(f, sheet="ProdnDs76", skip=4, na=".", col_types="text")
	# two header rows
	rI <- carobiner::read.excel(f, sheet="CurrWs76", skip=6, na=".", col_names=FALSE)
	names(rI) <- c("Hhcode", "FNO", "YEAR", "SEASON", "FERTKD", "FERTKG", "INSTKD", "INSTML", "INSTKG", "HERBKD", "HERBML", "HERBKG", "SEEDKG", "GASL")

	# "no dry" (no dry season crop) in AREA
	rD <- rD[!(rD$AREA %in% "no dry"), ]

	prod <- function(r, seas) {
		data.frame(
			hhid = as.character(r$HHcode),
			farm_no_ = as.character(r$FNO),
			field_id = as.character(r$PAR),
			season = seas,
			field_size = as.numeric(r$AREA),
			land_tenure = r[[7]],
			production_ = as.numeric(r$PRODN),
			crop_price_cavan_ = as.numeric(r$PRICE),
			variety = r[["VAR."]],
			damage_ = r$DAMAGE
		)
	}
	d <- rbind(prod(rW, "wet"), prod(rD, "dry"))
	d <- d[!is.na(d$hhid) & !is.na(d$field_size), ]

	dm <- tolower(d$damage_)
	st <- cbind(
		ifelse(grepl("leafhopper", dm), "pests", NA),
		ifelse(grepl("tungro", dm), "disease", NA),
		ifelse(grepl("flood", dm), "flood", NA),
		ifelse(grepl("rats", dm), "rodents", NA)
	)
	d$stress <- apply(st, 1, function(x) if (all(is.na(x))) NA else paste(x[!is.na(x)], collapse="; "))

	# household-level wet season inputs
	rI <- rI[!is.na(rI$Hhcode), ]
	rI$hhid <- as.character(rI$Hhcode)
	area <- tapply(d$field_size[d$season == "wet"], d$hhid[d$season == "wet"], sum)

	ftype <- c(UREA="urea", SULP="DAS", COMP="NPK", T162="16-20-0")
	rI$fert <- ftype[rI$FERTKD]
	rI$fkg <- as.numeric(rI$FERTKG)
	# hh 261: 3500 kg urea for 633.5 pesos (CRetWs76 FERTC), ten times cheaper per kg than other households
	rI$fkg[rI$hhid == "261" & rI$FERTKD %in% "UREA"] <- NA
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
	hh$fertilizer_type <- agg(rI$fert, rI$hhid, paste_u)
	hh$fertilizer_codes_ <- agg(rI$FERTKD, rI$hhid, paste_u)
	hh$fkg <- agg(inorg$fkg, inorg$hhid, sum)
	hh$Nkg <- agg(inorg$Nkg, inorg$hhid, sum)
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
	d$seed_rate <- as.numeric(hh$seed[i] / hh$area[i])
	d$insecticide_codes_ <- hh$insecticide_codes_[i]
	d$herbicide_codes_ <- hh$herbicide_codes_[i]
	# wet-season households without any CurrWs76 record are left NA, not FALSE
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
