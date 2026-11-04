# R script for "carob"
# license: GPL (>=3)

## NOTES
# 1995 resurvey of Tubuan village, Pila, Laguna (Hayami & Kikuchi). Same village as doi:10.7910/DVN/26316 (1974), 26317 (1976) and 26323 (1996).
# Records are household x season x parcel (rice area) from ProdnWS94 (wet season 1994) and ProdnDS95 (dry season 1994/95). There is no parcel number in these sheets.
# Production is in cavans of palay. The sheets have summary rows giving both "Yield cav/ha" and "Yield kg/ha", which imply 45 kg per cavan; that conversion is used here for yield (kg/ha, moisture not given).
# Harvest dates are given as month and week of month ("Oct 2nd"); only year-month is kept. Wet season harvests in Sep-Dec are 1994, in Jan 1995; dry season harvests (Mar-May) are 1995.
# Current inputs (CurrentWS, CurrentDS in "Tubuan 1995_2.xls") are listed per household and season, not per parcel. Fertilizer types and pesticide product names were attached to all parcels of the household in that season.
# Fertilizer kinds: UREA -> urea, 14-14-14 -> NPK, 21-0-0 -> DAS, 0-0-60 -> KCl, CHICKEN MANURE -> OM_type "poultry manure".

## ISSUES
# No coordinates. Single village: Tubuan, Pila, Laguna (location/adm2/adm1 set); georeference Tubuan manually.
# Units of input quantities (QTYWS/QTYDS) are not documented (cost per unit of urea is ~375 pesos, which suggests 50 kg bags, but this is not stated), so no fertilizer amounts, N rates or seed rates were computed.
# Fertilizer kind "143" (8 records, dry season) and "LIQUID FERT(MRG)" not mapped to fertilizer_type (kept in fertilizer_codes_).
# Pesticide products are brand names (MACHETE, 2-4D, CYMBUSH, KARATE, BRODAN, PORSNAIL, ...); kept as text (herbicide_names_, insecticide_names_) as herbicide_product/insecticide_product need active ingredients. PORSNAIL/POLSNAIL/BAYLUSIDE are molluscicides listed under insecticides/pesticides.
# CROPDMWS/CROPDMDS only say "Yes" (crop damaged), without the type of damage; kept as crop_damage_.
# Dry-season records with "DID NOT HARVEST" have production NA (yield not set to 0); write_files reports "NA detected: yield" for these 4 records.
# Highest yields (up to 10.5 t/ha, 233.6 cavans/ha) should be checked.
# Varieties kept as reported (RC10, R10, C10 are probably the same PSB Rc10 etc.; "RC40/RC14/Mlagkit" mixed).
# Tenure codes (L, O, S, CLT, M, SBE.S, "O-ovseer", ...) kept as codes; "NO DATA" set to NA.
# Unmapped: Basicinfo, Householdmember, Assets, Nonriceincome, Landholdings, rent and harvest/thresher shares, OutletWD/OutletDS (palay sales, price per cavan), CostShare, Power, Labor, Hired, Gama and credit sheets; fuel and irrigation fees in the input sheets.
# write_files: missing longitude/latitude/geo_from_source; unknown variables production_, crop_damage_, fertilizer_codes_, herbicide_names_, insecticide_names_.

## NEW VARIABLES
# production_: rice (palay) production of the parcel, cavans
# crop_damage_: "Yes" if the respondent reported crop damage (type not given)
# fertilizer_codes_: fertilizer kinds as reported by the household for the season
# herbicide_names_: herbicide (brand) names reported by the household for the season
# insecticide_names_: insecticide/molluscicide (brand) names reported by the household for the season


carob_script <- function(path) {

"
Laguna Village Resurvey

To examine how such a pattern of agrarian change has emerged through the interaction of land reform with strong population pressure and the development of new technology.  To trace the sudden surge of cottage manufacturing that occurred in the village - that increased employment opportunities for villagers and pulled significant amount of external labor into the village.
"

	uri <- "doi:10.7910/DVN/26322"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 40,
		carob_effort = 0.4
	)

	f1 <- ff[basename(ff) == "Tubuan 1995_1.xls"]
	f2 <- ff[basename(ff) == "Tubuan 1995_2.xls"]
	rW <- carobiner::read.excel(f1, sheet="ProdnWS94", skip=1, col_types="text")
	rD <- carobiner::read.excel(f1, sheet="ProdnDS95", skip=2, col_types="text")
	iW <- carobiner::read.excel(f2, sheet="CurrentWS", skip=2, col_types="text")
	iD <- carobiner::read.excel(f2, sheet="CurrentDS", skip=2, col_types="text")

	prod <- function(r, seas) {
		names(r) <- sub("WS$|DS$", "", names(r))
		# drop summary rows and households without a crop in this season ("NO ... SEASON CROP")
		r <- r[!is.na(r$HHCODE) & !grepl("NO ", r$AREA), ]
		prd <- ifelse(r$PRODN %in% "DID NOT HARVEST", NA, r$PRODN)
		hv <- substr(r$HVDATE, 1, 3)
		hm <- match(hv, month.abb)
		hy <- if (seas == "wet") ifelse(hm < 6, 1995, 1994) else 1995
		data.frame(
			hhid = r$HHCODE,
			season = seas,
			field_size = as.numeric(r$AREA),
			land_tenure = ifelse(r$TENR %in% "NO DATA", NA, r$TENR),
			production_ = as.numeric(prd),
			variety = r$VAR,
			crop_damage_ = r$CROPDM,
			harvest_date = ifelse(is.na(hm), NA, sprintf("%d-%02d", hy, hm))
		)
	}
	d <- rbind(prod(rW, "wet"), prod(rD, "dry"))
	d$yield <- d$production_ * 45 / d$field_size

	inp <- function(r, seas) {
		names(r) <- sub("WS$|DS$", "", names(r))
		r <- r[!is.na(r$HHCODE), ]
		data.frame(hhid = r$HHCODE, season = seas, input = toupper(trimws(r$INPUT)), kind = toupper(trimws(r$KIND)))
	}
	i <- rbind(inp(iW, "wet"), inp(iD, "dry"))
	i$key <- paste(i$hhid, i$season)
	fe <- i[grepl("^FERT", i$input) & !is.na(i$kind), ]
	he <- i[grepl("^HERB", i$input) & !is.na(i$kind), ]
	ins <- i[grepl("^INSEC|^INCEC|^PEST", i$input) & !is.na(i$kind) & i$kind != "NA", ]

	ftype <- c("UREA"="urea", "14-14-14"="NPK", "21-0-0"="DAS", "0-0-60"="KCl")
	fe$type <- ftype[fe$kind]
	paste_u <- function(x) {
		x <- unique(x[!is.na(x)])
		if (length(x) == 0) NA else paste(x, collapse="; ")
	}
	agg <- function(x, by) {
		as.vector(tapply(x, by, paste_u)[paste(d$hhid, d$season)])
	}
	d$fertilizer_type <- agg(fe$type, fe$key)
	d$fertilizer_codes_ <- agg(fe$kind, fe$key)
	d$OM_type <- agg(ifelse(fe$kind == "CHICKEN MANURE", "poultry manure", NA), fe$key)
	d$herbicide_names_ <- agg(he$kind, he$key)
	d$insecticide_names_ <- agg(ins$kind, ins$key)
	# households with any input record in that season; others are left NA
	k <- paste(d$hhid, d$season) %in% i$key
	d$herbicide_used <- ifelse(k, !is.na(d$herbicide_names_), NA)
	d$insecticide_used <- ifelse(k, !is.na(d$insecticide_names_), NA)

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
