# R script for "carob"
# license: GPL (>=3)

## NOTES
# 1996 Laguna Village Farmer Resurvey of Tubuan village, Pila, Laguna (Hayami & Kikuchi; farming households only). Same village as doi:10.7910/DVN/26316 (1974), 26317 (1976) and 26322 (1995). Interviews in July 1996.
# Records are household x season x parcel (rice area) from ProdnWS (wet season 1995) and ProdDS (dry season 1996). There is no parcel number in these sheets.
# Production (TOTAL YIELD) and disposal columns are in cavans of palay; production kept in cavans (production_), yield not computed.
# Water source shares (%, NIA irrigation, pump, rain/creek) are kept as new variables; irrigated = TRUE if NIA or pump share > 0, FALSE if only rain; irrigation_source lists NIA canal and/or pump.
# Harvest dates are written in many ways ("2nd wk Oct", "Nov 6th", "Oct 30th-Nov 5th"); only year-month (first month mentioned) is kept. Wet season 1995 harvests in Jan are 1996.
# Current inputs (CurrentWS, CurrentDS) are per household and season, not per parcel. Fertilizer types and pesticide names are attached to all parcels of the household in that season.
# Fertilizer kinds: UREA -> urea, 14-14-14 and COMPLETE -> NPK, 21-0-0 -> DAS, 0-0-60 -> KCl; CHICKEN MANURE -> OM_type "poultry manure", PIG MANURE -> "animal dung".
# MAJOR DAMAGE text mapped to stress: typhoon -> wind; flooded -> flood; drought, insufficient/lack of water, water shortage -> drought; rats -> rodents; snails, army worms, stem borer, insect damage, white heads -> pests. Raw text in damage_.

## ISSUES
# No coordinates. Single village: Tubuan, Pila, Laguna (location/adm2/adm1 set); georeference Tubuan manually.
# kg per cavan not given in this dataset (the 1995 resurvey, doi:10.7910/DVN/26322, uses 45 kg/cavan in its summary rows); production_ is in cavans and yield is not computed.
# Units of input quantities are not documented (numbers of bags? some entries "250ml"), so no fertilizer amounts, N rates or seed rates were computed.
# Fertilizer kind "MRG" not mapped (kept in fertilizer_codes_).
# Pesticide products are brand names (MACHETE, 2-4D, CYMBUSH, KARATE, PORSNAIL, BAYLUSCIDE, RACUMIN (rodenticide), ...); kept as text in herbicide_names_ and pesticide_names_ (insecticides, molluscicides, rodenticides together).
# Typhoon mapped to "wind" (could also be flood); "Pump destroyed" not mapped.
# Varieties kept as reported ("RC-14", "RC 14", "C-14" probably the same PSB Rc14; "RC-14,Burdogol" mixed).
# Tenure codes (L, O, S, CLT, M, SBE.S, SBE.M.L) kept as codes.
# Dropped: dry season rows "No dry season".
# Unmapped: HMember, Assets, Landholdings, disposal and share columns, rent, NIA fee, OutletWS/OutletDS (sales, price per cavan), Costsharing, Power, Labor and Irrigmethod sheets; fuel and other costs.
# Publication: IRRI Social Sciences Division discussion paper 98-06 (no DOI).
# write_files: missing longitude/latitude/geo_from_source; unknown variables production_, damage_, fertilizer_codes_, herbicide_names_, pesticide_names_, irr_nia_perc_, irr_pump_perc_, irr_rain_perc_.

## NEW VARIABLES
# production_: rice (palay) production of the parcel, cavans
# damage_: major crop damage as reported (text)
# fertilizer_codes_: fertilizer kinds as reported by the household for the season
# herbicide_names_: herbicide (brand) names reported by the household for the season
# pesticide_names_: insecticide, molluscicide and rodenticide (brand) names reported by the household for the season
# irr_nia_perc_: share of water from NIA irrigation, %
# irr_pump_perc_: share of water from pump irrigation, %
# irr_rain_perc_: share of water from rain (dry season: rain/creek), %


carob_script <- function(path) {

"
Laguna Village Farmer Resurvey

To examine how the influence of various modernizing forces affect the income level of the villagers, and how income distribution among household has changed over time.
"

	uri <- "doi:10.7910/DVN/26323"
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

	f <- ff[basename(ff) == "LVFResurvey1.xls"]
	rW <- carobiner::read.excel(f, sheet="ProdnWS", skip=10, col_types="text")
	rD <- carobiner::read.excel(f, sheet="ProdDS", skip=9, col_types="text")
	iW <- carobiner::read.excel(f, sheet="CurrentWS", skip=11, col_types="text")
	iD <- carobiner::read.excel(f, sheet="CurrentDS", skip=10, col_types="text")

	mon <- function(x) {
		m <- regmatches(x, regexpr("Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec", x, ignore.case=TRUE))
		out <- rep(NA, length(x))
		out[grepl("Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec", x, ignore.case=TRUE)] <- match(tolower(m), tolower(month.abb))
		out
	}

	prod <- function(r, seas) {
		names(r) <- sub("WS$|DS$|D$", "", names(r))
		r <- r[!is.na(r$HHCODE) & !grepl("No dry", r$AREA, ignore.case=TRUE), ]
		hm <- mon(r$HVDATE)
		hy <- if (seas == "wet") ifelse(hm < 6, 1996, 1995) else 1996
		nia <- as.numeric(r$IRGNIA)
		pump <- as.numeric(r$IRGPUMP)
		rain <- as.numeric(r$IRGRAIN)
		src <- cbind(ifelse(nia > 0, "NIA canal", NA), ifelse(pump > 0, "pump", NA))
		data.frame(
			hhid = r$HHCODE,
			season = seas,
			field_size = as.numeric(r$AREA),
			land_tenure = r$TENR,
			production_ = as.numeric(r$PRODN),
			variety = r$VAR,
			damage_ = r$CROPDM,
			harvest_date = ifelse(is.na(hm), NA, sprintf("%d-%02d", hy, hm)),
			irr_nia_perc_ = nia,
			irr_pump_perc_ = pump,
			irr_rain_perc_ = rain,
			irrigated = ifelse(is.na(nia) & is.na(pump) & is.na(rain), NA, (!is.na(nia) & nia > 0) | (!is.na(pump) & pump > 0)),
			irrigation_source = apply(src, 1, function(x) if (all(is.na(x))) NA else paste(x[!is.na(x)], collapse="; "))
		)
	}
	d <- rbind(prod(rW, "wet"), prod(rD, "dry"))

	dm <- tolower(d$damage_)
	st <- cbind(
		ifelse(grepl("typhoon", dm), "wind", NA),
		ifelse(grepl("flood", dm), "flood", NA),
		ifelse(grepl("drought|insufficient water|lack of water|water shortage", dm), "drought", NA),
		ifelse(grepl("rat", dm), "rodents", NA),
		ifelse(grepl("snail|army|borer|insect|white head", dm), "pests", NA)
	)
	d$stress <- apply(st, 1, function(x) if (all(is.na(x))) NA else paste(x[!is.na(x)], collapse="; "))

	inp <- function(r, seas) {
		names(r) <- sub("W$|D$", "", names(r))
		r <- r[!is.na(r$HHCODE), ]
		data.frame(hhid = r$HHCODE, season = seas, input = toupper(trimws(r$INPUT)), kind = toupper(trimws(r$KIND)))
	}
	i <- rbind(inp(iW, "wet"), inp(iD, "dry"))
	i$key <- paste(i$hhid, i$season)
	fe <- i[grepl("^FERT", i$input) & !is.na(i$kind), ]
	he <- i[grepl("^HERB", i$input) & !is.na(i$kind), ]
	pe <- i[grepl("^INSEC|^PEST", i$input) & !is.na(i$kind), ]

	ftype <- c("UREA"="urea", "14-14-14"="NPK", "COMPLETE"="NPK", "21-0-0"="DAS", "0-0-60"="KCl")
	omtype <- c("CHICKEN MANURE"="poultry manure", "PIG MANURE"="animal dung")
	paste_u <- function(x) {
		x <- unique(x[!is.na(x)])
		if (length(x) == 0) NA else paste(x, collapse="; ")
	}
	agg <- function(x, by) {
		as.vector(tapply(x, by, paste_u)[paste(d$hhid, d$season)])
	}
	d$fertilizer_type <- agg(ftype[fe$kind], fe$key)
	d$fertilizer_codes_ <- agg(fe$kind, fe$key)
	d$OM_type <- agg(omtype[fe$kind], fe$key)
	d$herbicide_names_ <- agg(he$kind, he$key)
	d$pesticide_names_ <- agg(pe$kind, pe$key)
	# households with any input record in that season; others are left NA
	k <- paste(d$hhid, d$season) %in% i$key
	d$herbicide_used <- ifelse(k, !is.na(d$herbicide_names_), NA)

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
