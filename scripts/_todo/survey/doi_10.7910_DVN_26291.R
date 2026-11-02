# R script for "carob"
# license: GPL (>=3)

## NOTES
# Nationwide survey of 769 rice farmers in 38 villages of 19 municipalities in 19 provinces, on rice cultivar diversity and yield losses.
# DUPLICATION: doi:10.7910/DVN/EA8PTP ("Land use, livelihoods and rice variety diversity...") uses "the same data set" (its Project Details); its Landholdings.xls is a cleaned, coded version of the LANDHOLDING sheet used here (same RECNO, same parcels). A human should decide which of the two to keep; EA8PTP has the crop areas filled in that are 0 or missing here.
# Records are parcel x season from sheet LANDHOLDING of "allfiles-as of dec905.xls": crop 1 = wet season, crop 2 = dry season; only rice records are included.
# yield = production (cavans) x kg per cavan (KGCAV, reported by the farmer) / crop area (CPAREA). yield_loss_ is computed the same way from LOSS (cavans).
# Provinces from the province code (PCODE, matching the codebook order); municipality and village names from the text columns (MUNPLTY, VILLAGE), with abbreviations spelled out ("Sn" -> "San").
# Reasons for crop loss (free text) mapped to stress with the same keyword rules as the EA8PTP script; raw text in loss_reason_.

## ISSUES
# No coordinates. Locations are villages (location) in municipalities (adm2) and provinces (adm1). Georeference manually.
# Crop area of the dry season crop (CPAREA2) is 0 or missing for many records although production is reported; yield and yield_loss_ are NA there (EA8PTP has these areas).
# Some yields are implausible (up to 264 t/ha; e.g. 600 cavans on 0.1 ha in the wet season, and many dry season records with CPAREA2 0.5 ha and 100-550 cavans, RECNO ~180-250); not removed. In EA8PTP the maximum yield is 15 t/ha (not checked record by record).
# PCODE missing for 9 records (adm1 NA); province could be taken from the municipality.
# Village names not harmonized ("Pinagsabangan", "Pinagsabangan 1", "Pinagsabangan 2").
# Year: Dataverse metadata gives 2002-2003; EA8PTP "Project Details" says 2003 wet and 2004 dry seasons. No dates set.
# Irrigation code (IRRCODE) has 16 values; only 1 gravity/NIA, 2 pump, 3 communal, 4 deep well, 5 rainfed are interpreted (irrigated TRUE for 1-4, FALSE for 5); raw source text in irrigation_source.
# Tenure codes 6 and 7 (combinations, "porcientuhan", "owned by brother") kept as the raw text.
# 27 rows at the end of the sheet without RECNO (stray numbers) were dropped. -99 = missing.
# Reason codes not documented; only the text was used.
# Unmapped sheets: BASICNFO, TENURE, SEEDS, WSLASTYR, DSLASTYR, WS10YR, DS10YR (yields last year and 10 years ago), PERCEPTION, IPMKAP, BORROWING, SPRDAS, PRIORTY420K, FRNDLYINS, PESTUSE (pesticide use).
# write_files: missing longitude/latitude/geo_from_source; unknown variables yield_loss_, loss_reason_; out of bounds yield (0, 264000); high crop yield; NA detected: yield.

## NEW VARIABLES
# yield_loss_: reported crop loss, kg/ha
# loss_reason_: reason for crop losses as reported (text)


carob_script <- function(path) {

"
Survey on Rice Cultivar Diversity and Yield Losses from Pests

A survey covering 19 major rice-producing provinces in the Philippines was conducted to determine rice yield loss caused by major insect pests, diseases and other biotic and abiotic stresses.  Analysis was done at the national and at the provincial level, the latter will determine the 'hot spots' for major pests.
"

	uri <- "doi:10.7910/DVN/26291"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 40,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "allfiles-as of dec905.xls"]
	r <- carobiner::read.excel(f, sheet="LANDHOLDING", col_types="text")
	r <- r[!is.na(r$RECNO), 1:51]

	num <- function(x) {
		x <- as.numeric(x)
		x[x %in% c(-99, -999)] <- NA
		x
	}

	prov <- c("Ilocos Norte", "Pangasinan", "Isabela", "Nueva Vizcaya", "Nueva Ecija", "Bulacan", "Camarines Sur", "Albay", "Oriental Mindoro", "Palawan", "Capiz", "Iloilo", "Antique", "Bohol", "Leyte", "Zamboanga del Sur", "Bukidnon", "North Cotabato", "Davao del Sur")
	mun <- gsub("^Sn ", "San ", r$MUNPLTY)
	mun <- carobiner::replace_values(mun, c("San Idlefonso", "Brookes Point", "Zaragosa"), c("San Ildefonso", "Brooke's Point", "Zaragoza"))
	vil <- gsub("^Sn ", "San ", r$VILLAGE)

	tcode <- num(r$TENCODE)
	tenure <- c("owned", "CLT holder", "sharecropped", "fixed rent", "mortgaged")[tcode]
	tenure[tcode %in% 6:7] <- tolower(r$TENURE[tcode %in% 6:7])
	icode <- num(r$IRRCODE)

	ssn <- function(seas, area, crop, var, prod, loss, kgcav, reason) {
		kg <- num(r[[kgcav]])
		kg[kg %in% 0] <- NA
		a <- num(r[[area]])
		a[a %in% 0] <- NA
		data.frame(
			hhid = r$RECNO,
			field_id = r$SERIALNO,
			adm1 = prov[num(r$PCODE)],
			adm2 = mun,
			location = vil,
			field_size = num(r$PARAREA),
			landscape_position = c("upland", "medium land", "lowland", "rolling", "combined")[num(r$ELEV)],
			land_tenure = tenure,
			irrigated = ifelse(icode %in% 1:4, TRUE, ifelse(icode %in% 5, FALSE, NA)),
			irrigation_source = r$SORSIRR,
			season = seas,
			crop_ = toupper(r[[crop]]),
			variety = r[[var]],
			yield = num(r[[prod]]) * kg / a,
			yield_loss_ = num(r[[loss]]) * kg / a,
			loss_reason_ = r[[reason]]
		)
	}
	d <- rbind(
		ssn("wet", "CPAREA1", "CROP1", "WSVAR", "PROD1", "LOSS1", "KGCAV1", "REASON1"),
		ssn("dry", "CPAREA2", "CROP2", "DSVAR", "PROD2", "LOSS2", "KGCAV2", "REASON2")
	)
	d <- d[d$crop_ %in% "RICE", ]
	d$crop_ <- NULL
	d$loss_reason_[d$loss_reason_ %in% c("NA", "0", "-99")] <- NA

	rs <- toupper(d$loss_reason_)
	st <- cbind(
		ifelse(grepl("INSUFF|DROUGHT|LACK OF WATER|WATER SHORTAGE|NO WATER", rs), "drought", NA),
		ifelse(grepl("STEM ?BORER|BLA?CK +BUG|RICE BUG|SNAIL|ARMY|GLH|BPH|HOPPER|HARABAS|WORM|INSECT", rs), "pests", NA),
		ifelse(grepl("\\bRATS?\\b", rs), "rodents", NA),
		ifelse(grepl("TUNGRO|BLB|BLAST|NECK ROT|FUNGUS|SHEATH", rs), "disease", NA),
		ifelse(grepl("TYPHOON|WIND", rs), "wind", NA),
		ifelse(grepl("FLOOD|SUBMERG", rs), "flood", NA),
		ifelse(grepl("WEED", rs), "weeds", NA),
		ifelse(grepl("COLD", rs), "cold", NA),
		ifelse(grepl("HEAT", rs), "heat", NA)
	)
	d$stress <- apply(st, 1, function(x) if (all(is.na(x))) NA else paste(x[!is.na(x)], collapse="; "))

	d$country <- "Philippines"
	d$crop <- "rice"
	d$yield_part <- "grain"
	d$trial_id <- paste(d$hhid, d$season, sep="_")
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
