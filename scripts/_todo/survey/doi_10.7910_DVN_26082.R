# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI SSD Central Luzon Loop Survey, 2007 dry season (SEASON 2) and 2008 wet season (SEASON 1). One workbook with 9 sheets; coding manual in "Central Luzon Coding Manual.xlsx".
# Unit of record is the Production sheet (rice only): household (HHCODE) x season x parcel (PARNO); keys are unique there.
# yield = PRODNKG / FSIZE. PRODNKG is total rough rice (palay) in kg at harvest (cavans x KGCAV, kg per cavan at harvest); FSIZE is area planted (ha).
# Farm Char is matched on key + FSIZE (some keys are duplicated there, e.g. two different parcels both coded PARNO 1), else on key only.
# IRRTYPE: "Irrigated" = gravity irrigation system (manual lists gravity irrigation expenses), "Pump" = pump irrigation, "Rainfed".
# Fertilizer: TOT_N is kg N on the parcel; TOT_P and TOT_K are already elemental P and K (checked: TOT_P = QTYKG * P% / 2.29, TOT_K = QTYKG * K% / 1.2051). Summed over inorganic applications and divided by FSIZE.
# N/P/K = 0 for parcels whose only fertilizer record is "no operation" (or organic only).
# planting_method from Crop Estab activities (TRANSPLANTING / DIRECT SEEDING); no dates in Crop Estab for this round.
# sex and age are of the household head (HHInfo, collected in the 2007 dry season round only).
# date is the survey YEAR of the season record (not a planting or harvest date).

## ISSUES
# No coordinates or admin names in the data. Dataset metadata only lists Bulacan, Nueva Ecija, Pampanga, Tarlac, Pangasinan and La Union. HHCODE first digit (1-6) may be a province/village code; verify with IRRI (not in coding manual) before georeferencing.
# Very high yield HHCODE 233/1 (1.3 ha, 10269 kg/ha).
# Parcels with an inorganic product lacking nutrient content (TOT_N empty: brand names, foliars) or an unnamed product with kg applied get NA for N/P/K rather than an underestimate.
# e.g. foliar products, ZINC (liters), HEXA PLUS.
# "ORGANIC" products with a nutrient grade (2-3-3, 2-2-2; TOT_N given) are treated as organic and excluded from N/P/K.
# Farm Char has 141 parcel records without a Production record (fallow, maize, onion, vegetables, fishpond, and many rice parcels (RC128, RC82, RC130, ...); 2007 dry season has 166 Farm Char vs 70 Production records). Not included; check whether dry-season rice production is missing from the Production sheet.
# Varieties kept as recorded (PSB/NSIC Rc numbers as "RC-82" etc., hybrids PG, SL-8H, Bigante).
# Compost and cow dung amounts in bags without kg; not converted.
# yield_moisture unknown (rough rice at harvest; KGCAV, kg per cavan, varies between records).
# Not processed: Land Prep, Crop Care (pesticides, weeding), HarvThreshPharvest (labor, costs), output shares (thresher/harvester/landlord), seed quantity (Seeds QTYSEED, unit not given), Farm Char rents/irrigation costs, organic fertilizer amounts.
# write_files: missing longitude/latitude/geo_from_source; unknown variable cavan_weight_; invalid fertilizer_type terms 0-0-17, 0-0-70, 15-15-15-19, 16-20-0, 17-0-0, 17-0-17, 25-0-0 and brand names (crop giant, mrg, ...); invalid OM_type terms organic (no terminag term; 16-20-0 is ammonium phosphate sulfate).

## NEW VARIABLES
# cavan_weight_: kilograms per cavan at harvest (KGCAV), used by the data providers to convert cavans to kg


carob_script <- function(path) {

"Central Luzon Loop Survey (2007-2008)

The objective of the study is to monitor the changes in rice farming in the major rice producing area of the Philippines - the Central Luzon region, which is called as the 'rice bowl of the Philippines.'"

	uri <- "doi:10.7910/DVN/26082"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=1,
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
		carob_completion = 50,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "Central Luzon Loop 2007-08.xlsx"]

	rP <- carobiner::read.excel(f, sheet="Production", skip=1)
	rF <- carobiner::read.excel(f, sheet="Farm Char", skip=1)
	rN <- carobiner::read.excel(f, sheet="Fertilizer", skip=1)
	rE <- carobiner::read.excel(f, sheet="Crop Estab", skip=1)
	rS <- carobiner::read.excel(f, sheet="Seeds", skip=1)
	rH <- carobiner::read.excel(f, sheet="HHInfo", skip=1, na=c("", "no data"))

	key <- function(r) paste(r$YEAR, r$SEASON, r$HHCODE, r$PARNO, sep="_")
	rP$id <- key(rP)
	rF$id <- key(rF)
	rN$id <- key(rN)
	rE$id <- key(rE)
	rS$id <- key(rS)

	d <- data.frame(
		hhid = as.character(rP$HHCODE),
		field_id = as.character(rP$PARNO),
		date = as.character(rP$YEAR),
		season = c("wet", "dry")[rP$SEASON],
		country = "Philippines",
		crop = "rice",
		variety = trimws(rP$VARIETY),
		field_size = rP$FSIZE,
		yield = rP$PRODNKG / rP$FSIZE,
		yield_part = "grain",
		cavan_weight_ = rP$KGCAV,
		crop_price = rP$PRICE,
		crop_amount_sold = rP$SOLDKG,
		currency = "PHP"
	)

	## parcel characteristics
	i <- match(paste(rP$id, rP$FSIZE), paste(rF$id, rF$FSIZE))
	j <- match(rP$id, rF$id)
	i[is.na(i)] <- j[is.na(i)]
	irr <- trimws(rF$IRRTYPE[i])
	d$irrigated <- irr != "Rainfed"
	d$irrigation_source <- as.vector(c(Irrigated="gravity irrigation system", Pump="pump", Rainfed=NA)[irr])
	d$land_tenure <- tolower(trimws(rF$TENURE[i]))

	## fertilizer
	brand <- toupper(trimws(rN$BRAND))
	organic <- brand %in% c("COMPOST", "COW DUNG", "ORGANIC")
	inorg <- rN$ACTIVTY %in% "FERTILIZER APPLICATION" & !organic
	# unnamed product with kg applied but TOT_N = 0: nutrient content unknown
	totN <- rN$TOT_N
	totN[is.na(brand) & !is.na(rN$QTYKG) & rN$QTYKG > 0] <- NA
	fN <- tapply(totN[inorg], rN$id[inorg], sum)
	fP <- tapply(rN$TOT_P[inorg], rN$id[inorg], sum)
	fK <- tapply(rN$TOT_K[inorg], rN$id[inorg], sum)
	d$N_fertilizer <- as.vector(fN[rP$id]) / d$field_size
	d$P_fertilizer <- as.vector(fP[rP$id]) / d$field_size
	d$K_fertilizer <- as.vector(fK[rP$id]) / d$field_size
	d$P_fertilizer[is.na(d$N_fertilizer)] <- NA
	d$K_fertilizer[is.na(d$N_fertilizer)] <- NA
	nofert <- rP$id %in% rN$id[rN$ACTIVTY %in% "no operation" | organic] & !(rP$id %in% rN$id[inorg | rN$ACTIVTY %in% "no data"])
	d$N_fertilizer[nofert] <- 0
	d$P_fertilizer[nofert] <- 0
	d$K_fertilizer[nofert] <- 0

	ftype <- tolower(brand)
	ftype[brand %in% c("UREA", "46-0-0")] <- "urea"
	ftype[brand %in% c("AMMONIUM SULFATE", "21-0-0")] <- "DAS"
	ftype[brand %in% c("0-0-60", "M. POTASH")] <- "KCl"
	ftype[brand %in% "ZINC SULFATE"] <- "ZnSO4"
	ftype[grepl("^[1-9][0-9]*-[1-9][0-9]*-[1-9][0-9]*$", brand)] <- "NPK"
	ok <- inorg & !is.na(ftype)
	ft <- tapply(ftype[ok], rN$id[ok], function(x) paste(unique(x), collapse=";"))
	d$fertilizer_type <- as.vector(ft[rP$id])
	d$fertilizer_type[nofert] <- "none"
	omtype <- tolower(brand)
	omtype[brand %in% "COW DUNG"] <- "cattle dung"
	om <- tapply(omtype[organic], rN$id[organic], function(x) paste(unique(x), collapse=";"))
	d$OM_type <- as.vector(om[rP$id])

	## crop establishment
	act <- toupper(trimws(rE$ACTIVITY))
	pm <- as.vector(c("TRANSPLANTING"="transplanting", "DIRECT SEEDING"="direct seeding")[act])
	pm <- tapply(pm[!is.na(pm)], rE$id[!is.na(pm)], function(x) paste(sort(unique(x)), collapse=";"))
	d$planting_method <- as.vector(pm[rP$id])

	d$seed_source <- trimws(rS$SEEDSRC[match(rP$id, rS$id)])

	## household head
	h <- rH[tolower(trimws(rH[["HOUSEHOLD MEMBER"]])) %in% "head", ]
	k <- match(rP$HHCODE, h$HHCODE)
	d$sex <- as.vector(c(M="male", F="female")[toupper(trimws(h$SEX[k]))])
	d$age <- as.numeric(h$AGE[k])

	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
