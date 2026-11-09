# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI SSD Central Luzon Loop Survey, 1979 wet season (SEASON 1) and 1980 dry season (SEASON 2). One workbook with 9 sheets; coding manual in "Central Luzon Coding Manual.xlsx".
# Unit of record is the Production sheet (rice only): household (HHCODE) x season x parcel (PARNO); keys are unique there.
# yield = PRODNKG / FSIZE. PRODNKG is total rough rice (palay) in kg at harvest (cavans x KGCAV, kg per cavan at harvest); FSIZE is area planted (ha).
# Farm Char is matched on key + FSIZE (some keys are duplicated there, e.g. two different parcels both coded PARNO 1), else on key only.
# IRRTYPE: "Irrigated" = gravity irrigation system (manual lists gravity irrigation expenses), "Pump" = pump irrigation, "Rainfed".
# Fertilizer: TOT_N is kg N on the parcel; TOT_P and TOT_K are already elemental P and K (checked: TOT_P = QTYKG * P% / 2.29, TOT_K = QTYKG * K% / 1.2051). Summed over inorganic applications and divided by FSIZE.
# N/P/K = 0 for parcels whose only fertilizer record is "no operation" (or organic only).
# planting_method from Crop Estab activities (TRANSPLANTING / DIRECT SEEDING); no dates in Crop Estab for this round.
# sex and age are of the household head (HHInfo, collected in the 1979 wet season round only).
# date is the survey YEAR of the season record (not a planting or harvest date).

## ISSUES
# No coordinates or admin names in the data. Dataset metadata only lists Bulacan, Nueva Ecija, Pampanga, Tarlac, Pangasinan and La Union. HHCODE first digit (1-6) may be a province/village code; verify with IRRI (not in coding manual) before georeferencing.
# 6 parcels have PRODN = PRODNKG = KGCAV = 0 and no variety (yield 0): not planted to rice?
# Very high yields on small parcels: HHCODE 233/1 (9915 kg/ha), 252/1 (0.3 ha, 10833 kg/ha). High N: 218/1 (273 kg/ha), 238/2 (336), 609/4 (0.09 ha, 256). Check FSIZE.
# Parcels with an inorganic product lacking nutrient content (TOT_N empty: brand names, foliars) or an unnamed product with kg applied get NA for N/P/K rather than an underestimate.
# e.g. SUMITOMO without nutrient content.
# soil_type kept as recorded (Filipino and English names, numeric codes 1, 2, 3, 7 and class labels); not mapped to soil_texture.
# Farm Char PLANT SPACING, CROP PLANTED LAST YEAR and DATE PLNTED (Excel serials and text such as "2nd week of Feb.") are filled in this round only and not processed.
# Varieties kept as recorded (IR lines, BPI, C1/C4, traditional names; "21", "1561" etc. not expanded).
# yield_moisture unknown (rough rice at harvest; KGCAV, kg per cavan, varies between records).
# Not processed: Land Prep, Crop Care (pesticides, weeding), HarvThreshPharvest (labor, costs), output shares (thresher/harvester/landlord), seed quantity (Seeds QTYSEED, unit not given), Farm Char rents/irrigation costs, organic fertilizer amounts.
# write_files: missing longitude/latitude/geo_from_source; unknown variable cavan_weight_; invalid fertilizer_type terms 16-20-0, sumitomo (no terminag term; 16-20-0 is ammonium phosphate sulfate).

## NEW VARIABLES
# cavan_weight_: kilograms per cavan at harvest (KGCAV), used by the data providers to convert cavans to kg



## NOT PROCESSED
# Read Me.txt: not read. columns: These publications are compiled publications of Central Luzon Loop Survey conducted on the following time series:. Observation records are missing from d until this file is standardized.

carob_script <- function(path) {

"Central Luzon Loop Survey (1979-1980)

The objective of the study is to monitor the changes in rice farming in the major rice producing area of the Philippines - the Central Luzon region, which is called as the 'rice bowl of the Philippines.'"

	uri <- "doi:10.7910/DVN/25908"
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

	f <- ff[basename(ff) == "Central Luzon Loop 1979-80.xlsx"]

	r1 <- carobiner::read.excel(f, sheet="Production", skip=1)
	r2 <- carobiner::read.excel(f, sheet="Farm Char", skip=1)
	r3 <- carobiner::read.excel(f, sheet="Fertilizer", skip=1)
	r4 <- carobiner::read.excel(f, sheet="Crop Estab", skip=1)
	r5 <- carobiner::read.excel(f, sheet="Seeds", skip=1)
	r6 <- carobiner::read.excel(f, sheet="HHInfo", skip=1, na=c("", "no data"))

	key <- function(r) paste(r$YEAR, r$SEASON, r$HHCODE, r$PARNO, sep="_")
	r1$id <- key(r1)
	r2$id <- key(r2)
	r3$id <- key(r3)
	r4$id <- key(r4)
	r5$id <- key(r5)

	d <- data.frame(
		hhid = as.character(r1$HHCODE),
		field_id = as.character(r1$PARNO),
		date = as.character(r1$YEAR),
		season = c("wet", "dry")[r1$SEASON],
		country = "Philippines",
		crop = "rice",
		variety = trimws(r1$VARIETY),
		field_size = r1$FSIZE,
		yield = r1$PRODNKG / r1$FSIZE,
		yield_part = "grain",
		cavan_weight_ = r1$KGCAV,
		crop_price = r1$PRICE,
		crop_amount_sold = r1$SOLDKG,
		currency = "PHP"
	)

	## parcel characteristics
	i <- match(paste(r1$id, r1$FSIZE), paste(r2$id, r2$FSIZE))
	j <- match(r1$id, r2$id)
	i[is.na(i)] <- j[is.na(i)]
	irr <- trimws(r2$IRRTYPE[i])
	d$irrigated <- irr != "Rainfed"
	d$irrigation_source <- as.vector(c(Irrigated="gravity irrigation system", Pump="pump", Rainfed=NA)[irr])
	d$land_tenure <- tolower(trimws(r2$TENURE[i]))
	d$soil_type <- trimws(r2[["SOIL TYPE"]][i])

	## fertilizer
	brand <- toupper(trimws(r3$BRAND))
	organic <- rep(FALSE, nrow(r3))
	inorg <- r3$ACTIVTY %in% "FERTILIZER APPLICATION" & !organic
	# unnamed product with kg applied but TOT_N = 0: nutrient content unknown
	totN <- r3$TOT_N
	totN[is.na(brand) & !is.na(r3$QTYKG) & r3$QTYKG > 0] <- NA
	fN <- tapply(totN[inorg], r3$id[inorg], sum)
	fP <- tapply(r3$TOT_P[inorg], r3$id[inorg], sum)
	fK <- tapply(r3$TOT_K[inorg], r3$id[inorg], sum)
	d$N_fertilizer <- as.vector(fN[r1$id]) / d$field_size
	d$P_fertilizer <- as.vector(fP[r1$id]) / d$field_size
	d$K_fertilizer <- as.vector(fK[r1$id]) / d$field_size
	d$P_fertilizer[is.na(d$N_fertilizer)] <- NA
	d$K_fertilizer[is.na(d$N_fertilizer)] <- NA
	nofert <- r1$id %in% r3$id[r3$ACTIVTY %in% "no operation" | organic] & !(r1$id %in% r3$id[inorg | r3$ACTIVTY %in% "no data"])
	d$N_fertilizer[nofert] <- 0
	d$P_fertilizer[nofert] <- 0
	d$K_fertilizer[nofert] <- 0

	ftype <- tolower(brand)
	ftype[brand %in% c("UREA", "46-0-0")] <- "urea"
	ftype[brand %in% c("AMMONIUM SULFATE", "21-0-0")] <- "DAS"
	ftype[brand %in% c("0-0-60", "M. POTASH")] <- "KCl"
	ftype[grepl("^[1-9][0-9]*-[1-9][0-9]*-[1-9][0-9]*$", brand)] <- "NPK"
	ok <- inorg & !is.na(ftype)
	ft <- tapply(ftype[ok], r3$id[ok], function(x) paste(unique(x), collapse=";"))
	d$fertilizer_type <- as.vector(ft[r1$id])
	d$fertilizer_type[nofert] <- "none"
	om <- tapply(tolower(brand[organic]), r3$id[organic], function(x) paste(unique(x), collapse=";"))
	d$OM_type <- as.vector(om[r1$id])

	## crop establishment
	act <- toupper(trimws(r4$ACTIVITY))
	pm <- as.vector(c("TRANSPLANTING"="transplanting", "DIRECT SEEDING"="direct seeding")[act])
	pm <- tapply(pm[!is.na(pm)], r4$id[!is.na(pm)], function(x) paste(sort(unique(x)), collapse=";"))
	d$planting_method <- as.vector(pm[r1$id])

	d$seed_source <- trimws(r5$SEEDSRC[match(r1$id, r5$id)])

	## household head
	h <- r6[tolower(trimws(r6[["HOUSEHOLD MEMBER"]])) %in% "head", ]
	k <- match(r1$HHCODE, h$HHCODE)
	d$sex <- as.vector(c(M="male", F="female")[toupper(trimws(h$SEX[k]))])
	d$age <- as.numeric(h$AGE[k])

	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
