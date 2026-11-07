# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI SSD Central Luzon Loop Survey, wet and dry seasons 2015-2016. Unit of record is household x season x parcel.
# Production PRODNKG is kg/parcel; yield = PRODNKG / FSIZE (FSIZE is area planted, ha).
# Fertilizer sheet mixes fertilizers and pesticides. TOT_N is kg N on the parcel; TOT_P and TOT_K are already elemental P and K (checked: 14-14-14 at 0.6 bag on 0.5 ha).
# Nutrient totals summed over fertilizer rows with non-missing TOT_N, then divided by FSIZE to kg/ha.
# Season 1 = wet, 2 = dry (coding manual). Transplanting/direct-seeding dates from Crop Estab are Excel serials.

## ISSUES
# No coordinates in the files. Place of work (e.g. San Rafael, Bulacan) is occupation location, not the farm. Human must georeference Central Luzon parcels (Bulacan/Nueva Ecija/Pampanga/Tarlac rice bowl).
# Variety codes (216, 218, Rc29, Bigante, …) not expanded to NSIC/PSB names; some parcels list several varieties.
# Land Prep, Crop Care, HarvThreshPHarv labor, Seeds, and household roster (beyond household head) not processed.
# Fertilizer BRAND includes herbicides/insecticides (2-4D, Surekill, Magnum); those rows have TOT_N NA and are excluded from N/P/K sums.
# Bag mass is implicit in TOT_N; not independently verified for every product.
# DATE PLNTED on Farm Char is almost empty; planting_date taken from Crop Estab Transplanting or Direct-seeding.
# FSIZE typo ".0.3" repaired to 0.3. Crop Estab date "7/16-22/2015" left unparsed (range). "27 Aug. 2015" / "03 Sept. 2015" parsed.
# write_files: missing latitude/longitude/geo_from_source; yield 0 and one parcel 174800 kg/ha (check FSIZE); NA yield; duplicate YEAR-SEASON-HH-PARNO possible.

## NEW VARIABLES
# none



## NOT PROCESSED
# Central Luzon Coding Manual.xlsx: not read. sheet HH Info (of 9): SEASON, Season. Observation records are missing from d until this file is standardized.

carob_script <- function(path) {

"Central Luzon Loop Survey (2015-2016)

The objective of the study is to monitor the changes in rice farming in the major rice producing area of the Philippines - the Central Luzon region, which is called as the \"rice bowl of the Philippines."

	uri <- "doi:10.7910/DVN/01ZTPA"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
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
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "Central Luzon Loop 2015-206.xlsx"]

	r1 <- carobiner::read.excel(f, sheet="Farm Char", skip=5, na=".")
	r2 <- carobiner::read.excel(f, sheet="Production", skip=7, na=".")
	r3 <- carobiner::read.excel(f, sheet="Fertilizer", skip=8, na=".")
	r4 <- carobiner::read.excel(f, sheet="Crop Estab", skip=5, na=".")
	r5 <- carobiner::read.excel(f, sheet="HHInfo", skip=5, na=".")

	# skipped: Land Prep, Crop Care, HarvThreshPHarv, Seeds (labor, pesticides, seed source)

	key <- function(year, season, hh, par) {
		paste(year, season, hh, par, sep="-")
	}

	# FSIZE typo ".0.3" in Farm Char
	r1$FSIZE <- gsub("^\\.(0\\.[0-9]+)$", "\\1", trimws(as.character(r1$FSIZE)))
	r1$FSIZE <- as.numeric(r1$FSIZE)

	r1$id <- key(r1$YEAR, r1$SEASON, r1$HHCODE, r1$PARNO)
	r2$id <- key(r2$YEAR, r2$SEASON, r2$HHCODE, r2$PARNO)

	# sum elemental nutrients on the parcel, then convert to kg/ha
	r3 <- r3[!is.na(r3$TOT_N), ]
	r3$id <- key(r3$YEAR, r3$SEASON, r3$HHCODE, r3$PARNO)
	fert <- data.frame(
		id = unique(r3$id)
	)
	fert$N_fertilizer <- tapply(r3$TOT_N, r3$id, sum, na.rm=TRUE)[fert$id]
	fert$P_fertilizer <- tapply(r3$TOT_P, r3$id, sum, na.rm=TRUE)[fert$id]
	fert$K_fertilizer <- tapply(r3$TOT_K, r3$id, sum, na.rm=TRUE)[fert$id]
	fert$fsize <- tapply(r3$FSIZE, r3$id, function(z) as.numeric(z[!is.na(z)][1]))[fert$id]
	fert$N_fertilizer <- as.numeric(fert$N_fertilizer) / as.numeric(fert$fsize)
	fert$P_fertilizer <- as.numeric(fert$P_fertilizer) / as.numeric(fert$fsize)
	fert$K_fertilizer <- as.numeric(fert$K_fertilizer) / as.numeric(fert$fsize)

	# planting date: transplanting preferred, else direct-seeding
	r4$act <- trimws(r4$ACTIVITY)
	r4 <- r4[r4$act %in% c("Transplanting", "Direct-seeding") & !is.na(r4$DATE), ]
	r4$id <- key(r4$YEAR, r4$SEASON, r4$HHCODE, r4$PARNO)
	r4$ord <- ifelse(r4$act == "Transplanting", 1, 2)
	r4 <- r4[order(r4$id, r4$ord), ]
	r4 <- r4[!duplicated(r4$id), ]
	dx <- trimws(as.character(r4$DATE))
	r4$planting_date <- rep(NA_character_, nrow(r4))
	is_serial <- grepl("^[0-9]+(\\.[0-9]+)?$", dx)
	r4$planting_date[is_serial] <- as.character(as.Date(as.numeric(dx[is_serial]), origin="1899-12-30"))
	# "27 Aug. 2015", "03 Sept. 2015"
	is_mon <- grepl("^[0-9]{1,2} [A-Za-z]+\\.? [0-9]{4}$", dx)
	mon <- dx[is_mon]
	mon <- sub("Sept\\.", "Sep", mon)
	mon <- sub("Aug\\.", "Aug", mon)
	r4$planting_date[is_mon] <- as.character(as.Date(mon, format="%d %b %Y"))
	# skipped ambiguous range "7/16-22/2015"

	headHH <- r5[tolower(trimws(r5[["HOUSEHOLD MEMBER"]])) == "head", ]
	headHH <- headHH[!duplicated(paste(headHH$YEAR, headHH$SEASON, headHH$HHCODE)), ]

	d <- data.frame(
		hhid = as.character(r1$HHCODE),
		field_id = as.character(r1$PARNO),
		country = "Philippines",
		crop = "rice",
		variety = r1$VARIETY,
		field_size = as.numeric(r1$FSIZE),
		land_tenure = r1$TENURE,
		soil_type = r1[["SOIL TYPE"]],
		season = ifelse(as.numeric(r1$SEASON) == 1, "wet",
			ifelse(as.numeric(r1$SEASON) == 2, "dry", NA)),
		planting_date = ifelse(grepl("^[0-9]{4}$", as.character(r1$YEAR)),
			as.character(r1$YEAR), NA)
	)

	irr <- trimws(tolower(as.character(r1$IRRTYPE)))
	d$irrigated <- ifelse(is.na(irr) | irr == "", NA,
		ifelse(irr == "rainfed", FALSE, TRUE))

	d$yield <- as.numeric(r2$PRODNKG[match(r1$id, r2$id)]) / d$field_size
	d$yield_part <- "grain"

	i <- match(r1$id, fert$id)
	d$N_fertilizer <- fert$N_fertilizer[i]
	d$P_fertilizer <- fert$P_fertilizer[i]
	d$K_fertilizer <- fert$K_fertilizer[i]

	j <- match(r1$id, r4$id)
	d$planting_date[!is.na(j)] <- r4$planting_date[j[!is.na(j)]]

	hhkey <- paste(r1$YEAR, r1$SEASON, r1$HHCODE)
	hk <- paste(headHH$YEAR, headHH$SEASON, headHH$HHCODE)
	hh <- match(hhkey, hk)
	sex <- tolower(trimws(headHH$SEX[hh]))
	d$sex <- ifelse(sex == "m", "male", ifelse(sex == "f", "female", sex))
	d$age <- as.numeric(headHH$AGE[hh])

	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
