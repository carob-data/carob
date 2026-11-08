# R script for "carob"
# license: GPL (>=3)

## NOTES
# Crop Care: "Pesticide application" in the raw data is a generic catch-all; its BRAND
# column shows it mixes insecticides, niclosamide snail baits, fungicides and a
# herbicide (44 of 49 households with "Pesticide" rows also have "Insecticide" rows,
# often with the same brands). Rows are therefore reclassified by product brand
# (the brand also overrides the activity label, e.g. "Insecticide application" rows
# with the fungicide brand Armure count as fungicide use); unidentifiable brands
# stay under the generic biocide_* variables.
# *_cost is the sum of the recorded application costs per parcel divided by the
# parcel area (PHP/ha). *_times is the number of recorded applications.
# The Land Prep, Crop Estab, Fertilizer and Harvest sheets have one row per operation
# (with no operation number or date); they are aggregated to one row per parcel:
# methods/implements/types joined with ";", passes counted (land_prep_times),
# labor days, amounts and costs summed, and all costs expressed in PHP/ha.
# The raw fertilizer N/P/K columns are product percentages; TOT_N/TOT_P/TOT_K are
# elemental kg per parcel and are used (divided by parcel area).

## ISSUES
# - Brand -> class and brand -> active ingredient assignments are based on product
#   names common in Philippine rice farming; they should be double-checked.
# - *_product holds the reported brand name where the active ingredient is unknown;
#   these are flagged as invalid terms but kept (same for some fertilizer_type values).
# - fertilizer_amount reaches ~2440 kg/ha (product weight) on some small parcels;
#   high but as reported, left as is (flagged as out of bounds).
# - a few harvest dates are implausibly close to (or >1 year after) planting dates;
#   left as reported (flagged by write_files as datespan).
## NOT PROCESSED
# - Crop Care QTY/UNIT/PRICE: application quantities are in mixed units (liter, kg, ml,
#   sachet, box) of formulated products whose concentration is unknown, so they cannot
#   be converted to the kg (ai)/ha required by *_amount; not processed.
# - Crop Care DBT/DAT (days before/after transplanting of each operation): per-application
#   timing cannot be kept in the one-row-per-parcel wide table; not processed.
# - Herbicide/fungicide/molluscicide application rows also have brands, but no active
#   ingredient lookup was made for brands that only occur under those specific labels.


carob_script <- function(path) {

"
Central Luzon Loop Survey (2015-2016)

The objective of the study is to monitor the changes in rice farming in the major rice producing area of the Philippines - the Central Luzon region, which is called as the 'rice bowl of the Philippines.
"

	uri <- "doi:10.7910/DVN/01ZTPA"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IRRI",
		publication = NA,
		project = "Loop Survey",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		notes = NA,
		carob_contributor = "Cedric Ngakou;Robert Hijmans",
		carob_LLM = "Claude Fable 5",
		carob_date = "2026-09-18",
		carob_completion = 90,	
		carob_effort = 5
	)
	


	f1 <- ff[basename(ff) == "Central Luzon Loop 2015-206.xlsx"]
	#f2 <- ff[basename(ff) == "Central Luzon Coding Manual.xlsx"]

	###
	r1 <- carobiner::read.excel(f1, sheet="HHInfo", skip = 5)
	r2 <- carobiner::read.excel(f1, sheet="Farm Char", skip = 5)
	## do not unique(): literally identical rows are real repeated operations
	## (the sheet has no operation number or date).
	## these reads warn about malformed cells ("PL" and "." in numeric columns,
	## numbers stored as text); that cannot be prevented and the values are handled below
	suppressWarnings(r3 <- carobiner::read.excel(f1, sheet="Land Prep", skip =7))
	suppressWarnings(r4 <- carobiner::read.excel(f1, sheet="Crop Estab", skip = 5))
	r5 <- carobiner::read.excel(f1, sheet="Crop Care", skip= 6)
	r6 <- carobiner::read.excel(f1, sheet="Fertilizer", skip = 8)
	r7 <- carobiner::read.excel(f1, sheet="HarvThreshPHarv", skip= 7)
	r8 <- carobiner::read.excel(f1, sheet="Production", skip =7)
	r9 <- carobiner::read.excel(f1, sheet="Seeds", skip =5)

#### process
	
	d1 <- data.frame(
	  date = r1$YEAR,
	  season = r1$SEASON,
	  hhid = as.character(r1$HHCODE),
	  hh_size = r1$`HOUSEHOLD SIZE`,
	  #hh_member = r1$`HOUSEHOLD MEMBER`,
	  sex = c("M"= "Male", "F"= "Female")[r1$SEX],
	  age = r1$AGE,
	  civil_status = r1$`CIVIL STATUS`,
	  occupation = r1$`PRIMARY OCCUPATION`
	)
	
	d1 <- d1[!duplicated(d1$hhid),]
	
	d2 <- data.frame(
	  date = r2$YEAR,
	  season = r2$SEASON,
	  hhid = as.character(r2$HHCODE),
	  plot_id = as.character(r2$PARNO),
	  cropland = round(as.numeric(gsub(".0.3", 0.3, r2$FSIZE)), 4),
	  variety = r2$VARIETY,
	  #irrigation_type = r2$IRRTYPE,
	  irrigated = !grepl("Rainfed", r2$IRRTYPE),
	  cropland_owned = ifelse(grepl("Own",r2$TENURE), as.numeric(gsub(".0.3", 0.3, r2$FSIZE)), NA),
	  cropland_rentedout = ifelse(grepl("Lent out|Rented out",r2$TENURE), as.numeric(gsub(".0.3", 0.3, r2$FSIZE)), NA),
	  soil_texture = tolower(r2$`SOIL TYPE`),
	  plant_spacing = r2$`PLANT SPACING`
	)
	
	## Fixing soil type 
	
	P <- carobiner::fix_name(d2$soil_texture)
	P <- gsub("^sandy$", "sand", P)
	P <- gsub("sandy loam \\(galas\\)", "sandy loam", P)
	P <- gsub("^clayee$", "clay", P)
	P <- gsub("clay loam \\(lagkitin, galas\\)|lagkitin galas", "clay loam", P)
	P <- gsub("sandy \\(buhaghag\\)", "sand", P)
	P <- gsub("clay loam \\(mestisong galas\\)", "clay loam", P)
	P <- gsub("clayee \\(malagkit\\)", "clay", P)
	P <- gsub("no idea", NA, P)
	d2$soil_texture <- P
	# the raw Farm Char sheet has one literally duplicated parcel record (household 220)
	d2 <- unique(d2)
	#### merge d1 and d2
	
	d <- merge(d1, d2, by= c("hhid", "date", "season"), all  = TRUE)
	
	## Land Prep has one row per operation (several passes per parcel, often identical
	## because the sheet has no operation number or date); aggregate to one row per parcel
	keys <- c("hhid", "date", "season", "plot_id", "cropland")
	sum_na <- function(x) ifelse(all(is.na(x)), NA, sum(x, na.rm=TRUE))
	uniq_na <- function(x) {
	  x <- unique(x[!is.na(x)])
	  ifelse(length(x) == 0, NA, paste(x, collapse="; "))
	}
	
	d3 <- data.frame(
	  date = r3$YEAR,
	  season = r3$SEASON,
	  hhid = as.character(r3$HHCODE),
	  plot_id = as.character(r3$PARNO),
	  cropland = round(as.numeric(r3$FSIZE), 4),
	  land_prep_method = tolower(r3$ACTIVITY),
	  land_prep_implement = r3$POWSOURCE,
	  farm_labour = r3$DAYFAM,
	  farm_labour_exchange = r3$DAYEXC,
	  farm_labour_hired = r3$DAYHIR,
	  land_prep_cost = as.numeric(gsub("566\\.\\.67|PL", NA, r3$WAGE)),
	  equipment_cost = r3$RENTAL
	)
	
	#### fix land_prep
	P <- carobiner::fix_name(d3$land_prep_method)
	# dike cleaning/repair ("Pre LP", "dukit") is not a tillage pass
	P[grepl("cleaning and repair of dikes|pre lp|dukit", P)] <- NA
	P <- gsub("rotavating", "rotovating", P)
	P <- gsub("plowing", "ploughing", P)
	d3$land_prep_method <- P 
	
	d3$land_prep_implement[grepl("tractor|Tractor", d3$land_prep_implement)] <- "4 wheel tractor"
	d3$land_prep_implement[grepl("Power|power tiller", d3$land_prep_implement)] <- "2 wheel tractor"
	d3$land_prep_implement[grepl("Grasscutter|grasscutter|Cow|Carabao", d3$land_prep_implement)] <- "animal"
	
	## costs include the dike cleaning/repair rows; currency/ha
	d3 <- do.call(rbind, lapply(split(d3, d3[keys], drop=TRUE), function(x) {
	  data.frame(x[1, keys],
	    land_prep_method = uniq_na(x$land_prep_method),
	    land_prep_implement = uniq_na(x$land_prep_implement),
	    land_prep_times = sum(!is.na(x$land_prep_method)),
	    farm_labour = sum_na(x$farm_labour),
	    farm_labour_exchange = sum_na(x$farm_labour_exchange),
	    farm_labour_hired = sum_na(x$farm_labour_hired),
	    land_prep_cost = sum_na(x$land_prep_cost) / x$cropland[1],
	    equipment_cost = sum_na(x$equipment_cost) / x$cropland[1]
	  )
	}))
	
	#### merge d and d3
	d <- merge(d, d3, by=keys, all = TRUE)
	
####	
	d4 <- data.frame(
	  date = r4$YEAR,
	  season = r4$SEASON,
	  hhid = as.character(r4$HHCODE),
	  plot_id = as.character(r4$PARNO),
	  cropland = round(r4$FSIZE, 4),
	  planting_method = r4$ACTIVITY,
	  planting_date = r4$DATE,
	  planting_cost = as.numeric(gsub("PL", NA, r4$WAGE))
	)
	
	d4 <- d4[grepl("Transplanting|Replanting|Direct-seeding", d4$planting_method),]
	d4 <- d4[!is.na(d4$planting_date),]
	d4$planting_method <- gsub("Direct-seeding", "direct seeding", d4$planting_method)
	d4$planting_method <- tolower(gsub("Replanting", "transplanting", d4$planting_method))
	
	### Fixing planting date 
	d4$planting_date <- gsub("03 Sept. 2015", "2015-09-03", d4$planting_date)
	d4$planting_date <- gsub("27 Aug. 2015", "2015-08-27", d4$planting_date)
	d4$planting_date <- gsub("7/16-22/2015", NA, d4$planting_date)
	
	i <- !grepl("-", d4$planting_date)
	d4$planting_date[i] <- as.character(as.Date(as.numeric(d4$planting_date[i]), origin = "1899-12-30"))
	
	## a few parcels have several planting rows (e.g. by worker sex); one row per parcel
	d4 <- do.call(rbind, lapply(split(d4, d4[keys], drop=TRUE), function(x) {
	  data.frame(x[1, keys],
	    planting_method = uniq_na(x$planting_method),
	    planting_date = min(x$planting_date),
	    planting_cost = sum_na(x$planting_cost) / x$cropland[1]
	  )
	}))
	
	#### merge d and d4
	d <- merge(d, d4, by=keys, all  = TRUE)
	
	##### Crop Care: pesticide applications and weeding
	d5 <- data.frame(
	  date = r5$YEAR,
	  season = r5$SEASON,
	  hhid = as.character(r5$HHCODE),
	  plot_id = as.character(r5$PARNO),
	  cropland = round(r5$FSIZE, 4),
	  activity = r5$ACTIVTY1,
	  brand = tolower(trimws(r5$BRAND)),
	  cost = as.numeric(r5$COST),
	  wage = as.numeric(gsub("PL|Pl", NA, r5$WAGE))
	)
	
	## product class from the activity label
	act <- tolower(d5$activity)
	d5$class <- NA
	d5$class[grepl("herbicide", act)] <- "herbicide"
	d5$class[grepl("fungicide", act)] <- "fungicide"
	d5$class[grepl("insecticide", act)] <- "insecticide"
	d5$class[grepl("molluscicide", act)] <- "molluscicide"
	d5$class[grepl("^pesticide", act)] <- "biocide"
	
	## "Pesticide application" is a generic catch-all: its BRAND column mixes insecticides,
	## molluscicides (snail bait), fungicides and a herbicide. Reclassify by product brand;
	## brands that cannot be identified stay "pesticide". The same lookup also supplies the
	## active ingredient(s) for *_product, where the product could be identified with
	## confidence ("chix" is beta-cypermethrin, recorded as cypermethrin)
	brands <- rbind(
	  data.frame(brand="brodan", bclass="insecticide", ai="chlorpyrifos; fenobucarb"),
	  data.frame(brand="azodrin", bclass="insecticide", ai="monocrotophos"),
	  data.frame(brand="marshall", bclass="insecticide", ai="carbosulfan"),
	  # BPMC/isoprocarb carbamates common in Philippine rice
	  data.frame(brand=c("hopcin", "hytox"), bclass="insecticide", ai="fenobucarb"),
	  data.frame(brand=c("etrofolan", "etropolan"), bclass="insecticide", ai="isoprocarb"),
	  data.frame(brand="metarisium", bclass="insecticide", ai="metarhizium anisopliae"),
	  data.frame(brand=c("chix", "cymbush", "supermethrin", "magnum"), bclass="insecticide", ai="cypermethrin"),
	  data.frame(brand="karate", bclass="insecticide", ai="lambda-cyhalothrin"),
	  data.frame(brand="bulldock", bclass="insecticide", ai="beta-cyfluthrin"),
	  data.frame(brand="solomon", bclass="insecticide", ai="imidacloprid; beta-cyfluthrin"),
	  data.frame(brand="virtako", bclass="insecticide", ai="chlorantraniliprole; thiamethoxam"),
	  data.frame(brand="prevathon", bclass="insecticide", ai="chlorantraniliprole"),
	  data.frame(brand="actara wp", bclass="insecticide", ai="thiamethoxam"),
	  data.frame(brand="malathion", bclass="insecticide", ai="malathion"),
	  data.frame(brand="lannate", bclass="insecticide", ai="methomyl"),
	  data.frame(brand="furadan", bclass="insecticide", ai="carbofuran"),
	  data.frame(brand="lorsban", bclass="insecticide", ai="chlorpyrifos"),
	  data.frame(brand="padan", bclass="insecticide", ai="cartab"),
	  data.frame(brand="trebon", bclass="insecticide", ai="etofenprox"),
	  data.frame(brand="folidol", bclass="insecticide", ai="methyl parathion"),
	  # insecticide brands without a confident active-ingredient match
	  data.frame(brand=c("arriba", "rocket", "kriss", "trojan", "soil 21",
	                     "assasin 580"), bclass="insecticide", ai=NA),
	  # niclosamide snail baits (golden apple snail control)
	  data.frame(brand=c("bayluscide", "surekill", "snailkill", "porsnail", "nicloscide",
	                     "niclosicide", "bayonet"), bclass="molluscicide", ai="niclosamide"),
	  data.frame(brand="metabait", bclass="molluscicide", ai="metaldehyde"),
	  data.frame(brand=c("funguran", "kocide", "kocide wp"), bclass="fungicide", ai="copper hydroxide"),
	  data.frame(brand="nativo wp", bclass="fungicide", ai="tebuconazole; trifloxystrobin"),
	  data.frame(brand="armure", bclass="fungicide", ai="propiconazole; difenoconazole"),
	  # common herbicides in Philippine rice; butachlor, pyribenzoxim and
	  # chlorimuron-ethyl are not (yet) in the terminag herbicide list
	  data.frame(brand="machete", bclass="herbicide", ai="butachlor"),
	  data.frame(brand=c("roundup", "round up"), bclass="herbicide", ai="glyphosate"),
	  data.frame(brand="sofit", bclass="herbicide", ai="pretilachlor"),
	  data.frame(brand="clincher", bclass="herbicide", ai="cyhalofop-butyl"),
	  data.frame(brand="nominee", bclass="herbicide", ai="bispyribac-sodium"),
	  data.frame(brand="2-4d", bclass="herbicide", ai="2,4-D"),
	  data.frame(brand="agroxone", bclass="herbicide", ai="mcpa"),
	  data.frame(brand="almix", bclass="herbicide", ai="metsulfuron; chlorimuron-ethyl"),
	  data.frame(brand="ronstar", bclass="herbicide", ai="oxadiazon"),
	  data.frame(brand="rice star", bclass="herbicide", ai="fenoxaprop-p-ethyl"),
	  data.frame(brand="pyanchor", bclass="herbicide", ai="pyribenzoxim"),
	  data.frame(brand="topshot", bclass="herbicide", ai="cyhalofop-butyl; penoxsulam"),
	  # "londox" taken to be Londax (bensulfuron-methyl)
	  data.frame(brand="londox", bclass="herbicide", ai="bensulfuron methyl"),
	  data.frame(brand="grassedge", bclass="herbicide", ai=NA)
	)
	i <- match(d5$brand, brands$brand)
	## the brand overrides the activity label (e.g. a few "Insecticide application"
	## rows have the fungicide brand Armure and are treated as fungicide use)
	d5$class <- ifelse(!is.na(d5$class) & !is.na(i), brands$bclass[i], d5$class)
	## active ingredient where known; otherwise the brand name as reported
	d5$ai <- ifelse(!is.na(i) & !is.na(brands$ai[i]), brands$ai[i], d5$brand)
	
	## one row per parcel and product class: used, number of applications, total cost
	## (PHP per ha of parcel), and the identified active ingredients
	chem <- d5[!is.na(d5$class), ]
	chem_agg <- do.call(rbind, lapply(split(chem, chem[c(keys, "class")], drop=TRUE), function(x) {
	  data.frame(x[1, keys], class = x$class[1],
	    times = nrow(x),
	    cost = ifelse(all(is.na(x$cost)), NA, sum(x$cost, na.rm=TRUE)) / x$cropland[1],
	    product = paste(sort(unique(stats::na.omit(x$ai))), collapse="; "))
	}))
	chem_agg$product[chem_agg$product == ""] <- NA
	
	d5f <- unique(d5[, keys])
	for (cl in c("herbicide", "fungicide", "insecticide", "molluscicide", "biocide")) {
	  x <- chem_agg[chem_agg$class == cl, c(keys, "times", "cost", "product")]
	  names(x)[6:8] <- paste0(cl, c("_times", "_cost", "_product"))
	  x[[paste0(cl, "_used")]] <- TRUE
	  d5f <- merge(d5f, x, by=keys, all.x=TRUE)
	  # parcels with crop-care records but no application of this class: FALSE, not unknown
	  d5f[[paste0(cl, "_used")]][is.na(d5f[[paste0(cl, "_used")]])] <- FALSE
	}
	
	## weeding
	w <- d5[grepl("Weeding|weeding", d5$activity), ]
	w <- do.call(rbind, lapply(split(w, w[keys], drop=TRUE), function(x) {
	  data.frame(x[1, keys], weeding_done = TRUE,
	    weeding_labour_cost = sum_na(x$wage) / x$cropland[1])
	}))
	d5f <- merge(d5f, w, by=keys, all.x=TRUE)
	d5f$weeding_done[is.na(d5f$weeding_done)] <- FALSE
	
	####  merge d and d5f
	d <- merge(d, d5f, by=keys, all.x = TRUE)
	
	##### Fertilizer: sum the applications per parcel (season totals)
	## the raw N/P/K columns are the nutrient percentages of the product;
	## TOT_N/TOT_P/TOT_K are the elemental amounts (kg) per parcel
	## (checked: TOT_P for 14-14-14 = QTYKG * 0.14 / 2.29, i.e. already converted from P2O5)
	d6 <- data.frame(
	  date = r6$YEAR,
	  season = r6$SEASON,
	  hhid = as.character(r6$HHCODE),
	  plot_id = as.character(r6$PARNO),
	  cropland = round(r6$FSIZE, 4),
	  fertilizer_type = r6$BRAND,
	  N_fertilizer = as.numeric(r6$TOT_N),
	  P_fertilizer = as.numeric(r6$TOT_P),
	  K_fertilizer = as.numeric(r6$TOT_K),
	  fertilizer_amount = as.numeric(r6$QTYKG),
	  fertilizer_cost = as.numeric(r6$FERCOST)
	)
	## a few pesticide applications were recorded in the fertilizer sheet; drop them here
	## (they are also in the Crop Care sheet)
	d6 <- d6[!d6$fertilizer_type %in% c("2-4D", "Magnum", "Surekill"), ]
	
	ftype <- c("Urea"="urea", "urea"="urea", "46-0-0"="urea",
	  "Ammonium sulfate"="DAS", "ammonium sulfate"="DAS", "Sulfate"="DAS", "21-0-0"="DAS",
	  "0-0-60"="KCl")
	i <- match(d6$fertilizer_type, names(ftype))
	d6$fertilizer_type <- ifelse(!is.na(i), ftype[i],
	  # compound grades (14-14-14, 16-20-0, ...) and other products
	  ifelse(grepl("^[0-9]+-[0-9]+-[0-9]+$", d6$fertilizer_type), "NPK", tolower(d6$fertilizer_type)))
	
	d6 <- do.call(rbind, lapply(split(d6, d6[keys], drop=TRUE), function(x) {
	  data.frame(x[1, keys],
	    fertilizer_type = uniq_na(x$fertilizer_type),
	    # kg/ha and PHP/ha of parcel
	    N_fertilizer = sum_na(x$N_fertilizer) / x$cropland[1],
	    P_fertilizer = sum_na(x$P_fertilizer) / x$cropland[1],
	    K_fertilizer = sum_na(x$K_fertilizer) / x$cropland[1],
	    fertilizer_amount = sum_na(x$fertilizer_amount) / x$cropland[1],
	    fertilizer_cost = sum_na(x$fertilizer_cost) / x$cropland[1]
	  )
	}))
	####  merge d and d6
	d <- merge(d, d6, by=keys, all.x = TRUE)
	
	###
	d7 <- data.frame(
	  date = r7$YEAR,
	  season = r7$SEASON,
	  hhid = as.character(r7$HHCODE),
	  plot_id = as.character(r7$PARNO),
	  cropland = round(r7$FSIZE, 4),
	  activy = r7$ACTIVITY,
	  harvest_date = r7$DATE,
	  harvest_cost = as.numeric(gsub("PL", NA, r7$WAGE))
	)
	
	d7 <- d7[grepl("Harvesting", d7$activy),]
	d7$activy <- NULL
	d7$harvest_date <- gsub("17 N0v 2015", "2015-11-17", d7$harvest_date)
	d7$harvest_date <- gsub("10 Nov. 2015", "2015-11-10", d7$harvest_date)
	d7$harvest_date <- gsub("28 Nov. 2015", "2015-11-28", d7$harvest_date)
	d7$harvest_date <- gsub("25 Nov. 2015", "2015-11-25", d7$harvest_date)
	d7$harvest_date <- gsub("15 Nov. 2015", "2015-11-15", d7$harvest_date)
	d7$harvest_date <- gsub("03 Dec. 2015", "2015-12-03", d7$harvest_date)
	d7$harvest_date <- gsub("22 Oct. 2015", "2015-10-22", d7$harvest_date)
	d7$harvest_date <- gsub("29 Oct. 2015", "2015-10-29", d7$harvest_date)
	d7$harvest_date <- gsub("15 Oct. 2015", "2015-10-15", d7$harvest_date)
	d7$harvest_date <- gsub("25 Oct. 2015", "2015-10-25", d7$harvest_date)
	
	i <- !grepl("-", d7$harvest_date)
	d7$harvest_date[i] <- as.character(as.Date(as.numeric(d7$harvest_date[i]), origin = "1899-12-30"))
	
	## one row per operation (e.g. by worker sex); one row per parcel
	d7 <- do.call(rbind, lapply(split(d7, d7[keys], drop=TRUE), function(x) {
	  data.frame(x[1, keys],
	    harvest_date = min(x$harvest_date),
	    harvest_cost = sum_na(x$harvest_cost) / x$cropland[1]
	  )
	}))
	
	####  merge d and d7
	d <- merge(d, d7, by=keys, all  = TRUE)
	
	##
	d8 <- data.frame(
	  date = r8$YEAR,
	  season = r8$SEASON,
	  hhid = as.character(r8$HHCODE),
	  plot_id = as.character(r8$PARNO),
	  cropland = round(r8$FSIZE, 4),
	  variety = r8$VARIETY,
	  yield = r8$PRODNKG/r8$FSIZE,# kg/ha
	  yield_marketable = r8$SOLDKG/r8$FSIZE, ## kg/ha
	  crop_price = r8$PRICE
	)
	
	####  merge d and d8
	d <- merge(d, d8, by= c("hhid", "date", "season", "plot_id", "cropland", "variety"), all  = TRUE)
	
	####
	d9 <- data.frame(
	  date = r9$YEAR,
	  season = r9$SEASON,
	  hhid = as.character(r9$HHCODE),
	  plot_id = as.character(r9$PARNO),
	  cropland = r9$FSIZE,
	  seed_rate = r9$QTYSEED/r9$FSIZE,
	  seed_cost = r9$SEEDVAL/r9$FSIZE, # PHP/ha
	  seed_source = r9$SEEDSRC
	)
	
	####  merge d and d9
	d <- merge(d, d9, by= c("hhid", "date", "season", "plot_id", "cropland"), all  = TRUE)
	
	# an empty spreadsheet row produces a record without household id (all NA); drop it
	d <- d[!is.na(d$hhid), ]
	
	d$season <- c("wet", "dry")[d$season]
	d$planting_date[is.na(d$planting_date)] <- d$date[is.na(d$planting_date)]
	d$date <- NULL
	
	d$crop <- "rice"
	d$is_survey <- TRUE
	d$on_farm <- FALSE
	d$trial_id <- "1"
	d$yield_moisture <- NA
	d$yield_part <- "none"
	d$country <- "Philippines"
	d$location <- "Central Luzon"
	d$currency <- "PHP"
	d$yield_isfresh <- TRUE
	d$geo_from_source <- FALSE
	d$latitude <- 15.91459
	d$longitude <- 121.0841
	
	### 
#	d$harvest_date[(as.Date(d$harvest_date)-as.Date(d$planting_date)) < 20] <- NA
#	d$harvest_date[(as.Date(d$harvest_date)-as.Date(d$planting_date))> 366] <- NA
	
	carobiner::write_files(path, meta, d)
}


