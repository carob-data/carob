# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI-CESD / PhilRice baseline household survey on drought adaptation, Tarlac and Pangasinan (Philippines), 109 households.
# Interviews Dec 2011 - Jan 2012; production refers to the 2010-2011 wet (ssid 1) and dry (ssid 2) seasons (coding manual).
# Record = household x season x parcel x plot, from "Section 4. Rice crops" joined to "Section 5. Output and disposal" (same keys, varieties identical).
# All codes decoded from the CSPro coding manual PDF (read with pdftotext -raw; the -layout text shifts labels by one line).
# Village codes are not unique: code 23 is San Jacinto in Victoria (commune 17) and Salpad in San Nicolas (commune 36); decoded with village x commune.
# Production (eproduction) is kg: it equals quantity sold + consumed + payment + other (all kg in the manual). yield = eproduction / darea (ha).
# Planting date = month/day/year from "Section 6.1. Seed use" (matched on hh, season, parcel, plot; 201 of 278 plots match).
# Fertilizer ("Section 6.3", f3chemical == 1): product kg = f3quantity * f3kgperunit (data's own conversion).
#   N/P2O5/K2O taken from the grade in the product name (e.g. 14-14-14, 21-0-0); UREA = 46-0-0, SULFATE = ammonium sulfate 21-0-0 (24% S).
#   P = P2O5 / 2.29, K = K2O / 1.2051. Fertilizer is recorded per household x season (no parcel/plot), so kg were divided by
#   the total rice area of the household in that season and the same rate assigned to all its plots.
# Irrigated = TRUE when "Section 6.4. Irrigation" f4var == 1 (rows with application records) for the household-season, FALSE when 2.
# Soil texture, tenure and water source from "Section 2. Landholdings" (household x parcel x plot).

## ISSUES
# No coordinates in the data. Human must georeference villages: San Roque and Salpad (San Nicolas, Pangasinan);
#   Maungib (Pura, Tarlac); Cabuluan and San Jacinto (Victoria, Tarlac). Check that village-commune pairs are real barangays.
# Fertilizer is household x season, not plot: N/P/K kg/ha assume equal rates on all rice plots of that season.
# Products without a grade (CROPGIANT, SWIRE, ARMURE, GROWN STAR, COMPLETE) have unknown nutrient content; a household-season with any of these
#   gets NA for the nutrient totals. "COMPLETE" is often 14-14-14 in the Philippines but this is not stated; "25-0-0" kept as recorded.
# HH 4: fertilizer unit "KG" but f3kgperunit = 50 and prices of bags; used f3kgperunit as given (likely bags).
# Variety codes 29 and 99 are not in the coding manual; kept as codes.
# yield 0 for 6 plots (crop failure?); hh 9 dry season > 10 t/ha (check area). One plot (hh 66, ssid 2) has no area -> NA yield.
# Not processed: 1.1-1.3 demography beyond head sex/age, consumption, assets, 6.2 machine/animal use, pesticide rows of 6.3,
#   6.5 gender labor, 10.1-10.2 non-rice crops (units messy: BAG, KG, TRUCK, numbers), income, credit, stress, coping, perception, social capital.
# Harvest dates are only in 6.2 (activity rows); not extracted.
# Soil code 34 "sandy-silt" kept as "sandy silt" (not a terminag texture class). fertilizer_type "unknown" used for brand names and 25-0-0/16-0-0.
# Households with chemical rows but no fertilizer row in a season get 0 N/P/K/S and fertilizer_type "none"; verify they truly applied none.
# write_files: missing longitude/latitude/geo_from_source; unknown seeding_type_, percent_irrigated_; NA yield (one plot without area).

## NEW VARIABLES
# variety_type_: traditional or modern variety (dvtype)
# percent_irrigated_: percentage of the plot that is irrigated (birrigated), %


carob_script <- function(path) {

"
A Baseline Study of Autonomous Adaptation Against Drought Among Filipino Rice Farmers, CURE

This study in general aims to look at the impacts of drought on rice production, assess risk/vulnerability, and determine adaptation strategies of rice farming households for better targeting of planned adaptation strategies. The emphasis would be on the possibility of rice varietal introduction as a planned adaptive strategy. It is guided by the following research questions:  1.	Is drought a hazard farmers face in the study areas?  2.	What are the possible impacts of drought in the selected study areas? 3.	What factors determine household choice of adaptive and coping mechanisms during drought episodes? 4.	How is household vulnerability to drought impacts using the concept of Vulnerability to Expected Poverty (VEP) measured? 5.	What household characteristics (or types) are most vulnerable to drought impacts? Special reference will be made to gender related issues or aspects. 6.	Does adaptive or coping mechanisms including social networks and the level of social capital, reduce the vulnerability of rice farming households?
"

	uri <- "doi:10.7910/DVN/26917"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = "CURE",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 40,
		carob_effort = 0.8
	)

	f <- ff[basename(ff) == "A Baseline Study of Autonomous Adaptation Against Drought Among Filipino Rice Farmers_Dataset_DManzanilla et al.xlsx"]

	r0 <- carobiner::read.excel(f, sheet="Section 0. Cover sheet")
	r1 <- carobiner::read.excel(f, sheet="Section 1.1. Demographic inform")
	r2 <- carobiner::read.excel(f, sheet="Section 2. Landholdings")
	r4 <- carobiner::read.excel(f, sheet="Section 4. Rice crops")
	r5 <- carobiner::read.excel(f, sheet="Section 5. Output and disposal")
	r61 <- carobiner::read.excel(f, sheet="Section 6.1. Seed use")
	r63 <- carobiner::read.excel(f, sheet="Section 6.3. Chemical and non-c")
	r64 <- carobiner::read.excel(f, sheet="Section 6.4. Irrigation")

	vars <- c("NSIC Rc160", "NSIC Rc152", "NSIC Rc218", "NSIC Rc216", "NSIC Rc154", "PSB Rc18", "SL8-H", "NSIC Rc222", "NSIC Rc130", "PSB Rc10", "NSIC Rc144", "NSIC Rc82", "NSIC Rc122", "Dinorado", "Jasmin", "Glutinuous Rice", "NSIC Rc224", "NSIC Rc168", "PSB Rc14", "NSIC Rc134", "NSIC Rc128", "NSIC Rc214", "Angelica", "NSIC Rc126", "NSIC Rc125", "NSIC Rc156", "PSB Rc64", "NSIC Rc212")
	variety <- vars[r4$dvariety]
	variety[is.na(variety)] <- as.character(r4$dvariety[is.na(variety)])

	r5$key <- paste(r5$hhid, r5$ssid, r5$eparcel, r5$eplot)
	r4$key <- paste(r4$hhid, r4$ssid, r4$dparcel, r4$dplot)
	i5 <- match(r4$key, r5$key)

	d <- data.frame(
		hhid = as.character(r4$hhid),
		field_id = paste(r4$dparcel, r4$dplot, sep="-"),
		season = c("wet", "dry")[r4$ssid],
		crop = "rice",
		variety = variety,
		variety_type = c("traditional", "modern")[r4$dvtype],
		planting_method = c("transplanting", "direct seeding", "direct seeding")[r4$dmethod],
		field_size = r4$darea,
		yield = r5$eproduction[i5] / r4$darea,
		crop_price = r5$efgprice[i5],
		crop_amount_sold = r5$eqsold[i5]
	)
	# dmethod 2 = dry-seeded, 3 = wet-seeded rice
	d$seeding_type_ <- c(NA, "dry seeded", "wet seeded")[r4$dmethod]
	d$currency <- "PHP"
	d$yield_part <- "grain"
	d$country <- "Philippines"
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	## location
	r0$village_name <- ifelse(r0$village == 23 & r0$commune == 17, "San Jacinto",
		ifelse(r0$village == 23 & r0$commune == 36, "Salpad",
		c("7"="Maungib", "8"="Cabuluan", "31"="San Roque")[as.character(r0$village)]))
	h <- match(r4$hhid, r0$hhid)
	d$adm1 <- c("55"="Pangasinan", "69"="Tarlac")[as.character(r0$province[h])]
	d$adm2 <- c("11"="Pura", "17"="Victoria", "36"="San Nicolas")[as.character(r0$commune[h])]
	d$location <- r0$village_name[h]

	## household head
	hd <- r1[r1$relationship == 1, ]
	hd <- hd[!duplicated(hd$hhid), ]
	j <- match(d$hhid, hd$hhid)
	d$sex <- c("male", "female")[hd$sex[j]]
	d$age <- hd$age[j]

	## planting date
	k61 <- paste(r61$hhid, r61$ssid, r61$f1parcel, r61$f1plot)
	j <- match(r4$key, k61)
	ok <- !is.na(j) & r61$f1month[j] %in% 1:12 & r61$f1day[j] %in% 1:31 & r61$f1year[j] %in% 2010:2011
	d$planting_date <- NA
	d$planting_date[ok] <- as.character(as.Date(paste(r61$f1year[j[ok]], r61$f1month[j[ok]], r61$f1day[j[ok]], sep="-")))
	d$seed_rate <- r61$f1qtyseed[j] / r61$f1areaplanted[j]

	## land
	k2 <- paste(r2$hhid, r2$bparcel, r2$bplot)
	j <- match(paste(r4$hhid, r4$dparcel, r4$dplot), k2)
	soil <- c("1"="clay", "2"="loam", "3"="sand", "4"="silt", "12"="clay loam", "23"="sandy loam", "34"="sandy silt")
	d$soil_texture <- soil[as.character(r2$bsoil[j])]
	tenure <- c("1"="owned", "2"="rented-in", "3"="rented-out", "4"="share-crop", "5"="government-owned")
	d$land_tenure <- tenure[as.character(r2$btenure[j])]
	wsource <- c("1"="rain", "2"="spring/river/creek", "3"="irrigation", "4"="shallow tube well", "12"="rain; spring", "13"="rain; irrigation", "14"="rain; shallow tube well", "23"="spring; irrigation", "24"="spring; shallow tube well", "34"="irrigation; shallow tube well")
	d$irrigation_source <- wsource[as.character(r2$bwsource[j])]
	d$percent_irrigated_ <- r2$birrigated[j]

	## irrigation (household x season)
	ir <- unique(r64[r64$f4var %in% 1:2, c("hhid", "ssid", "f4var")])
	j <- match(paste(r4$hhid, r4$ssid), paste(ir$hhid, ir$ssid))
	d$irrigated <- ir$f4var[j] == 1

	## fertilizer (household x season)
	fe <- r63[r63$f3chemical == 1, ]
	fe$product <- toupper(trimws(fe$f3brandname))
	fe$kg <- fe$f3quantity * fe$f3kgperunit
	fe$product[fe$product == "UREA"] <- "46-0-0"
	fe$product[fe$product == "SULFATE"] <- "21-0-0"
	grade <- strsplit(ifelse(grepl("^[0-9]+-[0-9]+-[0-9]+$", fe$product), fe$product, "NA-NA-NA"), "-")
	grade <- matrix(as.numeric(gsub("NA", NA, unlist(grade))), ncol=3, byrow=TRUE)
	fe$N <- fe$kg * grade[,1] / 100
	fe$P <- fe$kg * grade[,2] / 100 / 2.29
	fe$K <- fe$kg * grade[,3] / 100 / 1.2051
	fe$S <- ifelse(fe$product == "21-0-0", fe$kg * 0.24, 0)
	fe$S[is.na(grade[,1])] <- NA
	ftype <- c("46-0-0"="urea", "21-0-0"="DAS", "0-0-60"="KCl", "16-20-0"="NP", "14-14-14"="NPK", "17-17-17"="NPK", "17-0-17"="NPK", "25-0-0"="unknown", "16-0-0"="unknown")
	fe$ftype <- ftype[fe$product]
	fe$ftype[is.na(fe$ftype)] <- "unknown"
	fe$hs <- paste(fe$hhid, fe$ssid)

	area <- tapply(r4$darea, paste(r4$hhid, r4$ssid), sum, na.rm=TRUE)
	fhs <- unique(fe$hs)
	fert <- data.frame(
		hs = fhs,
		N = as.vector(tapply(fe$N, fe$hs, sum)[fhs]),
		P = as.vector(tapply(fe$P, fe$hs, sum)[fhs]),
		K = as.vector(tapply(fe$K, fe$hs, sum)[fhs]),
		S = as.vector(tapply(fe$S, fe$hs, sum)[fhs]),
		ftype = as.vector(tapply(fe$ftype, fe$hs, function(x) paste(unique(x), collapse="; "))[fhs])
	)
	fert$area <- as.vector(area[fert$hs])

	hs <- paste(r4$hhid, r4$ssid)
	j <- match(hs, fert$hs)
	d$fertilizer_used <- hs %in% fe$hs
	d$N_fertilizer <- fert$N[j] / fert$area[j]
	d$P_fertilizer <- fert$P[j] / fert$area[j]
	d$K_fertilizer <- fert$K[j] / fert$area[j]
	d$S_fertilizer <- fert$S[j] / fert$area[j]
	d$fertilizer_type <- fert$ftype[j]
	## households that reported no fertilizer rows for the season
	nof <- !d$fertilizer_used & hs %in% paste(r63$hhid, r63$ssid)
	d$N_fertilizer[nof] <- 0
	d$P_fertilizer[nof] <- 0
	d$K_fertilizer[nof] <- 0
	d$S_fertilizer[nof] <- 0
	d$fertilizer_type[nof] <- "none"

	d$trial_id <- paste(d$hhid, d$season, d$field_id, sep="_")

	carobiner::write_files(path, meta, d)
}
