# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI-CESD / UPLB / ICRR baseline household survey on drought adaptation in Indonesia (Indramayu - West Java, Pati - Central Java,
# Central Lombok - West Nusa Tenggara), 280 households, interviews 2012. Same questionnaire as the Philippine survey (doi:10.7910/DVN/26917).
# Record = household x season x parcel x plot, from "Section 4. Rice crops" joined to "Section 5. Output and disposal".
# Codes decoded from the CSPro coding manual PDF (pdftotext -raw). Season: ssid 1 = wet, 2 = dry.
# Production (eproduction) is kg (disposal columns are kg in the manual). yield = eproduction / darea (ha).
#   The "eyield" column (kg/ha) is filled for only 2 records and is not used.
# Planting date = month/day/year from "Section 6.1. Seed use" (matched on hh, season, parcel, plot).
# Fertilizer ("Section 6.3", f3chemical == 1): unit KW = kuintal = 100 kg (f3kgperunit is 0 for these rows); KG = 1 kg.
#   Nutrient contents: urea 46% N; ZA = ammonium sulfate 21% N, 24% S; TSP, KCl from terminag (P 19.23%, K 49.8%);
#   SP36 36% P2O5 (P = P2O5/2.29), 5% S; Phonska NPK 15-15-15 with 10% S (Petrokimia Gresik label, https://petrokimia-gresik.com).
#   Fertilizer is recorded per household x season (no parcel/plot); kg were divided by the household's total rice area that season.
#   Organic products (ORGANIC, ORGANIK, PETROGANIC, ...) are counted as organic matter, not as N/P/K.
# Soil texture, tenure and water source from "Section 2. Landholdings" (household x parcel x plot).

## ISSUES
# No coordinates. Human must georeference villages (location) within kecamatan (adm3) and kabupaten (adm2): see the code lists below.
#   Village 1 (Sumber mulya) appears once with commune 4 (Praya Barat) and village 11 (Mantingan Tengah) once with commune 5; probably entry errors.
#   Commune 7 "Sumber" is coded under district 2 (Pati); check the name (Sumber is a kecamatan of Rembang).
# Fertilizer is household x season, not plot: N/P/K kg/ha assume equal rates on all rice plots of that season.
# Products of unknown grade (NPK, PELANGI, NPK PELANGI, KUJANG, PUSRI, CANTIK, MAHKOTA, liquid/foliar products, ZN) give NA nutrient totals
#   for that household-season. Phonska and SP36 contents are from product labels, not the dataset; verify.
# Pesticides recorded as fertilizer rows (FURADAN, SCORE, VIRTAKO, PREVATON, MEOTIFIN, REAGEN, MATADOR, SWALOW) are excluded from fertilizer.
# Fertilizer rows with unit NA, BOTTLE or PACK have no kg conversion -> NA nutrient totals for that household-season.
# Very small plot areas (e.g. 0.007-0.025 ha) give yields up to 50 t/ha: check darea units. 19 plots have different variety names in Sections 4 and 5 (Section 4 used).
# Duplicate plot records: hh 145 dry season (one empty row), hh 288 (wet and dry rows duplicated).
# emilled (sold milled or unmilled) suggests some production may be milled rice; production assumed unmilled paddy.
# Prices (efgprice, f3price) appear to be in thousand IDR (e.g. 3.6 per kg paddy, 180 per kuintal urea) and esales is not quantity x price; not processed.
# "Section 6.4. Irrigation" has no yes/no flag (unlike the Philippine data); irrigated not set. Column K has text values ("4 DAY") that read as NA.
# Not processed: demography beyond head sex/age, consumption, assets, 6.2 machine use, pesticides, 6.5 gender labor, 10.1 non-rice crops,
#   income, credit, stress, coping, perception, social capital, access, decision making.
# fertilizer_type "SP36" (SP-36, 36% P2O5) is not in terminag; kept.
# write_files: missing longitude/latitude/geo_from_source; unknown seeding_type_, percent_irrigated_; NA yield;
#   out of bounds N_fertilizer (to 1128) and S_fertilizer, yield 50000 (tiny plot areas); duplicate records (hh 288).

## NEW VARIABLES
# seeding_type_: dry seeded, wet seeded or dibbled for direct-seeded plots (dmethod)
# percent_irrigated_: percentage of the plot that is irrigated (birrigated), %


carob_script <- function(path) {

"
A Baseline Study of Autonomous Adaptation Against Drought Among Indonesian Rice Farmers, CURE

This study in general aims to look at the impacts of drought on rice production, assess risk/vulnerability, and determine adaptation strategies of rice farming households for better targeting of planned adaptation strategies. The emphasis would be on the possibility of rice varietal introduction as a planned adaptive strategy. It is guided by the following research questions:  1.	Is drought a hazard farmer's face in the study areas?  2.	What are the possible impacts of drought in the selected study areas? 3.	What factors determine household choice of adaptive and coping mechanisms during drought episodes? 4.	How is household vulnerability to drought impacts using the concept of Vulnerability to Expected Poverty (VEP) measured? 5.	What household characteristics (or types) are most vulnerable to drought impacts? Special reference will be made to gender related issues or aspects. 6.	Does adaptive or coping mechanisms including social networks and the level of social capital, reduce the vulnerability of rice farming households?
"

	uri <- "doi:10.7910/DVN/26916"
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
		carob_completion = 35,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "A Baseline Study of Autonomous Adaptation Against Drought Among Indonesian Rice Farmers_Dataset_DManzanilla et al.xlsx"]

	r0 <- carobiner::read.excel(f, sheet="Section 0. Cover sheet")
	r1 <- carobiner::read.excel(f, sheet="Section 1.1. Demographic inform")
	r2 <- carobiner::read.excel(f, sheet="Section 2. Landholdings")
	r4 <- carobiner::read.excel(f, sheet="Section 4. Rice crops")
	r5 <- carobiner::read.excel(f, sheet="Section 5. Output and disposal")
	r61 <- carobiner::read.excel(f, sheet="Section 6.1. Seed use")
	r63 <- carobiner::read.excel(f, sheet="Section 6.3. Chemical and non-c")

	r5$key <- paste(r5$hhid, r5$ssid, r5$eparcel, r5$eplot)
	r4$key <- paste(r4$hhid, r4$ssid, r4$dparcel, r4$dplot)
	i5 <- match(r4$key, r5$key)

	d <- data.frame(
		hhid = as.character(r4$hhid),
		field_id = paste(r4$dparcel, r4$dplot, sep="-"),
		season = c("wet", "dry")[r4$ssid],
		crop = "rice",
		variety = trimws(r4$dvariety),
		variety_type = c("traditional", "modern")[r4$dvtype],
		planting_method = c("transplanting", "direct seeding", "direct seeding", "dibbling")[r4$dmethod],
		seeding_type_ = c(NA, "dry seeded", "wet seeded", NA)[r4$dmethod],
		field_size = r4$darea,
		yield = r5$eproduction[i5] / r4$darea,
		crop_amount_sold = r5$eqsold[i5]
	)
	d$yield_part <- "grain"
	d$country <- "Indonesia"
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	## location (coding manual)
	village <- c("Sumber mulya", "Kertanegara", "Jatimulya", "Situraja", "Rambitan", "Sengkol", "Segala Anyar", "Kawo", "Mangkung", "Kletek", "Mantingan Tengah", "Siti Mulyo", "Tondokerto", "Megulung")
	commune <- c("Gantar", "Haurgeulis", "Pujut", "Praya Barat", "Pucakwangi", "Jakenan", "Sumber")
	district <- c("Indramayu", "Pati", "Lombok Tengah")
	province <- c("Jawa Barat", "Jawa Tengah", "Nusa Tenggara Barat")
	h <- match(r4$hhid, r0$hhid)
	d$adm1 <- province[r0$province[h]]
	d$adm2 <- district[r0$district[h]]
	d$adm3 <- commune[r0$commune[h]]
	d$location <- village[r0$village[h]]

	## household head
	hd <- r1[r1$relationship == 1, ]
	hd <- hd[!duplicated(hd$hhid), ]
	j <- match(d$hhid, hd$hhid)
	d$sex <- c("male", "female")[hd$sex[j]]
	d$age <- hd$age[j]

	## planting date
	k61 <- paste(r61$hhid, r61$ssid, r61$f1parcel, r61$f1plot)
	j <- match(r4$key, k61)
	ok <- !is.na(j) & r61$f1month[j] %in% 1:12 & r61$f1day[j] %in% 1:31 & r61$f1year[j] %in% 2010:2012
	d$planting_date <- NA
	d$planting_date[ok] <- as.character(as.Date(paste(r61$f1year[j[ok]], r61$f1month[j[ok]], r61$f1day[j[ok]], sep="-")))
	# f1areaplanted is 0 for some records
	sarea <- r61$f1areaplanted[j]
	sarea[sarea %in% 0] <- NA
	d$seed_rate <- r61$f1qtyseed[j] / sarea

	## land
	k2 <- paste(r2$hhid, r2$bparcel, r2$bplot)
	j <- match(paste(r4$hhid, r4$dparcel, r4$dplot), k2)
	soil <- c("1"="clay", "2"="loam", "3"="sand", "4"="silt", "12"="clay loam", "23"="sandy loam", "34"="sandy silt")
	d$soil_texture <- soil[as.character(r2$bsoil[j])]
	tenure <- c("1"="owned", "2"="rented-in", "3"="rented-out", "4"="share-crop", "5"="government-owned", "6"="mortgaged")
	d$land_tenure <- tenure[as.character(r2$btenure[j])]
	wsource <- c("1"="rain", "2"="spring/river/creek", "3"="irrigation", "4"="shallow tube well", "5"="reservoir",
		"12"="rain; spring", "13"="rain; irrigation", "14"="rain; shallow tube well", "15"="rain; reservoir",
		"23"="spring; irrigation", "24"="spring; shallow tube well", "25"="spring; reservoir",
		"34"="irrigation; shallow tube well", "35"="irrigation; reservoir", "45"="shallow tube well; reservoir")
	d$irrigation_source <- wsource[as.character(r2$bwsource[j])]
	d$percent_irrigated_ <- r2$birrigated[j]

	## fertilizer (household x season)
	fe <- r63[r63$f3chemical %in% 1, ]
	fe$product <- toupper(trimws(fe$f3brandname))
	pest <- c("FURADAN", "SCORE", "VIRTAKO", "PREVATON", "MEOTIFIN", "REAGEN", "MATADOR", "SWALOW")
	fe <- fe[!fe$product %in% pest, ]
	fe$product[fe$product %in% c("UUREA", "UREAA")] <- "UREA"
	fe$product[fe$product %in% c("PHNSKA", "PHOSNKA", "PONSKA")] <- "PHONSKA"
	fe$product[fe$product == "SP 36"] <- "SP36"
	organic <- c("ORGANIC", "0RGANIC", "ORGANIK", "ORGANIC FERTILIZER", "ORGANIK FERTILIZER", "PETROGANIC")
	unit <- toupper(trimws(fe$f3unit))
	fe$kg <- fe$f3quantity * ifelse(unit == "KW", 100, ifelse(unit == "KG", 1, NA))
	# percentages N, P, K, S of the product
	comp <- data.frame(
		product = c("UREA", "ZA", "TSP", "KCL", "SP36", "PHONSKA"),
		N = c(46, 21, 0, 0, 0, 15),
		P = c(0, 0, 19.23, 0, 36/2.29, 15/2.29),
		K = c(0, 0, 0, 49.8, 0, 15/1.2051),
		S = c(0, 24, 2, 0, 5, 10),
		ftype = c("urea", "DAS", "TSP", "KCl", "SP36", "NPK")
	)
	i <- match(fe$product, comp$product)
	isorg <- fe$product %in% organic
	fe$N <- ifelse(isorg, 0, fe$kg * comp$N[i] / 100)
	fe$P <- ifelse(isorg, 0, fe$kg * comp$P[i] / 100)
	fe$K <- ifelse(isorg, 0, fe$kg * comp$K[i] / 100)
	fe$S <- ifelse(isorg, 0, fe$kg * comp$S[i] / 100)
	fe$ftype <- comp$ftype[i]
	fe$ftype[grepl("NPK|PELANGI", fe$product)] <- "NPK"
	fe$ftype[is.na(fe$ftype) & !isorg] <- "unknown"
	fe$hs <- paste(fe$hhid, fe$ssid)

	area <- tapply(r4$darea, paste(r4$hhid, r4$ssid), sum, na.rm=TRUE)
	fhs <- unique(fe$hs)
	sumv <- function(x) as.vector(tapply(x, fe$hs, sum)[fhs])
	fert <- data.frame(
		hs = fhs,
		N = sumv(fe$N),
		P = sumv(fe$P),
		K = sumv(fe$K),
		S = sumv(fe$S),
		ftype = as.vector(tapply(fe$ftype, fe$hs, function(x) paste(unique(x[!is.na(x)]), collapse="; "))[fhs]),
		OM = as.vector(tapply(fe$product %in% organic, fe$hs, any)[fhs]),
		area = as.vector(area[fhs])
	)
	fert$ftype[fert$ftype == ""] <- NA

	hs <- paste(r4$hhid, r4$ssid)
	j <- match(hs, fert$hs)
	d$fertilizer_used <- hs %in% fe$hs
	d$N_fertilizer <- fert$N[j] / fert$area[j]
	d$P_fertilizer <- fert$P[j] / fert$area[j]
	d$K_fertilizer <- fert$K[j] / fert$area[j]
	d$S_fertilizer <- fert$S[j] / fert$area[j]
	d$fertilizer_type <- fert$ftype[j]
	d$OM_used <- fert$OM[j]
	nof <- !d$fertilizer_used & hs %in% paste(r63$hhid, r63$ssid)
	d$N_fertilizer[nof] <- 0
	d$P_fertilizer[nof] <- 0
	d$K_fertilizer[nof] <- 0
	d$S_fertilizer[nof] <- 0
	d$fertilizer_type[nof] <- "none"
	d$OM_used[nof] <- FALSE

	d$trial_id <- paste(d$hhid, d$season, d$field_id, sep="_")

	carobiner::write_files(path, meta, d)
}
