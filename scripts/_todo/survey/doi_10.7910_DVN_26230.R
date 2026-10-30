# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI RTOP ("Mega project") farm monitoring of intensive irrigated rice at 9 sites in 6 Asian countries, 1994-1999; 1544 farm x season records.
# Only "Mega project process data_Pie Moya.xls" (sheet MegaAll) holds data; "Mega data_Pie Moya.doc" is the codebook. All "HA" variables are per hectare.
# IDSITE codes and places from the codebook (e.g. PR = PhilRice, Nueva Ecija; JI = ZAU, Jinhua, Zhejiang). IDSEASON = 2-digit year + season code.
# NHA, PHA, KHA are elemental N, P, K (kg/ha) according to the codebook, so no oxide conversion.
# PRODHA is production per ha (kg/ha), used as yield. FSIZE is area planted to rice (ha).
# Season codes: DS dry, WS wet, KH kharif, RB rabi; ER/LR = spring/summer rice (Hanoi) or early/late rice (Jinhua); TH thaladi, KR kuruvai, SM samba (Tamil Nadu).
# planting_date is only the season year from IDSEASON.
# Associated publications included in the dataset: Welch et al. 2010 PNAS (doi:10.1073/pnas.1001222107) and "The economics of intensively irrigated rice in Asia" (Moya et al.).

## ISSUES
# No coordinates or village names; only the 9 site names (Aduthurai, Thanjavur, Pantnagar, Omon, Hanoi, Sukamandi, Suphan Buri, Maligaya/Nueva Ecija, Jinhua). Human must georeference. Codebook lists PROVINCE, DISTRICT, VILLAGE but these columns are not in the xls.
# Pantnagar: codebook says Uttar Pradesh (state at the time); adm1 set to current GADM "Uttarakhand".
# Currency is not stated; assumed local currency per country (INR, VND, CNY, PHP, THB, IDR) based on price levels. Verify.
# Season code "WD" (Suphan Buri 1998, 9 rows) is not in the codebook; kept as "WD". Season values "spring", "summer", "early", "late", "thaladi", "kuruvai" are not in the season vocabulary.
# planting_date is the season year; thaladi/rabi crops may be sown in the previous calendar year. Verify against the season definitions.
# Manure type is not given (MANQTYHA only), so OM_type not set; very large MANQTYHA (up to 51724 kg/ha at AD) kept as reported.
# Pesticide active ingredient amounts (CRABAIHA, FUNGAIHA, HERAIHA, INSAIHA, MOLAIHA, RODAIHA; kg ai/ha assumed) mapped only for herbicide, insecticide, fungicide; crabicide, molluscicide, rodenticide kept as new variables.
# Some cost columns have implausible magnitudes (e.g. RODCSHA ~80000, HTLBCSHA ~400000) probably due to VND/IDR; costs mostly not mapped. Not mapped: all labour (family/exchange/hired/permanent) and other costs, sticker, hormone, drainage, land preparation and harvest/threshing costs.
# write_files: missing longitude/latitude/geo_from_source; invalid season terms listed above; new variables.

## NEW VARIABLES
# crabicide_amount_: crabicide active ingredient (kg/ha)
# molluscicide_amount_: molluscicide active ingredient (kg/ha)
# rodenticide_amount_: rodenticide active ingredient (kg/ha)


carob_script <- function(path) {

"
Reaching Toward Optimal Productivity (RTOP) with Socio-economic component

The major objectives of this project are to  a) understand the socioeconomic and technological differences in intensive irrigated rice production system across Asia in order to provide information on the context within which technology adoption (ex. SSNM) takes place,   b) to assess the trends in productivity at the farm level in a large number of intensive irrigated farming systems.
"

	uri <- "doi:10.7910/DVN/26230"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = "RTOP",
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

	f <- ff[basename(ff) == "Mega project process data_Pie Moya.xls"]
	r <- carobiner::read.excel(f, sheet="MegaAll")

	# site codes from "Mega data_Pie Moya.doc"
	sites <- data.frame(
		IDSITE = c("CL", "PR", "SU", "SB", "AD", "TH", "HA", "JI", "PA"),
		country = c("Vietnam", "Philippines", "Indonesia", "Thailand", "India", "India", "Vietnam", "China", "India"),
		adm1 = c("Cần Thơ", "Nueva Ecija", "Jawa Barat", "Suphan Buri", "Tamil Nadu", "Tamil Nadu", "Hà Nội", "Zhejiang", "Uttarakhand"),
		location = c("Omon", "Maligaya", "Sukamandi", "Suphan Buri", "Aduthurai", "Thanjavur", "Hanoi", "Jinhua", "Pantnagar"),
		currency = c("VND", "PHP", "IDR", "THB", "INR", "INR", "VND", "CNY", "INR")
	)

	sc <- substr(r$IDSEASON, 5, 6)
	season <- c(DS="dry", WS="wet", KH="kharif", RB="rabi", TH="thaladi", KR="kuruvai", SM="samba", WD="WD")[sc]
	er <- sc %in% c("ER", "LR")
	season[er & r$IDSITE == "HA"] <- c(ER="spring", LR="summer")[sc[er & r$IDSITE == "HA"]]
	season[er & r$IDSITE == "JI"] <- c(ER="early", LR="late")[sc[er & r$IDSITE == "JI"]]

	d <- data.frame(
		record_id = 1:nrow(r),
		trial_id = r$IDFARM,
		hhid = r$IDHHCODE,
		IDSITE = r$IDSITE,
		season = as.vector(season),
		planting_date = paste0("19", substr(r$IDSEASON, 3, 4)),
		crop = "rice",
		field_size = r$FSIZE,
		irrigated = TRUE,
		seed_rate = r$SEEDKGHA,
		N_fertilizer = r$NHA,
		P_fertilizer = r$PHA,
		K_fertilizer = r$KHA,
		fertilizer_cost = r$FERCOSHA,
		OM_used = r$MANQTYHA > 0,
		OM_amount = r$MANQTYHA,
		herbicide_amount = r$HERAIHA,
		herbicide_cost = r$HERCSHA,
		insecticide_amount = r$INSAIHA,
		insecticide_cost = r$INSCSHA,
		fungicide_amount = r$FUNGAIHA,
		crabicide_amount_ = r$CRABAIHA,
		molluscicide_amount_ = r$MOLAIHA,
		rodenticide_amount_ = r$RODAIHA,
		yield = r$PRODHA,
		crop_price = r$PRICE,
		crop_value = r$PRODVLHA
	)

	d <- merge(d, sites, by="IDSITE", all.x=TRUE)
	d$IDSITE <- NULL
	d <- d[order(d$record_id), ]
	d$record_id <- NULL

	d$herbicide_used <- d$herbicide_amount > 0
	d$insecticide_used <- d$insecticide_amount > 0
	d$fungicide_used <- d$fungicide_amount > 0
	d$yield_part <- "grain"
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
