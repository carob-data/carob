# R script for "carob"
# license: GPL (>=3)

## NOTES
# CPWF-IFAD baseline household survey (2006), Van Chan district, North Vietnam; 2 villages (code description: 1 = Suoi Giang, 2 = Nam Bung), ~200 farmers.
# Sheets are "Excel forms": several descriptive header rows, short variable names in the last header row (skip=8 or 7). Some leading helper columns (UpLow, x, Sort) are ignored.
# Main records: Landhold-Landuse.xls "Lowland use" (crops grown in the lowland in 2005) and "Upland use" (crops grown on upland parcels 2001-2005, column Year).
# One row per household x land type x parcel x plot x season x year x crop. yield = Prod (kg) / Area (ha). Rice production is rough rice (paddy) per the code description.
# Lowland "Area" in the plot columns is empty; the parcel area from the land resource sheet (helper column 2) is used instead.
# Crop, variety, establishment codes decoded from "Code description.xls".
# Fertilizer from "Rice inputs.xls" Input sheet (rice only, 2005), merged by village, farmer, parcel, plot, season and land type; kg product / Area (ha).
# Nutrient contents from the code description: urea 46-0-0; "Phosphorus" 0-22-0; mixed NPK 3-10-5; potassium 0-0-51. These are treated as N-P2O5-K2O grades and converted to elemental P (/2.29) and K (/1.2051).

## ISSUES
# No coordinates. Georeference villages Suoi Giang and Nam Bung (Van Chan district, Yen Bai province) manually.
# The code-table grades (P 22, NPK 3-10-5, K 51) are assumed to be oxide grades; verify. NPK 3-10-5 may be a mis-ordered 5-10-3.
# Lowland plot area = parcel area; when a parcel has several plots in a season, yield may be underestimated. Verify.
# Upland use covers 2001-2005; fertilizer data are only merged to 2005 records (Rice inputs refers to the reference year). Check that this is right.
# Mixed cropping (Cropping = 2): the code description gives area shares used to "estimate area for mixed crops"; unclear if Area already reflects this split.
# Crop code 20 "Keo (glue tree)" (Acacia) mapped to "acacia", which is not a terminag crop. Crop code 33 "Rong" mapped to taro (code description: "Rong (Taro)").
# yield_part for tea (leaves), cinnamon (bark? set to "stems"), and acacia ("wood") needs checking.
# Upland "Plot" contains "1&2" in one row (read as NA by read.excel: "Expecting numeric" warning).
# Not processed: Assets, Consumption-Food security, Crop disposal, Family info (head age/sex/education), Income, Nonrice inputs (costs), Others (credit, practices, irrigation), Rice inputs Labor, seed/organic/pesticide costs.
# write_files: missing coordinates; crop "acacia" invalid; yield out of bounds and "high crop yield" (cassava/maize/taro up to 900000 kg/ha) on very small upland plots (0.002-0.05 ha) - check area units/typos; seed_rate up to 24000 kg/ha for the same reason; 5 exact duplicate records (e.g. hh 1-26 tea) in Upland use; NA yield where Prod is empty.

## NEW VARIABLES
# land_type_: lowland or upland parcel
# establishment_method_: rice crop establishment code (TPR = transplanted, DSR = dry direct seeded, WSR = wet direct seeded)


carob_script <- function(path) {

"
CPWF-IFAD baseline survey in Vietnam

This farm household survey is an activity under the socio-economic component of the project entitled 'Managing Rice Landscapes in Marginal Uplands for Household Food Security and Environmental Sustainability'. The objective of the project is to improve food security of farming households in the marginal highlands through more efficient production over the rice landscape. The socio-economic component focuses on analyzing how various component technologies contribute to improving farmers' livelihoods. Specific objectives are a) to describe and understand existing farm practices and livelihoods in the marginal uplands; b) to identify possible points of intervention for improving productivity and food security and initiating income growth; and c) to serve as a baseline for future impact assessment studies. Results of the survey were used in another project entitled 'Rice Landscape Management for Raising Water Productivity, Conserving Resources, and Improving Livelihoods in Upper Catchments of the Mekong and Red River Basins'.
"

	uri <- "doi:10.7910/DVN/26159"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = "CPWF-IFAD",
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

	f1 <- ff[basename(ff) == "Landhold-Landuse.xls"]
	f2 <- ff[basename(ff) == "Rice inputs.xls"]
	rL <- carobiner::read.excel(f1, sheet="Lowland use", skip=8)
	rU <- carobiner::read.excel(f1, sheet="Upland use", skip=8)
	rI <- carobiner::read.excel(f2, sheet="Input", skip=7)

	crops <- c("1"="rice", "3"="maize", "5"="groundnut", "14"="cassava", "15"="soybean", "16"="tea",
		"19"="cinnamon", "20"="acacia", "24"="cabbage", "25"="pumpkin", "26"="ginger", "28"="longan", "33"="taro")
	varieties <- c("1"="Nhi Uu 838", "2"="Red non-glutinous", "3"="Striped non-glutinous", "4"="White non-glutinous",
		"5"="White sticky", "6"="Loc rice", "7"="Local sticky", "8"="Tien Uu 63", "9"="Non-glutinous")
	cem <- c("1"="TPR", "2"="DSR", "3"="WSR")

	d1 <- data.frame(
		village = rL$Village,
		farmer = rL$Farmer,
		field_id = rL$Parcel,
		plot_id = as.character(rL$Plot),
		season = c("wet", "dry")[rL$Season],
		planting_date = "2005",
		crop_code = rL$Crop,
		intercropped = rL$Cropping == 2,
		field_size = rL[["Area...2"]],
		production = rL$Prod,
		variety = varieties[as.character(rL$Variety)],
		variety_type = c("modern", "traditional")[rL$MVTV],
		variety_traits = c("glutinous", "non-glutinous")[rL$Glut],
		establishment_method_ = cem[as.character(rL$CEMethod)],
		land_type_ = "lowland"
	)

	d2 <- data.frame(
		village = rU$Village,
		farmer = rU$Farmer,
		field_id = rU$Parcel,
		plot_id = as.character(rU$Plot),
		season = c("wet", "dry")[rU$Season],
		planting_date = as.character(rU$Year),
		crop_code = rU$Crop,
		intercropped = rU$Cropping == 2,
		field_size = rU$Area,
		production = rU$Prod,
		seed_rate = rU$Seed / rU$Area,
		variety = varieties[as.character(rU$Variety)],
		variety_type = c("modern", "traditional")[rU$MVTV],
		variety_traits = c("glutinous", "non-glutinous")[rU$Glut],
		establishment_method_ = cem[as.character(rU$CEMethod)],
		land_type_ = "upland"
	)

	d <- carobiner::bindr(d1, d2)
	d <- d[!is.na(d$crop_code), ]
	d$crop <- crops[as.character(d$crop_code)]
	d$yield <- d$production / d$field_size
	d$planting_method <- ifelse(d$establishment_method_ == "TPR", "transplanting", "direct seeding")
	d$location <- c("Suoi Giang", "Nam Bung")[d$village]
	d$hhid <- paste(d$village, d$farmer, sep="-")

	# fertilizer on rice, kg product per plot -> kg/ha
	fert <- data.frame(
		land_type_ = tolower(rI$Landtype),
		key = paste(rI$Village, rI$Farmer, rI$Parcel, rI$Plot, rI$Season),
		OM_amount = rI$OrgQty / rI$Area
	)
	# empty quantity cells in the input form are taken to mean the product was not used
	for (v in c("FertQty1", "FertQty2", "FertQty3", "FertQty4")) {
		rI[[v]][is.na(rI[[v]])] <- 0
	}
	fert$N_fertilizer <- (0.46 * rI$FertQty1 + 0.03 * rI$FertQty3) / rI$Area
	fert$P_fertilizer <- (0.22 * rI$FertQty2 + 0.10 * rI$FertQty3) / 2.29 / rI$Area
	fert$K_fertilizer <- (0.05 * rI$FertQty3 + 0.51 * rI$FertQty4) / 1.2051 / rI$Area
	fert$fertilizer_type <- apply(rI[, c("FertQty1", "FertQty2", "FertQty3", "FertQty4")] > 0, 1,
		function(i) paste(c("urea", "SSP", "NPK", "KCl")[i], collapse="; "))
	fert$fertilizer_type[fert$fertilizer_type == ""] <- "none"

	d$key <- paste(d$village, d$farmer, d$field_id, d$plot_id, c(wet=1, dry=2)[d$season])
	d$key[d$crop != "rice" | d$planting_date != "2005"] <- NA
	d <- merge(d, fert, by=c("land_type_", "key"), all.x=TRUE)

	d$village <- d$farmer <- d$crop_code <- d$production <- d$key <- NULL

	d$country <- "Vietnam"
	d$adm1 <- "Yen Bai"   # Van Chan district is in Yen Bai province
	d$adm2 <- "Van Chan"
	d$trial_id <- d$hhid
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$irrigated <- NA
	d$yield_part <- "grain"
	d$yield_part[d$crop == "cassava"] <- "roots"
	d$yield_part[d$crop == "taro"] <- "tubers"
	d$yield_part[d$crop == "ginger"] <- "rhizome"
	d$yield_part[d$crop %in% c("tea", "cabbage")] <- "leaves"
	d$yield_part[d$crop %in% c("pumpkin", "longan")] <- "fruit"
	d$yield_part[d$crop == "groundnut"] <- "pod"
	d$yield_part[d$crop == "cinnamon"] <- "stems"
	d$yield_part[d$crop == "acacia"] <- "wood"

	carobiner::write_files(path, meta, d)
}
