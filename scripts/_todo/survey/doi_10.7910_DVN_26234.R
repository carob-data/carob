# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI / OUAT panel survey on risk in rice production, 1996-2001, villages Taraboi (Khurda district) and Thailo (Jagatsinghpur district), Orissa (Odisha), India. Farmers stratified by farm size (TYPE: marginal, small, large).
# Farmers are numbered within village and farm-size class; hhid = village-type-farmer.
# Land use ("Land profile.xlsx", Landuse): one row per year x season x parcel x plot x subplot with land type, soil, crop, variety, establishment method, area (ha), production (qt). yield = PROD * 100 / AREA (kg/ha). Price is Rs/qt and converted to Rs/kg.
# Parcel irrigation and tenure from the Landtype sheet merged by year, farmer and parcel (irrigated = IRAREA > 0).
# Fertilizer ("Rice-Inputs and Labor.xlsx", Fert) for rice plots (kharif): basal + up to 4 top dressings x 3 products, all in kg. Grades from the code description: urea 46% N, Super 16% P, DAP 18-46-0, potash (MOP) 58% K, Grommer 10-26-26, Shymala 28-28-0, CAN 26% N, Raja Ammonia 20% N. "P" and "K" contents were taken as P2O5 and K2O (as is usual for these products); P = P2O5/2.29, K = K2O/1.2051.
# Manure (kg) from the same sheet mapped to OM_amount.
# Planting and harvest dates from the Harvest sheet (week codes 1-48: month = (code-1) %/% 4 + 1) as "yyyy-mm".

## ISSUES
# No coordinates. Georeference villages Taraboi (Khurda) and Thailo (Jagatsinghpur), Odisha.
# Fertilizer code "0.5" (1 record) is not in the code description; that record has NA N/P/K.
# The code description of 26233 (same team) lists Grommer as 28-28-0 and Iffco as 10-26-26, here Grommer is 10-26-26 and Shymala 28-28-0; check.
# Fert, Harvest sheets have no season column; assumed kharif rice and merged to kharif rice records only.
# Variety code 105 ("- By - ???") set to NA. Variety codes for non-rice crops only indicate modern/local.
# Not processed: household info, income, assets, labor and power use, seedbed, herbicide, insecticide, irrigation details, risk modules (Risk1-3, Risk management), non-rice inputs (MatInp).
# Some rice plots have several varieties ("6&11", "52,81 & 96"); these are kept as "variety1; variety2" (area and yield are not split).
# Fertilizer and dates matched to 1132 of 6999 rice records (the rice input module covers selected plots).
# Landtype LRENTKG has text values ("Purchased") read as NA; not used.
# write_files: invalid season "summer" (zaid), crops "cabbage; cauliflower", "horse gram", "mung bean; lentil", "vegetables"; fertilizer types "10-26-26", "28-28-0"; OM_type "manure"; low yields (groundnut, mixed crops); NA yield for some records.
# Mixed crop records ("Mung + Lentil", "Cabbage + Cauliflower") kept as "crop1; crop2" with intercropped = TRUE; "-Continuing" crops (perennial crops from previous season) set to the crop.

## NEW VARIABLES
# farm_size_class_: sampling stratum (marginal, small, large)
# land_type_: upland, medium land, lowland


carob_script <- function(path) {

"
Risk management in rice production in inland areas of Orissa

The purpose of the project is to analyze the nature of risk in rice production in Orissa and draw out implications for technology design and policy improvement to help farmers better manage the climatic risk in rice production. Although the risk associated with uncertainty in prices may also be important to some farmers, the focus of the paper is on climatic risk and how farmers have adapted to such risk.
"

	uri <- "doi:10.7910/DVN/26234"
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
		carob_effort = 0.6
	)

	f1 <- ff[basename(ff) == "Land profile.xlsx"]
	f2 <- ff[basename(ff) == "Rice-Inputs and Labor.xlsx"]
	r1 <- carobiner::read.excel(f1, sheet="Landuse", skip=6)
	r2 <- carobiner::read.excel(f1, sheet="Landtype", skip=6)
	r3 <- carobiner::read.excel(f2, sheet="Fert", skip=8)
	r4 <- carobiner::read.excel(f2, sheet="Harvest", skip=8)

	vars <- c("1"="Lalat", "2"="Mashuri", "3"="Mayurkantha", "4"="T-141", "5"="Telisakar", "6"="Swarna Mahsuri", "7"="Ratna Chudi", "8"="Gopalbhoga", "9"="CR 1009", "10"="Azana", "11"="Bauluchi", "12"="Khandagiri", "13"="CR 1014", "14"="Kalama", "15"="Khandasagar", "16"="Hybrid", "17"="Puagi", "18"="Machhua bankei", "19"="Sukasari", "20"="Tulasi", "21"="Godikhajara", "22"="Rusikulya", "23"="Golabhundhi", "24"="Hatipanjara", "25"="Kakhara", "26"="Saruosha", "27"="Padmakeshari", "28"="Rangiarai", "29"="CR 1030", "30"="Motora", "31"="Jaganathkantia", "32"="Laghu Bankai", "33"="Laghu Saunlimadhei", "34"="Kanak Champa", "35"="Kalafarm", "36"="Sana Gopalbhog", "37"="Jimania", "38"="Chandrakanti", "39"="Suakantha", "40"="Nadiafula", "41"="Leondi Champa", "42"="Mahipal", "43"="Kataki Champa", "44"="Baijhali", "45"="CR 1009 (white stem)", "46"="Laghu Ratanchudi", "47"="Daunagoba", "48"="Chota Champa", "49"="Bada Saunlimadhei", "51"="Matia", "52"="OR-1590", "53"="Machakanta", "54"="Oriyan", "55"="Khajura", "56"="Parijat", "57"="CR-1015", "58"="CR-1018", "59"="Baunsagaja", "60"="Budhamanda", "61"="T-1242", "62"="B-9", "63"="CR-1018", "64"="Nadia Rangi", "65"="Muthaguli", "66"="Basana Bangi", "67"="Kakudia", "68"="Hirakani", "69"="Harishankar", "70"="Balabhadra Pakshia", "71"="Moti", "72"="Panisandha", "73"="Chandrakanta", "74"="Cuttack-B", "75"="Bangi", "76"="Basumati", "77"="CR-1017", "78"="Kakudimanji", "79"="Pathuria", "80"="Panianla", "81"="Cuttack Chandi", "82"="NaliMakara", "83"="Pakhira", "84"="Danda Balunga", "85"="CR-1004", "86"="Kaniya", "87"="Kalamula", "88"="Lunishree", "89"="Gajaramala", "90"="Chanduri", "91"="Gudumatia", "92"="OR-2036", "93"="Luhathenga", "94"="Mahalakshmi", "95"="Kala Pakshia", "96"="Patali", "97"="Khajuri champa", "98"="Ballava champa", "99"="Baikani", "100"="Bada gopalbhog", "101"="CR 90", "102"="Lunipatali", "103"="CR-77", "104"="Saruchuna", "106"="CR-36", "107"="Betanasia", "108"="Sunder Madhei", "109"="OR-371", "110"="Khapuri sundari", "111"="Saunti", "112"="Nalikhuda", "113"="Kalamahipal", "114"="Nidhia", "115"="Bhutia", "116"="Magurmechi", "117"="Rangapakhia", "118"="Manika", "119"="Behampani - 9", "120"="Chinikamini", "121"="Farm Bhudamanda", "122"="Kanchan", "123"="Jagabalia", "124"="OR-2061", "125"="Champa", "126"="Mani", "127"="Kedargouri", "128"="Ratna", "129"="Srilakhsmi", "130"="Anada", "131"="Gaurishanka", "132"="OR-2071", "133"="Barun", "134"="Agni Kumari", "135"="Athagadia rangi", "136"="Puruhapata", "137"="CR-1004", "138"="Nariamagur", "139"="OR-1212", "140"="Pankaj", "141"="Kamini", "142"="5656", "143"="Srilakshmi", "144"="OR-1036", "145"="Sunderpatua", "146"="T-141", "147"="Basanti", "148"="2056 (Rajahindustani)", "149"="Ramchandi", "150"="BTP", "151"="Panidhan", "152"="DPD", "153"="Jatakamal", "154"="2086", "155"="Daya")
	modern <- c(1, 2, 4, 6, 9, 12, 13, 16, 20, 29, 45, 52, 56, 57, 58, 61, 63, 71, 77, 85, 88, 92, 94, 101, 103, 106, 109, 118, 122, 124, 128, 130, 132, 137, 139, 140, 142, 144, 146, 148, 150, 151, 154, 155)
	crops <- c("Rice"="rice", "Mung"="mung bean", "Horsegram"="horse gram", "Vegetable"="vegetables", "Vegetable-Continuing"="vegetables", 
		"Colocasia"="taro", "Sugarcane"="sugarcane", "Blackgram"="black gram", "Chilli"="chili pepper", "Chilli-Continuing"="chili pepper", 
		"Potato"="potato", "Lentil"="lentil", "Watermelon"="watermelon", "Pumpkin"="pumpkin", "Jute"="jute", "Tomato"="tomato", 
		"Cabbage"="cabbage", "Cauliflower"="cauliflower", "Cucumber"="cucumber", "Mung + Lentil"="mung bean; lentil", "Ragi"="finger millet", 
		"Bitter gourd"="bitter gourd", "Greens"="vegetables", "Groundnut"="groundnut", "Cabbage + Cauliflower"="cabbage; cauliflower", "Fallow"="none")

	vill <- c("Taraboi"=1, "Thailo"=2)
	d <- data.frame(
		year = r1$YEAR,
		village = vill[r1$VILLAGE],
		type = as.integer(substr(r1$TYPE, 1, 1)),
		farmer = r1$FARMER,
		parcel = r1$PARCEL,
		plot = r1$PLOT,
		subplot = r1$SUBPLOT,
		season = tolower(r1$SEASON),
		land_type_ = c("upland", "medium land", "lowland")[as.integer(substr(r1$LAND, 1, 1))],
		soil_texture = c("clay", "clay loam", "sandy clay loam", "sandy loam", "loamy sand", "loam", "sand", "sand")[r1$SOIL],
		crop = crops[r1$CROP],
		establishment_method_ = r1$CEMETHOD,
		planting_method = c(DSR="direct seeding", TPR="transplanting", WSR="direct seeding", Broadcast="broadcasting")[r1$CEMETHOD],
		field_size = r1$AREA,
		yield = r1$PROD * 100 / r1$AREA,
		fwy_residue = r1$BYPROD * 100 / r1$AREA,
		crop_price = r1$PRICE / 100
	)
	d$soil_type <- c("gravelly sand")[match(r1$SOIL, 8)]
	d$intercropped <- grepl("\\+", r1$CROP)
	isrice <- d$crop %in% "rice"
	vcode <- carobiner::read.excel(f1, sheet="Landuse", skip=6, col_types="text")$VARIETY
	vcode <- strsplit(gsub(" ", "", vcode), "[,&]")
	d$variety <- sapply(vcode, \(v) { v <- vars[v]; if (all(is.na(v))) NA else paste(na.omit(v), collapse="; ") })
	d$variety_type <- sapply(vcode, \(v) { v <- v[v %in% names(vars)]; if (length(v) == 0) NA else paste(unique(ifelse(v %in% modern, "modern", "traditional")), collapse="; ") })
	d$variety[!isrice] <- NA
	d$variety_type[!isrice] <- c("modern", "traditional")[r1$VARIETY[!isrice]]

	## parcels
	lt <- data.frame(
		year = r2$YEAR,
		village = vill[r2$VILLAGE],
		type = as.integer(substr(r2$TYPE, 1, 1)),
		farmer = r2$FARMER,
		parcel = r2$PARCEL,
		irrigated = r2$IRAREA > 0,
		land_tenure = tolower(r2$TENURE)
	)
	lt <- lt[!duplicated(lt[, 1:5]), ]
	d <- merge(d, lt, by=c("year", "village", "type", "farmer", "parcel"), all.x=TRUE)

	## fertilizer
	grades <- data.frame(
		code = 1:8,
		name = c("urea", "SSP", "DAP", "KCl", "10-26-26", "28-28-0", "CAN", "Raja Ammonia"),
		N = c(46, 0, 18, 0, 10, 28, 26, 20),
		P = c(0, 16, 46, 0, 26, 28, 0, 0),
		K = c(0, 0, 0, 58, 26, 0, 0, 0)
	)
	key <- c("Year", "Village", "Type", "Farmer", "Parcel", "Plot", "Subplot")
	r3$rec <- 1:nrow(r3)
	tcol <- c(37, 40, 43, 47, 50, 53, 57, 60, 63, 67, 70, 73, 77, 80, 83)
	fl <- do.call(rbind, lapply(tcol, \(i) data.frame(rec=r3$rec, code=r3[[i]], qty=r3[[i+1]])))
	fl <- fl[!is.na(fl$code) & !is.na(fl$qty) & fl$qty > 0, ]
	fl <- merge(fl, grades, by="code", all.x=TRUE)
	fl$N <- fl$qty * fl$N / 100
	fl$P <- fl$qty * fl$P / 100 / 2.29
	fl$K <- fl$qty * fl$K / 100 / 1.2051
	fl$name[is.na(fl$name)] <- "unknown"
	fn <- aggregate(fl[, c("N", "P", "K")], fl["rec"], \(x) sum(x))
	ft <- aggregate(fl["name"], fl["rec"], \(x) paste(unique(x), collapse="; "))
	fert <- data.frame(r3[, c(key, "rec")], ha = r3$Area, man = r3$Manure)
	fert <- merge(fert, merge(fn, ft, by="rec"), by="rec", all.x=TRUE)
	nofert <- !fert$rec %in% fl$rec
	fert[nofert, c("N", "P", "K")] <- 0
	fert$name[nofert] <- "none"
	fert$N_fertilizer <- fert$N / fert$ha
	fert$P_fertilizer <- fert$P / fert$ha
	fert$K_fertilizer <- fert$K / fert$ha
	fert$fertilizer_type <- fert$name
	fert$OM_amount <- ifelse(is.na(fert$man), 0, fert$man) / fert$ha
	fert$OM_type <- ifelse(fert$OM_amount > 0, "manure", "none")
	fert <- fert[, c(key, "N_fertilizer", "P_fertilizer", "K_fertilizer", "fertilizer_type", "OM_amount", "OM_type")]

	## dates
	wk <- function(w, y) ifelse(is.na(w) | w < 1 | w > 48, NA, paste0(y, "-", formatC((w - 1) %/% 4 + 1, width=2, flag="0")))
	hv <- data.frame(r4[, key], planting_date = wk(r4$CEdate, r4$Year), harvest_date = wk(r4$Hardate, r4$Year))
	fert <- merge(fert, hv, by=key, all=TRUE)
	fert <- fert[!duplicated(fert[, key]), ]
	names(fert)[1:7] <- c("year", "village", "type", "farmer", "parcel", "plot", "subplot")
	fert$season <- "kharif"
	fert$crop <- "rice"
	d <- merge(d, fert, by=c("year", "village", "type", "farmer", "parcel", "plot", "subplot", "season", "crop"), all.x=TRUE)

	d$hhid <- paste(d$village, d$type, d$farmer, sep="-")
	d$farm_size_class_ <- c("marginal", "small", "large")[d$type]
	d$location <- c("Taraboi", "Thailo")[d$village]
	d$adm2 <- c("Khurda", "Jagatsinghpur")[d$village]
	d$plot_id <- paste(d$parcel, d$plot, d$subplot, sep="-")
	d$trial_id <- paste(d$hhid, d$year, sep="-")
	d$village <- d$type <- d$farmer <- d$parcel <- d$plot <- d$subplot <- NULL
	d$planting_date[is.na(d$planting_date)] <- as.character(d$year[is.na(d$planting_date)])
	d$year <- NULL

	d$country <- "India"
	d$adm1 <- "Odisha"
	d$currency <- "INR"
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$yield_part <- "grain"
	d$yield_part[d$crop %in% c("mung bean", "black gram", "horse gram", "lentil", "mung bean; lentil")] <- "seed"
	d$yield_part[d$crop %in% c("potato", "taro")] <- "tubers"
	d$yield_part[d$crop %in% c("chili pepper", "watermelon", "pumpkin", "tomato", "cucumber", "bitter gourd")] <- "fruit"
	d$yield_part[d$crop %in% c("cabbage", "vegetables", "cabbage; cauliflower")] <- "leaves"
	d$yield_part[d$crop %in% c("cauliflower")] <- "flowers"
	d$yield_part[d$crop %in% c("jute", "sugarcane")] <- "stems"
	d$yield_part[d$crop == "groundnut"] <- "pod"
	d$yield_part[d$crop == "none"] <- "none"

	carobiner::write_files(path, meta, d)
}
