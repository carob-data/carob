# R script for "carob"
# license: GPL (>=3)

## NOTES
# Impact assessment survey of "Three Reductions - Three Gains" (3R3G) in An Giang, Can Tho and Soc Trang provinces, Mekong Delta, Vietnam
# (507 households). Sheets "DS" and "WS": one record per household x parcel x season. Sheet "CY" (crop year) is used for the household
# variables (age, education, farm size, household size).
# Seasons (code manual): DS = dry season 2006 for An Giang and Can Tho, 2007 for Soc Trang; WS = wet season 2006.
# Village and district from the respondent-number ranges in the code manual ("Impact Assessment of 3R3G_CM.XLS").
# Yield: the code manual says kg/ha, but the values (e.g. 6.2, 7.7) are in t/ha; multiplied by 1000.
# N, P, K are the authors' elemental kg/ha (KgN, KgP, KgK). fertilizer_amount is the gross fertilizer weight (FRTGrWt, kg/ha).
# Prices and costs in USD: crop_price and seed_price converted from USD/MT to USD/kg.

## ISSUES
# Fertilizer products per farm are not given (only prices of urea, DAP, Kali 30 and Kali 60); fertilizer_type not set.
# Planting and harvest dates are not given; planting_date is the season year from the code manual.
# Respondent 1199 is listed under Dao Huu Canh (Chau Phu); used as such.
# Not mapped: labor costs per operation, material costs, molluscicide use, snail picking, gross/net income, average variable cost,
#   3R3G awareness and information sources, adoption level details, per capita income.
# Yield moisture content is not given (unmilled rice).
# 5 records have fertilizer_amount > 1000 kg/ha (max 1920), flagged as out of bounds; kept as given.
# Fungicide frequency is missing for 16 records.
# No coordinates.

## NEW VARIABLES
# adopt_3R3G_: adopter of 3R3G (Adpt3R)
# adopt_seed_, adopt_fertilizer_, adopt_pesticide_: adopter of the seed, fertilizer and pesticide reductions
# education_years_: years of education of the respondent


carob_script <- function(path) {

"
Impact Assessment of Three Reductions – Three Gains in South Vietnam

Measure awareness and adoption of 3R3G in the Mekong Delta Region and assess differences in input use, productivity, farm costs and incomes between adopters and non-adopters.
"

	uri <- "doi:10.7910/DVN/26196"
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
		carob_completion = 55,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "Impact Assessment of Three R and Three G.xls"]

	proc <- function(s) {
		r <- carobiner::read.excel(f, sheet=s, skip=3)
		r <- r[!is.na(r$ResNo), ]
		names(r) <- gsub("-", "", names(r))
		data.frame(
			hhid = as.character(r$ResNo),
			field_id = as.character(r$ParcNo),
			season = ifelse(s == "DS", "dry", "wet"),
			province = r$Province,
			field_size = r$ParcArea,
			adopt_3R3G_ = r$Adpt3R == 1,
			adopt_seed_ = r$AdptS == 1,
			adopt_fertilizer_ = r$AdptF == 1,
			adopt_pesticide_ = r$AdptP == 1,
			yield = r$Yield * 1000,
			crop_price = r$Price / 1000,
			variety = r$Variety,
			seed_source = c("certified", "own harvest", "co-farmer", "local seed farm")[r$SdSrc],
			seed_rate = r$SdRate,
			seed_price = r$SdPrc / 1000,
			fertilizer_cost = r$FRTCst,
			fertilizer_amount = r$FRTGrWt,
			N_fertilizer = r$KgN,
			P_fertilizer = r$KgP,
			K_fertilizer = r$KgK,
			herbicide_amount = r$HRBAI,
			herbicide_times = as.integer(r$HRBFrq),
			insecticide_amount = r$INSAI,
			insecticide_times = as.integer(r$INSFrq),
			fungicide_amount = r$FNGAI,
			fungicide_times = as.integer(r$FNGFrq),
			irrigation_cost = r$H2OCst
		)
	}
	d <- rbind(proc("DS"), proc("WS"))
	d$herbicide_used <- d$herbicide_times > 0
	d$insecticide_used <- d$insecticide_times > 0
	d$fungicide_used <- d$fungicide_times > 0
	d$currency <- "USD"

	d$adm1 <- c(AG="An Giang", CT="Can Tho", ST="Soc Trang")[d$province] |> as.vector()
	d$province <- NULL
	d$planting_date <- ifelse(d$season == "dry" & d$adm1 == "Soc Trang", "2007", "2006")

	rng <- rbind(
		c(1001, 1029, "Vinh Binh", "Chau Thanh"), c(1030, 1058, "Vinh Nhuan", "Chau Thanh"), c(1059, 1081, "Vinh Hanh", "Chau Thanh"),
		c(1082, 1123, "O Long Vi", "Chau Phu"), c(1124, 1152, "Dao Huu Canh", "Chau Phu"), c(1199, 1199, "Dao Huu Canh", "Chau Phu"),
		c(1153, 1180, "Binh Phu", "Chau Phu"), c(1181, 1188, "Vinh Te", "Chau Doc"), c(1189, 1194, "Vinh Chau", "Chau Doc"),
		c(1195, 1198, "Chau Phu B", "Chau Doc"),
		c(2001, 2012, "Trung Nhut", "Thot Not"), c(2013, 2023, "Trung Thanh", "Thot Not"), c(2024, 2034, "Thuan Hung", "Thot Not"),
		c(2035, 2064, "Thoi Dong", "Co Do"), c(2065, 2090, "Truong Xuan A", "Co Do"), c(2091, 2113, "Dong Hiep", "Co Do"),
		c(2114, 2146, "Trung Hung", "Vinh Thanh"), c(2147, 2172, "Thanh An", "Vinh Thanh"), c(2173, 2198, "Thanh Thang", "Vinh Thanh"),
		c(3001, 3012, "Long Hung", "My Tu"), c(3057, 3079, "Long Hung", "My Tu"),
		c(3013, 3022, "Thuan Hung", "My Tu"), c(3080, 3104, "Thuan Hung", "My Tu"),
		c(3023, 3032, "My Phuoc", "My Tu"), c(3105, 3129, "My Phuoc", "My Tu"),
		c(3033, 3043, "Dai Hai", "Ke Sach"), c(3130, 3149, "Dai Hai", "Ke Sach"),
		c(3044, 3056, "Ke An", "Ke Sach"), c(3150, 3174, "Ke An", "Ke Sach"),
		c(3175, 3199, "Thanh Thai An", "My Xuyen")
	)
	id <- as.numeric(d$hhid)
	j <- sapply(id, function(i) {
		k <- which(i >= as.numeric(rng[,1]) & i <= as.numeric(rng[,2]))
		if (length(k) == 1) k else NA
	})
	d$location <- rng[j, 3]
	d$adm2 <- rng[j, 4]

	h <- carobiner::read.excel(f, sheet="CY", skip=3)
	h <- h[!is.na(h$ResNo), ]
	k <- match(d$hhid, as.character(h$ResNo))
	d$age <- h$Age[k]
	d$education_years_ <- h$Educ[k]
	d$farmland <- h$TFArea[k]
	d$hh_size <- h$HHSize[k]

	d$country <- "Vietnam"
	d$crop <- "rice"
	d$yield_part <- "grain"
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$trial_id <- paste(d$hhid, d$field_id, d$season, sep="_")

	carobiner::write_files(path, meta, d)
}
