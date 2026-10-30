# R script for "carob"
# license: GPL (>=3)

## NOTES
# Varietal tracking (Phase II) under STRASA: adoption of Swarna-Sub1 (India: Odisha, Uttar Pradesh) and BRRI dhan51/52 (Bangladesh).
# Codes decoded with "Coding Manual - India and Bangladesh.xlsx".
# India: one record per household x parcel x plot ("Plot list" files), joined with "Parcel list" (tenure, land type, irrigation)
#   and "Household" (district, village). Production = q_prod x unit_prod (kg, gauni = 2.5 kg, bags of 40/75/100 kg).
#   Plot size converted to ha only for documented units: decimal (1/100 acre), acre, ha.
# Bangladesh: one record per household x parcel ("Parcel_list"): area in decimal (/247.1 for ha), tenure, irrigation, variety,
#   variety type, planting method, submergence. No production; "Rice_variety" (sales of BRRI dhan51/52) not processed.
# Interviews: Odisha April 2012, Uttar Pradesh May 2012, Bangladesh April 2013; the plot data refer to the preceding kharif/aman season.

## ISSUES
# The NEPAL_2011_* files (Crop, Plot_list, Variety_list) are identical to the NEPAL_2014_* files of doi:10.7910/DVN/0VPRGD
#   (they refer to the 2013 kharif season); they are processed in that script and not here.
# No coordinates. Georeference with adm1/adm2/location. Uttar Pradesh village codes (1-6 within district) have no names; location NA.
# Odisha plot unit 16 "Guntha" (510 plots) has no conversion in the coding manual ("1 Guntha= Decimal"); unit 90 "other";
#   Uttar Pradesh unit 13 (203 plots) and 253 are not in the India coding manual (in the Nepal manual 13 = Bisswa, 253 = Bigha).
#   For these field_size and yield are NA; the reported size and unit code are kept in plot_size_local_ and plot_unit_code_.
# Season year not given in the plot files; planting_date not set.
# Bangladesh Parcel_list "quantity" (0-120) and "irrigated_2012" are not documented; not processed.
# Rice sales (quantity, price), seed transfers, animals, assets, individual lists and household attitude/perception scores not processed.
# India price_sold units (per kg or per sold unit) unclear; not processed.
# Second crop in intercropped India plots not processed (crop_id_2nd mostly "other").
# variety codes "Don't know" -> NA; "Others" -> unlisted variety name when given.
# Remaining write_files messages: missing latitude/longitude; new variables (trailing _) reported as unknown;
#   yield out of bounds / high crop yield (Uttar Pradesh rice up to 679525 kg/ha, sugarcane 535383 kg/ha: production or plot size probably
#   recorded in other units than coded); NA yield (Bangladesh, undocumented units); NA yield_part for crop "other" (crop NA).

## NEW VARIABLES
# survey_month_: month and year of the household interview
# plot_size_local_: plot size in the reported (local) unit
# plot_unit_code_: code of the plot size unit in the coding manual
# submerged_: crop was submerged during the season
# submergence_damage_: % crop damage due to submergence
# drought_: crop suffered from drought
# drought_damage_: % crop damage due to drought
# irrigated_percent_: % of the parcel area irrigated
# intercropped_: plot was intercropped (second crop not processed)


carob_script <- function(path) {

"
Varietal tracking and impact assessment (Phase II) under the Stress-tolerant Rice in Africa and South Asia

(1)To investigate the farmer-to-farmer seed distribution, we conducted a neighbor survey of farmers who have adopted Swarna-Sub1 rice seeds in Eastern India (Odisha and Uttar Pradesh (2) To analyze the factors that determine the adoption of Swarna-Sub1 (3) To understand the perception of farmers regarding the characteristics of Swarna-Sub1
"

	uri <- "doi:10.7910/DVN/VVGCH7"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "IRRI",
		publication = NA,
		project = "STRASA",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 30,
		carob_effort = 0.6
	)

	rd <- function(name) carobiner::read.excel(ff[basename(ff) == name], guess_max=100000)

	tenure <- c("own", "rented-in fixed rate", "rented-in sharecropping", "rented-out fixed rate", "rented-out sharecropping", "public land", "other")
	irsrc <- c("0"="none", "1"="electric shallow tube well", "2"="electric deep tube well", "3"="diesel shallow tube well",
		"4"="diesel deep tube well", "5"="canal", "90"="other")
	vtype <- c("traditional", "hybrid", "high-yielding", "submergence tolerant", "drought tolerant", "salinity tolerant", "other", NA)
	pmeth <- c("1"="transplanting", "2"="direct seeding")

	## India
	india <- function(state, fplot, fparcel, fhh, districts, villages) {
		r <- rd(fplot)
		p <- rd(fparcel)
		h <- rd(fhh)
		i <- match(paste(r$HHID, r$Parcel_ID), paste(p$HHID, p$Parcel_ID))
		j <- match(r$HHID, h$HHID)
		ha <- c("4"=0.01/2.471, "405"=1/2.471, "1000"=1)[as.character(r$plot_unit)]
		kg <- c("1"=1, "2"=2.5, "40"=40, "75"=75, "80"=80, "100"=100)[as.character(r$unit_prod)]
		variety <- c("1"="Swarna", "2"="Swarna-Sub1", "3"="CSR36", "4"="Sarju-52", "5"="Sambh Mahsori", "6"="Pant-4",
			"7"="Pant-11", "8"="NDR-359", "9"="NDR-97", "10"="Saket-4", "11"="Golden Mahsori", "12"="Sarbati", "13"="Kalanamak",
			"14"="Kastori", "15"="Samba-Sub1", "16"="IR64-Sub1", "17"="Sahbhagi Dhan", "18"="Sosku Sumarat")[as.character(r$rice_variety)]
		variety[r$rice_variety %in% 90] <- r$unlisted_variety_name[r$rice_variety %in% 90]
		data.frame(
			country = "India",
			adm1 = state,
			adm2 = districts[h$district[j]],
			location = villages[h$village[j]],
			hhid = paste(state, r$HHID, sep="-"),
			field_id = paste(r$Parcel_ID, r$Plot_ID, sep="-"),
			survey_month_ = substr(h$starting_time[j], 4, 10),
			season = "kharif",
			crop = c("1"="rice", "2"="maize", "3"="sugarcane", "4"="pigeon pea", "5"="fodder")[as.character(r$crop_id)],
			intercropped_ = r$intercropped == "Y",
			variety = variety,
			variety_type = vtype[r$variety_type],
			planting_method = pmeth[as.character(r$planting_method)],
			field_size = r$plot_size * ha,
			plot_size_local_ = r$plot_size,
			plot_unit_code_ = r$plot_unit,
			yield = r$q_prod * kg / (r$plot_size * ha),
			submerged_ = c(TRUE, FALSE)[r$yes_submerged],
			submergence_damage_ = r$damage_submergence,
			drought_ = c(TRUE, FALSE)[r$yes_drought],
			drought_damage_ = r$damage_drought,
			land_tenure = tenure[p$tenure[i]],
			landscape_position = c("lowland", "medium land", "upland")[p$land_type[i]],
			irrigation_source = irsrc[as.character(p$irrigation_source[i])],
			irrigated_percent_ = p$irrigation_percent[i]
		)
	}
	d1 <- india("Odisha", "INDIA_2011_Plot list_Odisha.xls", "INDIA_2011_Parcel list_Odisha.xls", "INDIA_2011_Household_Odisha.xls",
		c("Puri", "Khurda"),
		c("Dekheta", "Bhuan", "Bada Machhapur", "Ghodasalia", "Seulakhala", "Hansapada", "Lambilo", "Janapur 2", "Asanpur",
		"Pampalo", "Jhinti", "Danapada", "Bisuniapada", "Panikata", "Guapur", "Buddhipada"))
	d2 <- india("Uttar Pradesh", "INDIA_2011_Plot_list_UP.xls", "INDIA_2011_Parcel_List_UP.xls", "INDIA_2011_Household_UP.xls",
		c("Gorakhpur", "Maharajganj", "Siddharthnagar", "Basti", "Sant Kabir Nagar", "Mau"), rep(NA, 6))

	## Bangladesh
	r <- rd("BANGLADESH_2011_Parcel_list_BD.xls")
	h <- rd("BANGLADESH_2011_Household_BD.xls")
	j <- match(r$household, h$household)
	h$upazila <- cbind(h$upazila_kurigram, h$upazila_rangpur, h$upazila_lalmonirhat)[cbind(1:nrow(h), h$district)]
	h$village <- cbind(h$village_kurigram, h$village_rangpur, h$village_lalmonirhat)[cbind(1:nrow(h), h$district)]
	upz <- list(c("Bhurungamari", "Nageswari", "Rajarhat", "Rowmari", "Kurigram Sadar", "Ulipur"),
		c("Badarganj", "Gangachara", "Kaunia", "Pirgachha", "Rangpur Sadar"),
		c("Aditmari", "Lalmonirhat Sadar"))
	vil <- list(c("Agri Polashbari", "Baushmari", "Belgacha", "Char Kawniarchar", "Gatiasham", "Gobordhondola", "Hudordoba",
		"Kamarpara", "Khaliatari", "Madhupur", "Monnearper", "Mulbanga", "Satropur Purbopara", "Shobandhoho", "South Madhupur",
		"Soverkuti", "Tapurchar", "Utter Ghony Shampur", "West Debotter", "West Keder"),
		c("Baroawlia", "Dushmara", "Gopalpur", "Prannatchhar", "Sotorupai", "Sadar, Gangchara", "Sutrapur", "Sutrapur", "Shebdeb"),
		c("Kanteshorpara", "Ramdas"))
	variety <- c("1"="BRRI dhan51", "2"="BRRI dhan52", "3"="BR11", "4"="BR23", "5"="Paijam", "6"="Swarna")[as.character(r$variety)]
	d3 <- data.frame(
		country = "Bangladesh",
		adm2 = c("Kurigram", "Rangpur", "Lalmonirhat")[h$district[j]],
		adm3 = sapply(j, \(k) upz[[h$district[k]]][h$upazila[k]]),
		location = sapply(j, \(k) vil[[h$district[k]]][h$village[k]]),
		hhid = paste("Bangladesh", r$household, sep="-"),
		field_id = as.character(r$plot_list),
		survey_month_ = substr(h$time_start[j], 4, 10),
		season = "aman",
		crop = "rice",
		variety = variety,
		variety_type = c("1"="traditional", "2"="hybrid", "3"="high-yielding", "4"="submergence tolerant",
			"5"="drought tolerant", "6"="salinity tolerant", "90"="other")[as.character(r$type_variety)],
		planting_method = pmeth[as.character(r$planting_method)],
		field_size = r$plot_are_dec / 247.1,
		yield = as.numeric(NA),
		submerged_ = c(TRUE, FALSE)[r$yes_submerged],
		submergence_damage_ = r$damaged_submergence,
		land_tenure = c(tenure[1:3], "rented-out fixed rate", "rented-out sharecropping", "public land", "mortgaged in")[r$tenure],
		irrigation_source = irsrc[as.character(r$irrigation_source)]
	)

	d <- carobiner::bindr(d1, d2, d3)
	d$field_size[d$field_size %in% 0] <- NA
	d$yield[is.na(d$field_size)] <- NA
	d$irrigated <- ifelse(is.na(d$irrigation_source), NA, d$irrigation_source != "none")
	d$irrigation_source[d$irrigation_source %in% "none"] <- NA
	d$yield_part <- unname(c(rice="grain", maize="grain", "pigeon pea"="grain", sugarcane="stems")[d$crop])
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}
