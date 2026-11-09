# R script for "carob"
# license: GPL (>=3)

## NOTES
# Conservation-agriculture potato trial at Licame, Chugay, Sánchez Carrión, La Libertad, Peru. Variety INIA-325 Poderosa.
# The dictionary states FTY and MTY in t ha-1. Yield and yield_marketable are those columns times 1000. DMCt is tuber dry matter in percent. yield_moisture is 100 minus that value.
# CF_ha is the carbon footprint in kg CO2 eq ha-1 and is stored as emission_GHG. CaCO3 in percent is stored as soil_CaCO3 in g/kg. C, OC, and OM2 in g/kg are stored in percent. N in mg/g is stored as soil_N_total in mg/kg. EC in dS/m is soil_EC. Exchangeable cations in meq/100g are cmol(+)/kg.
# Fr. is franco (loam) and Fr.A. is franco arenoso (sandy loam).
# Moisture rows repeat each plot on five dates. The plot is Sub-Sub-Plot, which matches the yield ID. The moisture ID is a row number.
# The abstract gives plots of 3 x 4 m and spacing of 0.3 m between plants and 1 m between furrows. Spacing is stored in cm.
# The dataset geographic coverage is one west longitude and one south latitude.

## ISSUES
# The abstract gives the season as 23 October 2023 to 15 April 2024. Those dates are not labeled planting and harvest.
# No fertilizer rate and no irrigation statement.
# The data include minimum tillage as well as conventional and zero tillage, and a mulch level of 0. The abstract names conventional and zero tillage and mulch of 10, 20, and 30 cm.
# mulch in the vocabulary is kg/ha. The factor here is thickness in cm, so it is stored as mulch_thickness_.
# Soil pH, EC, texture, and exchangeable cations have no sampling depth. Carbon, organic carbon, organic matter, and nitrogen do: level A is 0-24 cm and level B is 24-36 cm.
# The dictionary unit for Bsat is meq/100g, but the description says percent and the values equal sum-of-bases / CEC. It is stored as soil_BS.
# Column K is 142-560 while K+ in the same dictionary unit (meq/100g) is about 0.3-1.1. K is not mapped.
# Gravimetric moisture is percent. soil_GWC has no unit in the vocabulary.
# Logger ID in the temperature file is a row number, not a plot. Those long rows have no plot_id, so the plot-id check reports a mismatch.

## NOT PROCESSED
# 1_Harvest LI2.xlsx: MTW, NoMTW, and WDT are kg per inner plot. FTY and MTY are the per-area yields. The harvested area is not stated, so the tuber and plant counts are not converted to a density.
# 7_Soil analysis LI2.xlsx: K (meq/100g in the dictionary). The values do not match K+.
# 7_Soil analysis LI2.xlsx: OM1, organic matter in percent with no depth. soil_SOM is the depth-resolved OM2.
# 7_Soil analysis LI2.xlsx: Scat and Sbas. They are the sums of the cations that are stored.
# 7_Soil analysis LI2.xlsx: Nlevel A or B written "<0,01". That is below the reported detection and is stored as missing.
# 5_Gravimetric soil moisture LI2.xlsx: blank Hg cells. They are missing soil_GWC.
# 8_Data_Dictionary LI2.xlsx: codebook.
# Design Schematic_AC.xlsx: plot layout, not observations.
# Licame S2.jpg: site photograph.

## NEW VARIABLES
# tillage_: tillage factor (conventional, minimum, zero), as recorded.
# plastic_: plastic barrier factor, as recorded.
# mulch_thickness_: oat/barley mulch thickness (cm).
# decomposition_rate_: tea-bag decomposition rate (DR). The dictionary gives no unit.
# tea_bag_remaining_: remaining tea-bag biomass (BRT). The dictionary gives no unit.
# tea_bag_initial_, tea_bag_final_: tea-bag weight (g) before burial and after drying. bag_ is the bag number 1-3.
# benefit_cost_ratio_: benefit-cost rate (BCR). The dictionary gives no unit.
# carbon_footprint_total_: CF, kg CO2 eq for the plot.
# carbon_footprint_per_ton_: CF_ton, kg CO2 eq per t of fresh tubers.
# plants_harvested_, marketable_tubers_, nonmarketable_tubers_, damaged_tubers_: counts on the harvested area.
# tuber_uniformity_: tuber uniformity class, as recorded.
# sensor_mulch_cm_: mulch thickness (cm) at the soil-temperature sensor. 0 is the sensor without mulch.


carob_script <- function(path) {

"
Dataset for: Conservation agriculture trial in the locality of Licame, Chugay district, La Libertad - Peru (Second season 2023-2024)

An experimental potato trial based on conservation agriculture in the second season (October 23rd, 2023 to April 15th, 2024) was conducted in a total area of 987 m2 with the support of the 'Asociación de Productores Agropecuarios Mi Licame Unido' of the locality of Licame, district of Chugay, province of Sánchez Carrión, La Libertad. For this trial, the improved potato variety INIA-325 Poderosa was used with a split-plit plot design considering three factors: tillage (conventional and zero tillage), plastic barriers (with and without barrier) and amount of oat/barley of mulching (10, 20 and 30 cm thickness). The combination of these factors generated a total of 12 treatments; and an additional control treatment (conventional practices in the area). Four replicates were established, with a total of 52 plots of 3x4 m2 each (distance between plant and furrow of 0.3 m and 1 m, respectively). The variables evaluated were: fresh tuber yield (t ha-1), cost-benefit ratio, carbon footprint (kg CO2e Total), descompositio rate, soil moisture (%), soil temperature (°C) and soil analysis at different times during the vegetative period of the crop.
"

	uri <- "doi:10.21223/WYOYAN"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "split-split plot",
		data_type = "experiment",
		treatment_vars = "tillage_;plastic_;mulch_thickness_",
		response_vars = "yield;yield_marketable",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 75,
		carob_effort = 1
	)

	f1 <- ff[basename(ff) == "2_Yield-BCR-CF LI2.xlsx"]
	f2 <- ff[basename(ff) == "3_Dry matter of tubers LI2.xlsx"]
	f3 <- ff[basename(ff) == "4_Descomposition rate LI2.xlsx"]
	f4 <- ff[basename(ff) == "5_Gravimetric soil moisture LI2.xlsx"]
	f5 <- ff[basename(ff) == "6_Soil temperature LI2.xlsx"]
	f6 <- ff[basename(ff) == "7_Soil analysis LI2.xlsx"]
	f7 <- ff[basename(ff) == "1_Harvest LI2.xlsx"]
	r1 <- carobiner::read.excel(f1, na=c("", "NA"))
	r2 <- carobiner::read.excel(f2, na=c("", "NA"))
	r3 <- carobiner::read.excel(f3, na=c("", "NA"))
	r4 <- carobiner::read.excel(f4, na=c("", "NA"))
	r5 <- carobiner::read.excel(f5, na=c("", "NA"))
	r6 <- carobiner::read.excel(f6, na=c("", "NA"))
	r7 <- carobiner::read.excel(f7, na=c("", "NA"))
	names(r1) <- trimws(names(r1))
	names(r2) <- trimws(names(r2))
	names(r3) <- trimws(names(r3))
	names(r4) <- trimws(names(r4))
	names(r6) <- trimws(names(r6))
	names(r7) <- trimws(names(r7))
	dmc <- r2$DMCt[match(r1$ID, r2$ID)]
	num <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "na", "<0,01", "<0.01")] <- NA_character_
		suppressWarnings(as.numeric(sub(",", ".", x, fixed=TRUE)))
	}
	as_date <- function(x) {
		if (inherits(x, "POSIXt")) return(as.character(as.Date(x, tz="UTC")))
		if (inherits(x, "Date")) return(as.character(x))
		if (is.numeric(x)) return(as.character(as.Date(x, origin="1899-12-30")))
		as.character(as.Date(x))
	}
	as_time <- function(x) {
		if (inherits(x, "POSIXt")) return(format(x, "%H:%M:%S", tz="UTC"))
		if (is.numeric(x)) {
			sec <- as.integer(round((x %% 1) * 86400))
			sprintf("%02d:%02d:%02d", sec %/% 3600, (sec %/% 60) %% 60, sec %% 60)
		} else {
			sub("^.* ", "", as.character(x))
		}
	}

	d <- data.frame(
		trial_id = "Licame_2023",
		country = "Peru",
		adm1 = "La Libertad",
		adm2 = "Sánchez Carrión",
		adm3 = "Chugay",
		location = "Licame",
		# dataset geographic coverage: one west longitude and one south latitude
		latitude = -7.76,
		longitude = -77.85,
		geo_from_source = TRUE,
		station_id = "Licame",
		crop = "potato",
		variety = "INIA-325 Poderosa",
		treatment = trimws(as.character(r1$Code)),
		tillage_ = trimws(as.character(r1$TT_f1)),
		plastic_ = trimws(as.character(r1$PB_f2)),
		mulch_thickness_ = trimws(as.character(r1$M_f3)),
		rep = as.integer(r1$Block),
		plot_id = as.character(r1$ID),
		plot_area = 3 * 4,
		plant_spacing = 0.3 * 100,
		row_spacing = 1 * 100,
		yield = as.numeric(r1$FTY) * 1000,
		yield_marketable = as.numeric(r1$MTY) * 1000,
		yield_part = "tubers",
		yield_moisture = 100 - as.numeric(dmc),
		on_farm = TRUE,
		is_survey = FALSE,
		benefit_cost_ratio_ = num(r1$BCR),
		emission_GHG = num(r1$CF_ha),
		carbon_footprint_total_ = num(r1$CF),
		carbon_footprint_per_ton_ = num(r1$CF_ton)
	)

	k <- match(d$plot_id, as.character(r3$ID))
	d$decomposition_rate_ <- num(r3$DR)[k]
	d$tea_bag_remaining_ <- num(r3$BRT)[k]
	h <- match(d$plot_id, as.character(r7$ID))
	d$plants_harvested_ <- num(r7$nPH)[h]
	d$marketable_tubers_ <- num(r7$nMTW)[h]
	d$nonmarketable_tubers_ <- num(r7$nNoMTW)[h]
	d$damaged_tubers_ <- num(r7$nWDT)[h]
	d$tuber_uniformity_ <- trimws(as.character(r7$Tub_Unif))[h]

	j <- match(d$plot_id, as.character(r6$ID))
	tex <- trimws(as.character(r6$TC))
	d$soil_pH <- num(r6$pH)[j]
	d$soil_EC <- num(r6$EC)[j]
	d$soil_CaCO3 <- num(r6$CaCO3)[j] * 10
	d$soil_P <- num(r6$P)[j]
	d$soil_sand <- num(r6$Sand)[j]
	d$soil_silt <- num(r6$Silt)[j]
	d$soil_clay <- num(r6$Clay)[j]
	d$soil_texture <- c("Fr."="loam", "Fr.A."="sandy loam")[tex][j]
	d$soil_CEC <- num(r6$CEC)[j]
	d$soil_Ca_exch <- num(r6[["Ca2+"]])[j]
	d$soil_Mg_exch <- num(r6[["Mg2+"]])[j]
	d$soil_K_exch <- num(r6[["K+"]])[j]
	d$soil_Na_exch <- num(r6[["Na+"]])[j]
	d$soil_ex_acidity <- num(r6[["Al+3 + H+3"]])[j]
	d$soil_BS <- num(r6$Bsat)[j]
	d$soil_bd <- num(r6$BD)[j]

	bag <- function(value, n, variable) {
		data.frame(
			plot_id = as.character(r3$ID),
			DAP = as.integer(r3$dap),
			bag_ = n,
			variable = variable,
			value = num(value)
		)
	}
	layer <- function(top, bot, variable, value) {
		data.frame(
			plot_id = as.character(r6$ID),
			depth_top = top,
			depth_bottom = bot,
			variable = variable,
			value = value
		)
	}
	sensor <- function(value, depth, mulch) {
		data.frame(
			trial_id = "Licame_2023",
			date = as_date(r5$Date),
			time = as_time(r5$Time),
			depth = depth,
			sensor_mulch_cm_ = mulch,
			variable = "soil_temp",
			value = num(value)
		)
	}
	long <- carobiner::bindr(
		data.frame(
			plot_id = as.character(r4[["Sub-Sub-Plot"]]),
			DAP = as.integer(r4$dap),
			variable = "soil_GWC",
			value = num(r4$Hg)
		),
		bag(r3$ITB_1, 1, "tea_bag_initial_"),
		bag(r3$ITB_2, 2, "tea_bag_initial_"),
		bag(r3$ITB_3, 3, "tea_bag_initial_"),
		bag(r3$FTB_1, 1, "tea_bag_final_"),
		bag(r3$FTB_2, 2, "tea_bag_final_"),
		bag(r3$FTB_3, 3, "tea_bag_final_"),
		layer(0, 24, "soil_C_total", num(r6[["Clevel A"]]) / 10),
		layer(24, 36, "soil_C_total", num(r6[["Clevel B"]]) / 10),
		layer(0, 24, "soil_SOC", num(r6[["OClevel A"]]) / 10),
		layer(24, 36, "soil_SOC", num(r6[["OClevel B"]]) / 10),
		layer(0, 24, "soil_SOM", num(r6[["OM2level A"]]) / 10),
		layer(24, 36, "soil_SOM", num(r6[["OM2level B"]]) / 10),
		layer(0, 24, "soil_N_total", num(r6[["Nlevel A"]]) * 1000),
		layer(24, 36, "soil_N_total", num(r6[["Nlevel B"]]) * 1000),
		sensor(r5$ST_25_M30, 25, 30),
		sensor(r5$ST_25_WM, 25, 0),
		sensor(r5$ST_0_M30, 0, 30),
		sensor(r5$ST_10_WM, 10, 0),
		sensor(r5$ST_10_M30, 10, 30)
	)

	wth <- data.frame(
		station_id = "Licame",
		date = as_date(r5$Date),
		time = as_time(r5$Time),
		temp = num(r5$Ta),
		rhum = num(r5$RH),
		prec = num(r5$PP)
	)

	carobiner::write_files(path, meta, d, long=long, wth=wth)
}
