# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. The description says "conventional and zero tillage", but the data and the data dictionary have
#    minimum tillage and zero tillage for the 12 treatments, and conventional tillage only for the
#    control. The data are used.
# 2. The control is "conventional practices in the area" (conventional tillage, no plastic, no mulch);
#    other practices are not described. Fertilizer use is not reported for any treatment, so
#    N, P and K fertilizer are not included.
# 3. Tea bag decomposition: the dictionary says the bags were extracted on "April 26, 2023", which is
#    before the trial (Oct 2023 - Apr 2024); the file gives 137 days after planting. Not verified.
# 4. Soil "K" is reported in meq/100g, but the values (142-560) exceed the CEC (15-22 meq/100g)
#    (maybe mg/kg). Kept as soil_K_meq_ (as reported).
# 5. Total N by depth is reported in mg/g, but the values (0.04-0.05; one "<0,01") are much too low
#    for soils with 6-9% organic matter. Not processed.
# 6. Base saturation (Bsat) is reported in meq/100g, but the values equal 100 * Sbas / CEC, so it is
#    a percentage. Stored as soil_BS (%).
# 7. The data dictionary gives FTY = (MTW + NoMTW) / 4.8 m2, but the FTY values equal
#    (MTW + NoMTW + WDT) / 4.8 m2, i.e. they include damaged tubers. FTY is used as reported.
# 8. Soil temperature: on 2024-03-21 11:45 two sensors give implausible values (-7.3 deg C at 25 cm
#    without mulch; 271.2 deg C at 10 cm without mulch). Kept as reported. Air temperature and soil
#    temperature at 10 cm under mulch have missing values ("NA").

## NEW VARIABLES
# plastic_barrier_: whether a plastic barrier was installed around the plot (TRUE/FALSE).
#   Its purpose is not described.
# mulch_thickness_: thickness (cm) of the oat/barley straw mulch (0, 10, 20 or 30 cm).
# decomposition_rate_: tea bag decomposition rate k (1/year), k = ln(initial / final weight) / t
#   (Tea Bag Index, https://doi.org/10.1111/2041-210X.12097), average of 3 bags per plot.
# soil_K_meq_: soil K as reported (see ISSUES 4).
# soil_temp_0cm_m30_, soil_temp_10cm_m30_, soil_temp_10cm_m0_, soil_temp_25cm_m30_, soil_temp_25cm_m0_:
#   soil temperature (deg C) at 0, 10 or 25 cm depth, under 30 cm mulch (m30) or without mulch (m0),
#   every 15 minutes (weather table). According to the contributor, soil sensors are usually placed in
#   the middle of a plot, as complementary data to detect frost or heat events; the plots with the
#   sensors are not identified.

## NOTES
# 1. One variety (INIA 325 Poderosa). Factors: tillage (minimum or zero), plastic barrier (yes or no)
#    and oat/barley mulch thickness (10, 20, 30 cm), plus a control. Split-split plot, 4 blocks.
#    Plots of 3 x 4 m; 0.3 m between plants and 1 m between rows.
# 2. Planting date from the description (23 October 2023); harvest at 175 days after planting
#    (15 April 2024), consistent with the description.
# 3. Potato yield is total fresh tuber yield (FTY, t/ha), including damaged tubers, from the 16 inner
#    plants (2.4 x 2 m = 4.8 m2) of each plot. Marketable yield (MTY) excludes small tubers.
# 4. yield_moisture is 100 - tuber dry matter content (DMCt, %), measured per plot.
# 5. On-farm trial with the producers' association "Mi Licame Unido".
# 6. Coordinates and elevation of the populated centre of Licame are not in the data; they were
#    provided by the contributor (elevation 2677 m from DePeru.com) (geo_from_source = FALSE).
# 7. Soil samples per plot at 145 days after planting. Organic and total carbon at 0-24 and 24-36 cm
#    (long table); the other soil variables have no depth information.
# 8. Gravimetric soil water content (%, at 30 cm depth) per plot at 36, 57, 68, 81 and 121 days after
#    planting, in the long table.
# 9. A logger in the trial recorded air temperature, relative humidity and precipitation every
#    15 minutes from 26 Oct 2023 to 11 Apr 2024, and soil temperature at 0, 10 and 25 cm depth with
#    30 cm mulch or without mulch (all in the weather table).
# 10. Not processed: benefit-cost ratio, carbon footprint (Cool Farm Tool estimates), tuber counts by
#    category are used only for tuber_density, tuber uniformity, OM2 by depth (= 1.72 * OC).

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
		data_type = "on-farm experiment",
		treatment_vars = "land_prep_method;plastic_barrier_;mulch_thickness_",
		response_vars = "yield;yield_marketable",
		notes = NA,
		carob_contributor = "Vilma Hualla",
		carob_LLM = "Claude Opus 5.5",
		carob_date = "2026-11-06",
		carob_completion = 85,
		carob_effort = 2
	)

	getf <- function(x) {
		r <- carobiner::read.excel(ff[basename(ff) == x], na=c("", "NA", "<0,01"))
		# "Block " has a trailing space
		names(r) <- trimws(names(r))
		r
	}
	r1 <- getf("1_Harvest LI2.xlsx")
	r2 <- getf("2_Yield-BCR-CF LI2.xlsx")
	r3 <- getf("3_Dry matter of tubers LI2.xlsx")
	r4 <- getf("4_Descomposition rate LI2.xlsx")
	r5 <- getf("5_Gravimetric soil moisture LI2.xlsx")
	r6 <- getf("6_Soil temperature LI2.xlsx")
	r7 <- getf("7_Soil analysis LI2.xlsx")
	# the plot tables have the same 52 plots in the same order
	stopifnot(all(r1$ID == r2$ID), all(r1$ID == r3$ID), all(r1$ID == r4$ID), all(r1$ID == r7$ID))

	pdate <- as.Date("2023-10-23")
	tillage <- c("Conventional tillage"="conventional", "Minimum tillage"="minimum tillage", "Zero tillage"="none")
	mulch <- c("0"=0, "10 cm"=10, "20 cm"=20, "30 cm"=30)[as.character(r2$M_f3)]
	harea <- 2.4 * 2  # harvested area (m2): 16 inner plants
	trial <- "Licame_2023-2024"

	d <- data.frame(
		trial_id = trial,
		plot_id = as.character(r2[["Sub-Sub-Plot"]]),
		rep = as.integer(r2$Block),
		treatment = r2$Code,
		crop = "potato",
		variety = "INIA 325 Poderosa",
		land_prep_method = unname(tillage[r2$TT_f1]),
		plastic_barrier_ = r2$PB_f2 == "Plastic",
		mulch_type = ifelse(mulch > 0, "oat/barley straw", "none"),
		mulch_thickness_ = unname(mulch),
		country = "Peru",
		adm1 = "La Libertad",
		adm2 = "Sánchez Carrión",
		adm3 = "Chugay",
		location = "Licame",
		on_farm = TRUE,
		is_survey = FALSE,
		planting_date = as.character(pdate),
		harvest_date = as.character(pdate + r1$dap),
		plot_area = 12,
		row_spacing = 100,
		plant_spacing = 30,
		seed_density = 10000 / (1 * 0.3),
		plant_density = r1$nPH * 10000 / harea,
		tuber_density = (r1$nMTW + r1$nNoMTW + r1$nWDT) * 10000 / harea,
		yield_part = "tubers",
		yield = r2$FTY * 1000,
		yield_marketable = r2$MTY * 1000,
		yield_isfresh = TRUE,
		yield_moisture = 100 - r3$DMCt,
		decomposition_rate_ = r4$DR
	)

	## soil, per plot (NOTES 7)
	texture <- c("Fr."="loam", "Fr.A."="sandy loam")
	s <- data.frame(
		plot_id = as.character(r7[["Sub-Sub-Plot"]]),
		soil_pH = r7$pH,
		soil_EC = r7$EC,  # dS/m = mS/cm
		soil_CaCO3 = r7$CaCO3 * 10,  # % to g/kg
		soil_SOM = r7$OM1,
		soil_P = r7$P,  # ug/g = mg/kg
		soil_K_meq_ = r7$K,  # see ISSUES 4
		soil_sand = r7$Sand,
		soil_silt = r7$Silt,
		soil_clay = r7$Clay,
		soil_texture = unname(texture[r7$TC]),
		soil_CEC = r7$CEC,  # meq/100g = cmol(+)/kg
		soil_Ca_exch = r7[["Ca2+"]],
		soil_Mg_exch = r7[["Mg2+"]],
		soil_K_exch = r7[["K+"]],
		soil_Na_exch = r7[["Na+"]],
		soil_ex_acidity = r7[["Al+3 + H+3"]],
		soil_BS = r7$Bsat,  # see ISSUES 6
		soil_bd = r7$BD
	)
	d <- merge(d, s, by="plot_id", all.x=TRUE)

	## Licame, Chugay (NOTES 6)
	d$longitude <- -77.834803
	d$latitude <- -7.766443
	d$elevation <- 2677
	d$geo_from_source <- FALSE

	## long table 1: soil carbon by depth (g/kg to %; NOTES 7)
	n <- nrow(r7)
	lc <- data.frame(
		plot_id = rep(s$plot_id, 2),
		date = as.character(pdate + r7$dap),
		depth_top = rep(c(0, 24), each=n),
		depth_bottom = rep(c(24, 36), each=n),
		soil_C_total = c(r7[["Clevel A"]], r7[["Clevel B"]]) / 10,
		soil_SOC = c(r7[["OClevel A"]], r7[["OClevel B"]]) / 10
	)

	## long table 2: gravimetric soil water content (%) at 30 cm (NOTES 8)
	lw <- data.frame(
		plot_id = as.character(r5[["Sub-Sub-Plot"]]),
		date = as.character(pdate + r5$dap),
		depth = 30,
		soil_GWC = r5$Hg
	)

	long <- carobiner::bindr(lc, lw)

	## weather table, with the soil temperature (deg C) recorded by the same logger (NOTES 9)
	# Excel times are read as date-times on 1899-12-31
	tm <- format(r6$Time, "%H:%M")
	dt <- as.character(as.Date(r6$Date))
	wth <- data.frame(
		country = "Peru",
		location = "Licame",
		longitude = -77.834803,
		latitude = -7.766443,
		date = dt,
		time = tm,
		temp = r6$Ta,
		rhum = r6$RH,
		prec = r6$PP,
		# soil temperature at 0, 10 or 25 cm depth, under 30 cm mulch (m30) or without mulch (m0)
		# ISSUES 8: two readings on 2024-03-21 11:45 are not plausible (-7.3 and 271.2)
		soil_temp_0cm_m30_ = r6$ST_0_M30,
		soil_temp_10cm_m30_ = r6$ST_10_M30,
		soil_temp_10cm_m0_ = r6$ST_10_WM,
		soil_temp_25cm_m30_ = r6$ST_25_M30,
		soil_temp_25cm_m0_ = r6$ST_25_WM
	)

	carobiner::write_files(path, meta, d, long=long, wth=wth)
}
