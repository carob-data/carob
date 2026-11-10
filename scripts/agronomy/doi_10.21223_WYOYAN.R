# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Soil "K" is labelled meq/100g, but the values (142-560) exceed the CEC (15-22 meq/100g), and
#    exchangeable K is reported separately ("K+", meq/100g). "K" is probably extractable K in mg/kg and
#    is stored as soil_K (mg/kg). The data contact (D. Ramirez) was asked; no confirmation received.
# 2. Total N by depth is reported in mg/g; the values (0.03-0.07, i.e. 30-70 mg/kg) are
#    much too low for soils with 6-9% organic matter. With soil organic C of 3.7-4.7% and a C:N ratio
#    of 10-15, total N would be about 2.5-4.5 mg/g; the reported values give a C:N ratio of about 900.
#    Stored as soil_N_total (mg/kg) in the long table as reported; it can be excluded later. Two values
#    "<0,01" (below the detection limit) are set to NA. The data contact (D. Ramirez) was asked; no
#    confirmation received.
# 3. Plastic and straw: 6 of the 12 treatments have plastic covering the soil ("Plastic") in addition to
#    the oat/barley straw mulch (10, 20 or 30 cm). mulch_type is "plastic;straw" for these plots,
#    but terminag does not allow multiple values for mulch_type. How should this be coded?
# 4. Soil temperature sensors: the logger recorded soil temperature with 30 cm mulch or without mulch,
#    but the plots with the sensors are not identified. The values are in the long table linked by
#    trial_id (not plot_id; hence the plot_id warning), with sensor_mulch_depth_ (30 or 0 cm), a new
#    variable because mulch_depth is already in the wide table.

## NOTES
# 1. One variety (INIA 325 Poderosa). Factors: tillage (minimum or zero), plastic (yes or no)
#    and oat/barley mulch thickness (10, 20, 30 cm), plus a control. Split-split plot, 4 blocks.
#    Plots of 3 x 4 m; 0.3 m between plants and 1 m between rows. Sister trial of doi:10.21223/TIOWXR
#    (same team, Chugay).
# 2. The description says "conventional and zero tillage", but the data and the data dictionary have
#    minimum tillage and zero tillage for the 12 treatments, and conventional tillage only for the
#    control. The data are used.
# 3. The control is "conventional practices in the area" (conventional tillage, no plastic, no mulch);
#    other practices are not described. Fertilizer and irrigation are not reported for any treatment.
# 4. Planting date from the description (23 October 2023); harvest at 175 days after planting
#    (15 April 2024), consistent with the description.
# 5. yield is total fresh tuber yield (FTY, t/ha), from the 16 inner plants (2.4 x 2 m = 4.8 m2) of
#    each plot. The data dictionary gives FTY = (MTW + NoMTW) / 4.8 m2, but the FTY values equal
#    (MTW + NoMTW + WDT) / 4.8 m2, i.e. they include damaged tubers. yield_marketable (MTY) excludes
#    small tubers.
# 6. yield_moisture is 100 - tuber dry matter content (DMCt, %), measured per plot.
# 7. On-farm trial with the producers' association "Mi Licame Unido".
# 8. Coordinates (-7.76, -77.85) and elevation (3586.8 m) of the trial field in Licame are not in the
#    data; they were provided by the contributor (geo_from_source = FALSE).
# 9. Base saturation (Bsat) is labelled meq/100g, but the values equal 100 * Sbas / CEC, so it is
#    a percentage. Stored as soil_BS (%).
# 10. Soil samples per plot at 145 days after planting. Organic and total carbon and total N at
#    0-24 and 24-36 cm (long table); the other soil variables have no depth information.
# 11. Gravimetric soil water content (%, at 30 cm depth) per plot at 36, 57, 68, 81 and 121 days after
#    planting, in the long table.
# 12. Tea bag decomposition rate (TBI_k), average of 3 bags per plot, 137 days after planting. The
#    dictionary says the bags were extracted on "April 26, 2023", which is before the trial.
# 13. A logger in the trial recorded air temperature, relative humidity and precipitation every
#    15 minutes from 26 Oct 2023 to 11 Apr 2024 (weather table), and soil temperature at 0, 10 and
#    25 cm depth (long table, ISSUES 4). On 2024-03-21 11:45 two soil temperature readings are not
#    plausible (-7.3 deg C at 25 cm and 271.2 deg C at 10 cm, both without mulch); kept as reported.
#    Air temperature and soil temperature at 10 cm under mulch have missing values ("NA").
# 14. Not processed: benefit-cost ratio, carbon footprint (Cool Farm Tool estimates), tuber uniformity,
#    OM2 by depth (= 1.72 * OC). Tuber counts by category are used only for tuber_density.

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
		treatment_vars = "land_prep_method;mulch_type;mulch_depth",
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
		# ISSUES 3
		mulch_type = ifelse(r2$PB_f2 == "Plastic", "plastic;straw", ifelse(mulch > 0, "straw", "none")),
		mulch_depth = unname(mulch),
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
		TBI_k = r4$DR
	)

	## soil, per plot (NOTES 10)
	texture <- c("Fr."="loam", "Fr.A."="sandy loam")
	s <- data.frame(
		plot_id = as.character(r7[["Sub-Sub-Plot"]]),
		soil_pH = r7$pH,
		soil_EC = r7$EC,  # dS/m = mS/cm
		soil_CaCO3 = r7$CaCO3 * 10,  # % to g/kg
		soil_SOM = r7$OM1,
		soil_P = r7$P,  # ug/g = mg/kg
		soil_K = r7$K,  # labelled meq/100g, probably mg/kg (ISSUES 1)
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
		soil_BS = r7$Bsat,  # NOTES 9
		soil_bd = r7$BD
	)
	d <- merge(d, s, by="plot_id", all.x=TRUE)

	## Licame, Chugay (NOTES 8)
	d$longitude <- -77.85
	d$latitude <- -7.76
	d$elevation <- 3586.8
	d$geo_from_source <- FALSE

	## long table 1: soil carbon (g/kg to %) and total N (mg/g to mg/kg) by depth (NOTES 10, ISSUES 2)
	n <- nrow(r7)
	lc <- data.frame(
		trial_id = trial,
		plot_id = rep(s$plot_id, 2),
		date = as.character(pdate + r7$dap),
		depth_top = rep(c(0, 24), each=n),
		depth_bottom = rep(c(24, 36), each=n),
		soil_C_total = c(r7[["Clevel A"]], r7[["Clevel B"]]) / 10,
		soil_SOC = c(r7[["OClevel A"]], r7[["OClevel B"]]) / 10,
		soil_N_total = c(r7[["Nlevel A"]], r7[["Nlevel B"]]) * 1000
	)

	## long table 2: gravimetric soil water content (%) at 30 cm (NOTES 11)
	lw <- data.frame(
		trial_id = trial,
		plot_id = as.character(r5[["Sub-Sub-Plot"]]),
		date = as.character(pdate + r5$dap),
		depth = 30,
		soil_GWC = r5$Hg
	)

	## long table 3: soil temperature (deg C) by depth, with or without 30 cm mulch (NOTES 13, ISSUES 4)
	# Excel times are read as date-times on 1899-12-31
	tm <- format(r6$Time, "%H:%M")
	dt <- as.character(as.Date(r6$Date))
	sensor <- function(x, depth, mulch) {
		data.frame(trial_id = trial, date = dt, time = tm, depth = depth, sensor_mulch_depth_ = mulch, soil_temp = x)
	}
	lt <- carobiner::bindr(
		sensor(r6$ST_0_M30, 0, 30),
		sensor(r6$ST_10_M30, 10, 30),
		sensor(r6$ST_10_WM, 10, 0),
		sensor(r6$ST_25_M30, 25, 30),
		sensor(r6$ST_25_WM, 25, 0)
	)

	long <- carobiner::bindr(lc, lw, lt)

	## weather table (NOTES 13)
	wth <- data.frame(
		country = "Peru",
		location = "Licame",
		longitude = -77.85,
		latitude = -7.76,
		date = dt,
		time = tm,
		temp = r6$Ta,
		rhum = r6$RH,
		prec = r6$PP
	)

	carobiner::write_files(path, meta, d, long=long, wth=wth)
}
