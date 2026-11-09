# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. The control is "conventional practices in the study zone", but these practices (e.g. mineral
#    fertilizer, fungicide schedule) are not described. OA_f3 = 0 for the control, so OM_amount is 0;
#    N, P and K fertilizer are NA for the control. For the 12 organic treatments, no mineral fertilizer
#    is assumed (organic agriculture trial), so N, P and K fertilizer are 0.
# 2. Soil "K" is reported in meq/100g, but the values (68-190) are not plausible in that unit (maybe mg/kg).
#    It is kept as soil_K_meq_ (as reported).
# 3. Total N by depth is reported in mg/g, but the values (0.01-0.08; one "<0,01") are much too low for
#    soils with 6-10% organic matter. Not processed.
# 4. Base saturation (Bsat) is reported in meq/100g, but the values equal 100 * Sbas / CEC, so it is a
#    percentage. Stored as soil_BS (%).
# 5. Plants harvested (nPha) should be at most 16 (inner plants), but plot 20 has 19.
# 6. The data dictionary gives FTY = (MTW + NoMTW) / 4.8 m2, but the FTY values equal
#    (MTW + NoMTW + WDT) / 4.8 m2, i.e. they include damaged tubers. FTY is used as reported.

## NOTES
# 1. One variety (INIA 325 Poderosa). Factors: intercropping with faba bean (cv. Senorita) or not;
#    the "Juego de Ruedas" decision support tool for fungicide application (disc / no disc), included
#    in "treatment" (e.g. IN_DI_PM1); and poultry manure (1, 2, 4 t/ha). Split-split plot, 4 blocks.
# 2. Intercropped plots are split into a potato and a faba bean record (same plot_id).
#    Faba bean yield (FFBY, kg/ha, fresh weight) is used as reported; the plant part is not described.
# 3. Potato yield is total fresh tuber yield (FTY, t/ha), including damaged tubers, from the 16 inner
#    plants (2.4 x 2 m = 4.8 m2) of each 12 m2 plot. Marketable yield: tubers > 3 cm (MTY).
# 4. yield_moisture is 100 - tuber dry matter content (DMCt, %), measured per plot.
# 5. Soil samples per plot. Organic and total carbon were measured at 0-24 and 24-36 cm depth
#    (in the "long" table); the other soil variables have no depth information.
# 6. Coordinates and elevation of La Soledad are not in the data; they were provided by the
#    contributor (geo_from_source = FALSE).
# 7. The trial was in farmers' fields of the producers' association (confirmed by the contributor),
#    so on_farm = TRUE.
# 8. Not stated whether irrigated (rainy season, October-April), so "irrigated" is not included.
# 9. Not processed: total and marketable equivalent yield (TEY, MEY; computed with prices),
#    production cost, gross return, benefit-cost ratio, carbon footprint (Cool Farm Tool estimate),
#    tuber uniformity, textural class codes other than the three listed, OM2 by depth (= 1.72 * OC).

carob_script <- function(path) {

"
Dataset for: Organic agriculture trial in the locality of La Soledad, Chugay district, La Libertad - Peru (Second season 2023-2024)

An experimental potato trial based on organic agriculture in the second season (October 25th, 2023 to April 16th, 2024) was conducted in a total area of 987 m2 with the support of the 'Asociación de Productores Agropecuarios y Semilleristas La Poderosa' of the locality of La Soledad, Chugay district, province of Sánchez Carrión, La Libertad. This trial, the improved potato variety INIA-325 Poderosa was used with a split-plit plot design considering three factors: crop intercropping (intercropping with faba bean, variety Señorita and no intercropping), use of a decision support system for fungicide application or 'Juego de Ruedas' (Disc and no disc) and the application of organic amendments (1, 2 and 4 t ha-1 of poultry manure). The combination of these factors generated a total of 12 treatments; and an additional control treatment (conventional practices in the study zone). Four replicates were established, with a total of 52 plots of 3x4 m2 each (distance between plant and furrow of 0.3 m and 1 m, respectively). The variables evaluated were: fresh tuber yield (t ha-1), dry matter content of tubers (%), cost-benefit ratio, carbon footprint (kg CO2e Total) and soil analysis.
"

	uri <- "doi:10.21223/TIOWXR"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "split-split plot",
		data_type = "on-farm experiment",
		treatment_vars = "intercrops;OM_amount;treatment",
		response_vars = "yield;yield_marketable",
		notes = NA,
		carob_contributor = "Vilma Hualla",
		carob_LLM = "Claude Opus 5.5",
		carob_date = "2026-11-05",
		carob_completion = 85,
		carob_effort = 2
	)

	f1 <- ff[basename(ff) == "1_Harvest LS2.xlsx"]
	f2 <- ff[basename(ff) == "2_Yield-BCR-CF LS2.xlsx"]
	f3 <- ff[basename(ff) == "3_Dry matter of tuber LS2.xlsx"]
	f4 <- ff[basename(ff) == "4_Soil analysis LS2.xlsx"]
	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4, na=c("", "<0,01"))
	# "Block " has a trailing space in the column names
	names(r1) <- trimws(names(r1))
	names(r2) <- trimws(names(r2))
	names(r4) <- trimws(names(r4))
	# all files have the same 52 plots in the same order
	stopifnot(all(r1$ID == r2$ID), all(r1$ID == r3$ID), all(r1$ID == r4$ID))

	om <- c("0"=0, "1 t ha-1"=1000, "2 t ha-1"=2000, "4 t ha-1"=4000)[as.character(r2$OA_f3)]
	om <- unname(om)
	inter <- r2$CI_f1 == "Intercropping"
	ctrl <- r2$Code == "Control"
	harea <- 2.4 * 2  # harvested area (m2): 16 inner plants

	d <- data.frame(
		trial_id = "LaSoledad_2023-2024",
		plot_id = as.character(r2[["Sub-Sub-Plot"]]),
		rep = as.integer(r2[["Block"]]),
		treatment = r2$Code,
		crop = "potato",
		variety = "INIA 325 Poderosa",
		intercrops = ifelse(inter, "faba bean", "none"),
		country = "Peru",
		adm1 = "La Libertad",
		adm2 = "Sánchez Carrión",
		adm3 = "Chugay",
		location = "La Soledad",
		on_farm = TRUE,
		is_survey = FALSE,
		planting_date = "2023-10-25",
		harvest_date = "2024-04-16",
		plot_area = 12,
		row_spacing = 100,
		plant_spacing = 30,
		seed_density = 10000 / (1 * 0.3),
		plant_density = r1$nPha * 10000 / harea,
		tuber_density = (r1$nMTW + r1$nNoMTW + r1$nWDT) * 10000 / harea,
		OM_used = om > 0,
		OM_type = ifelse(om > 0, "poultry manure", "none"),
		OM_amount = om,
		N_fertilizer = ifelse(ctrl, NA, 0),
		P_fertilizer = ifelse(ctrl, NA, 0),
		K_fertilizer = ifelse(ctrl, NA, 0),
		yield_part = "tubers",
		yield = r2$FTY * 1000,
		yield_marketable = r2$MTY * 1000,
		yield_isfresh = TRUE,
		yield_moisture = 100 - r3$DMCt
	)

	## faba bean records for the intercropped plots (NOTES 2)
	fb <- d[inter, ]
	fb$crop <- "faba bean"
	fb$variety <- "Señorita"
	fb$intercrops <- "potato"
	fb$yield_part <- "seed"
	fb$yield <- r2$FFBY[inter]
	fb[, c("yield_marketable", "yield_moisture", "plant_density", "tuber_density", "seed_density",
		"plant_spacing", "row_spacing")] <- NA
	d <- rbind(d, fb)

	## soil, per plot
	texture <- c("Fr."="loam", "Fr.L."="silty loam", "Fr.A."="sandy loam")
	s <- data.frame(
		plot_id = as.character(r4[["Sub-Sub-Plot"]]),
		soil_pH = r4$pH,
		soil_EC = r4$EC,  # dS/m = mS/cm
		soil_CaCO3 = r4$CaCO3 * 10,  # % to g/kg
		soil_SOM = r4$OM1,
		soil_P = r4$P,  # ug/g = mg/kg
		soil_K_meq_ = r4$K,  # see ISSUES 2
		soil_sand = r4$Sand,
		soil_silt = r4$Silt,
		soil_clay = r4$Clay,
		soil_texture = unname(texture[r4$TC]),
		soil_CEC = r4$CEC,  # meq/100g = cmol(+)/kg
		soil_Ca_exch = r4[["Ca2+"]],
		soil_Mg_exch = r4[["Mg2+"]],
		soil_K_exch = r4[["K+"]],
		soil_Na_exch = r4[["Na+"]],
		soil_ex_acidity = r4[["Al+3 + H+3"]],
		soil_BS = r4$Bsat,  # see ISSUES 4
		soil_bd = r4$BD
	)
	d <- merge(d, s, by="plot_id", all.x=TRUE)

	## soil carbon by depth (NOTES 5); g/kg to %
	long <- data.frame(
		trial_id = "LaSoledad_2023-2024",
		plot_id = rep(s$plot_id, 2),
		depth_top = rep(c(0, 24), each=nrow(r4)),
		depth_bottom = rep(c(24, 36), each=nrow(r4)),
		soil_C_total = c(r4[["Clevel A"]], r4[["Clevel B"]]) / 10,
		soil_SOC = c(r4[["OClevel A"]], r4[["OClevel B"]]) / 10
	)

	## La Soledad, Chugay: coordinates and elevation are not in the data (NOTES 6);
	## provided by the contributor (8 01' 31" S, 77 28' 33" W; ~3027 m)
	d$longitude <- -77.47575
	d$latitude <- -8.025389
	d$elevation <- 3027
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d, long=long)
}
