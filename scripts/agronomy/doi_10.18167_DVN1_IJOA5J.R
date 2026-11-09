# R script for "carob"

## NOTES
# On-farm maize-cowpea trial on 14 farms in Goromonzi, homefield and outfield, 2017/18 and 2018/19.
# Namatsheve et al. 2021, Field Crops Research 263:108052 (doi:10.1016/j.fcr.2020.108052).
# Maize is SC627. Improved cowpea is CBC2. The landrace is not named.
# Yields are already at 12.5% moisture for maize and 9.7% for cowpea. 100-seed mass is converted to g per 1000 seeds.
# Planting and harvest dates are the end of the window given in the paper, not a single recorded day.
# N is 15 kg/ha as ammonium nitrate at sowing and at four and six weeks (45 kg/ha) on +N plots, and 0 on -N plots. All plots received 15 kg P/ha as SSP and 30 kg K/ha as muriate of potash.
# Soil was sampled once in October 2017 at 0-20 cm, one composite per farm and field type, and is repeated on both seasons. Total N in the file is on the same scale as SOC percent (C:N about 14) and is stored as mg/kg.
# Table 1 of the paper reports ICP total Ca of about 3.5-4.1 mg/kg and total Mg of about 2 mg/kg. Those are the Ca_ICP and Mg_ICP columns.

## ISSUES
# A few ICP calcium values are below 1 mg/kg, and fine sand reaches 53%, so soil_Ca, soil_Mg, and soil_sand_fine stay out of bounds. The paper reports the same low Ca and Mg totals.
# One maize yield is missing.
# All 14 farms share one district coordinate, so there are fewer coordinates than sites.
# Planting density is the population after thinning, not a counted stand.

## NOT PROCESSED
# Grain_minerals: Fe, Zn, Ca, Mg, Mn, Cu, P, K and phytate in maize and cowpea grain. The sheet has no unit.
# N2_fixation: %Ndfa, delta 15N, and fixed N. No plot-level cowpea biomass date.
# Nodules: nodule counts and weights, six rows per plot. No observation date.
# Soil_characteristics: Mineral_N_initital and Mineral_N_after_incubation. Two laboratory measurements with no date.
# Soil_characteristics: P_ICP_mg_kg. It is lower than resin-extractable P, and the paper does not list P among the ICP totals.
# Soil_characteristics: Previous_land_use, Soil_colour. Not used.
# Yield_LER: unadjusted seed masses and the plot grain weights already used for yield. LER is the sum of the partial ratios stored here.

## NEW VARIABLES
# field_position_: homefield or outfield.
# partial_ler_: partial land equivalent ratio of this crop. Unitless.


carob_script <- function(path) {

"This dataset comprises data from on farm trials conducted in sub-humid Zimbabwe during two cropping seasons (2017/18-2018/19). The study investigated the effect of maize-cowpea intercropping on productivity, biological N2-fixation and grain mineral content compared to sole crops. The effect of soil characteristics (homefield vs outfield) and of cowpea varieties landrace) was also studied (2020-08-17)"

	uri <- "doi:10.18167/DVN1/IJOA5J"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		publication = "doi:10.1016/j.fcr.2020.108052",
		data_organization = "UZIM;CIRAD;IRD",
		data_type = "on-farm experiment",
		project = NA,
		design = "split-plot",
		treatment_vars = "field_position_;variety;intercrops;N_fertilizer",
		response_vars = "yield",
		carob_contributor = "Effie Ochieng'",
		carob_date = "2023-10-23",
		carob_LLM = "Cursor Grok 4.7",
		carob_effort = NA,
		notes = NA
	)

	f <- ff[basename(ff) == "Namatsheve_et_al_Dataset.xlsx"]
	r1 <- carobiner::read.excel(f, sheet = "Yield_LER", na = c("", "NA"))
	r2 <- carobiner::read.excel(f, sheet = "Soil_characteristics", na = c("", "NA"))

	num <- function(x) as.numeric(as.character(x))

	# Methods: maize SC627, cowpea CBC2 or a local landrace.
	variety <- rep("SC627", nrow(r1))
	variety_type <- rep(NA_character_, nrow(r1))
	cowpea <- !is.na(r1$Cowpea_variety)
	variety[cowpea & r1$Cowpea_variety == "Improved"] <- "CBC2"
	variety[cowpea & r1$Cowpea_variety == "Landrace"] <- "landrace"
	variety_type[cowpea & r1$Cowpea_variety == "Improved"] <- "improved"
	variety_type[cowpea & r1$Cowpea_variety == "Landrace"] <- "landrace"

	inter <- r1$System == "Intercrop"
	plus_n <- r1$Fertilizer == "+N"
	base <- data.frame(
		site = as.character(r1$Site),
		field_position_ = tolower(trimws(r1$Field)),
		treatment = trimws(as.character(r1$Treatment_desc)),
		season = ifelse(r1$Year == 1, "first", "second"),
		plot_area = num(r1$Net_plot_m2),
		N_fertilizer = ifelse(plus_n, 45, 0),
		N_splits = ifelse(plus_n, 3L, NA_integer_),
		fertilizer_type = ifelse(plus_n, "AN; SSP; KCl", "SSP; KCl"),
		P_fertilizer = 15,
		K_fertilizer = 30,
		variety = variety,
		variety_type = variety_type,
		partial_ler_ = NA_real_,
		country = "Zimbabwe",
		adm1 = "Mashonaland East",
		adm2 = "Goromonzi",
		location = "Goromonzi",
		# Goromonzi district. Not a plot coordinate.
		latitude = -17.80695,
		longitude = 31.36372,
		geo_from_source = FALSE,
		on_farm = TRUE,
		is_survey = FALSE,
		irrigated = FALSE,
		yield_isfresh = FALSE
	)

	maize_row <- inter | grepl("maize", r1$Treatment_desc)
	cowpea_row <- inter | grepl("cowpea", r1$Treatment_desc)

	d1 <- base[maize_row, ]
	d1$crop <- "maize"
	d1$variety <- "SC627"
	d1$variety_type <- NA_character_
	d1$intercrops <- ifelse(inter[maize_row], "cowpea", "none")
	d1$yield <- num(r1$Maize_yield_kg_ha)[maize_row]
	d1$yield_part <- "grain"
	d1$yield_moisture <- 12.5
	# 100-seed mass (g) to g per 1000 seeds
	d1$seed_weight <- num(r1[["100_seed_mass_maize_12.5"]])[maize_row] * 10
	d1$partial_ler_ <- num(r1$Partial_LER_maize)[maize_row]
	d1$row_spacing <- 90
	d1$plant_spacing <- 25
	d1$plant_density <- 44444
	# Planting finished on these dates. It started on 28 Nov 2017 and 30 Nov 2018.
	d1$planting_date <- ifelse(d1$season == "first", "2017-12-01", "2018-12-04")
	d1$harvest_date <- ifelse(d1$season == "first", "2018-05-05", "2019-04-22")

	d2 <- base[cowpea_row, ]
	d2$crop <- "cowpea"
	d2$intercrops <- ifelse(inter[cowpea_row], "maize", "none")
	d2$yield <- num(r1$Cowpea_yield_kg_ha)[cowpea_row]
	d2$yield_part <- "seed"
	d2$yield_moisture <- 9.7
	d2$seed_weight <- num(r1[["100_seed_mass_cowpeas_9.7"]])[cowpea_row] * 10
	d2$partial_ler_ <- num(r1$Partial_LER_cowpeas)[cowpea_row]
	d2$plant_spacing <- 25
	sole <- d2$intercrops == "none"
	d2$row_spacing <- ifelse(sole, 45, 90)
	d2$plant_density <- ifelse(sole, 88888, 44444)
	# 2017/18 sowing was 28 Dec-1 Jan. 2018/19 sowing was 14-18 Dec.
	d2$planting_date <- ifelse(d2$season == "first", "2018-01-01", "2018-12-18")
	d2$harvest_date <- ifelse(d2$season == "first", "2018-05-05",
		ifelse(d2$variety == "CBC2", "2019-03-31", "2019-04-22"))

	d <- carobiner::bindr(d1, d2)
	d$trial_id <- paste(d$site, d$season, sep = "_")

	# One composite per farm and field type, measured before the first season.
	soil <- data.frame(
		site = as.character(r2$Site),
		field_position_ = tolower(trimws(r2$Field)),
		previous_crop = tolower(trimws(as.character(r2$Previous_crop))),
		depth_top = 0,
		depth_bottom = 20,
		soil_clay = num(r2$Clay_perc),
		soil_silt = num(r2$Silt_perc),
		soil_sand = num(r2$Total_sand_perc),
		soil_sand_fine = num(r2$Fine_sand_perc),
		soil_sand_medium = num(r2$Medium_sand_perc),
		soil_sand_coarse = num(r2$Coarse_sand_perc),
		soil_pH_CaCl2 = num(r2$pH_CaCl2),
		soil_P = num(r2$Available_P),
		soil_P_method = "resin",
		soil_N_total = num(r2$Total_Nitrogen) * 10000,
		soil_SOC = num(r2$SOC_perc),
		soil_K_exch = num(r2$K_exch),
		soil_Ca_exch = num(r2$Ca_exch),
		soil_Mg_exch = num(r2$Mg_exch),
		soil_CEC = num(r2$CEC),
		soil_K = num(r2$K_ICP_mg_kg),
		soil_Ca = num(r2$Ca_ICP_mg_kg),
		soil_Mg = num(r2$Mg_ICP_mg_kg),
		soil_Zn = num(r2$Zn_ICP_mg_kg),
		soil_Cu = num(r2$Cu_ICP_mg_kg),
		soil_Fe = num(r2$Fe_ICP_mg_kg),
		soil_Mn = num(r2$Mn_ICP_mg_kg),
		soil_Al = num(r2$Al_ICP_mg_kg)
	)
	soil <- soil[!duplicated(soil[, c("site", "field_position_")]), ]
	soil$previous_crop <- carobiner::replace_values(soil$previous_crop,
		c("sweet_potatoes", "groundnuts", "velvet_beans", "fallow"),
		c("sweetpotato", "groundnut", "velvet bean", "none"))

	d <- merge(d, soil, by = c("site", "field_position_"), all.x = TRUE)

	carobiner::write_files(path, meta, d)
}
