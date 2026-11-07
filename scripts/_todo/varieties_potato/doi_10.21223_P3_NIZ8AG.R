# R script for "carob"
# license: GPL (>=3)

## NOTES
# Six vitamin C fieldbooks. The tidy table is the Fieldbook sheet. ASC_DW and ASC_FW are ascorbic acid in mg/100 g. DM is percent dry matter. yield_moisture is 100 minus DM.
# Installation: RCBD, 10 plants, plot 2.7 m2, 0.30 m between plants, 0.90 m between rows. Spacing is stored in cm. The abstract says one hill per plot. The installation count is used.
# Planting and harvest dates are the Crop_management rows of those types. They match the Minimal begin and end dates. Several vine-killing rows repeat the planting date, so that row is not used.
# Country, admin names, locality, elevation, and coordinates are the Minimal values. The abstract elevations differ (Ccasapata 3766 vs 3765, Sotopampa 3730 vs 3754, Ccollpaccasa 4068 vs 4067, La Victoria 3216 vs 3265, Rangra 4105 vs 3323).
# The Ccollpaccasa longitude in Minimal is -14.75289. That point is not in Peru. The Dataverse box lists -74.75289. The workbook value is kept.
# Soil "Lime" plus sand and clay sums to 100, so Lime is stored as silt. Texture: Franco arcilloso(s) clay loam, Franco arenoso sandy loam, Franco loam.
# Electrical conductivity is labeled mmh/cm and stored as mS/cm. CaCO3 is percent and stored as g/kg. CEC is Meq/100 g, the same scale as cmol/kg.
# Field history: cebada and the Sotopampa spelling cabada are barley, haba is faba bean, papa is potato, descanso is fallow (none). crop_rotation runs from season t-5 to t-1, separated with ";" because that is how multiple crop values are checked.
# Phosphorus and potassium are ppm and are stored as mg/kg.

## ISSUES
# No tuber yield, fertilizer rate, or irrigation statement.
# La Victoria has no field history.
# The abstract says the trials were in 2010. Planting was November or December 2009.
# Mariscal Caceres is the Conayca locality. The abstract names Conayca.
# Junin, San Agustin, and Mariscal Caceres should be checked against GADM.
# write_files: missing yield, yield_part, irrigated, K_fertilizer, N_fertilizer, P_fertilizer. unknown variables: ascorbic_acid_mg_100g_dw_, ascorbic_acid_mg_100g_fw_. coordinates not on land: Peru.

## NOT PROCESSED
# Weather_data is empty. The original weather sheet is 30-minute temperature and humidity, with no rain column.
# Summary by clone, the laboratory layout sheets, and the dictionary.
# Code Laboratory.
# Var List mineral names. Those columns are not in the Fieldbook.
# Total nitrogen is a percentage. soil_N is extractable N in mg/kg, so the percentage is not stored.
# Exchangeable and soluble cations other than the available P and K above.
# Soil iron, copper, zinc, boron, and manganese.
# Dictionary_VitaminC.xls: documentation or a codebook, not an observation table.


## NEW VARIABLES
# ascorbic_acid_mg_100g_dw_: ascorbic acid, mg/100 g dry weight.
# ascorbic_acid_mg_100g_fw_: ascorbic acid, mg/100 g fresh weight.


carob_script <- function(path) {

"
Dataset for: Stability analysis for nutritional contents (Vitamin C) on 40 native potato cultivars

Breeding efforts for enriching potato tubers with more Zn and Fe are in progress at the International Potato Center (CIP). Knowledge of genotype by environment interaction effects on the micronutrient concentrations of different genotypes is needed to identify cultivars that show high and stable concentrations and to inform breeding and selection schemes. Stability analysis for micronutrient content has been applied to biofortification and varietal dissemination strategies. Forty native potato cultivars were evaluated to: assess the magnitude and nature of Genotype (G), Environment (E), and GxE interaction effects for Vitamin C (Ascorbic Acid) in the tropical highlands of Peru and study the contribution of soil fertility to the micronutrient content of potato tubers. Tubers were taken from plots grown in randomized complete block designs with three replications of one hill per plot in each of 6 sites of the central Peruvian Andes in 2010: Ccasapata (3765 m.a.s.l); Sotopampa (3754 m.a.s.l); Ccollpaccasa (4067 m.a.s.l); Conayca (4178 m.a.s.l), la Victoria (3265 m.a.s.l) and Rangra (3323 m.a.s.l). Well-matured tubers were harvested at 150 and 180 days. Samples were prepared and analysed for ascorbic acid concentrations (AA) by the spectrophotometric method of Egoville (Burgos et al., 2009). Statistical analyses were performed using SAS software (SAS. 2003). ANOVA was performed using combined data for all environments. The Additive Main Effects and Multiplicative Interaction model (AMMI) was used for studying Genotype X Enviroment interaction, examining genotypic yield stability and adaptation (Crossa et al., 2002).
"

	uri <- "doi:10.21223/P3/NIZ8AG"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=6,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "Randomized Complete Block Design (RCBD)",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "ascorbic_acid_mg_100g_dw_;ascorbic_acid_mg_100g_fw_;yield_moisture",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.7
	)

	clean <- function(x) {
		x <- gsub("\u00a0", " ", as.character(x), fixed=TRUE)
		x <- trimws(x)
		x[!nzchar(x)] <- NA_character_
		x
	}
	crop_name <- function(x) {
		x <- tolower(trimws(x))
		m <- c(cebada="barley", cabada="barley", haba="faba bean", papa="potato", descanso="none")
		if (x %in% names(m)) unname(m[[x]]) else x
	}

	one <- function(f) {
		r <- carobiner::read.excel(f, sheet="Fieldbook")
		r1 <- carobiner::read.excel(f, sheet="Minimal")
		r2 <- carobiner::read.excel(f, sheet="Installation")
		r3 <- carobiner::read.excel(f, sheet="Crop_management")
		bn <- basename(f)
		mat_sheet <- if (grepl("RANGR", bn)) "Material-list" else "Material List"
		soil_sheet <- if (grepl("RANGR", bn)) "Soil-analysis" else "Soil_analysis"
		r4 <- carobiner::read.excel(f, sheet=mat_sheet)
		r5 <- carobiner::read.excel(f, sheet=soil_sheet)
		v <- setNames(trimws(as.character(r1$Value)), trimws(r1$Factor))
		w <- setNames(trimws(as.character(r2$Value)), trimws(r2$Factor))
		soil <- function(code) {
			hit <- which(trimws(as.character(r5$Abbreviture)) == code)
			as.numeric(r5$Data1[hit[1]])
		}
		tex <- trimws(as.character(r5$Data1[trimws(as.character(r5$Abbreviture)) == "STEX"][1]))
		tex <- c(
			"Franco arcillosos"="clay loam",
			"Franco arcillosa"="clay loam",
			"Franco arenoso"="sandy loam",
			"Franco"="loam"
		)[tex]
		key <- clean(r4[["Institutional number"]])
		nm <- clean(r4[["Clone or variety name"]])
		instn <- clean(r$INSTN)
		i <- match(instn, key)
		variety <- nm[i]
		variety[is.na(variety)] <- instn[is.na(variety)]
		seasons <- c(
			"Field history cropping season t-5",
			"Field history cropping season t-4",
			"Field history cropping season t-3",
			"Field history cropping season t-2",
			"Field history cropping season t-1"
		)
		hist <- vapply(seasons, function(s) {
			val <- w[[s]]
			if (is.null(val) || is.na(val) || !nzchar(val)) NA_character_ else crop_name(val)
		}, "")
		rotation <- if (all(is.na(hist))) NA_character_ else paste(unname(hist[!is.na(hist)]), collapse=";")
		data.frame(
			trial_id = v[["Short name or Title"]],
			crop = "potato",
			variety = variety,
			variety_code = instn,
			treatment = variety,
			rep = as.integer(r$REP),
			plot_id = as.character(r$PLOT),
			ascorbic_acid_mg_100g_dw_ = as.numeric(r$ASC_DW),
			ascorbic_acid_mg_100g_fw_ = as.numeric(r$ASC_FW),
			yield_moisture = 100 - as.numeric(r$DM),
			plot_area = as.numeric(w[["Plot size (m2)"]]),
			plant_spacing = as.numeric(w[["Distance between plants (m)"]]) * 100,
			row_spacing = as.numeric(w[["Distance between rows (m)"]]) * 100,
			previous_crop = unname(hist[5]),
			crop_rotation = rotation,
			country = v[["Country"]],
			adm1 = carobiner::fix_name(v[["Admin1"]], "title"),
			adm2 = carobiner::fix_name(v[["Admin2"]], "title"),
			adm3 = carobiner::fix_name(v[["Admin3"]], "title"),
			location = carobiner::fix_name(v[["Locality"]], "title"),
			elevation = as.numeric(v[["Elevation"]]),
			latitude = as.numeric(v[["Latitude"]]),
			longitude = as.numeric(v[["Longitude"]]),
			planting_date = as.character(as.Date(r3$Date[r3$Intervention_type == "Planting"][1])),
			harvest_date = as.character(as.Date(r3$Date[r3$Intervention_type == "Harvest"][1])),
			soil_pH = soil("SOILPH"),
			soil_SOM = soil("ORGM"),
			soil_P = soil("PHOS"),
			soil_K = soil("POT"),
			soil_sand = soil("SAND"),
			soil_silt = soil("LIME"),
			soil_clay = soil("CLAY"),
			soil_texture = as.character(unname(tex)),
			soil_EC = soil("SOILEC"),
			soil_CaCO3 = soil("CACO3") * 10,
			soil_CEC = soil("SCEC"),
			on_farm = FALSE,
			is_survey = FALSE,
			geo_from_source = TRUE
		)
	}

	files <- c(
		"TVIT200911_CCASAP_Stability_Field.xls",
		"TVIT200911_CCOLL_Stability_Field.xls",
		"TVIT200911_LAVICT_Stability_Field.xls",
		"TVIT200911_MRSCAC_Stability_Field.xls",
		"TVIT200911_RANGR_Stability_Field.xls",
		"TVIT200912_SOTOP_Stability_Field.xls"
	)
	d <- do.call(rbind, lapply(files, function(bn) one(ff[basename(ff) == bn])))
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
