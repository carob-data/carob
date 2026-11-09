# R script for "carob"
# license: GPL (>=3)

## NOTES
# Two diploid-potato vitamin C trials. Both are field RCBDs with 10 plants on 2.7 m2 (0.30 m between plants, 0.90 m between rows).
# La Victoria, Huancayo, Junin has four blocks. Aco, Paucartambo, Pasco is described as three blocks. The Aco fieldbook also has two plots labeled rep 4.
# Aco planting and harvest are the Crop_management rows. Those cells are Excel serials stored as text: 2015-05-03 and 2015-10-12. The laboratory sheet repeats those dates.
# La Victoria planting and harvest cells are blank. The Minimal begin and end serials are 2016-01-19 and 2016-06-09. They are not labeled planting or harvest.
# AVDM is tuber dry matter percent. yield_moisture is 100 minus AVDM. ASC_FW and ASC_DW are ascorbic acid in mg/100 g. The dictionary computes ASC_DW as ASC_FW * 100 / AVDM.
# Variety is the accession name, otherwise the accession code, otherwise the institutional number.
# La Victoria soil samples are labeled REP1 to REP4 and are joined on rep. Data1 and Data2 are "CLAY SLIT" (about 11% sand, 45-48% silt, 41-44% clay), stored as silty clay. Data3 and Data4 are "FRANK CLAY SLIT" (about 10-12% sand, 51-52% silt, 37-38% clay), stored as silty clay loam. Data5 is a guano sample.
# Aco texture "Loamy sand soil" (about 75% sand, 16% silt, 9% clay) is stored as loamy sand. The four Aco samples are not labeled with the fieldbook rep, so Data1 is stored for every plot. Phosphorus and potassium are ppm and are stored as mg/kg.
# The description spells the Pasco site Acco. The locality field says Aco. Aco cycle is the fieldbook OBS value because the material-list cycle is blank.

## ISSUES
# No fertilizer rate and no irrigation statement.
# No yield and no yield moisture of a harvested yield. The moisture column is 100 minus tuber dry matter.
# La Victoria has no planting or harvest date.
# Junin and San Agustin should be checked against GADM (Junín; the district spelling).
# Aco phosphorus in Data1 is 299.38 mg/kg. Samples 2 to 4 are also about 277 to 299 mg/kg. The vocabulary maximum for soil_P is 200. La Victoria phosphorus is 24.9 to 27.3 mg/kg, which is inside that range. The write_files span is the whole column.
# write_files: missing carob_contributor, yield, yield_part, irrigated, K_fertilizer, N_fertilizer, P_fertilizer. unknown variables: selection_cycle_, ascorbic_acid_mg_100g_dw_, ascorbic_acid_mg_100g_fw_. out of bounds: soil_P (24.9, 299.38).


## NOT PROCESSED
# Weather_data: 15-minute HOBO records. The rain column has no unit.
# Crop_management sprays: product doses are g or ml per 20 L, with no area.
# Nitrogen total is a percentage. soil_N is extractable N in mg/kg, so the percentage is not stored.
# Soil nutrients other than pH, organic matter, texture fractions, and available P and K. The guano sample is not field soil.
# CODE_LAB, the Summary sheet, and the Aco Original_laboratory worksheet. The fieldbook already has the ascorbic acid values.
# Planting density on the installation sheet. plant_density is emerged plants, and this fieldbook has no stand count.
# Data_dictionary.xls: documentation or a codebook, not an observation table.


## NEW VARIABLES
# selection_cycle_: recurrent-selection cycle. La Victoria uses the material list (Cycle_0, Cycle_I, Cycle_2, Cycle_3). Aco uses the fieldbook (Cycle0, Cycle2, Cycle3).
# ascorbic_acid_mg_100g_dw_: ascorbic acid, mg/100 g dry weight.
# ascorbic_acid_mg_100g_fw_: ascorbic acid, mg/100 g fresh weight.


carob_script <- function(path) {

"
Dataset for: Genetic gains in the content of \"Vitamin C” in diploid potato for three cycles recurrent selection

The purpose of this study was to estimate the genetic gains of three cycles of recurrent selection for the Vitamin C content in diploid potatoes, and to determine the heritability and relationships with variables of agronomical importance. The field evaluation was conducted in La Victoria, Huancayo, Junin, Peru in a Randomized Complete Block Design, with four repetitions, evaluating 61 genotypes (16=cycle0, 2=cycle1, 21=cycle2, 22=cycle3) and in Acco, Paucartambo, Pasco, Peru in a Randomized Complete Block Design, with three repetitions, evaluating 55 genotypes (15=cycle0, 0=cycle1, 20=cycle2, 20=cycle3).
"

	uri <- "doi:10.21223/P3/G4MWZN"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = "Advancing Achievements in Breeding for Early, Resilient and Nutritious Potato and Sweetpotato; Bio-fortified potato varieties to help overcome micronutrient malnutrition in East Africa",
		design = "Randomized Complete Block Design (RCBD)",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield_moisture;ascorbic_acid_mg_100g_fw_;ascorbic_acid_mg_100g_dw_",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.7
	)

	one <- function(f) {
		r <- carobiner::read.excel(f, sheet="Fieldbook")
		r1 <- carobiner::read.excel(f, sheet="Material_List")
		r2 <- carobiner::read.excel(f, sheet="Minimal")
		r3 <- carobiner::read.excel(f, sheet="Soil_analysis")
		r4 <- carobiner::read.excel(f, sheet="Crop_management")
		r5 <- carobiner::read.excel(f, sheet="Installation")
		v <- setNames(trimws(as.character(r2$Value)), trimws(r2$Factor))
		iv <- setNames(trimws(as.character(r5$Value)), trimws(r5$Factor))
		blank <- function(x) {
			x <- trimws(as.character(x))
			x[!nzchar(x)] <- NA_character_
			x
		}
		as_date <- function(x) {
			if (length(x) == 0 || is.na(x[1])) return(NA_character_)
			x <- x[1]
			if (inherits(x, "POSIXt") || inherits(x, "Date")) {
				return(as.character(as.Date(x)))
			}
			x <- trimws(as.character(x))
			if (!nzchar(x)) return(NA_character_)
			if (grepl("^[0-9]+$", x)) {
				return(as.character(as.Date(as.numeric(x), origin="1899-12-30")))
			}
			as.character(as.Date(x))
		}
		clean_tex <- function(x) {
			x <- gsub("\u00a0", " ", as.character(x))
			x <- trimws(gsub("[[:space:]]+", " ", x))
			known <- c(
				"CLAY SLIT" = "silty clay",
				"FRANK CLAY SLIT" = "silty clay loam",
				"Loamy sand soil" = "loamy sand"
			)
			unname(known[x])
		}
		grab <- function(col, name) as.numeric(r3[[col]][r3$Abbreviature == name][1])
		grab_tex <- function(col) r3[[col]][r3$Abbreviature == "Soil texture"][1]
		key <- trimws(as.character(r1$Accession_Number))
		nm <- blank(r1$Accession_Name)
		code <- blank(r1$Accession_Code)
		instn <- trimws(as.character(r$INSTN))
		i <- match(instn, key)
		variety <- nm[i]
		variety[is.na(variety)] <- code[i][is.na(variety)]
		variety[is.na(variety)] <- instn[is.na(variety)]
		cycle <- blank(r1$Cycle[i])
		obs <- blank(r$OBS)
		cycle[is.na(cycle)] <- obs[is.na(cycle)]
		d <- data.frame(
			trial_id = v[["Trial_name"]],
			crop = tolower(v[["Crop"]]),
			variety = variety,
			variety_code = instn,
			treatment = variety,
			selection_cycle_ = cycle,
			female_parent = blank(r1$Female_AcceNumb[i]),
			male_parent = blank(r1$Male_AcceNumb[i]),
			rep = as.integer(r$REP),
			plot_id = as.character(r$PLOT),
			yield_moisture = 100 - as.numeric(r$AVDM),
			ascorbic_acid_mg_100g_dw_ = as.numeric(r$ASC_DW),
			ascorbic_acid_mg_100g_fw_ = as.numeric(r$ASC_FW),
			plot_area = as.numeric(iv[["Plot_size_(m2)"]]),
			plant_spacing = as.numeric(iv[["Distance_between_plants_(m)"]]) * 100,
			row_spacing = as.numeric(iv[["Distance_between_rows_(m)"]]) * 100,
			country = v[["Country"]],
			adm1 = carobiner::fix_name(v[["Admin1"]], "title"),
			adm2 = carobiner::fix_name(v[["Admin2"]], "title"),
			adm3 = carobiner::fix_name(v[["Admin3"]], "title"),
			location = carobiner::fix_name(v[["Locality"]], "title"),
			elevation = as.numeric(v[["Elevation"]]),
			latitude = as.numeric(v[["Latitude"]]),
			longitude = as.numeric(v[["Longitude"]]),
			planting_date = as_date(r4$Date[r4$Intervention_type == "Planting"][1]),
			harvest_date = as_date(r4$Date[r4$Intervention_type == "Harvest"][1]),
			on_farm = FALSE,
			is_survey = FALSE,
			geo_from_source = TRUE
		)
		if (v[["Site_short_name"]] == "LAVICT") {
			cols <- paste0("Data", 1:4)
			s <- data.frame(rep = 1:4)
			s$soil_pH <- vapply(cols, function(col) grab(col, "Soil PH"), numeric(1))
			s$soil_SOM <- vapply(cols, function(col) grab(col, "Organic matter"), numeric(1))
			s$soil_P <- vapply(cols, function(col) grab(col, "Phosphorus"), numeric(1))
			s$soil_K <- vapply(cols, function(col) grab(col, "Potassium"), numeric(1))
			s$soil_sand <- vapply(cols, function(col) grab(col, "Sand"), numeric(1))
			s$soil_silt <- vapply(cols, function(col) grab(col, "Silt"), numeric(1))
			s$soil_clay <- vapply(cols, function(col) grab(col, "Clay"), numeric(1))
			s$soil_texture <- clean_tex(vapply(cols, grab_tex, character(1)))
			j <- match(d$rep, s$rep)
			d$soil_pH <- s$soil_pH[j]
			d$soil_SOM <- s$soil_SOM[j]
			d$soil_P <- s$soil_P[j]
			d$soil_K <- s$soil_K[j]
			d$soil_sand <- s$soil_sand[j]
			d$soil_silt <- s$soil_silt[j]
			d$soil_clay <- s$soil_clay[j]
			d$soil_texture <- s$soil_texture[j]
		} else {
			d$soil_pH <- grab("Data1", "Soil PH")
			d$soil_SOM <- grab("Data1", "Organic matter")
			d$soil_P <- grab("Data1", "Phosphorus")
			d$soil_K <- grab("Data1", "Potassium")
			d$soil_sand <- grab("Data1", "Sand")
			d$soil_silt <- grab("Data1", "Silt")
			d$soil_clay <- grab("Data1", "Clay")
			d$soil_texture <- clean_tex(grab_tex("Data1"))
		}
		d
	}

	f1 <- ff[basename(ff) == "PTVitaminC012016_LAVICT.xls"]
	f2 <- ff[basename(ff) == "PTVitaminC052015_ACO.xls"]
	d <- rbind(one(f1), one(f2))
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
