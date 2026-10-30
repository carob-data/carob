# R script for "carob"
# license: GPL (>=3)

## NOTES
# Soil tests from Peru, 1989-2012. Dictionary: pH is a water or KCl suspension and the file does not say which row is which. EC is dS/m. CaCO3, organic matter, sand, silt and clay are percent. Extractable P is Olsen P in ppm. Extractable K is ppm or, in other rows, kg/ha. Total N is percent. Exchangeable cations and CEC are meq/100g.
# The column Lime is silt (dictionary abbreviation Silt). Sand + silt + clay is not always 100.
# Latitude and longitude are decimal degrees. The dictionary calls the unit sexagesimal, which does not match the numbers.
# Altitude -9999 is a missing code.
# Potassium in kg/ha is converted to mg/kg as kg/ha / 1.1. The two potassium columns never overlap.
# CaCO3 percent is stored as g/kg (percent * 10). Total N percent is stored as mg/kg (percent * 10000).

## ISSUES
# pH method is not identified per row, so soil_pH may mix water and KCl.
# Iron, copper, zinc and boron are 0/1 flags in this file, not the ppm concentrations named in the dictionary.
# Some pH, EC, organic matter, Olsen P and base-saturation values sit outside the terminag range. They are left as recorded.
# 16 rows have no coordinates.
# write_files: missing carob_contributor. out of bounds: soil_pH (3.2, 10.46), soil_EC (0.02, 42.4), soil_SOM (0.03, 50.9), soil_P (0.7, 503.7), soil_BS (5, 101). invalid terms: soil_texture organic soil. duplicate coordinates: fewer coordinates than locations. NA detected: latitude, longitude.

## NOT PROCESSED
# Date of submittion, Date of reporting, Sheet Reception, Researcher: lab administration, not the sample.
# Type of Analysis: almost all rows are "Soil characterization".
# Field, Sampling area, Observation: free text, not a location code.
# Laboratory code: not unique.
# Extractable Potassium (kg/ha): used only to fill soil_K where the ppm column is empty.
# Total Cations and Total Bases: sums of the exchangeable cations.
# Soluble Magnesium: empty.
# Iron, Copper, Zinc, Soluble Boron: 0/1 flags, not concentrations.


carob_script <- function(path) {

"
Dataset for: Soil Analysis Compilation Dataset

Soil characterization of diverse localities from Peru (1989-2012) with the following variables collected: soil pH, electrical conductivity, calcium carbonate, organic matter, extractable phosporus, extractable potassium (kg/ha), sand, lime, clay, soil texture, cation exchange capacity, exchangable calcium, exchangable magnesium, exchangable potassium, exchangable sodium, exchangable aluminium + hidrogenum, total nitrogen and altitude.
"

	uri <- "doi:10.21223/SVQF9U"
	group <- "soil_samples"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 75,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "01_soil_data.xlsx"]
	nms <- names(carobiner::read.excel(f, n_max=0))
	types <- rep("guess", length(nms))
	# Sheet Reception mixes plot codes such as "001 L - 02 C" with numbers.
	types[nms == "Sheet Reception"] <- "text"
	types[nms == "Laboratory code"] <- "text"
	types[nms == "Field"] <- "text"
	r <- carobiner::read.excel(f, na=c("NULL", ""), col_types=types)

	tex <- tolower(trimws(as.character(r[["Soil texture"]])))
	tex[tex == "silt loam"] <- "silty loam"
	tex[!nzchar(tex)] <- NA_character_

	k_ppm <- as.numeric(r[["Extractable Potassium (ppm)"]])
	k_kg <- as.numeric(r[["Extractable Potassium (kg/ha)"]])
	# Rows have one potassium column or the other. kg/ha / 1.1 approximates mg/kg.
	k_ppm[is.na(k_ppm)] <- k_kg[is.na(k_ppm)] / 1.1

	elev <- as.numeric(r$Altitude)
	elev[!is.na(elev) & elev < -200] <- NA_real_

	d <- data.frame(
		sample_id = as.character(r$Id),
		date = as.character(r$Year),
		country = trimws(as.character(r$Country)),
		adm1 = carobiner::fix_name(as.character(r$Adm1), "title"),
		adm2 = carobiner::fix_name(as.character(r$Adm2), "title"),
		adm3 = carobiner::fix_name(as.character(r$Adm3), "title"),
		location = carobiner::fix_name(as.character(r$Locality), "title"),
		longitude = as.numeric(r$Longitude),
		latitude = as.numeric(r$Latitude),
		elevation = elev,
		soil_pH = as.numeric(r[["Soil pH"]]),
		soil_EC = as.numeric(r[["Electrical conductivity"]]),
		soil_CaCO3 = as.numeric(r[["Calcium Carbonate"]]) * 10,
		soil_SOM = as.numeric(r[["Organic matter"]]),
		soil_P = as.numeric(r[["Extractable Phosphorus"]]),
		soil_P_method = "Olsen",
		soil_K = k_ppm,
		soil_sand = as.numeric(r$Sand),
		soil_silt = as.numeric(r$Lime),
		soil_clay = as.numeric(r$Clay),
		soil_texture = tex,
		soil_CEC = as.numeric(r[["Cation exchange capacity"]]),
		soil_Ca_exch = as.numeric(r[["Exchangeable Calcium"]]),
		soil_Mg_exch = as.numeric(r[["Exchangeable Magnesium"]]),
		soil_K_exch = as.numeric(r[["Exchangeable Potassium"]]),
		soil_Na_exch = as.numeric(r[["Exchangeable Sodium"]]),
		soil_ex_acidity = as.numeric(r[["Exchangeable Aluminium + hidrogenum"]]),
		soil_BS = as.numeric(r[["Base Saturation"]]),
		soil_N_total = as.numeric(r[["Total nitrogen"]]) * 10000
	)

	d$geo_from_source <- NA
	d$geo_from_source[!is.na(d$longitude) & !is.na(d$latitude)] <- TRUE
	d$is_survey <- TRUE
	d$on_farm <- NA

	carobiner::write_files(path, meta, d)
}
