# R script for "carob"
# license: GPL (>=3)

## NOTES
# Soil samples on a 1 km grid, 0-30 cm. Latitude and longitude are in the file.
# The first row of the sheet is the unit line.
# Nitrate is the N-NO3 column, in ppm. Electric conductivity is dS/m, equal to mS/cm.
# Where both Bray and Olsen phosphorus are filled, Bray is kept.
# Geographic coverage is empty. The state column is Morelos. Country is not set from that.

## ISSUES
# write_files: missing variables: country. location: variable 'country' is missing.

## NOT PROCESSED
# Carbonates, exchangeable cations in meq/100 g, and saturation percentages.
# Map images.


carob_script <- function(path) {

"
Fertility Maps of \"Jonacatepec, Morelos\".

Soil sampling with 1x1 km grid in the agricultural area of the Jonacatepec Municipality at at 0 to 30 cm depth.
"

	uri <- "hdl:11529/10548739"
	group <- "soil_samples"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "BD_Suelo_Jonacatepec.xls"]
	r <- carobiner::read.excel(f, sheet="Jonacatepec", na=c(".", "NA", ""))
	if (as.character(r$Arena[1]) == "%") r <- r[-1, ]

	tex <- trimws(as.character(r$`Clase Textural`))
	tex[tex %in% c("", "No aplica")] <- NA_character_
	tex <- gsub("Franco Arcillo Arenoso", "sandy clay loam", tex)
	tex <- gsub("Franco Arcillo Limoso", "silty clay loam", tex)
	tex <- gsub("Franco Arcilloso", "clay loam", tex)
	tex <- gsub("Franco Arenoso", "sandy loam", tex)
	tex <- gsub("Franco Limoso", "silty loam", tex)
	tex <- gsub("Arcilla", "clay", tex)
	tex <- gsub("Arena", "sand", tex)
	tex <- gsub("Franco", "loam", tex)

	depth <- do.call(rbind, strsplit(as.character(r$Profundidad), "-", fixed=TRUE))
	bray <- as.numeric(r$`Fosforo Bray 1`)
	olsen <- as.numeric(r$`Fosforo Olsen`)
	place <- trimws(as.character(r$Localidad))
	place[!nzchar(place)] <- NA_character_

	d <- data.frame(
		sample_id = as.character(r$Folio),
		adm1 = carobiner::fix_name(r$Estado, "title"),
		adm2 = carobiner::fix_name(r$Municipio, "title"),
		location = carobiner::fix_name(place, "title"),
		latitude = as.numeric(r$Latitud),
		longitude = as.numeric(r$Longitud),
		depth_top = as.numeric(depth[, 1]),
		depth_bottom = as.numeric(depth[, 2]),
		soil_sand = as.numeric(r$Arena),
		soil_clay = as.numeric(r$Arcilla),
		soil_silt = as.numeric(r$Limo),
		soil_texture = tex,
		soil_WHC_sat = as.numeric(r$`Punto de Saturación`),
		soil_bd = as.numeric(r$`Densidad Aparente`),
		soil_Ksat = as.numeric(r$`Conductividad Hidráulica`),
		soil_pH = as.numeric(r$`pH(1:2 Agua)`),
		soil_pH_buffer = as.numeric(r$`pH Buffer`),
		soil_SOM = as.numeric(r$`Materia Orgánica`),
		soil_NO3 = as.numeric(r$`Nitrógeno (N-NO3)`),
		soil_P = ifelse(!is.na(bray), bray, olsen),
		soil_P_method = ifelse(!is.na(bray), "Bray1", ifelse(!is.na(olsen), "Olsen", NA_character_)),
		soil_K = as.numeric(r$`Potasio (K)`),
		soil_Ca = as.numeric(r$`Calcio (Ca)`),
		soil_Mg = as.numeric(r$`Magnesio (Mg)`),
		soil_Na = as.numeric(r$`Sodio (Na)`),
		soil_Fe = as.numeric(r$`Hierro (Fe)`),
		soil_Zn = as.numeric(r$`Zinc (Zn)`),
		soil_Mn = as.numeric(r$`Manganeso (Mn)`),
		soil_Cu = as.numeric(r$`Cobre (Cu)`),
		soil_B = as.numeric(r$`Boro (B)`),
		soil_Al = as.numeric(r$`Alumino (Al)`),
		soil_S = as.numeric(r$`Azufre (S)`),
		soil_CEC = as.numeric(r$`(CIC)`),
		soil_EC = as.numeric(r$`CE (Extracto)`),
		geo_from_source = TRUE
	)
	d <- d[!is.na(d$latitude) & !is.na(d$longitude), ]
	d$is_survey <- TRUE
	d$on_farm <- TRUE

	carobiner::write_files(path, meta, d)
}
