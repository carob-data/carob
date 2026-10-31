# R script for "carob"
# license: GPL (>=3)

## NOTES
# Soil samples, 0-30 cm, from the agricultural area of Tlachichuca, Puebla. The Estado column is Puebla.
# Texture classes follow the USDA names in the metadata. Arcilla is clay. Franco Arcilloso is clay loam.
# Nutrients are ppm, stored as mg/kg. Phosphorus in this file is Bray 1. Carbonates are percent CaCO3, stored as g/kg. The token of dots is missing.
# Electric conductivity is dS/m, equal to mS/cm. Latitude and longitude are whole degrees. Uncertainty is 111 km.
# pH is a 1:2 soil:water ratio.

## ISSUES
# Coordinates are whole degrees. Positive longitudes were given a minus sign.
# One silt value is negative and one nitrate value is negative. They are kept.
# Hydraulic conductivity, milliequivalents, saturation percentages, and CIC are calculated in the metadata.
# write_files: unknown variables: saturation_pct_. out of bounds: soil_silt (-8, 40), soil_NO3 (-0.144, 100.188). duplicate coordinates: fewer coordinates than locations.

## NEW VARIABLES
# saturation_pct_: saturation percentage, gravimetric maximum water content, percent.


## NOT PROCESSED
# Conductividad Hidráulica, milliequivalents, cation saturation, and CIC. The metadata says they are calculated.
# The map PDF.


carob_script <- function(path) {

"
Soil sampling with 1x1 km grid in the agricultural area of the Tlachichuca Municipality at at 0 to 30 cm depth.
"

	uri <- "hdl:11529/10548744"
	group <- "soil_samples"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 65,
		carob_effort = 0.35
	)

	f <- ff[basename(ff) == "BD_Suelo_Tlachichuca.xlsx"]
	r <- carobiner::read.excel(f, sheet="Suelos", na=".")
	r <- r[!is.na(r$Folio), ]

	num <- function(x) {
		x <- trimws(as.character(x))
		x[grepl("^[.…]{1,}$", x)] <- NA_character_
		as.numeric(x)
	}
	tex_key <- c(
		"arcilla"="clay", "arcilloso"="clay",
		"franco arcilloso"="clay loam",
		"franco arcillo arenoso"="sandy clay loam",
		"arcillo arenoso"="sandy clay",
		"franco"="loam",
		"franco arenoso"="sandy loam",
		"franco limoso"="silt loam",
		"arcilla limosa"="silty clay",
		"franco arcillo limoso"="silty clay loam",
		"arena"="sand",
		"arena franca"="loamy sand",
		"areno franco"="loamy sand"
	)
	tex <- trimws(as.character(r$`Clase Textural`))
	mapped <- unname(tex_key[tolower(tex)])
	miss <- is.na(mapped) & !is.na(tex)
	mapped[miss] <- tex[miss]

	depth <- strsplit(as.character(r$Profundidad), "-", fixed=TRUE)
	lat <- num(r$Latitud)
	lon <- num(r$Longitud)
	# Puebla is west of Greenwich. Positive longitudes in this file are the same meridian without the minus sign.
	lon[!is.na(lon) & lon > 0] <- -lon[!is.na(lon) & lon > 0]
	whole <- !is.na(lat) & !is.na(lon) & (lat == round(lat)) & (lon == round(lon))
	unc <- rep(NA_real_, length(lat))
	unc[whole] <- 111000
	rancho <- trimws(as.character(r$Rancho))
	rancho[!nzchar(rancho)] <- NA_character_
	loc <- rep(NA_character_, length(rancho))
	has <- !is.na(rancho)
	loc[has] <- carobiner::fix_name(rancho[has], "title")

	d <- data.frame(
		sample_id = trimws(as.character(r$Folio)),
		country = "Mexico",
		adm1 = carobiner::fix_name(r$Estado, "title"),
		adm2 = carobiner::fix_name(r$Municipio, "title"),
		location = loc,
		latitude = lat,
		longitude = lon,
		geo_from_source = TRUE,
		geo_uncertainty = unc,
		depth_top = as.numeric(vapply(depth, function(z) z[1], "")),
		depth_bottom = as.numeric(vapply(depth, function(z) z[2], "")),
		soil_sand = num(r$Arena),
		soil_clay = num(r$Arcilla),
		soil_silt = num(r$Limo),
		soil_texture = mapped,
		saturation_pct_ = num(r$`Punto de Saturación`),
		soil_bd = num(r$`Densidad Aparente`),
		soil_pH = num(r$`pH(1:2 Agua)`),
		soil_pH_buffer = num(r$`pH Buffer`),
		soil_CaCO3 = num(r$Carbonatos) * 10,
		soil_SOM = num(r$`Materia Orgánica`),
		soil_NO3 = num(r$`Nitrógeno (N-NO3)`),
		soil_P = num(r$`Fosforo Bray 1`),
		soil_P_method = "Bray 1",
		soil_K = num(r$`Potasio (K)`),
		soil_Ca = num(r$`Calcio (Ca)`),
		soil_Mg = num(r$`Magnesio (Mg)`),
		soil_Na = num(r$`Sodio (Na)`),
		soil_Fe = num(r$`Hierro (Fe)`),
		soil_Zn = num(r$`Zinc (Zn)`),
		soil_Mn = num(r$`Manganeso (Mn)`),
		soil_Cu = num(r$`Cobre (Cu)`),
		soil_B = num(r$`Boro (B)`),
		soil_Al = num(r$`Alumino (Al)`),
		soil_S = num(r$`Azufre (S)`),
		soil_EC = num(r$`CE (Extracto)`),
		is_survey = TRUE,
		on_farm = TRUE
	)
	if (all(is.na(d$soil_CaCO3))) d$soil_CaCO3 <- NULL
	d$record_id <- as.integer(seq_len(nrow(d)))
	carobiner::write_files(path, meta, d)
}
