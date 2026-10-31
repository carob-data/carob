# R script for "carob"
# license: GPL (>=3)

## NOTES
# Soil samples, 0-30 cm, from Salvatierra, Guanajuato, sheet Matriz 2022. The Estado column is Guanajuato. The crop to be established is maize.
# Some coordinates include a degree symbol. That symbol is removed. Folio 0103-22, El Sabino, is stored at 29.27 N. The other points are near 20.2 N.
# Phosphorus is Bray 1 or Olsen, never both on the same row. Nutrients labeled in the sibling lab files as ppm are stored as mg/kg. Carbonates in this sheet are missing.
# Farmer names are not stored.

## ISSUES
# One coordinate is far north of the other Salvatierra points. Folio 0103-22 is at 29.27 N.
# Some sodium values are above the vocabulary maximum and are kept.
# Hydraulic conductivity and the milliequivalent columns are calculated in the sibling metadata.
# write_files: unknown variables: saturation_pct_. out of bounds: soil_Na (107.931, 859.406).

## NEW VARIABLES
# saturation_pct_: saturation percentage, percent.


## NOT PROCESSED
# Matriz 20 y 22 has the same folios. The single-element sheets and Comparación repeat those values.
# Farmer names, target yield, previous crop (recorded as SIN DATO), field capacity, permanent wilting point, and cation ratios.
# Map images.


carob_script <- function(path) {

"
Soil sampling with 1x1 km grid in the agricultural area of the Salvatierra Municipality at at 0 to 30 cm depth.
"

	uri <- "hdl:11529/10548663"
	group <- "soil_samples"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "BD_Suelo_Salvatierra.xlsx"]
	r <- carobiner::read.excel(f, sheet="Matriz 2022", na=".")
	r <- r[!is.na(r$Folio), ]

	num <- function(x) {
		x <- gsub("°", "", as.character(x), fixed=TRUE)
		x <- trimws(x)
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
		"arena franca"="loamy sand"
	)
	tex <- trimws(as.character(r$`Clase Textural`))
	mapped <- unname(tex_key[tolower(tex)])
	miss <- is.na(mapped) & !is.na(tex) & nzchar(tex)
	mapped[miss] <- tex[miss]

	depth <- strsplit(as.character(r$Profundidad), "-", fixed=TRUE)
	lat <- num(r$Latitud)
	lon <- num(r$Longitud)
	rancho <- trimws(as.character(r$Rancho))
	rancho[!nzchar(rancho)] <- NA_character_
	loc <- rep(NA_character_, length(rancho))
	has <- !is.na(rancho)
	loc[has] <- carobiner::fix_name(rancho[has], "title")

	bray <- num(r$`Fosforo Bray 1`)
	olsen <- num(r$`Fosforo Olsen`)
	p <- bray
	p[!is.na(olsen)] <- olsen[!is.na(olsen)]
	method <- ifelse(!is.na(olsen), "Olsen", "Bray 1")
	method[is.na(p)] <- NA_character_

	d <- data.frame(
		sample_id = trimws(as.character(r$Folio)),
		country = "Mexico",
		adm1 = carobiner::fix_name(r$Estado, "title"),
		adm2 = carobiner::fix_name(r$Municipio, "title"),
		location = loc,
		latitude = lat,
		longitude = lon,
		geo_from_source = !is.na(lat) & !is.na(lon),
		crop = "maize",
		depth_top = as.numeric(vapply(depth, function(z) z[1], "")),
		depth_bottom = as.numeric(vapply(depth, function(z) z[2], "")),
		soil_sand = num(r$Arena),
		soil_clay = num(r$Arcilla),
		soil_silt = num(r$Limo),
		soil_texture = mapped,
		saturation_pct_ = num(r$`Punto de Saturación`),
		soil_bd = num(r$`Densidad Aparente`),
		soil_pH = num(r$`pH(1:2 Agua)`),
		soil_SOM = num(r$`Materia Orgánica`),
		soil_NO3 = num(r$Nitratos),
		soil_P = p,
		soil_P_method = method,
		soil_K = num(r$`Potasio(K)`),
		soil_Ca = num(r$`Calcio(Ca)`),
		soil_Mg = num(r$`Magnesio(Mg)`),
		soil_Na = num(r$`Sodio(Na)`),
		soil_Fe = num(r$`Hierro(Fe)`),
		soil_Zn = num(r$`Zinc(Zn)`),
		soil_Mn = num(r$`Manganeso(Mn)`),
		soil_Cu = num(r$`Cobre(Cu)`),
		soil_B = num(r$`Boro(B)`),
		soil_Al = num(r$`Alumino(Al)`),
		soil_S = num(r$`Azufre(S)`),
		soil_EC = num(r$`CE(Extracto)`),
		is_survey = TRUE,
		on_farm = TRUE
	)
	d$record_id <- as.integer(seq_len(nrow(d)))
	carobiner::write_files(path, meta, d)
}
