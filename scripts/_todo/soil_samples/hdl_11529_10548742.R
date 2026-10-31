# R script for "carob"
# license: GPL (>=3)

## NOTES
# 1 km soil grid. The first sheet row is the unit line and is not data.
# Latitude and longitude are in the sheet, so geo_from_source is TRUE.
# There is no country column and geographicCoverage is empty. The state is taken from the Estado column.
# "Guanjuato" in the Salamanca sheet is the spelling of Guanajuato. "Michoacan" is stored as Michoacán.
# "Estado de Mexico" is stored as México, the GADM state name.
# Nitrate-N, P, and the other nutrients are ppm, stored as mg/kg. Electrical conductivity is dS/m, equal to mS/cm.
# CEC is meq/100g, equal to cmol/kg.
# Bray-1 is the P method when that column is filled. Olsen is used only on rows where Bray-1 is empty.
# "." in the sheet is a missing value.

## ISSUES
# Country is not in the sheet or in geographicCoverage.
# meq/100g cation columns and saturation percentages are calculated from the ppm values and are not stored.
# write_files: missing variables: country. location: variable 'country' is missing. contributor:

## NOT PROCESSED
# Carbonates. Exchangeable cations in meq/100g and the saturation percentages.
# Hydrogen in ppm. Map images.


carob_script <- function(path) {

'
Fertility Maps of "Zacualpan, Morelos".

Soil sampling with 1x1 km grid in the agricultural area of the Zacualpan Municipality at at 0 to 30 cm depth.
'

	uri <- "hdl:11529/10548742"
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

	f <- ff[basename(ff) == "BD_Suelo_Zacualpan.xls"]
	r <- carobiner::read.excel(f, sheet="Zacualpan", na=".")
	r <- r[!is.na(r$Folio), ]

	prof <- as.character(r$Profundidad)
	prof[prof %in% c("NA", "N/A", "")] <- NA
	depth_top <- rep(NA_real_, length(prof))
	depth_bottom <- depth_top
	ok <- !is.na(prof)
	parts <- strsplit(prof[ok], "-", fixed=TRUE)
	depth_top[ok] <- vapply(parts, function(z) as.numeric(z[1]), numeric(1))
	depth_bottom[ok] <- vapply(parts, function(z) as.numeric(z[2]), numeric(1))
	texture <- trimws(as.character(r$`Clase Textural`))
	texture[texture %in% c("", "No aplica")] <- NA
	texture <- gsub("Franco arcillo arenoso", "sandy clay loam", texture, ignore.case=TRUE)
	texture <- gsub("Franco arcillo limoso", "silty clay loam", texture, ignore.case=TRUE)
	texture <- gsub("Franco arenoso", "sandy loam", texture, ignore.case=TRUE)
	texture <- gsub("Franco limoso", "silty loam", texture, ignore.case=TRUE)
	texture <- gsub("Franco arcilloso", "clay loam", texture, ignore.case=TRUE)
	texture <- gsub("Arcillo arenoso", "sandy clay", texture, ignore.case=TRUE)
	texture <- gsub("Arcillo limoso", "silty clay", texture, ignore.case=TRUE)
	texture <- gsub("^Franco$", "loam", texture)
	texture <- gsub("^Arcilla$", "clay", texture)
	texture <- gsub("^Arena$", "sand", texture)
	texture <- gsub("^Limo$", "silt", texture)

	bray <- as.numeric(r$`Fosforo Bray 1`)
	olsen <- as.numeric(r$`Fosforo Olsen`)
	p <- bray
	method <- rep("Bray1", length(p))
	use_olsen <- is.na(p) & !is.na(olsen)
	p[use_olsen] <- olsen[use_olsen]
	method[use_olsen] <- "Olsen"
	method[is.na(p)] <- NA

	adm <- trimws(as.character(r$Estado))
	adm[adm %in% c("Estado de Mexico", "Estado de México")] <- "México"
	adm[adm == "Michoacan"] <- "Michoacán"
	adm[adm == "Guanjuato"] <- "Guanajuato"

	place <- if ("Localidad" %in% names(r)) r$Localidad else r$Rancho

	d <- data.frame(
		sample_id = as.character(r$Folio),
		adm1 = adm,
		adm2 = carobiner::fix_name(r$Municipio, "title"),
		location = carobiner::fix_name(place, "title"),
		latitude = as.numeric(r$Latitud),
		longitude = as.numeric(r$Longitud),
		depth_top = depth_top,
		depth_bottom = depth_bottom,
		soil_sand = as.numeric(r$Arena),
		soil_clay = as.numeric(r$Arcilla),
		soil_silt = as.numeric(r$Limo),
		soil_texture = texture,
		soil_WHC_sat = as.numeric(r$`Punto de Saturación`),
		soil_bd = as.numeric(r$`Densidad Aparente`),
		soil_Ksat = as.numeric(r$`Conductividad Hidráulica`),
		soil_pH = as.numeric(r$`pH(1:2 Agua)`),
		soil_pH_buffer = as.numeric(r$`pH Buffer`),
		soil_SOM = as.numeric(r$`Materia Orgánica`),
		soil_N = as.numeric(r$`Nitrógeno (N-NO3)`),
		soil_P = p,
		soil_P_method = method,
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
		geo_from_source = TRUE,
		is_survey = TRUE,
		on_farm = TRUE
	)

	carobiner::write_files(path, meta, d)
}

