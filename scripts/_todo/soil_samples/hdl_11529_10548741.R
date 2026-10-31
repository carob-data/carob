# R script for "carob"
# license: GPL (>=3)

## NOTES
# 1 km soil grid. The units row under the header is dropped.
# Estado is a Mexican state name. country is Mexico from that column.
# Longitud is stored as positive degrees in some files. Values east of 0 are set west of Greenwich because these municipalities are in Mexico.
# Where both Bray and Olsen P are filled, Bray is stored and Olsen is not.
# Carbonatos are percent CaCO3, stored as g/kg.
# CE (Extracto) is dS/m, the same scale as mS/cm.
# Exchangeable cations are meq/100g, stored as cmol/kg.

## ISSUES
# write_files: duplicate coordinates: fewer coordinates than locations.

## NOT PROCESSED
# Percent saturation of Ca, Mg, K, Na, Al, and H.
# Hydrogen in ppm, map images, and the METADATA dictionary sheet.

carob_script <- function(path) {

"Fertility Maps of Yecapixtla, Morelos.

Soil sampling with 1x1 km grid in the agricultural area of the Yecapixtla Municipality at at 0 to 30 cm depth."

	uri <- "hdl:11529/10548741"
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
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "BD_Suelo_Yecapixtla.xls"]
	r <- carobiner::read.excel(f, sheet="Yecapixtla", na=c(".", ""))
	r <- r[!is.na(r$Estado), ]

	num <- function(nm) {
		if (!nm %in% names(r)) return(rep(NA_real_, nrow(r)))
		x <- r[[nm]]
		if (is.character(x)) {
			x <- trimws(x)
			x[x %in% c("No aplica", "-", "NA", "")] <- NA_character_
		}
		as.numeric(x)
	}

	lon <- num("Longitud")
	# Western Mexico. A positive longitude in these files is degrees west without a sign.
	if (length(lon) && all(lon > 0, na.rm=TRUE)) lon <- -lon

	place <- if ("Rancho" %in% names(r)) r$Rancho else if ("Localidad" %in% names(r)) r$Localidad else rep(NA_character_, nrow(r))

	tex <- r[["Clase Textural"]]
	tex <- trimws(tex)
	tex[tex %in% c("No aplica", "")] <- NA_character_
	tex <- gsub("Franco Arcillo Arenoso", "sandy clay loam", tex)
	tex <- gsub("Franco Arcillo Limoso", "silty clay loam", tex)
	tex <- gsub("Franco Arcilloso", "clay loam", tex)
	tex <- gsub("Franco Limoso", "silty loam", tex)
	tex <- gsub("Franco Arenoso", "sandy loam", tex)
	tex <- gsub("Areno Franco", "sandy loam", tex)
	tex <- gsub("Arcillo Arenoso", "sandy clay", tex)
	tex <- gsub("Arcilla", "clay", tex)
	tex <- gsub("Franco", "loam", tex)
	tex <- gsub("Arena", "sand", tex)
	tex <- gsub("Limo", "silt", tex)

	bray <- num("Fosforo Bray 1")
	olsen <- num("Fosforo Olsen")
	p <- ifelse(!is.na(bray), bray, olsen)
	pmethod <- ifelse(!is.na(bray), "Bray1", ifelse(!is.na(olsen), "Olsen", NA_character_))

	parts <- strsplit(as.character(r$Profundidad), "-")
	depth_top <- as.numeric(vapply(parts, function(z) trimws(z[1]), character(1)))
	depth_bottom <- as.numeric(vapply(parts, function(z) if (length(z) < 2) NA_character_ else trimws(z[2]), character(1)))

	hnames <- grep("^Hidrogeno|^Hidrógeno", names(r), value=TRUE)
	h_exch <- if (length(hnames)) num(hnames[length(hnames)]) else rep(NA_real_, nrow(r))

	d <- data.frame(
		country = "Mexico",
		adm1 = carobiner::fix_name(r$Estado, "title"),
		adm2 = carobiner::fix_name(r$Municipio, "title"),
		location = carobiner::fix_name(place, "title"),
		latitude = num("Latitud"),
		longitude = lon,
		sample_id = as.character(r$Folio),
		depth_top = depth_top,
		depth_bottom = depth_bottom,
		soil_sand = num("Arena"),
		soil_clay = num("Arcilla"),
		soil_silt = num("Limo"),
		soil_texture = tex,
		soil_SP = num("Punto de Saturación"),
		soil_bd = num("Densidad Aparente"),
		soil_Ksat = num("Conductividad Hidráulica"),
		soil_pH = num("pH(1:2 Agua)"),
		soil_pH_buffer = num("pH Buffer"),
		soil_CaCO3 = num("Carbonatos") * 10,
		soil_SOM = num("Materia Orgánica"),
		soil_NO3 = num("Nitrógeno (N-NO3)"),
		soil_P = p,
		soil_P_method = pmethod,
		soil_K = num("Potasio (K)"),
		soil_Ca = num("Calcio (Ca)"),
		soil_Mg = num("Magnesio (Mg)"),
		soil_Na = num("Sodio (Na)"),
		soil_Fe = num("Hierro (Fe)"),
		soil_Zn = num("Zinc (Zn)"),
		soil_Mn = num("Manganeso (Mn)"),
		soil_Cu = num("Cobre (Cu)"),
		soil_B = num("Boro (B)"),
		soil_Al = num("Alumino (Al)"),
		soil_S = num("Azufre (S)"),
		soil_Ca_exch = num("Calcio"),
		soil_Mg_exch = num("Magnesio"),
		soil_K_exch = num("Potasio"),
		soil_Na_exch = num("Sodio"),
		soil_Al_exch = num("Aluminio"),
		soil_H_exch = h_exch,
		soil_CEC = num("(CIC)"),
		soil_EC = num("CE (Extracto)"),
		geo_from_source = TRUE
	)

	d$is_survey <- TRUE
	d$on_farm <- NA

	carobiner::write_files(path, meta, d)
}
