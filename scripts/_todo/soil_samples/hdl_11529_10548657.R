# R script for "carob"
# license: GPL (>=3)

## NOTES
# Soil samples on a 1 km grid, 0-30 cm. Latitude and longitude are in the file.
# The first row of each data sheet is the unit line.
# Nitrate is the N-NO3 column, in ppm. Electric conductivity is dS/m, equal to mS/cm.
# Where both Bray and Olsen phosphorus are filled, Bray is kept.
# Geographic coverage is empty. Country is not set from the state name.

## ISSUES
# write_files: missing variables: country. out of bounds: soil_P (4.76, 368.87), soil_Ca (697.315, 10323.523), soil_Na (0.703, 221.077). location: variable 'country' is missing.

## NOT PROCESSED
# Carbonates, exchangeable cations in meq/100 g, and saturation percentages.
# Map images.


carob_script <- function(path) {

"
Fertility Maps of Puente de Ixtla, Morelos 2020.

Soil sampling with 1x1 km grid in the agricultural area of the Puente de Ixtla Municipality at at 0 to 30 cm depth.
"

	uri <- "hdl:11529/10548657"
	group <- "soil_samples"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
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

	one <- function(f, sheet, swap=FALSE) {
		r <- carobiner::read.excel(f, sheet=sheet, na=c(".", "NA", ""))
		u <- trimws(as.character(unlist(r[1, ])))
		if (any(u %in% c("%", "ppm", "dS/m", "meq/100g", "gr/cm3", "g/cm3", "cm/hr", "cm/h", "cm"))) r <- r[-1, ]
		num <- function(nms) {
			hit <- nms[nms %in% names(r)]
			if (!length(hit)) return(rep(NA_real_, nrow(r)))
			x <- trimws(as.character(r[[hit[1]]]))
			# One Huanimaro aluminium cell is the unreadable token 3.33%0.
			x[x == "3.33%0"] <- NA_character_
			as.numeric(x)
		}
		tex <- tolower(trimws(as.character(r$`Clase Textural`)))
		tex[is.na(tex) | tex %in% c("", "no aplica")] <- NA_character_
		tex <- gsub("areno franco", "sandy loam", tex)
		tex <- gsub("franco arcillo arenoso", "sandy clay loam", tex)
		tex <- gsub("franco arcillo limoso", "silty clay loam", tex)
		tex <- gsub("franco arcilloso", "clay loam", tex)
		tex <- gsub("franco arenoso", "sandy loam", tex)
		tex <- gsub("franco limoso", "silty loam", tex)
		tex <- gsub("arcillo arenoso", "sandy clay", tex)
		tex <- gsub("arcillo limoso", "silty clay", tex)
		tex <- gsub("arcilloso", "clay", tex)
		tex <- gsub("arcilla", "clay", tex)
		tex <- gsub("arenoso", "sandy loam", tex)
		tex <- gsub("arena", "sand", tex)
		tex <- gsub("franco", "loam", tex)
		depth <- do.call(rbind, strsplit(as.character(r$Profundidad), "-", fixed=TRUE))
		bray <- num("Fosforo Bray 1")
		olsen <- num("Fosforo Olsen")
		place <- trimws(as.character(r$Rancho))
		place[is.na(place) | place %in% c("", "NA", ".")] <- NA_character_
		ok <- !is.na(place)
		if (any(ok)) place[ok] <- carobiner::fix_name(place[ok], "title")
		lat <- num("Latitud")
		lon <- num("Longitud")
		if (swap) {
			# This sheet's Longitud column holds latitude and its Latitud column holds longitude.
			lat <- num("Longitud")
			lon <- num("Latitud")
		}
		data.frame(
			sample_id = as.character(r$Folio),
			adm1 = carobiner::fix_name(r$Estado, "title"),
			adm2 = carobiner::fix_name(r$Municipio, "title"),
			location = place,
			latitude = lat,
			longitude = lon,
			depth_top = as.numeric(depth[, 1]),
			depth_bottom = as.numeric(depth[, 2]),
			soil_sand = num("Arena"),
			soil_clay = num("Arcilla"),
			soil_silt = num("Limo"),
			soil_texture = tex,
			soil_WHC_sat = num("Punto de Saturación"),
			soil_bd = num(c("Densidad Aparente", "DAP")),
			soil_Ksat = num("Conductividad Hidráulica"),
			soil_pH = num("pH(1:2 Agua)"),
			soil_pH_buffer = num("pH Buffer"),
			soil_SOM = num("Materia Orgánica"),
			soil_NO3 = num(c("Nitrógeno (N-NO3)", "Nitratos")),
			soil_P = ifelse(!is.na(bray), bray, olsen),
			soil_P_method = ifelse(!is.na(bray), "Bray1", ifelse(!is.na(olsen), "Olsen", NA_character_)),
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
			soil_CEC = num(c("(CIC)", "CIC")),
			soil_EC = num("CE (Extracto)"),
			geo_from_source = TRUE
		)
	}

	f <- ff[basename(ff) == "BD_Suelo_Puente de Ixtla.xlsx"]
	d <- one(f, "PUENTE DE IXTLA")
	d <- d[!is.na(d$latitude) & !is.na(d$longitude), ]
	d$is_survey <- TRUE
	d$on_farm <- TRUE

	carobiner::write_files(path, meta, d)
}
