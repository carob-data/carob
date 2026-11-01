# R script for "carob"
# license: GPL (>=3)

## NOTES
# English xlsx is the same 6338 records as the Spanish file; only the Spanish file is read.
# Sand/clay/silt and Bouyoucos columns are complementary (one overlapping row); Bouyoucos used when the other is NA.
# Bray and Olsen P of 0 are treated as missing.
# Coordinates in this file are truncated to 2 decimals (~1.2 km); geo_uncertainty is 1200.
# Ten municipal fertility-map workbooks that repeat these samples are read below.
# A map coordinate is copied when it matches this file at 2 decimals and has more than two decimals.
# Those rows are from Salvatierra (hdl:11529/10548663) and get geo_uncertainty 1.
# State-level subsets of this dataset are in scripts/_rejected/soil_samples/.


carob_script <- function(path) {

"
Georeferenced soil analysis results for farmers fields in Mexico 2016 to 2026

The file contains the coordinates for 6338 soil samples taken from 2016 to 2026 in farmer fields in Mexico with the corresponding soil analysis data. These soil samples were analyzed at Fertilab (www.fertilab.com.mx).  Coordinates are limited to two digits behind the point. In case that full precision GPS data is needed for research please contact CIMMYT.
"

	uri <- "doi:10.71682/10549439"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_contributor = "Robert Hijmans",
		carob_LLM = "Cursor Grok 4.6",
		carob_date = "2026-10-15",
		carob_completion = 90,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "Coordenadas_Sitios_de_muestreo_de_suelos_CIMMYT_2016_2026_COMPLETO.xlsx"]
	# text: Municipio, Estado, Cul_Est, Prof_Suelo, Cul_Ant, Meta_Rend, Clase_Tex
	ct <- rep("numeric", 53)
	ct[c(4:9, 13)] <- "text"
	r <- carobiner::read.excel(f, na = c("-9999", "N/A", "NA", "NC", "SIN DATO", "Sin dato", "No proporciono"), col_types = ct)

	# not processed: Meta_Rend (expected yield, not measured); Anio (duplicate of Año);
	#   Ca_pSat, Mg_pSat, K_pSat, Na_pSat, Al_pSat, H_pSat (cation saturation % = 100 * exch / CEC)

	# Limon mapped to lime. Nogal (Nuez) mapped to pecan.
	crop_from <- c(
		"Ninguno", "Maiz", "Cebada", "Sorgo Grano", "Frijol",
		"Trigo Harinero", "Trigo Duro", "Trigo", "Agave", "Aguacate",
		"Nogal (Nuez)", "Chile Jalapeno", "Chile Guajillo", "Chile Ancho",
		"Chile Serrano", "Chile", "Maiz Forrajero", "Avena", "Ajo",
		"Tomate de Cascara", "Cebolla", "Acelga", "Alfalfa",
		"Avena Forrajera", "Calabaza", "Durazno", "Zanahoria",
		"Cana de Azucar", "Girasol", "Limon", "Garbanzo", "Tuna",
		"Cacahuate", "Fresa", "Granada", "Melon", "Sorgo Forrajero",
		"Triticale", "Ajonjoli", "Arandano", "Cilantro", "Citricos",
		"Jitomate", "Lechuga Campo", "Maiz Dulce", "Pradera",
		"Tomate Determinado", "Vid", "Macadamia", "Brocoli",
		"Naranja", "Rabano", "Maiz - frijol"
	)
	crop_to <- c(
		"none", "maize", "barley", "sorghum", "common bean",
		"wheat", "durum wheat", "wheat", "agave", "avocado",
		"pecan", "chili pepper", "chili pepper", "chili pepper",
		"chili pepper", "chili pepper", "forage maize", "oats", "garlic",
		"tomatillo", "onion", "chard", "lucerne",
		"oats", "squash", "peach", "carrot",
		"sugarcane", "sunflower", "lime", "chickpea", "nopal",
		"groundnut", "strawberry", "pomegranate", "melon", "sorghum",
		"triticale", "sesame", "blueberry", "coriander", "citrus",
		"tomato", "lettuce", "maize", "pasture",
		"tomato", "grape", "macadamia nut", "broccoli",
		"orange", "radish", "maize_common bean"
	)

	bray <- r$P_Bray1
	bray[bray == 0] <- NA
	olsen <- r$P_Olsen
	olsen[olsen == 0] <- NA
	soil_P <- bray
	soil_P[is.na(soil_P)] <- olsen[is.na(soil_P)]
	soil_P_method <- ifelse(!is.na(bray), "Bray-1", ifelse(!is.na(olsen), "Olsen", NA))

	# complementary texture methods
	soil_sand <- r$Arena
	soil_clay <- r$Arcilla
	soil_silt <- r$Limo
	i <- is.na(soil_sand)
	soil_sand[i] <- r$Arena_Bouy[i]
	soil_clay[i] <- r$Arcilla_Bo[i]
	soil_silt[i] <- r$Limo_Bouy[i]

	depth <- r$Prof_Suelo
	d <- data.frame(
		country = "Mexico",
		adm1 = r$Estado,
		adm2 = carobiner::fix_name(r$Municipio, "title"),
		longitude = r$Longitud,
		latitude = r$Latitud,
		geo_from_source = TRUE,
		geo_uncertainty = 1200,
		date = as.character(as.integer(r$Año)),
		crop = carobiner::replace_values(r$Cul_Est, crop_from, crop_to, must_have = FALSE),
		previous_crop = carobiner::replace_values(r$Cul_Ant, crop_from, crop_to, must_have = FALSE),
		depth_top = as.numeric(sub("-.*", "", depth)),
		depth_bottom = as.numeric(sub(".*-", "", depth)),
		soil_sand = soil_sand,
		soil_clay = soil_clay,
		soil_silt = soil_silt,
		soil_texture = tolower(trimws(r$Clase_Tex)),
		soil_SP = r$Punto_Sat,
		soil_bd = r$Dens_Apar,
		soil_Ksat = r$CH,
		soil_pH = r$pH_12agua,
		soil_pH_buffer = r$pH_Buffer,
		soil_CaCO3 = r$Carbonatos * 10,
		soil_SOM = r$Mat_Org,
		soil_NO3 = r$Nitratos,
		soil_P = soil_P,
		soil_P_method = soil_P_method,
		soil_K = r$Potasio,
		soil_Ca = r$Calcio,
		soil_Mg = r$Magnesio,
		soil_Na = r$Sodio,
		soil_Fe = r$Hierro,
		soil_Zn = r$Zinc,
		soil_Mn = r$Manganeso,
		soil_Cu = r$Cobre,
		soil_B = r$Boro,
		# soil_H = r$Hidrogeno, #redudant with soil_H_exch
		soil_Al = r$Aluminio,
		soil_S = r$Azufre,
		soil_Ca_exch = r$Ca_Meq100,
		soil_Mg_exch = r$Mg_Meq100,
		soil_K_exch = r$K_Meq100,
		soil_Na_exch = r$Na_Meq100,
		soil_Al_exch = r$Al_Meq100,
		soil_H_exch = r$H_Meq100,
		soil_CEC = r$CIC,
		soil_EC = r$CE_Ext,
		on_farm = TRUE,
		is_survey = TRUE
	)

	d$adm1 <- gsub("^Mexico$", "México", d$adm1)
	d$adm1 <- gsub("Michoacan de Ocampo", "Michoacán", d$adm1)
	d$adm1 <- gsub("Queretaro", "Querétaro", d$adm1)
	d$adm1 <- gsub("Yucatan", "Yucatán", d$adm1)

	d$soil_texture <- carobiner::replace_values(d$soil_texture,
		c("franco arcillo arenoso", "franco arcilloso", "franco arenoso",
			"franco limoso", "arcillo arenoso", "areno franco", "arcilloso",
			"arcilla", "arena", "franco", "fino", "gruesa", "medio"),
		c("sandy clay loam", "clay loam", "sandy loam",
			"silty loam", "sandy clay", "loamy sand", "clay",
			"clay", "sand", "loam", "fine", "coarse", "medium"),
		must_have = FALSE
	)

	# Same samples, sometimes with a finer coordinate. Match at 2 decimals
	# on the coordinate and on sand, clay, silt, pH, organic matter, and K.
	maps <- rbind(
		c("hdl:11529/10548653", "BD_Suelo_Yautepec.xlsx", "Datos Yautepec"),
		c("hdl:11529/10548658", "BD_Suelo_Jojutla.xlsx", "Datos "),
		c("hdl:11529/10548655", "BD_Suelo_Tlaltizapan.xlsx", "Resultados"),
		c("hdl:11529/10548657", "BD_Suelo_Puente de Ixtla.xlsx", "PUENTE DE IXTLA"),
		c("hdl:11529/10548751", "BD_Suelo_Muna.xlsx", "Muna"),
		c("hdl:11529/10548661", "BD_Suelo_Yuriria.xls", "Base de datos"),
		c("hdl:11529/10548662", "BD_Suelo_Salamanca.xls", "Salamanca"),
		c("hdl:11529/10548663", "BD_Suelo_Salvatierra.xlsx", "Matriz 2022"),
		c("hdl:11529/10548666", "BD_Suelo_Manuel Doblado.xlsx", "M. Doblado"),
		c("hdl:11529/10548667", "BD_Suelo_La Piedad.xls", "Suelos")
	)
	as_num <- function(x) {
		x <- gsub("°", "", as.character(x), fixed=TRUE)
		x <- trimws(x)
		x[x %in% c("%", "ppm", "No aplica", "-", ".", "NA", "")] <- NA_character_
		as.numeric(x)
	}
	col1 <- function(x, pat) {
		j <- grep(pat, names(x), ignore.case=TRUE)
		if (!length(j)) return(rep(NA_real_, nrow(x)))
		as_num(x[[j[1]]])
	}
	rnd2 <- function(x) round(x, 2)
	key_of <- function(lat, lon, sand, clay, silt, ph, om, k) {
		paste(rnd2(lat), rnd2(lon), rnd2(sand), rnd2(clay), rnd2(silt), rnd2(ph), rnd2(om), rnd2(k))
	}
	ckey <- key_of(r$Latitud, r$Longitud, r$Arena, r$Arcilla, r$Limo, r$pH_12agua, r$Mat_Org, r$Potasio)
	for (i in seq_len(nrow(maps))) {
		ffm <- carobiner::get_data(maps[i, 1], path, group)
		fm <- ffm[basename(ffm) == maps[i, 2]]
		m <- carobiner::read.excel(fm, sheet=maps[i, 3])
		lat <- col1(m, "^Latitud$|^Latitd$")
		lon <- col1(m, "^Longitud$")
		finer <- !is.na(lat) & !is.na(lon) & (abs(lat - rnd2(lat)) > 1e-6 | abs(lon - rnd2(lon)) > 1e-6)
		mk <- key_of(lat, lon, col1(m, "^Arena"), col1(m, "^Arcilla"), col1(m, "^Limo"), col1(m, "pH"), col1(m, "Materia"), col1(m, "^Potasio"))
		keep <- finer & !duplicated(mk) & !duplicated(mk, fromLast=TRUE)
		j <- match(ckey, mk)
		use <- !is.na(j) & keep[j]
		d$latitude[use] <- lat[j[use]]
		d$longitude[use] <- lon[j[use]]
		d$geo_uncertainty[use] <- 1
	}

	carobiner::write_files(path, meta, d)
}
