# R script for "carob"
# license: GPL (>=3)


## NOTES
# English xlsx is the same 6338 records as the Spanish file; only the Spanish file is read.
# Sand/clay/silt and Bouyoucos columns are complementary (one overlapping row); Bouyoucos used when the other is NA.
# Bray and Olsen P of 0 are treated as missing.
# Coordinates truncated to 2 decimals (~1.2 km); geo_uncertainty set accordingly.
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

	carobiner::write_files(path, meta, d)
}
