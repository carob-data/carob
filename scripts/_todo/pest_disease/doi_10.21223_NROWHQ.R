# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 18 records in 01_Data_Surveillance_of_the_potato_psyllid.xlsx have no province name or parish code (DPA_PARROQ = NA) and therefore lack coordinates (45 other rows missing nameprovincia were recovered from the first two digits of DPA_PARROQ).
# Row 951 of 01_Data_Surveillance_of_the_potato_psyllid.xlsx (tree tomato, missing parish code) records altitude = 0 m, which is likely a missing GPS elevation; kept as recorded.
# Coordinates are province-level (GADM adm1) centroids; finer parish-level georeferencing is possible from the 6-digit Ecuador INEC DPA_PARROQ codes if a parish lookup table is added.

## NEW
# pest_incidence_pct_: percentage of plants/field with potato psyllid (Bactericera cockerelli) incidence (%).
# crop: tree tomato (Solanum betaceum, raw: tomate_arbol), cape gooseberry (Physalis peruviana, raw: uvilla).
# pest_species: Bactericera cockerelli.
# disease: potato purple top (raw: punta morada).

## NOTES
# Surveillance records from Agrocalidad (with CIP and Technical University of Cotopaxi) in Ecuador (2018-2021) for potato psyllid (n = 951) and potato purple top (n = 309) across six Solanaceae host crops.
# Crop species are mapped using 03_Data_dictionary_Surveillance_of_the_potato.xlsx (prinontomate -> tomato, pimiento -> bell pepper, papa -> potato, pepino_dulce -> pepino, tomate_arbol -> tree tomato, uvilla -> cape gooseberry).
# Purple top dates in 02_Data_Surveillance_pmp.xlsx are stored as Excel serial numbers and converted with origin = "1899-12-30".

## NOT PROCESSED
# 01 and 02 xlsx: 38 parish-level botanical family land-cover percentages (fabaceae through linaceae), superficie_can_total, and num_superficietotal_par (parish-level agricultural census landscape covariates, not field measurements).
# 01 and 02 xlsx: presencia (presence/absence; 100% redundant with incidencia > 0 across all 1,260 rows), year (redundant with fecha), and raw administrative codes (DPA_PARROQ, ual_prov, ual_cant, ual_parr; province extracted into adm1).


carob_script <- function(path) {

"
Dataset for: Surveillance of the potato psyllid and purple top associated to the agricultural landscape in Ecuador from 2018 to 2021

The psyllid (Bactericera cockerelli) and purple top are serious productivity concerns for the potato production in Ecuador. Unfortunately, it is still unknown the drivers that facilitate or limit the presence of the psyllid or the purple top in the farmers fields. To contribute to this gap of knowledge, we constructed a dataset with: (1) the surveillance information provided by Agrocalidad since 2018 until 2021 and (2) the surveys of agricultural area and productivity of Ecuador from 2018 until 2021. In this file, you can find two databases. The 'surveillance_psyllid' sheet refers to the surveillance of potato psyllid and the agricultural landscape composition present during the moment of data collection with 951 observations. The 'surveillance_pmp' sheet contains information of purple top and the agricultural landscape composition present during the moment of data collection with 309 observations. It is expected that this information contributes to understand better the drivers that affect the presence of the potato psyllid and purple top in the  Andes, and support the decision making to manage the potato psyllid and purple top.
"

	uri <- "doi:10.21223/NROWHQ"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "disease_incidence;pest_incidence_pct_",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 55,
		carob_effort = 0
	)

	# Ecuador INEC 2-digit province codes (first two digits of 6-digit DPA_PARROQ)
	prov_codes <- c(
		"01" = "Azuay",
		"02" = "Bolívar",
		"03" = "Cañar",
		"04" = "Carchi",
		"05" = "Cotopaxi",
		"06" = "Chimborazo",
		"08" = "Esmeraldas",
		"10" = "Imbabura",
		"11" = "Loja",
		"17" = "Pichincha",
		"18" = "Tungurahua",
		"21" = "Sucumbíos"
	)

	crop_map <- c(
		"papa" = "potato",
		"pepino_dulce" = "pepino",
		"pimiento" = "bell pepper",
		"prinontomate" = "tomato",
		"tomate (lycopersicon esculentum)" = "tomato",
		"tomate_arbol" = "tree tomato",
		"uvilla" = "cape gooseberry"
	)

	one <- function(f, stress) {
		r <- carobiner::read.excel(f, na=c("", "NA"))

		sp <- tolower(trimws(as.character(r[["Especie vegetal"]])))
		crop <- unname(crop_map[sp])
		if (anyNA(crop)) stop("unmapped crop: ", paste(unique(sp[is.na(crop)]), collapse=", "))

		# Recover missing province names from the first two digits of DPA_PARROQ
		dpa <- ifelse(is.na(r$DPA_PARROQ), NA_character_, sprintf("%06d", as.integer(r$DPA_PARROQ)))
		pcode <- ifelse(is.na(dpa), NA_character_, substr(dpa, 1, 2))
		prov <- unname(prov_codes[pcode])
		if (any(!is.na(pcode) & is.na(prov))) {
			stop("unmapped Ecuador province code: ", paste(unique(pcode[!is.na(pcode) & is.na(prov)]), collapse=", "))
		}

		# 01 has POSIXct dates; 02 has numeric Excel serial dates
		dt <- if (is.numeric(r$fecha)) {
			as.character(as.Date(r$fecha, origin="1899-12-30"))
		} else {
			as.character(as.Date(r$fecha))
		}

		inc <- as.numeric(r$incidencia)
		data.frame(
			country = "Ecuador",
			adm1 = prov,
			elevation = as.numeric(r$altitude),
			date = dt,
			crop = crop,
			pest_species = if (stress == "pests") "Bactericera cockerelli" else NA_character_,
			pest_incidence_pct_ = if (stress == "pests") inc else NA_real_,
			disease = if (stress == "disease") "potato purple top" else NA_character_,
			# % incidence of potato purple top in the surveyed field
			disease_incidence = if (stress == "disease") as.character(inc) else NA_character_,
			stress = stress,
			on_farm = TRUE,
			is_survey = TRUE
		)
	}

	f1 <- ff[basename(ff) == "01_Data_Surveillance_of_the_potato_psyllid.xlsx"]
	f2 <- ff[basename(ff) == "02_Data_Surveillance_pmp.xlsx"]
	d <- carobiner::bindr(one(f1, "pests"), one(f2, "disease"))
	d$trial_id <- as.character(seq_len(nrow(d)))

	# Province (GADM 4.1 adm1) centroids and approximate bounding-radius uncertainties (m)
	geo <- data.frame(
		adm1 = c("Azuay", "Bolívar", "Cañar", "Carchi", "Chimborazo", "Cotopaxi", "Esmeraldas", "Imbabura", "Loja", "Pichincha", "Sucumbíos", "Tungurahua"),
		longitude = c(-79.1542, -79.1069, -79.0304, -78.0759, -78.7208, -78.8565, -79.3500, -78.3709, -79.6559, -78.5936, -76.8500, -78.5071),
		latitude = c(-3.0141, -1.5872, -2.5324, 0.7702, -1.9750, -0.8580, 0.7800, 0.4054, -4.0901, -0.1049, 0.0500, -1.2944),
		geo_uncertainty = c(92116, 63004, 55137, 67254, 75052, 61695, 110000, 100962, 96226, 93783, 140000, 48740),
		geo_source = "GADM 4.1, adm1"
	)
	d <- merge(d, geo, by="adm1", all.x=TRUE, sort=FALSE)
	d$geo_from_source <- ifelse(is.na(d$longitude), NA, FALSE)

	carobiner::write_files(path, meta, d)
}
