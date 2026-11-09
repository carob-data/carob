# R script for "carob"
# license: GPL (>=3)

## NOTES
# One row per plot in ML_97_parcelles.xlsx for 1997-98. Yield is Rendement/ha, which the variable list defines as kg/ha. Plot area is stored as hectares because the values sit in the same range as farm area, which the variable list gives in ha.
# F.O is organic manure kg/ha. A zero is none. Cotton and cereal fertilizer columns are product kg/ha. Their nutrient grades are not in the variable list, so elemental N, P, and K are not computed.
# Country is Mali from geographicCoverage. adm1 Sikasso is the geographicCoverage state "Région de Sikasso". Villages are Dampéla, Danderesso, Kodougouni, and Nténa. No coordinates are in the open files.

## ISSUES
# voandzou and pois de terre are both stored as bambara groundnut. mil is stored as millet. patate is stored as sweetpotato.
# One plot has no crop, and some plots have no Rendement/ha. Those cells are kept.
# write_files: missing geo_from_source, latitude, longitude. unknown variables: cotton_fertilizer_kg_ha_, cereal_fertilizer_kg_ha_. NA detected: yield, yield_part.

## NOT PROCESSED
# ML_97_Struc.xlsx: farm population, equipment, and farm-level mean yields. Plot yield is already on the plot sheet.
# Previous crops, individual weeding and spray dates, manure application units, and harvest plant density (no unit in the variable list).
# Quantité urée, super-simple, and the complex-fertilizer quantities. The per-hectare FM Coton, FM Céréale, and F.O columns are the ones with a stated kg/ha unit.
# List_variables.xlsx: not read. sheet Feuil1: Variables, Signification, précision, Meaning. Observation records are missing from d until this file is standardized.
# Readme.txt: documentation or a codebook, not an observation table.


carob_script <- function(path) {

"
Exploitations cotonnières et pratiques culturales dans quatre villages de la zone CMDT au Mali, campagne 1997-98

Données issues du suivi de 55 exploitations dans quatre villages pour cerner leurs caractéristiques structurelles et connaître les opérations culturales réalisées dans toutes les parcelles des exploitations, soit une dizaine en moyenne, portant du coton et les cultures venant en rotation. Les informations collectées ont correspondu aux dates et aux modes de réalisation des opérations, aux quantités d'intrants utilisées et aux productions obtenues
"

	uri <- "doi:10.18167/DVN1/PKCW2S"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "yield",
		carob_LLM = "Cursor Grok 4.7",
		carob_date = "2026-10-31",
		carob_completion = 40,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "ML_97_parcelles.xlsx"]
	r <- carobiner::read.excel(f, sheet="BaseParcel")

	crop_map <- c(
		coton="cotton", riz="rice", sorgho="sorghum", "maïs"="maize",
		"niébé"="cowpea", mil="millet", voandzou="bambara groundnut",
		arachide="groundnut", patate="sweetpotato", igname="yam",
		sesame="sesame", "pois de terre"="bambara groundnut",
		"pastèques"="watermelon", fonio="fonio"
	)
	part_map <- c(
		cotton="seed", rice="grain", sorghum="grain", maize="grain",
		cowpea="seed", millet="grain", "bambara groundnut"="seed",
		groundnut="seed", sweetpotato="roots", yam="tubers",
		sesame="seed", watermelon="fruit", fonio="seed"
	)
	crop <- unname(crop_map[trimws(as.character(r$Culture))])
	part <- unname(part_map[crop])
	om <- as.numeric(r[["F.O"]])
	herb <- r[["Herbicidage?"]]
	sow <- r[["Date semis 1...10"]]

	d <- data.frame(
		trial_id = trimws(as.character(r[["Clé parcelle"]])),
		hhid = trimws(as.character(r$Exploitation)),
		country = "Mali",
		adm1 = "Sikasso",
		location = trimws(as.character(r$Village)),
		crop = crop,
		yield = as.numeric(r[["Rendement/ha"]]),
		yield_part = part,
		field_size = as.numeric(r$Surface),
		soil_type = trimws(as.character(r[["type sol"]])),
		planting_date = as.character(as.Date(sow, tz="UTC")),
		OM_amount = om,
		OM_used = om > 0,
		OM_type = ifelse(om > 0, "farmyard manure", "none"),
		cotton_fertilizer_kg_ha_ = as.numeric(r[["FM Coton"]]),
		cereal_fertilizer_kg_ha_ = as.numeric(r[["FM Céréale"]]),
		herbicide_used = as.integer(herb) == 1,
		insecticide_times = as.integer(r[["Nombre traitements"]]),
		weeding_times = as.integer(r[["Nombre sarclage...11"]]),
		date = "1997",
		on_farm = TRUE,
		is_survey = TRUE
	)
	d$herbicide_used[is.na(herb)] <- NA
	d$OM_type[is.na(om)] <- NA_character_
	d$OM_used[is.na(om)] <- NA

	carobiner::write_files(path, meta, d)
}
