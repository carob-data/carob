# R script for "carob"
# license: GPL (>=3)

## NOTES
# Two surveillance tables: potato psyllid (01) and purple top (02). Ecuador, from the dataset title.
# fecha is the observation date. Provincia is stored as adm1 in title case.
# papa is potato. pepino_dulce is pepino (Solanum muricatum). Other species names are kept as recorded in crop.
# incidencia is a percent in the file. It is stored as incidence_ because disease_incidence is text and pest_incidence is a count.
# Presence is the text in presencia.

## ISSUES
# No coordinates. Parish codes are not longitude or latitude.
# Landscape cover by plant family is not mapped.
# papa is potato, pepino_dulce is pepino, and tomate / Lycopersicon text is tomato. pimiento, prinontomate, tomate arbol, and uvilla are kept as recorded and are not vocabulary crop names.
# Bactericera cockerelli is not in the pest list. The purple-top diagnosis is recorded as punta morada, which is not in the disease list.
# No yield or fertilizer. These are surveillance records.
# write_files: missing variables: carob_contributor; yield, yield_moisture, yield_part, geo_from_source, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.
# write_files: unknown variable: incidence_ (pest_incidence?); unknown variables: presence_.
# write_files: invalid terms: crop: pimiento, prinontomate, tomate arbol, uvilla; pest_species: Bactericera cockerelli; disease: punta morada.

## NOT PROCESSED
# Family cover columns (fabaceae through linaceae), superficie_can_total, and num_superficietotal_par. Parish land-cover composition, not a plot measurement.
# ual_prov, ual_cant, ual_parr, DPA_PARROQ: administrative codes. The province name is kept.
# year: the calendar year of fecha.

## NEW VARIABLES
# incidence_: recorded incidence, percent
# presence_: presence text as recorded

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
		response_vars = "incidence_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.45
	)

	one <- function(f, stress) {
		r <- carobiner::read.excel(f)
		sp <- tolower(trimws(as.character(r[["Especie vegetal"]])))
		crop <- gsub("_", " ", sp, fixed=TRUE)
		crop[sp == "papa"] <- "potato"
		crop[sp == "pepino_dulce"] <- "pepino"
		crop[crop == "tomate" | grepl("lycopersicon", crop, fixed=TRUE)] <- "tomato"
		diag <- trimws(as.character(r$diagnostico))
		diag <- ifelse(tolower(diag) == "bactericera cockerelli", "Bactericera cockerelli", diag)
		data.frame(
			country = "Ecuador",
			adm1 = tools::toTitleCase(tolower(trimws(as.character(r$nameprovincia)))),
			elevation = as.numeric(r$altitude),
			date = as.character(as.Date(r$fecha)),
			crop = crop,
			pest_species = if (stress == "pests") diag else NA_character_,
			disease = if (stress == "disease") diag else NA_character_,
			stress = stress,
			incidence_ = as.numeric(r$incidencia),
			presence_ = trimws(as.character(r$presencia)),
			trial_id = if (stress == "pests") "psyllid" else "purple_top",
			on_farm = TRUE,
			is_survey = TRUE,
			irrigated = NA
		)
	}

	f1 <- ff[basename(ff) == "01_Data_Surveillance_of_the_potato_psyllid.xlsx"]
	f2 <- ff[basename(ff) == "02_Data_Surveillance_pmp.xlsx"]
	d <- carobiner::bindr(one(f1, "pests"), one(f2, "disease"))
	d$plot_id <- as.character(seq_len(nrow(d)))

	carobiner::write_files(path, meta, d)
}
