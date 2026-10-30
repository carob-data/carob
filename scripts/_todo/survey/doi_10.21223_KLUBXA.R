# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rows are Encuesta Productores - La Libertad - Junin - Huancavelica - COVID-19.xlsx.
# The dictionary maps P_01 to the interview date and P_03 to gender (Masculino, Femenino), stored as male/female.
# Other P_ columns are mapped only when the dictionary description contains distrito, provincia, departamento, latitud, or longitud.
# Country Peru is in the dataset description. Respondent names are not copied.

## ISSUES
# No yield in the columns mapped here.
# write_files: missing variables: carob_contributor; geo_from_source, latitude, longitude.

## NOT PROCESSED
# Questionnaire items whose dictionary description is not a place, a coordinate, the interview date, or gender.

carob_script <- function(path) {

"
Dataset for: Health crisis and quarantine measures in Peru: Effects on livelihoods of potato farmers: Potato producers survey

International Potato Center (CIP) and partners collaborated to develop strategies to improve welfare and nutrition of Andean potato producers in La Libertad, Juinin and Huancavelica. COVID-19 pandemic affected those interventions and there was a need to understand the project implication of COVID-19 measurements in production, consumption and commercialization of potatoes in those settings. A representative household survey  to Andean potato producers was conducted from June to July 2020. The sample size includes 214 households located in Andean communities from 8 provinces and 3 Departments.  The survey covered the four major domains: food security, labor availability, farm system resilience, and agricultural systems connectivity.
"

	uri <- "doi:10.21223/KLUBXA"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 20,
		carob_effort = 0.35
	)

	f <- ff[grepl("^Encuesta", basename(ff))]
	fd <- ff[grepl("^Diccionario", basename(ff))]
	r <- carobiner::read.excel(f)
	dict <- carobiner::read.excel(fd)
	desc <- tolower(as.character(dict$Description))
	vname <- trimws(as.character(dict[["Variable name"]]))
	col_for <- function(pattern) {
		i <- grep(pattern, desc)[1]
		if (is.na(i)) return(NA_character_)
		vname[i]
	}
	sex <- tolower(trimws(as.character(r[[col_for("genero")]])))
	sex[sex == "masculino"] <- "male"
	sex[sex == "femenino"] <- "female"
	dt <- r[[col_for("fecha")]]
	if (inherits(dt, "Date") || inherits(dt, "POSIXt")) {
		date <- as.character(as.Date(dt))
	} else {
		date <- trimws(as.character(dt))
	}
	take <- function(pattern) {
		cn <- col_for(pattern)
		if (is.na(cn) || !(cn %in% names(r))) return(NULL)
		trimws(as.character(r[[cn]]))
	}
	d <- data.frame(
		hhid = trimws(as.character(r$ID)),
		country = "Peru",
		date = date,
		sex = sex,
		on_farm = TRUE,
		is_survey = TRUE,
		irrigated = NA,
		trial_id = "COVID_producers",
		stringsAsFactors = FALSE
	)
	adm <- take("departamento")
	if (!is.null(adm)) d$adm1 <- adm
	prov <- take("provincia")
	if (!is.null(prov)) d$adm2 <- prov
	dist <- take("distrito")
	if (!is.null(dist)) d$adm3 <- dist
	lat <- take("latitud")
	lon <- take("longitud")
	if (!is.null(lat) && !is.null(lon)) {
		d$latitude <- as.numeric(lat)
		d$longitude <- as.numeric(lon)
		d$geo_from_source <- TRUE
	}
	carobiner::write_files(path, meta, d)
}
