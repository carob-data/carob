# R script for "carob"
# license: GPL (>=3)

## NOTES
# Stakeholder interviews, June to July 2020. The rows are authorities, not potato plots. Country Peru is in the title. Region, province, district, and populated center are P07a to P07d.
# Interview dates are day-month-year. 26-6-2020 and 06-07-2020 both occur. Day-month-year fits the June to July window in the description.
# Names and telephone numbers are not copied.

## ISSUES
# No plot coordinates, yield, or crop measurements.
# The description spells the department Juinin. The region column is kept as recorded.
# write_files: missing carob_contributor, latitude, longitude.

## NOT PROCESSED
# P02 Nombre de la autoridad and P05 Número de teléfono: personal identifiers.
# P03 Cargo, P03a Genero, P03b Sector, P04 Institución, P06 Código de la Encuesta: respondent and form fields.
# P08 through P16: open answers on consent, COVID effects on crops and sales, mobility, migration, aid, health, and food supply. They are not standardized measurements.
# Diccionario_Encuesta_Autoridades.xlsx and the PDF questionnaire repeat the dictionary in the workbook.


carob_script <- function(path) {

"
Dataset for: Health crisis and quarantine measures in Peru: Effects on livelihoods of potato farmers: Stakeholders survey

International Potato Center (CIP) and partners collaborated to develop strategies to improve welfare and nutrition of Andean potato producers in La Libertad, Juinin and Huancavelica. COVID-19 pandemic affected those interventions and there was a need to understand the project implication of COVID-19 measurements in production, consumption and commercialization of potatoes in those settings. A stakeholder survey from Andean potato producing areas was conducted from June to July 2020. The sample size includes 44 stakeholders located in Andean communities from 8 provinces and 3 Departments. The survey covered the four major domains: food security, labor availability, farm system resilience, and agricultural systems connectivity.
"

	uri <- "doi:10.21223/46KVFG"
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
		carob_completion = 35,
		carob_effort = 0.3
	)

	f <- ff[grepl("^Encuesta Autoridades.*\\.xlsx$", basename(ff))]
	r <- carobiner::read.excel(f, sheet="Autoridades")

	parse_dmy <- function(x) {
		x <- gsub("/", "-", trimws(as.character(x)), fixed=TRUE)
		out <- rep(NA_character_, length(x))
		for (i in seq_along(x)) {
			if (is.na(x[i]) || !nzchar(x[i])) next
			p <- strsplit(x[i], "-", fixed=TRUE)[[1]]
			if (length(p) != 3) stop("unparsed interview date: ", x[i])
			day <- as.integer(p[1])
			month <- as.integer(p[2])
			year <- as.integer(p[3])
			if (any(is.na(c(day, month, year))) || month > 12 || day > 31 || day < 1) {
				stop("unparsed interview date: ", x[i])
			}
			out[i] <- sprintf("%04d-%02d-%02d", year, month, day)
		}
		out
	}

	d <- data.frame(
		hhid = as.character(r$ID),
		date = parse_dmy(r$P01),
		country = "Peru",
		adm1 = trimws(as.character(r$P07a)),
		adm2 = trimws(as.character(r$P07b)),
		adm3 = trimws(as.character(r$P07c)),
		location = trimws(as.character(r$P07d)),
		geo_from_source = FALSE,
		is_survey = TRUE,
		on_farm = NA,
		trial_id = "stakeholders_Peru_2020"
	)
	d$location[!nzchar(d$location)] <- NA_character_
	d$adm2[!nzchar(d$adm2)] <- NA_character_
	d$adm3[!nzchar(d$adm3)] <- NA_character_

	carobiner::write_files(path, meta, d)
}
