# R script for "carob"
# license: GPL (>=3)

## NOTES
# Baseline of the yam EPCP study in Sucre and Bolivar, Colombia, 2008. The crop is yam (ñame). Country, department, and municipality are columns on the survey.
# The dictionary codes household-member sex as 1 = Masculino and 2 = Femenino. Sex here is member 01, the first person listed.
# 7NroMiembros is the number of household members.

## ISSUES
# No latitude or longitude.
# missing variables: carob_contributor
# missing variables: latitude, longitude
# unknown variables: survey_group_

## NOT PROCESSED
# Names of people, the member roster after person 01, association and production questions. Area and production units are not mapped from the closed questionnaire.
# Dictionary, pdf questionnaire, and the pdf report.


carob_script <- function(path) {

"
Dataset for: Metodología del Enfoque Participativo de Cadenas Productivas (EPCP) en Colombia: Caso Ñame, 2008.

El presente informe tiene como objetivo principal describir la situación inicial de la población objetivo y control previo a la implementación de la metodología participativa 'Enfoque Participativo de Cadenas Productivas (EPCP)' ejecutado por la Corporación PBA y en el marco del proyecto de Cambio Andino, 2007-2010. El ámbito de la intervención fue la zona atlántica de Colombia, en los Municipios de Ovejas, Sampues, San Jacinto, Toluviejo, Los Palmitos, El Carmen, en los departamentos de Sucre y Bolívar. La población objetivo fueron los productores que conforman cinco asociaciones: APROS, ASOPROMAPES, ASOMUDEPAS, ASOPROYUS Y NAPOLES VENECIA y como grupo control otras asociaciones que no forman parte del proyecto del EPC. En el primer caso se aplicó un cuestionario de preguntas cerradas y fue aplicado a los jefes de familia pertenecientes a las asociaciones del grupo objetivo y a los jefes de familias de otras asociaciones del grupo control. En total se realizaron 71 encuestas al grupo objetivo y 35 al grupo control, en el año 2008.
"

	uri <- "doi:10.21223/P3_AZZZRF"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 30,
		carob_effort = 0.3
	)

	f <- ff[basename(ff) == "01_DataBase Name Colombia.xlsx"]
	r <- carobiner::read.excel(f, na=c("", "NA"))
	sex <- trimws(as.character(r[["8Sexo_01"]]))
	sex <- c("1" = "male", "2" = "female")[sex]

	d <- data.frame(
		trial_id = "EPCP_name_Colombia_2008",
		hhid = trimws(as.character(r$NroEnc)),
		country = trimws(as.character(r[["1Pais"]])),
		adm1 = trimws(as.character(r[["2Departamento"]])),
		adm2 = trimws(as.character(r[["3Municipo"]])),
		location = trimws(as.character(r[["4Correg_Vereda"]])),
		crop = "yam",
		hh_size = as.numeric(r[["7NroMiembros"]]),
		sex = unname(sex),
		survey_group_ = trimws(as.character(r$Grupo)),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE,
		stringsAsFactors = FALSE
	)

	carobiner::write_files(path, meta, d)
}
