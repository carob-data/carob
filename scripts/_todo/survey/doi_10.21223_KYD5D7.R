# R script for "carob"
# license: GPL (>=3)

## NOTES
# Baseline CAP survey of potato-producing households in La Libertad, Peru, between 28 September and 21 October 2021. Country is Peru. The department in the abstract and the dataset description is La Libertad. The crop is potato.
# Grupo is the intervention or control assignment. The abstract counts 142 intervention households and 100 control households.

## ISSUES
# Eleven rows have no ID. They are not household records. A few of those rows have values in P54-P61, P74, and P76.
# The districts named in the abstract are not a column on the data sheet, so they are not assigned to every row.
# No latitude or longitude.
# The dictionary is a questionnaire layout, not a table of units, so the Pxx answers are not mapped.
# missing variables: carob_contributor
# missing variables: latitude, longitude
# unknown variables: survey_group_

## NOT PROCESSED
# P01 through P133, including production questions. The question text is not joined, and production is not mapped to kg/ha.
# 02_DataDictionary and the docx questionnaire.


carob_script <- function(path) {

"
Dataset for: Linea de salida de Conocimientos, Actitudes y Prácticas (CAP) sobre Deficiencia de Hierro (Anemia) en familias productoras de papa en la Sierra de La Libertad (September - October)

Through a gender-responsive pilot intervention, the project Anemia Zero is contributing to improve food security, nutrition, gender equality, and empowerment in potato-based production systems in high Andean regions of Peru (Curgos and Julcan Districts, Province Sanchez Carrion, Department of La Libertad) and to scale agricultural and nutrition innovations in coordination with the social protection programs (SPP) Juntos and Cuna Mas (both overseen by the Ministry of Development and Social Inclusion (MIDIS), as well as with other stakeholders. This survey was conducted between September 28th and October 21st, 2021 to evaluate changes in knowledge, attitude and practices corresponding to Anemia among 242 potato producing households (142 households from the intervention group and 100 households from the control group). The survey  presented 75 questions divided in the following sections: (1) Informed consent (2 questions), (A) Socioeconomic information from both parents and children (50 questions), (B) knowledge, attitude and practices corresponding to Anemia (31 questions), (C) crop production aspects (48 questions).
"

	uri <- "doi:10.21223/KYD5D7"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 25,
		carob_effort = 0.3
	)

	f <- ff[grepl("^01_DataSet", basename(ff))]
	r <- carobiner::read.excel(f, na=c("", "NA"))
	id <- trimws(as.character(r$ID))
	keep <- !is.na(id) & nzchar(id)
	r <- r[keep, ]
	id <- id[keep]

	d <- data.frame(
		trial_id = "CAP_anemia_LaLibertad_2021",
		hhid = id,
		country = "Peru",
		adm1 = "La Libertad",
		crop = "potato",
		survey_group_ = trimws(as.character(r$Grupo)),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE,
		stringsAsFactors = FALSE
	)

	carobiner::write_files(path, meta, d)
}
