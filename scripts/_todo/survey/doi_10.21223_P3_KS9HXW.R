# R script for "carob"
# license: GPL (>=3)

## NOTES
# Baseline of the dry-faba DOI study in Cusco, Peru, 2008. Country, department, province, district, and community are columns. The crop is faba bean.
# The sheet has two columns named NroEnc. The first is the questionnaire number.
# The dictionary codes household-member sex as 1 = Masculino and 2 = Femenino. Sex here is member 01.
# Interview dates are day-month-year (FecApl).

## ISSUES
# One district is blank. Pampa marca and Pampamarca, and Cay cay, are stored as written.
# No latitude or longitude.
# missing variables: latitude, longitude

## NOT PROCESSED
# Names, the member roster after person 01, and the second NroEnc column.
# Farm area, faba sales, association questions, and the other production blocks. The area columns are described as green faba while the product names also list other crops.
# Locality is blank for 35 of 93 rows, so the community is the location.
# Dictionary, the pdf questionnaire, and the pdf report.
# 02_Data dictionary_Caso DOI (HABA).xlsx: documentation or a codebook, not an observation table.


carob_script <- function(path) {

"
Dataset for: Desarrollo Organizacional para la Innovación” (DOI): Caso HABA - PERU, 2008.

El presente es un informe del estudio de base de la metodología “Desarrollo Organizacional para la Innovación” (DOI) que viene implementando el Instituto Nacional de Innovación Agrícola (INIA) del Cusco, en el marco del Proyecto Alianza Cambio Andino, en las Asociación de productores de haba en seco del Cusco: APHAC y CUSCA Purispa. En el marco de este proceso se implementará la metodología participativa DOI, que busca Promover la organización de pequeños productores para procesos de innovación rural participativa, promover el desarrollo de liderazgos participativos, colectivos y autogestionarios que contribuyan a fortalecer las capacidades administrativas y de gestión de las organizaciones comunitarias de los pequeños productores. Convertir los grupos de pequeños productores en asociaciones formales, y consolidarlas con proyección empresarial. Para la realización del estudio se ha aplicado técnicas e instrumentos cuantitativos y cualitativos. En el primer caso se ha utilizado un cuestionario semiestructurado de preguntas cerradas y estuvo dirigido a los jefes de familia de dos grupos de estudio: factual quienes aplicarán la metodología DOI y contrafactual quienes no aplicarán la metodología. El instrumento utilizado ha sido un cuestionario generalmente de preguntas cerradas, dirigido a productores (agricultores), de aproximadamente 76 preguntas. Se han aplicado un total de 93 cuestionarios, de los cuales 65 corresponde al grupo factual (las dos asociaciones de cusco) y 28 al grupo contrafactual (asociación wankarumi de cusco). La duración de la aplicación de cada cuestionario fue entre 30 a 60 minutos aproximadamente.
"

	uri <- "doi:10.21223/P3/KS9HXW"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 20,
		carob_effort = 0.35
	)

	f <- ff[basename(ff) == "01_DataBase Caso DOI (Haba) Cusco.xlsx"]
	r <- carobiner::read.excel(f, na=c("", "NA"))
	sex <- trimws(as.character(r[["9Sexo_01"]]))
	sex <- c("1" = "male", "2" = "female")[sex]
	when <- as.Date(trimws(as.character(r$FecApl)), "%d%m%Y")

	d <- data.frame(
		trial_id = "DOI_haba_Cusco_2008",
		hhid = trimws(as.character(r[["NroEnc...1"]])),
		country = trimws(as.character(r[["1Pais"]])),
		adm1 = carobiner::fix_name(trimws(as.character(r[["2Departamento"]])), "title"),
		adm2 = carobiner::fix_name(trimws(as.character(r[["3Provinvia"]])), "title"),
		adm3 = carobiner::fix_name(trimws(as.character(r[["4Distrito"]])), "title"),
		location = carobiner::fix_name(trimws(as.character(r[["5Comunidad"]])), "title"),
		crop = "faba bean",
		hh_size = as.numeric(r[["8NroMiembros"]]),
		sex = unname(sex),
		date = as.character(when),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
