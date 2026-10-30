# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The previous draft assigned coordinates that are not in the workbook and set
# geo_from_source to FALSE: Chugay -7.78167, -77.8683 and Curgos -7.75000, -77.8500.
# Hemoglobin and anemia columns are not yet standard vocabulary names.
# Dates mix text (d-m-y) and Excel serial numbers.


carob_script <- function(path) {

"
Dataset for: Efecto de una barra nutricional NUTRIBARRA ANDINA en el control de la anemia en niños menores de 5 años en Chugay y Curgos, La Libertad, Peru.
"

	uri <- "doi:10.21223/OMOIO4"
	group <- "nutrition"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "treatment",
		response_vars = "",
		carob_completion = 20,
		carob_effort = 0,
		carob_contributor = "Your Name",
		carob_date = "2026-10-30",
		notes = "",
		
	)

	r <- carobiner::read.excel(ff[basename(ff) == "01_Datos_NUTRI_BARRAS.xlsx"])

	date_initial <- ifelse(grepl("-", r[["Fecha-tamizaje-inicial"]]),
		as.character(as.Date(r[["Fecha-tamizaje-inicial"]], format = "%d-%m-%y")),
		as.character(as.Date(suppressWarnings(as.numeric(r[["Fecha-tamizaje-inicial"]])), origin = "1899-12-30")))

	d <- data.frame(
		trial_id = "OMOIO4",
		hhid = r$Codigo,
		treatment = tolower(r$Grupo),
		farmer_gender = ifelse(r$Sexo == "M", "male", "female"),
		location = r$Distrito,
		adm2 = r$Provincia,
		country = "Peru",
		is_survey = TRUE,
		on_farm = FALSE,
		date_initial_ = date_initial,
		hb_initial_ = as.numeric(r[["HB-Tamizado1"]]),
		hb_month1_ = as.numeric(r[["HB-Tamizado2"]]),
		hb_month2_ = as.numeric(r[["HB-Tamizado-3"]]),
		hb_month3_ = as.numeric(r[["HB-Tamizado-4"]]),
		anemia_initial_ = r$Nivel_Anemia,
		anemia_month1_ = r$Nivel_Anemia_Tamizado2,
		anemia_month2_ = r$Nivel_Anemia_Tamizado3,
		anemia_month3_ = r$Nivel_Anemia_Tamizado4
	)

	carobiner::write_files(path, meta, d)
}
