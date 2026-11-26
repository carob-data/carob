# R script for "carob"
# license: GPL (>=3)

## NOTES
# Seed-demand survey of 150 potato producers in five Ecuadorian provinces (Cañar, Carchi, Chimborazo, Cotopaxi, Tungurahua). Province centroids added from gazetteer (geo_from_source = FALSE).
# The dictionary says a3_sexo is 1 for female and 0 for male, c5_altura is meters above sea level, c8_papa is hectares of potato (stored in potato_area_), and c9_riego is 0 for yes and 1 for no.

## ISSUES
# No yield or fertilizer rates; c8_papa is potato area (ha), not yield.
# The interview sheets have no data table.

## NEW VARIABLES
# potato_area_: farmer potato area (ha).

## NOT PROCESSED
# survey_answers2 and the parcel-by-variety block on survey_answers1. Those columns are preference and plot codes, not a yield.
# X2 and X3 repeat the region and province already stored as adm1.
# Interviews.xls sheets interview1 to interview13 are empty.
# Dictionary.xlsx: the definitions used above.


carob_script <- function(path) {

"
Replication data for: Demand of high quality seed in Ecuador

The use of high quality seed is one of the current strategies to increase seed and food security in the highlands of Ecuador. Unfortunately, poor information about farmers' demand of high quality seed limits the implementation of this strategy. For this reason, this study aimed to investigate the characteristics of the demand of high quality seed in the country. To do this, 150 potato producers were surveyed using the rapid market assessment in five Ecuadorian provinces: Cañar, Carchi, Chimborazo, Cotopaxi, and Tungurahua (data in 'survey_answers1' and 'survey_answers2' sheet). Also, 13 interviews were carried out with experts that are involved in the seed value chain (data in the sheets referring to interviews and in the report associated to this study).
"

	uri <- "doi:10.21223/P3/ZP9LYT"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 40,
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "Surveys.xls"]
	r <- carobiner::read.excel(f, sheet = "survey_answers1", na=c("", "NA"))
	sex <- trimws(as.character(r$a3_sexo))
	sex <- c("1" = "female", "0" = "male")[sex]
	# dictionary: 0 = yes, 1 = no
	riego <- r$c9_riego
	irrigated <- rep(NA, nrow(r))
	irrigated[!is.na(riego) & riego == 0] <- TRUE
	irrigated[!is.na(riego) & riego == 1] <- FALSE

	adm1 <- trimws(as.character(r$c1_provin))
	geo <- data.frame(
		adm1 = c("Cañar", "Canar", "Carchi", "Chimborazo", "Cotopaxi", "Tungurahua"),
		latitude = c(-2.559, -2.559, 0.783, -1.664, -0.935, -1.254),
		longitude = c(-78.938, -78.938, -77.883, -78.654, -78.615, -78.623)
	)
	gm <- match(tolower(adm1), tolower(geo$adm1))

	d <- data.frame(
		trial_id = "Ecuador_seed_demand",
		hhid = trimws(as.character(r$X1)),
		country = "Ecuador",
		adm1 = adm1,
		adm2 = trimws(as.character(r$c2_canton)),
		adm3 = trimws(as.character(r$c3_parroq)),
		location = trimws(as.character(r$c4_comun)),
		latitude = geo$latitude[gm],
		longitude = geo$longitude[gm],
		crop = "potato",
		sex = unname(sex),
		age = as.numeric(r$a2_edad),
		elevation = as.numeric(r$c5_altura),
		potato_area_ = as.numeric(r$c8_papa),
		irrigated = irrigated,
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
