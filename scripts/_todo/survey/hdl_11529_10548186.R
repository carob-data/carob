# R script for "carob"
# license: GPL (>=3)

## NOTES
# Farmer interviews on boro rice and irrigation-service payments in Bangladesh. The variable label says yield is boro yield, kg/ha, and boroha is the boro field in ha cultivated in 2013/14.
# Country is Bangladesh in the title and in geographicCoverage. Division, district, and subdistrict are divi, district, and subdistrict.
# Household coordinates are in the file. geo_from_source is TRUE.
# The service-provider file is a business interview and is not used.

## ISSUES
# high crop yield: rice: 34305.5546875
# coordinates not on land: Bangladesh
# coordinates in wrong country: Bangladesh/India, Bangladesh/India

## NOT PROCESSED
# Irrigation prices, pump ownership, and the reasons farmers give for choosing a provider.
# Service-provider interviews.
# landpha and landserha, which describe the pump owner's command area rather than the farmer's boro field.


carob_script <- function(path) {

"
Understanding clients, providers and the institutional dimensions of irrigation services in developing countries: A study of water markets in Bangladesh

Boro rice yield reported by farmers who buy irrigation from water-market service providers.
"

	uri <- "hdl:11529/10548186"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=5,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "07052019clientfarmer.dta"]
	r <- carobiner::read.dta(f)

	d <- data.frame(
		country = "Bangladesh",
		adm1 = as.character(r$divi),
		adm2 = as.character(r$district),
		location = as.character(r$subdistrict),
		latitude = as.numeric(r$hh_latitud),
		longitude = as.numeric(r$hh_longitu),
		hhid = as.character(r$hh_id),
		trial_id = as.character(r$hh_id),
		crop = "rice",
		yield_part = "grain",
		yield = as.numeric(r$yield),
		field_size = as.numeric(r$boroha),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = TRUE
	)

	carobiner::write_files(path, meta, d)
}
