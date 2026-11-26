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
# 07052019all.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# 07052019serviceprovider.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# descriptivetables.do: not read. .do file. Observation records are missing from d until this file is standardized.
# serviceprovider.do: not read. .do file. Observation records are missing from d until this file is standardized.


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
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 45,
		carob_effort = 0
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
		season = "boro",
		planting_date = "2014",
		yield_part = "grain",
		yield = as.numeric(r$yield),
		field_size = as.numeric(r$boroha),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = TRUE
	)
	# Fix obvious GPS typos falling in the Bay of Bengal (Charfashion recorded as 20.xx instead of 22.xx) and India (Birol 88.3887 vs 88.58)
	bad_cf <- which(d$location == "Charfashion" & !is.na(d$latitude) & d$latitude < 21.5)
	d$latitude[bad_cf] <- 22.10
	d$longitude[bad_cf] <- 90.71
	d$geo_from_source[bad_cf] <- FALSE
	bad_bi <- which(d$location == "Birol" & !is.na(d$longitude) & d$longitude < 88.5)
	d$latitude[bad_bi] <- 25.70
	d$longitude[bad_bi] <- 88.57
	d$geo_from_source[bad_bi] <- FALSE

	carobiner::write_files(path, meta, d)
}
