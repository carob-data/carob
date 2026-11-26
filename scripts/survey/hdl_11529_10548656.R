# R script for "carob"

## ISSUES
# - herbicide_product contains standardized active ingredients not yet in terminag
#   (oxadiargyl, ethoxysulfuron, fluchloralin, quizalofop-ethyl); unidentifiable
#   farmer free-text entries are mapped to "unknown".
# - insecticide_product contains standardized active ingredients not yet in terminag
#   (profenofos, triflumezopyrim, triazophos, ethiprole, phosphamidon, thiodicarb);
#   unidentifiable farmer free-text entries are mapped to "unknown".
# - A small number of records have extreme mineral fertilizer rates (e.g., N_fertilizer > 600 kg/ha,
#   Zn_fertilizer > 100 kg/ha, S_fertilizer > 250 kg/ha) from raw survey plot-unit or entry outliers.
# - O.largestPlotGPS.Altitude is not mapped to elevation because raw mobile GPS altitudes are
#   uncalibrated (median 1 m across inland India, min -223 m).

carob_script <- function(path) {

"Landscape Diagnostic Survey (LDS) data contains current rice production practices of rice applied by 8,355 farmers in eight states of India. The objective of collecting this data is to bridge the existing data-gap and to generate data-based evidence that can help in evidence-based planning. The LDS is designed in a way that data is collected from randomly selected farmers spread uniformly within a KVK (government extension system) domain/district. Survey questionnaire captures all production practices applied by farmers from land preparation to harvesting, including detailed sections on rice establishment, fertilizer use, weed control and irrigation application. Data is captured through electronically enabled Open Data Kit (ODK) tool on mobile phone or tablet. (2021-12-01)"

	uri <- "hdl:11529/10548656"
	group <- "survey"

	ff <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		project="CSISA",
		publication= NA,
		data_organization = "CIMMYT;ICAR;IRRI;DRC;CU",
		data_type="survey",
		response_vars = "none",
		treatment_vars = "none",
		carob_contributor="Robert Hijmans",
		carob_date="2024-01-27",
		carob_effort = NA
	)

	f <- ff[basename(ff)=="CSISA_IND_LDS_Rice_2018_Data.csv"]
	r <- read.csv(f, na.strings=c("", "NA"))
	r <- carobiner::change_names(r, c("X", "X.1"), c("O.largestPlotGPS.Latitude", "O.largestPlotGPS.Longitude"))

	do_LCAS <- carobiner::get_function("do_LCAS", path, group)
	d <- do_LCAS(r)

	# Only one Fatehpur record (Harischandrapur II, Maldah, West Bengal) has missing GPS;
	# impute from the other observations of the same village ("Fatepur") in Harischandrapur II.
	i <- is.na(d$longitude) & d$location == "Fatehpur" & d$adm2 == "Maldah"
	d$longitude[i] <- 87.861
	d$latitude[i] <- 25.305
	d$geo_from_source[i] <- FALSE

	carobiner::write_files(path, meta, d)
}

