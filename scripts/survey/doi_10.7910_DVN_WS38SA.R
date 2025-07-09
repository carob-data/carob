# R script for "carob"

## ISSUES

carob_script <- function(path) { 
  
"Here we release the Rural Household Multi-Indicator Survey (RHoMIS) dataset, derived from the open-source RHoMIS toolkit. RHoMIS is designed to simplify the collection, analysis, and dissemination of interoperable data from farm-household surveys. This release harmonizes 54,873 household observations spanning 35 countries in Latin America, Africa, and Asia into a single dataset, collected between 2015 and 2023. The data supports the investigation of system dynamics relating to food production, food security, and pathways out of poverty for smallholder farmers. This dataset is an update and revision of the previous RHoMIS dataset, published in 2020. In this new release we present a database quadruple the size, with observations of 54,873 households from 35 countries and 119 unique projects, collected from 2015 until 2023. These data are accompanied with radically improved analytical routines to process the data and calculate common performance indicators. We report 1599 variables and 41 farm household level indicators, as well as crop-level information (production, area planted, use of the produce, and sale value). We also report gendered decision-making information over the consumption of produced foodstuffs and over the incomes derived from on- and off-farm activities. (2025-01-28)"

	uri <- "doi:10.7910/DVN/WS38SA"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group, recursive=TRUE)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		project="RHoMIS",
		publication= "doi:10.1038/s41597-020-0388-8",
		data_organization = "UBR;ILRI;FRB;WUR;CIMMYT;CIRAD;ABC;TAID;ABIO;USM;SNV;IER;UED;ICRAF;COLSU;FVAL;CU;RUA;UCBU;SLU;IITA;UFS;ROTH;YANAPAI;STATS4SD;CIP;BAU;GIZ;KALRO;OAF;TUAF;ZALF",
		data_type="survey", 
		response_vars = "none",
		treatment_vars = "none",
		carob_contributor="Robert Hijmans",
		carob_date="2025-05-30",
		completion = 1  #1%, a lot more to be done
	)
  
  
	f1 <- ff[basename(ff) == "full_survey_data.csv"]
	f2 <- ff[basename(ff) == "indicator_data.csv"]
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r <- merge(r1, r2, by="id_unique")
	
	d <- data.frame(
		trial_id=as.character(NA),
		planting_date=as.character(NA),
		longitude = r$gps_lon_rounded,
		latitude = r$gps_lat_rounded,
		geo_from_source = TRUE,
		elevation = r$gps_alt,
		adm1 = carobiner::fix_name(r$region, case="title"),
		location = carobiner::fix_name(r$sublocation, case="title"),
		farmland_owned = r$land_owned_ha,
		on_farm = TRUE,
		is_survey = TRUE
	)

	cc <- geodata::country_codes()
	d$country <- cc$NAME[match(r$iso_country_code.x, cc$ISO2)]
	
# dates
	d1 <- r$starttime_auto
	d2 <- rep(as.Date(NA), length(d1))
	i <- grep("/", d1)
	d2[i] <- as.Date(substr(d1[i], 1, 10), "%d/%m/%Y")
	i <- grep("-", d1)
	d2[i] <- as.Date(substr(d1[i], 1, 10), "%Y-%m-%d")
	i <- is.na(d2) & (!is.na(d1))
	x <- do.call(rbind, strsplit(d1[i], ", "))
	x[,2] <- sapply(strsplit(x[,2], " "), \(i) i[1])
	y <- do.call(rbind, strsplit(x[,1], " "))
	x <- cbind(x[,2], y)
	x[,2] <- carobiner::eng_months_to_nr(x[,2])
	d2[i] <- as.Date(apply(x, 1, \(i) paste(i, collapse="-")))
	d2 <- as.character(d2)
	i <- is.na(d2)
	d2[i] <- r$year.x[i]
	d$date <- d2
# end dates

#	farmland_rentedin = r$landrentin,
#	farmland_rentedout= r$landrentout,
#	cropland_used = r$landcultivated,
#	land_irrigated = r$land_irrigated,
#	land_tenure = r$land_tenure

	
	carobiner::write_files(meta, d, path=path)
}

