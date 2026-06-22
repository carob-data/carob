# R script for "carob"


## not processed because the data are superseded by (and included in) doi:10.7910/DVN/WS38SA



carob_script <- function(path) { 

"[THIS VERSION HAS BEEN SUPERSEDED AND IS KEPT ONLINE FOR LEGACY PURPOSES ONLY. PLEASE FIND THE MOST RECENT VERSION OF THE DATASET AT https://doi.org/10.7910/DVN/WS38SA] \nThe Rural Household Multiple Indicator Survey (RHoMIS) is a standardized farm household survey approach which collects information on 753 variables covering household demographics, farm area, crops grown and their production, livestock holdings and their production, agricultural product use and variables underlying standard socio-economic and food security indicators like the Poverty Probability Index, the Household Food Insecurity Access Scale and dietary diversity. These variables are used to quantify more than 40 different aggregate indicators on farm household characteristics, welfare, productivity and economic performance. Between 2015 and the beginning of 2018, the survey instrument has been applied in 21 countries in Central America, sub-Saharan Africa and Asia. The data presented here cover the raw data, the indicator calculation code and the resulting indicator values, and can be used to quantify on- and off-farm pathways to food security, diverse diets and reduced poverty of rural smallholder farm households."
  
"The Rural Household Multiple Indicator Survey (RHoMIS) is a standardized farm household survey approach which collects information on 753 variables covering household demographics, farm area, crops grown and their production, livestock holdings and their production, agricultural product use and variables underlying standard socio-economic and food security indicators like the Poverty Probability Index, the Household Food Insecurity Access Scale and dietary diversity. These variables are used to quantify more than 40 different aggregate indicators on farm household characteristics, welfare, productivity and economic performance. Between 2015 and the beginning of 2018, the survey instrument has been applied in 21 countries in Central America, sub-Saharan Africa and Asia. The data presented here cover the raw data, the indicator calculation code and the resulting indicator values, and can be used to quantify on- and off-farm pathways to food security, diverse diets and reduced poverty of rural smallholder farm households. (2019-10-31)"


	uri <- "doi:10.7910/DVN/9M6EHS"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- data.frame(
		uri=uri, 
		group=group, 
		major=2, 
		minor=1,
		dataset_id = "doi_10.7910_DVN_9M6EHS_nodata", 
		group = "survey", 
		uri = "doi:10.7910/DVN/9M6EHS", 
		license = "CC0-1.0", 
		title = "SUPERSEDED - The Rural Household Multiple Indicator Survey (RHoMIS) data of 13,310 farm households in 21 countries.", 
		authors = "-, RHoMIS", 
		data_published = "2019-10-31", 
		project = "RHoMIS", 
		publication = "doi:10.1038/s41597-020-0388-8", 
		data_organization = "ILRI", 
		data_type = "survey", 
		response_vars = "none", 
		treatment_vars = "none", 
		carob_contributor = "Robert Hijmans", 
		carob_date = "2024-01-29"
	)
	
	carobiner::write_files(path, meta)
}

.ignore <- function(path) {

	uri <- "doi:10.7910/DVN/9M6EHS"
	group <- "survey"

	ff <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		project="RHoMIS",
		publication= "doi:10.1038/s41597-020-0388-8",
		data_organization = "ILRI",
		data_type="survey", 
		response_vars = "none",
		treatment_vars = "none",
		carob_contributor="Robert Hijmans",
		carob_date="2024-01-29"
	)
	
  
	f <- ff[basename(ff) == "RHoMIS_Full_Data.csv"]
	r <- read.csv(f, encoding="latin1", na.strings=c("NA", "", "na"))

	d <- data.frame(
		trial_id=as.character(NA),
		planting_date=as.character(NA),
		longitude = r$GPS_LON,
		latitude = r$GPS_LAT,
		geo_from_source = TRUE,
		elevation = r$GPS_ALT,
		adm1 = carobiner::fix_name(r$region, case="title"),
		location = r$sublocation,
		date = as.character(r$YEAR),	
		farmland_owned = r$landowned,
		farmland_rentedin = r$landrentin,
		farmland_rentedout= r$landrentout,
		cropland_used = r$landcultivated,
		land_irrigated = r$land_irrigated,
		land_ownedby = r$land_ownership,
		land_tenure = r$land_tenure
	)
	
	cdf <- data.frame(
		code= c("TZ", "GT", "SV", "HN", "ML", "BF", "MW", "KE", "IN", "KH", "VN", "LA", "ET", "CD", "ZM", "GH", "UG", "CR", "BI", "PE", "NI"),
		country = c("Tanzania", "Guatemala", "El Salvador", "Honduras", "Mali", "Burkina Faso", "Malawi", "Kenya", "India", "Cambodia", "Vietnam", "Laos", "Ethiopia", "Democratic Republic of the Congo", "Zambia", "Ghana", "Uganda", "Costa Rica", "Burundi", "Peru", "Nicaragua")
	)


	d$is_survey <- TRUE
	d$country <- cdf$country[match(r$ID_COUNTRY, cdf$code)]
	carobiner::write_files(meta, d, path=path)
}

