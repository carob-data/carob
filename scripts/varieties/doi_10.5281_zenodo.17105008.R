# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
This dataset is part of the database compiled as an outcome of Work Area 1 in project OrganicYieldsUP. Variable definitions can be found here:  https://doi.org/10.5281/zenodo.15276082

Data on spring wheat from a long term trial on cereal production for the period 2015-2024 (available for researchers on request). The dataset comprises yield information on 44  varieties, fertilisation operations in seven localities (not all present all years: Grästorp, Västerås, Vikingstad, Vara, Skara, Skultuna, Borrby).
"

	uri <- "doi:10.5281/zenodo.17105008"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=8, minor=NA,
		data_organization = "Swedish University of Agricultural Sciences",
		publication = NA,
		project = "Organic Yields UP",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-08-01",
		carob_completion = 90,	
		carob_effort = 6
	)
	

	library(DBI)
	library(RSQLite)
	
	# Connect to the SQLite file
	
	con <- dbConnect(RSQLite::SQLite(),ff)
	
	# See what tables are inside
	dbListTables(con)
	
	# Reading specific table into a data.frame
	fertilizer <- dbReadTable(con, "farm_op_fertilisation_view")
	harvest <- dbReadTable(con, "farm_op_harvest_view")
	irrigation <- dbReadTable(con, "farm_op_irrigation_view")
	protection <- dbReadTable(con, "farm_op_plant_protection_view")
	planting <- dbReadTable(con, "farm_op_seedingplanting_view")
	tillage <- dbReadTable(con, "farm_op_tillage_view")
	fertilizer2 <- dbReadTable(con, "farm_operation_fertilisation_tbl")
	harvest1 <- dbReadTable(con, "farm_operation_harvest_tbl")
	irrigation1 <- dbReadTable(con, "farm_operation_irrigation_tbl")
	planting2 <- dbReadTable(con, "farm_operation_seedingplanting_tbl")
	tillage2 <- dbReadTable(con, "farm_operation_tillage_tbl")
	harvest2 <- dbReadTable(con, "harvest_tbl")
	harvest3 <- dbReadTable(con, "harvest_variety_view")
	harvest4 <- dbReadTable(con, "harvest_view")
	diseases <- dbReadTable(con, "pest_disease_data_tbl")
	diseases2 <- dbReadTable(con, "pest_disease_view")
	plots <- dbReadTable(con, "plot_soil_measurement_view")
	site <- dbReadTable(con, "site_tbl")
	soil2 <- dbReadTable(con, "soil_classification_tbl")
	soil3 <- dbReadTable(con, "soil_measurement_tbl")
	treatment <- dbReadTable(con, "study_treatment_site_distinct_view")
	treatment2 <- dbReadTable(con, "treatment_tbl")
	variety <- dbReadTable(con, "variety_data_tbl")
	variety2 <- dbReadTable(con, "variety_data_view")
	variety3 <- dbReadTable(con, "variety_specification_tbl")
	weather <- dbReadTable(con, "weather_data_daily_tbl")
	weather <- dbReadTable(con, "weather_data_monthly_tbl")
	fertilizer3 <- dbReadTable(con, "farm_op_fertilisation_view")
	
	dbDisconnect(con)

	fert <- data.frame(
	  id=fertilizer$fk_site_id,
	  country=fertilizer$country,
	  adm2=fertilizer$Town,
	  fertilizer_date=fertilizer$operation_date,
	  crop="wheat",
	  fertilizer_implement=fertilizer$machinery_used,
	  OM_amount=fertilizer$fertiliser_quantity*1000,
	  OM_type=fertilizer$comment_operation
	  )
	
	yield <- data.frame(
	  id=harvest3$fk_site_id,
	  yield_part="grain",
	  yield=harvest3$yield_biomass*1000,
	  variety=harvest3$variety_name,
	  yield_moisture=as.numeric(NA),
	  yield_isfresh=NA
	)
	
	d <- merge(fert,yield,by="id",all.x = TRUE)
	
	sowing <- data.frame(
	  id=planting2$fk_site_id,
	  planting_date=planting2$operation_date,
	  planting_implement=planting2$machinery_used
	)
	
	d <- merge(d,sowing,by="id",all.x = T)
	
	##all other tables either have empty info(0 obs.) or the info cant be standardized
	d$id <- NULL
	d <- unique(d)
	
	om_values <- c(
	  "fermentation residue that remains after the anaerobic digestion of
	  organic material in a biogas plant"="digestate",
	  "Biofer 10-3-1"="Biofer",                                                                                    
	  "Vinasse fertilizer"="Vinasse",                                                                                   
	  "Mixture of cow and horse manure"="animal dung",                                                                      
	  "Organic fertilizer unknown origin"="unknown"
	)
	
	d$OM_type <- om_values[d$OM_type]
	
	d$trial_id <- paste(d$adm2,d$variety,sep = "_")
	d$planting_implement <- gsub("_", " ", d$planting_implement)
	d$fertilizer_implement <- gsub("_", " ", d$fertilizer_implement)
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- NA #0 raw data on irrigation
	d$geo_from_source <- FALSE
	d$harvest_date  <- NA
  d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
  
  ##adding coordinates
  #there are 2 place names that do not fall under adm2 because they are localities, so i will find their corresponding adm2 places
  d$adm2 <- gsub("Borrby","Simrishamn",d$adm2)
  d$adm2 <- gsub("Vikingstad","Linköping",d$adm2)
  
  loc <- data.frame(
    adm1 = c("Östergötland", "Skåne", "Västmanland", "Västra Götaland", "Västra Götaland", "Västra Götaland"),
    adm2 = c("Linköping", "Simrishamn", "Västerås", "Grästorp", "Skara", "Vara"),
    longitude = c(15.6026, 14.2069, 16.5681, 12.6643, 13.4821, 13.065),
    latitude = c(58.3644, 55.5772, 59.6209, 58.3225, 58.372, 58.2376),
    geo_uncertainty = c(36838, 20475, 26727, 16056, 18019, 20733),
    geo_source = c("GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2"))
  
  d <- merge(d,loc,by="adm2")
  
	carobiner::write_files(path, meta, d)
}

