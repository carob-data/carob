# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Replication Data for: Dual purpose sorghum hybrids evaluation at multiple locations in Ethiopia during 2018-2020

Data on agronomic traits of maturity, plant height, grain yield and plant aspect score collected for dual purpose sorghum hybrids evaluated at multiple locations in Ethiopia during 2018-2020
"

	uri <- "doi:10.7910/DVN/NWROYH"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		carob_date = "2025-09-01",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;dmy_total;fwy_total", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	ff <- ff[grepl("xlsx", basename(ff))]
	
#### processing 
	
	process <- function(f){
	   
	   r <- carobiner::read.excel(f, fix_names = TRUE, na= "NA")
	   names(r) <- gsub("Replicate", "rep", names(r))
	   names(r) <- gsub("PHT", "PHTMean", names(r))
	   names(r) <- gsub("Biomass.1.5", "FreshBiomass", names(r))
	   if("X100GW" %in% names(r)) r$X1000GW <- as.numeric(r$X100GW)*10
	   if(is.null(r$X1000GW)) r$X1000GW <- NA
	   if(is.null(r$BirdDamage)) r$BirdDamage <- NA
	   if(is.null(r$FreshBiomass)) r$FreshBiomass <- NA
	   if(is.null(r$Sown)) r$Sown <- NA
	   if(is.null(r$DryBiomass)) r$DryBiomass <- NA
	   if(is.null(r$Pedigree)) r$Pedigree <- NA
	   if(is.null(r$Yield.Kg.Ha)) r$Yield.Kg.Ha <- NA
	   
	   d <- data.frame(
	      year = r$Year,
	      location = r$Site,
	      plot_id = as.character(r$Plot),
	      rep = as.integer(r$rep),
	      variety = r$Genotype,
	      variety_pedigree = r$Pedigree,
	      flowering_days = r$DTF,
	      maturity_days = r$DTM,
	      plant_height = r$PHTMean,
	      seed_weight = r$X1000GW, 
	      yield = r$Yield.Kg.Ha,
	      planting_date= as.character(r$Sown),
	      fwy_total = r$FreshBiomass,
	      dmy_total = r$DryBiomass,
	      bird_damage = as.character(r$BirdDamage),
	      trial_id = gsub("Dual purpose sorghum hybrids at|.xlsx| ", "", basename(f)),
	      crop = "sorghum",
	      country = "Ethiopia",
	      yield_part = "grain",
	      on_farm = TRUE,
	      is_survey = FALSE,
	      irrigated = NA,
	      yield_moisture = as.numeric(NA)
	      
	   )
	}

	dd <- lapply(ff, process)
	d <- do.call(rbind, dd)
	
	### remove rows with missing yield
	d <- d[!is.na(d$yield),]
	
	d$location <- gsub("2020|2018|2019| ", "", d$trial_id)
	i <- grepl("/", d$planting_date)
	d$planting_date[i] <- as.character(as.Date(d$planting_date[i], "%m/%d/%Y"))
	d$planting_date <- ifelse(is.na(d$planting_date), paste("20", d$year, sep ="" ), d$planting_date)
	
	### Adding lon and lat
	
	geo <- data.frame(
	   location = c("Bako", "Kobo", "Melkassa", "Mieso", "Shiraro"),
	   longitude = c(37.0578, 39.636, 39.326, 40.754, 37.77),
	   latitude = c(9.1255, 12.154, 8.417, 9.233, 14.397),
	   geo_from_source= FALSE
	)
	 
 d <- merge(d, geo, by= "location", all.x = TRUE)
 
 d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
 d$year <- NULL
 
carobiner::write_files(path, meta, d)

}

