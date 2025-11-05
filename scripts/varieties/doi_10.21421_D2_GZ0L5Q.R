# R script for "carob"

carob_script <- function(path) {
   
" This dataset includes the research work carried out in evaluating rust disease resistance in Groundnut mini core accessions. The experiment was laid out in field during rainy seasons of 2012 and 2013 at ICRISAT, Patancheru, India. A total of 190 accessions (184 mini core plus two standard checks and two other checks) were screened for rust experiment. The trial was laid out in a randomized complete block design (RCBD) on June during both years with three replications on broad bed furrows. The size of the beds was 4.0 m×1.5 m and the mean planting density was 26.7plants/m2. Altogether, there were 80 plants/ replication for each accession, spread in two rows, and there were four rows for each bed. Precaution was taken to ensure uniform and proper depth of planting (5 cm). Standard agronomic and cultural practices were followed during the cropping season. Infector row method was followed by sowing one bed (4 rows) of TMV 2 (susceptible to rust) for every four beds of test materials. The pathological data (disease severity- based on disease rating scale 1-9) on rust infection @ 75, 90 and 105 days after sowing (DAS) intervals respectively were presented here. The data on pod yield (kg/ha) in disease plot was also presented here. " 
   
   uri <- "doi:10.21421/D2/GZ0L5Q"
   group <- "varieties" 
   ff  <- carobiner::get_data(uri, path, group)

   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3, 
      data_organization = "ICRISAT", 
      publication ="doi:10.1007/s13313-015-0368-1", 
      project = NA, 
      data_type = "experiment",
      treatment_vars = "variety", 
      response_vars = "yield;disease_severity",
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-08-03"
   )
   
   ff <-ff[grep("xlsx",basename(ff))]
   
   process <- function(f){
      r <- carobiner::read.excel(f, na=c("Damage",NA))
      names(r) <- gsub("Rust  at 105 Days After Sowing", "Rust at 105Days After Sowing", names(r))
      names(r) <- gsub("Genotye","Genotype",names(r))
      data.frame(
         variety = r$Genotype,
         yield= as.numeric(r$`Pod yield`),
         trial_id= substr(gsub("xlsx", "", basename(f)), 45, 59),
         planting_date= ifelse(grepl("2012", basename(f)), "2012-06-20", "2013-06-20"),
         plot_area= 2*4,
		 rust75 = r$`Rust at 75 Days After Sowing`,
		 rust90 = r$`Rust at 90 Days After Sowing`,
		 rust105 = r$`Rust at 105Days After Sowing`
      )
   }
   
   d <- lapply(ff, process)
   d <- do.call(rbind,d)
   #  Removing one row with NA in yield
   ## unique because the data contain duplicate rows
   d <- unique(d[!is.na(d$yield),]) 
	d$record_id <- 1:nrow(d)

   d$country <- "India"
   d$adm1 <- "Telangana"
   d$location <- "Patancheru"
   d$site <-  "ICRISAT campus"
   d$crop <- "groundnut"
   d$plant_density <- 26.7*10000 ## plants/ha
   d$irrigated <- FALSE ## rainy season
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "pod"
   d$geo_from_source <- FALSE
   d$longitude  <- 78.2739
   d$latitude  <-  17.5119
	d$geo_uncertainty <- 1500
   
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   ## Rust disease score at different time after planting
    rsvars <- grep("^rust", colnames(d), value=TRUE)
    dd <- d[, c("record_id", "planting_date", rsvars)]
	dd$planting_date <- as.Date(dd$planting_date)
	DAP <- c(75, 90 , 105) |> as.integer()
    x <- reshape(dd, direction="long", varying =rsvars, v.names="disease_severity", timevar="step")
    x$time <- as.character(as.Date(x$planting_date)+ DAP[x$step])
    x$DAP <- DAP[x$step]
    x$id <- x$step <- x$planting_date <- NULL
    x$disease_severity <- as.character(x$disease_severity)
    x$severity_scale <- "1-9"    
	x$diseases <- "rust"
	
	d[rsvars] <- NULL
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files (path, meta, d, long=x)    
}
