# R script for "carob"


carob_script <- function(path) {
  
"Group B1, cycle B1C5 of Population B (fifth cycle of recombination of the pure native Andigena group B1), is the result of a new population improvement strategy in the absence of R genes started at CIP in 1990. The group B1 derives from the primitive cultivars of Solanum tuberosum ssp. andigena, known to be free of R-genes.   These clones were planted in a randomized complete block design (RCBD) with 2-4  replicates at Oxapampa,  located at 1810 masl in Pasco-Peru in the Eastern mountain ranges facing the Amazon. The trials were established at Oxapampa due to the high disease pressure of late blight in these areas from 2005 to 2006."
    
    uri <- "doi:10.21223/P3/VYCLVX"
    group <- "varieties_potato"
    ff  <- carobiner::get_data(uri, path, group)
    
    meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
        data_organization = "CIP",
        publication = NA,
        project = NA,
        data_type = "experiment",
        treatment_vars = "variety",
        response_vars = "yield;yield_marketable", 
        carob_contributor = "Henry Juarez",
        carob_date = "2024-09-11",
        notes = NA
    )
    
    
    process <- function(filename){

		r <- carobiner::read.excel(filename, sheet="Fieldbook")               
        minimal <- carobiner::read.excel(filename, sheet="Minimal")
        m <- as.list(minimal$Value)
        names(m) <- minimal$Factor
#		dat <- c("REP","INSTN","AUDPC","rAUDPC","TTYNA","MTYNA",'NoMTWP','TTWP','MTWP')
		data.frame(
			rep = as.integer(r$REP),
			variety = r$INSTN,
			yield = as.numeric(r$TTYNA) * 1000,
			yield_marketable = as.numeric(r$MTYNA) * 1000,
			AUDPC = as.numeric(r$AUDPC) / 100,
        	country = m$Country,
        	adm1 = m$Admin1,
        	adm2 = m$Admin2,
        	adm3 = m$Admin3,
			location = m$Locality,
        	longitude = as.numeric(m$Longitude),
        	latitude = as.numeric(m$Latitude),
        	elevation = as.numeric(m$Elevation),
        	planting_date = m$`Begin date`,
        	harvest_date = m$`End date`,
        	trial_id = gsub(".xls", "", basename(filename))
		)
    }
	
    f <- ff[grep("OXAPMP", basename(ff))]
    f <- f[grep("^0._|_Data_dictionary|_processed", basename(f), invert=TRUE)]
    d <- lapply(f, process)
    d <- do.call(rbind, d)
    
    d$on_farm <- TRUE
    d$is_survey <- FALSE
    d$irrigated <- FALSE
    d$crop <- "potato"
    d$pathogen <- "Phytophthora infestans"
    d$yield_part <- "tubers"
    d$geo_from_source <- TRUE
    d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
    
    carobiner::write_files(path = path, metad = meta, wide=d)
}
