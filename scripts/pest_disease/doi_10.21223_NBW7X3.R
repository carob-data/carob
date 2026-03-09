# R script for "carob"

carob_script <- function(path) {
   
"Evaluation of yield under late blight and viral disease presence, in Kenya. In the first experiment,16 clones were evaluated under RCBD with 3 repetitions. In the second experiment, 32 clones were evaluated in RCBD with 3 reps. Both trials were established at University of Nairobi, Kabete Campus, Nairobi-Kenya (1800 m a.s.l.). All late blight evaluations were performed 30, 40, 50, 60, 70, 80 and 90 days after planting. The plots were set with a distance of 0.75 m between rows and 0.20 m plants"

   uri <- "doi:10.21223/NBW7X3"
   group <- "pest_disease"
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
      data_organization = "CIP",
      publication= NA,
      project=NA,
      data_type= "experiment",
      response_vars = "yield",
      treatment_vars = "variety_code",
      carob_contributor= "Cedric Ngakou",
      carob_date="2024-06-12"
   )
   
	r1 <- carobiner::read.excel(ff[basename(ff)=="12136_PTYield062018_UNINAIROBI_exp1.xlsx"], sheet = "Fieldbook")
	d1 <- data.frame(      
		variety_code= r1$INSTN,
		rep= as.integer(r1$REP),
		yield=r1$TTYNA*1000, # in kg/ha
		AUDPC= r1$AUDPC / 100,
		rAUDPC= r1$rAUDPC / 100,
		virus_severity=tolower(r1$Virus),
#		seed_density=r1$SPBE, this is seed production per berry, not seed used to plant the crop!
		planting_date="2018-06-04", ## from data description
		trial_id = "3"
	)
	lbvars <- grep('^LB', colnames(r1), value=TRUE)
	lb1 <- r1[, lbvars]

  
	sheets <- c("2018-07-17_Fieldbook_Trial1", "2018-07-22_Fieldbook_Trial2")
	dlst <- vector(length=2, mode="list")
	lblst <- vector(length=2, mode="list")
	for (i in 1:length(sheets)) { 
		r <- carobiner::read.excel(ff[basename(ff)=="caf1cc988f3c58c6cb1af73bed6ca249.xlsx"], sheet=sheets[i])
		names(r) <- gsub("CloneID", "Clones", names(r))
		names(r) <- gsub("Seeds", "seeds", names(r))
		dlst[[i]] <- data.frame( 
			variety_code=r$Clones,
			rep= as.integer(r$Rep),
			flowering_date= as.character(r$`Flowering Date`),
			AUDPC=r$AUDPC / 100,
			rAUDPC=r$rAUDPC / 100,
			virus_severity=tolower(r$Virus),
		#	seed_density=r$seeds,
			planting_date= as.character(r$Planted),
			yield=r$Yield*1000, # in kg/ha
			trial_id = as.character(i)
		)

		lb <- r[, as.character(seq(30, 90, 10))]
		lb[is.na(lb)] <- 0
		colnames(lb) <- lbvars
		lblst[[i]] <- lb 
	}
 
   d <- carobiner::bindr(d1, do.call(rbind, dlst))
   d$record_id <- 1:nrow(d)
   ### Add more variables 
   d$country <- "Kenya"
   d$adm1 <- "Nairobi"
   d$location <- "Kabete"
   d$site <- "University of Nairobi"
   d$latitude <- -1.25438889
   d$longitude <- 36.729999
   d$elevation <- 1800
   d$geo_from_source <- TRUE # sheet = minimal
   d$crop <- "potato"
	d$pathogen <- "Phytophthora infestans"
   d$diseases <- "potato late blight"
   d$row_spacing <- 75  ## from data description 
   d$plant_spacing <- 20  
   d$plant_density <- 44444.44 # plants/ha 
   d$on_farm <- TRUE
   d$irrigated <- FALSE
   d$inoculated <- FALSE
   d$is_survey <- FALSE
   
   d$yield_part <- "tubers"
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   ### Add disease scores during the season
	lb <- rbind(lb1, do.call(rbind, lblst))  
	lb$record_id <- as.integer(1:nrow(lb))
	lb$pdate <- as.Date(d$planting_date)
   
	x <- reshape(lb, direction="long", varying =lbvars, v.names="disease_severity", timevar="step")
	x$time <- as.character(x$pdate + seq(30, 90, 10)[x$step])

   x$step <- x$id <- x$pdate <- NULL
   x$disease_severity <- as.character(x$disease_severity)
   
   carobiner::write_files(path, meta, d, long=x)   
}

