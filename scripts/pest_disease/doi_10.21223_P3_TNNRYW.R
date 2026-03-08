# R script for "carob"

carob_script <- function(path) {

"B3C0 is the initial cycle of recombination of group B3- Population B. This population has been derived from population A. R genes have been eliminated from this group and horizontal resistance has been retained. Currently, these clones are being used on the variety selection in developing countries. This group of clones were planted in a randomized complete block design (RCBD) with 2 replicates at Comas, located at 2400 to 2788 masl in Junin situated in the Central mountain ranges in Peru. The trials were established at Comas due to the high disease pressure of late blight present in the area and used to screen selected potato genotypes for resistance to this disease."


	uri <- "doi:10.21223/P3/TNNRYW"
	group <- "pest_disease"
	
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		project=NA,
		publication= NA,
		data_organization = "CIP",
		carob_contributor="Henry Juarez",
		carob_date="2023-06-21",	   
		data_type="experiment",
		response_vars= "yield",
		treatment_vars="variety"
 	)

	f <- ff[basename(ff) == "PTLB200112_VIENA_B3C0COM02-01.xls"]

	proc_lb <- carobiner::get_function("proc_lb_trial", path, group)
	dates <- as.character(as.Date(c("2002-02-03", "2002-02-13", "2002-02-21", "2002-02-28", "2002-03-07")))
	p <- proc_lb(f, dates)
	d <- p$d
	x <- p$tim
	x$disease_severity <- x$severity
	x$severity <- NULL
	x$disease_severity <- as.character(x$disease_severity)
	
##### Location #####
	d$country <- "Peru"
	d$adm1 <- "Junin"
	d$adm2 <- "Concepcion"
	d$adm3 <- "Mariscal Castilla"
	d$location <- "Comas"
	d$site <- "Viena"
	d$elevation <- 2415
	d$longitude <- -75.1314
	d$latitude <- -11.5237
	d$geo_from_source <- FALSE
	d$trial_id <- "1"
	d$record_id <- 1:nrow(d)
	d$planting_date <- as.character(as.Date("2001-12-10"))
	d$harvest_date  <- as.character(as.Date("2002-04-02"))
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d$pathogen <- "Phytophthora infestans"
	d$diseases <- "potato late blight"
   d$is_survey = FALSE
   

	carobiner::write_files(path, meta, d, long=x)
}

