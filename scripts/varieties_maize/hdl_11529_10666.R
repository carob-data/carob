# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {


"Summary results and individual trial results from the International Late Yellow Hybrid - ILYH, (Elite TropicalLate Yellow Normal and QPM Hybrid Trial Version A - CHTTY-A) conducted in 2012."

	uri <- "hdl:11529/10666"
	group <- "varieties_maize"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "International Late Yellow Hybrid Trial",
		data_type = "experiment",
		treatment_vars = "variety_pedigree",
		response_vars = "yield", 
		completion = 100,
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2025-09-15",
		notes = NA,
		design = NA
	)

	get_data <- function(fname, id, longitude, latitude, elevation) {
	  f <- ff[basename(ff) == fname]
	  r <- carobiner::read.excel(f, sheet = "Master", skip = 1) 
	  d <- data.frame( 
	    trial_id = id,
	    rep = r$REP,
	    plot_id = r$PLOT,
	    location = r$...124,
	    country = r$...125,
	    planting_date = r$...129,
	    harvest_date = r$...130,
	    variety_pedigree = r$ENTRY,
	    plot_area = r$...131,
	    yield_part = "grain", 
	    yield = r$GYgrn * 1000,
	    plant_density = r$nP * 1567,
	    plant_height = r$PH,
	    e_rot = r$pER,
	    e_asp = r$EarAsp,
	    p_asp = r$PltAsp,
	    yield_moisture = r$MOI,
	    gtext = r$TEX,
	    rlper = r$pRL,
	    slper = r$pSL,
	    rust  = r$Psorg1,
	    streak = r$MSV,
	    blight = r$Eturc1,
	    borer = r$StBor,
	    gls = r$GLS,
	    asi = r$ASI,
	    silking_days = r$SD,
	    anthesis_days = r$AD,
	    longitude = longitude,
	    latitude = latitude,
	    elevation = elevation
	  )
	}
	
	d0 <- get_data("12CHTTY-A1-1.xls", "1", 80.4667, 8.1167, 117)
	d1 <- get_data("12CHTTY-A2-1.xls", "2", 30.9833, 0.9667, 1007)
	d2 <- get_data("12CHTTY-A6-1.xls", "3", 89.35, 24.7833, 18)
	d3 <- get_data("12CHTTY-A7-1.xls", "4", 30.9833, 0.9667, 1007)
	
	#specifying variety_pedigrees
	entry_code = c(1,2,3,4,5,6,7,8)
	
	var_pedigree1 = c("(CLQRCYQ59)/(CLQS89YQ04)","(CLQRCYQ64)/(CLQRCYQ65)","(CLQRCYQ59)/(CML161)",
	             "(CLQRCYQ59)/(CLQRCYQ49)","(CML161)/(CML165)","CL02720/CLRCY017",
	             "Local Check-1: Sampath","Local Check-2: Pacific 984")
	
	d0$variety_pedigree <- var_pedigree1[match(d0$variety_pedigree, entry_code)]
	
	var_pedigree2 = c("(CLQRCYQ59)/(CLQS89YQ04)","(CLQRCYQ64)/(CLQRCYQ65)","(CLQRCYQ59)/(CML161)",
	                 "(CLQRCYQ59)/(CLQRCYQ49)","(CML161)/(CML165)","CL02720/CLRCY017",
	                 "Local Check-1: Longe 10H","Local Check-2: Longe 7H")
	
	d1$variety_pedigree <- var_pedigree2[match(d1$variety_pedigree, entry_code)]
	
	var_pedigree3 = c("(CLQRCYQ59)/(CLQS89YQ04)","(CLQRCYQ64)/(CLQRCYQ65)","(CLQRCYQ59)/(CML161)",
	                  "(CLQRCYQ59)/(CLQRCYQ49)","(CML161)/(CML165)","CL02720/CLRCY017",
	                  "Local Check-1: Heera-101 (Supreme DC hybrid)","Local Check-2: Pacific-984 (Pacific SC hybrid)")
	
	d2$variety_pedigree <- var_pedigree3[match(d2$variety_pedigree, entry_code)]
	
	var_pedigree4 = c("(CLQRCYQ59)/(CLQS89YQ04)","(CLQRCYQ64)/(CLQRCYQ65)","(CLQRCYQ59)/(CML161)",
	                  "(CLQRCYQ59)/(CLQRCYQ49)","(CML161)/(CML165)","CL02720/CLRCY017",
	                  "Local Check-1: Longe 10H","Local Check-2: VP MAX")
	
	d3$variety_pedigree <- var_pedigree4[match(d3$variety_pedigree, entry_code)]
	
	d <- carobiner::bindr(d0, d1, d2, d3)

	d$crop <- "maize"
	d$on_farm <- TRUE
	d$striga_trial <- FALSE
	d$striga_infected <- FALSE
	d$borer_trial <- FALSE
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- TRUE
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	d$planting_date <- as.character(d$planting_date)
	d$harvest_date <- as.character(d$harvest_date)
	d$rep <- as.integer(d$rep)
	d$plot_id <- as.character(d$plot_id)
	
	
	carobiner::write_files(path, meta, d)
}

