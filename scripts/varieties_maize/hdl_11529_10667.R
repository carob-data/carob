# R script for "carob"



carob_script <- function(path) {
  
"Summary results and individual trial results from the International Late Yellow Hybrid - ILYH, (Mid-altitude / Subtropical Three Way Crosses Yellow Hybrids, with High Concentrations of Provitamins A (especially beta-carotene) - CHTSPROA) conducted in 2012."
  
	uri <- "hdl:11529/10667"
	group <- "varieties_maize"

	ff <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		publication= NA,
		project = "International Late Yellow Hybrid Trial",		
		data_organization = "CIMMYT",
		data_type="experiment", 
		response_vars = "yield",
		treatment_vars = "variety",
		carob_contributor="Mitchelle Njukuya",
		carob_date="2023-01-30",
		modified_by="Robert Hijmans"
	)
    
	#f <- ff[basename(ff) == "12CHTSPROA-Locations.xls"]
	#locs <- carobiner::read.excel(f) 
	
	get_data <- function(fname, id) {
		f <- ff[basename(ff) == fname]
		r <- carobiner::read.excel(f, fix_names=TRUE, n_max=24) 
		colnames(r) <- gsub("\\.\\.", ".", colnames(r))
		d <- data.frame( 
			trial_id = id,
			crop = "maize",
			on_farm = TRUE,
			striga_trial = FALSE, 
			striga_infected = FALSE,
			borer_trial = FALSE,
 			yield_part = "grain", 
			yield = r$Yield.t.ha * 1000,
			plant_height = r$Plt.Hght.cm,
			variety = r$Pedigree,
			e_asp = r$Ear.Asp.1.5,
			p_asp = r$Plt.Asp.1.5,
			ear_height  = r$Ear.Hght.cm
		)
		d$moist = r$pct.Moi.sture
		d$e_rot = r$Ear.Rot.pct
		d$rlper = r$Rt.Ldg.pct
		d$slper = r$Stlk.Ldg.pct
		d$gtext = r$Text.1.5
		d$rust  = r$Rust.P.sor.1.5
		d$streak = r$MSV.1.5
		d$gls = r$GLS.1.5
		d$asi = r$ASI
		d$borer = r$Stem.Bor.1.5
		d$plant_height = r$Plt.Hght.cm
		d
	}

	d0 <- get_data("12CHTSPROA10-1.xls", "0")
    # location and dates from 12CHTSPROA-Locations.xls
	d0$country <- "Bolivia"
	d0$location <- "San Pedro" 
	d0$longitude <- -63.4667
	d0$latitude <- -16.9 
	d0$elevation <- 230
	d0$planting_date <- "2013-01-17"
	d0$harvest_date  <- "2013-05-30"
    
	d1 <- get_data("12CHTSPROA11-1.xls", "1")
	d1$country <- "Bolivia"
	d1$location <- "Algarrobal" 
	d1$longitude <- -63.65
	d1$latitude <- -21.45 
	d1$elevation <- 580
	d1$planting_date <- "2013-01-05"
	d1$harvest_date  <- "2013-06-14"
  
	d2 <- get_data("12CHTSPROA12-1.xls", "2")  
    d2$country <- "Bolivia"
	d2$location <- "Muyupampa" 
	d2$longitude <- -63.75
	d2$latitude <- -19.8833 
	d2$elevation <- 1177
	d2$planting_date <- "2013-01-15"
	d2$harvest_date  <- "2013-06-20"

	d3 <- get_data("12CHTSPROA16-1.xls", "3")
	d3$country <- "Ghana"
	d3$location <- "Kwadaso" 
	d3$longitude <- -1.65
	d3$latitude <- 6.7 
	d3$elevation <- 275
	d3$planting_date <- "2012-10-15"
	d3$harvest_date  <- "2013-01-30"

	d4 <- get_data("12CHTSPROA17-1.xls", "4")
	d4$country <- "Ghana"
	d4$location <- "Kwadaso" 
	d4$longitude <- -1.65
	d4$latitude <- 6.7 
	d4$elevation <- 275
	d4$planting_date <- "2012-10-15"
	d4$harvest_date  <- "2013-01-30"
      
	d5 <- get_data("12CHTSPROA18-1.xls", "5")	  
	d5$country <- "Ghana"
	d5$location <- "Kwadaso" 
	d5$longitude <- -1.65
	d5$latitude <- 6.7 
	d5$elevation <- 275
	d5$planting_date <- "2012-10-15"
	d5$harvest_date  <- "2013-01-30"
  
	d6 <- get_data("12CHTSPROA19-1.xls", "6")  
	d6$country <- "Myanmar"
	d6$location <- "Yezin" 
	d6$longitude <- 96
	d6$latitude <- 19
	d6$elevation <- NA
	d6$planting_date <- "2012-10-19"
	d6$harvest_date  <- "2013-02-10"	

	d7 <- get_data("12CHTSPROA22-1.xls", "7")  
	d7$country <- "Uganda"
	d7$location <- "Nalweyo, Kibaale District" 
	d7$longitude <- 30.9833
	d7$latitude <- 0.9667
	d7$elevation <- 1007
	d7$planting_date <- "2012-10-09"
	d7$harvest_date  <- "2013-02-05"
 
	d <- carobiner::bindr(d0, d1, d2, d3, d4, d5, d6, d7)
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- TRUE
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
		   
		   
	carobiner::write_files(meta, d, path=path)
}

