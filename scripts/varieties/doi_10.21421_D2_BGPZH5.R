# R script for "carob"

   
carob_script <- function(path) {
      
" This dataset includes the research work carried out in evaluating marker assisted backcross (MABC) lines for rust and late leaf spot (LLS) resistance in five locations during 2015 rainy season. The evaluation were carried out at Aliyarnagar, Tamil Nadu; Directorate of Groundnut Research (DGR) Junagadh, Gujarat; Dharwad, Karnataka; ICRISAT, Patancheru and Kasbe Digraj, Maharashtra. A set of 41 ILs – 18 ILs in ICGV 91114 background, 14 ILs in TAG 24 background and 9 in JL 24 background along with recurrent parents – ICGV 91114, TAG 24, JL24; donor parent – GPBD 4 and three local checks were evaluated. Multi-location evaluation is useful to identify stable performing entries across locations to facilitate their release both at national and state level. Based on the data on disease score, yield and yield associated traits (Shelling percent and Hundred seed weight), 12 ILs were selected and used for conducting first ever near isogenic line (NIL) trial under All India Co-ordinated Research Project on Groundnut (AICRP-G). Subsequently three best performing ILs were identified and advanced to Advanced Varietal Trials (AVT-1 & AVT-2) under AICRP-G. The data presented here is the mean location wise performance of the entries for disease resistance, yield and associated traits that enabled selection and advancement of best performing ILs at ICRISAT, Patancheru, Hyderabad"
      
    uri <- "doi:10.21421/D2/BGPZH5"
    group <- "varieties" 
    ff  <- carobiner::get_data(uri, path, group)
    meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3, 
       data_organization = "ICRISAT", 
       publication =NA, 
       project = NA, 
       data_type = "experiment", 
       response_vars = "yield",
       treatment_vars = "variety", 
       carob_contributor = "Cedric Ngakou", 
       carob_date = "2024-07-16"
    )
    
    ff <- ff[grep(".xlsx", basename(ff))]

    process <- function(f){
        r <- carobiner::read.excel(f)
        if (is.null(r$`Rust 90`)) { r$`Rust 90`<- NA} 
        data.frame(
			variety= r$Genotype,
			yield= r$PYH,
			seed_weight = as.numeric(r$HSW)* 10, ## 1000 seed weight,
			diseases = "rust",
			trial_id= gsub(".xlsx", "", basename(f)),
			disease_severity = as.character(round(r$`Rust 90`, 2)),
			shelling_percentage = r$SHP
		)     
    }
    
    d <- lapply(ff, process)
    d <- do.call(rbind,d)
    
    d$country <- "India"
    d$adm1 <- "Hyderabad"
    d$location <- "Patancheru"
    d$crop <- "groundnut"
    
    d$irrigated <- TRUE
    d$irrigated[grep("RF ICRISAT", d$trial_id)] <- FALSE 
    d$on_farm <- TRUE
    d$is_survey <- FALSE
    d$inoculated <- FALSE
    d$yield_part <- "pod"
    
    d$planting_date <- "2015"
    d$longitude  <- 78.2674254
    d$latitude  <-  17.5286092
    d$geo_from_source <- FALSE
    
    d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
    
	d$yield_moisture <- as.numeric(NA) #needs to be checked

    carobiner::write_files (path, meta, d)    
}
