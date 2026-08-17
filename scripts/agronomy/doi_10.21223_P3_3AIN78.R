# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Dataset for: On-farm Sweetpotato–Legume Intercropping Trial Data (Malawi, 2022–2024)

On-farm trial dataset evaluating sweetpotato intercropped with soybean and pigeonpea under eight spatial-arrangement treatments (sole crop and 1:1 / 2:1 / row intercrop combinations), conducted with smallholder farmers in Salima and Mulanje districts, Malawi, across the 2022/2023 and 2023/2024 growing seasons. Variables include sweetpotato root count and weight (marketable and non-marketable), vine weight, soybean and pigeonpea grain yield, and partial/total Land Equivalent Ratio (LER).
"

	uri <- "doi:10.21223/P3/3AIN78"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "intercrops;spatial_arrangement",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-08-07",
		carob_completion = 100,	
		carob_effort = 2
	)
	

	ff <- ff[grepl("ANONYMIZED", basename(ff))]
	#f3 <- ff[basename(ff) == "Data_Dictionary_Sweetpotato-Legume_OnFarm.xlsx"]
	#r <- carobiner::read.excel(f3)

#### 
  proces <-	function(f){
    r <- carobiner::read.excel(f, fix_names = TRUE)
    names(r) <- gsub("EPA|Site.EPA", "site", names(r))
    names(r) <- gsub("Soybean.grain.weight.t.ha", "Soybean.grain.yield.t.ha", names(r))
    names(r) <- gsub("Pieonpea.grain.weight.t.ha", "Pigeonpea.grain.yield.t.ha", names(r))
    if(is.null(r$Vine.wt.t.ha)) r$Vine.wt.t.ha <- NA
    if(is.null(r$Season)) r$Season <- "2022-2023"
   data.frame(
      adm2 = carobiner::fix_name(r$District, "title"),
      location = r$site,
      trial_id = r$Farmer.ID,
      planting_date = substr(r$Season, 1, 4),
      treatment = r$Treatment,
      fwy_total_sp = r$Vine.wt.t.ha*1000,
      fwy_total_sb = NA,
      fwy_total_pp = NA,
      #r$Weight.of.non.marketable.t.ha,
      yield_marketable_sp = r$Weight.of.marketable.roots.t.ha*1000,
      yield_sp = r$Total.root.weight.t.ha*1000,
      yield_sb = r$Soybean.grain.yield.t.ha*1000,
      yield_pp = r$Pigeonpea.grain.yield.t.ha*1000,
      yield_marketable_sb = NA,
      yield_marketable_pp = NA
    )
  }
	
  d <- lapply(ff, proces)
  d <- do.call(rbind, d)
	
	d <- reshape(d, varying = list(c("yield_sp", "yield_sb", "yield_pp"), c("yield_marketable_sp", "yield_marketable_sb", "yield_marketable_pp"), c("fwy_total_sp", "fwy_total_sb", "fwy_total_pp")),
	              v.names = c("yield", "yield_marketable", "fwy_total"),
	              timevar = "crop",
	              times = c("sweetpotato", "soybean", "pigeon pea"),
	              direction = "long")
	
  d <- d[!is.na(d$yield),] 

  d$id <- NULL
  row.names(d) <- NULL
  
  i <- grepl("Sole", d$treatment)
  d$intercrops[i] <- "none"
  i <- grepl("SP\\+SB", d$treatment) & d$crop=="sweetpotato"
  d$intercrops[i] <- "soybean"
  i <- grepl("SP\\+PP|SP:PP", d$treatment) & d$crop=="sweetpotato"
  d$intercrops[i] <- "pigeon pea"
  i <- grepl("SP\\+PP|SP:PP", d$treatment) & d$crop=="pigeon pea"
  d$intercrops[i] <- "sweetpotato"
  i <- grepl("SP\\+SB", d$treatment) & d$crop=="soybean"
  d$intercrops[i] <- "sweetpotato"
  
  d$spatial_arrangement <- ifelse(grepl("1:1", d$treatment),"1:1",
                           ifelse(grepl("2:1", d$treatment), "2:1", 
                           ifelse(grepl("Row", d$treatment), "alternating row", "none")))
  
  ### Adding geo-coordinate
  
  geo <- data.frame(
    location = c("Chinguluwe", "Chipoka", "Kamwendo", "Boma", "Thuchila", "Thuchira"),
    longitude = c(34.213, 34.5119, 33.0403, 33.7905, 35.358, 35.309),
    latitude = c(-11.564, -13.993, -13.831, -13.948, -15.914, -15.971),
    geo_source = "Google Maps"
  )
  
  d <- merge(d, geo, by = "location", all.x = TRUE) 
  
  d$is_survey <- FALSE
  d$on_farm <- TRUE
  d$yield_isfresh <- TRUE
  d$irrigated <- NA
  d$yield_moisture <- NA_real_
  d$yield_part <- ifelse(grepl("sweetpotato", d$crop), "roots", "grain")
  d$country <- "Malawi"
  d$geo_from_source <- FALSE
  d$harvest_date <- NA_character_
  
  d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA) 
  

	carobiner::write_files(path, meta, d)
}


