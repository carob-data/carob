# R script for "carob"
# license: GPL (>=3)

## ISSUES
# one case of above normal rice yield (25000 kg/ha)

carob_script <- function(path) {

"
Analysis of Household-Level Survey Data: Farm Characteristics and Resource Allocation in Laos PDR (2024)

Processed dataset from a household-level survey describing the main farm characteristics, production, and resource allocation in two municipalities.  The survey covers 300 farms across three districts (Kham, Moke, and Nonghet) in Xieng Khouang Province, collected between December 2023 and May 2024.
"

	uri <- "doi:10.71682/10549336"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT; LFN",
		publication = "hdl:10883/35403",
		project = NA,
		design = "Online survey",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-06-26",
		carob_completion = 90,	
		carob_effort = 6
	)
	
	f <- ff[basename(ff) == "Laos 2024 farm characterization for typologies MFS.xlsx"]
	r <- carobiner::read.excel(f, sheet="data", na=c("", "NA"))

	d <- data.frame(
		hhid = r$f_id,
		country="Laos",
		adm2=r$district,
		adm3=r$village,
		age=r$f_age_i,
		education=r$school_i,
		hh_size=r$hh_members,
		hh_adult_women=r$hh_memwom_s,
		farmland=r$surface_ha,
		cropland_owned=r$ownland_ha,
		land_irrigated=r$irri_ha,
		cattle=r$livcow23_n,
		pig=r$livpig23_n,
		chicken=r$livpouktry23_n,
		TLU=r$tlu_2023, #capturing previously recorded TLU at the time of the survey
		hh_income=as.numeric(r$inco_mon) * 12,
		currency="LAK",
		farm_labour=r$crop_lab,
		maize=r$yield_maizem, #estimated yield
		rice=r$yield_uplandm, #estimated yield
		rice_irri=r$yield_paddym #estimated yield
	)
	
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- FALSE
	
	d$adm2 <- gsub("Nonghet","Nonghed",d$adm2)
	d$adm2 <- gsub("Moke","Morkmay" ,d$adm2)
	##xy <- carobiner::adm_pointRadius("Laos", 2)
	##s <- xy[xy$adm2 %in% c("Kham","Morkmay","Nonghed"), ]
	##carobiner::dfput(s, name="geo", drop="country")
	
	loc <-  data.frame(
		adm2 = c("Kham", "Morkmay", "Nonghed"),
		longitude = c(103.6574, 103.9981, 103.9249),
		latitude = c(19.7589, 19.0724, 19.5603),
		geo_uncertainty = c(35266, 37324, 47903),
		geo_source = c("GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2")
	)
	
	d <- merge(d,loc,by="adm2", all.x = T)
	
	d$planting_date <- as.character(NA)
	d$harvest_date  <- as.character(NA)
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)

	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE
  
  
  #extract midpoint from range of values
  age_values<- c(
    "0"  =21, #18-24
    "0.3"=30, #"25-34"
    "0.6"=40, #"35-44"
    "0.9"=50, #"45-54"
    "1.2"=60, #"55-64"
    "1.5"=65  #>65
  )

  d$age <- age_values[as.character(d$age)]
  
  #cleaning education
  edu_values <- c("0"="none", "0.5"="primary", "1"="secondary", "1.5"="high school", "2"="postgrad")
  d$education <- edu_values[as.character(d$education)]
  
# % of women to number of women in a household
  d$hh_adult_women <- (d$hh_adult_women * d$hh_size) / 100
    
  #re-shaping livestock data from wide to long
  x <- reshape(d[, c("hhid", "cattle", "pig", "chicken")], varying = c("cattle", "pig", "chicken"), v.names = "heads", 
			timevar = "animal", times = c("cattle", "pig", "chicken"),  idvar = "hhid", direction = "long")
  rownames(x) <- NULL
  x$heads <- as.numeric(x$heads)
  
  #reshaping crop data to long format
  y <- reshape(d[, c("hhid", "maize", "rice", "rice_irri")], varying = c("maize", "rice", "rice_irri"), v.names = "yield",
			timevar = "crop", times = c("maize", "rice", "rice_irri"), idvar = "hhid", direction = "long")
  y$yield <- y$yield * 1000 #converting to kg/ha
  y <- y[!is.na(y$yield), ]
  y <- y[y$yield > 0, ]
  y$irrigated <- NA
  y$irrigated[y$crop=="rice"] <- FALSE
  y$irrigated[y$crop=="rice_irri"] <- TRUE
  y$crop[y$crop=="rice_irri"] <- "rice"
  
  xy <- carobiner::bindr(x, y)
  xy$id <- NULL
  
  d$maize <- d$rice <- d$rice_irri <- d$cattle <- d$pig <- d$chicken <- NULL

  carobiner::write_files(path, meta, d, long=xy)
}
