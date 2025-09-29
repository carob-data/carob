# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {

"Six years of data on genotype by tillage interaction and performance progress for 13 bread and 13 durum wheat genotypes on irrigated raised beds in Mexico.
  
Agronomic systems based on zero tillage and residue retention are becoming more important due to their potential for climate change adaptation through the reduction of soil erosion and improved water availability. Genotype by tillage interactions for yield are not well understood and it is unknown whether tillage should be an evaluation factor in breeding programs. Twenty-six CIMMYT bread (Triticum aestivum) and durum (Triticum turgidum) wheat genotypes, created between 1964 and 2009, were tested for yield and agronomic performance at CIMMYT’s experimental station near Ciudad Obregon, Mexico, over six years (harvest years 2010 to 2015). Treatments included conventional and permanent raised beds with full and reduced irrigation. The data set contains data on wheat performance (days to flowering and maturity, plant height, harvest index, grain yield, thousand grain weight, test weight, NDVI during early vegetative growth and maximum NDVI), performance traits calculated from these data (.e.g number of grains per m2, number of grains per spike, grain production rate from flowering to maturity) and monthly weather data during the study period (reference evapotranspiration, precipitation, maximum, minimum and average temperature)."

  uri <- "hdl:11529/10548474"
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = "doi: 10.1016/j.fcr.2017.11.011",
		project = NA,
		data_type = "experiment",
		treatment_vars = "land_prep_method",
		response_vars = "yield", 
		completion = 100,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2025-09-22",
		notes = NA, 
		design = NA)
	
	f <- ff[basename(ff) == "PUB-201-GxT-Database-2010-2015.xlsx"]
	r <- carobiner::read.excel(f, sheet ="Wheat data", na=".")
	gen_data <- carobiner::read.excel(f, sheet = "Table S1. Genotypes", skip=2)

	d <- data.frame(
	   country="Mexico",
		 planting_date = sub("-.*", "", r$year),
		 harvest_date= sub(".*-", "", r$year),
		 crop="wheat",
		 rep=as.integer(r$block),
		 land_prep_method = r$till,
		 irrigation_fulfullment = r$irr,
		 flowering_days = as.numeric(r$FLO),
		 maturity_days= r$MAT,
		 plant_height= r$HEI,
		 harvest_index= r$HI,
		 yield=r$YLD,
		 yield_part ="grain",
		 yield_moisture= 12,
		 spike_density=r$Spm2*10000,
		 seed_weight=r$TGW,
		 adm1 = "Sonora",
		 location = "Ciudad Obregón",
		 site = "INIFAP, Norman E. Borlaug Experimental Station",
		 latitude=27.368,
		 longitude = -109.928,
		 elevation=38
	)

	  d$on_farm <- FALSE
	  d$is_survey <- FALSE
	  d$irrigated <- TRUE
	  d$geo_from_source <- TRUE
    d$P_fertilizer <- 23
    d$N_fertilizer <- 278
    d$S_fertilizer <- d$K_fertilizer <-d$lime <- as.numeric(NA)
    d$variety <- gen_data$Name[ match(r$gen_code, gen_data$Genotype) ]
    d$trial_id <- paste0(d$location,"_",d$planting_date)
    d$land_prep_method <- gsub("CB","conventional tilled beds", d$land_prep_method)
    d$land_prep_method <- gsub("PB","permanent beds", d$land_prep_method)
    d$irrigation_fulfullment <- ifelse(d$irrigation_fulfullment=="FI","full irrigation","reduced irrigation")
    
	carobiner::write_files(path, meta, d)
}

