# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Sanitised human urine (Oga) as a fertilizer auto-innovation from women farmers in Niger

This is a data set of the of three years of field trial with sanitised human urine that was submitted to ASDE Journal for publication consideration purpose.
"


	uri <- "doi:10.5281/zenodo.4071827"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=NA,
		data_organization = "INRNA;UHO;IDEMS", # INRNA: National Institute of Agricultural Research of Niger # UHO:University of Hohenheim
		publication = "10.1007/s13593-021-00675-2",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_organic;P_organic;K_organic;Mg_organic;Ca_organic",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-09-20",
		carob_completion = 100,	
		carob_effort = 3
	)
	

	f1 <- ff[basename(ff) == "Data.xlsx"]

	r1 <- carobiner::read.excel(f1, sheet="2014")
	r2 <- carobiner::read.excel(f1, sheet="2015")
	r3 <- carobiner::read.excel(f1, sheet="2016")
	r4 <- carobiner::read.excel(f1, sheet="Rainfall")


	d1 <- data.frame(
		location = r1$Site,
		site = r1$Village,
		soil_type = r1$`Soil type`,
		weeding_done = !is.na(r1$`Weed management`),
		yield_c = as.numeric(gsub("^.$", NA, r1$`Control yield (kg/ha)`)),
		yield_trt = r1$`Treatment yield (kg/ha)`,
		planting_date = "2014",
		trial_id = "1"
	)


	d2 <- data.frame(
		location = r2$Site,
		site = r2$Village,
		soil_type = r2$`Soil type`,
		weeding_done = !is.na(r2$`Weed management`),
		yield_c = as.numeric(gsub("^.$", NA, r2$`Control yield (kg/ha)`)),
		yield_trt = as.numeric(gsub("^.$", NA, r2$`Treatment yield (kg/ha)`)),
		planting_date = "2015",
		trial_id = "2"
	)
	
	dd <- carobiner::bindr(d1, d2)
	
	dd <- reshape(dd, varying = c("yield_c", "yield_trt"), v.names = "yield",
	              timevar = "treatment",
	              times = c("conventional sowing", "OGA + OM"),
	              direction = "long")
  row.names(dd) <- dd$id <- NULL
  

	d3 <- data.frame(
	  treatment = gsub("Control", "conventional sowing", r3$Treatment),
	  location = r3$Site,
	  site = r3$Village,
	  soil_type = r3$`Soil type`,
	  yield = as.numeric(r3$`Yield (kg/ha)`),
	  planting_date = "2016",
	  trial_id = "3"
	)
	
	d <- carobiner::bindr(dd, d3)
	
	### From publication
	## Oga composition: 10.0 kg N, 0.8 kg P, 4.0 kg K, 6.0 kg Mg, 60.0 kg Ca, and
	#0.4 kg Fe per hecter
	
	
	i <- grepl("OGA", d$treatment)
	d$N_organic <- d$P_organic <- d$K_organic <- d$Mg_organic <- d$Ca_organic <- d$Fe_organic <- 0
	d$N_organic[i] <- 10
	d$P_organic[i] <- 0.8
	d$K_organic[i] <- 4
	d$Mg_organic[i] <- 6
	d$Ca_organic[i] <- 60
	d$Fe_organic[i] <- 0.4
	
	d$OM_used <- grepl("OM|OGA", d$treatment)
	d$OM_type <- ifelse(grepl("OGA \\+ OM", d$treatment), "animal dung;Oga",
	             ifelse(grepl("OGA", d$treatment), "Oga", "none"))
	
	### adding lon and lat coordinate 
	
	geo <- data.frame(
	  site = c("Garin Mai Gari", "Garin Labo", "Bokki", "Djangore"),
	  longitude = c(7.090064, 7.07433, 2.3264, 2.27002),
	  latitude = c(13.3702, 13.3439, 12.955, 12.9502),
	  geo_from_source = FALSE,
	  geo_source = "Google Maps",
	  geo_uncertainty = NA
	)
	d <- merge(d, geo, by = "site", all.x = TRUE) 
	
	### Fixing unknown experiment site name with location name
	
	geo1 <- data.frame(
	  location = c("Say", "Serkin Haussa", "Safo"),
	  long = c(1.8819,  7.5916, 7.112),
	  lat = c(12.9363, 13.8455, 13.4169),
	  geo_un = c(127830, NA, NA),
	  geo_s = c("GADM 4.1, adm2", rep("Google Maps", 2))
	  
	)
	
	d <- merge(d, geo1, by = "location", all.x = TRUE) 
	i <- is.na(d$longitude)|is.na(d$latitude)
	d$longitude[i] <- d$long[i]
	d$latitude[i] <- d$lat[i]
	d$geo_uncertainty[i] <- d$geo_un[i]
	d$geo_source[i] <- d$geo_s[i]
	
	d$geo_s <- d$geo_un <- d$lat <- d$long <- NULL
	
	########### locations are missing 
	#dw <- data.frame(
	#  dt1 = r4$`2014`,
	#  prec1 = r4$`Rainfall (mm)...2`,
	#  dt2 = r4$`2015...3`,
	#  prec2 = r4$`Rainfall (mm)...4`,
	#  dt3 = r4$`2015...5`,
	#  prec3 = r4$`Rainfall (mm)...6`
	#)
	
	#dw <- reshape(dw, varying = list(c(paste0("prec", c(1:3))), c(paste0("dt", c(1:3)))), v.names = c("prec", "date"), times = c("2014", "2015", "2015"), timevar = "date", direction = "long")
  #dw$id <- NULL

  ### 
	d$is_survey <- FALSE
	d$on_farm <- TRUE
	d$yield_moisture <- NA
	d$crop <- "pearl millet"
	d$yield_part <- "grain"
	d$country <- "Niger"
	d$irrigated <- NA
	d$yield_isfresh <- NA
	d$harvest_date <- NA
	
	
	d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA)
	
	###
	d <- unique(d)
	
	carobiner::write_files(path, meta, d)
}


