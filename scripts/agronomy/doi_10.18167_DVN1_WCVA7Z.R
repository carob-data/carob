# R script for "carob"
# license: GPL (>=3)

## ISSUES
## DataCPWI20162018_Climate.csv file is not open 

carob_script <- function(path) {

"
Dataset on weed sampling, biomass and rice yield to determine critical period of weed interference in rainfed low-input cropping systems in the middle-west region of Madagascar

Full dataset (raw data calculated at plot level) on weed biomass, Weed cover, weed surveys and rice yield, tiller number and height based on a experiment to determine the critical period of weed interference in the middle-west region of Madagascar on two cropping seasons (2016/17 and 2017/18).
"

	uri <- "doi:10.18167/DVN1/WCVA7Z"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "SSSA; CIRAD; FOFIFA",
		publication = NA,
		project = NA,
		carob_date = "2025-12-04",
		design = NA,
		data_type = "experiment",
		treatment_vars = "land_prep_method",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 75,	
		notes = NA
	)
	
   
	f1 <- ff[basename(ff) == "DataCPWI_201718_RiceTillers&Height.csv"]
	f2 <- ff[basename(ff) == "DataCPWI20162018_WeedBiomass&Riceharvest.csv"]
	f3 <- ff[basename(ff) == "DataCPWI201618_WeedSurvey.csv"]
	#f4 <- ff[basename(ff) == "DataCPWI20162018_Climate.csv"]
	#f5 <- ff[basename(ff) == "DataCPWI20162018_readme.csv"]
	#f6 <- ff[basename(ff) == "DataCPWI20162018_SPADMeasurements.csv"]
	#f7 <- ff[basename(ff) == "DataCPWI20162018_WeedTypeBiomass.csv"]
	#f8 <- ff[basename(ff) == "DataCPWI20162018_YieldLoss.csv"]

	r1 <- read.csv(f1, sep=";")
	r2 <- read.csv(f2, sep=';')
	r3 <- read.csv(f3, sep=";")
	#r4 <- read.csv(f4, sep=";")
	#r5 <- read.csv(f5, sep=";")
	#r6 <- read.csv(f6, sep=";")
	#r7 <- read.csv(f7, sep=";")
	#r8 <- read.csv(f8, sep= ";")

## process 
	
	d1 <- data.frame(
	   land_prep_method = ifelse(grepl("CT", r1$SoilMgt), "conventional", "none"),
	   year = r1$Year,
	   planting_date = ifelse(grepl("CT", r1$SoilMgt) & grepl("2017", r1$Year), "2016-11-22",
	                   ifelse(grepl("CT", r1$SoilMgt) & grepl("2018", r1$Year), "2017-11-28", "2017-11-27")),
	   
	   harvest_date = ifelse(grepl("CT", r1$SoilMgt) & grepl("2017", r1$Year), "2017-03-21",
	                  ifelse(grepl("CT", r1$SoilMgt) & grepl("2018", r1$Year), "2018-04-04", "2018-03-29")),
	   
	   DAP = r1$DAS,
	   rep = r1$Block,
	   weeding_reg = r1$Weeding_regimes,
	   plant_height = r1$height_cm,
	   record_id = as.integer(1:nrow(r1)),
	   crop = "rice",
	   country = "Madagascar",
	   location = "Ivory-vakinankaratra",
	   longitude = 46.4164, 
	   latitude = -19.548, 
	   trial_id = paste(r1$Weeding_regimes,r1$SoilMgt, sep = "-"), 
	   on_farm = TRUE, 
	   is_survey = FALSE, 
	   yield_part = "grain", 
	   yield_moisture = 14, 
	   irrigated = NA, 
	   geo_from_source = FALSE
	   
	) 
	
	
###### process rice yield data
	
	d2 <- data.frame(
	   land_prep_method = ifelse(grepl("CT", r2$SoilMgt), "conventional", "none"),
	   year = r2$Year,
	   planting_date = ifelse(grepl("CT", r2$SoilMgt) & grepl("2017", r2$Year), "2016-11-22",
	                   ifelse(grepl("CT", r2$SoilMgt) & grepl("2018", r2$Year), "2017-11-28", "2017-11-27")),
	   
	   harvest_date = ifelse(grepl("CT", r2$SoilMgt) & grepl("2017", r2$Year), "2017-03-21",
	                  ifelse(grepl("CT", r2$SoilMgt) & grepl("2018", r2$Year), "2018-04-04", "2018-03-29")),
	   
	   rep = r2$Block,
	   weeding_reg = r2$Weeding_regimes,
	   yield = r2$Rice_yield*1000,
	   weed_biomass = r2$TotalWeedBiomass,
	   dmy_residue = r2$Straw_dryweight*1000,
	   plot_area = r2$HarvestedArea
	)
	
### merge d1 and d2
d <- merge(d1, d2, by= intersect(names(d1), names(d2)), all = TRUE)	
	
	### process weed cover

d3 <- data.frame(
   land_prep_method = ifelse(grepl("CT", r3$SoilMgt), "conventional", "none"),
   year = r3$Year,
   planting_date = ifelse(grepl("CT", r3$SoilMgt) & grepl("2017", r3$Year), "2016-11-22",
                   ifelse(grepl("CT", r3$SoilMgt) & grepl("2018", r3$Year), "2017-11-28", "2017-11-27")),
   
   harvest_date = ifelse(grepl("CT", r3$SoilMgt) & grepl("2017", r3$Year), "2017-03-21",
                         ifelse(grepl("CT", r3$SoilMgt) & grepl("2018", r3$Year), "2018-04-04", "2018-03-29")),
   
   rep = r3$Block,
   weeding_reg = r3$Weeding_regimes,
   weed_cover = r3$WeedCover,
   date = as.character(as.Date(r3$Date, "%d/%m/%Y"))
)

d <- merge(d, d3, by= intersect(names(d), names(d3)),all = TRUE)

### drop duplicate record_id 
d <- d[!duplicated(d$record_id),]

d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
d$year <- d$weeding_reg <- NULL



carobiner::write_files(path, meta, d)
}



