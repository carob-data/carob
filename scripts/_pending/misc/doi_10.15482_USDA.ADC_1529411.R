# R script for "carob"


carob_script <- function(path) {

"

    This dataset consists of growth and yield data for each season when sorghum [Sorghum bicolor (L.)] was grown at the USDA-ARS Conservation and Production Laboratory (CPRL), Soil and Water Management Research Unit (SWMRU) research weather station, Bushland, Texas 

"

	uri <- "doi:10.15482/USDA.ADC/1529411"
	group <- "conservation_agriculture"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		project=NA,
		publication= "",
		data_organization = "USDA-ARS",
   		data_type="experiment", 
		carob_contributor="Shumirai Manzvera",
		carob_effort = NA,
		carob_date="2023-10-06" 
	)

	f <- ff[basename(ff) == "_____________"]

	r <- read.csv(f)
	r <- readxl::read_excel(f) |> as.data.frame()

	
## process file(s)

## use a subset
	d <- carobiner::change_names(r, from, to)
  d<-X2015_W_Sorghum_Growth_and_Yield_V3
	
#### about the data #####
## (TRUE/FALSE)

	
	d$on_farm <- TRUE
	d$is_survey <-FALSE 
	d$is_experiment <- TRUE
	d$irrigated <- TRUE
## the treatment code	
	d$treatment <- 

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "United States"
	d$site <-"texas" 
	d$adm1 <- 
	d$adm2 <- 
	d$adm3 <- 
	d$elevation <- 1170 
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude <- -102.094189
	d$latitude <- 35.186714

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- "sorghum" 
	d$variety <- 



	d$yield <- d$`Dry grain yield in kg/ha`
	#what plant part does yield refer to?
	 
	
	carobiner::write_files(meta, d, path=path)
}


