# R script for "carob"

carob_script <- function(path) {

"
Performance of Maize Following Groundnut Varieties at Different Densities

This dataset is generated from maize response trials (legume/maize rotation system) that were conducted in Linthipe EPA, Dedza district, and Ntubwi EPA in Machinga district in Malawi, 2019/2020 cropping season. In the previous season, certifies and recycled seed of different varieties of groundnut were grown. In the current season, maize was planted as a test crop.
"

	uri <- "doi:10.7910/DVN/1T4Q3F"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "LUANAR",
		publication= NA,
		project=NA,
		data_type= "experiment",
		treatment_vars = "crop_rotation;variety_code",
		response_vars="yield",
		carob_contributor= "Shumirai Manzvera",
		carob_date="2024-02-22",
		modified_by="Siyabusa Mkuhlani",
		last_modified = "2024-07-31"
	)
	f0 <- ff[basename(ff) == "Linthipe_Dedza_MaizeResopnsetoGroundnut_2019_2020.csv"]
	f1 <- ff[basename(ff) == "Mtubwi_Machinga_MaizeResopnsetoGroundnut_2019_2020.csv"]
	r0 <- read.csv(f0)
	r1 <- read.csv(f1)

## use a subset
	d0 <- data.frame(
		adm2=r0$District, 
		adm3=r0$EPA, 
		rep=r0$REPLICATION,
		variety_code=r0$maize.variety.planted.in.2019.2020.season,
	    previous_crop="groundnut", 
		crop_rotation="maize; groundnut",
	    dmy_total=r0$Total.biomass..kg.ha.,
		yield=r0$Grain.yld.ha, 
		treatment=r0$'TREATMENT.NAME..groundnut.planted.in.previuos.season.',
		longitude=34.1253751, 
		latitude=-14.1832077, 
		trial_id="1"
	)
					
	d1 <- data.frame(
		adm2=r1$District, 
		adm3=r1$EPA, 
		rep=r1$REPLICATION,
		variety_code=r1$maize.variety.2019.2020.season,
	    previous_crop="groundnut", 
		crop_rotation="maize; groundnut",
	    dmy_total=r1$Total.biomass..kg.ha.,
		yield=r1$Grain.yld..kg.ha.,
		treatment=r1$'PREVIOUS.TREATMENT..2018.2019.season.',
		longitude =35.5737, 
		latitude = -14.9458, 
		trial_id="2"
	)
		
	d <- carobiner::bindr(d0, d1)
	d$geo_from_source <- FALSE
	
	d$country<- "Malawi"
	
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- FALSE

	d$planting_date <- "2019"
	d$harvest_date  <- "2020"

	d$crop <- "maize"
	d$yield_part <- "grain"
	d$dmy_total <- gsub(",", "", d$dmy_total)
	d$dmy_total <- as.numeric(d$dmy_total)
	d$treatment <- as.character(d$treatment)
	
	#No fertilizer application information was provided.
	#From: Communication with one of the data authors R.Chikowo.
	#Basal dressing: 200kg/ha of 23:10:5+6S +1Zn-Across all treatments; 
	#Top dressing: Urea 50kg/ha for maize after groundnut rotation; 
	#100 kg/ha for for maize-maize monoculture.
	
	#The data set only shows maize-groundnut rotation treatments but does not show
	#which treatments had maize-maize. Hence fert rates are based on on maize-groundnut rotation.
	
	#The fertilizer type has no official classification e.g. D-compound, though the nearest is D-compound. https://agra.org/wp-content/uploads/2020/08/Malawi-Report_Assessment-of-Fertilizer-Distribution-Systems-and-Opportunities-for-Developing-Fertilizer-Blends.pdf
	d$fertilizer_type<- 'D-compound; urea'
	d$N_fertilizer <- 0.23*200 + 23
	d$P_fertilizer <- 0.1*200*0.437
	d$K_fertilizer <- 0.05*200
	d$K_fertilizer <- 0.06*200
	d$K_fertilizer <- 0.01*200
	
	carobiner::write_files(meta, d, path=path)
}



