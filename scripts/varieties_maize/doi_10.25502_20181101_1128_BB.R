# R script for "carob"



carob_script <- function(path) {
   
"Drought and Striga are principal constraints to maize (Zea mays L.) production in sub-Saharan Africa. An early yellow maize population, TZE-Y Pop DT STR, which had undergone five cycles of selection for resistance to Striga, followed by three cycles of improvement for drought tolerance, was investigated for yield gains, changes in genetic variance, and interrelationships among traits under drought stress and optimum environments. Two hundred and forty S1 lines comprising 60 each from the base population and subsequent populations from three selection cycles improved for grain yield and drought tolerance were assessed under drought and optimal environments in Nigeria from 2010 to 2012. Genetic improvements in grain yield of 423 and 518 kg ha−1 cycle−1 were achieved under drought stress and optimal environments. Predicted improvements in selection for yield were 348 and 377 kg ha−1 cycle−1 under drought stress and optimum environments, respectively. The highest yield observed in C3 was accompanied by reduced days to silking and anthesis–silking interval, improved plant aspect and ear aspect, and increased plant height and ears per plant across research environments, as well as improved stay-green characteristic under drought. The level of genetic variability for yield and a few other traits were maintained under drought and optimal environments in the population.The presence of residual genetic variability for yield and other assayed traits in C3 indicated that progress could be made from future selection in the population depending on the ability of breeders to identify outstanding genotypes and the precision level of experimentation. Substantial improvement has been made in yield and drought tolerance in C3 of the population."
   
	uri <-  "doi:10.25502/20181101/1128/BB"
	group <- "varieties_maize" 
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		publication= NA, # 
		data_organization = "IITA", 
		carob_contributor="Cedric Ngakou", 
		carob_date="2023-10-11", 
		data_type="experiment", 
		response_vars = "yield",
		treatment_vars = "variety",
		project=NA 
	)
	
	
	bn <- basename(ff)
	
	# read Yellow_lines_BBA_DS dataset
	r <- read.csv(ff[bn==tolower("20181029aao_S1_Yellow_lines_BBA_DS.csv")])  
	
	d1 <- r[, c("ID", "Country", "Location", "Study", "Year", "Rep", "Entry", "Pedigree", "Yield", "ASI", "PLTH", "EHT", "PASP", "EASP", "DS", "HC")]#
	colnames(d1) <- c("ID", "country", "location", "treatment", "planting_date", "rep", "variety_code", "variety", "yield", "asi", "plant_height", "ear_height", "p_asp", "e_asp", "silking_days", "husk")#, 
	
	
	# read Yellow_lines_BBA_ww dataset
	r1 <- read.csv(ff[bn=="20181029aao_S1_Yellow_lines_BBA_WW.csv"])  
	
	d2 <- r1[, c("ID", "country", "Location", "Study", "YEAR", "Rep", "Entry", "Pedigree", "YIELD", "ASI", "PLTH", "EHT", "PASP", "EASP", "DS", "HC")]#

	colnames(d2) <- c("ID", "country", "location", "treatment", "planting_date", "rep", "variety_code", "variety", "yield", "asi", "plant_height", "ear_height", "p_asp", "e_asp", "silking_days", "husk")#, 
	
	# append d1 and d2
	d <- rbind(d1, d2)
	
	# add columns

	d$crop <- "maize" 
	
	d$trial_id <- paste(d$ID, d$location, sep = "-")
	d$yield_part <- "grain"
	d$on_farm <- TRUE
	d$irrigated <- FALSE
	d$borer_trial <- FALSE
	d$striga_infected <- FALSE
	d$ID <- NULL
	d$striga_trial <- TRUE
	
	### add long and lat coordinate
	i <- which(d$location=="Ikenne")
	d$longitude[i] <- 3.698
	d$latitude[i] <- 6.901
	
	i <- which(d$location=="kadawa")
	d$longitude[i] <- 8.434
	d$latitude[i] <-  11.633
	
	#data type
	d$variety_code <- as.character(d$variety_code)
	d$planting_date <- as.character(d$planting_date)
	d$is_survey <- FALSE
	
	carobiner::write_files(meta, d, path=path)
	
}


