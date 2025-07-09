# R script for "carob"


carob_script  <- function(path) {
	
"The objectives of this research were to estimate the impact of genotype × environment interactions (G×E) in sweetpotato and select genotypes based on drought indices such as geometric mean, percent yield reduction, drought sensitivity index and harvest index. Experiments were set up at Umbeluzi Research Station, 32 km South of Maputo. A total of 58 clones were evaluated during the dry season of 2006 and 2008. Two treatments were applied for this multi-year trial: full irrigation and without irrigation at the middle of root initiation growth stage. The field layout was a randomized complete block design with three replications. ‘Jonathan’, ‘Resisto’ and ‘Tanzania’ were the check cultivars in each treatment. Data were collected on storage root and vine yields in the field and DM in the quality laboratory using root samples from the fields. Computation of Harvest index, geometric means, drought sensitity index, drought intensity index as well as drought tolerance expression were made from the field data. The analysis of variance, regression and the additive main effects multiplicative interaction (AMMI) analyses, plus phenotypic coefficient of variation and ecovalence were used for dissecting the G×E and assessing the stability of each clone."
	
	uri <- "doi:10.21223/P3/M0HGJ4"
	group <- "varieties"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=11,
		publication= NA,# 
		data_organization = "CIP;IIAM;SLU",
		carob_contributor="Cedric Ngakou",
		carob_date="2023-11-06",
		data_type="experiment",
		response_vars = "yield",
		treatment_vars = "variety",
		project=NA 
	)
	
	r <- carobiner::read.excel(ff[basename(ff)=="Drought_06_08.xls"])  
	
	d <- r[,c("YEAR","TREATMENT", "REP", "CULTIVAR", "RYTHA", "FYTHA", "BIOM")]
	colnames(d) <- c("planting_date", "irrigation", "rep", "variety", "yield", "dmy_leaves", "dmy_total")
	
	d$crop <- "sweetpotato" 	
	d$country <- "Mozambique"
	d$adm1 <- "Maputo"
	d$location <-  "Umbeluzi"
	d$trial_id <- as.character(as.integer(as.factor(d$location)))
	d$yield_part <- "roots"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	i <- grepl("Non_Irrigation",d$irrigation)
	d$irrigated[i] <- FALSE
	d$irrigation <- NULL
	### add long and lat coordinate
	d$longitude <- 32.3777344
	d$latitude <- -26.0299562
	d$geo_from_source <- FALSE
	
	## fix yield value
	d$yield <- gsub("\\*",NA,d$yield)
	d$dmy_leaves <- gsub("\\*", NA, d$dmy_leaves)
	d$dmy_total <- gsub("\\*", NA, d$dmy_total)
	d <- d[!is.na(d$yield),] ## remove NA in yield	
	#data type
	d$rep <- as.integer(d$rep)
	d$yield <- (as.numeric(d$yield))*1000 # in kg/ha
	d$dmy_leaves <- (as.numeric(d$dmy_leaves))*1000 # in kg/ha
	d$dmy_total <- (as.numeric(d$dmy_total))*1000 # in kg/ha
	d$planting_date <- as.character(d$planting_date)
	
	### fix the bounds error 
	## d$dmy_leaves[d$dmy_leaves>20000] <- NA 
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	carobiner::write_files(meta, d, path=path)
	
}



