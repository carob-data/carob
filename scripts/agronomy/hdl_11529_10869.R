# R script for "carob"


carob_script <- function(path) {

"Conservation agriculture (CA) had recently gained popularity and promotion in the southern parts of Africa. Research has shown a number of benefits of CA in contrast to the widely practiced conventional ways (CP), which chiefly include water and soil conservation. These gains have positive benefits towards grain yield in maize. However, the maize varieties that performs better than others in these different environments have to be investigated and updated for farmers and breeding purposes. Furthermore, physiological traits that are suitable for the CA system needs to be dissected for breeding purposes. Hence a study was conducted across Zimbabwe at University of Zimbabwe farm (heavy red clay), Domboshawa Training centre (DTC) (sandy loamy soils), Madziva (sandy soils), Hereford (red clays) and Zimuto (sandy soils) from 2012 up to 2015. Investigations of effects of CA and CP practices on emergence, chlorophyll content, early vigour, biomass and grain yield of different maize varieties using 12 hybrids and 4 open pollinated varieties (OPVs) were conducted. Emergence was collected as the number of days taken by the different varieties to emerge. At 6 weeks after sowing a destructive sampling was performed to quantify the vigor of the maize varieties using averages of height, number of leaves per plant, dry matter and chlorophyll content using a SPAD meter. At harvesting grain yield and biomass yield were calculated."

	uri <- "hdl:11529/10869"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIMMYT",
		publication = "doi:10.5539/jas.v8n11p112",
		project = NA,
		data_type = "experiment",
		response_vars = "yield",
		treatment_vars = "land_prep_method;variety", 
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2024-05-30"
	)
	
	f <- ff[basename(ff) == "MOTHER TRIAL 2012-2015.xlsx"]

	r1 <- carobiner::read.excel(f, sheet = "emergence ")
	r2 <- carobiner::read.excel(f, sheet = "ASI")
	r3 <- carobiner::read.excel(f, sheet = "GRAIN ")
	r6 <- carobiner::read.excel(f, sheet = "BEFORE CP INTRODUCTION ")
	
	
## r4 seems to be for a sample during the season; and not especially relevant in this context.
#	r4 <- carobiner::read.excel(f, sheet = "EARLY VIGOUR ")

## not clear what biomass refers to (fresh or dry?) the numbers make no sense relative to yield
## so let's hope yield is ok
#	r5 <- carobiner::read.excel(f, sheet = "BIOMASS ")


# not used
#	r7 <- carobiner::read.excel(f, sheet = "varieties.treats ")
	

## process file(s)
	d1 <- data.frame(
		year=r1$YEAR,
		rawsite=r1$SITE, 
		land_prep_method=r1$SYSTEM,
		rep=r1$rep,
		treatment=r1$treat,
		emergence_days= r1$`DAYS TO 50% E`
	)
	
	d2 <- data.frame(
	  year=r2$YEAR,
	  rawsite=r2$SITE, 
	  land_prep_method=r2$SYSTEM,
	  rep=r2$rep,
	  treatment=r2$treat,
	  tassling_days= r2$tasseling,
	  silking_days=r2$silking, 
	  asi=r2$ASI
	)
	
	d3 <- data.frame(
	  year=r3$YEAR,
	  rawsite=r3$SITE, 
	  land_prep_method=r3$SYSTEM,
	  rep=r3$Replicate,
	  treatment=r3$Treatment,
	  yield=r3$`Grain yield`
	)

#	d4 <- data.frame(
#	  year=r4$YEAR,
#	  rawsite=r4$SITE,
#	  land_prep_method=r4$SYSTEM,
#	  rep=r4$rep,
#	  treatment=r4$treat,
#	  leaf_biomass=r4$`fresh wgt`,
#	  dmy_total=r4$`dry wght`,
#	  plant_height=r4$height
#	)	
	
#	d5 <- data.frame(
#	  year=r5$YEAR,
#	  rawsite=r5$SITE, 
#	  land_prep_method=r5$SYSTEM,
#	  rep=r5$Replicate,
#	  treatment=r5$Treatment,
#	  fresh_biomass=r5$Biomass
#	) 

	d6 <- data.frame(
	  year=r6$Year,
	  rawsite=r6$site, 
	  land_prep_method=r6$SYSTEM,
	  rep=r6$REP,
	  treatment=r6$TREAT,
	  yield= r6$`grain yield`  
	)
	
	dd <- merge(d1, d2, by=c("year", "rawsite", "land_prep_method", "rep", "treatment"), all.x=TRUE)
	dd <- merge(dd, d3, by=c("year", "rawsite", "land_prep_method", "rep", "treatment"), all.x=TRUE)
#	dd <- merge(dd, d4, by=c("year", "rawsite", "land_prep_method", "rep", "treatment"), all.x=TRUE) 
#	dd <- merge(dd, d5, by=c("year", "rawsite", "land_prep_method", "rep", "treatment"), all.x=TRUE)
	d  <- merge(dd, d6, by=c("year", "rawsite", "land_prep_method", "rep", "treatment", "yield"), all.x=TRUE)

	varietyname = c("SC 533", "Pristine 601", "Pannar 53", "Pannar 413", "ZM309",
	              "PGS 51", "Zap 61", "PHB 3253", "ZM 525", "ZM 401", "PGS 63",
	              "SC 637", "ZS 265", "SC 301", "SC 513", "ZS 261")
	
	d$variety <- varietyname[d$treatment]
	
#	d$land_prep_method <- gsub("CA", "conservation agriculture", d$land_prep_method)
	d$land_prep_method <- gsub("CA", "none", d$land_prep_method)
	d$land_prep_method  <- gsub("CP", "conventional", d$land_prep_method)
	
# from r4
# 	d$dmy_total <- gsub("5  2.2" , "52.2",d$dmy_total)
#    d$dmy_total <- as.numeric(d$dmy_total)
#    d$dmy_total[d$dmy_total < 0] <- NA
   
	#allocation of geo locations from publication
	d$country <- "Zimbabwe"
	geo <- data.frame(
		rawsite=c("DTC", "HFORD", "UZ", "MADZIVA", "ZIMUTO"),
		location=c("Domboshawa Training Centre", "Hereford", "Madziva", "University of Zimbabwe", "Zimuto"),
	    latitude=c(-18.0333, -17.7000, -17.0000, -17.4200, -20.4167),
	    longitude=c(31.2833, 31.7333, 31.7167, 31.0528, 31.4667),
	    elevation=c(1500, 1054, 1169, 1223, 1483),
		geo_from_source = FALSE
	)
	d <- merge(d, geo, by="rawsite", all.x = TRUE)
	d$rawsite <- NULL  
	  
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$trial_id <- "1"
	
	
##### Time #####

	d$planting_date <- substr(d$year, 1, 4)
	d$harvest_date <- paste0("20", substr(d$year, 6, 7))
	d$year <- NULL

##### Fertilizers #####
#fertilizer rates obtained from publication
   d$P_fertilizer <- 12.2 / 2.29
   d$K_fertilizer <-11.6 / 1.2051
   d$N_fertilizer <- 14 ## 83 ?
   d$N_splits <- 2
   d$fertilizer_type <- "D-compound; AN"
   d$inoculated <- FALSE
   d$plant_density <- 44444
   ##CN
   ## Others variables from publication (doi:10.5539/jas.v8n11p112)
   d$plant_spacing <- 50
   d$row_spacing <- 90 
   
	d$emergence_days[d$emergence_days == 0] <- NA

	d$rep <- as.integer(d$rep)
	d$N_splits <- as.integer(d$N_splits)
	d$treatment <- as.character(d$treatment)
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d)
}



