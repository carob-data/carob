# R script for "carob"

carob_script <- function(path) {
   
"The database contains data about on-farm trials with transplanted rice were conducted during monsoon ('Aman') season in 2016 and 2017 and winter ('Boro') season in 2016 to 2017 in agroecological zones (AEZs) 11 and 12 of south-west Bangladesh with ten treatments - seven herbicide-based IWM options, one mechanical weed control-based option, and two checks – farmers' current weed control practice and weed-free, to assess effects on weed control, grain yield, labor use, and profitability. (2021-07-09)"
   
   uri <- "hdl:11529/10548600"
   group <- "pest_disease" 
   ff <- carobiner::get_data(uri, path, group)
  
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		publication= NA,
		data_organization = "CIMMYT",
		carob_contributor="Cedric Ngakou",
		carob_effort = NA,
		carob_date="2023-09-27",
		data_type="experiment",
		response_vars= "yield",
		treatment_vars="herbicide_product",
		project=NA 
	)
   
   
	bn <- basename(ff)
 	r0 <- read.csv(ff[bn=="Aman_weed_data.csv"]) 
	r1 <- read.csv(ff[bn=="Aman_Yield_and_economics.csv"]) 
	r2 <- read.csv(ff[bn=="Boro_weed_data.csv"]) 
	r3 <- read.csv(ff[bn=="Boro_yield_and_economics-1.csv"])
   
   ### process Aman_weed_data file
#   dd <- r[,c("Season","Replication","Treatments","SITE","TWbS")]
#   colnames(dd) <- c("season","rep","Treatment","site","weed_biomass")
 
	dd0 <- data.frame(
		season = r0$Season,
		rep = r0$Replication,
		Treatment = r0$Treatments,
		location = r0$SITE,
		weed_biomass = r0$TWbS
	)

	treat <- data.frame(
   		Treatment= 1:10,
		weeding_done = TRUE,
		treatment= c(
			"farmers practices + pretilachlor fb twice hand-weeding", 
			"pendimethalim fb hand-weeding", 
			"mefenacet + bensulfuron fb hand-weeding", 
			"mefenacet + bensulfuron fb MW", 
			"bispyribac-sodium fb hand-weeding", 
			"penoxsulam fb hand-weeding", 
			"mefenacet+bensulfuron fb bispyribac-sodium fb hand-weeding", 
			"mefenacet + ben sulfuron fb penoxulam fb hand-weeding",
			"MW fb HW",
			"Weed-free by frequent hand-weeding"
		), 
		herbicide_product = c(
			"pretilachlor", 
			"pendimethalin", 
			"mefenacet;bensulfuron", 
			"mefenacet;bensulfuron", 
			"bispyribac-sodium", 
			"penoxsulam", 
			"mefenacet;bensulfuron;bispyribac-sodium", 
			"mefenacet;bensulfuron;penoxsulam",
			"none",
			"none"
		),		
		herbicide_used = c(rep(TRUE, 8), FALSE, FALSE)
	)
	
    ### process Aman_Yield_and_economics file()
	dd1 <- data.frame(
		season=r1$Season,
		rep = r1$Replication,
		Treatment = r1$Treatment,
		location = r1$SITE,
		yield = r1$Grain.yield..t.ha.
	)
	
   ## merge dd and dd1 
   d1 <- merge(dd0, dd1, by=c("season", "Treatment", "location", "rep"))
   d1$variety <- "BRRI dhan49"
   d1$planting_date <- "2016-07-15"
   d1$trial_id <- "1"
 
 ### process Boro_weed_data file()
   #dd2 <- r2[,c("Season","Replication","Treatment","Site","TWbS")]
   #colnames(dd2) <- c("season","rep","Treatment","site","weed_biomass")

	dd2 <- data.frame(
		season=r2$Season,
		rep = r2$Replication,
		Treatment = r2$Treatment,
		location = r2$Site,
		weed_biomass = r2$TWbS
	)

   ### process Boro_yield_and_economics-1 file()
#   dd3 <- r3[,c("Season","Replication","Treatment","SITE","Grain.yield..t.ha.")]
#   colnames(dd3) <- c("season","rep","Treatment","site","yield")

	dd3 <- data.frame(
		season=r3$Season,
		rep = r3$Replication,
		Treatment = r3$Treatment,
		location = r3$SITE,
		yield = r3$Grain.yield..t.ha.
	)
	
	## merge dd2 and dd3 
	d2 <- merge(dd2, dd3, by=c("season", "Treatment", "location", "rep"))
	d2$variety <- "BRRI dhan28"
	d2$planting_date <- "2016-12-10"
	d2$trial_id <- "2"

	#### join d1 and d2
	d <- rbind(d1, d2)
	
	d$yield <- d$yield*1000  ## kg/ha
	d$weed_biomass <- d$weed_biomass * 10 # kg/ha 
	# add columns
	d$country <- "Bangladesh"
	d$crop <- "rice" 
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	
	### add long and lat coordinate
	
	d$longitude[d$location=="Faridpur"] <- 89.4427
	d$latitude[d$location=="Faridpur"] <- 24.1606
	d$longitude[d$location=="Jashore"] <- 89.2094
	d$latitude[d$location=="Jashore"] <- 23.1665
	
	d$yield_part <- "grain" 
	
	d <- merge(d, treat, by="Treatment")
	d$Treatment <- NULL

	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	carobiner::write_files(meta, d, path=path)
	
}
