# R script for "carob"

## there is now a file for this dataset in soil samples

# putting this here

# this overlaps with 
# doi_10.25502_20180814_1446_HJ.R"
# doi_10.25502_20180814_1446_HJ.R
# and others. 
# But this dataset has the soil chemistry data. 
# So we should probably use this file as a basis and enrich with the other data sources


carob_script <- function(path) {
  
  "
  Omission trials conducted in 5 countries under AfSIS Phase 1 under CIAT
  "
			
	uri <- "doi:10.7910/DVN/C6DIIC"
	group <- "agronomy" 

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=5,
		project= "AfSIS", 
		publication = "doi:10.1016/j.agee.2016.05.012",
		data_organization = "CIAT",
		data_type="Multi-location trials",
		carob_contributor = "Robert Hijmans",
		carob_effort = NA,
		carob_date="2024-03-02"
	)
## read data
	## yield data
	r <- carobiner::read.excel(ff[basename(ff)=="02. DiagnosticTrials_2009-2012_SSA_Yld.xlsx"])
	
	d1<- r[,c("FieldID","Site","Season","Tcrop","Flat","Flong","TrtDesc","Rep","N","P","K","TGrainYld_Uncorr","TStoverYld")]

	d<-carobiner::change_names(d1,names(d1),c("trial_id","site","season","crop","latitude","longitude","treatment","rep","N_fertilizer","P_fertilizer","K_fertilizer","dmy_yield","dmy_residue"))
	
	## Soil data 
	r1 <- read.csv(ff[basename(ff)=="03. DiagnosticTrials_2009-2012_SSA_Wetchem.csv"])
	dd<- r1[,c("FieldID","Site","Country","Year","pH","m3.ECS","m3.Al","m3.B","m3.Ca","m3.Cu",
	           "m3.Fe","m3.K","m3.Mg","m3.Mn","m3.Na","m3.P","m3.S","m3.Zn","ExNa","ExCa","ExMg","ExK",
	           "psa.c4clay","psa.c4silt","psa.c4sand","Total_Carbon")]
	
	dd<- carobiner::change_names(dd, names(dd), c("trial_id","site","country","planting_date","soil_pH","soil_CEC","soil_ex_Al","soil_B","soil_ex_Ca","soil_Cu", "soil_Fe","soil_ex_K","soil_ex_Mg","soil_ex_Mn","soil_ex_Na","soil_P_available","soil_S","soil_Zn","soil_Na","soil_Ca","soil_Mg","soil_K", "soil_clay","soil_silt","soil_sand","soil_C"))

	dd<- dd[(dd$planting_date!="10LR" & dd$planting_date!="10SR"),] ## remove unknown terms in planting_date  
	### merge 
	d<- merge(d,dd,by=c("trial_id","site"),all.x = TRUE)
	d<- d[!is.na(d$country),] ### remove rows with Na in country after merge. This is because unique of trial_id in yield data is slightly different from unique of trial_id in soil data
	d$crop<- carobiner::fix_name(d$crop,"lower")
	d$yield <- (d$dmy_yield/0.85)*1000 ## in kg/ha
	d$dmy_residue <- (d$dmy_residue)*1000 ## in Kg/ha 
	d$dmy_yield <-NULL
	### Add columns 
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$plant_spacing[d$crop=="maize"] <- 25 # from doi:10.1016/j.agee.2016.05.012
	d$row_spacing[d$crop=="maize"] <-   75 
	d$plant_spacing[d$crop=="sorghum"] <- 50
	d$row_spacing[d$crop=="sorghum"] <-   80 
	d$yield_part <- "grain"
	## fix longitude and latitude 
	geo<- data.frame(site=c("Finkolo","Kandara","Kasungu","Kiberashi","Thuchila"),
	                 lat=c(11.2287362,-0.8822624,-12.9924877,-5.375225,-15.887993),
	                 long=c(-6.1975996,37.0249341,33.4723567,37.4430921,35.5828978))
	d<- merge(d,geo,by="site",all.x = TRUE)
	d$longitude[!is.na(d$long)]<- d$long[!is.na(d$long)] 
	d$latitude[!is.na(d$lat)]<- d$lat[!is.na(d$lat)] 
	d$long<- d$lat <- NULL
	
	## Fix rows with NA in fertilizer 
	fert<- data.frame(treatment=c("NP","NPK+MN","Control","NPK+Manure","NPK+Lime","PK","NK","NPK"),
	                  N=c(100,100,0,100,100,0,100,100),
	                  P=c(30,30,0,30,30,30,0,30),
	                  K=c(0,60,0,60,60,60,60,60))
	d<- merge(d,fert,by="treatment",all.x = TRUE)
   d$N_fertilizer[is.na(d$N_fertilizer)] <- d$N[is.na(d$N_fertilizer)]
   d$P_fertilizer[is.na(d$P_fertilizer)] <- d$P[is.na(d$P_fertilizer)]
   d$K_fertilizer[is.na(d$K_fertilizer)] <- d$K[is.na(d$K_fertilizer)]
   d$N<- d$P<- d$K <- NULL
	## data type
	d$rep <- as.integer(d$rep)
	d$season <- as.character(d$season)
	d<- d[!is.na(d$yield),]
	carobiner::write_files(meta, d, path=path)

}


