# R script for "carob"
# license: GPL (>=3)

## NOTES
# 26 measured variables across a 3-season maize biochar/basanite trial;
# Selected only: Yield, root biomass, bulk density, and Striga
# Root Biomass and Soil Bulk Density were measured once for the whole trial

# Suggested new terms"
#  striga_count
#  amend_rate
#  ammendment_type



carob_script <- function(path) {

"
Data for: Effect of biochar and basanite on maize yield,
fertilizer efficiency and phosphorus sorption in a tropical Oxisol (PyMiCCS Kenya field trial)

# Field trial and in-vitro batch experiment data; Effect of biochar and 
basanite on maize yield, fertilizer efficiency and phosphorus sorption in a tropical Oxisol &nbsp; 
This dataset contains the raw experimental data underlying the manuscript 'Effect of biochar and basanite on maize yield,
fertilizer efficiency and phosphorus sorption in a tropical Oxisol' by Meyer zu Drewer, Roobroeck, Maritim, Lotz, Kammann, 
Hartmann, Broszat, Schmidt, and Hagemann.; 
The data were collected during a three-season maize field trial on an Oxisol in Siaya, western Kenya (2023 - 2025), 
and during a complementary six-week in-vitro phosphorus sorption batch experiment.
"

	uri <- "doi:10.5281/zenodo.19929127"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=6, minor=NA,
	          data_organization = "Ithaka Institute",
	          publication = NA,
	          project = "PyMiCCS",
	          design = "9 treatments (controls, biochar, basanite, co-application)",
	          data_type = NA,
	          treatment_vars = "treatment; N_fertilizer; P_fertilizer; K_fertilizer",
	          response_vars = "yield; dmy_residue",
	          notes = NA,
	          carob_contributor = "Stella Muthoni",
	          carob_date = "2026-08-30",
	          carob_completion = 75,
	          carob_effort = 3
	)
	
	f1 <- ff[basename(ff) == "Data_Meyer_zu_Drewer_Kenya.csv"]          # field trial, long format
	f2 <- ff[basename(ff) == "Data_Meyer_zu_Drewer_P-sorption.csv"]     # in-vitro P-sorption batch experiment, long format
	f3 <- ff[basename(ff) == "readme_Meyer-zu-Drewer_fin.rtf"]          # Meta-data
	
	r1 <- read.csv(f1, sep=";", skip=1)
	r2 <- read.csv(f2, sep=";")
	
	# Create the yield trial database
	## season 1 N:P:K rates (kg/ha) - season 2-3 differs for the amended treatments (P/K rise from 0/0 to 10/20)
	trt <- data.frame(
	  treatment = c("Zero Control","Control","PK-half","PK-full","RE-BC_5","RE-BC_10","Co-Ap_10","BC_5","RP_5"),
	  N_fertilizer = c(0,90,90,90,90,90,90,90,90),
	  P_fertilizer = c(0,0,10,20,0,0,0,0,0),
	  K_fertilizer = c(0,0,20,40,0,0,0,0,0),
    amendment_type = c("none","none","none","none","rock-enhanced biochar","rock-enhanced biochar","biochar+basanite co-application","biochar","basanite (rock powder)"),
	  amend_rate = c(0,0,0,0,5,10,10,5,5),
	  OM_used = c(FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,FALSE)   # TRUE only for biochar-containing treatments
	)
	trt$OM_amount <- ifelse(trt$OM_used, trt$amend_rate * 1000, NA)   # kg/ha
	
	trt2 <- trt
	trt2$P_fertilizer[5:9] <- 10
	trt2$K_fertilizer[5:9] <- 20
	trt <- rbind(data.frame(season=1, trt), data.frame(season="2", trt2), data.frame(season="3", trt2))

	# get season 
	r1$season <- gsub("\\D", "", r1$parameter)

	# make wide
	s <- r1[grepl("season", r1$parameter) & !grepl("Soil pH|Striga", r1$parameter), ]
	s$parameter <- gsub(" 1st season| 2nd season| 3rd season", "", s$parameter)
	s <- reshape(s, timevar = "parameter",  idvar = c("season", "treatment", "subgroup", "replicate"),  direction = "wide")

	d1 <- data.frame(
		treatment = s$treatment, 
		replicate = s$replicate,
		season = s$season,
		yield = s$`value.Grain Yield t ha` * 1000, 
		dmy_residue = s$`value.Stover Yield t ha`  * 1000
	)
    d1 <- merge(d1, trt, by=c("treatment", "season"), all.x=TRUE)
	
	#Bulk density - Measured once 
	bd <- r1[r1$parameter == "Soil Bulk Density g cm3", c("treatment","replicate","value")]
	names(bd)[3] <- "soil_bd"
	d1 <- merge(d1, bd, by=c("treatment","replicate"), all.x=TRUE)
	
	# Roots biomass - measured once for the whole trial
	roots <- r1[r1$parameter == "Root Biomass g dw", c("treatment","replicate","value")]
	names(roots)[3] <- "dw_roots"
	d1 <- merge(d1, roots, by=c("treatment","replicate"), all.x=TRUE)
	
	## Soil pH, wide - averaged per treatment+season
	ph_exsitu_s1 <- r1[r1$parameter == "Soil pH ex-situ season 1", c("treatment","value")]
	ph_exsitu_s1$season <- "1"
	ph_exsitu_s2 <- r1[r1$parameter == "Soil pH ex-situ season 2", c("treatment","value")]
	ph_exsitu_s2$season <- "2"
	ph_insitu <- r1[r1$parameter == "Soil pH in-situ season 1", c("treatment","value")]
	ph_insitu$season <- "1"
	
	ph_all <- rbind(ph_exsitu_s1, ph_exsitu_s2, ph_insitu)
	ph_mean <- aggregate(value ~ treatment + season, data=ph_all, FUN=mean)
	names(ph_mean)[names(ph_mean)=="value"] <- "soil_pH"
	
	d1 <- merge(d1, ph_mean, by=c("treatment","season"), all.x=TRUE)
	
	## Extract the striga data - long format,
	## DAP assumed as week_number * 7
	striga_s2 <- r1[r1$parameter == "Striga count season 2", c("treatment","replicate","subgroup","value")]
	names(striga_s2) <- c("treatment","replicate","week","striga_count")
	striga_s2$trial_id <- "2"
	
	striga_s3 <- r1[r1$parameter == "Striga count season 3", c("treatment","replicate","subgroup","value")]
	names(striga_s3) <- c("treatment","replicate","week","striga_count")
	striga_s3$trial_id <- "3"
	
	d_striga <- rbind(striga_s2, striga_s3)
	d_striga$DAP <- as.integer(gsub("\\D", "", d_striga$week)) * 7L
	d_striga$week <- NULL
	names(d_striga)[names(d_striga)=="replicate"] <- "rep"
	d_striga$plot_id <- paste(d_striga$trial_id, d_striga$treatment, d_striga$rep, sep="_")
	d_striga$treatment <- NULL
	d_striga$rep <- NULL
	
	d1$crop <- "maize"
	d1$country <- "Kenya"
	d1$adm1 <- "Siaya"
	## Georeference Siaya county (adm1) per https://carob-data.org/contribute/georeference.html
	d1$longitude = 34.2488
	d1$latitude = -0.0546
	d1$geo_uncertainty = 44931
	d1$geo_source = "GADM 4.1, adm1"
	d1$geo_from_source <- FALSE
		
	d1$is_survey <- FALSE
	d1$on_farm <- TRUE
	d1$irrigated <- FALSE
	d1$planting_date <- NA
	d1$harvest_date <- NA
	d1$yield_moisture <- NA
	d1$yield_isfresh <- NA
	d1$yield_part <- "grain"
	
	names(d1)[names(d1)=="replicate"] <- "rep"
	names(d1)[names(d1)=="season"] <- "trial_id"
	
	d1$plot_id <- paste(d1$trial_id, d1$treatment, d1$rep, sep="_")
	
	carobiner::write_files(path, meta, wide=d1, long=d_striga)
}


