# R script for "carob"
# license: GPL (>=3)

## NOTES
# 26 measured parameters across a 3-season maize biochar/basanite trial;
# Selected only: Yield, root biomass, bulk density, and Striga
# Root Biomass and Soil Bulk Density were measured once for the whole trial

# Suggested new terms"
#                      str_co4-8 (different weeks of striga count)
#                      amend_rate
#                      ammendment_type

## ISSUES
# No coordinates anywhere in source - Location only says Siaya Kenya
# id: plot_id(s) do not match between long and wide records
#                   caused by in-situ pH, which has 9 replicates per treatment (vs 3 everywhere else in the trial).


carob_script <- function(path) {

"
Data for: Effect of biochar and basanite on maize yield,
fertilizer efficiency and phosphorus sorption in a tropical Oxisol (PyMiCCS Kenya field trial)

# Field trial and in-vitro batch experiment data &mdash; Effect of biochar and 
basanite on maize yield, fertilizer efficiency and phosphorus sorption in a tropical Oxisol &nbsp; 
This dataset contains the raw experimental data underlying the manuscript 'Effect of biochar and basanite on maize yield,
fertilizer efficiency and phosphorus sorption in a tropical Oxisol' by Meyer zu Drewer, Roobroeck, Maritim, Lotz, Kammann, 
Hartmann, Broszat, Schmidt, and Hagemann. &nbsp; 
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
	                                design = "9 treatments (controls, biochar, basanite, co-application), 3 replicates, 3 seasons, Siaya, Kenya, 2023-2025.",
	                                data_type = NA,   # on-farm vs on-station not stated in source - not guessed
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
	trt_s1 <- data.frame(
	  treatment = c("Zero Control","Control","PK-half","PK-full","RE-BC_5","RE-BC_10","Co-Ap_10","BC_5","RP_5"),
	  N = c(0,90,90,90,90,90,90,90,90),
	  P = c(0,0,10,20,0,0,0,0,0),
	  K = c(0,0,20,40,0,0,0,0,0),
	  fertilizer_type = "unknown",   
	  amendment_type_ = c(NA,NA,NA,NA,"rock-enhanced biochar","rock-enhanced biochar","biochar+basanite co-application","biochar","basanite (rock powder)"),
	  amend_rate = c(NA,NA,NA,NA,5,10,10,5,5),
	  OM_used = c(FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,FALSE)   # TRUE only for biochar-containing treatments
	)
	names(trt_s1)[names(trt_s1) %in% c("N","P","K")] <- c("N_fertilizer","P_fertilizer","K_fertilizer")
	trt_s1$OM_amount <- ifelse(trt_s1$OM_used, trt_s1$amend_rate * 1000, NA)   # t/ha -> kg/ha
	
	trt_s23 <- trt_s1
	trt_s23$P_fertilizer[5:9] <- 10
	trt_s23$K_fertilizer[5:9] <- 20
	
	# Select relevant carob terminag per season
	d1_s1 <- merge(
	  setNames(r1[r1$parameter == "Grain Yield 1st season t ha", c("treatment","replicate","value")], c("treatment","replicate","yield")),
	  setNames(r1[r1$parameter == "Stover Yield 1st season t ha", c("treatment","replicate","value")], c("treatment","replicate","dmy_residue")),
	  by = c("treatment","replicate"), all = TRUE)
	d1_s1 <- merge(d1_s1, trt_s1, by="treatment", all.x=TRUE)
	d1_s1$trial_id <- "1"
	
	d1_s2 <- merge(
	  setNames(r1[r1$parameter == "Grain Yield 2nd season t ha", c("treatment","replicate","value")], c("treatment","replicate","yield")),
	  setNames(r1[r1$parameter == "Stover Yield 2nd season t ha", c("treatment","replicate","value")], c("treatment","replicate","dmy_residue")),
	  by = c("treatment","replicate"), all = TRUE)
	d1_s2 <- merge(d1_s2, trt_s23, by="treatment", all.x=TRUE)
	d1_s2$trial_id <- "2"
	
	d1_s3 <- merge(
	  setNames(r1[r1$parameter == "Grain Yield 3rd season t ha", c("treatment","replicate","value")], c("treatment","replicate","yield")),
	  setNames(r1[r1$parameter == "Stover Yield 3rd season t ha", c("treatment","replicate","value")], c("treatment","replicate","dmy_residue")),
	  by = c("treatment","replicate"), all = TRUE)
	d1_s3 <- merge(d1_s3, trt_s23, by="treatment", all.x=TRUE)
	d1_s3$trial_id <- "3"
	
	d1 <- rbind(d1_s1, d1_s2, d1_s3)
	d1$yield <- d1$yield * 1000
	d1$dmy_residue <- d1$dmy_residue * 1000
	
	#Bulk density - Measured once 
	bd <- r1[r1$parameter == "Soil Bulk Density g cm3", c("treatment","replicate","value")]
	names(bd)[3] <- "soil_bd"
	d1 <- merge(d1, bd, by=c("treatment","replicate"), all.x=TRUE)
	
	# Roots biomass
	roots <- r1[r1$parameter == "Root Biomass g dw", c("treatment","replicate","value")]
	names(roots)[3] <- "dw_roots"
	d1 <- merge(d1, roots, by=c("treatment","replicate"), all.x=TRUE)
	
	## Extract the striga data
	## Already defined in terminag: Week 8 = str_co1, 10 weeks = str_co2, 3 weeks = str_co3
	## Suggested for this data: "Week 2"="str_co4", "Week 4"="str_co5", "Week 6"="str_co6", "Week 12"="str_co7", "Week 14"="str_co8"
	striga_s2 <- r1[r1$parameter == "Striga count season 2", c("treatment","replicate","subgroup","value")]
	names(striga_s2) <- c("treatment","replicate","week","striga_count")
	striga_s2$trial_id <- "2"
	
	striga_s3 <- r1[r1$parameter == "Striga count season 3", c("treatment","replicate","subgroup","value")]
	names(striga_s3) <- c("treatment","replicate","week","striga_count")
	striga_s3$trial_id <- "3"
	
	striga_long <- rbind(striga_s2, striga_s3)
	
	week_map <- c("Week 8"="str_co1", "Week 10"="str_co2",
	              "Week 2"="str_co4", "Week 4"="str_co5", "Week 6"="str_co6",
	              "Week 12"="str_co7", "Week 14"="str_co8")
	
	striga_long$col <- week_map[striga_long$week]
	striga_wide <- reshape(striga_long[, c("treatment","replicate","trial_id","col","striga_count")],
	                       idvar=c("treatment","replicate","trial_id"), timevar="col", direction="wide")
	names(striga_wide) <- gsub("^striga_count\\.", "", names(striga_wide))
	
	d1 <- merge(d1, striga_wide, by=c("treatment","replicate","trial_id"), all.x=TRUE)
	
	d1$crop <- "maize"
	d1$country <- "Kenya"
	d1$adm1 <- "Siaya"
	d1$is_survey <- FALSE
	d1$on_farm <- TRUE
	d1$irrigated <- FALSE
	d1$geo_from_source <- FALSE
	d1$latitude <- NA
	d1$longitude <- NA
	d1$planting_date <- NA
	d1$harvest_date <- NA
	d1$yield_moisture <- NA
	d1$yield_isfresh <- NA
	d1$yield_part <- "grain"
	
	names(d1)[names(d1)=="replicate"] <- "rep"
	
	d1$plot_id <- paste(d1$trial_id, d1$treatment, d1$rep, sep="_")
	
	## Soil metrics
	## Ex-situ pH, both seasons
	ph_exsitu_s1 <- r1[r1$parameter == "Soil pH ex-situ season 1", c("treatment","replicate","subgroup","value")]
	names(ph_exsitu_s1) <- c("treatment","replicate","depth","soil_pH")
	ph_exsitu_s1$trial_id <- "1"
	ph_exsitu_s1$ph_type <- "exsitu"
	
	ph_exsitu_s2 <- r1[r1$parameter == "Soil pH ex-situ season 2", c("treatment","replicate","subgroup","value")]
	names(ph_exsitu_s2) <- c("treatment","replicate","depth","soil_pH")
	ph_exsitu_s2$trial_id <- "2"
	ph_exsitu_s2$ph_type <- "exsitu"
	
	## In-situ pH, season 1 only - replicate here spans 1-9
	ph_insitu <- r1[r1$parameter == "Soil pH in-situ season 1", c("treatment","replicate","subgroup","value")]
	names(ph_insitu) <- c("treatment","replicate","depth","soil_pH")
	ph_insitu$trial_id <- "1"
	ph_insitu$ph_type <- "insitu"
	
	d_soil <- rbind(ph_exsitu_s1, ph_exsitu_s2, ph_insitu)
	
	## depth_top/depth_bottom
	depth_top_map <- c("0-15 cm"=0, "15-30 cm"=15, "0-15 cm depth"=0, "15-30 cm depth"=15, "Application Zone (15cm depth)"=15)
	depth_bottom_map <- c("0-15 cm"=15, "15-30 cm"=30, "0-15 cm depth"=15, "15-30 cm depth"=30, "Application Zone (15cm depth)"=15)
	
	d_soil$depth_top <- depth_top_map[d_soil$depth]
	d_soil$depth_bottom <- depth_bottom_map[d_soil$depth]
	d_soil$plot_id <- paste(d_soil$trial_id, d_soil$treatment, d_soil$replicate, sep="_")
	
	d_soil$depth <- NULL
	d_soil$treatment <- NULL
	d_soil$replicate <- NULL
	
	carobiner::write_files(path, meta, wide=d1, long=d_soil)
}


