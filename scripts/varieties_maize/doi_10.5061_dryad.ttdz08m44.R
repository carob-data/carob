# R script for "carob"
# license: GPL (>=3)

## ISSUES
 
 ### We added a new variable: 
 # grain_aflatoxin: grain aflatoxin concentration measured in ng/g.

carob_script <- function(path) {

"
Data from: Combining ability of experimental maize lines for yield and aflatoxin in the southeastern USA

Aflatoxin contamination of maize (Zea mays L.) grain presents risks to animal and human health and causes economic losses for growers, particularly in the southeastern United States. Development of hybrids with genetic resistance to aflatoxin contamination by Aspergillus flavus will reduce these risks. Two factorial crossing experiments were conducted to evaluate yield, agronomic traits, and aflatoxin in maize hybrids. In Exp. 1, 18 aflatoxin-resistant (GT) lines were each crossed to six testers. In Exp. 2, 13 of the same lines were each crossed to 10 different testers. Each experiment was conducted for two years at Tifton, GA and commercial check hybrids were included for comparison.  Ears were inoculated with A. flavus isolate NRRL 3357 using the side-needle technique 14 d after silking. Aflatoxin was quantified after harvest. Broad sense heritability for aflatoxin was low in Exp. 1 (H2 = 0.09) but was higher in Exp. 2 (H2 = 0.27). Heritability was moderate to high for plant height (H2 = 0.49–0.51) and days to silking (H2 = 0.54–0.72).  Among experimental lines, GT1209 and GT1309 had consistent positive general combining ability (GCA) for yield, whereas GT1203 and GT1204 had consistent negative (favorable) GCA for aflatoxin. Significant GCA effects, both positive and negative, for aflatoxin were also observed among testers in both experiments. Some experimental hybrids had yields that were comparable to commercial checks while also having lower aflatoxin, demonstrating the progress made in improvement of both traits in maize hybrids adapted to the Southern United States.
"

	uri <- "doi:10.5061/dryad.ttdz08m44"
	group <- "varieties_maize"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=5, minor=NA,
		data_organization = "USDA-ARS",
		publication = "doi:10.1002/csc2.21050",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety_pedigree",
		response_vars = "yield;grain_aflatoxin", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-10-19",
		carob_completion = 100,	
		carob_effort = 2
	)
	

	f1 <- ff[basename(ff) == "Data_from_Knoll_et_al_2023.xlsx"]
	f2 <- ff[basename(ff) == "README.md"]

	r1 <- carobiner::read.excel(f1, sheet="2014yld", na = "-")
	r2 <- carobiner::read.excel(f1, sheet="2014afla", na = "-")
	r3 <- carobiner::read.excel(f1, sheet="2015yld", na = "-")
	r4 <- carobiner::read.excel(f1, sheet="2015afla", na = "-")
	r5 <- carobiner::read.excel(f1, sheet="2018all", na = "-")
	r6 <- carobiner::read.excel(f1, sheet="2019all", na = "-")
	#r2 <- read.???(f2)


### 2014 yield experiment 1 
	d1 <- data.frame(
		plot_id = as.character(r1$Plot_ID),
		rep = as.integer(r1$Rep),
		variety_pedigree = r1$Pedigree,
		female_parent = r1$P_female,
		male_parent = r1$P_male,
		ear_height = r1$Ear_ht,
		plant_height = r1$Plant_ht,
		anthesis_days = r1$anthesis,
		asi = r1$ASI,
		yield = r1$kg_ha_15,
		yield_moisture = r1$`%_Moisture`,
		root_lodging = as.character(r1$Lodging),
		crop = "maize",
		planting_date = "2014-04-02",
		harvest_date = "2014-08-27",
		trial_id = "1"
	)

## 2014 aflatoxin experiment 1
	d2 <- data.frame(
		plot_id = as.character(r2$Plot_ID),
		rep = as.integer(r2$Rep),
		variety_pedigree = r2$Pedigree,
		female_parent = r2$p_female,
		male_parent = r2$p_male,
		grain_aflatoxin = r2$Afla_ppb, ## in ng/g 
		crop = "maize",
		planting_date = "2014-04-23",
		harvest_date = as.character(as.Date("2014-04-23") +60),
		trial_id = "2"
	)

	### 2015 yield experiment 1
	d3 <- data.frame(
		plot_id = as.character(r3$Plot_ID),
		rep = as.integer(r3$Rep),
		variety_pedigree = r3$Pedigree,
		female_parent = r3$P_female,
		male_parent = r3$P_male,
		ear_height = r3$Ear_ht,
		plant_height = r3$Plant_ht,
		anthesis_days = r3$Anthesis,
		asi = r3$ASI,
		yield = r3$kg_ha_15,
		yield_moisture = r3$`%_Moisture`,
		root_lodging = as.character(r3$Lodging),
		crop = "maize",
		planting_date = "2014-04-02",
		harvest_date = "2014-08-27",
		trial_id = "3"
	
	)
	
	### 2015 aflatonxin experiment 1
	d4 <- data.frame(
	  plot_id = as.character(r4$Plot_ID),
	  rep = as.integer(r4$Rep),
	  variety_pedigree = r4$Pedigree,
	  female_parent = r4$P_female,
	  male_parent = r4$P_male,
	  grain_aflatoxin = r4$Afla_ppb, ## in ng/g 
	  planting_date = "2015-04-22",
	  harvest_date = as.character(as.Date("2015-04-22") +60),
	  trial_id = "4"
	)
	
	## 2018 yield and aflatoxin experiment 2
	
	d5 <- data.frame(
	  plot_id = as.character(r5$Plot_ID),
	  rep = as.integer(r5$Rep),
	  variety_pedigree = r5$Pedigree,
	  female_parent = r5$P_female,
	  male_parent = r5$P_male,
	  ear_height = r5$ear_ht,
	  plant_height = r5$Plant_ht,
	  anthesis_days = r5$Anthesis,
	  asi = r5$ASI,
	  yield = r5$kg_ha_15,
	  crop = "maize",
	  grain_aflatoxin = r5$Afla_ppb, ## in ng/g 
	  root_lodging = as.character(r5$Root_lodge),
	  stalk_lodging = as.character(r5$stalk_lodge),
	  planting_date = "2018-05-08",
	  harvest_date = "2018-09-18",
	  trial_id = "5"
	)
	
	### 2019 yield and aflatoxin experiment 2
	d6 <- data.frame(
	  plot_id = as.character(r6$Plot_ID),
	  rep = as.integer(r6$Rep),
	  variety_pedigree = r6$pedigree,
	  female_parent = r6$p_female,
	  male_parent = r6$p_male,
	  ear_height = r6$ear_ht,
	  plant_height = r6$Plant_ht,
	  anthesis_days = r6$Anthesis,
	  asi = r6$ASI,
	  yield = r6$kg_ha_15,
	  crop = "maize",
	  grain_aflatoxin = r6$Afla_ppb, ## in ng/g 
	  root_lodging = as.character(r6$Root_lodge),
	  stalk_lodging = as.character(r6$stalk_lodge),
	  planting_date = "2019-04-25",
	  harvest_date = "2019-08-31",
	  trial_id = "6"
	)
	
	### combine all files
	
	d <- carobiner::bindr(d1, d2, d3, d4, d5, d6)
	
	###
	d$is_survey <- FALSE
	d$on_farm <- TRUE
	d$yield_part <- "grain"
	d$country <- "United States"
	d$geo_from_source <- TRUE ## from publication
	d$latitude <- 31.50658
	d$longitude <- - 83.55621
	d$irrigated <- NA
	d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA)

	carobiner::write_files(path, meta, d)
}

