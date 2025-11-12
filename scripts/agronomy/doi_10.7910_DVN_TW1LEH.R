# R script for "carob"
# license: GPL (>=3)

## ISSUES

#### The amount of NAFAKA+ and YARA fertilizer  applied is missing 


carob_script <- function(path) {

"
Evaluating Fertilizer Recommendations with Farmers

The Africa RISING program adopts the mother-baby trial approach to test, validate and disseminate research results. Under this approach farmers have been exposed to the technologies tested and validated on-farm (mother trials). Thereafter, farmers are given the opportunity to experiment technology they chose on their farms (baby trials) after a training. In this context, ICRAF and partners developed fertilizer recommendations (30 kg P/ha and 60 kg N/ha) for maize in semi-arid Tanzania during the 2013 and 2014 growing seasons. Farmers were involved to test these rates widely in their fields (as baby plots) when integrated with improve maize varieties and different types of fertilizers.  Fertilizer tested were Minjingu and Yara Mila Cereals fertilizers and maize varieties were Staha, Kilima, SEED Co and a local variety known as Gunzi Jekundu. The work started with training farmers on promising fertilizer technologies and good agronomic practices (GAP) during the beginning of the 2015 (293 farmers) and 2016 seasons (682 farmers). About 605 farmers (55% Male and 45% Female) who attended training in 2016 established baby trials to validate fertilizer-maize variety technologies. Each baby farmers had a max of four plots assessing performance of improved maize variety and local variety with and without Minjingu (Nafaka Plus for basal application at planting and Minjingu top dressing) or Yara Mila Cereal fertilizers. Yara Mila Cereal was used for a basal and top dressing applications as per guideline printed in the fertilizer bag and from the company's agronomist.This data study contains data produced from these trials.  About the project  Project title: Intensification of Maize-Legume Based Systems in the Semi-Arid Areas of Tanzania to Increase Farm Productivity and Improve Farming Natural Resource Base  Project abstract  The aim of the Africa RISING project in Kongwa and Kiteto Districts, Tanzania is to provide a scientific basis for sustainably intensifying agricultural production in semi-arid areas of central Tanzania. The project activities are falls under 4 thematic areas that address three critical elements of sustainable intensification (SI), i.e. genetic, ecological and socio-economic intensification technologies. The scope of activities being implemented include: packaging of new legume and cereal varieties with over 120% yield advantage, packaging and validation of integrated productivity enhancing technologies for cereals, legumes, legume trees and soil health technologies, food safety primarily to reduce aflatoxin contamination and integration of livestock into the cropping systems. The innovation platform is used to set R4D priority in the action sites.  The project team is comprised of national partners (e.g. ARI-Hombolo, District Agricultural Officers, SUA and UDOM) and CG Partners (CIMMYT and ICRAF) under the leadership of ICRISAT.  Project website:    http://africa-rising.net/where-we-work/west-africa/  Project start date: 2012-05-01  Project end date : 2016-09-30
"

	uri <- "doi:10.7910/DVN/TW1LEH"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "ICRAF; SUA",
		publication = NA,
		project = "Africa RISING",
		carob_date = "2025-11-12",
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;S_fertilizer;variety",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f <- ff[basename(ff) == "001_kkMaizeYieldBabyTrialData.csv"]
	r <- read.csv(f)

	#### process 
	
	d <- data.frame(
		location = r$Village,
		trial_id = r$FarmerID,
		treatment = trimws(r$Treatment_Fertilizer.Type),
		farmer_gender = r$Gender,
		variety = r$Treatment_Seed.type,
		yield = r$Yield..t.ha.*1000,
		crop = "maize",
		planting_date = "2016",
		country = "Tanzania",
		on_farm = TRUE, 
		is_survey = FALSE, 
		yield_part = "grain", 
		yield_moisture = as.numeric(NA), 
		irrigated = NA, 
		geo_from_source = TRUE,
		
		## We consider: NAFAKA+ 9% N 18% P2O5 6%K20 25%Ca  5%S and 2%Mg from https://www.tfra.go.tz/uploads/documents/sw-1703173172-sw-1633535270-Final%20Register_Book%20English.pdf
		##              YARA  as NPKMgSZn
		N_fertilizer = ifelse(grepl("YARA",  trimws(r$Treatment_Fertilizer.Type)), 21, 
		                ifelse(grepl("NAFAKA+", r$Treatment_Fertilizer.Type), 9, 0)),
		
		P_fertilizer = ifelse(grepl("YARA",  trimws(r$Treatment_Fertilizer.Type)), 7.48, 
		                ifelse(grepl("NAFAKA+", r$Treatment_Fertilizer.Type), 18/2.26, 0)),
		
		K_fertilizer = ifelse(grepl("YARA",  trimws(r$Treatment_Fertilizer.Type)), 1.8, 
		                ifelse(grepl("NAFAKA+", r$Treatment_Fertilizer.Type), 6/1.2051, 0)),
		
		S_fertilizer = ifelse(grepl("YARA",  trimws(r$Treatment_Fertilizer.Type)), 12, 
		                ifelse(grepl("NAFAKA+", r$Treatment_Fertilizer.Type), 5, 0))
		
	)

	### Adding fertilizer 
	geo <- data.frame(
	   location = c("Mlali", "Molet", "Njoro"),
	   longitude = c(36.7505, 34.888, 36.454),
	   latitude = c(-6.2817, -6.368, -5.253)
	)
	
	d <- merge(d, geo, by="location", all.x = TRUE)
	
	
carobiner::write_files(path, meta, d)

}

