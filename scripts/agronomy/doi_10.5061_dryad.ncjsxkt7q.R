# R script for "carob"
# license: GPL (>=3)
## ISSUES



carob_script <- function(path) {

"
Seed treatment affected establishment and yield in two pennycress lines

This dataset contains data related to pennycress establishment, growth, and yield as affected by applied treatments and site effects. Data related to sites includes field location, management practices of tillage, rotational crop, and soil type, and precipitation before and after planting. Treatments tested include two genetic pennycress lines (black-seeded line called 'MN106NS' and a golden-seeded line called 'tt8-t/ARV1') and six seed treatments: 1) non-treated control, 2) a treatment where seeds were soaked for 12 hrs in 0.01% w/w solution of Gibberellic Acid A4+A7, 3) treament with fludioxonil fungicide at a rate of 50 μg ai per g of seed, 4) seeds were pelleted using a commercial binder and diatomaceous earth as the filler, 5) a combination of pelleting + fungicide, and 6) the pelleting treatment in 4) but added 0.01% w/w GA solution to the binding agent during pellet construction plus the fungicide. All combinations of pennycress line and seed treatment were assigned a unique value from 1-12 as described in the column header information for 'Treatment' within the dataset.
"


	uri <- "doi:10.5061/dryad.ncjsxkt7q"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=7, minor=NA,
		data_organization ="OHSU;PSU;USDA-ARS;ILSU;WIU;UMINN;UWP", 
		publication = "doi:10.3389/fagro.2023.1205259",
		project = NA,
		carob_date = "2025-07-22",
		design = NA,
		data_type = "experiment",
		treatment_vars ="seed_treatment;variety",
		response_vars ="yield;plant_density", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes ="Precipitation 7 days before planting and 14 days after planting has not been processed."
	)
	
	f <- ff[basename(ff) == "Pellet_field.csv"]
	
	r <- read.csv(f, na=".")[-c(1:5),]
	
	### Process 
	d <- data.frame(
		year = r$Year,
		location = r$Location,
		latitude= as.numeric(gsub("\\°|,", "", substr(r$GPS.Coordinates, 1, 9))),
		longitude= as.numeric(gsub(" |, |, |", "", substr(r$GPS.Coordinates, 10, 19))),
		country= "United States",
		land_prep_method= ifelse(grepl("Conventional Till" , r$Prepared.Tillage), "conventional",
		                 ifelse(grepl("fall disc", r$Prepared.Tillage), "disk tillage", 
		                 ifelse(grepl("roto-til", r$Prepared.Tillage), "rotovating", "none"))),
		planting_date= as.character(as.Date(as.numeric(r$Planting.Day..DOY.) - 1, origin = paste0(as.numeric(r$Year)-1, "-01-01"))),
		rep= as.integer(r$Block..Rep.),
		trt_number= r$Treatment,
		soil_WHC_sat= as.numeric(r$Estimated.soil.moisture),
		disease_severity= r$PC.Leaf.Disease,
		plant_density= as.numeric(r$PC.Autumn.establishment.in.plants.m2)*10000,
		diseases= "leaf",
		harvest_date= as.character(as.Date(as.numeric(r$PC.Harvest.Day..DOY.) - 1, origin = paste0(r$Year, "-01-01"))),
		soil_type = tolower(r$Soil.Series.and.Texture),
		crop= "pennycress",
		previous_crop = tolower(r$Previous.crop),
		plot_id = r$PlotID,
		residue_prevcrop_used= ifelse(!is.na(r$Residue.Coverage....), TRUE, FALSE),
		previous_crop_residue_perc= as.numeric(r$Residue.Coverage....), ## 
		seed_weight= as.numeric(r$X1000.seed.weight),
		yield = as.numeric(r$PC.Seed.Yield..kg.ha.),
		trial_id = r$Location, 
		on_farm= TRUE, 
		is_survey= FALSE, 
		yield_part= "seed",
		geo_from_source= TRUE,
		irrigated= NA
	)


	d$seed_treatment <- c("untreated", "Gasoak", "fungicide", "pellet", "pellet+fungicide","pellet+fungicide+Gasoak", "untreated", "Gasoak", "fungicide", "pellet", "pellet+fungicide", "pellet+fungicide+Gasoak")[as.numeric(d$trt_number)]
	d$variety <- c(rep("MN106NS", 6), rep("tt8-t/ARV1", 6))[as.numeric(d$trt_number)]
	d$trt_number <- d$year <- NULL

	d <- d[!is.na(d$yield), ]
	
	P <- carobiner::fix_name(d$soil_type)
	P <- gsub("kokomo silty clay loam", "clay loam", P)
	P <- gsub("waukeegan silt loam", "silty loam", P)
	P <- gsub("aazdahl-formdale-balaton clay loams", "clay loam", P)
	P <- gsub("171b catlin silt loam", "silt loam", P)
	P <- gsub("68a sable silty clay loam", "clay loam", P)
	P <- gsub("kegonsa silt loam (85.5) plano silt loam (14.5)", "silty loam", P)
	d$soil_type <- P
	
	P <- carobiner::fix_name(d$previous_crop)
	P <- gsub("corn sillage|sweetcorn", "maize", P)
	P <- gsub("spring wheat", "wheat", P)
	P <- gsub("stover", "none", P)
	d$previous_crop <- P
	
 d$N_fertilizer <- 45
 d$P_fertilizer <- d$K_fertilizer <- 0

 ### Adding soil data from paper
 soil <- data.frame(
    location= c("OSU_Wat", "UMn_2", "USDA_Mn", "ISU", "WIU", "UW_1"),
    soil_SOM= (c( 34, 33, 37, 42, 34, 32))/10, ## in %
    soil_CEC= c( 16.6, 13.9, 17.4, 19.5, 22, 14.8),
    soil_pH= c( 7, 5, 6.9, 5.9, 5.8, 7.2),
    soil_P_available= c( 164, 36, 18, 85, 25, 35),
    soil_K= c( 590, 99, 125, 155, 165, 163),
    soil_Ca= c( 2250, 1100, 2400, 2300, 2500, 1800),
    soil_Mg= c(455, 260, 575, 340, 515, 645)
 )

 d	<- merge(d, soil, by="location", all.x = TRUE)
 
carobiner::write_files(path, meta, d)

}

