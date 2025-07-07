# R script for "carob"


carob_script <- function(path) {
  
"The objective of this work set is to demonstrate the best options currently available for the management of conservation agriculture (CA) practices in different communities in Mozambique. Eleven communities were selected from the districts of Sofala, Tete and Manica (approximately 100 to 200 families in each community) to host these demonstration sites and six demo fields were installed in each community from 2006-2015 (9 seasons). The treatments in each community were as follows: 1. Farmers' practice (control)- Traditional management with removal of stubble. 2. Conservation agriculture- The stubble is kept in the ground, there is no preparation of the ground, and the sowing is done manually in covachos previamento open (see the management of the covachos) and with SULCADOR in Nhamatiquite. 3. Direct sowing (SD): The stubble is kept in the soil, the direct sowing is done with Matraca or sharp bread. (2016-12-08)" 
  
	uri <- "hdl:11529/10830" 
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		project=NA,
		publication= "doi:10.1017/S1742170515000332",
		data_organization = "CIMMYT",
		data_type="experiment", 
		carob_contributor="Effie Ochieng'",
		carob_date="2023-11-21",
		treatment_vars=NA,
		response_vars="yield"		
	)
	
	f <- ff[basename(ff) == "Summary Mozambique On-farm Demonstration 2006-2015.xlsx" ]
	r0 <- readxl::read_excel(f, sheet = "Working data maize")
	r1 <- readxl::read_excel(f, sheet = "Working data legumes")
	
	d0 <- data.frame(
		planting_date=as.character(r0$`Harvest Year` - 1),
		harvest_date=as.character(r0$`Harvest Year`),
		adm2=r0$District,
		location=r0$Village,
		farmer=r0$Farmer,
		crop=r0$`Crop grown`,
		treatment=r0$Tmnt.,
		variety=r0$Variety,
		plant_density=r0$`Final stand (pl/ha)`,
		fwy_residue=r0$`Stalk yield (kg/ha)`,
		yield=r0$`Grain yield (kg/ha)`
	)
		
	d1 <- data.frame(
		planting_date=as.character(r1$`Harvest Year` - 1),
		harvest_date=as.character(r1$`Harvest Year`),
		adm2=r1$District,
		location=r1$Village,
		farmer=r1$Farmer,
		treatment=r1$Tmnt.,
		crop=r1$`Crop grown`,
		plant_density=r1$`Final stand (pl/ha)`,
		fwy_residue=r1$`Stalk yield (kg/ha)`,
		yield=r1$`Grain yield (kg/ha)`
	)
	
	d <- carobiner::bindr(d0, d1)
	d$irrigated <- FALSE
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	
	trmt <- carobiner::fix_name(d$treatment, "lower")
	trmt[trmt %in% c("conventional control","tradicional","conventional","conevntional","convencional","check")] <- "control"
	trmt[trmt %in% c("basins","bacio","bacia", "matraca","sulcador","coavacho","couvacho","covacho")] <- "conservation_agriculture"
	trmt[trmt %in% c("direct seeding","ds", "direct seeder","siembra directa","pao")] <- "direct_sowing"
	d$treatment <- trmt
	# efyrouwa: "ripper treatment meaning??"
	
	fam <- tolower(d$farmer)
	fam <- gsub("\\.", " ", fam)
	fam <- gsub("	"," ",fam)
	fam <- gsub(" ", "_",fam)
	fam <- gsub("\\/","",fam)
	d$trial_id <- as.character(as.integer(as.factor(fam)))
	d$farmer <- NULL
	
	d$crop <- tolower(d$crop)
	d$crop[d$crop %in% c("1", "2", "3")] <- "maize"
	d$crop[d$crop %in% c("beans", "feijao")] <- "common bean"
	
	
	## from pub
	#Maize and cowpea was planted in full rotation in Lamego, Pumbuto, Nhamizhinga and Malomwe. In Nzewe, farmers rejected cowpea but opted for common bean (Phaseolus vulgaris L.) as their rotational crop.
	d$previous_crop <- "maize"
	d$previous_crop[d$crop == "maize"] <- "cowpea"
	d$previous_crop[d$crop == "maize" & d$location == "Nzewe"] <- "common bean"


	#Maize was fertilized with 58N:24P2O5:12K2O applied as basal dressing at planting and as top-dressing at 4 weeks after planting. The same amount of fertilizer was applied to all treatments. 
	#Legume treatments only received a basal dressing of 12N:24P2O5:12K2O at planting and no further mineral fertilizer application.
	d$N_fertilizer <- 12
	d$N_fertilizer[d$crop == "maize"] <- 58 
	d$P_fertilizer <- 24 / 2.29
	d$K_fertilizer <- 12 / 1.2051

	#Weed control on all CA plots was achieved with an initial application of glyphosate (glyphosate [N-(phosphonomethyl) glycine] at a rate of 3 liters ha−1) and manual hand hoe weeding. In conventional systems, farmers used the hand hoe only for weed control. 

	#Pest control especially on the cowpea was achieved through regular (bi-weekly) spray of Dimethoate (O,O-dimethyl S-[2-(methylamino)-2-oxoethyl] dithiophosphate) as they were most affected by control aphids (Aphis ssp.) and elegant grasshoppers (Zonocerus elegans Thunberg).
	
	
	d$country <- "Mozambique"
	d$yield_part <- ifelse(d$crop == "maize", "grain", "seed")

	d$location <- carobiner::replace_values(d$location, 
		c("Nhamizhinga", "Maguai", "Madjiga", "Belia", "Gimu", "Lamego Ndeja", "Mussianharo", "Malomwe", "Lamego John Segredo", "Madjigo", "Guaraguara, Belia", "Nhaufo", "Madgiga", "Mussinharo"),
		c("Nhamizinga", "Tsangano", "Magiga", "Buzi", "Tsangano", "Lamego", "Barue", "Malomue",	"Lamego", "Magiga", "Tsangano", "Buzi", "Magiga", "Barue"))
	
	geo <- data.frame(
		location = c("Pumbuto", "Nhanguo", "Puanda", "Guro", "Malomue", "Ruaca", "Nhamizinga", "Nzewe", "Nhamatiquite", "Tsangano", "Magiga", "Lamego", "Buzi", "Ulongue", "Nharuchonga", "Barue", "lamego", "Gimo"), 
		latitude = c(-19.0025, -21.195, -19.8500034, -16.95262, -18.17028,	-13.11722, -17.06833, -14.519, -19.24278, -15.20012, -13.74806, -19.33251,-20.03925, -14.72278, -19.23972, -17.81047, -19.33251, -18.60111), 
		longitude = c(33.75028, 34.94222, 34.17028, 33.51865, 33.3075, 38.21194, 34.83083, 34.304, 33.76694, 34.32685, 35.26222, 34.31678, 34.37237, 34.36083, 34.12306, 33.17267,34.31678, 34.545),
		geo_from_source=FALSE
	)
	
	d <- merge(d, geo, by = "location", all.x = TRUE)

	d <- unique(d[!is.na(d$yield), ])
	d$plant_density[d$plant_density < 1] <- NA
	
	carobiner::write_files(meta, d, path=path)
}


