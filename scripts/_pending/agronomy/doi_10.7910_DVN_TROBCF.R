# R script for "carob"
# license: GPL (>=3)

## ISSUES
# phosphorous fertilizer  applied needs to be quantified.


carob_script <- function(path) {

"
Pigeonpea Productivity and Parameters

This data study contains data on pigeonpea and soybean intensification through phosphorous fertilization. It contains data on cropping system, phosphorous level, treatments and variety of pigeonpea, and yields among many others. About the project  Project title: AfricaRISING - Sustainable Intensification of Maize-Legume-Livestock Integrated Farming Systems in East and Southern Africa  Project abstract  	The Malawi project has local theme 'Agro-ecological intensification in Malawi through action research with smallholder farmers' with a lot of emphasis on co-learning with farmers and other stakeholders.The purpose of the Africa RISING Malawi component is to enhance farmer knowledge and support sustainable intensification (SI) pathways for productivity gains in maize-legume diversified systems, that also integrates livestock-related enterprises such as improved fodder for intensified dairy production. The project is setting up a research approach that systematically assesses SI best-bet options that appropriately respond to the needs of resource-poor farmers - particularly female headed households. Building on successful examples of participatory action research and experiences from biophysical research on smallholder farms in Malawi over the past two decades, the research team has begun taping into these products of agricultural research to move towards more sustainable smallholder production systems. We envisage that farm-scale production strategies employed by different farm/farmer typologies will be further distilled through scenario analyses using farming systems simulation modeling approaches. The project works with an alliance of actors (agro-dealers, extension services, NGOs, local government structures, etc) as R4D platforms for the two districts. Project website:   http://africa-rising.net  Project start date: 01/01/2012   Project end date : 09/30/2016
"

	uri <- "doi:10.7910/DVN/TROBCF"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "LUANAR;MSU",
		publication = NA,
		project = "Africa RISING",
		carob_date = "2025-08-28",
		design = NA,
		data_type = "experiment",
		treatment_vars = "intercrops;P_fert_level",
		response_vars = "yield;dmy_total", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes =NA
	)
	
	f1 <- ff[basename(ff) == "003_pigeonpeaProductivityParameters_PPgrainYield.csv"]
	f2 <- ff[basename(ff) == "001_pigeonpeaProductivityParameters_PPBNF.csv"]
	f3 <- ff[basename(ff) == "002_pigeonpeaProductivityParameters_Nodulation.csv"]
	

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)

### yield data
	d1 <- data.frame(
	   location = r1$Site,
	   treatment = r1$Treatment.Description,
	   rep= as.integer(r1$Rep),
	   crop = tolower(r1$Crop),
	   variety = r1$Variety,
	   P_fert_level = r1$p.level,
	   yield = r1$Grain.Yield..Kg.ha.,
	   yield_moisture= r1$X..Moisture,
	   dmy_residue= r1$dry.biomass..kgha.1.,
	   dmy_total = r1$total.dry.matter.yield..kgha.1,
	   harvest_index= r1$Harvest.Index,
	   intercrops= ifelse(grepl("pigeonpea/soybean", r1$Cropping.system), "soybean", "none"),
	   inoculated= ifelse(grepl("inoculated", r1$Treatment.Description), TRUE, FALSE),
	   country= "Malawi",
	   N_fertilizer= 0, 
	   P_fertilizer= ifelse(grepl("p0", r1$p.level), 0 , as.numeric(NA)) ,
	   K_fertilizer= 0,
	   is_survey= FALSE,
	   on_farm= TRUE,
	   geo_from_source= FALSE,
	   irrigated= NA,
	   yield_part= "grain",
	   trial_id= ifelse(grepl("Golomoti", r1$Site), "1", "2")
	   
	)
	
d1	<- d1[!is.na(d1$yield),]


## Adding N fixation 
	d2 <- data.frame(
		location= r2$Site,
		rep= as.integer(r2$Rep),
		intercrops= ifelse(grepl("pigeonpea/soybean", r2$Csystem), "soybean", "none") ,
		P_fert_level= r2$plevel,
		treatment=  r2$Treatment.Description,
		crop= tolower(r2$Crop),
		variety= r2$Variety,
		residue_N= r2$plant.N.concentration....,
		#grain_N= r2$N.yield.legume..kgha.1., # 
		N_fixation= r2$BNF..kgha.1.
	)

d <- merge(d1, d2, by= c("location", "rep","intercrops", "P_fert_level", "treatment", "crop", "variety"), all.x = TRUE)	
	
#### Adding node 	
	d3 <- data.frame(
		location = r3$Site,
		treatment = r3$Treatment.Description,
		crop = tolower(r3$Crop),
		rep= as.integer(r3$Rep),
		P_fert_level= r3$plevel,
		node_count= r3$ave.no..nod.per.plant
	
	)

	
	d <- merge(d, d3, by= c("location", "rep", "P_fert_level", "treatment", "crop"), all.x = TRUE)	
	
	d$planting_date <- "2012" ## need to be check out
	d$crop <- gsub("pigeonpea", "pigeon pea", d$crop)
	## Adding longitude and latitude
	
	i <- grepl("Golomoti", d$location)
	d$longitude[i] <- 34.591
	d$latitude[i] <- -14.427  
	
	i <- grepl("Nsipe", d$location)
	d$longitude[i] <- 34.635
	d$latitude[i] <- -14.821
	

 carobiner::write_files(path, meta, d)
 
}


