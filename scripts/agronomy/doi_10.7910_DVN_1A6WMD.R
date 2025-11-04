# R script for "carob"


carob_script <- function(path) {
  
"Many soils on smallholder farms in Malawi have poor soil organic matter content. This results in poor maize productivity when sufficient mineral fertilizers are not added. Building soil organic matter requires improving both cereal and legume crops primary productivity through mineral fertilizers, and retaining the associated crop residues on the cropped lands. These residues decompose to provide mineral N to crops grown in sequence, as well as being an important source for SOM capitalization. Residues of legumes crops have a narrow C/N ration and are hypothesized to improve N cycling and benefit the rotational crop, whereas residues of maize, which have a wide C/N ratio, promote immobilization. While this knowledge is widely known, what is not clear is the interaction between crop residue quality, quantity and soil water management on maize productivity and mineral N dynamics. The data will address the following: 1. Does incorporating soil water enhancing technologies increase/reduce the immobilization potential of maize residues? 2. What is the effect of varying the quantity of the crop residues incorporated (both maize and legumes) on mineral N dynamics, soil water content and maize productivity; 3. For farmers with limited fertilizer use (50% NP), how detrimental is use of maize residues (X0, X1, X2), with or without water conservation measures; 4. What is the fertilizer substitution value of different quantity residues generated from a groundnut/pigeonpea doubled up system? This data is for the residue generation phase"
  
	uri <- "doi:10.7910/DVN/1A6WMD"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		project=NA,
		publication= "doi:10.1016/j.fcr.2021.108225",
		data_organization = "IITA",
		data_type="on-farm experiment", 
		carob_contributor="Mitchelle Njukuya",
		carob_date="2024-01-09",
		response_vars = "yield",
		treatment_vars = NA
	)
  
	f <- ff[basename(ff) == "NewDesign_gnut yields.csv"]
	r <- read.csv(f)

	d <- data.frame(
		adm2=r$District,
		location=r$Village,
		site=r$EPA,
		treatment=tolower(r$Treatment),
		intercrops="pigeon pea",
		crop=tolower(r$Crop.harvested),
		variety=r$Groundnut.variety,
		rep=r$Rep,
		dmy_residue=r$stover.dry.weight..kg.ha.,
		yield=r$Grain.weight..kg.ha.,
		dmy_total=r$Total.biomass..kg.ha.
	)
				  
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$trial_id <- d$Treatment.number
	d$country <- "Malawi"

	#planting dates sourced from publication
	d$planting_date <- "2017"
	d$N_fertilizer <- 34.5
	### not in publication ???
	d$P_fertilizer <- as.numeric(NA)
	d$K_fertilizer <- as.numeric(NA)
	d$yield_part <- "seed"
	d$plant_density <- 164000
  
	d$trial_id <- as.character(as.integer(as.factor(d$location)))
	geo <- data.frame(
		location = c("Mbizi", "Mkambiri", "Tambula"), 
		latitude = c(-14.176, -14.567, -14.623),
		longitude= c(34.125, 35.538, 35.6456 ),
		geo_from_source = FALSE
	)
	
	d <- merge(d, geo, by="location", all.x=TRUE)   
	d$irrigated <- FALSE

	d$yield_moisture <- as.numeric(NA) #needs to be checked

    carobiner::write_files(meta, d, path=path)
}

