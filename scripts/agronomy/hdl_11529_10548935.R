# R script for "carob"


carob_script <- function(path) {

"Six seed treatments were tested for 2 growing cycles (summer of 2021 and 2022) in a field experiment with maize (Zea mays L.) and barley (Hordeum vulgare L.) under conservation agriculture in the Mexican highlands, at CIMMYT’s experiment station
of El Batán Texcoco, the State of Mexico, Mexico. The experiment was a randomized complete block design with 3 replicates, with separate areas for maize and barley. The six seed treatments included a negative control, a chemical seed treatment (different for maize and barley, depending on common practices in the area), Trichoderma, Metarhizium, a commercial mixture of plant growth promoting rhizobacteria, and a combination of Trichoderma and Metarhizium. Soil and root samples were taken at two and three sampling times during the 2021 crop cycle for barley and maize, respectively. Yield and yield components were determined at the end of the crop cycle in 2021 and 2022. The soil and root samples were used to measure root growth (root biomass per core), root colonization with mycorrhizal fungi, root infection with pathogens (Polymyxa, Pythium, Microdochium), soil microbial communities in terms of biomarker fatty acids, and ecological guilds of soil nematodes (Bacterivores, fungivores, plant parasitic and predators). (2023-08-01)"


	uri <- "hdl:11529/10548935"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "UNAM;CIMMYT;AAU",
		publication = "doi:10.1016/j.ejsobi.2024.103653", 
		project = NA,
		data_type = "experiment",
		response_vars = "yield",
		treatment_vars = "seed_treatment", 
		carob_contributor = "Blessing Dzuda",
		carob_date = "2024-05-21"
	)

	f <- ff[basename(ff) == "DAT-BV234-Database-2021-2022-Micro and yield.xlsx"]

	r1 <- carobiner::read.excel(f, sheet = "MAIZE yield 2021")
	r2 <- carobiner::read.excel(f, sheet = "MAIZE yield 2022")
	r3 <- carobiner::read.excel(f, sheet = "MAIZE Root, nematodes, fatty ac", skip=1)
	
	r4 <- carobiner::read.excel(f, sheet = "BARLEY yied 2021")
	r5 <- carobiner::read.excel(f, sheet = "BARLEY yield 2022")
	r6 <- carobiner::read.excel(f, sheet = "BARLEY Root, nematode, fatty ac", skip=1)

	expr <- carobiner::read.excel(f, sheet = "Experiment", skip=6)[1:7,-1]
	
	expr$ai <- c("none", 
		"Carboxin; Thiram; Chlorothalonil; Thiamethoxam", 
		"Thiabendazole; Fludioxonil; Metalaxyl-M; Azoxystrobin; Thiamethoxam", 
		"Trichodermaharzianum T-22", 
		"Metarhizium anisopliae", 
		"Thiabendazole; Fludioxonil; Metalaxyl-M; Azoxystrobin; Thiamethoxam; Trichodermaharzianum T-22", 
		"Arthrobacter; Azotobacter vinlandii; Clostridium pasteurianum; Bacillus licheniformis; B. subtilis; Rhodococcus; Azospirillum; Rhodobacter*"
	)
	
	d1 <- data.frame(
		crop=r1$Crop,
		rep=r1$Rep,
		treatment=r1$Trt,
		flowering_days=r1$Tasseling,
		maturity_days=r1$Maturity,
		plant_height=r1$Height,
		dmy_storage=r1$Yield_dry,
		yield=r1$`Yield_12%H2O`,
		seed_weight=r1$Thou,
		silking_days=r1$Silking,
		plant_density=r1$`Plants/m²`,
		trial_id="1",
		planting_date="2021"
	)

	d2 <- data.frame(
		crop=r2$Crop,
		rep=r2$Rep,
		treatment=r2$Trt,
		flowering_days=r2$Tasseling,
		maturity_days=r2$Maturity,
		plant_height=r2$Height,
		dmy_storage=r2$Yield_dry,
		yield=r2$`Yield_12%H2O`,
		seed_weight=r2$Thou,
		silking_days=r2$Silking,
		plant_density=r2$`Plants/m²`,
		trial_id="2",
		planting_date="2022"
	)

	d3 <- data.frame(
		samptime = r3$TIME,
		treatment = r3$TREATMENT,
		# not sure how to go from g/core to kg/ha or other standard measure 
		#dmy_roots = r3$`Root biomass (g)`,
		root_AMF = r3$`AMF`,
		root_infection = r3$`Total pathogens`		
	)

	
	d4 <- data.frame(
		crop=r4$Crop,
		rep=r4$Rep,
		treatment=r4$Trt,
		flowering_days=r4$Flowering,
		maturity_days=r4$Maturity,
		plant_height=r4$Altura,
		dmy_storage=r4$Yield_Dry,
		yield=r4$`Yield_12%H2O`,
		seed_weight=r4$Thou,
		spike_density=r4$`Spikes/m2`,
		trial_id="3",
		planting_date="2021"
	)
	
	d5 <- data.frame(
		crop=r5$Crop,
		rep=r5$Rep,
		treatment=r5$Trt,
		flowering_days=r5$Flowering,
		maturity_days=r5$Maturity,
		plant_height=r5$Altura,
		dmy_storage=r5$Yield_Dry,
		yield=r5$`Yield_12%H2O`,
		seed_weight=r5$Thou,
		spike_density=r5$`Spikes/m2`,
		trial_id="4",
		planting_date="2022"
	)

	d6 <- data.frame(
		samptime = r6$Time,
		treatment = r6$Treatment,
		# not sure how to go from g/core to kg/ha or other standard measure 
		# dmy_roots = r6$`Root weight (g)`,
		root_AMF = r6$`AMF`,
		root_infection = r6$`Total Pathogens`		
	)

	# using the last root sample
	d1 <- cbind(d1, d3[d3$samptime==3, -c(1:2)])
	dm <- carobiner::bindr(d1, d2)
	exp_m <- expr[- grep("barley", expr$Abbreviation), ]
	dm$seed_treatment <- exp_m$ai[dm$treatment]

	d4 <- cbind(d4, d6[d6$samptime==2, -c(1:2)])
	db <- carobiner::bindr(d4, d5)
	exp_b <- expr[- grep("maize", expr$Abbreviation), ]
	db$seed_treatment <- exp_b$ai[db$treatment]

	d <- carobiner::bindr(dm, db)
	exp_m$Abbreviation <- gsub(" - maize", "", exp_m$Abbreviation)
	d$treatment <- exp_m$Abbreviation[d$treatment]
	
	d$on_farm <- FALSE #on experimental station!
	d$is_survey <- FALSE
	d$irrigated <- FALSE; #are you sure? else use NA
	d$yield_part <- "grain"

	d$country <- "Mexico"
	d$location <- "El Batán Experimental Station"
	d$adm1 <- "Estado de México"
	d$adm2 <- "Texcoco"
	d$elevation <- 2257  # define as numeric, not character
	d$longitude <- -98.88056
	d$latitude <- 19.517
	d$geo_from_source <- FALSE
	d$crop <- tolower(d$crop)
	d$rep <- as.integer(d$rep)
	
	d[d=="."] <- NA # to avoid warnings from as.numeric

	for (v in c("flowering_days" , "maturity_days", "plant_height", "dmy_storage", "yield", "seed_weight", "silking_days")) {
		d[[v]] <- as.numeric(d[[v]])
	}
	d$plant_density <- as.numeric(d$plant_density) * 1000

	# empty rep 
	d <- d[!is.na(d$yield), ]
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	carobiner::write_files(path, meta, d)
}


