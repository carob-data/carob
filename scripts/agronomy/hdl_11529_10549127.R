# R script for "carob"
# license: GPL (>=3)


carob_script <- function(path) {
  
 
"
Yield of maize and wheat under permanent beds in a long-term experiment in Mexico's semi-arid highlands

This dataset includes experimental data collected from a long-term field experiment conducted over 20 years (1999-2019) in a semiarid region of central Mexico.

The study focused on evaluating the effects of various soil management practices—including the use of permanent beds, crop residue management, and tied ridges—on the yield and profitability of maize (Zea mays L.) and wheat (Triticum aestivum L.) under rainfed conditions.

The dataset includes: Grain yield data for maize and wheat, Biomass production measurements, Information on soil management practices applied during the experiment.
"

	uri <- "hdl:11529/10549127"
	group <- "agronomy"  
	ff  <- carobiner::get_data(uri, path, group)
  
	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication = "doi:10.1002/agj2.70106",
		project = NA,
		data_type = "on-farm experiment",
		treatment_vars = "land_prep_method;residue_prevcrop_used",
		response_vars = "yield", 
		completion = 90,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2025-08-11",
		notes = NA,
		design = NA
	)
   
	f <- ff[basename(ff) == "DAT-H9Texcoco-1999-2019_V2.xlsx"]
	r <- carobiner::read.excel(f, sheet = "Data field experiment", na = ".")

	d <- data.frame(
		# location from paper
		country = "Mexico",
		adm1 = "Mexico",
		adm2 = "Texcoco",
		location = "El Batán",
		elevation = 2200,
		latitude= 23.9513,
		longitude= -102.5144,

		planting_date= as.character(r$Year),
		crop=tolower(r$Crop),
		rep=as.integer(r$Repetition),
		treatment=as.character(r$Abbreviation),
		dmy_residue=r$`Biomass (kg/ha)`,
		yield=r$`Yield (kg/ha)`,
		grain_protein=as.numeric(r$`PGr (%)`),
		yield_moisture=12,
		residue_prevcrop_used = r$Residue != "Remove"
    )

	trt <- do.call(rbind, strsplit(r$Abbreviation, ", "))
	d$land_prep_method <- ifelse(trt[,1] == "CB", "conventional tilled beds", "permanent beds")
	i <- trt[,3] == "NT"
	d$land_prep_method[i] <- paste0(d$land_prep_method[i], "; open furrows")
	d$land_prep_method[!i] <- paste0(d$land_prep_method[!i], "; tied ridges")

	d$seed_density <- ifelse(d$crop=="maize", 67500, NA)
	d$seed_rate <- ifelse(d$crop=="maize", NA, 105)

	d$K_fertilizer <- as.numeric(NA)
	d$trial_id <- d$planting_date
	d$yield_part<- "grain"

	d$on_farm <- FALSE
	d$is_survey <- FALSE 
	d$irrigated <- FALSE
	d$geo_from_source <- FALSE


	# table S1 from paper
	x <- data.frame(
		planting_date = c("1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"), 
		variety = c("WEEBILL #1 (36Y)", "WEEBILL #1 (36Y)", "BABAX/LR42//BABAX", "IRENA/BABAX//PASTOR", "CNO79//PF70354/MUS/3/PASTOR/4/BABAX", "IRENA/BABAX//PASTOR", "MILAN/KAUZ//PRINIA/3/BABAX", "BABAX/LR42//BABAX", "BABAX/LR42//BABA", "BABAX/LR42/BABAX", "IRENA/BABAX/PASTOR", "IRENA/BABAX/PASTOR", "27TH SAWSN/2034/TC870344/GUI", "27TH SAWSN/2034/TC870344/GUI", "VALK", "NANA", "DON CARLOS", "WAXBILL", "WAXBILL", "KASUKO", "KASUKO", "BA-99-2222-CMT-939011", "BA-99-2222-CMT-939011", "CMS 939083", "CMS 939083", "BA03-2199", "BA03-2199", "BA01-2143", "CML.457 x CML.45", "CML457XCML459", "CML457XCML459", "BUHO", "BUHO", "OCELOTE", "OCELOTE", "OCELOTE", "OCELOTE", "AS722", "XR-12", "XR-12", "CHLHW09035", "CHLHW09035"), 
		N_fertilizer = c(120, 120, 120, 120, 120, 120, 120, 120, 120, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 120, 120, 120, 120, 120, 120, 120, 120, 120, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150), 
		P_fertilizer = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 17, 17, 17, 17, 17, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 17, 17, 17, 17, 17, 17), 
		crop = c("wheat", "wheat", "wheat", "wheat", "wheat", "wheat", "wheat", "wheat", "wheat", "wheat", "wheat", "wheat", "wheat", "wheat", "wheat", "wheat", "wheat", "wheat", "wheat", "wheat", "wheat", "maize", "maize", "maize", "maize", "maize", "maize", "maize", "maize", "maize", "maize", "maize", "maize", "maize", "maize", "maize", "maize", "maize", "maize", "maize", "maize", "maize")
	)

	x$fertilizer_type <- "urea"
	x$fertilizer_type[x$planting_date < "2009" & x$crop == "wheat"] <- "AN"
	x$fertilizer_type[x$P_fertilizer == 17] <- paste0(x$fertilizer_type[x$P_fertilizer == 17], "; TSP"); 

	d <- merge(d, x, by=c("planting_date", "crop"))

	carobiner::write_files(path, meta, d)
}
