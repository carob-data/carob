# R script for "carob"
# license: GPL (>=3)

## ISSUES



carob_script <- function(path) {

"
Data from: Using perennial groundcover crops to suppress weeds and thrips in the southeast cotton belt

This is digital research data corresponding to a published manuscript, Using perennial groundcover crops to suppress weeds and thrips in the southeast cotton belt, in Crop Science, Vol. 63 p. 3037 - 3050. Modern cotton production (Gossypium hirsutum L.) in the United States relies on chemical and physical inputs that increase the environmental and monetary costs of managing the crop. Perennial groundcover crops (PGCC) may reduce inputs by persisting in the interrow spaces of the cotton crop during summer months. A 2-year field study was conducted in Florence, SC, to evaluate growing PGCCs with cotton using a 4 × 4 Latin square consisting of four cover crop treatments: (1) a fallow, unplanted control, (2) annual ryegrass (Lolium multiflorum Lam.) monoculture, (3) a binary red clover (Trifolium pratense L.) and white clover (Trifolium repens L.) mixture, and (4) a trinary mixture of annual ryegrass, red clover, and white clover. Fallow and annual ryegrass treatments were killed with a burndown herbicide application, while treatments containing clovers were mowed. Plots were strip-tilled and planted with cotton in May each year. Interrow biomass, weed and thrips populations, and perennial clover populations were collected from June to October along with annual lint yields from cotton harvest in October.
"

	uri <- "doi:10.5061/dryad.c866t1gcs"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=NA,
		data_organization = "USDA-ARS;CLU",
		publication = "doi:10.1002/csc2.21048",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "cover_crop",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-09-11",
		carob_completion = 80,	
		carob_effort = 2
	)
	

	f1 <- ff[basename(ff) == "PGCC_Manuscript_1_Dryad_Data.xlsx"]
	#f2 <- ff[basename(ff) == "README.md"]

	r1 <- carobiner::read.excel(f1, sheet="Figure 1")
	r2 <- carobiner::read.excel(f1, sheet="Figure 2a & 2b")
	r3 <- carobiner::read.excel(f1, sheet="Figure 3a & 3b")
	r4 <- carobiner::read.excel(f1, sheet="Figure 7a & 7b")
	#r5 <- carobiner::read.excel(f1, sheet="Figures 4a, 4b & 5a, 5b")
	#r6 <- carobiner::read.excel(f1, sheet="Figure 6a, 6b, 6c, & 6d")


### process	
	d1 <- data.frame(
		date = paste(gsub("1990-2020", "2020", r1$Year), c("January" = "01", "February" = "02", "March" = "03", "April" = "04", "May" = "05", "June" = "06", "July" = "07", "August" = "08", "September" = "09", "October" = "10", "November" = "11", "December" = "12")[r1$Month], sep = "-"),
		prec = as.numeric(gsub("^.$", NA, r1$`Rainfall Total (mm)`)),
		temp = as.numeric(gsub("^.$", NA,r1$`Avg Air Temp (C)`)),
		tmax = as.numeric(gsub("^.$", NA,r1$`Max Air Temp (C)`)),
		tmin = as.numeric(gsub("^.$", NA,r1$`Min Air Temp (C)`)),
		location = "Florence",
		country  = "United States",
		geo_from_source = FALSE,
		latitude = 34.1981,
		longitude = -79.7697
	)


	d2 <- data.frame(
	  year = r2$Year,
	  season = tolower(r2$Season),
	  planting_date = paste(r2$Year,c("August"= "08", "Sept" = "09", "June" = "06", "July" = "07" )[r2$Month],sep = "-"),
	  treatment = r2$Trt,
	  cover_crop = c("ARG+RC+WC" = "annual ryegrass;red clover;white clover", "RC+WC" = "red clover;white clover", "ARG" = "annual ryegrass", "Fallow" = "none" )[r2$Trt],
	  rep = as.integer(r2$Rep),
	  plot_id = as.character(r2$Plot),
	  dmy_total = r2$`Dry Weight (kg/ha)`
	)


	d3 <- data.frame(
		season = tolower(r3$Season),
		planting_date = paste(c(2021, 2022)[r3$Year],c("October"= "10","August"= "08", "Sept" = "09", "June" = "06", "July" = "07" )[r3$Month],sep = "-"),
		treatment = r3$Trt,
		cover_crop = c("ARG+RC+WC" = "annual ryegrass;red clover;white clover", "RC+WC" = "red clover;white clover", "ARG" = "annual ryegrass", "Fallow" = "none" )[r3$Trt],
		rep = as.integer(r3$Rep),
		plot_id = as.character(r3$Plot),
		weed_density = r3$`Weeds/m2`
	)

	### merge d2 and d3
	d3_Ag <- aggregate(. ~ season+ plot_id + treatment +rep + cover_crop+planting_date,d3, function(X) mean(X) )
	d <- merge(d2, d3_Ag, by= c("season", "planting_date", "treatment", "cover_crop", "rep", "plot_id"), all.x  = TRUE)

	d4 <- data.frame(
		year = r4$Year,
		plot_id = as.character(r4$Plot),
		treatment = r4$Treatment,
		cover_crop = c("ARG+RC+WC" = "annual ryegrass;red clover;white clover", "RC+WC" = "red clover;white clover", "ARG" = "annual ryegrass", "Fallow" = "none" )[r4$Treatment],
		yield_seed = r4$`Seedcotton yield (lbs/ac)`*1.12, # kg/ha
		rep = as.integer(r4$Rep),
		yield_lint = r4$`Lint Yield (kg/ha)`
	)
	
	
	### merge d and d4
	d <- merge(d, d4, by= c("year","treatment", "cover_crop", "rep", "plot_id"), all.x  = TRUE)
	
	d <- reshape(d, varying = c("yield_lint", "yield_seed"), v.names = "yield", timevar = "yield_part", times = c("lint", "seed"), direction = "long")
	d$id <- d$year <- NULL

	d$season <- "kharif"
	d$location <- "Florence"
	d$country <- "United States"
	d$crop <- "cotton"
	d$geo_from_source <- FALSE
	d$geo_source = "Goole Maps"
	d$latitude <-  34.1981
	d$longitude <- -79.7697
	d$is_survey <-  FALSE
	d$on_farm <-  TRUE 
	d$trial_id <-  "1"
	d$yield_moisture <- NA 
	d$irrigated <- NA  
	d$harvest_date <-  NA
	d$yield_isfresh <- NA
	
	d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <-  as.numeric(NA) 
	
	
	carobiner::write_files(path, meta, d, wth = d1)
}

