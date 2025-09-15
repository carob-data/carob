# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
BSF finger millet crowdsourcing data for Hoima Uganda

43 different accessions/varieties of finger millet were randomly distributed to 200 farmers using the Alliance of Bioversity International and CIAT Climmob Version 2 software( http://climmob.net/Version_2/menu.html ). Three blind varieties were given to each farmer coded A, B and C and were planted in three 2m by 3m plots. A total of 8 villages with 25 farmers from each were engaged. This data set comprised of a total of 163 farmers who managed to successfully evaluate the three blind varieties they were given and successfully shared their data.
"

	uri <- "doi:10.7910/DVN/1BCWOY"
	group <- "survey"	
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "BIOV; NARO",
		publication = NA,
		project = NA,
		carob_date = "2025-09-15",
		design = NA,
		data_type = "survey",
		treatment_vars = "variety",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f <- ff[basename(ff) == "Crowdsourcing data for millet in Hoima Uganda, 2017 to 2018.xlsx"]

	r1 <- carobiner::read.excel(f, sheet="Data")
	r2 <- carobiner::read.excel(f, sheet="Varieties given to farmers")
	#r3 <- carobiner::read.excel(f1, sheet="Key")

	### Process
	names(r1) <- r1[1, ] 
	d1 <- data.frame(
	   trial_id = r1$`Farmers ID`,
	   location = r1$Region,
	   farmer_gender = r1$`gender(M/F)`,
	   #age = r1$age,
	   planting_date = r1$`planting_date(yyyy-mm-dd)`,
	   harvest_date =  r1$`harvest_date(yyyy-mm-dd)`,
	   harvest_days = r1$`Days to harvest`,
	   yield_A = r1$weight_of_all_yield_of_the_variety_a_in_kg,
	   yield_B = r1$weight_of_all_yield_of_the_variety_b_in_kg,
	   yield_C = r1$weight_of_all_yield_of_the_variety_c_in_kg,
	   country = "Uganda",
	   crop = "finger millet",
	   is_survey = TRUE,
	   on_farm = FALSE,
	   geo_from_source = FALSE,
	   yield_moisture = as.numeric(NA),
	   irrigated = NA,
	   longitude = 31.3489 ,
	   latitude = 1.4273,
	   yield_part = "grain"
	   
	)
	
	d2 <- data.frame(
	   trial_id = r2$`Farmers ID`,
	   variety_A = r2$`Variety A`,
	   variety_B = r2$`Variety B`,
	   variety_C = r2$`Variety C`,
	   plot_area = 6 # m2
	)
	
	
	dd <- merge(d1, d2, by= ("trial_id"), all.x = TRUE)[-1,]
	
	d <- reshape(dd, varying = list(c("yield_A", "yield_B", "yield_C"), c("variety_A", "variety_B", "variety_C")), v.names = c("yield", "variety"),
	             times = c("A", "B", "C"),
	             direction = "long")
	
	d$yield <- (as.numeric(d$yield)/6)*10000
	d$trial_id <- paste(d$trial_id, d$time, sep = "-")
	d$id	<- d$time <-  NULL
	
	### Fixing date
	d$planting_date <- as.character(as.Date(as.numeric(d$planting_date), origin = "1899-12-31"))
	d$harvest_date <- as.character(as.Date(as.numeric(d$harvest_date), origin = "1899-12-31"))
	d$harvest_days <- as.numeric(d$harvest_days)
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	### drop on row with missing yield
	d <- d[!is.na(d$yield), ]
	
carobiner::write_files(path, meta, d)

}

