# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Dry-direct seeded rice experiments in Odisha

On-farm trials were conducted from 2016 to 2018 in four districts of Odisha (Mayurbhanj, Cuttack, Bhadrak, and Puri to evaluate the potential of dry-direct seeding of rice (DSR) in combination with integrated weed management (IWM) to reduce the yield gap and increase the income of rice farmers in Odisha where traditional practice of beushening has been practiced. The experiments were conducted in collaboration with Odisha University of Agriculture and Technology (OUAT), and National Rice Research Institute (NRRI). Agronomic data were collected manually from the treatments in each experiment and processed in excel file. Data on labour use, seed, cost of cultivation, grain yield, straw yield, gross income, net benefit and benefit cost ration were collected or calculated for each experiment and presented in excel file.
"

	uri <- "hdl:11529/10548512"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT; IRRI; OUAT; CU",
		publication = NA,
		project = NA,
		carob_date = "2026-06-04",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "planting_method;N_fertilizer;P_fertilizer;K_fertilizer;herbicide_used",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	# how is it 100% if you do not process most variables???
		notes = NA
	)
	

	#f1 <- ff[basename(ff) == "Experiment_Info_Sheet.csv"]
	#f2 <- ff[basename(ff) == "Experiment_Variables_Detail.csv"]
	#r1 <- read.csv(f1)
	#r2 <- read.csv(f2)

	ff1 <- ff[grepl("Data", basename(ff))]
	

	proc <- function(f){
	  r <- unique(read.csv(f))
	  names(r) <- gsub("Treatment.Description", "Treatment.descripton", names(r))
	  if(is.null(r$SdDate)) r$SdDate <- as.character(r$Year)
	  if(is.null(r$Treatment.descripton)) r$Treatment.descripton <- NA
	  if(is.null(r$Weeder_cost)) r$Weeder_cost <- NA
	  if(is.null(r$IR_cost)) r$IR_cost <- NA
	  data.frame(
	    adm1 = carobiner::fix_name(r$STATE, "title"),
		  location = carobiner::fix_name(r$District, "title"),
		  planting_method = gsub("drill-dsr", "direct seeding", tolower(r$CropEst)),
		  variety = trimws(r$Var),
		  planting_date = ifelse(grepl("/", r$SdDate), as.character(as.Date(r$SdDate, "%Y/%m/%d")), r$SdDate),
		  seed_rate = r$SdRate,
		  seed_cost = r$SdCost,
		  treatment = trimws(r$Treatment.descripton),
		  herbicide_product = ifelse(grepl("pretilachlor", r$Treatment.descripton), "pretilachlor",
		                      ifelse(grepl("Bispyribac", r$Treatment.descripton), "bispyribac-sodium;pyrazosulfuron", 
		                      ifelse(grepl("^0$",r$Herbi_cost), "none", "unknown"))),
		  herbicide_amount = ifelse(grepl("pretilachlor", r$Treatment.descripton), 0.5,
		                     ifelse(grepl("Bispyribac", r$Treatment.descripton), 0.04, 
		                     ifelse(grepl("^0$", r$Herbi_cost), 0, NA))),
		  herbicide_used = !grepl("^0$",r$Herbi_cost),
		  herbicide_cost = r$Herbi_cost,

		  #pesticide_price = r$Pest_cost,
		  #pesticide_used = !grepl("^0$", r$Pest_cost),
		  weeding_done = !grepl("^0$",r$Weedmgt_cost),
		  N_fertilizer = r$FN_amt,
		  P_fertilizer = r$FP_amt,
		  K_fertilizer = r$FK_amt,
		  fertilizer_cost = r$Fert_cost,
		  yield = r$GY* 1000,
		  dmy_total = r$StDM*1000,
		  trial_id = gsub(".csv", "", basename(f)),
		  crop = "rice",
		  country = "India",
		  on_farm = TRUE, 
		  is_survey = FALSE, 
		  yield_part = "grain", 
		  yield_moisture = as.numeric(NA),
		  yield_isfresh = NA,
		  irrigation_cost = r$IR_cost,
		  irrigated = ifelse(!is.na(r$IR_cost), !grepl("^0$", r$IR_cost), NA),
		  net_benefit = r$Net_benefit,
		  planting_cost = r$CropEst_cost,
		  variable_cost = r$Variable_cost,
		  harvest_cost = r$Harvest_cost,
		  weeding_cost= r$Weedmgt_cost,
		  weeding_equiment_cost = r$Weeder_cost,
		  weeding_labour = r$Lab_nm
	   )
	}

		
	d <- lapply(ff1, proc)
	d <- do.call(rbind, d)

  i <- d$planting_method == "bueshening"
	d$planting_method[i] <- "direct seeding"
	d$land_prep_method[i] <- "post-emergence tillage" #beushening
		
	### Adding long and lat coordinate
	geo <- data.frame(
	  location = c("Bhadrak", "Mayurbhanj", "Cuttack","Puri" ),
	  longitude = c(86.497, 86.3389,  85.883, 85.8306),
	  latitude = c(21.0574, 21.936, 20.4621, 19.8136),
	  geo_from_source = FALSE
	  )
	
	d <- merge(d, geo, by = "location", all.x = TRUE)
	
	carobiner::write_files(path, meta, d)
}


