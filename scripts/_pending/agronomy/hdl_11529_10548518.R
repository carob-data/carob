# R script for "carob"

## RH has contacted AA for georeferencing
## years 2 and 3 do not have village names 
# There are three years of experimental data on maize in Mayurbhanj district in Odisha. The data for the first year includes the village names, but the second and third years do not have that.  None of the years has location coordinates. 
# There is data on soil properties in the associated publication https://doi.org/10.1017/S0014479722000187; but the the village names in the publication do not match the names in the data very well.

carob_script <- function(path) {

"Maize is the staple crop cultivated during the monsoon season in the rainfed uplands in tribal-dominated plateau regions of Odisha in eastern India. However, productivity is low because of multiple factors, including poor adoption of best management practices. We conducted three types of experiments viz..single vs. layered best management practices, hybrids, and decision support tools on nutrient management for two years (2013 and 2014) to explore the opportunities for reducing rainfed maize yield gaps. On-farm trials were conducted in Mayurbhanj district in Odisha and data were collected manually from the treatments in each experiment and processed in excel file and analyzed using R software. (2020-09-30)"

	uri <- "hdl:11529/10548518"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = "doi:10.1017/S0014479722000187",
		project = NA,
		data_type = "on-farm experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;lime_used",
		response_vars = "yield", 
		carob_contributor = "Blessing Dzuda",
		carob_effort = NA,
		carob_date = "2024-09-05"
	)
	
	f1 <- ff[basename(ff) == "Experiment-I.csv"]
	f2 <- ff[basename(ff) == "Experiment-II.csv"]
	f3 <- ff[basename(ff) == "Experiment-III.csv"]
	
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)

	r1$Treat_details <- iconv(r1$Treat_details, from = "latin1", to = "UTF-8", sub = "")
	r3$Treat_details <- iconv(r3$Treat_details, from = "latin1", to = "UTF-8", sub = "")
	
	d1 <- data.frame(
		planting_date = as.character(r1$Year),
		adm1=r1$STATE,
		adm2=r1$District,
		location=r1$Village,
		treatment=r1$Treat_details,
		rep=r1$Rep,
		land_prep_method=tolower(r1$Tillage),
		variety=r1$Var,
		maturity_days=r1$Duration,
		seed_density=r1$SdRate,
		row_spacing=NA,
		plant_spacing=NA,
		N_fertilizer=r1$FN_amt,
		P_fertilizer=r1$FP_amt,
		K_fertilizer=r1$FK_amt,
		weeding_done=TRUE,
		weeding_method=r1$Weedmgt_type,
		fertilizer_price=r1$Fert_cost,
		plant_density=r1$PN,
		yield=r1$GY
	)
		
	d2 <- data.frame(
		planting_date = as.character(r2$Year),
		adm1=r2$STATE,
		adm2=r2$District,
		location=NA,
		treatment=r2$Treatment,
		rep=r2$Rep,
		land_prep_method=NA,
		variety=NA,
		maturity_days=r2$Duration,
		seed_density=NA,
		row_spacing=NA,
		plant_spacing=NA,
		N_fertilizer=r2$FN_amt,
		P_fertilizer=r2$FP_amt,
		K_fertilizer=r2$FK_amt,
		weeding_done=TRUE,
		weeding_method=NA,
		fertilizer_price=r2$Fert_cost,
		plant_density=NA,
		yield=r2$GY
	)
		
	d3 <- data.frame(
		planting_date = as.character(r3$Year),
		adm1=r3$STATE,
		adm2=r3$District,
		location=NA,
		treatment=r3$Treat_details,
		rep=r3$Rep,
		land_prep_method=NA,
		variety=r3$Var,
		maturity_days=NA,
		seed_density=r3$SdRate,
		row_spacing=r3$RS,
		plant_spacing=r3$PS,
		N_fertilizer=r3$FN_amt,
		P_fertilizer=r3$FP_amt,
		K_fertilizer=r3$FK_amt,
		weeding_done=TRUE,
		weeding_method=NA,
		fertilizer_price=r3$Fert_cost,
		plant_density=NA,
		yield=r3$GY
	)
	
	d <- rbind(d1, d2, d3)
	d$treatment <- gsub("\u0092", "'", d$treatment)
	
	d$country <- "India"
	d$crop <- "maize"
	d$yield <- d$yield * 1000
     
	d$trial_id <- as.character(as.integer(as.factor(paste(d$adm2, d$location, d$year))))
	d$land_prep_method <- gsub("zerotillage", "none", d$land_prep_method)
    d$fertilizer_price <- as.character(d$fertilizer_price)
	
	d$yield_part <- "grain"
	d$on_farm <- TRUE
	d$is_survey <- FALSE 
	d$irrigated <- NA

	d$lime_used <- grepl("\\+lime", d$treatment, ignore.case=TRUE)
	micro <- grepl("Micro", d$treatment)

	# from publication
	# lime 500 kg ha−1 as paper mill sludge
	# Zn sulfate 25 kg ha−1, S 25 kg ha−1, B 10 kg ha−1 as Borax,
	d$lime <- d$lime_used * 500
    d$S_fertilizer <- ifelse(micro, 25, 0)
    d$B_fertilizer <- ifelse(micro, 10, 0)
    d$Zn_fertilizer <- ifelse(micro, 25 * .405, 0)
	d$N_splits  <- ifelse(d$S_fertilizer > 0, 2, NA) |> as.integer()
	

# unique(d$location) |> sort()
# [1] "Bagdofa"        "Barbill"        "Baria"          "Batupondugandi" "Deogaon"        "Jashipur"       "Labda"          "Rasamtala"     
# [9] "Sarangarh"      "Singarpur"      "Tikarpara"      "Tisira"        

# Table 1; names do not match
	soils <- data.frame(
		location = c("Badbil", "Deogaon", "Kashipal", "Majigaon", "Dhanguriposi", "Panasi", "Batapondugondi", "Dayaposi"), 
		#            "Barbill", "Deogaon", "?",           "?",     "?",            "?",     "Batupondugandi"," ?"
		pH = c(4.9, 4.77, 5.42, 4.94, 5, 5.25, 4.84, 4.82), 
		EC.dSm = c(0.17, 0.13, 0.09, 0.12, 0.12, 0.09, 0.09, 0.1), 
		Organic.carbon = c(0.14, 0.32, 1, 0.56, 0.26, 0.82, 0.36, 0.36), 
		Available.nitrogen.kg.ha = c(150, 112, 162, 137, 150, 200, 150, 150), 
		Available.phosphorus.kg.ha = c(63, 66, 26, 30, 28, 92, 38, 80),
		Available.potassium.kg.ha = c(228, 117, 308, 364, 197, 328, 247, 86)
	)
	
	d <- unique(d)
    carobiner::write_files(path, meta, d)
}

