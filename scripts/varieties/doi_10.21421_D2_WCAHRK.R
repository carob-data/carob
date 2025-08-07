# R script for "carob"
# license: GPL (>=3)


carob_script <- function(path) {

"
Data on Nitrogen Use Efficiency in Sorghum: exploring native variability for traits under variable N-regimes

A set of sixty diverse Sorghum genotypes including parents of several genetic populations such as Back-Cross derived Nested Association Mapping (BCNAM) populations; bi-parental mapping populations, along with accessions from different countries such as Nigeria, USA, Lesotho, Ethiopia, and Mali were evaluated for different leaf, growth, panicle, and biological yield traits along with NUE traits in the field (under three N regimes0%, 50% and 100% of the recommended (90 kg ha-1 for two seasons (2016-17 & 2017-18)) in the black soil precision fields of ICRISAT, Patancheru, India). Different physiological, agronomical, and biological yield attributes associated with NUE were systematically recorded in three different N dosages; chlorophyll content, leaf area, leaf number, days to 50% flowering, plant height, a number of tillers, panicle number, panicle weight, fresh straw yield, dry straw yield, grain yield, test weight, harvest Index, N content in grain and straw. On the basis of the data selection under high and low nitrogen, efficient lines will be identified and contribute towards the development of nitrogen efficient sorghum cultivars.
"

	uri <- "doi:10.21421/D2/WCAHRK"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = "doi:10.3389/fpls.2021.643192",
		project = NA,
		carob_date = "2025-08-07",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;N_fertilizer",
		response_vars = "yield;fwy_stems;dmy_stems", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	
	f <- ff[basename(ff) == "NUE_60_2016-2017.xlsx"]

	r1 <- carobiner::read.excel(f, sheet="2016", na=("."))
	r2 <- carobiner::read.excel(f, sheet="2017", na= ("."))

	### Processing r1
	d1 <- data.frame(
		plot_id = as.character(r1$PLOT_NO),
		trial_id= as.character(r1$TRIAL_I.TANCE),
		variety= r1$DESIGNATION,
		N_fertilizer= r1$NFERT_NO*90/100,
		P_fertilizer= 0,
		K_fertilizer= 0,
		rep= as.integer(r1$REP_NO),
		flowering_days= r1$Days_to_flowering,
		plant_height= r1$Plant_height, 
		fwy_stems= r1$StmHvYld_C_kgha,
		dmy_stems= r1$StmDMYld_C_kgha,
		#seed_weight= r1$GW_100grnM_g*10,
		yield= r1$GHvYld_C_kgha,
		harvest_index= r1$GHI_C_pct,
		residue_N= (r1$StoND_Kjdl_pct*100)*10, ##from % to mg/g
		crop= "sorghum",
		planting_date= "2016-12-15",
		harvest_date= "2017-04-27",
		is_survey= FALSE,
		on_farm= TRUE,
		yield_part= "grain",
		irrigated= NA
	)
	### Processing r2
	 d2 <- data.frame(
	      plot_id = as.character(r2$PLOT_NO),
	      trial_id= as.character(r2$TRIAL_INSTANCE),
	      variety= r2$DESIGNATION,
	      N_fertilizer= r2$NFERT_NO *90/100,
	      P_fertilizer= 0,
	      K_fertilizer= 0,
	      rep= as.integer(r2$REP_NO),
	      flowering_days= r2$Days_to_flowering,
	      plant_height= r2$Plant_height, 
	      fwy_stems= r2$StmHvYld_C_kgha,
	      dmy_stems= r2$StmDMYld_C_kgha,
	      #seed_weight= r2$GW_100grnM_g*10, 
	      yield= r2$GHvYld_C_kgha,
	      harvest_index= r2$GHI_C_pct,
	      residue_N= (r2$StoND_Kjdl_pct*100)*10, ##from % to mg/g
	      crop= "sorghum",
	      planting_date= "2017-11-17",
	      harvest_date= "2018-04-04",
	      is_survey= FALSE,
	      on_farm= TRUE,
	      yield_part= "grain",
	      irrigated= NA
	)
	 
	 d <- rbind(d1, d2)
	 d$yield_moisture <- as.numeric(NA)
	 
	 d$country <- "India"
	 d$adm1 <- "Hyderabad"
	 d$location <- "Patancheru"
	 d$site <- "ICRISAT"
	 d$latitude <- 17.5180 
	 d$longitude <- 78.27903  
	 d$geo_from_source <- FALSE
	 

	carobiner::write_files(path, meta, d)
}

