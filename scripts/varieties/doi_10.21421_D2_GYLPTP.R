# R script for "carob"
# license: GPL (>=3)

## ISSUES
## 


carob_script <- function(path) {

"
Heat tolerance of chickpea (Cicer arietinum L.) genotypes in thermal zone of Ethiopia, a case of Werer station

This database includes the research work carried out to assess the variability among chickpea genotypes for heat tolerance and high yield potential under thermal zone of Ethiopia by screening at hot spot location. The station is located at 9°20'31' N and 40°10'11' E in the Middle Awash rift valley. The elevation is at about 740 meters above sea level. The long rainy season occurring from July to September accounts for 264 mm rainfall and the short rainy season from February to April accounts 156 mm. The minimum/maximum annual temperatures is 18.9OC/38OC, while the average annual temperature is 28.4OC.  Field evaluation of 18 early maturing chickpea genotypes including both Kabuli and Desi types was conducted during 2015 offseason using two planting dates of 33 January and 24th February. It involves advancing sowing date to synchronize the reproductive phase of the crop with the occurrence of higher temperature (>35OC). This method was employed to optimize planting date and to effectively identify heat tolerant germplasms. This study used measure of plant growth, yield traits and temperature prediction at different developmental stages of chickpea as tools for heat tolerance screening. Heat indices including SSI, TOL and STI were also calculated for better evaluation of genotypes for heat tolerance and some genotypes were found heat tolerant relative to others. Hence, these lines can be used in breeding program either for direct advancement based on their agronomic merit or could be used in crossing program for further manipulation.
"

	uri <- "doi:10.21421/D2/GYLPTP"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = "https://www.researchgate.net/publication/325094273",
		project = NA,
		carob_date = "2025-08-09",
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f <- ff[basename(ff) == "Data file of Heat tolerance of chickpea genotypes in thermal zone of Ethiopia.xlsx"]

	r <- carobiner::read.excel(f)
 
	d <- data.frame(
		variety = r$Treatment,
		rep= as.integer(r$`Replication number`),
		flowering_days= r$DFFL_1stCount_day,
		seed_weight= r$SDWT_100Wgh_g*10,
		yield= r$SYHA_Comp_kgha,
		fwy_total= (r$BY_Meas_gPlot/(4.8*1000))*10000, # from g/plot to  kg/ha
		harvest_index= r$HI_Comp_pct,
		location= "Werer station",
		country= "Ethiopia",
		longitude= 40.16,
		latitude= 9.33 ,
		elevation= 740,
		stress= "heat",
		crop= "chickpea",
		row_spacing= 30 ,
		plant_spacing= 10,
		plot_area= 4.8/10000,
		yield_part ="seed",
		trial_id= "1",
		is_survey= FALSE,
		on_farm= FALSE ,
		irrigated= NA,
		yield_moisture = as.numeric(NA),
		geo_from_source= TRUE,
		record_id= as.integer(1:nrow(r))
	)
	
 i <- which(d$record_id <= 63)	
 d$planting_date <- "2015-02-24"  	
 d$planting_date[i] <- "2015-01-23"
 
d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

carobiner::write_files(path, meta, d)

}


