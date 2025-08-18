# R script for "carob"
# license: GPL (>=3)

## ISSUES
## Missing plot size 

carob_script <- function(path) {

"
Agronomic performance of Sorghum margaritiferum Kende in the Guinean zone of Mali-West and Central Africa

Sorghum bicolor, sub-race margaritiferum trial conducted by ICRISAT-Mali and IER in IER research station at Farako, Sikasso region in 2014. The objective was to identify sorghum varieties with very vitreous grains, adapted to Guinean zone where new variety has not been released many years ago. Our main target group was women who were looking for sorghum variety named 'Kende' in Bambara language, known for their vitreous grains for local food processing. We have applied the recommended doses of fertilizer (DAP = 100 kg/ha and urea = 50 kg/ha). The main traits recorded include plant vigor, plant height, grains weight and farmer appreciation of panicles and grains.  

  Experiment Location on Google map
"
   
	uri <- "doi:10.21421/D2/CHAUNT"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "ICRISAT; IER",
		publication =NA,
		project = NA,
		carob_date = "2025-08-18",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;dmy_storage", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f <- ff[basename(ff) == "Data file of Kende ML Farako.xlsx"]

	r <- carobiner::read.excel(f)

	d <- data.frame(
		plot_id = as.character(r$`Plot No`) ,
		rep= as.integer(r$`Replication number`),
		variety= r$Hybrids,
		#r$SdlgVig_E_1to5,
		heading_date= as.character(as.Date(r$Hd_date_jd, origin = "1899-12-31")),
		heading_days= r$Hd_C_day,
		plant_height= r$PH_M_cm,
		fertilizer_type= "DAP;urea",
		N_fertilizer= 100*0.18 + 0.46*50, ## DAP= 100kg/ha, urea= 50kg/ha
		P_fertilizer= 100*0.201,
		K_fertilizer= 0 ,
		fertilizer_amount= 150, 
		fertilizer_used= TRUE,
		#plot_area= , #m2
		dmy_storage= r$`Grain dry weight`,
		yield = r1$`Grain yield`,
		yield_moisture= 0,
		crop= "sorghum",
		planting_date= "2014",
		adm1= "Sikasso",
		location="iscrisat, Farako",
		country= "Mali",
		longitude= -8.0723 ,
		latitude= 12.5306,
		geo_from_source= FALSE,
		yield_part= "grain",
		is_survey= FALSE,
		irrigated= NA,
		on_farm= FALSE,
		trial_id= "1"
	)
	
	

	carobiner::write_files(path, meta, d)
}


