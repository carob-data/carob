# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Yield stability assessment -- Rice and Pearl millet data

1. Sadoré , Niger: Data are from field experiments carried out at ICRISAT research station (13.25°N, 2.283°E, 240 m above sea level) under rainfed conditions. Experimental design: factorial combination of two plant densities [10,000 pockets ha–1 (PDENS1) and 15,000 pockets ha–1 (PDENS2)] with three levels of fertilization [control (no fertilizer), one microdose of 2 g diammonium phosphate (DAP), and one microdose of 6 g of 15-15-15 NPK per planting hole] and two levels of crop residue (CR) management (with and without CR retention). For each level of CR management, the treatments were organized in a randomized complete block design with three replications (individual plot size of 7 m × 7 m). The pearl millet variety grown was Haïni Kirey Précoce.

 2. Ndiaye, Senegal: Data are from field experiments established at the AfricaRice research station (16.219°N, 16.278°W, 12 m above sea level) as a continuous double cropping system during the wet season and the hot dry season. Experimental design: randomized complete block design with six treatments and four replications (individual plot size of 5 m × 5 m). Fertilizer treatments included (T1) 0–0–0 kg NPK/ha, (T2) 60-26-50 kg NPK/ha, (T5 120-26-50 kg NPK/ha, and (T6) 180-26-50 kg NPK/ha. Inorganic fertilizers used were urea (46 % N), DAP (46 % P), and potassium chloride (KCI, 51 % K). All treatments were fully irrigated from transplanting until 14 days prior to maturity. The rice variety grown was Sahel 108 (IR 13240–108-2-2-3).
"

   uri <- "doi:10.7910/DVN/GFWCYQ"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT;AfricaRice",
		publication ="doi:10.1371/journal.pone.0317170",
		project = NA,
		carob_date = "2025-08-20",
		design = NA,
		data_type ="experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;plant_density",
		response_vars = "yield;fwy_total", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	
	
	
	f1 <- ff[basename(ff) == "Yield_and_Biomass_data_NDiaye_Senegal.xlsx"]
	f2 <- ff[basename(ff) == "Yield_and_Biomass_data_Sadore_Niger.xlsx"]

	
	r1 <- carobiner::read.excel(f1, fix_names = TRUE)
	r2 <- carobiner::read.excel(f2, fix_names = TRUE)
	
### process
	## crop: rice
	d1 <- data.frame(
	   planting_date = as.character(r1$Year),
		season = ifelse(grepl("Dry", r1$Cropping.season), "dry", "wet") ,
		treatment = r1$Fertilizer.treatment,
		N_fertilizer= ifelse(grepl("T2", r1$Fertilizer.treatment), 60, 
		              ifelse(grepl("T5", r1$Fertilizer.treatment), 120, 
		              ifelse(grepl("T6", r1$Fertilizer.treatment), 180, 0))),
		
		P_fertilizer= ifelse(grepl("T2|T5|T6", r1$Fertilizer.treatment), 26, 0),
		K_fertilizer= ifelse(grepl("T2|T5|T6", r1$Fertilizer.treatment), 50, 0),
		fertilizer_type= ifelse(grepl("T2|T5|T6", r1$Fertilizer.treatment), "DAP;KCl;urea", "none"),
		fwy_total= r1$Avg.Observed.Aboveground.Biomass.t.ha* 1000,
		yield = r1$Avg.Observed.Yield.t.ha* 1000,
		crop= "rice",
		variety= "sahel 108",
		country= "Senegal",
		location= "Ndiaye",
		longitude= -16.278,
		latitude= 16.219,
		elevation= 12,
		plot_area= 25, #m2
		irrigated= TRUE,
		is_survey= FALSE,
		yield_part= "grain",
		on_farm= FALSE,
		trial_id= "1",
		yield_moisture = as.numeric(NA), 
		geo_from_source= TRUE
	)
	
	d1$treatment <- ifelse(grepl("T2|T5|T6", r1$Fertilizer.treatment), paste( "N", d1$N_fertilizer, "P", d1$P_fertilizer, "K", d1$K_fertilizer, sep = ""), "control")
	
	
	## crop: pearl millet
	d2 <- data.frame(
		crop = tolower(r2$Crop),
		variety= "Haïni Kirey Précoce",
		planting_date = as.character(r2$Year),
		treatment = r2$Fertilizer.treatment,
		N_fertilizer= ifelse(grepl("T1", r2$Fertilizer.treatment) & grepl("PDENS_1", r2$Plant.density), 8.2,
		              ifelse(grepl("T1", r2$Fertilizer.treatment) & grepl("PDENS_2", r2$Plant.density), 9, 
		              ifelse(grepl("T2", r2$Fertilizer.treatment) & grepl("PDENS_1", r2$Plant.density), 12.3, 
		              ifelse(grepl("T2", r2$Fertilizer.treatment) & grepl("PDENS_2", r2$Plant.density), 13.5, 0)))),
		
		P_fertilizer= ifelse(grepl("T1", r2$Fertilizer.treatment) & grepl("PDENS_1", r2$Plant.density), 4.02,
		              ifelse(grepl("T1", r2$Fertilizer.treatment) & grepl("PDENS_2", r2$Plant.density), 3.93, 
		              ifelse(grepl("T2", r2$Fertilizer.treatment) & grepl("PDENS_1", r2$Plant.density), 6.03, 
		              ifelse(grepl("T2", r2$Fertilizer.treatment) & grepl("PDENS_2", r2$Plant.density), 5.9, 0)))),
		
		K_fertilizer= ifelse(grepl("T1", r2$Fertilizer.treatment) & grepl("PDENS_2", r2$Plant.density), 7.47,
		              ifelse(grepl("T2", r2$Fertilizer.treatment) & grepl("PDENS_2", r2$Plant.density), 11.21, 0)),
	
			fertilizer_type= ifelse(grepl("T2|T1", r2$Fertilizer.treatment), "DAP;NPK", "none"),
		residue_prevcrop_used= ifelse(grepl("Yes", r2$Crop.residue.retention), TRUE, FALSE) ,
		plant_density= ifelse(grepl("PDENS_1", r2$Plant.density), 10000, 15000) ,
		fwy_total= r2$Avg.Observed.Aboveground.Biomass.t.ha * 1000,
		yield = r2$Avg.Observed.Yield.t.ha* 1000,
		country= "Niger",
		location= "Sadoré",
		longitude= 2.283,
		latitude= 13.25,
		elevation= 240,
		plot_area= 7*7, #m2
		irrigated= NA,
		is_survey= FALSE,
		yield_part= "grain",
		on_farm= FALSE,
		trial_id= "2",
		yield_moisture = as.numeric(NA), 
		geo_from_source= TRUE
	)

	d2$treatment <- ifelse(grepl("T2|T1", r2$Fertilizer.treatment), paste( "N", d2$N_fertilizer, "P", d2$P_fertilizer, "K", d2$K_fertilizer, sep = ""), "control")
	
	d2$planting_date <- ifelse(grepl("2011", d2$planting_date), paste0(d2$planting_date,"-", "06-20"),
	                    ifelse(grepl("2012", d2$planting_date), paste0(d2$planting_date,"-", "06-12"), 
	                    ifelse(grepl("2013", d2$planting_date), paste0(d2$planting_date,"-", "07-02"),
	                    ifelse(grepl("2014", d2$planting_date), paste0(d2$planting_date,"-", "06-01"),
	                    ifelse(grepl("2015", d2$planting_date), paste0(d2$planting_date,"-", "07-09"),
	                    ifelse(grepl("2016", d2$planting_date), paste0(d2$planting_date,"-", "06-15"),
	                    ifelse(grepl("2017", d2$planting_date), paste0(d2$planting_date,"-", "06-14"), paste0(d2$planting_date,"-", "06-24"))))))))
	
	### bind d1 and d2
	d <- carobiner::bindr(d1, d2)
	
	## Adding soil
	
	soil <- data.frame(
	   location= c("Sadoré", "Ndiaye"),
	   depth_top= c(5, 0),
	   depth_bottom= c(10, 21),
	   soil_bd= c(1.55, 1.5),
	   soil_clay= c(9.1, 40),
	   soil_sand= c(85.5, 16),
	   soil_texture= c("loamy sand", "clay"),
	   soil_SOM= c(NA, 7.4),
	   soil_SOC= c(2.6, 4.3),
	   soil_N= c(0.2, 0.3)
	)
	
	d <- merge(d, soil, by="location", all.x = TRUE)
	
carobiner::write_files(path, meta, d)

}

