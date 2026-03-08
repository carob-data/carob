# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Agronomic dataset on potato growth and yield in the Netherlands

Data was collected at two field locations in the Netherlands: on the clayey soils in Lelystad and the sandy soils of Vredepeel during 2019 and 2020. The original aim of this dataset was to calibrate and evaluate crop growth models for estimating potential, water-limited and nitrogen-limited yield levels for modern potato cultivars. Therefore, treatments include different cultivars, irrigation regimes, nitrogen fertilization and light interception. During the two seasons extensive data was collected either passively, via non-destructive or via destructive measurements. Passive measurements consisted of weather and soil moisture data and were taken continuously throughout the season. Non-destructive data was collected on photosynthesis, reflection / light intensity, SPAD chlorophyll values, plant height, crop phenology and groundwater level roughly every other week throughout the seasons. Destructive measurements were taken on biomass (leaves, stems and tubers), leaf area, NPK content (leaves) and the number of tubers between five and seven times per season. Additionally, during the final harvest data was collected on the tuber size distribution, marketability and NPK content of the tubers.
"


	uri <- "doi:10.7910/DVN/1LC6W7"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "WUR",
		publication = "doi:10.18174/odjar.v10i0.18680",
		project = NA,
		carob_date = "2025-12-17",
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;variety",
		response_vars = "yield;dmy_roots;fwy_roots;dmy_total;fwy_total", 
		carob_contributor = "Cedric Ngakou",
		completion = 70,	
		notes = "We process only files with useful information for carob"
	)
	

	f1 <- ff[basename(ff) == "Groundwaterlevel.xlsx"]
	f2 <- ff[basename(ff) == "Irrigation.xlsx"]
	f3 <- ff[basename(ff) == "NPK.xlsx"]
	f4 <- ff[basename(ff) == "Penetrologger.xlsx"]
	f5 <- ff[basename(ff) == "Photosynthesis.xlsx"]
	f6 <- ff[basename(ff) == "Plotspecific_processed_meta.xlsx"]
	f7 <- ff[basename(ff) == "Plotspecific_raw.xlsx"]
	f8 <- ff[basename(ff) == "Reflection.xlsx"]
	f9 <- ff[basename(ff) == "Waterpotential.xlsx"]
	f10 <- ff[basename(ff) == "Weatherfile_lelystad.xlsx"]
	f11 <- ff[basename(ff) == "Weatherfile_vredepeel.xlsx"]
	f12 <- ff[basename(ff) == "Plotspecific_processed.csv"]
	f13 <- ff[basename(ff) == "Plotspecific.R"]

	#r1a <- carobiner::read.excel(f1, sheet="Meta")
	#r1b <- carobiner::read.excel(f1, sheet="Data")
	r2a <- carobiner::read.excel(f2, sheet="Meta")
	r2b <- carobiner::read.excel(f2, sheet="Data")
	r3a <- carobiner::read.excel(f3, sheet="Meta")
	r3b <- carobiner::read.excel(f3, sheet="Data")
	#r4a <- carobiner::read.excel(f4, sheet="Meta")
	#r4b <- carobiner::read.excel(f4, sheet="Data")
	#r5a <- carobiner::read.excel(f5, sheet="Meta")
	#r5b <- carobiner::read.excel(f5, sheet="Data")
	r6 <- carobiner::read.excel(f6)
	r7a <- carobiner::read.excel(f7, sheet="meta")
	r7b <- carobiner::read.excel(f7, sheet="2019") ## include in r12
	r7c <- carobiner::read.excel(f7, sheet="2020") ## include in r12
	#r8a <- carobiner::read.excel(f8, sheet="Data1")
	#r8b <- carobiner::read.excel(f8, sheet="Data2")
	#r8c <- carobiner::read.excel(f8, sheet="meta")
	#r9a <- carobiner::read.excel(f9, sheet="Meta")
	#r9b <- carobiner::read.excel(f9, sheet="Vredepeel 2019")
	r9c <- carobiner::read.excel(f9, sheet="Lelystad 2019")
	#r9d <- carobiner::read.excel(f9, sheet="Vredepeel 2020")
	#r9e <- carobiner::read.excel(f9, sheet="Lelystad 2020")
	r10 <- carobiner::read.excel(f10)
	r11 <- carobiner::read.excel(f11)
	r12 <- read.csv(f12)
	


	
### process files

	  ### crop content 
	d1 <- data.frame(
	   planting_date = ifelse(grepl("L", r3b$Location) & grepl("2019", r3b$Year), "2019-04-25",
	                          ifelse(grepl("L", r3b$Location) & grepl("2020", r3b$Year), "2020-04-20",
	                          ifelse(grepl("V", r3b$Location) & grepl("2019", r3b$Year), "2019-04-28",
	                          ifelse(grepl("V", r3b$Location) & grepl("2020", r3b$Year), "2020-04-21", r3b$Year)))) ,
		date = as.character(r3b$Date),
		location = ifelse(grepl("V", r3b$Location), "Vredepeel", 
		                  ifelse(grepl("L", r3b$Location), "Lelystad" , r3b$Location)),
		variety = r3b$Cultivar,
		trial_id = r3b$Tshort,
		N_fertilizer = ifelse(grepl("N1", r3b$Nitrogen), 75, 
		                ifelse(grepl("N2", r3b$Nitrogen) & grepl("Fontane", r3b$Cultivar), 320,
		                ifelse(grepl("N2", r3b$Nitrogen) & grepl("Premiere", r3b$Cultivar), 320, 
		                ifelse(grepl("N2", r3b$Nitrogen) & !grepl("Premiere", r3b$Cultivar), 265, 0)))) ,
		irrigated = r3b$Irrigation,
		treatment = r3b$Treatment,
		leaf_N = r3b$leaves_ppm_N/1000,# mg/g
		leaf_P = r3b$leaves_ppm_P/1000,
		leaf_K = r3b$leaves_ppm_K/1000,
		tuber_N = r3b$tubers_ppm_N/1000,
		tuber_P = r3b$tubers_ppm_P/1000,
		tuber_K = r3b$tubers_ppm_K/1000
	)

## process yield data

	#### 
	d2 <- data.frame(
	   
	   planting_date = ifelse(grepl("L", r12$Location) & grepl("2019", r12$Year), "2019-04-25",
	                   ifelse(grepl("L", r12$Location) & grepl("2020", r12$Year), "2020-04-20",
	                   ifelse(grepl("V", r12$Location) & grepl("2019", r12$Year), "2019-04-28",
	                   ifelse(grepl("V", r12$Location) & grepl("2020", r12$Year), "2020-04-21", r12$Year)))) ,
	   date = as.character(r12$Date),
	   location = ifelse(grepl("V", r12$Location), "Vredepeel", 
	              ifelse(grepl("L", r12$Location), "Lelystad" , r12$Location)) ,
	   plot_id = as.character(r12$Plotnumber),
	   variety = ifelse(grepl("C1", r12$Cultivar),"Innovator" , 
	             ifelse(grepl("C3", r12$Cultivar), "Markies", 
	             ifelse(grepl("C4", r12$Cultivar),"Premiere" ,
	             ifelse(grepl("C6", r12$Cultivar),"Festien" ,
	             ifelse(grepl("C2|C5", r12$Cultivar), "Fontane", r12$Cultivar))))) ,
	   N_fertilizer = r12$Nitrogen,
	   irrigated = r12$Irrigation,
	   plant_height = rowMeans(r12[, c("Height1", "Height2", "Height3")])*100,# cm
	   fwy_stems = r12$StemFW,
	   dmy_stems = r12$StemDW,
	   fwy_leaves = r12$LeavesFW,
	   dmy_leaves = r12$LeavesDW,
	   LAI = r12$LAI,
	   fwy_total = r12$tubersFW,
	   dmy_total = r12$tubersDW,
	   tuber_density = r12$nrtubers_m2,
	   yield_moisture = r12$tuberDMpercentage,
	   fwy_roots = r12$rootsFW,
	   dmy_roots = r12$rootsDW,
	   yield = r12$Yield_gross_t.ha*1000
	)

	d2$N_fertilizer = ifelse(grepl("N1", d2$N_fertilizer), 75, 
	                  ifelse(grepl("N2", d2$N_fertilizer) & grepl("Fontane", d2$variety), 320,
	                  ifelse(grepl("N2", d2$N_fertilizer) & grepl("Premiere", d2$variety), 320, 
	                  ifelse(grepl("N2", d2$N_fertilizer) & !grepl("Premiere", d2$variety), 265, 0)))) 
	
	d <- merge(d1, d2, by= intersect(names(d1), names(d2)), all = TRUE)
	d <- d[!is.na(d$location),]
	
	### Adding fertilizer from publication
	d$P_fertilizer <- ifelse(grepl("Lelystad", d$location), 170, 45 )/2.29 
	d$K_fertilizer <- 275/1.2051
	d$Mg_fertilizer <- ifelse(grepl("Lelystad", d$location), 60, NA ) 
	
	d$irrigated <- TRUE
	d$longitude <- ifelse(grepl("Vredepeel", d$location),  5.8584, 5.55)
	d$latitude <- ifelse(grepl("Vredepeel", d$location),  51.5431, 52.54)
	d$country <- "Netherlands"
	d$crop <- "potato" 
	d$on_farm <- TRUE 
	d$is_survey <- FALSE
	d$yield_part <-  "roots"
	d$geo_from_source <- TRUE
	
	#### process weather information
	
	names(r10) <- r10[9,]
	dw1 <- suppressWarnings(data.frame(
	   date =  r10$DAY,
	   irad =  as.numeric(r10$IRRAD),
	   tmin =  as.numeric(r10$TMIN),
	   tmax =  as.numeric(r10$TMAX),
	   wspd =  as.numeric(r10$WIND),
	   prec =  as.numeric(r10$RAIN),
	   #snow = r10$SNOWDEPTH,
	   rhum =  as.numeric(r10$RH),
	   #VAP = r10$VAP,
	   ETo =  as.numeric(r10$Etref),
	   location = "Lelystad",
	   longitude =5.55 ,
	   latitude = 52.54,
	   geo_from_source = TRUE,
	   country = "Netherlands"
	))
	
	names(r11) <- r11[9,]
	dw2 <- suppressWarnings(data.frame(
	   date = r11$DAY,
	   irad = as.numeric(r11$IRRAD),
	   tmin = as.numeric(r11$TMIN),
	   tmax = as.numeric(r11$TMAX),
	   wspd = as.numeric(r11$WIND),
	   prec = as.numeric(r11$RAIN),
	   #snow = r11$SNOWDEPTH,
	   rhum = as.numeric(r11$RH),
	   #VAP = r10$VAP,
	   ETo = as.numeric(r11$Etref),
	   location = "Vredepeel",
	   longitude = 5.8584 ,
	   latitude = 51.5431,
	   geo_from_source = TRUE,
	   country = "Netherlands"
	))
	
	dw <- carobiner::bindr(dw1, dw2)
	dw <- dw[-c(1:10),]
	dw$irad <- dw$irad*1000/(3600*24)
	dw$date <- suppressWarnings(as.character(as.Date(as.numeric(dw$date), "1899-01-31")))
	
   
	carobiner::write_files(path, meta, d, wth = dw)
}


