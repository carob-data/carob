# R script for "carob"


carob_script <- function(path) {

"The intensification of agricultural systems in sub-Saharan Africa (SSA) is necessary to reduce poverty and improve food security but requires increased nutrient applications to smallholder systems. To avoid the negative consequences of intensification that result from many large-scale agricultural systems (e.g., eutrophication), we must better understand how diverse soil systems in SSA will respond to increased fertilizer applications. We tracked nitrogen (N) inputs and outputs in fertilizer trials at two maize (Zea mays)-agroecological regions of contrasting soil type. We measured maize biomass, grain yields, N leaching, and N gaseous losses from a clayey soil in Yala, Kenya, and a sandy soil in Tumbi, Tanzania, with application rates of 0, 50, 75, 100, 150, and 200 kg N ha-1 yr‑1 over multiple years. Using measurements of NO, N2O, NO3-, biomass N, and an 15N enrichment experiment, we show that N budgets in Yala were nearly always negative, meaning more N was exported in yields or lost from the system than was added in fertilizer. In Tumbi, however, N budgets were negative at lower fertilizer levels (0 and 50 kg N ha-1), but positive at higher fertilizer levels (75 and 200 kg N ha-1). At both sites, most of the N was lost through maize biomass/grain removal and N leaching (over 96% of total losses). Gas losses were a minor component of N budgets. These results highlight the importance of tailoring fertilizer recommendations to a farm’s specific soil and climatic conditions. However, on these two contrasting sites, fertilizer additions at or below 50 kg N ha-1 do not lead to major losses of N (via gaseous or leaching) and may be recommended at a range of sites across SSA soils in maize agroecosystems."

	uri <- "doi:10.5061/dryad.rxwdbrvcj"
	group <- "agronomy"

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=6, minor=NA,
		project="Millennium Villages Project",
		publication= "doi:10.1029/2022JG007128",
		data_organization = "UMD;GISS;PSU;WHRC;TARI;ILO;IITA;UFL",
		data_type="experiment",
		response_vars= "yield",
		treatment_vars="N_fertilizer;P_fertilizer",
		carob_contributor="Eduardo Garcia Bendito",
		carob_date="2025-01-31"
	)

	f1 <- ff[basename(ff) == "KETZ_harvest.xlsx"]
	f2 <- ff[basename(ff) == "KETZ_weather.xlsx"]
	rk <- carobiner::read.excel(f1, sheet = "KE_harvest")
	rt <- carobiner::read.excel(f1, sheet = "TZ_harvest")

	dk <- data.frame(
		country = "Kenya",
		adm1 = "Siaya county",
		adm2 = "Yala division",
		location = "Nyamninia",
		site = "Kenya Broadcasting Corporation (KBC)",
		longitude = 34.5167,
		latitude = 0.1072,
		geo_from_source = FALSE,
		trial_id = "1",
		on_farm = FALSE,
		is_survey = FALSE,
		crop = "maize",
		variety_type = "hybrid",
		variety_code = "WH403",
		previous_crop = "maize",
		plot_area = 10.82,
		season = "Long",
		planting_date = ifelse(rk$trt == 0 & rk$Year > 2011, paste0(rk$Year-1,"-10-15"), paste0(rk$Year,"-03-15")), # From publication: "In Yala, fertilized maize was cultivated during the long rains and unfertilized maize during the short rains"
		harvest_date = ifelse(rk$trt == 0 & rk$Year > 2011, NA, paste0(rk$Year,"-08-25")),
		fertilizer_used = TRUE,
		fertilizer_type = "DAP;urea",
		fertilizer_dap = "0;35",
		fertilizer_amount = ((rk$trt*0.66)/0.46) + ((rk$trt*0.33)/0.18),
		N_fertilizer = rk$trt,
		N_splits = 2L,
		P_fertilizer = ((rk$trt*0.33)/0.18)*0.201,
		OM_used = FALSE,
		N_organic = 0,
		inoculated = FALSE,
		irrigated = FALSE,
		row_spacing = 75,
		plant_spacing = 30,
		fw_yield = rk$ke_grain*1000,
		yield_part = "grain",
		fwy_residue = rk$ke_stover*1000,
		grain_N = 10 * rk$ke_grainN / rk$ke_grain, # maybe... seems too low
		residue_N = 10 * rk$ke_stoverN / rk$ke_stover, #maybe...
		soil_pH = 5.97,
		soil_sand = 52.2,
		soil_clay = 35.2,
		soil_silt = 12.4,
		soil_CEC = 15.40,
		soil_P = 0.06,
		soil_SOC = 1.90,
		soil_N = 1100,
		soil_sample_top = 15
	)
	
	glir <- rt$trt == "GLIR"
	rt$trt[glir] <- 0
	rt$trt <- as.numeric(rt$trt)
	
	dt <- data.frame(
	  country = "Tanzania",
	  location = "Tumbi",
	  site = "Tumbi Agricultural Research Institute",
	  longitude = 32.6887,
	  latitude = -5.071,
	  geo_from_source = FALSE,
	  trial_id = "2",
	  on_farm = FALSE,
	  is_survey = FALSE,
	  crop = "maize",
	  variety_type = "hybrid",
	  variety = "Dekalb",
	  variety_code = "8053",
	  previous_crop = "none",
	  plot_area = 10.82,
	  season = "Long",
	  planting_date = paste0(rt$Year, "-11-01"),
	  harvest_date = paste0(rt$Year+1, "-05-01"),
	  fertilizer_used = TRUE,
	  fertilizer_type = "DAP;urea",
	  fertilizer_dap = "0;35",
	  fertilizer_amount = rt$trt*0.66/0.46 + rt$trt*0.33/0.18,
	  N_fertilizer = rt$trt,
	  N_splits = 2L,
	  P_fertilizer = 0.201 * rt$trt*0.33/0.18,
	  OM_used = glir,
	  OM_type = ifelse(glir, "Gliricidia sepium leaves", "none"),
	  N_organic = ifelse(glir, 75, 0),
	  inoculated = FALSE,
	  irrigated = FALSE,
	  row_spacing = 75,
	  plant_spacing = 30,
	  fw_yield = rt$tz_grain*1000,
	  yield_part = "grain",
	  fwy_residue = rt$tz_stover*1000,
	  grain_N = 10 * rt$tz_grainN / rt$tz_grain, # maybe... seems too low
	  residue_N = 10 * rt$tz_stoverN / rt$tz_stover, # maybe...
	  soil_pH = 5.47,
	  soil_sand = 87.4,
	  soil_clay = 8.9,
	  soil_silt = 3.6,
	  soil_CEC = 1.78,
	  soil_P = 0.30,
	  soil_SOC = 2.05,
	  soil_N = 1800,
	  soil_sample_top = 15
	)
	
	d <- carobiner::bindr(dk, dt)
	d$K_fertilizer <- as.numeric(NA)
	
	### processing weather data 
	wh <- lapply(c("KE_weather","TZ_weather"), function(i){
	   
	   r <- carobiner::read.excel(f2,  sheet=i, na=c("NA"))
	   
	   data.frame(
	      country= r$site,
	      location= ifelse(grepl("Kenya", r$site), "yala", "Tumbi"),
	      latitude= ifelse(grepl("KE", i),0.1072, -5.071 ),
	      longitude=  ifelse(grepl("KE", i), 34.5167, 32.6887 ),
	      geo_from_source= FALSE,
	      date= as.character(r$date),
	      prec= as.numeric(r$precip.mm),
	      tmin= as.numeric(r$min.temp.C),
	      tmax= as.numeric(r$max.temp.C),
	      temp= as.numeric(r$avg.temp.C)
	   )
	   
	})
	
	wth <- do.call(rbind, wh)
	
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path, wth = wth)
}

