# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. although dataset was used in modelling, it is observed data
#2. windspeed value out of bounds (87, 1115)


carob_script <- function(path) {

"
Data from the winter wheat potential yield experiment in New Zealand and response to variable sowing dates and densities: field experiments and AgMIP-Wheat multi-model simulations

The dataset contains 6 growing seasons of a local winter wheat cultivar 'Wakanui' at two farms located in the Canterbury Region of New Zealand. The data of the experiment was used in the AgMIP-Wheat Phase 4 project to evaluate the performance of an ensemble of 29 crop models to predict the effect of changing sowing dates and rates on yield and yield components, in a high-yielding environment. The treatments were managed for non-stress conditions. Data include local daily weather, soil characteristics and initial soil N conditions, crop measurements (anthesis and maturity dates, total above-ground biomass, final grain yield, and yield components), and cultivar information. Simulations include both daily in-season and end-of-season results from 29 wheat crop models.
"

	uri <- "doi:10.7910/DVN/XA4VA2"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "INRAE; New Zealand Institute for Plant & Food Research; Technical University of Munich; UIA; Foundation for Arable Research; UFL; CIMMYT; Pir Mehr Ali Shah Arid Agriculture University; IASU; MSU; National Institute of Agricultural Research (INIA); University of Göttingen; PURDUE; CAAS; UOR; ZALF; UFL; UHoh; CSIR; ZALF; AARU; Luke; Helmholtz Zentrum München-German Research Center for Environmental Health; Technic University of Madrid; ROTH; MSU; WUR; Zhejiang University; HEBAU",
		publication = NA,
		project = NA,
		design = "split plot",
		data_type = "experiment",
		treatment_vars = "planting_date;plant_density",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-10-22",
		carob_completion = 90,	
		carob_effort = 7
	)
	

	f <- ff[basename(ff) == "NZ_AgMIP_dataset.xlsx"]
  r1 <- carobiner::read.excel.hdr(f, sheet="Obs_crop_summary_plots", skip = 2, hdr = 2, fix_names = FALSE, lower = FALSE)
  r2 <- carobiner::read.excel.hdr(f, sheet="Treatments", skip = 2, hdr = 2, fix_names = FALSE, lower = FALSE)
  r3 <- carobiner::read.excel.hdr(f, sheet="Planting_events", skip = 2, hdr = 2, fix_names = FALSE, lower = FALSE)
  r4 <- carobiner::read.excel.hdr(f, sheet="Harvest_events", skip = 2, hdr = 2, fix_names = FALSE, lower = FALSE)
  r5 <- carobiner::read.excel.hdr(f, sheet="Irrigation_events", skip = 2, hdr = 2, fix_names = FALSE, lower = FALSE)
  r6 <- carobiner::read.excel.hdr(f, sheet="Weather_daily", skip = 2, hdr = 2, fix_names = FALSE, lower = FALSE)
  r7 <- carobiner::read.excel.hdr(f, sheet="Residue", skip = 2, hdr = 2, fix_names = FALSE, lower = FALSE)
  r8 <- carobiner::read.excel.hdr(f, sheet="Fields", skip = 2, hdr = 2, fix_names = FALSE, lower = FALSE)
  r9 <- carobiner::read.excel.hdr(f, sheet="Fertilizer_events", skip = 2, hdr = 2, fix_names = FALSE, lower = FALSE)
  
  
  plots <- data.frame(
    plot_id=r1$plot_id_text_PLTID,
    exp_id=r1$experiment_ID_text_EID,
    trt_id=r1$treatment_ID_text_TREAT_ID,
    variety=r1$cultivar_identifier_text_CUL_NAME,
    rep=as.integer(r1$replicate_number_number_RP),
    yield=r1$`grain_dry_wt_at_maturity_kg[DM]/ha_GWAM`,
    yield_moisture=r1$`grain_moisture_maturity_% of grain FM_GWPCM`,
    harvest_index=r1$`harvest_index_maturity_-_HIAM`,
    crop="wheat"
  )
  
  trt <- data.frame(
    exp_id=r2$experiment_ID_number_EID,
    trt_id=r2$treatment_ID_text_TREAT_ID,
    treatment=r2$treatment_name_text_TRT_NAME,
    irrigated=r2$irrig_applied_code_IRRIG,
    fertilizer_used=r2$fert_applied_code_FERTILIZER
  )
  
  d <- merge(plots,trt,by=c("exp_id","trt_id"), all.x = TRUE)
  
  sowing <- data.frame(
    trt_id=r3$treatment_ID_text_TREAT_ID,
    row_spacing=r3$row_spacing_cm_PLRS,
    planting_depth=r3$planting_depth_mm_PLDP,
    planting_date=as.character(r3$planting_date_date_PDATE),
    plant_density=r3$`plant_pop_at_planting_number/m2_PLPOP`*10000
  )
  
  d <- merge(d,sowing,by="trt_id",all.x = TRUE)
  
  harvesting <- data.frame(
    trt_id=r4$treatment_ID_text_TREAT_ID,
    harvest_date=as.character(r4$harvest_operations_date_date_HADAT)
  )
  
  d <- merge(d,harvesting,by="trt_id",all.x = TRUE)
  
  irrig <- data.frame(
    exp_id=r5$experiment_ID_text_EID,
    irrigation_dates=as.character(r5$irrigation_date_date_IDATE),
    irrigation_amount=r5$irrig_amount_depth_mm_IRVAL
  )
  
  d <- merge(d,irrig,by="exp_id",all.x = TRUE)
  
  prev_crop <- data.frame(
    exp_id=r7$experiment_ID_text_EID,
    previous_crop=tolower(r7$Previous_crop_notes_text_PC_NOTES)
  )
  
  d <- merge(d,prev_crop,by="exp_id",all.x = TRUE)
  
  weather <- data.frame(
    date=as.character(r6$weather_date_date_W_DATE),
    srad=r6$`solar_radiation_MJ/m2/d_SRAD`,
    tmax=r6$`maximum_temperature_°C_TMAX`,
    tmin=r6$`minimum_temperature_°C_TMIN`,
    prec=r6$`rain_snow_fall_mm/d_RAIN`,
    wspd=r6$`wind_speed_daily_km/d_WIND`,
    vapr=r6$vapor_pressure_kPa_VPRSD
  )
  
  loc <- data.frame(
    exp_id=r8$experiment_ID_text_EID,
    latitude=r8$field_latitude_degre_FL_LAT,
    longitude=r8$field_longitude_degre_FL_LONG,
    elevation=r8$field_elevation_m_FLELE,
    country=r8$field_country_text_FL_LOC_1,
    adm1=r8$field_sub_country_text_FL_LOC_2,
    location=r8$field_sub_sub_country_text_FL_LOC_3
  )
  
  d <- merge(d,loc,by="exp_id",all.x = TRUE)
  
  fert <- data.frame(
    exp_id=r9$experiment_ID_text_EID,
    N_fertilizer=r9$N_in_applied_fertilizer_code_FEAMN,
    fertilization_method=r9$fertilizer_applic_method_code_FEACD,
    fertilizer_type=r9$fertilizer_material_code_FECD
  )
  
  d <- merge(d,fert,by="exp_id", all.x=TRUE)
  
  d$fertilizer_used <- d$fertilizer_used=="Y"
  d$irrigated <- d$irrigated=="Y"
  d$fertilization_method <- ifelse(d$fertilization_method=="AP006","foliar spray","broadcasted")
  d$fertilizer_type <- ifelse(d$fertilizer_type=="FE005","urea","Ammonium nitrate sulfate")
  d$previous_crop[d$previous_crop=="cornsalad"] <- "corn salad"
  
	d$trial_id <- paste(d$treatment,seq_len(nrow(d)),sep = "_")
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$geo_from_source <- TRUE
  d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d$yield_part <- "grain"
	d$yield_isfresh <- TRUE
	
	d[,c("exp_id","trt_id","plot_id")] <- NULL#removing joint columns

	carobiner::write_files(path, meta, d,wth = weather)
}

