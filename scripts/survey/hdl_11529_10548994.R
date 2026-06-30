# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Very high values in fertilizer_amount,yield and 4 other crops


carob_script <- function(path) {

"
Analysis of Household-Level Survey Data: Farm Characteristics and Resource Allocation in Nepal (2023)

Dataset processed from a household-level survey to describe the main farm characteristics, production, and resource allocation in two municipalities across three regions of the country: Surkhet (Gurbhakot) and Khotang (Tuwachung) districts. Data was collected between March and June 2023.
"

	uri <- "hdl:11529/10548994"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT; IWMI; Kathmandu University",
		publication = "https://hdl.handle.net/10568/139116",#cannot cite a report
		project = NA,
		design = "unitOfAnalysis: Household level",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-06-30",
		carob_completion = 90,	
		carob_effort = 8
	)
	

	f <- ff[basename(ff) == "BD Khotang-Nepal Houshold processed survey 2023 CIMMYT.xlsx"]
	f2 <- ff[basename(ff) == "BD Nepal Houshold processed survey 2023 CIMMYT.xlsx"]
	f3 <- ff[basename(ff) == "BD Surkhet-Nepal Houshold processed survey 2023 CIMMYT.xlsx"]

	r1 <- carobiner::read.excel(f, sheet="DB")
	r2 <- carobiner::read.excel(f2, sheet="DB")
	r3 <- carobiner::read.excel(f3, sheet="DB")

	r <- rbind(r1,r2,r3)
	#r <- unique(r) no duplicates at t5his point
	
	d <- data.frame(
	  hhid=r$hh_id2,
	  country="Nepal",
	  latitude=r$latitud,
	  longitude=r$longitud,
	  adm2=r$district,
	  adm3=r$municipality,
	  adm4=as.character(r$ward),
	  location=r$tole_ham,
	  age=r$farm_age,
	  education=r$farm_edu,
	  sex=r$HH_gender,
	  hh_size=r$HH_hab,
	  market_distance=r$market_dist,
	  farmland=r$surface,
	  #season=r$seasons,season values from dataset are more than what the questionaire defines
	  cropland_owned=r$ownland_gen,
	  cropland_rentedin=r$rentedinland_gen,
	  cropland_rentedout=r$rentedoutland_gen,
	  OM_amount=r$manure_inp,
	  seed_rate=r$seed_inp,
	  fertilizer_amount=r$chemfert_inp,
	  TLU=r$tlu_all,
	  hh_income=r$income,
	  banana=r$yield_banana,
	  irri_banana=r$irri_banana,
	  lentil=r$yield_lentil,
	  irri_lentil=r$irri_lentil,
	  maize=r$yield_maize,
	  irri_maize=r$irri_maize,
	  mango=r$yield_mango,
	  irri_mango=r$irri_mango,
	  ricebean=r$yield_masayng,
	  irri_ricebean=r$irri_masayng,
	  millet=r$yield_millet,
	  irri_millet=r$irri_millet,
	  potato=r$yield_potato,
	  irri_potato=r$irri_potato,
	  rice=r$yield_rice_as,
	  irri_rice=r$irri_rice_as,
	  wheat=r$yield_wheat,
	  irri_wheat=r$irri_wheat,
	  yield_moisture=as.numeric(NA),
	  planting_date=as.character(NA)
	  )

	#reshaping data to long to capture various crops and their yields
	
	crop_cols <- c("banana", "lentil", "maize", "mango", "ricebean",
	               "millet", "potato", "rice", "wheat")
	
	irri_cols <- c("irri_banana", "irri_lentil", "irri_maize", "irri_mango", "irri_ricebean",
	               "irri_millet", "irri_potato", "irri_rice", "irri_wheat")
	

	d$id <- 1:nrow(d)
	
	d <- reshape(
	  d,
	  direction = "long",
	  varying = list(yield = crop_cols, irri_pct = irri_cols),
	  v.names = c("yield", "irri_pct"),
	  timevar = "crop",
	  times = crop_cols,
	  idvar = "id"
	)
	
	rownames(d) <- NULL
	
	
	d$yield <- d$yield*1000 #converting t/ha to kg/ha
	d$OM_used <- r$manure_sys == "1"
	d$OM_type <- ifelse(d$OM_used, "animal dung", NA)
	d$irrigated <- long_d$irri_pct > 0
	d$geo_from_source <- TRUE
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d$trial_id <- paste(d$hhid,d$location,sep = "_")
	d$yield_isfresh <- NA
	
	
	
	#cleaning education values
	edu <- c(
	  "0" = "no formal education",
	  "1" = "can read and write",
	  "2" = "primary school",
	  "3" = "secondary school",
	  "4" = "college"
	)

d$education <- edu[as.character(d$education)]

#cleaning gender
d$sex <- ifelse(d$sex=="1","male","female")

d[c("irri_pct", "id")] <- NULL

#standardizing yield part
yield_part <- c(
  banana   = "fruit",
  lentil   = "seed",
  maize    = "grain",
  mango    = "fruit",
  ricebean = "seed",
  millet   = "grain",
  potato   = "tubers",
  rice     = "grain",
  wheat    = "grain"
)

d$yield_part <- yield_part[d$crop]
	
d$currency <- "NPR"	

d <- unique(d)#because some differentiating variables were ommitted during standardization, and
             #reshaping, more entries behaved like duplicates

carobiner::write_files(path, meta, d)
}
