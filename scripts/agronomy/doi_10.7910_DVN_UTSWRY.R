# R script for "carob"

# ## ISSUES 
#Fertiliser amount are given as ratios 
# ....


carob_script <- function(path) {
  
"This study contains data originated from on-farm trials that were conducted to test and demonstrate the crop yield and economic benefits derived from manual and animal traction conservation agriculture (CA) systems on smallholder farms where the ridge and furrow tillage system is the traditional practice. The farm trials were conducted at six farms in Chipata, Lundazi, and Sinda districts of the eastern province of Zambia. In each site, the trials were replicated four times and had two general treatment sets:1) manual CA; and 2) animal traction CA.

The manual CA system trial consisted of three treatments and these treatments were compared with conventional ridge and furrow practice at each farmer's field. The four treatments including control were:

Conventional ridge and furrow with continuous sole maize (CRF);
No-tillage / Dibblestick CA system with continuous sole maize;
Dibblestick CA system with maize intercropped with cowpea;
Dibblestick CA with maize rotated with legumes
The animal traction CA system consisted of two treatments that were compared with a conventional ridge and furrow practice at each farmer's field. The three treatments including control were:

Conventional ridge and furrow with continuous sole maize (CRF);
Animal traction (AT) ripline seeding with continuous sole maize;
Animal traction (AT) ripline seeding with maize rotated with legumes"
  
  uri <- "doi:10.7910/DVN/UTSWRY"
  group <- "agronomy"
  ff <- carobiner::get_data(uri, path, group)
 
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
    project=NA,
    publication= "doi:10.1017/S1742170517000606",
    data_organization = "CIMMYT;ZARI;TLC",
    data_type="experiment",
    carob_contributor="Fredy Chimire",
    carob_date="2024-01-16",
	response_vars = "yield",
	treatment_vars = "land_prep_method"
   )
  
   
  f <- ff[basename(ff) == "AR_ZAM_CIMMYT_CAmother_onfarm_2020.csv"]
  
  # Select sheet with revised data from the excel file 
  r <- read.csv(f)
  
  d <- data.frame(
		country= r$Country,
		harvest_date=r$Year,
		rep= r$Rep,
		crop= r$Cropgrown,
		treatment= r$Description,
		adm2=r$District,
		location=r$Camp,
		dmy_total = r$Biomassyield, 
		yield = r$Grainyield,
		trial_id = as.character(r$Farmerno),
		planting_date = "2020"
	)
  
	# see "dictionary_AR_ZAM_CIMMYT_CAmother_onfarm_2020.csv"
	# compound D (10:20:10) and Urea (46N)
	fert <- strsplit(r$Fertilizerrate, ":")
	fert <- data.frame(do.call(rbind, fert))
	fert <- sapply(fert, as.numeric)
	d$N_fertilizer <- fert[,1] * .10 + fert[,2] * .46 
	d$P_fertilizer <- fert[,1] * .20
	d$K_fertilizer <- fert[,1] * .10	
  
	d$fertilizer_type <- "none"
	d$fertilizer_type[fert[,1] > 0] <- "D-compound"
	d$fertilizer_type[fert[,2] > 0] <- paste0(d$fertilizer_type[fert[,2] > 0], ";urea")
  
  
  d$is_survey <- FALSE
  d$on_farm <- TRUE
  
  
  d$yield_part <- "grain"
  
    d$crop <- tolower(d$crop)
	d$harvest_date <- as.character(d$harvest_date)


  # https://www.mindat.org/feature-905632.html
	geo <- data.frame(
		adm2 = c("Chipata", "Lundazi", "Sinda"), 
		longitude = c(32.65, 32.75, 32.012),
		latitude = c(-14.017, -12.5, -14.187),
		geo_from_source = FALSE
	)
 
	d <- merge(d, geo, by="adm2", all.x=TRUE)
  
  	d$irrigated <- FALSE
	d <- d[!is.na(d$yield), ]

	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path)
}

