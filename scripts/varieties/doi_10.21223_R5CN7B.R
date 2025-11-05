# R script for "carob"

carob_script <- function(path) {
   
"The purpose of this study was to estimate the genetic gains for yield and quality traits in sweetpotatoes. The field evaluation was conducted in San Ramon 2016, 2018A, 2018B; Huaral 2016, 2019A, 2019B; Ica 2016, 2019A, 2019B and Satipo 2016, 2018A, 2018B evaluating 17 clones (Abigail, Adriano, Alexander, Arne, Atacama, Benjamin, Caplina, Costanero, Huambachero, INA-100, Isabel, Jonathan, Milagrosa, PZ06.120, Sumi, Tacna, Yarada) and 3 checks (Cemsa, Dagga, Salyboro).  Each of the trials was harvested at 90 and then 120 days."
   
   uri <-  "doi:10.21223/R5CN7B"
   group <- "varieties"
   ff <- carobiner::get_data(uri, path, group)
  
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		publication= NA,
		data_organization = "CIP",
		carob_contributor="Cedric Ngakou",
		carob_date="2023-11-02",
		data_type="experiment",
		response_vars = "yield",
		treatment_vars = "variety",
		project=NA 
   )
   meta$license <- "CC-BY-4.0"
   
   # read file
   r <- carobiner::read.excel(ff[basename(ff)=="Genetic_Gain_Trials_Peru_Cost_Amazon_90_120_days_full_data_.xlsx"])  
   
	d <- data.frame(
		trial_id = r$trial_name,
		planting_date = r$planting_date,
		#season = r$season ,
		location = r$loc,
		variety_code = r$cipno,
		variety = r$geno,
		rep = r$rep,
		yield = r$rytha * 1000,
		leaf_Fe = r$fe,
		leaf_Zn = r$zn,
		leaf_Ca = r$ca,
		leaf_Mg = r$mg,
		fwt_total = r$bytha * 1000,
		fwt_leaves= r$fytha * 1000
	)
   
##CN
### The unit of fwy_residue and dmy_leaves is unclear: it is given in tons/ha, but after conversion to kg/ha, almost all values are  out of bounds.
   #d$fwt_residue <- d$fwt_residue*1000 # in kg/ha
   #d$dmy_leaves<- d$dmy_leaves*1000
   d <- d[!is.na(d$yield),] ## remove NA in yield
   ## add columns
   d$crop <- "sweetpotato" 
   
   d$planting_date <- gsub("'", "", d$planting_date)
   d$country <- "Peru"
   d$yield_part <- "tubers"
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$irrigated <- FALSE

   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   geo <- data.frame(location=c("San Ramon", "Huaral", "Ica", "Satipo"),
                  longitude=c(-75.2237, -76.916667, -75.35835, -74.1181641),
                  latitude=c(-12.0093, -11.25, -11.12787, -11.5538237),
				  geo_from_source = FALSE
			)
   d <- merge(d, geo, by="location", all.x = TRUE)

   d$rep <- as.integer(d$rep)
   d$harvest_date <- as.character(NA)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(meta, d, path=path)
   
}


