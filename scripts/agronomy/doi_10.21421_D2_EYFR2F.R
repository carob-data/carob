# R script for "carob"

carob_script <- function(path) {
  
"Sorghum productivity and water use under phosphorus fertilization in the sudan savanna of Nigeria  
Abstract: Assess the effects of P-fertilization on sorghum growth and productivity, crop evapotranspiration (ETc), water use efficiency (WUE) and agronomic phosphorus use efficiency (APUE) and also establish relationships among crop yield, WUE and ET and determine optimum P-fertilizer rates in the Sudan savanna zone of Nigeria."
  
 
	uri <- "doi:10.21421/D2/EYFR2F"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)
  
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		publication="https://oar.icrisat.org/10842/",
		carob_contributor="Siyabusa Mkuhlani",
		carob_date="2022-09-12",
		data_type="experiment",
		data_organization="ICRISAT",
		project=NA,
		response_vars = "yield",
		treatment_vars = "P_fertilizer"
	)
  
  
	f <- ff[basename(ff) == "Data file of Sorghum Phosphorus trial Kano Nigeria.xlsx"]
	r <- carobiner::read.excel(f)
  

	d <- data.frame(
		country = "Nigeria",
		adm1 = "Kano",
		location  = carobiner::replace_values(r$Location, "BUK", "Bayero University, Kano"),
		latitude = ifelse(r$Location == "Minjibir", 12.17, 11.975),
		longitude = ifelse(r$Location == "Minjibir", 8.65, 8.423),
		geo_from_source = FALSE,
		crop = "sorghum",
		yield_part = "grain",
		variety = r$Sorghum,
		rep = as.integer(r$`Replication number`),
		seed_weight = r$GW_1000grnM_g,
		fwy_residue = r$`Stalk yield`,
		yield = r$GHvYld_C_kgha,
		P_fertilizer = r$Phosphorus,
		trial_id = paste(r$Year, r$Location, sep = "_")
	)
	
  # additional info from the reference then merge
  
	d$on_farm <- TRUE
	d$yield_part <- "grain"
	d$is_survey <- FALSE
	d$N_fertilizer <- 60 #extracted from the reference
	d$K_fertilizer <- 30 #extracted from the reference
			#KCl was in form of muriate of potash
	  
	  # filling in the fertilizer types as in the reference
	d$fertilizer_type <- "urea;SSP;KCl"
    d$fertilizer_type[d$P_fertilizer == 0] <- "urea;KCl"

	ss <- data.frame(
		trial_id = c("2014_Minjibir","2015_Minjibir","2014_BUK","2015_BUK"),
                 soil_pH = c(5.01,5.35 ,4.86,5.7),
                soil_SOC = c(0.196,0.359,0.359,0.299),
				soil_P = c(9.013,3.352,4.456,9.219),
               soil_sand = c(92.3,82.64, 79.85,78.64),
               soil_clay = c(3.36,16.08,9.91,10.08),
               soil_silt = c(4.35,1.28,10.2,11.28),
              planting_date = c("2014-07-07","2015-07-04","2014-07-19","2015-07-20"),
              harvest_date   = c("2014","2015","2014","2015"))
  
	d <- merge(d, ss, by = "trial_id", all.x = TRUE)
	d$irrigated <- NA

	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path)
}


