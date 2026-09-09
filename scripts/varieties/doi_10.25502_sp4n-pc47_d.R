# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {

"
Validation trial of improved yam varieties in Nigeria in 2017

The average yam yields of local varieties is less than 25% of the yield of improved released varieties, which range from 30 to 40 tons/ha. Absence of a formal seed system has not encouraged the promotion and adoption of released improved yam varieties. To sensitize farmers on the superiority of improved varieties, seed tubers of selected released varietiesproduced by IITA were planted in comparison with local checks in 80 on-farm validation/demonstration trials in six states of Nigeria - Enugu, Benue, Nasarawa, Federal Capital Territory (FCT), Niger and Oyo. Three improved yam varieties [two Dioscorea rotundata (TDr 89/02665 and TDr 95/19177) and one D. alata (TDa 98/01176)], and one location specific farmers’ best variety were used to quantify the superiority of the improved varieties over the locals.
"

	uri <- "doi:10.25502/sp4n-pc47/d"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = NA,
		treatment_vars = "variety",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-08-23",
		carob_completion = 90,	
		carob_effort = 5
	)

	f <- ff[basename(ff) == "seed-company-2017-data.csv"]

	r <- read.csv(f)

  d <- data.frame(
    country="Nigeria",
    adm1=r$State,
    location=r$Village,
    adm2=r$LGA,
    variety=r$Variety,
    plot_size=r$VarPlotSize,
    crop="yam",
    yield=r$Yield_t_ha*1000
  )
  
  d$trial_id <-  paste(d$location,d$variety,sep = "_")
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- FALSE
	d$planting_date <- NA
	d$harvest_date  <- NA 
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
	d$yield_part <- "tubers"
	d$yield_moisture <- NA
	d$yield_isfresh <- NA
	
	#fixing location
	d$adm1[d$adm1=="FCT"] <- "Federal Capital Territory"

	name_fix <- c(
	  "Gwagwalade" = "Gwagwalada",
	  "Nassarawa Eggon" = "Nasarawa Eggon",
	  "katcha" = "Katcha",
	  "Paiko" = "Paikoro",
	  "Oorelope" = "Orelope",
	  "Atigbo" = "Atisbo",
	  "K/Ala" = "Katsina-Ala",
	  "AMAC" = "Abuja Municipal Area Council"
	)
	
	d$adm2 <- ifelse(d$adm2 %in% names(name_fix),name_fix[d$adm2],d$adm2)
	
	#from google maps
	loc = data.frame(
	  location=c("Rafin Zurfi", "Angwar Dodo", "Shazhi", "Yaba", "Kpaduma", "Guto", "Dorowa", "Sabongida",
	             "Angida Gida", "Angwaan Kadaura", "Akunza Maralaba", "Obi", "Tundun Adabu", "Doma", "Kadarko",
	             "Sarkin Loma", "Bakinrijiya", "Adoyi", "Nassarawa Eggon", "Agunji", "Kpanga", "Egbanasara",
	             "Chachafu", "Maali", "Ndaabarshi", "Paiko Lugodan", "Popoi", "Badna", "Ganamadi", "Cheche", "Nami", 
	             "Boku", "Lambata/Gaiji", "Ugba", "Adum East", "Igboho", "Oloje", "Agunrege", "Araromi"),
	latitude = c(10.2599, 11.288, 10.7, 6.498, 10.509, 9.493, 8.78, 8.987, 9.089, 8.843, 8.473, 8.369, 8.425, 
	            8.404, 8.229, 8.254, 8.536, 8.524, 8.74, 8.673, 10.258, 9.257, 9.197, 9.199, 8.768, 9.422, 9.631,
	            8.928, 9.036, 9.083, 9.04, 9.083, 9.274, 7.509, 6.991, 8.82, 8.514, 8.393, 6.557),
	longitude = c(9.784, 6.568, 4.8, 3.368, 7.436, 6.535, 7.85, 8.003, 8.297, 8.132, 8.587, 8.761, 8.733, 8.347,
	              8.578, 8.565, 8.589, 8.65, 8.542, 8.313, 4.644, 6.198, 5.812, 5.815, 6.318, 6.633, 6.365, 7.604,
	              6.606, 6.562, 6.483, 6.472, 7.037, 9.358, 8.335, 3.758, 4.511, 3.392, 3.371))
	                                           
	d <- merge(d,loc,by="location", all.x=TRUE)
	
	#from gadm
	geo <- data.frame(
	adm2 = c("Gboko", "Katsina-Ala", "Oju", "Tarka", "Aninri", "Awgu", "Bwari", "Kuje", "Kwali", "Gurara", "Lapai", "Shiroro", "Ibarapa East", "Irepo", "Iseyin", "Orelope", "Saki West","Lafia"),
	longitude = c(8.864, 9.5706, 8.3591, 8.8503, 7.5898, 7.437, 7.4299, 7.2308, 6.9518, 7.0232, 6.6352, 6.6721, 3.4921, 3.9348, 3.5333, 3.8114, 3.1472, 8.6996),
	latitude = c(7.3212, 7.3217, 6.8304, 7.584, 6.0394, 6.1452, 9.2079, 8.6389, 8.7196, 9.322, 8.727, 10.1111, 7.608, 8.9998, 7.8647, 8.8038, 8.5698, 8.6401),
	geo_uncertainty = c(35739, 43688, 32151, 15486, 16862, 20355, 30998, 38288, 25406, 36704, 74470, 65837, 25571, 34600, 32578, 27262, 46638, 60748),
	geo_source = c("GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2", "GADM 4.1, adm2"))
	
	na_idx <- which(is.na(d$latitude) | is.na(d$longitude))
	match_idx <- match(d$adm2[na_idx], geo$adm2)
	
	d$latitude[na_idx] <- geo$latitude[match_idx]
	d$longitude[na_idx] <- geo$longitude[match_idx]
	d$geo_uncertainty[na_idx] <- geo$geo_uncertainty[match_idx]
	d$geo_source[na_idx] <- geo$geo_source[match_idx]
	
	#using google maps to fill in adm2 name that's not present in gadm
	d$latitude[is.na(d$latitude)] <- 7.412
	d$longitude[is.na(d$longitude)] <- 9.223
	
	d$geo_source[is.na(d$geo_source)] <- "Google Maps"
	
	#adding pests
	d$nematodes <- r$NemaSev
	d$mealy_bug <- r$MBSev
	d$beetle <- r$BeetleSev
	
	# reshaping pests to long
	d$row_id <- seq_len(nrow(d))
	
	d <- reshape(d,varying = c("nematodes", "mealy_bug", "beetle"),v.names = "severity_scale",
	          timevar = "pest",times = c("nematodes", "mealy_bug", "beetle"),idvar = "row_id",direction = "long")
	
	rownames(d) <- NULL
	d$row_id <- NULL

	 carobiner::write_files(path, meta, d)
}
