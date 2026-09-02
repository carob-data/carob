# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. I left out the severity and incidence data because there is no scale for both parameters

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
	
	xy <- carobiner::adm_pointRadius("Nigeria", 2)
	nig <- unique(xy$adm2)
	
	name_fix <- c(
	  "Gwagwalade" = "Gwagwalada",
	  "Nassarawa Eggon" = "Nasarawa Eggon",
	  "katcha" = "Katcha",
	  "Paiko" = "Paikoro",
	  "Oorelope" = "Orelope",
	  "Atigbo" = "Atisbo",
	  "Buruku" = "Bukuru",
	  "K/Ala" = "Katsina-Ala",
	  "AMAC" = "Abuja Municipal Area Council"
	)
	
	d$adm2 <- ifelse(d$adm2 %in% names(name_fix),name_fix[d$adm2],d$adm2)
	xy_cols <- c("adm2", "latitude", "longitude", "geo_uncertainty")  
	d <- merge(d,xy[, xy_cols],by = "adm2",all.x = TRUE)	
	
	d$longitude[d$adm2=="Abuja Municipal Area Council"] <- 7.37831
	d$latitude[d$adm2=="Abuja Municipal Area Council"] <- 8.99997
	d$latitude[d$location=="Ndaabarshi"] <- 9.30692#kpaduma doesnt exist,Naarbashi corresponds to a school, coordinates pointing in the nearest field
	d$longitude[d$location=="Ndaabarshi"] <- 6.2523
	d$trial_id <-  paste(d$location,d$variety,sep = "_")

	carobiner::write_files(path, meta, d)
}
