# R script for "carob"

carob_script <- function(path) {

"Gudeta W. Sileshi, Festus K. Akinnifesi, Oluyede C. Ajayi, Bart Muys, 2011. Integration of legume trees in maize-based cropping systems improves rain use efficiency and yield stability under rain-fed agriculture, Agricultural Water Management 98: 1364-1372

Water availability is a major constraint to crop production in sub-Saharan Africa (SSA) where agriculture is predominantly rain-fed. This study aimed to investigate the effect of the nitrogen-fixing legume tree Leucaena (Leucaena leucocephala) and ino rganic fertilizer on rain use efficiency (RUE), a robust measure of productivity and land degradation, in three long-term (11–12 years) experiments conducted in Zambia and Nigeria. On the two Zambian sites, sole maize (Zea mays) grown continuously (for 11–12 years) with the recommended fertilizer achieved the highest RUE (3.9–4.6 kg ha−1 mm−1) followed by maize intercropped with Leucaena (2.5–3.4 kg ha−1 mm−1). This translated to 192–383% increase in RUE over the control (maize grown without nutrient inputs), which is the de facto resource-poor farmers’ practice. RUE was more stable in fully fertilized sole maize on the first Zambian site and not statistically different from the maize–Leucaena associations on the second site. On the Nigerian site, RUE was higher in maize planted between Leucaena hedgerows supplemented with 50% of the recommended fertilizer (3.9 kg ha−1 mm−1), maize grown between Leucaena hedgerows without fertilizer (3.0 kg ha−1 mm−1) and sole maize receiving the recommended fertilizer (2.8 kg ha−1 mm−1), which translated to increases in RUE of 202%, 139% and 85%, respectively, over the control. RUE was more stable in the maize grown between Leucaena hedgerows than in the fully fertilized maize. On all sites RUE was least stable in the control. Yield stability in the maize–Leucaena association was not significantly different from the fully fertilized maize on the Zambian sites. On the Nigerian site, maize yields were more stable in maize grown in Leucaena hedgerows than in fully fertilized sole maize. Supplementation of maize grown in Leucaena hedgerows with 50% of the recommended fertilizers resulted in greater yield stability. It is concluded that intercropping cereals with legume trees and supplementation with inorganic fertilizer can increase rain use efficiency and yield stability in rain-fed agriculture in SSA. (2011-07)"

	uri <- "doi:10.34725/DVN/FUZDMU"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=0,
		publication = "doi:10.1016/j.agwat.2011.04.002",
		carob_contributor = "Camila Bonilla",
		carob_date="2021-06-01",
		data_type = "experiment",
		data_organization ="ICRAF;KEU",
		project=NA,
		response_vars= "yield",
		treatment_vars="N_fertilizer;P_fertilizer;K_fertilizer;trees"		
	)



	f <- ff[basename(ff) == "Ibadan data.xls"]
	d <- carobiner::read.excel(f)
	d1 <- d[1:12, 1:16]
	d2 <- d[16:27, ]
	colnames(d2) <- d[15,]
	d12 <- cbind(d1, d2[,-1])

	d <- d12[, c("Year", "TotalRF", "GRLL50F", "GRLL", "GRFert", "GRNofert")]
	d <- reshape(d, varying = c("GRLL50F", "GRLL", "GRFert", "GRNofert"), 
		timevar = "treatment", direction="long", v.names = "yield",  
		times = c("GRLL50F", "GRLL", "GRFert", "GRNofert"))
	d$id <- NULL	
	colnames(d) <- c("planting_date", "rain", "treatment", "yield")
	
	# Information from the paper
	d$soil_type <- 'Ferric luvisol'
	d$N_fertilizer <- 0
	d$P_fertilizer <- 0
	d$K_fertilizer <- 0
	d$N_fertilizer[d$treatment=='GRFert'] <- 90
	d$P_fertilizer[d$treatment=='GRFert'] <- 40
	d$K_fertilizer[d$treatment=='GRFert'] <- 40
	d$N_fertilizer[d$treatment=='GRLL50F'] <- 45
	d$P_fertilizer[d$treatment=='GRLL50F'] <- 20
	d$K_fertilizer[d$treatment=='GRLL50F'] <- 20
	i <- grep("GRLL", d$treatment)
	d$treatment <- "no Leucaena"
	d$treatment[i] <- "Leucaena"
	d$longitude <- 3.54
	d$latitude <- 7.30
	d$location <- "Ibadan"
	d$country <- "Nigeria"
	d$adm2 <- "Oyo"
	d$variety <- "MM604"
	d$variety_type <- "hybrid"
	d$harvest_date <- paste0(d$planting_date, "-10")
	d$planting_date <- paste0(d$planting_date, "-04")
	d$soil_SOC <- NA
	d$soil_P_total <- NA
	d$soil_N <- NA
	d$soil_K <- NA
	d$soil_pH <- NA
	d$dmy_total <- NA
	d$soil_sand <- NA
	d$soil_clay <- NA
	d$rain <- as.numeric(d$rain)
	d$yield <- as.numeric(d$yield) * 1000
	
	f <- ff[basename(ff) == "Msekera data.xls"]
	z <- as.data.frame(readxl::read_excel(f))
	z <- z[, c("Year", "Treat", "Grain", "Total", "Rainfall")]
	colnames(z) <- c("year", "treatment", "yield", "dmy_total", "rain")
	z$yield <- as.numeric(z$yield) * 1000
	z$dmy_total[z$dmy_total == "."] <- NA
	z$dmy_total <- as.numeric(z$dmy_total) * 1000
	i <- which(colnames(z) %in% c("yield", "dmy_total"))
	z <- aggregate(z[,i], z[,-i], mean)

	z$year <- z$year + 1994
	z$planting_date <- paste0(z$year, "-11")
	z$harvest_date <- paste0(z$year + 1, "-04")
	z$year <- NULL
	z$longitude <- 32.34
	z$latitude <- -13.39
	z$location <- "Msekera"
	z$country <- "Zambia"
	z$adm2	 <- NA
	z$soil_type <- "Ferric luvisol"
	z$soil_sand <- 61
	z$soil_clay <- 28
	z$soil_SOC <- 10.2
	z$soil_P_total <- 2.02 # mg/kg
	z$soil_N <- 100 * 0.70 # g/kg to mg/kg
	z$soil_K <- 390 * 1.47 # cmolc kg−1 to mg/kg
	z$soil_pH <- 5.3
	z$K_fertilizer <- z$P_fertilizer <- z$N_fertilizer <- 0
	i <- z$treatment == "Fertilized maize"
	z$N_fertilizer[i] <- 20 + 92
	z$P_fertilizer[i] <- 18
	z$K_fertilizer[i] <- 16

	z$variety <- NA
	z$variety_type <- NA

	d <- rbind(d, z)

	i <- z$treatment == "Leucaena"
	d$treatment <- "no Leuceaena"
	d$treatment[i] <- "Leuceaena"
	d$trees <- "none"
	d$trees[i] <- "Leuceaena"

	
	d$rain <- as.numeric(d$rain)
	d$on_farm <- FALSE
	d$crop <- "maize"
	d$yield_part <- "grain"
	
	d$fertilizer_type <- "none"
	d$fertilizer_type[d$N_fertilizer > 0] <- "urea; NPK"
	
	
	d$trial_id <- as.character(as.integer(as.factor(paste(d$location, d$planting_date))))

	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- FALSE

	carobiner::write_files(meta, d, path=path)
}




