# R script for "carob"

# NOTE 
# should contact authors for yield data and coordinates

carob_script <- function(path) {
   
"Fertilizer application can affect nutrient concentrations of edible plant products. Data from 70 crop-nutrient response trials conducted in Mali, Niger, Nigeria, and Tanzania were used to evaluate nutrient application effects on nutrient concentrations for grain of five pulse and five cereal crops and for storage roots of cassava (Manihot esculenta L.).

Treatments per trial were ≥12 but this study was limited to: no fertilizer applied; macronutrients applied (NPK or PK); and the macronutrient treatment plus Mg, S, Zn, and B applied (MgSZnB). Dried grain or cassava flour samples were analyzed for concentrations of all essential soil nutrients except for Ni and Cl. Concentrations of N and K were positively correlated with concentrations of most other nutrients.

The concentrations were relatively low overall for cowpea (Vigna unguiculata L.) and pigeonpea (Cajanus cajan L.) compared with other pulse crops and for maize (Zea mays L.) compared with other cereal crops. Application of NPK or PK had little effect on nutrient concentrations except for increased mean cereal grain concentrations for N, Ca, Mg, S, Zn, Cu, and B.

Bean (Phaseolus vulgaris L.), maize and rice (Oryza sativa L.) grain concentrations were reduced by MgSZnB for N, K, S, Cu, Mn, and B.There were no or inconsistent effects of MgSZnB on other crop-nutrient concentrations. Nutrient concentrations are not reduced by NPK for non-legumes or PK for pulses but MgSZnB often reduced bean and cereal nutrient concentrations with greater reductions for immobile compared with mobile nutrients."
 
	uri <- "doi:10.5061/dryad.pj76g30"
	group <- "agronomy" 
   
	ff  <- carobiner::get_data(uri, path, group)
   
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=NA, 
		data_organization = "UNL;ABU;INRAN;TARI", 
		publication ="doi:10.2134/agronj2018.04.0274", 
		project = NA, 
		data_type = "experiment", 
		response_vars = "yield",
		treatment_vars = "N_fertilizer;P_fertilizer;P_fertilizer;K_fertilizer;S_fertilizer;Mg_fertilizer;Zn_fertilizer;B_fertilizer", 
		carob_contributor = "Cedric Ngakou", 
		carob_date = "2024-06-29"
	)
   
   f <- ff[basename(ff)=="Grain Nutrient Conc Study for Tropical Africa 2018.xlsx"]
   
   r <- carobiner::read.excel(f,sheet = "Data")
   
   d <- data.frame(
      country= r$Country,
      trial_id= r$Plot_code,
      planting_date= as.character(r$Year),
      crop= r$Crop,
      treatment= r$Contr1Macro2MgZnSB3,
      on_farm= TRUE,
      irrigated= NA,
      is_survey= FALSE,

	  # soil 
      soil_pH= r$pH,
      soil_B= r$B...21,
      soil_Cu= r$CU,
      soil_Fe= r$FE,
      soil_Mn= r$MN,
      soil_S= r$S...26,
      soil_Zn= r$Zn...27,
      soil_ex_Ca= r$ExCa,
      soil_ex_Mg= r$ExMg,
      soil_ex_K= r$ExK,
      soil_N= r$TotN,
      soil_P_Mehlich= r$m3P,
      soil_C= r$TotCarbon,
      soil_clay= r$Clay,
      soil_silt= r$Silt,
      soil_sand= r$Sand,

	  # Nutrient contain in grain 
      grain_B= r$B...7,
      grain_Ca= r$Ca,
      grain_Cu= r$Cu,
      grain_K= r$K,
      grain_Mg= r$Mg,
      grain_Mn= r$Mn,
      #grain_Na= r$Na,
      grain_P= r$P,
      grain_S= r$S...16,
      grain_N= r$N
   )

   d$treatment <- c("control", "NPK", "MgSZnB")[d$treatment]
   ## adding yield data ( from doi:10.2134/agronj2018.04.0274)
   ## It is a mean value of yield per country and per crop 
   ## Tanzania 
	yd1 <- data.frame(
		country= "Tanzania",
        crop=rep(c("Bean", "Cassava", "Cowpea", "Maize", "Pigeonpea", "Soybean", "Sorghum", "Wheat"), 3),
		treatment = rep(c("control", "NPK", "MgSZnB"), each=8),
        yield = c(1.83, 16.40, 2.35, 3.76, 2.08, 1.09, 4.33, 1.85, # control
                   2.06, 25.9, 2.61, 4.41, 2.29, 1.36, 4.935, 2.32, # NPK
                   1.83, 19.21, 2.35, 3.76, 2.08, 1.09, 4.33, 2.18)   # MgSZnB
   )
   ## Niger 
   yd2 <- data.frame(
      country= "Niger",
      crop=rep(c("Cowpea", "Groundnut", "Maize", "Pearl millet", "Rice", "Sorghum"),3),
	  treatment = rep(c("control", "NPK", "MgSZnB"), each=6),
      yield=c(0.56, 0.64, 1.47, 0.66, 2.30, 1, # control
				0.74, 0.87, 1.93, 0.87, 2.84, 1.43, # NPK
				0.71, 0.79, 1.47, 0.66, 2.30, 1) # MgSZnB
   )
   ## Nigeria 
   yd3 <- data.frame(
      country="Nigeria",
      crop=rep(c("Groundnut", "Maize", "Soybean", "Sorghum"), 3),
	  treatment = rep(c("control", "NPK", "MgSZnB"), each=4),
      yield=c(1.07, 4.87, 1.83, 1.21, # control
				1.74, 5.75, 2.42, 1.85, # NPK
				1.07, 4.87, 2.06, 1.47) #MgSZnB
   )
   ## Mali 
   yd4 <- data.frame(
      country= "Mali",
      crop=rep(c("Maize", "Pearl millet", "Rice"), 3),
	  treatment = rep(c("control", "NPK", "MgSZnB"), each=3),
      yield=c(2.82, 1.05, 2.21, # control treatment
				4.03,1.24, 2.66,  # NPK trial treatment
				2.82, 1.05, 2.21) ## MgSZnB treatment
   )

   yd <- rbind(yd1, yd2, yd3, yd4)
   yd$yield <- yd$yield * 1000 ## in kg/ha
   
   d <- merge(d, yd, by=c("country", "crop", "treatment"), all.x = TRUE)
   
   p <- carobiner::fix_name(d$crop, "lower")
   p <- gsub("bean", "common bean", p)
   p <- gsub("soycommon bean", "soybean", p)
   p <- gsub("pigeonpea", "pigeon pea", p)
   d$crop <- p
   
   ### fertilizer 
   d$N_fertilizer <- d$P_fertilizer <- d$P_fertilizer  <- d$K_fertilizer <- d$S_fertilizer <- d$Mg_fertilizer <-  d$Zn_fertilizer <- d$B_fertilizer <- 0
   
   d$N_fertilizer[d$treatment=="NPK"] <- 90
   d$P_fertilizer[d$treatment=="NPK"] <- 15/2.29
   d$K_fertilizer[d$treatment=="NPK"] <- 20/1.2051
   d$S_fertilizer[d$treatment=="MgSZnB"] <- 15
   d$Mg_fertilizer[d$treatment=="MgSZnB"] <- 10
   d$Zn_fertilizer[d$treatment=="MgSZnB"] <- 3.5
   d$B_fertilizer[d$treatment=="MgSZnB"] <- 0.5
   
   ## The data and the article do not specify the site where the experiment was carried out.
	d$longitude <- d$latitude <- as.numeric(NA)
	d$geo_from_source <- FALSE
	
   d$yield_part= "grain"
   d$yield_part[d$crop=="cassava"] <- "roots"

	d <- d[!is.na(d$treatment), ]
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
}


