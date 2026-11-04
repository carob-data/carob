# R script for "carob"

# NOTE 
# should contact authors for yield data and coordinates

## ISSUES
# Plot yields are not in the data file, so country-crop-treatment means from the paper were not written as plot yields.

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
		response_vars = "grain_N",
		treatment_vars = "N_fertilizer;P_fertilizer;P_fertilizer;K_fertilizer;S_fertilizer;Mg_fertilizer;Zn_fertilizer;B_fertilizer", 
		carob_contributor = "Cedric Ngakou", 
		carob_effort = NA,
		carob_date = "2024-06-29",
		notes = "LLM-review: [medium, fertilizer] N, P, and K are filled only when treatment is NPK. The MgSZnB treatment is left at 0. Pulses and groundnut stay yield_part grain. Total N is not converted to mg/kg."
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
   
	d$yield_isfresh <- as.numeric(NA) #needs to be checked
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
}


