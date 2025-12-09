# R script for "carob"
# license: GPL (>=3)

## ISSUES
### costs and benefits records are missing
#### fertilizer type and amount used are also missing 

carob_script <- function(path) {

"
Dataset for supporting the net agronomic assessment of yield limiting factors in maize production in Machakos county, Kenya

This dataset is used for a holistic analysis of the costs, benefits, and risks of on-farm soil and plant health management. The dataset was produced in 2017 by a combination of field measurements and farmer surveys. It was collected for a research study aimed at identifying and testing accurate, consistent, and cost-effective measurement tools and methodologies for evaluating the outcomes of agricultural projects. Soils data was analysed by wet spectral methods to generate estimates of the Nitrogen (N), Phosphorus (K), and Potassium (P) levels in the soils which was then used as inputs for a stochastic crop production model. The decision model consisted of two main sections targeting interactions between biotic factors (rainfall variability, availability of soil nutrients, risk of drought and temperature) and abiotic factors (farm management practices/intensity of farm management). With the two datasets, we ran a risk-return model to project the productivity of maize production and highlight yield-limiting factors.  The project was funded by Bill & Melinda Gates Foundation and TechnoServe under the Innovation in Outcome Measurement (IOM) program
"

	uri <- "doi:10.34725/DVN/KKHVOF"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "ICRAF",
		publication = NA,
		project = NA,
		carob_date = "2025-12-08",
		design = "unitOfAnalysis: Farm scale; dataCollector: World Agroforestry (ICRAF); samplingProcedure: We conducted a household survey to collect primary qualitative and quantitative information. A structured questionnaire which was pretested to evaluate the questions that covered a wide range of relevant agronomic and soil management topics was dispensed. A total of 134 households, comprising 58 females (43%) and 76 males (57%) were interviewed.; collectionMode: To clarify the decision problem and identify variables of interest in soil and plant health management, stakeholder engagement targeted Machakos county government officials, officials from fertilizer companies, agricultural research organizations and other development partners in a stakeholder workshop held in Nairobi on the 8-9 march, 2017. We used the insights generated during this workshop to design both the household survey used to collect sample data and the conceptual model used to project the decision’s outcomes.  The model was scripted in R programming language and ran using the ‘decisionSupport’ package. We ran 100,000 monte carlo iterations to produce distributions of output variables i.e. actual yield, projected yield and farm profits.; researchInstrument: Structured questionnaire and interviews",
		data_type = "compilation",
		treatment_vars = "none",
		response_vars = "none", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	
	f1 <- ff[basename(ff) == "SoilTissueGrainYieldHS4_Soil agronomy.csv"]
	#f2 <- ff[basename(ff) == "maize_production_input_table.csv"]
	#f3 <- ff[basename(ff) == "Maize_production_legend.csv"]
	#f4 <- ff[basename(ff) == "Variables Description_Soil agronomy.csv"]
	

	r1 <- read.csv(f1, na= c("", "NA", "<llq"))
	#r2 <- read.csv(f2)
	#r3 <- read.csv(f3)
	#r4 <- read.csv(f4)
	

	d1 <- data.frame(
	   adm1 = r1$County,
	   adm2 = r1$Sub.County,
	   adm3 = r1$Location,
	   location = r1$Sub.Location,
	   site = r1$Village,
	   trial_id = r1$Farmer.Name,
	   farmer_gender = r1$Gender,
	   field_size = r1$Estimate.Field.Size..Acres.,
	   irrigated = grepl("Irrigation", r1$Source.of.Water),
	   OM_used = grepl("Manure", r1$FarmNutrientInput),
	   OM_type = ifelse(grepl("Manure", r1$FarmNutrientInput), "farmyard manure", "none"),
	   fertilizer_used = grepl("Fertilizer", r1$FarmNutrientInput),
	   market_type = r1$Market,
	   elevation = r1$Altitude,
	   yield = r1$Reported.Yield.Kg.Acre./0.4047,
	   country = "Kenya",
	   soil_P = r1$m3_P_S,
	   soil_B = r1$m3_B_S,
	   soil_Cu = r1$m3_Cu_S,
	   soil_S = r1$m3_S_S,
	   soil_Mn = r1$m3_Mn_S,
	   soil_Zn = r1$m3_Zn_S,
	   soil_Fe = r1$m3_Fe,
	   soil_Al_total = r1$Al_S,
	   soil_Cr_total = r1$Cr_S,
	   soil_Fe_total = r1$Fe_S,
	   soil_Mn_total = r1$Mn_S,
	   soil_P_total = r1$P_S,
	   soil_Ti_total = r1$Ti_S,
	   soil_Zn_total = r1$Zn_S,
	   soil_C_total = r1$TC_S,
	   soil_N_total = r1$TN_S,
	   soil_CEC = r1$CEC,
	   soil_pH = r1$pH,
	   soil_EC = r1$ECd,
	   soil_ex_acidity = r1$Ex_Ac,
	   soil_Ca_exch = r1$ExCa,
	   soil_K_exch = r1$Ex_K,
	   soil_Mg_exch = r1$ExMg,
	   soil_Na_exch = r1$ExNa,
	   soil_Al = r1$m3_Al,
	   soil_Ni_total = r1$Ni_S,
	   soil_SOC = r1$OC,
	   soil_clay = r1$Clay,
	   soil_silt = r1$Silt,
	   soil_sand = r1$Sand,
	   #r1$m3_P_Pl,
	   #r1$m3_K_Pl,
	   #r1$m3_B_Pl,
	   #r1$m3_Cu_Pl,
	   #r1$m3_S_Pl,
	   #r1$m3_Mn_Pl,
	   #r1$m3_Zn_Pl,
	   #r1$m3_Ca_Pl,
	   #r1$m3_Mg_Pl,
	   #r1$m3_Mo_Pl,
	   #r1$m3_Na_Pl,
	   #r1$m3_Fe_Pl,
	   residue_Al = r1$Al_Pl/1000,
	   residue_Fe = r1$Fe_Pl/1000,
	   residue_Mn = r1$Mn_Pl/1000,
	   residue_P = r1$P_Pl/1000,
	   residue_Ti = r1$Ti_Pl/1000,
	   residue_C = r1$TC_Pl/1000,
	   residue_N = r1$TN_Pl/1000,
	   residue_Br = r1$Br_Pl/1000,
	   residue_Ca = r1$Ca_Pl/1000,
	   residue_Cd = r1$Cd_Pl/1000,
	   residue_Cl = r1$Cl_Pl/1000,
	   residue_Co = r1$Co_Pl/1000,
	   residue_Cu = r1$Cu_Pl/1000,
	   residue_K = r1$K_Pl/1000,
	   residue_Mg = r1$Mg_Pl/1000,
	   residue_Mo = r1$Mo_Pl/1000,
	   residue_Na = r1$Na_Pl/1000,
	   residue_Pb = r1$Pb_Pl/1000,
	   residue_Rb = r1$Rb_Pl/1000,
	   residue_S = r1$S_Pl/1000,
	   residue_Hg = r1$Hg_Pl/1000,
	   #r1$m3_P_Gr,
	   #r1$m3_K_Gr,
	   #r1$m3_B_Gr,
	   #r1$m3_Cu_Gr,
	   #r1$m3_S_Gr,
	   #r1$m3_Mn_Gr,
	   #r1$m3_Zn_Gr,
	   #r1$m3_Fe_Gr,
	   #r1$m3_Ca_Gr,
	   #r1$m3_Mg_Gr,
	   #r1$m3_Mo_Gr,
	   #r1$m3_Na_Gr,
	   #grain_Cr = r1$Cr_Gr,
	   grain_Fe = r1$Fe_Gr/1000,
	   grain_Mn = r1$Mn_Gr/1000,
	   grain_P = r1$P_Gr/1000,
	   grain_Ti = r1$Ti_Gr/1000,
	   grain_C = r1$TC_Gr/1000,
	   grain_Zn = r1$Zn_Gr/1000,
	   grain_N = r1$TN_Gr/1000,
	   grain_Rb = r1$Rb_Gr/1000,
	   grain_Hg = r1$Hg_Gr/1000,
	   #r1$Ba_Gr,
	   grain_Br = r1$Br_Gr/1000,
	   grain_Cd = r1$Cd_Gr/1000,
	   grain_Co = r1$Co_Gr/1000,
	   grain_K = r1$K_Gr/1000,
	   grain_Cl = r1$Cl_Gr/1000,
	   grain_Mg = r1$Mg_Gr/1000,
	   grain_Mo = r1$Mo_Gr/1000,
	   grain_Pb = r1$Pb_Gr/1000,
	   grain_S = r1$S_Gr/1000,
	   soil_texture = r1$Soil.Texture,
	   soil_color = r1$color,
	   planting_date = "2017", 
	   on_farm = FALSE, 
	   is_survey = TRUE, 
	   crop = "maize", 
	   yield_part = "grain", 
	   yield_moisture = as.numeric(NA), 
	  geo_from_source = FALSE
		
	)
	
	d1 <- d1[!is.na(d1$yield),]
	
	soilmeta <- data.frame(
	   variable= c("soil_P", "soil_B" , "soil_Cu", "soil_S" , "soil_Mn", "soil_Zn", "soil_Fe", "soil_Al", 
	               "soil_Al_total", "soil_Cr_total" ,"soil_Fe_total" ,"soil_Mn_total", "soil_P_total","soil_Ti_total","soil_Zn_total", "soil_C_total", "soil_N_total"),
	   method = c(rep("Mehlich3", 8), rep("X-Ray Fluorescence Spectrometry", 9))
	)
	
	### Fixing soil texture
	
	P <- carobiner::fix_name(d1$soil_texture)
	P <- gsub("Sandy Red Soil|Sandy Black Soil|Sandy Soil|^Sandy$","sand", P)
	P <- gsub("Loamy Red Soil|Loamy Soil","loam", P)
	P <- gsub("Black Cotton|Red Soil|Greyish Black Soil", NA , P)
	d1$soil_texture <- tolower(P) 
	
	### Adding long and lat coordinate 
	
	geo <- data.frame(
	   site = c("Kathuma", "Kyanganga", "Kithuiani", "Wetaa", "Iani", "Katithi", "Kiwanza", "Kithiani", "Kyangii", "Kangungu", "Kanduu", "Kamuya", "Kavete", "Kivingoni", "Muamba", "Ithanga", "Mikuyu", "Ngomano", "Mukayauni", "Mbembani", "Kyeni", "Kakongo", "Muaani", "Kianisyoni", "Kwa Muthee", "Ithekethini", "Kithekani", "Kitange", "Manyatta", "Kinyungu", "Kivaani", "Kikuyuni", "Miu", "Kithimani","Kithini", "Ngomani", "Mikoikoni", "Katui", "Uamani", "Kivuluni", "Kayangi", "Kaseveni", "Kibao", "Kivulusa", "Mathunthini"),
	   longitude = c(38.0128, 37.447, 37.393, 37.433, 37.775, 37.442, 38.326, 37.99, 38.037, 38.383, 38.096, 37.446, 38.075, 37.508, 38.182, 37.341, 37.199, 37.479, 37.557, 37.475, 37.582, 37.915, 38.668, 37.546, 36.951, 37.543, 38.155, 37.420, 37.397, 37.4006, 37.388, 37.198, 37.401, 37.447, 37.388, 37.380, 37.3827, 38.138, 37.344, 37.125, 37.685, 37.448, 37.120, 37.448, 37.459),
	   latitude = c(-1.312, -1.407, -1.068, -1.4116, -0.906, -1.328, -0.206, -1.779, -0.686, -0.932, -1.485, -1.291, -0.801, -1.023, -1.766, -0.989, -1.559, -0.712, -0.978, -1.0343, -0.403, -0.835, -0.520, -1.147, -0.395, -1.195, -2.618, -1.366, -1.354, -1.367, -1.399, -1.172, -1.348, -1.182, -1.3949, -1.349, -1.333, -0.782, -1.256, -1.0595, -1.407, -1.624, -0.755, -1.373, -1.377)
	)
	
	d <- merge(d1, geo, by= "site", all.x = TRUE)
	
	##  these "sites" are unknown on Google map : "Kitia", ""Kwamelo","Kivusula, Kwanguli, Ndumbuli, Kwa Ndula, Kwa Mulinda, Kavalia, Kyaana, Ngolyu, Kithundi, Kithuiyani, Yanthoko, Kwa Ngunzu
     ## We use "location" to fill the geocoordinates related to these "sites".
		geo1 <- data.frame(
	   location = c("Katitu", "Ngumuti", "Kyevaluki", "Mango", "Kamwala", "Ikaatini", "Ekalakala", "Ndalani", "Kibau", "Kyawango", "Mumbuni"),
	   lon = c(37.351, 37.369, 37.343, 37.399, 37.249, 37.557, 37.470, 37.423, 37.367, 37.487, 37.234),
	   lat = c(-1.205, -1.342, -1.394, -1.398, -1.452, -1.079, -0.968, -1.116, -1.3146, -1.311, -1.498)
	   
	)
	
	d <- merge(d, geo1, by= "location", all.x = TRUE)
	
	d$longitude[!is.na(d$lon)] <- d$lon[!is.na(d$lon)]
	d$latitude[!is.na(d$lat)] <- d$lat[!is.na(d$lat)]
	d$lon <- d$lat <- NULL
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	
carobiner::write_files(path, meta, d, var_meta=soilmeta)

}


