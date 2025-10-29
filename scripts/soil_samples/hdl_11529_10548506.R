# R script for "carob"
# license: GPL (>=3)



carob_script <- function(path) {

"Soil health data of 20 maize-based conservation agriculture experiments in Mexico
  
Conservation agriculture (CA) is based on minimal tillage, permanent soil cover and crop diversification. The data in this study were collected in a field trial network established to adapt CA to Mexico’s diverse cropping systems and local conditions. Soil health was studied in 20 trials with maize (Zea mays L.) in acro-ecologies ranging from handplanted traditional systems to intensive irrigated systems, initiated between 1991 and 2016. Soil in CA was compared to the local conventional practice, commonly involving tillage, residue removal and continuous maize. Samples for lab analysis were collected at 0-5 cm and 5-30 cm. Soil health parameters measured were organic matter, P concentration, interchangeable bases (Ca, Mg, Na and K), micronutrients (B, Cu, Mn, Fe, Zn), S, nitrates, soil texture, electrical conductivity and mean weight diameter of soil aggregates after dry and wet sieving. Measurements of physical soil health in the field included time-to-pond and penetration resistance with a dynamic penetrometer."


	uri <- "hdl:11529/10548506"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = "doi/10.1002/ldr.3894",
		project = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		completion = 100,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2025-10-26",
		notes = NA,
		design = NA
	)

	f <- ff[basename(ff) == "DAT-SoilHealthPlatforms.xlsx"]
	r <- carobiner::read.excel(f, sheet ="Chemical soil parameters", na=".")

	d <- data.frame(
		country = "Mexico",
		soil_sand = r$Sand,
		soil_clay = r$Clay,
		soil_silt = r$Silt,
		soil_pH = r$pH,
		soil_SOM = r$`Soil Organic Matter`,
		soil_K = r$Potassium,
		soil_Ca = r$Calcium,
		soil_Mg = r$Magnesium,
		soil_Na = r$Sodium,
		soil_Fe = r$Iron,
		soil_Zn = r$Zinc,
		soil_Mn = r$Manganese,
		soil_Cu = r$Copper,
		soil_B = r$Boron,
		soil_Al= as.numeric(r$Aluminum),
		soil_S = r$Sulphur,
		soil_EC =r$`Electrical Conductivity`,
		soil_CEC= as.numeric(r$`Cation Exchange Capacity`)
	) 

	loc <- do.call(rbind, strsplit(r$Site, ", "))
	d$location <- loc[,1]

	abb <- data.frame(
		abb = c('CAM', 'CHI', 'GTO', 'HID', 'JAL', 'MEX', 'MIC', 'OAX', 'PUE', 'QTO', 'SLP'),
		adm = c("Campeche", "Chiapas", "Guanajuato", "Hidalgo", "Jalisco", "México", "Michoacán", "Oaxaca", "Puebla", "Querétaro", "San Luis Potosí")
	)
	i <- match(loc[,2], abb$abb)
	d$adm1 <- abb$adm[i]
 
	geo <- data.frame(
		location = c("Apaseo el Alto", "Cadereyta de Montes", 
               "Comitán", "Cuautempan", "Francisco I. Madero", "Hopelchén", 
               "Indaparapeo", "Metepec", "Molcaxac", "Pénjamo", "San Juan Cotzocón", 
               "San Juan del Rio II", "San Martin Hidalgo", "San Miguel Tlacamama", 
               "Santo Domingo Yanhuitlán", "Soledad de Graciano Sanchez", 
               "Texcoco I", "Texcoco II", "Venustiano Carranza", "Villa Corzo"),
		longitude = c(-100.5922, -99.666, -92.1349, -97.7938, -99.1094, 
                -89.6207, -100.9429, -99.5937, -97.88, -101.8405, -95.5297, 
                -99.9789, -103.9118, -98.1126, -97.3359, -100.8672, -98.8487, 
                -98.8487, -92.6379, -93.265),
		latitude = c(20.4326, 20.7912, 16.2441, 19.9183, 20.2540, 
				19.5393, 19.7602, 19.2511, 18.6737, 20.4055, 17.3126, 
				20.3945, 20.4594, 16.445, 17.5261, 22.2971, 19.5287,
				19.5287, 16.309, 16.0632)
	)

	d <- merge(d, geo, by="location")
	d$geo_from_source <- FALSE

	r$`Phosphorous Bray` <- as.numeric(r$`Phosphorous Bray`)
	r$`Phosphorous Olsen`  <- as.numeric(r$`Phosphorous Olsen`)
	d$soil_P <- ifelse(!is.na(r$`Phosphorous Bray`), r$`Phosphorous Bray`, r$`Phosphorous Olsen`)

	depth <- do.call(rbind, strsplit(r$Depth, "_"))
	d$depth_top <- as.numeric(depth[,1])
	d$depth_bottom <- as.numeric(depth[,2])

	#from pulication
	soilmeta <- data.frame(
		variable = c("soil_Al", "soil_B", "soil_Ca", "soil_Fe", "soil_K", "soil_Mg", "soil_Mn", "soil_Na", "soil_S", "soil_P", "soil_Zn", "soil_Cu"),
		method = c("Potassium Chloride","DTPA+Sorbitol pH 7","Ammonium Acetate","DTPA+Sorbitol pH 7","Ammonium Acetate","Ammonium Acetate","DTPA+Sorbitol pH 7","Ammonium Acetate","Turbidimetric","Bray;Olsen","DTPA+Sorbitol pH 7","DTPA+Sorbitol pH 7")
	)

	carobiner::write_files(path, meta, d, var_meta=soilmeta)
}
