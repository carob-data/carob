# R script for "carob"
# license: GPL (>=3)

## ISSUES
## The plot area is unusually large compared to the reported harvested yield.


carob_script <- function(path) {

"
Replication Data for: SRP Survey Data for multiple sub-Saharan African Countries

Data are from surveys conducted by Africa Rice Center (AfricaRice) and National Agricultural Research and Extension Services (NARES) partners after the 2020/2021 rice harvest period across ten countries in SSA. The surveyed countries include Burkina Faso, Burundi, Democratic Republic of Congo, Kenya, Madagascar, Nigeria, Rwanda, Sierra Leone, Tanzania, and Uganda. Data were collected from 3,081 rice farming households using the SRP survey tool (RiceGap Finder) and covered all 46 requirements of the Sustainable Rice Platform (SRP) Standard and 12 Performance Indicators (SRP version 2.2). Farmers were randomly selected with the assistance of NARES partners in irrigated lowland, rainfed lowland, and rainfed upland systems. Individual interviews were conducted with the head farmers or the farm manager.
"

	uri <- "doi:10.7910/DVN/RWYT3T"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "AfricaRice; IRRI",
		publication = NA,
		project = NA,
		carob_date = "2026-06-09",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "SRP_Dataset.xls"]
	f2 <- ff[basename(ff) == "SRP_Dataset_dictionary.xls"]
	f3 <- ff[basename(ff) == "doi_10.7910_DVN_RWYT3T.json"]
	f4 <- ff[basename(ff) == "doi_10.7910_DVN_RWYT3T.zip"]
	f5 <- ff[basename(ff) == "doi_10.7910_DVN_RWYT3T_files.txt"]
	f6 <- ff[basename(ff) == "ok.txt"]

	r1 <- carobiner::read.excel(f1, sheet="Description")
	r2 <- carobiner::read.excel(f1, sheet="Data")
	
	### process
	d <- data.frame(
		hhid = as.character(r2$UID),
		country = gsub("DRC", "Democratic Republic of the Congo", r2$country),
		adm1 = r2$states,
		location = r2$village,
		farmer_gender = r2$gender,
		#crop_system = r2$farmsystem,
		irrigation_method = tolower(r2$irrigation),
		irrigated = !is.na(r2$irrigation),
		season = tolower(r2$riceseason),
		variety = ifelse(is.na(r2$variety_firstchoice) & !is.na(r2$variety_2ndchoice), r2$variety_2ndchoice,
		          ifelse(is.na(r2$variety_firstchoice) & is.na(r2$variety_2ndchoice), r2$variety_3rdchoice, r2$variety_firstchoice)),
		#soil_salinity = r2$SRPS_soil_salinity,
		plot_area = r2$SRPPI_pp_plotsize*10000,# m2
		yield = r2$SRPPI_yield_tha*1000,
		net_benefit = r2$SRPPI_profit_usdha,
		labour = r2$SRPPI_labour_mandayha,
		irrigation_number = as.integer(r2$SRPPI_irrig_times),
		N_fertilizer = r2$SRPPI_N_applied,
		P_fertilizer = r2$SRPPI_P_applied,
		trial_id = paste(r2$village, r2$UID, sep = "-"), 
		on_farm = FALSE, 
		is_survey = TRUE, 
		crop = "rice", 
		yield_part = "grain", 
		yield_moisture = as.numeric(NA), 
		planting_date = as.character(NA), 
		K_fertilizer = as.numeric(NA),
		yield_isfresh = TRUE,
		geo_from_source = FALSE
		
	)

### Adding lon and lat coordinate
	geo = data.frame(
	  country = c(rep("Rwanda", 15), rep("Madagascar", 6),rep("Burundi", 7),rep("Kenya", 9), rep("Burundi", 4),rep("Kenya", 7),rep("Madagascar", 3),rep("Burundi", 4),rep("Uganda", 15),rep("Burundi", 20),rep("Madagascar", 12),rep("Burundi", 10),rep("Sierra Leone", 41),rep("Burundi", 10),rep("Democratic Republic of the Congo", 8), rep("Sierra Leone", 8), rep("Rwanda", 15), rep("Madagascar", 2)),
	  location = c("Akarambo", "Bushishi", "Karambo", "Kigarama", "Kivumu", "Kiyanza", "Mpinga", "Mubuga", "Nyabitare", "Rugarama", "Rwabuye", "Taba", "Kamuhoza", "Karama", "Gitwa", "Ilaka  Centre", "Soavina", "Tsaramandroso", "Andramena", "Vohimasy", "Tsararano", "Gahwazi 1", "Nyamabere", "Mpanda", "Mwiruzi", "Kibimba", "Twinkwavu", "Gisoro", "Gacharu", "Canal", "Kangai", "Kiaga", "Kimathi", "Marura", "Githuri", "Ndindiruku", "Kandongu", "Ndava Village", "Nyamitanga", "Munyika", "Nyamitanga Village", "Kimorigo 4", "Kimorigo Block 4", "Madarasani", "Kimorigo", "Kimorigo_Block 4", "Kimorigo C", "Gategi", "Andriana", "Marofarihy", "Ambila", "Mutaho", "Ruhanza", "Kibogoye", "Rutegama", "Alii A", "Bira", "katikati A", "Katikati A", "Lacic", "lodi", "Lodi", "Lumule", "luyam", "Luyam", "Olony", "Ongai", "Pakuma", "Parubanga", "Perecu", "Ngugo", "Gasasa", "Rubuga", "Bugenyuzi", "Gitaramuka", "Mwenya", "Rwimbogo", "Rambo", "Nyakibingo", "Ramba", "Gatwe", "Gikuyo", "Gitwenzi", "Rutabo", "Burara", "Gasaka", "Mayengo", "Gisenyi", "Makamba Ii", "Muyange", "Ampanasana", "Befotaka", "Andranofotsy", "Andranomainty", "Androhibe", "Ankilizato", "Ankotrofotsy", "Antanimainty", "Antaninarenina", "Morarano", "Bemarivo Atsimo", "Bevoay", "Burambira", "Burima", "Ndava", "Gasave", "Gitwa", "Camugani", "Gahororo", "Kiremba", "Kibuye", "Ruhama", "Barbara", "Gbainty", "Gbainty  Wallah", "Gbainty wallah", "Gbaneh", "Kalangba", "Kamem", "Rosinor", "Koya", "Lokomasama", "magboka", "Magboka", "Malia", "Maloko", "masama", "Royark", "Royeama", "Koinadugu Village", "Kondembaia", "makeh", "makump", "Makump", "mamanso", "Mamanso", "Mabang", "Madina", "magbass", "Makakura", "moria", "Moria", "Musaia", "Robis", "Rochen", "Mamanso Kafla", "marunia", "Marunia", "Masang", "Masoko", "Kabala Town", "Kagbasia", "Yomadugu", "Bukemba", "Mukazye", "Nkurye", "Butezi", "Kabingo", "Kinyinya", "Rusange", "Nyamunazi", "Gisuru", "Gacokwe", "Kiliba", "Bwegera", "Kawizi", "Kavimvira", "kavimvira", "Luberizi", "Rugenge", "Luvungi", "Mando", "Mattru", "mattru", "Mamie", "Torma Bum", "Mokombo", "Tissana", "Torma  Bum", "Kabeza", "Kabuye", "Agatare", "Nyange", "Nyarubande", "Rugina", "Rugogwe", "Runyinya", "Rwankuba", "Kiyoro", "Murehe", "Nyakabungo", "Nyakariba", "Kigabiro", "kamatamu", "Ankarimaso", "Amboraka"),
	  longitude = c(29.473, 29.375, 29.840, 30.699, 29.318, 30.066, 29.049, 29.316, 28.995, 30.399, 29.753, 29.704, 30.050, 29.641, 28.885, 47.140, 46.734, 47.045, 47.765, 47.749, 47.527, 29.792, 29.407, 29.370, 30.804, 29.596, 30.655, 30.624, 37.217, -4.073, 37.300, 37.267, 36.079, 36.202, 36.439, 37.433, 37.283, 30.433, 29.264, 29.081, 29.264, 37.690, 37.690, 37.594, 37.690, 37.690, 37.690, 37.425, 48.016, 47.969, 48.009, 29.866, 30.025, 29.562, 29.749, 32.894, 30.944, 32.257, 32.257, 33.434, 31.876, 31.876, 33.476, 31.941, 31.941, 30.847, 31.994, 32.107, 32.108, 32.114, 30.101, 30.433, 30.041, 30.064, 30.173, 30.137, 30.048, 30.060, 30.062, 29.344, 29.991, 30.074, 30.036, 29.269, 30.224, 29.794, 29.522, 30.169, 29.805, 29.999, 48.489, 48.024, 49.803, 46.226, 47.466, 45.046, 45.529, 47.400, 47.525, 47.313, 44.774, 47.047, 30.149, 29.404, 29.811, 30.426, 29.803, 29.821, 29.726, 30.024, 29.509, 29.935, -12.773, -13.108, -13.108, -13.108, -12.953, -12.696, -13.201, -12.997, -11.006, -13.104, -13.096, -13.096, -11.229, -13.165, -11.933, -12.996, -12.912, -11.369, -11.220, -12.114, -12.206, -12.206, -11.886, -11.886, -12.173, -12.672, -12.004, -11.575, -11.934, -12.203, -11.571, -12.644, -12.284, -12.142, -12.255, -12.255, -11.886, -13.053, -11.550, -11.778, -11.439, 30.062, 30.270, 30.254, 30.155, 30.571, 30.343, 30.503, 30.375, 30.498, 30.553, 29.201, 29.052, 29.161, 29.164, 29.164, 29.063, 29.155, 29.023, -11.803, -11.376, -11.376, -11.206, -12.007, -12.558, -12.203, -12.007, 30.433, 30.082, 29.891, 29.636, 29.942, 29.937, 30.123, 29.606, 29.360, 29.863, 30.544, 29.504, 30.417, 30.448, 29.593, 47.611, 47.621)	, 
	  latitude = c(-2.704, -1.846, -1.626, -2.346, -1.779, -1.760, -2.644, -2.137, -2.610, -1.701, -2.586, -1.632, -1.959, -2.607, -2.546, -20.327, -19.795, -16.363, -22.629, -22.816, -22.950, -2.906, -3.229, -3.194, -3.102, -3.433, -3.262, -3.275, -0.633, 5.350, -0.617, -0.600, -0.304, -0.348, -0.777, -0.717, -0.650, -2.558, -3.065, -2.818, -3.065, -3.461, -3.461, -3.447, -3.461, -3.461, -3.461, -0.749, -22.137, -22.025, -21.967, -3.144, -3.575, -3.248, -3.304, 1.596, 3.118, 2.765, 2.765, 3.031, 2.604, 2.604, 2.731, 2.584, 2.584, 2.505, 2.736, 2.975, 3.064, 3.056, -3.266, -2.720, -2.381, -3.094, -2.981, -2.573, -2.621, -2.585, -2.632, -3.509, -4.220, -2.614, -2.591, -2.729, -2.514, -4.209, -4.127, -2.550, -4.135, -3.522, -20.450, -14.531, -15.428, -19.597, -18.997, -20.409, -19.800, -19.017, -18.910, -18.906, -21.726, -24.501, -2.730, -3.495, -3.742, -2.953, -2.776, -2.917, -3.027, -2.830, -3.392, -2.830, 8.706, 8.806, 8.806, 8.806, 9.080, 9.087, 8.594, 8.971, 8.899, 8.688, 8.952, 8.952, 8.461, 8.588, 8.967, 8.934, 8.940, 9.536, 9.161, 8.814, 8.401, 8.401, 8.788, 8.788, 8.565, 9.288, 8.679, 9.516, 8.758, 8.379, 9.760, 8.520, 8.667, 8.493, 8.654, 8.654, 8.559, 8.391, 9.588, 9.325, 9.573, -4.007, -3.772, -3.784, -3.394, -3.375, -3.657, -3.425, -3.703, -3.458, -3.431, -3.262, -2.904, -3.284, -3.350, -3.350, -3.020, -3.351, -2.858, 7.286, 7.874, 7.874, 7.863, 7.416, 7.782, 7.572, 7.416, -2.185, -1.875, -2.573, -1.429, -2.017, -1.921, -1.557, -1.789, -2.165, -1.509, -1.876, -2.423, -1.947, -1.979, -2.490, -21.302, -21.319)
	)
	
	
	d <- merge(d, geo, by= c("country", "location"), all.x = TRUE)
	i <- grepl("Canal", d$location) & grepl("Kenya", d$country)
	d$longitude[i] <- NA
	d$latitude[i]  <- NA
	## use adm1 to fix unknow location coordinate
	geo1 <- data.frame(
	  adm1 = c("Ngozi", "Karuzi", "Rutana", "Muyinga", "Kirundo", "Ruyigi", "Bubanza", "Makamba", "Cankuzo", "Gitega", "Cibitoke", "South Kivu", "Coast", "Central", "Eastern", "Menabe", "Fitovinany", "Vatovavy", "Atsimo Atsinanana", "Amoron'i Mania", "Umujyi wa Kigali", "Amajyepfo", "Southern", "Northern", "North West", "Gulu"),
	  lon = c(29.8249, 30.1625, 29.990, 30.338, 30.0961, 30.248, 29.393, 29.803, 30.551, 29.922, 29.127, 27.704, 35.0876, 36.0693, 37.247, 45.3106, 48.187,  47.910, 47.258, 46.576, 30.059, 29.645, -12.187, -11.629, -12.465, 32.287),
	  lat = c(-2.9108, -3.1039, -3.9283, -2.846, -2.584, -3.476, -3.083, -4.138, -3.218, -3.426, -2.910, -2.551, 0.2026, -0.284, -1.373, -19.914, -21.640, -21.028, -23.184, -20.359, -1.944, -2.323, 8.079, 9.327, 9.339, 2.77)
	  
	)
	
	d <- merge(d, geo1, by = "adm1", all.x = TRUE)
	
	d$longitude[is.na(d$longitude)] <- d$lon[is.na(d$longitude)]
	d$latitude[is.na(d$latitude)] <- d$lat[is.na(d$latitude)]
	d$lat <- d$lon <- NULL
	
	### Fixing Irrigation method
	d$irrigation_method <- ifelse(grepl("surface", d$irrigation_method), "surface", "unknown")
	
	d$N_fertilizer[which(d$N_fertilizer > 600)] <- NA
	d$P_fertilizer[which(d$P_fertilizer > 350)] <- NA
	
	### drop rows with missing location and adm1
	
	d <- d[!is.na(d$location)& !is.na(d$adm1),]
	
	carobiner::write_files(path, meta, d)
}
