# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Rapid appraisal of potato production practices in Rwanda

Assessment of the current status of agronomy practices in potato cultivation in Rwanda through a farm survey with 288 households. Five districts in Rwanda were selected for the study based on the total area under potato cultivation: Nyabihu, Rubavu, Musanze, Burera and Gicumbi. Most of the sectors in those districts are located in two major agroecological zones: the Cone and High Volcanic Plains and the Buberuka Highlands.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/HUHFXN"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: farmers; universe: Musanze, Nyabihu, Burera, Gicumbi districts; timeMethod: Cross-sectional; samplingProcedure: 49 Farmers in Musanze, 24 in Nyabihu, 48 in Rubavu, 96 in Burera and 72 in Gicumbi district",
		
		# data_type can be e.g. "on-farm experiment", "survey", "compilation"
		data_type = NA,
		
		# treatment_vars has semi-colon separated variable names that represent the
		# treatments if the data is from an experiment. E.g. "N_fertilizer;P_fertilizer;K_fertilizer"
		treatment_vars = "",
		
		# response variables of interest such as yield, fwy_residue, disease incidence, etc. Do not include variables
		# that describe management for all treatments or other observations that were not related to the aim of 
		# the trial (e.g. the presence of a disease).
		response_vars = "", 

		# notes for the end-user
		notes = "",

		carob_contributor = "Your Name",
		carob_date = "2026-10-30",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "001_Rwanda_Potato_Production_Rapid.xlsx"]
	f2 <- ff[basename(ff) == "003_fertilizers_of_Rwanda_q010_group_q011_group_part_3_s4.xlsx"]
	f3 <- ff[basename(ff) == "004_fertilizer_of_Rwanda_q010_group_q011_group_part_3_s5.xlsx"]
	f4 <- ff[basename(ff) == "005_fertilizer_of_Rwanda_q010_group_q011_group_part_3_s6.xlsx"]
	f5 <- ff[basename(ff) == "006_fertilizer_of_Rwanda_q010_group_q011_group_part_3_s8.xlsx"]
	f6 <- ff[basename(ff) == "007_Data_dictionary_of_Rwanda.xlsx"]
	f7 <- ff[basename(ff) == "Rwanda_Potato_Production_Rapid_Appraisal_V7.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)
	r5 <- carobiner::read.excel(f5)
	r6a <- carobiner::read.excel(f6, sheet="dictionary")
	r6b <- carobiner::read.excel(f6, sheet="Fertilizate")
	r7a <- carobiner::read.excel(f7, sheet="survey")
	r7b <- carobiner::read.excel(f7, sheet="choices")
	r7c <- carobiner::read.excel(f7, sheet="settings")

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		latitude = r1[["q010_group/q011_group/part_3/_gps_points_latitude"]],
		longitude = r1[["q010_group/q011_group/part_3/_gps_points_longitude"]],
		elevation = r1[["q010_group/q011_group/part_3/_gps_points_altitude"]],
		hhid = r1[["q079/Farmer bulls"]],
		crop = tolower(r1[["q112/Imidacroplid"]])
	)
##r1: "part_1/q001", "part_1/q003", "part_1/q004", "part_1/q005", "part_1/q007", "part_1/q008", "part_1/q009", "part_1/q010", "q010_group/q011", "code", "q010_group/q011_group/q016", "q010_group/q011_group/q017", "q010_group/q011_group/q018", "q010_group/q011_group/q018_oth", "q010_group/q011_group/q019", "q010_group/q011_group/q020", "q010_group/q011_group/q021", "q010_group/q011_group/q022", "q010_group/q011_group/q023", "q010_group/q011_group/q024", "q010_group/q011_group/q025", "q010_group/q011_group/q026", "q010_group/q011_group/q027", "q010_group/q011_group/q030", "q030/Wheelbarrow", "q030/Spray pump", "q030/House", "q030/Car", "q030/Motorbike", "q030/Bicycle", "q030/None of the above", "q010_group/q011_group/q031", "q031/Wheat", "q031/Maize", "q031/Sorghum", "q031/Rice", "q031/Sweetpotato", "q031/Irish potato", "q031/Cassava", "q031/Taro", "q031/Yam", "q031/Cooking banana", "q031/Tomato", "q031/Cabbage", "q031/Carrots", "q031/Onion", "q031/Pumpkin", "q031/Chili pepper", "q031/Amaranth", "q031/Beetroot", "q031/Green/red pepper", "q031/Eggplant (purple, large)", "q031/Eggpplant (green, small)", "q031/Banana (wine)", "q031/Banana fruit", "q031/Mango", "q031/Papaya", "q031/Passion fruit", "q031/Tree tomato", "q031/Pineapple", "q031/Avocado", "q031/Bush beans", "q031/Climbing beans", "q031/Peas", "q031/Soyabeans", "q031/Groundnuts", "q031/French beans", "q031/Tea", "q031/Coffee", "q031/Tobacco", "q031/Sugar cane", "q031/Pyrethrum", "q031/Mushrooms", "q031/Other (1)", "q031/Other (2)", "q010_group/q011_group/q031_oth1", "q010_group/q011_group/q031_oth2", "q010_group/q011_group/q032b", "q010_group/q011_group/q032", "q032/Wheat", "q032/Maize", "q032/Sorghum", "q032/Rice", "q032/Sweetpotato", "q032/Irish potato", "q032/Cassava", "q032/Taro", "q032/Yam", "q032/Cooking banana", "q032/Tomato", "q032/Cabbage", "q032/Carrots", "q032/Onion", "q032/Pumpkin", "q032/Chili pepper", "q032/Amaranth", "q032/Beetroot", "q032/Green/red pepper", "q032/Eggplant (purple, large)", "q032/Eggpplant (green, small)", "q032/Banana (wine)", "q032/Banana fruit", "q032/Mango", "q032/Papaya", "q032/Passion fruit", "q032/Tree tomato", "q032/Pineapple", "q032/Avocado", "q032/Bush beans", "q032/Climbing beans", "q032/Peas", "q032/Soyabeans", "q032/Groundnuts", "q032/French beans", "q032/Tea", "q032/Coffee", "q032/Tobacco", "q032/Sugar cane", "q032/Pyrethrum", "q032/Mushrooms", "q032/Other (1)", "q032/Other (2)", "q010_group/q011_group/q033", "q033/Wheat", "q033/Maize", "q033/Sorghum", "q033/Rice", "q033/Sweetpotato", "q033/Irish potato", "q033/Cassava", "q033/Taro", "q033/Yam", "q033/Cooking banana", "q033/Tomato", "q033/Cabbage", "q033/Carrots", "q033/Onion", "q033/Pumpkin", "q033/Chili pepper", "q033/Amaranth", "q033/Beetroot", "q033/Green/red pepper", "q033/Eggplant (purple, large)", "q033/Eggpplant (green, small)", "q033/Banana (wine)", "q033/Banana fruit", "q033/Mango", "q033/Papaya", "q033/Passion fruit", "q033/Tree tomato", "q033/Pineapple", "q033/Avocado", "q033/Bush beans", "q033/Climbing beans", "q033/Peas", "q033/Soyabeans", "q033/Groundnuts", "q033/French beans", "q033/Tea", "q033/Coffee", "q033/Tobacco", "q033/Sugar cane", "q033/Pyrethrum", "q033/Mushrooms", "q033/Other (1)", "q033/Other (2)", "q010_group/q011_group/q034", "q010_group/q011_group/q035", "q010_group/q011_group/q036", "q010_group/q011_group/q037", "q010_group/q011_group/q038", "q010_group/q011_group/q039", "q010_group/q011_group/q040", "q040/Labour on other farms", "q040/Employee in private company/business", "q040/Own business", "q040/Remittances", "q040/Work for government or public institution", "q040/NGO", "q040/Renting out land", "q040/Renting out equipment", "q040/Other", "q010_group/q011_group/q040_oth", "q010_group/q011_group/q041", "q010_group/q011_group/q043", "q043/Nov 2018", "q043/Dec 2018", "q043/Jan 2019", "q043/Feb 2019", "q043/March 2019", "q043/April 2019", "q043/May 2019", "q043/June 2019", "q043/July 2019", "q043/Aug 2019", "q043/Sep 2019", "q043/Oct 2019", "q043/Never", "q010_group/q011_group/part_2/q044", "q010_group/q011_group/part_2/q045", "q010_group/q011_group/part_2/q046", "q010_group/q011_group/part_2/q047", "q047/Dec 2018", "q047/Jan 2019", "q047/Feb 2019", "q047/Mar 2019", "q047/Apr 2019", "q047/May 2019", "q047/Jun 2019", "q047/Jul 2019", "q047/Aug 2019", "q047/Sep 2019", "q047/Oct 2019", "q047/Nov 2019", "q010_group/q011_group/part_2/q048", "q048/Dec 2018", "q048/Jan 2019", "q048/Feb 2019", "q048/Mar 2019", "q048/Apr 2019", "q048/May 2019", "q048/Jun 2019", "q048/Jul 2019", "q048/Aug 2019", "q048/Sep 2019", "q048/Oct 2019", "q048/Nov 2019", "q048/Never", "q010_group/q011_group/part_2/a049", "q010_group/q011_group/part_2/a050", "q010_group/q011_group/part_2/q051", "q010_group/q011_group/part_2/q052", "q052/Bush beans", "q052/Climbing bean", "q052/Peas", "q052/Soybean", "q052/Sorghum", "q052/Maize", "q052/Wheat", "q052/Banana", "q052/Pyrethrum", "q052/Other", "q010_group/q011_group/part_2/q052_oth", "q010_group/q011_group/part_2/q055d", "q010_group/q011_group/part_2/q055", "q010_group/q011_group/part_2/q055b", "q010_group/q011_group/part_2/q055c", "q010_group/q011_group/part_2/q058", "q010_group/q011_group/part_2/a058_oth", "q010_group/q011_group/part_2/q059", "q059/Low soil fertility", "q059/Pests and diseases", "q059/Fluctuating market prices", "q059/Lack of extension services", "q059/Lack of access to credit", "q059/High cost of good quality seeds", "q059/High cost of fertilizers", "q059/High cost of pesticides", "q059/Fertilizers are unavailable", "q059/Pesticides are unavailable", "q059/Good seeds are unavailable", "q059/High post-harvest losses", "q059/Unpredictable rainfall", "q059/Labour availability", "q059/Land availability", "q059/Drought", "q059/Flood", "q059/Other (1)", "q059/Other (2)", "q010_group/q011_group/part_2/q059_oth1", "q010_group/q011_group/part_2/q059_oth2", "q010_group/q011_group/part_3/q060b", "q010_group/q011_group/part_3/gps_points", "q010_group/q011_group/part_3/_gps_points_precision", "q010_group/q011_group/part_3/q060", "q010_group/q011_group/part_3/q060c", "q010_group/q011_group/part_3/q061", "q010_group/q011_group/part_3/q062", "q010_group/q011_group/part_3/q062_oth", "q010_group/q011_group/part_3/q071", "q071/Cruza", "q071/Gikungu", "q071/Kazeneza", "q071/Kirundo", "q071/Mabondo", "q071/Victoria", "q071/Izihirwe", "q071/Kigega", "q071/Kinigi", "q071/Ndeze", "q071/Nkunganire", "q071/Twihaze", "q071/Peko", "q071/Gahinga", "q071/Rwangume", "q071/Kuruseke", "q071/Soropio", "q071/Rutuku", "q071/Rwashaki", "q071/Mabunoyabigega", "q071/Other", "q010_group/q011_group/part_3/q071_oth", "q010_group/q011_group/part_3/q072", "q010_group/q011_group/part_3/q073", "q073/Bush beans", "q073/Climbing bean", "q073/Peas", "q073/Soybean", "q073/Sorghum", "q073/Maize", "q073/Wheat", "q073/Banana", "q073/Pyrethrum", "q073/Other", "q010_group/q011_group/part_3/q073_oth", "q010_group/q011_group/part_3/q074", "q074/None = Fallow", "q074/Bush beans", "q074/Climbing bean", "q074/Peas", "q074/Soybean", "q074/Sorghum", "q074/Maize", "q074/Wheat", "q074/Banana", "q074/Pyrethrum", "q074/Potato", "q074/Other", "q010_group/q011_group/part_3/q074_oth", "q010_group/q011_group/part_3/q075", "q075/None = Fallow", "q075/Bush beans", "q075/Climbing bean", "q075/Peas", "q075/Soybean", "q075/Sorghum", "q075/Maize", "q075/Wheat", "q075/Banana", "q075/Pyrethrum", "q075/Potato", "q075/Other", "q010_group/q011_group/part_3/q075_oth", "q010_group/q011_group/part_3/q076", "q076/None =fallow", "q076/Bush beans", "q076/Climbing bean", "q076/Peas", "q076/Soybean", "q076/Sorghum", "q076/Maize", "q076/Wheat", "q076/Banana", "q076/Pyrethrum", "q076/Potato", "q076/I don't know", "q076/Other", "q010_group/q011_group/part_3/q076_oth", "q010_group/q011_group/part_3/q077", "q010_group/q011_group/part_3/q078", "q010_group/q011_group/part_3/q079", "q079/Hand hoe", "q079/Tiller", "q079/Tractor", "q079/Pitchfork (Majagu)", "q079/Spring (Rasoro)", "q079/Other", "q010_group/q011_group/part_3/q079_oth", "q010_group/q011_group/part_3/q080", "q010_group/q011_group/part_3/q081", "q081/From own farm", "q081/From neighbour", "q081/Non-certified seeds purchased at market", "q081/Certified seeds purchased from registered multiplier", "q081/Certified seeds purchased from RAB", "q081/Other", "q010_group/q011_group/part_3/q081_oth", "q010_group/q011_group/part_3/q082", "q082/Based on size", "q082/Based on best performing plants", "q082/No selection", "q082/Other", "q010_group/q011_group/part_3/q082_oth", "q010_group/q011_group/part_3/q083", "q010_group/q011_group/part_3/q083d", "q010_group/q011_group/part_3/q084d", "q010_group/q011_group/part_3/q085d", "q010_group/q011_group/part_3/q085dd", "q010_group/q011_group/part_3/q086d", "q010_group/q011_group/part_3/q0842", "q010_group/q011_group/part_3/q0843", "q010_group/q011_group/part_3/q085", "q010_group/q011_group/part_3/q0852", "q010_group/q011_group/part_3/q083b", "q010_group/q011_group/part_3/q084b", "q010_group/q011_group/part_3/q085b", "q010_group/q011_group/part_3/q085c", "q010_group/q011_group/part_3/q083c", "q010_group/q011_group/part_3/q084c", "q010_group/q011_group/part_3/q085ddd", "q010_group/q011_group/part_3/q085e", "q010_group/q011_group/part_3/q084c2", "q010_group/q011_group/part_3/q085ddd2", "q010_group/q011_group/part_3/q086", "q010_group/q011_group/part_3/q088", "q010_group/q011_group/part_3/q089", "q010_group/q011_group/part_3/q090", "q090/Pre-planting", "q090/Post-planting", "q010_group/q011_group/part_3/q091", "q091/Hoes", "q091/Spring (Rasoro)", "q091/Other", "q010_group/q011_group/part_3/q091_oth", "q010_group/q011_group/part_3/q092", "q010_group/q011_group/part_3/q093", "q010_group/q011_group/part_3/q094", "q010_group/q011_group/part_3/q094b", "q010_group/q011_group/part_3/q094c", "q010_group/q011_group/part_3/q095", "q095/Compost", "q095/Cattle manure", "q095/Chicken manure", "q095/Goat manure", "q095/Sheep manure", "q095/Pig manure", "q095/Other", "q010_group/q011_group/part_3/q095_oth", "q010_group/q011_group/part_3/select_organic_count", "q010_group/q011_group/part_3/q094d", "q010_group/q011_group/part_3/q095b", "q095b/Compost", "q095b/Cattle manure", "q095b/Chicken manure", "q095b/Goat manure", "q095b/Sheep manure", "q095b/Pig manure", "q095b/Other", "q010_group/q011_group/part_3/q095b_oth", "q010_group/q011_group/part_3/select_organic3_count", "q010_group/q011_group/part_3/q094e", "q010_group/q011_group/part_3/q095c", "q095c/Compost", "q095c/Cattle manure", "q095c/Chicken manure", "q095c/Goat manure", "q095c/Sheep manure", "q095c/Pig manure", "q095c/Other", "q010_group/q011_group/part_3/q095c_oth", "q010_group/q011_group/part_3/select_organic4_count", "q010_group/q011_group/part_3/q095dd", "q010_group/q011_group/part_3/q095d", "q095d/Compost", "q095d/Cattle manure", "q095d/Chicken manure", "q095d/Goat manure", "q095d/Sheep manure", "q095d/Pig manure", "q095d/Other", "q010_group/q011_group/part_3/q095d_oth", "q010_group/q011_group/part_3/select_organic5_count", "q010_group/q011_group/part_3/q100", "q010_group/q011_group/part_3/q102", "q010_group/q011_group/part_3/q102b", "q010_group/q011_group/part_3/q101", "q101/NPK 17:17:17", "q101/NPK 23:10:5 + S, Zn, Mg", "q101/NPK 15:9:20 + S, B, Zn, Mg, Mn", "q101/NPK (unknown composition)", "q101/DAP", "q101/Urea", "q101/TSP", "q101/KCl/MOP", "q101/Ammonium sulphate", "q101/40N + 55S", "q101/Other (1)", "q101/Other (2)", "q010_group/q011_group/part_3/q101_oth1", "q010_group/q011_group/part_3/q101_oth2", "q010_group/q011_group/part_3/select_fertilizers_count", "q010_group/q011_group/part_3/q108", "q010_group/q011_group/part_3/q101b", "q101b/NPK 17:17:17", "q101b/NPK 23:10:5 + S, Zn, Mg", "q101b/NPK 15:9:20 + S, B, Zn, Mg, Mn", "q101b/NPK (unknown composition)", "q101b/DAP", "q101b/Urea", "q101b/TSP", "q101b/KCl/MOP", "q101b/Ammonium sulphate", "q101b/40N + 55S", "q101b/Other (1)", "q101b/Other (2)", "q010_group/q011_group/part_3/q101b_oth1", "q010_group/q011_group/part_3/q101b_oth2", "q010_group/q011_group/part_3/select_fertilizers6_count", "q010_group/q011_group/part_3/q108b", "q010_group/q011_group/part_3/q101c", "q101c/NPK 17:17:17", "q101c/NPK 23:10:5 + S, Zn, Mg", "q101c/NPK 15:9:20 + S, B, Zn, Mg, Mn", "q101c/NPK (unknown composition)", "q101c/DAP", "q101c/Urea", "q101c/TSP", "q101c/KCl/MOP", "q101c/Ammonium sulphate", "q101c/40N + 55S", "q101c/Other (1)", "q101c/Other (2)", "q010_group/q011_group/part_3/q101c_oth1", "q010_group/q011_group/part_3/q101c_oth2", "q010_group/q011_group/part_3/select_fertilizers7_count", "q010_group/q011_group/part_3/q108c", "q010_group/q011_group/part_3/q101d", "q101d/NPK 17:17:17", "q101d/NPK 23:10:5 + S, Zn, Mg", "q101d/NPK 15:9:20 + S, B, Zn, Mg, Mn", "q101d/NPK (unknown composition)", "q101d/DAP", "q101d/Urea", "q101d/TSP", "q101d/KCl/MOP", "q101d/Ammonium sulphate", "q101d/40N + 55S", "q101d/Other (1)", "q101d/Other (2)", "q010_group/q011_group/part_3/q101d_oth1", "q010_group/q011_group/part_3/q101d_oth2", "q010_group/q011_group/part_3/select_fertilizers8_count", "q010_group/q011_group/part_3/q112b", "q010_group/q011_group/part_3/q112", "q112/Dithane", "q112/Ridomil", "q112/Nimbecidine", "q112/Pyrethrum 5 EW", "q112/Pyrethrum EWC", "q112/Lamda", "q112/Roket", "q112/Radiant", "q112/Magi magi", "q112/Dudu", "q112/Tioda", "q112/Mbolea yamajimaji", "q112/Other", "q010_group/q011_group/part_3/q112_oth", "q010_group/q011_group/part_3/select_pesticides_count", "q010_group/q011_group/part_3/q115", "q115/None", "q115/Late blight (Indwara y'imvura)", "q115/Bacterial wilt (Kirabiranya/cyumya)", "q115/Potato viruses (Imfunyarazi)", "q115/Other", "q010_group/q011_group/part_3/q115_oth", "q010_group/q011_group/part_3/q116", "q116/None", "q116/Leaf miner flies (Urunyo)", "q116/Potato tuber moth (Akanyugunyugu, Nyamwihina)", "q116/Aphids (Ubuhunduguru/ Inda zo mu birayi)", "q116/Nematodes (Amavunja y'ibirayi)", "q116/Other", "q010_group/q011_group/part_3/q116_oth", "q010_group/q011_group/part_3/q117", "q117/None", "q117/Gloves", "q117/Mouth mask", "q117/Boots", "q117/Workwear", "q117/Other", "q010_group/q011_group/part_3/q117_oth", "q010_group/q011_group/part_3/q063", "q010_group/q011_group/part_3/q064", "q064/Man/men from the HH", "q064/Woman/women from the HH", "q064/Hired labour", "q064/Unpaid help from outside the HH", "q010_group/q011_group/part_3/q065", "q065/Man/men from the HH", "q065/Woman/women from the HH", "q065/Hired labour", "q065/Unpaid help from outside the HH", "q010_group/q011_group/part_3/q066", "q066/Man/men from the HH", "q066/Woman/women from the HH", "q066/Hired labour", "q066/Unpaid help from outside the HH", "q010_group/q011_group/part_3/q067b", "q067b/Man/men from the HH", "q067b/Woman/women from the HH", "q067b/Hired labour", "q067b/Unpaid help from outside the HH", "q010_group/q011_group/part_3/q067", "q067/Man/men from the HH", "q067/Woman/women from the HH", "q067/Hired labour", "q067/Unpaid help from outside the HH", "q010_group/q011_group/part_3/q068", "q068/Man/men from the HH", "q068/Woman/women from the HH", "q068/Hired labour", "q068/Unpaid help from outside the HH", "q010_group/q011_group/part_3/q069", "q069/Man/men from the HH", "q069/Woman/women from the HH", "q069/Hired labour", "q069/Unpaid help from outside the HH", "q010_group/q011_group/part_3/q069b", "q069b/Man/men from the HH", "q069b/Woman/women from the HH", "q069b/Hired labour", "q069b/Unpaid help from outside the HH", "q010_group/q011_group/part_3/q118", "q010_group/q011_group/part_3/q118b", "q010_group/q011_group/part_3/q118c", "q010_group/q011_group/part_3/q118d", "q010_group/q011_group/part_3/q118e", "q010_group/q011_group/part_3/q118f", "q010_group/q011_group/part_3/q119", "q010_group/q011_group/part_3/q119b", "q010_group/q011_group/part_3/q120", "part_4/q121", "part_4/q122", "part_4/q123", "part_4/q124", "meta/instanceID", "_id", "_uuid", "_submission_time", "_index", "_parent_table_name", "_parent_index", "_tags", "_notes", "_version", "_duration", "_submitted_by", "_xform_id"


	d2 <- data.frame()
##r2: "q010_group/q011_group/part_3/select_fertilizers/cur_selection2", "q010_group/q011_group/part_3/select_fertilizers/cur_selection_lbl2", "q010_group/q011_group/part_3/select_fertilizers/q103", "q010_group/q011_group/part_3/select_fertilizers/q105", "_index", "_parent_table_name", "_parent_index"


	d3 <- data.frame()
##r3: "q010_group/q011_group/part_3/select_fertilizers6/cur_selection6", "q010_group/q011_group/part_3/select_fertilizers6/cur_selection_lbl6", "q010_group/q011_group/part_3/select_fertilizers6/q103d", "q010_group/q011_group/part_3/select_fertilizers6/q105d", "q010_group/q011_group/part_3/select_fertilizers6/q105d_oth", "_index", "_parent_table_name", "_parent_index"


	d4 <- data.frame()
##r4: "q010_group/q011_group/part_3/select_fertilizers7/cur_selection7", "q010_group/q011_group/part_3/select_fertilizers7/cur_selection_lbl7", "q010_group/q011_group/part_3/select_fertilizers7/q103b", "q010_group/q011_group/part_3/select_fertilizers7/q105b", "_index", "_parent_table_name", "_parent_index"


	d5 <- data.frame()
##r5: "q010_group/q011_group/part_3/select_pesticides/cur_selection9", "q010_group/q011_group/part_3/select_pesticides/cur_selection_lbl9", "q010_group/q011_group/part_3/select_pesticides/q113", "q010_group/q011_group/part_3/select_pesticides/q114", "_index", "_parent_table_name", "_parent_index"


	d6a <- data.frame()
##r6a: "abbreviature", "Variable name"


	d6b <- data.frame()
##r6b: "group", "name", "variable_name"


	d7a <- data.frame(
		rep = r7a[["repeat_count"]]
	)
##r7a: "type", "name", "label::English", "choice_filter", "hint", "constraint", "constraint_message", "calculation", "required", "appearance", "default", "relevant", "read_only", "image", "audio", "media::video"


	d7b <- data.frame()
##r7b: "list_name", "name", "label::English", "Q004"


	d7c <- data.frame()
##r7c: "form_title", "form_id", "default_language", "style"


## separate individual trials. For example trials in different locations or years. 
## do _not_ separate by treatments within a trial. For a survey, each row gets a unique trial_id
	d$trial_id <- as.character(as.integer(as.factor( ____ )))
	
## about the data (TRUE/FALSE)
	d$on_farm <- 
	d$is_survey <- 
	d$irrigated <-
	
## crop rotation. If available, add all crops, including "d$crop". Use an underscore for intercrops 
    d$crop_rotation <- "crop1;crop2;crop3_crop4"
	
## each site must have corresponding longitude and latitude
## if the raw data do not provide them you can estimate them from the location/adm data 
## see carobiner::geocode
	d$longitude <- 
	d$latitude <- 
# are the coordinates from the source (data/publication) or estimated by you?	
	d$geo_from_source <- TRUE/FALSE


## time can be year ("2023", four characters), year-month ("2023-07", 7 characters) or date ("2023-07-21", 10 characters).
## if dates come as character values, you can use as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- as.character(as.Date(   ))
	d$harvest_date  <- as.character(as.Date(    ))

### Fertilizers 
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- 
   d$K_fertilizer <-
   d$N_fertilizer <- 
   d$S_fertilizer <- 
   d$lime <- 
## normalize names 
   d$fertlizer_type <- 

## for legumes   
   d$inoculated <- TRUE or FALSE
   d$inoculant <- "name of inoculant"
   
### in general, add comments to your script if computations are
### based on information gleaned from metadata, a publication, 
### or when they are not immediately obvious for other reasons

### Yield

	yield <- r$yield_tonha * 1000
	#what plant part does yield refer to?
	d$yield_part <- "tubers"
	d$yield_moisture <- r$moisture * 100

#NOTE: yield is the _fresh weight_ production (kg/ha) of the "yield_part 
# Also record fresh and/or dry weight production of other organs (or "residue" or "total")
# if the data allow for that 

	d$fwy_storage <- r$yield_tonha * 1000
	d$dmy_storage <- (1-r$moisture) * r$yield_tonha * 1000
	d$dmy_totat <- r$dry_biomass
	
# all scripts must end like this
	carobiner::write_files(path, meta, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)

