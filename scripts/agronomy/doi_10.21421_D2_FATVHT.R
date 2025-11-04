# R script for "carob"


carob_script <- function(path) {

"Nitrogen (N) is an essential nutrient for sorghum growth and development but often becomes limiting due to low availability and loss. The effects of N fertilization on water use efficiency (WUE) and physiological and yield traits of sorghum were investigated in two locations over two cropping seasons (2014 and 2015) in the Sudan Savanna zone of Nigeria. Three sorghum varieties were evaluated under six (6) N-levels (0, 20, 40, 60, 80, and 100 kg ha−1) at a constant phosphorus and potassium level of 30 kg ha−1. Results showed that N increased grain yield by 35–64% at the Bayero University Kano (BUK) and 23–78% at Minjibir. The highest mean grain yield in the N-fertilizer treatments (2709 kg ha−1 and 1852 kg ha−1 at BUK and Minjibir, resp.) was recorded at 80 kg N ha−1. ICSV400 produced the highest mean grain yields (2677 kg ha−1 and 1848 kg ha−1 at BUK and Minjibir, resp.). Significant differences were observed among the N-levels as well as among the sorghum varieties for estimated water use efficiency (WUE).To review the nutrient needs, especially N of some of selected sorghum varieties and their water use efficiency on marginal land	"

	uri <- "doi:10.21421/D2/FATVHT"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		publication="doi:10.1155/2018/7676058",
		carob_contributor="Eduardo Garcia Bendito",
		carob_date="2021-06-29",
		data_type="experiment",
		data_organization="ICRISAT",
		project=NA,
		response_vars= "yield",
		treatment_vars="N_fertilizer"
 	)

	f <- ff[basename(ff) == "Data file of Sorghum N trial Kano Nigeria.xlsx"]
	r <- carobiner::read.excel(f)
	
	d <- data.frame(
		country = "Nigeria",
		adm1 = "Kano",
		adm2 = ifelse(r$Location == "BUK", "Gezawa", "Minjibir"),
		location = r$Location,
		trial_id = paste0(r$Location, "_", r$Year),
		latitude = ifelse(r$Location == "BUK", 8.5922, 8.5978),
		longitude = ifelse(r$Location == "BUK", 12.0034, 12.1733),
		geo_from_source = FALSE,
		# As reported in the associated publication:
		planting_date = ifelse(r$Location == "BUK" & r$Year == 2014, "2014-07-19",
                  ifelse(r$Location == "BUK" & r$Year == 2015, "2014-07-20",
                  ifelse(r$Location == "Minjibir" & r$Year == 2014, "2014-07-07", "2014-07-04"))), 
											  
		on_farm = FALSE,
		is_survey = FALSE,
		treatment = paste0("N", r$Nitrogen, "-P30-K30"),
		rep = as.integer(r$`Replication umber`),
		crop = "sorghum",
		variety = r$Sorghum,
		
		yield = r$`Grain yield`,
		fwy_residue = r$`Stalk yield`,
		seed_weight = r$GW_1000grnM_g,
# Not reported in the associated publication	
		fertilizer_type = "unknown" ,
# As reported in the associated publication	
		N_splits = 2L,
		N_fertilizer = r$Nitrogen,
# As reported in the associated publication. Converting P2O5 to P-elemental
		P_fertilizer = 30/2.29 ,
# As reported in the associated publication Converting K2O to K-elemental	
		K_fertilizer = 30/1.2051, 
		OM_used = FALSE,
# As reported in the associated publication	
		plant_spacing = 30, 
		row_spacing = 75 ,
		yield_part = "grain",
		harvest_days = r$`Days to Maturity`
	)

	d$harvest_date <- as.character(as.Date(d$planting_date) + d$harvest_days)
	d$irrigated <- FALSE
	d$location[d$location == "BUK"] <- "Bayero University, Kano"
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path)

}
