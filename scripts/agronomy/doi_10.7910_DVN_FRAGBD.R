# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {

"
Unpacking the push-pull system: Assessing the contribution of companion crops along a gradient of landscape complexity

The push-pull system, a stimulo-deterrent cropping strategy consisting of intercropping cereals with herbaceous legumes and surrounded by fodder grasses, is presented as a promising crop diversification strategy for smallholder farmers in Africa as it may contribute to maize stemborer Busseola fusca (Fuller) suppression, while improving soil fertility and providing feed for livestock. The push-pull system has often been assessed at plot level and as a package (e.g., maize+Desmodium+Napier grass). However, it is unclear how the system performs in different landscape settings or when companion crops are changed to better meet household needs. Here we evaluate the potential of the push-pull system to suppress maize stemborer infestations in three landscapes in the Rift Valley region of Ethiopia along a gradient of landscape complexity. Within each landscape, experimental plots were established on four representative smallholder farms. At each farm we used a split-plot factorial design with main plots surrounded or not by Napier grass, and subplots consisting of sole maize, maize-bean or maize-Desmodium. We assessed stemborer infestation level and maize grain and stover yields during two years, as well as natural enemies abundance and egg predation at two maize development stages in the second year. In the simple landscape, which was dominated by maize, all treatments had high stemborer infestation levels, irrespective of within-field crop diversity; the presence of Napier grass was associated with higher predator abundance, while egg predation rates were the highest in the maize-bean intercrop. In the intermediate complexity landscape, subplots with sole maize had higher stemborer infestation levels compared to maize-bean or maize-Desmodium. In the complex landscape, infestation levels were low in all treatments. However, none of these effects led to significant differences in maize grain and stover yields among treatments in any of the landscapes. The benefits of the push-pull system accrued from the companion crops (bean, Desmodium and Napier), rather than from stemborer suppression per se. Our findings highlight the importance of the surrounding landscape in mediating the performance of the push-pull system, provide new insights on the contribution of the different components of push-pull system and can guide the design of ecologically intensive agroecosystems.
"

	uri <- "doi:10.7910/DVN/FRAGBD"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "intercrops;trap_crop_used",
		response_vars = "tunnelling_length;yield", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-09-25",
		carob_completion = 90,	
		carob_effort = 4
	)

	f <- ff[basename(ff) == "Kebede et al 2018b.xlsx"]

	r <- carobiner::read.excel(f, sheet="Infestation&Yield")


	d <- data.frame(
	  country="Ethiopia",
	  date=as.character(r$Year),
	  adm2=r$DistrictName,
	  crop="maize",
	  treatment=tolower(r$Napier),
	  intercrops=r$CroppingSystem,
	  pest_species="stem borer",
	  damage_type="tunnelling",
	  tunnelling_length=r$LengthTunnelling,
	  yield=r$DryGrainYield_T_ha*1000,
	  yield_part="grain",
	  yield_isfresh=FALSE,#as stated by the yield column
	  yield_moisture=NA
	)

	d$trap_crop_used <- r$Napier=="With Napier"
	d$trap_crop_type <- "napier grass"
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$planting_date <- NA
	d$harvest_date  <- NA
	d$geo_from_source <- TRUE #coordinates extracted from sheet "Land_Use"
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)

	geo <- data.frame(
	  adm2 =c("WondoGenet","Tula","HawassaZuria"),
	  latitude =c(7.036562,6.929622,7.050936),
	  longitude = c(38.571389,38.427193,38.338616)
	)

	d <- merge(d,geo,by="adm2", all.x = TRUE) 
	
	d$adm2[d$adm2=="WondoGenet"] <- "Wondo Genet"
	d$adm2[d$adm2=="HawassaZuria"] <- "Hawassa Zuria"
	
	d$trial_id <- paste(d$adm2,seq(nrow(d)), sep = "_")
	
	d$intercrops[d$intercrops=="MD"] <- "maize_desmodium"
	d$intercrops[d$intercrops=="M"] <- "maize"
	d$intercrops[d$intercrops=="MB"] <- "maize_common bean"
	
	carobiner::write_files(path, meta, d)
}

