# R script for "carob"


carob_script <- function(path) {

"Weeds are responsible for major crop losses worldwide but can provide beneficial agroecosystem services. This study aimed to elucidate how arbuscular mycorrhizal fungi (AMF) in weeds respond to host identity and conservation agricultural practices. The study was carried out at two locations in Southern Africa during off-season and in-season maize cultivation. Off-season AMF root colonisation, diversity indices and community composition significantly differed among weed species at both locations. Glomus sp. VTX00280 explains most of the AMF community differences. In-season, implementation of conventional tillage with mulching alone (CT + M) or together with crop rotation (CT + M + R) resulted in a 20% increase in AMF colonisation of the constantly occurring weed species, Bidens pilosa (BIDPI) and Richardia scabra (RCHSC), compared with conventional tillage plus rotations (CT + R). The diversity of AMF was highest under no-tillage plus mulching (NT + M). Off-season and in-season AMF structures of both BIDPI and RCHSC were not related, but 39% of the taxa were shared. Structural equation modelling showed a significant effect of the cropping system on weed AMF diversity parameters and weed and maize root colonisation, but no significant influence of weed root AMF traits and maize colonisation was detected on maize yield. This may be explained by the improvement in weed competitive ability, which may have offset the AMF-mediated benefits on yield. Our findings highlight that implementing M and CR to CT and NT positively affected weed AMF colonisation and diversity. The similarity between the off-season and in-season AMF composition of weeds supports the fact that weeds functionally host AMF during the non-crop period.
"

	uri <- "hdl:11529/10548831"
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT;SSSA",
		publication = "doi:10.1007/s00374-022-01678-1",
		project = NA,
		data_type = "experiment",
		response_vars = "yield",
		treatment_vars = "season;land_prep_method;crop_rotation;OM_used", 
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2024-06-24"
	)
	

	f <- ff[basename(ff) == "DTC UZ - IN-SEASON - OFF-SEASON.xlsx"]
	r <- carobiner::read.excel(f, sheet = "RawData")

	d <- data.frame(
		season=r$Period,
		location=r$Location,
		weed_species=r$Species,
		trial_id=as.character(r$Plot),
		rep=as.integer(r$Replicates),
		root_AMF=r$Arbuscules
	)

#treatment variables derived from system variable in dataset r
	
	d$land_prep_method[r$System %in% c("CT","CT+R","CT+M","CT+M+R")] <- "conventional"
	d$land_prep_method[r$System %in% c("NT","NT+R","NT+M","NT+M+R")] <- "none"
	d$land_prep_implement[r$System %in% c("CT","CT+R","CT+M","CT+M+R")] <- "hand-held hoe"
	d$land_prep_implement[r$System %in% c("NT","NT+R","NT+M","NT+M+R")] <- "none"
	d$crop_rotation[r$System %in% c("CT+R","CT+M+R","NT+R","NT+M+R")] <- "maize;cowpea"
	d$crop_rotation[r$System %in% c("NT","CT","NT+M","CT+M")] <- NA
	d$OM_used <- ifelse(r$System %in% c("CT+M","NT+M","CT+M+R","NT+M+R"), TRUE, FALSE)
	d$OM_amount <- ifelse(d$OM_used, 2500, 0)
	
#additional information extracted from publication
	
	d$plot_length <- 12
	d$plot_width <- 6
	d$plot_area <- 72
	d$plant_spacing <- 25
	d$row_spacing <- 90
	d$plant_density <- 44444
	
	#information on fertilizers was obtained from publication 
	d$P_fertilizer <- 10.1
	d$K_fertilizer <- 9.6
	d$N_fertilizer <- 57.6
	d$N_splits <-as.integer(2) 
	d$fertilizer_type <- "D-compound;AN"
	d$herbicide_used <- TRUE
	d$herbicide_product <- "glyphosate"
	
	#fixing location names
	d$country<-"Zimbabwe"
	d$longitude[d$location=="UZ"] <- 31.0528
	d$latitude[d$location=="UZ"] <- -17.7839
	d$longitude[d$location=="DTC"] <- 31.140
	d$latitude[d$location=="DTC"] <- -17.605
	d$location[d$location=="UZ"] <- "University of Zimbabwe"
	d$location[d$location=="DTC"] <- "Domboshawa Training Center"
	d$geo_from_source <- FALSE
	
	
	# about the data #####
## (TRUE/FALSE)
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$crop <- "maize"
	
	#d$planting_date <- as.character(as.Date(   ))
	#d$harvest_date  <- as.character(as.Date(    ))

	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d)
}

