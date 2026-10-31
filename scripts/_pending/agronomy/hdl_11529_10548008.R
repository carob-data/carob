# R script for "carob"


## The sites are not georeferenced. Need to ask the authors 


carob_script <- function(path) {
  
	"
	 Farmer participatory on-farm trials with CA technologies comparing with farmers’ practices (CT), were conducted in several fields in each community. Likewise, farmer-participatory validation trials were conducted comparing to existing practices and to find out suitable and more profitable crop production practices, prioritized to increase visibility and to avoid implementation and management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners. Project partners and staff coordinated monitoring and data acquisition. Where possible, collaborating farmers were selected by the community, and the project worked with existing farmer groups, with groups of both men and women farmers
	ff	<- carobiner::get_data(uri, path, group)
		"
	
	uri <- "hdl:11529/10548008"
	group <- "conservation_agriculture"

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		project="Rabi (winter) crops-all nodes- Validation trials -Rangpur-Bangladesh",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= NA,
		data_organization = "CIMMYT",
		data_type="on-farm experiment",
		carob_contributor="Fredy Chimire",
		carob_effort = NA,
		carob_date="2023-11-07"
	)
	
	
	

	proc <- function(f) {
	
		r1 <- carobiner::read.excel.hdr(f, sheet ="2 - Site information", skip=5, hdr=3)
	
		r1 <- carobiner::read.excel.hdr(f, sheet ="2 - Site information", skip=5, hdr=3)
		d1 <- data.frame(
			season=r1$Season, location=r1$Node, 
			trial_id=r1$Site.No.Unique.farmer.ID,
			treatment=r1$Tmnt.Short.abbreviation.as.mentioned.in.protocol,
			soil_type=r1$Soil.texture.sand.silt.clay.etc)
		
		r2 <- carobiner::read.excel.hdr(f, sheet ="4- Stand counts & Phenology", skip=4, hdr=2)
		d2 <- data.frame(
			treatment=r2$Tmnt, trial_id=r2$Site.No, 
			variety=r2$Variety,
			row_spacing=r2$Row.spacing.cm,
			crop=tolower(r2$Types.of.Trial),
			planting_date=as.character(r2$Date.of.seeding.dd.mm.yy),
			harvest_date=as.character(r2$Datw.of.harvest.dd.mm.yy))
		
		r3 <- carobiner::read.excel.hdr(f, sheet ="6 - Fertilizer amounts ", skip=4, hdr=2)
		
		d3 <- data.frame(
			treatment=r3$Tmnt, trial_id=r3$Site.No,
			N_fertilizer=r3$N.kg.ha, 
			P_fertilizer=r3$P2O5.kg.ha / 2.29, #!
			gypsum=r3$Gypsum.Kg.ha,
			Zn_fertilizer=r3$ZnSO4.kg.ha) 
		
		if (!is.null(r3$K2O.kg.ha)) {
			d3$K_fertilizer <- r3$K2O.kg.ha / 1.2051
		} else { # assuming this is also K2O
			d3$K_fertilizer=r3$K.kg.ha / 1.2051
		}
		
		d3$fertilizer_type <- apply(r3[, grep("Application_Product.used", names(r3))], 1, 
									\(i) paste(unique(i), collapse="; "))
		
		
		
		r4 <- carobiner::read.excel.hdr(f, sheet ="14 - Grain Harvest ", skip=4, hdr=2)	
		colnames(r4) <- gsub("Calculation_", "", colnames(r4))
		d4 <- data.frame(
			treatment=r4$Tmnt, trial_id=r4$Site.No, 
			yield=r4$Grain.yield.t.ha * 1000,
			fwy_residue=r4$Straw.yield.t.ha * 1000,
			dmy_total=r4$Biomass.t.ha * 1000)
		
		## merge all 
		dd <- merge(d1, d2, by=c("treatment", "trial_id"), all.x=TRUE)
		dd <- merge(dd, d3, by=c("treatment", "trial_id"), all.x=TRUE)
		dd <- merge(dd, d4, by=c("treatment", "trial_id"), all.x=TRUE)
		#dd <- unique(dd) if you need this, something is wrong
		dd
	}

	ff <- grep(".xlsx", ff, value=TRUE)
	fi <- basename(ff) == "Rabi 2016-17-validation trials-all nodes-Rangpur.xlsx"
## this file needs to be processed separately

	ff <- ff[-fi]

	d <- lapply(ff, proc)
	d <- do.call(rbind, d)
	
	
	#add columns
	
	d$country <- "Bangladesh"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$inoculated <- FALSE
	d$yield_part <- "grain" 
	d$trial_id <- as.character(d$trial_id)
	d$fertilizer_type <- gsub("MOP", "KCl", d$fertilizer_type)
	d$fertilizer_type <- gsub("Urea", "urea", d$fertilizer_type)
	d$fertilizer_type <- gsub("Gypsum", "gypsum", d$fertilizer_type)
	
# Zn and S? 
# 
	
	geo= data.frame(location=c("Kolkondo", "Lakkhiitari", "Durgapur" , "Borodargha", "Mohanpur"), 
					latitude=c(25.86245, 25.88404, 25.55461, 25.82308, 25.93235), 
					longitude=c(89.20563, 89.25188, 89.29195, 89.25810, 88.69496))
	d <- merge(d, geo, by="location", all.x = TRUE)	# do not use "T"
	
	##data type
	
	carobiner::write_files(meta, d, path=path)	
}
	
	
	
