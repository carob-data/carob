# R script for "carob"


carob_script <- function(path) {
  
"Farmer participatory on-farm trials with CA technologies comparing with farmers’ practices (CT), were conducted in several fields in each community. Likewise, farmer-participatory alternative cropping systems trials were conducted comparing to existing systems and to find out suitable and more profitable cropping systems, prioritized to increase visibility and to avoid implementation and management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners. Project partners and staff coordinated monitoring and data acquisition. Where possible, collaborating farmers were selected by the community, and the project worked with existing farmer groups, with groups of both men and women farmers."
  
  
	uri <- "hdl:11529/10547966"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		project=NA, 
		publication= "doi:10.1016/j.fcr.2019.04.005", 
		data_organization = "CIMMYT", 
		data_type="on-farm experiment", 
		carob_contributor="Mitchelle Njukuya", 
		carob_date="2024-04-25",
		response_vars = "yield",
		treatment_vars = "land_prep_method"
	)
  
 
    ## process all Wheat and Maize -Purnea files  
	proc_data <- function(f) {
   
		suppressWarnings(r1 <- carobiner::read.excel.hdr(f, sheet ="4- Stand counts & Phenology", skip=4, hdr=3))
		colnames(r1) <- gsub("Date.of.harvest.dd.mm.yy", "Datw.of.harvest.dd.mm.yy", colnames(r1))
		colnames(r1) <- gsub("X.6", "Node", colnames(r1))
		
		d1 <- data.frame(
			treatment=r1$Tmnt, 
			trial_id=paste0(r1$Node, "_", r1$Site.No), 
			location=r1$Node,
			variety=r1$Variety,
			row_spacing=r1$Row.spacing.cm,
			crop=tolower(r1$Crop),
			planting_date=as.character(r1$Date.of.seeding.dd.mm.yy),
			harvest_date=as.character(r1$Datw.of.harvest.dd.mm.yy)
		)
		if (d1$location[1] == "Rabi maize") {
			d1$location <- r1$Site.No.Unique.farmer.ID
		}
		
		r2 <- carobiner::read.excel.hdr(f, sheet ="6 - Fertilizer amounts ", skip=4, hdr=3)
		
		colnames(r2) <- gsub("P.kg.ha|P.2O5.kg.ha", "P2O5.kg.ha", colnames(r2))
		colnames(r2) <- gsub("K.kg.ha", "K2O.kg.ha", colnames(r2))
		colnames(r2) <- gsub("Zn.kg.ha", "ZnSO4.kg.ha", colnames(r2))
		
		d2 <- data.frame(
			treatment=r2$Tmnt,
			location=r2$Node,
			trial_id=paste0(r2$Node, "_", r2$Site.No),
			N_fertilizer=r2$N.kg.ha, 
			P_fertilizer=r2$P2O5.kg.ha / 2.29,
			K_fertilizer=r2$K2O.kg.ha /  1.2051,
			Zn_fertilizer=r2$ZnSO4.kg.ha
		) 
		
		d2$fertilizer_type <- apply(r2[, grep("Application_Product.used", names(r2))], 1, 
									\(i) paste(unique(i), collapse=";"))
		

		string0 <- readxl::excel_sheets(f)[grep("Grain Harvest", readxl::excel_sheets(f))]
		
		r3 <- carobiner::read.excel.hdr(f, sheet = string0, skip=4, hdr=3)	
		
		colnames(r3) <- gsub("Gy.t.ha|Grain.Yield.t.Ha|Calculation_Grain.Yield.t.ha|Grain.Yield.t.ha", 
							"Grain.yield.t.ha", colnames(r3))
		colnames(r3) <- gsub("Biomass.t.ha", "Biomass", colnames(r3))
		
		if (is.null(r3$Starw.t.ha)) r3$Starw.t.ha <- NA
		d3 <- data.frame(
			treatment=r3$Tmnt,
			location=r3$Node,
			trial_id=paste0(r3$Node, "_", r3$Site.No), 
			yield=r3$Grain.yield.t.ha * 1000,
			fwy_residue=r3$Starw.t.ha * 1000,
			dmy_total=r3$Biomass * 1000
		)
		
		## merge all 
		dd <- merge(d1, d2, by=c("treatment", "trial_id", "location"), all.x=TRUE)
		dd <- merge(dd, d3, by=c("treatment", "trial_id", "location"), all.x=TRUE) 
	}

	fx <- grep("xlsx$", ff, value=TRUE)
	dx <- lapply(fx, proc_data)
	d <- do.call(rbind, dx)
	
	i <- which(substr(d$planting_date, 1, 1) == 4)
	d$planting_date[i] <- as.character(as.Date("1899-12-31") + as.integer(d$planting_date[i]))
	d$planting_date[d$planting_date=="0"] <- NA
	i <- which(substr(d$harvest_date, 1, 1) == 4)
	d$harvest_date[i] <- as.character(as.Date("1899-12-31") + as.integer(d$harvest_date[i]))
	d$harvest_date[d$harvest_date=="0"] <- NA
	d$harvest_date[d$harvest_date=="N/A"] <- NA
	d$harvest_date[d$harvest_date=="14-04-16"] <- "2016-04-14"
	
	
	treatcode = c("CTW", "ZTW", "CTTPR-CTM", "CTTPR-CTW", "CTTPR-ZTM", "CTTPR-ZTW", "FP", 
	              "UPTR-ZTM", "UPTR-ZTW", "ZT", "ZTDSR-ZTM", "ZTDSR-ZTW", "ZTTPR_ZTW", "CTM", 
	              "ZTM", "UPTPR-ZTM", "UPTPR-ZTW", "CTPR-ZTW", "CTPTR-CTW", "CTPTR-ZTW", "CTPTR ZCTW", "UPPTR-ZTW")
	
	treatname = c("Conventional tillage wheat", "Zero tillage wheat", "Conservation tillage transplanted puddle riced_CTM", 
	              "Conservation tillage transplanted puddled rice_Conventional tillage wheat", 
	              "Conservation tillage transplanted puddled rice_Zero tillage maize", "Conservation tillage transplanted puddled rice_Zero tillage wheat", 
	              "FP", "UPTR_Zero tillage maize", "Zero tillage", "ZTDSR_Zero tillage maize", "ZTDSR_Zero tillage wheat", "ZTTPR_Zero tillage wheat", "CTM", 
	              "Zero tillage maize", "Unpuddled transplanted rice_Zero tillage maize", "UPTPR_Zero tillage wheat", "CTPR_Zero tillage wheat", 
	              "CTPTR_Conventional tillage wheat", "CTPTR_Zero tillage wheat", "CTPTR ZCTW", "UPPTR_Zero tillage wheat")
	
	d$treatment <- treatname[match(d$treatment,treatcode)]
  
	d$country <- "India"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	d$yield_part <- "grain" 
	d$fertilizer_type <- gsub("MOP|urea+MOP|Mop|mop", "KCl", d$fertilizer_type)
	d$fertilizer_type <- gsub("Urea|UREA", "urea", d$fertilizer_type)
	d$fertilizer_type <- gsub("Zinc sulphate", "ZnSO4", d$fertilizer_type)
	d$fertilizer_type <- gsub("\\+", ";", d$fertilizer_type)
	d$crop <- gsub("rabi maize", "maize", d$crop)
	d$location <- gsub("Takapati","Tikapatti", d$location)
	d$location <- gsub("Dogachi","Dogachhi", d$location)
  
	geo <- data.frame(
		location=c("Puranigarel", "Dogachhi", "Katheli" , "Tikapatti", "Udaynagar"), 
        latitude=c(25.7711, 24.6722, 23.9759, 26.0944, 22.4917), 
        longitude=c(87.4822, 88.4500, 78.3306, 86.2764, 76.2655)
	)
	d$geo_from_source <- FALSE
  
	d <- merge(d, geo, by="location", all.x = TRUE)  
	d <- d[!is.na(d$yield), ]
	d <- d[d$yield > 0, ]
	
	d$fertilizer_type <- gsub(";NA|NA", "", d$fertilizer_type)
	d$fertilizer_type[d$fertilizer_type == ""] <- "unknown"
	d <- unique(d)
  
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path)	
}


