# R script for "carob"

# ## ISSUES 

#RH: perhaps we should capture more management variables 


carob_script <- function(path) {
  
"Farmer participatory on-farm trials with CA technologies comparing with farmers’ practices (CT), were conducted in several fields in each community. Likewise, farmer-participatory alternative cropping systems trials were conducted comparing to existing systems and to find out suitable and more profitable cropping systems, prioritized to increase visibility and to avoid implementation and management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners. Project partners and staff coordinated monitoring and data acquisition. Where possible, collaborating farmers were selected by the community, and the project worked with existing farmer groups, with groups of both men and women farmers. "
  
	uri <- "hdl:11529/10547997"
	group <- "agronomy"

	ff	<- carobiner::get_data(uri, path, group)
 	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		project="Rabi (winter) crops-all nodes-Alternative cropping systems trial-Sunsari-Nepal",
		publication= NA,
		data_organization = "CIMMYT;NARC",
		data_type="on-farm experiment",
		carob_contributor="Fredy Chimire",
		carob_date="2023-10-31",
		modified_by="Robert Hijmans",
		last_modified="2023-11-04",
		treatment_vars=NA, 
		response_vars = "yield"
	)
	
	
	## is duplicate??: Maize-Rabi 2015-16-ACS-Saalbani-Sunsar.xlsx"
	sf <- c('Kidneybean-Rabi 2015-16-ACS-Saalbani-Sunsari.xlsx', 'Maize-Rabi 2015-16-ACS-Saalbani-Sunsari.xlsx', 'Maize-Rabi 2016-17-ACS-Bhokraha-Sunsari.xlsx', 'Maize-Rabi 2016-17-ACS-Saalbani-Sunsari.xlsx', 'Mustard-Rabi 2015-16-ACS-Saalbani-Sunsari.xlsx', 'Potato-Rabi 2016-17-ACS-Bhokraha-Sunsari.xlsx', 'Sunflower-Rabi 2016-17-ACS-Saalbani-Sunsari.xlsx')
	ff <- ff[basename(ff) %in% sf]

		
	get_raw_data <- function(f) {
		r1 <- carobiner::read.excel.hdr(f, sheet ="4- Stand counts & Phenology", skip=4, hdr=2)
		r2 <- carobiner::read.excel.hdr(f, sheet ="14 - Grain Harvest ", skip=4, hdr=2)
		r3 <- carobiner::read.excel.hdr(f, sheet ="6 - Fertilizer amounts ", skip=4, hdr=2)

		nms <- c("Site.No", "Tmnt", "Grain.yield.t.ha", "TGW.g", "Biomass.t.ha", "Straw.yield.t.ha")
		r2 <- r2[, nms]
		
		colnames(r3) <- gsub("Kg.ha_N.kg.ha", "N.kg.ha", colnames(r3))
		nms <- c("Site.No", "Tmnt", "N.kg.ha", "P2O5.kg.ha", "K2O.kg.ha", "Gypsum.kg.ha", "ZnSO4.kg.ha", "Boric.acid.kg.ha", grep("Product.used", names(r3), value=TRUE))
		r3 <- r3[, nms]

		r <- merge(r1, r2, by=c("Site.No", "Tmnt"))
		merge(r, r3, by=c("Site.No", "Tmnt"))
	}

	
	#### about the data #####

	process_data <- function(r) {
	
		d <- data.frame(trial_id = as.character(r$Trial.Code), season = r$Season,
			crop=tolower(r$Crop), variety= r$Variety, 
			treatment = r$Tmnt, 
			yield = r$Grain.yield.t.ha * 1000,	
			dmy_total = r$Biomass.t.ha * 1000,
			fwy_residue = r$Straw.yield.t.ha * 1000,
			N_fertilizer = r$N.kg.ha,
			P_fertilizer = r$P2O5.kg.ha / 2.29,
			K_fertilizer = r$K2O.kg.ha / 1.2051,
			B_fertilizer = r$Boric.acid.kg.ha * 0.1748,
			row_spacing = r$Row.spacing.cm,
			site = paste("site ", r$Site.No),
			country= "Nepal",
			adm2 = "Sunsari", # district provided in the excel
			S_fertilizer= 0,
			Zn_fertilizer= 0,
			lime =0, gypsum =0
		)

		d$irrigated <- TRUE
		d$fertilizer_type <- apply(r[, grep("Product.used", names(r), value=TRUE)], 1, 
			function(i) {
				i <- gsub("Urea.*", "urea", i[!is.na(i)])
				i <- gsub("MOP", "KCl", i[!is.na(i)])
				i <- gsub("Muriate", "KCl", i[!is.na(i)])
				paste(unique(i[!is.na(i)]), collapse="; ")
			})

		i <- grep("Date.of.seeding", names(r))
		d$planting_date = as.character(as.Date(r[,i]))
		i <- grep("Dat..of.harvest", names(r))
		d$harvest_date = as.character(as.Date(r[,i]))			

		if (is.character(d$row_spacing)) {
			d$row_spacing <- gsub("30-40", "35", d$row_spacing)
			d$row_spacing <- gsub("40-50", "45", d$row_spacing)
			d$row_spacing <- as.numeric(d$row_spacing)
		}

		i <- grep("Date.of.50.anthesis", names(r))
		if (length(i) != 0) {
			d$anthesis_date = as.character(as.Date(r[,i]))
		}
		d	
	}


	fun <- function(f) {
		#print(basename(f)); flush.console()
		r <- get_raw_data(f)
		d <- process_data(r)
		
		if (grepl("Bhokraha", f)) {
			d$latitude <- 26.592 # https://www.gps-coordinates.net/
			d$longitude <- 87.103
			d$location <- "Bhokraha"
		} else { #Saalbani please add lon/lat
			d$latitude <- NA
			d$longitude <- NA
			d$location <- "Saalbani"
		}
		d$geo_from_source <- FALSE
		crop <- tolower(strsplit(basename(f), "-")[[1]][1])
		d$crop <- gsub("kidneybean", "kidney bean", crop)
		if (d$crop[1] == "potato") {
			d$yield_part <- "tubers"
		} else if (d$crop[1] %in% c("maize", "kidney bean")) {
			d$yield_part <- "grain"
		} else { # mustard, sunflower
			d$yield_part <- "seed"
		}
		d
	}
	
	dd <- lapply(ff, fun)
	dd <- do.call(rbind, dd)

	dd$on_farm <- TRUE
	dd$is_survey <- FALSE
	dd$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, dd, path=path)
}
