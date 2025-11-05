# R script for "carob"


carob_script <- function(path) {
   
"
The data set has on-station advanced yield trials for 2005 to 2006 for Kachwekano, Ngetta, Serere, and Namulonge, and on-farm trials for Busia, Kabale, Mpigi, Nakasongola,Soroti and Wakiso districts in Uganda. Descriptions of the data are in a pdf file; the on-station data and on-farm data are in Excel format for Sweetpotato Breeding Protocol (manual) http://www.sweetpotatoknowledge.org/. The on-station and on-farm data on disease resistance, beta-carotene content, dry matter content, taste test and root weight from plots used to compute root yield, and biomass yield was the basis for official cultivar release of ‘NASPOT 7’, ‘NASPOT 8’, ‘NASPOT 9 O’, ‘NASPOT 10 O’, and ‘Dimbuka-Bukulula’ in 2007 in Uganda. The pdf file has details on background information, cultivar, site, material and methods, analysis, production package, cultivar maintenance and availability, and relevant tables and illustrations
"
   
   uri <- "doi:10.21223/P3/CGLAJW"
   group <- "varieties" 
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0, 
      data_organization ="CIP;NARO;NCSU", 
      publication ="doi:10.21273/HORTSCI.44.3.828", 
      project =NA, 
      data_type = "experiment",
      response_vars = "yield;yield_marketable",
      treatment_vars = "variety", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-09-24",
      notes="beta-carotene content data is missing and dry matter data is missing in some location"
   )
   
   f1 <- ff[grepl("on-station", basename(ff))]
   f2 <- ff[grepl("On-Farm", basename(ff))]
   
   ### Processing On-station trials 
   
   sheets <- c("Kachwe20042005", "Kachwe2005B", "Ngetta2004B2005A", "SAARI2005B2006A", "SAARI2005B2006B", 
               "SAARI2006A2007", "SAARI2006A", "NaCRRI2005B2006A", "Kachwe2006B2007A", "Ngetta2005B2006A")
   
   d1 <- lapply(sheets, \(i){
      
      r <- carobiner::read.excel(f1, sheet = i, na=c(".", "*"), fix_names = TRUE)
      names(r) <- r[grepl("REP|Rep", r[,1]),]
      r <- r[-(1:grep("REP|Rep", r[,1])),] 
      r <- r[,!is.na(colnames(r))]
      vr= grep("Code|Clone name|Name of clone", r[,c(1:length(names(r)))])
      ## dataset with variety
      rr <- r[c(grep("Code|Clone name|Name of clone",r[,vr[1]]):nrow(r)),]
      
      if(!is.null(r$Rep)){ cols <- grep("(t/ha)|t/ha", r[,c(1:length(names(r)))])[1]
         r <- r[-(grep("(t/ha)|t/ha", r[,cols])),]}
      
      names(r) <- gsub("Sweetpotaot", "Sweetpotato", names(r))
      names(r) <- gsub("Total root","Total root yield", names(r))
      names(r) <- gsub("Biom yield","Biomass", names(r))
      names(r) <- gsub("Biomass yield","Biomass", names(r))
      names(r) <- gsub("virius","virus", names(r))
      names(r) <- gsub("Vineyield","Vine yield", names(r))
      names(r) <- gsub("Entry","CODE_ENTRY", names(r))
      names(r) <- gsub("Rep","REP", names(r))
      names(r) <- gsub("ALT","Alternaria blight", names(r))
      names(r) <- gsub("Marketablle","Marketable root yield", names(r))
      if(is.null(r$SPVD)) r$SPVD <- r$`Sweetpotato virus disease (SPVD)`
      if(is.null(r$`DM yield t/ha`)) r$`DM yield t/ha` <- NA
      if(is.null(r$Biomass)) r$Biomass <- NA
      if(is.null(r$STANDS)) r$STANDS <- NA
      
         
      r <- r[!is.na(r$`Alternaria blight`),] ## drop empty rows in the data
      cond= "Ngetta2005B2006A|Kachwe2006B2007A|NaCRRI2005B2006A"
      d <- data.frame(
         rep= as.integer(r$REP),
         plant_density= (as.numeric(r$STANDS)/12)*10000,
         yield_marketable= ifelse(grepl(cond, i), as.numeric(r$`Marketable root yield`)*1000, (as.numeric(r$`WT.MKT RTS(KG)`)/18)*10000),
         yield= ifelse(grepl(cond, i), as.numeric(r$`Total root yield yield`)*1000, ((as.numeric(r$`WT. UN MKT RTS (KG)`) + as.numeric(r$`WT.MKT RTS(KG)`))/18)*10000) ,
         fwy_residue= ifelse(grepl(cond, i), as.numeric(r$`Vine yield`)*1000, (as.numeric((r$`WT.VINES(KG)`))/18)*10000),
         fwy_total= as.numeric(r$Biomass)*1000,
         dmy_total= as.numeric(r$`DM yield t/ha`)*1000, 
         on_farm= FALSE,
         location= i,                           
         trial_id= i,
         code= r$CODE_ENTRY,
         virus_severity= r$SPVD,
         diseases= "alternaria blight",
         disease_severity= r$`Alternaria blight`,
         severity_scale= "1-9"
      )
     
      ### Adding variety in the data
      
      rr <- rr[, vr] 
      names(rr) <- c("code","variety")
      rr <- rr[-1,][!is.na(rr[-1,]$code),]
      d <- merge(d, rr, by="code", all.x =TRUE)
   })
   
   d1 <- do.call(rbind, d1)
   
   
   ### Processing  On-Farm trials 
   
   d2 <- lapply(c("on-farm Nakasongola2006-2007", "On-farm Mpigi2006", "On-farm Mpigi 2006-2007"), \(j) { 
      r <- carobiner::read.excel(f2, sheet = j, fix_names = TRUE, na=c("*","."))
      names(r) <- r[grepl("REP|Rep", r[,1]),]
      r <- r[,!is.na(colnames(r))]
      r <- r[-(1:grep("REP|Rep", r[,1])),]
      
      
      if(!is.null(r$REP)) { 
		r <- r[!is.na(r$REP)& !is.na(r$STANDS) ,] ## drop empty rows in the data
		if (is.null(r$CODE_ENTRY)) r$CODE_ENTRY <- NA
		d <- data.frame(
			 rep= as.integer(r$REP),
			 plant_density= (as.numeric(r$STANDS)/12)*10000,
			 yield_marketable= (as.numeric(r$`WT.MKT RTS(KG)`)/12)*10000,
			 yield= ((as.numeric(r$`WT. UN MKT RTS (KG)`) + as.numeric(r$`WT.MKT RTS(KG)`))/18)*10000,
			 flesh_color= r$`FLESH COL.`,
			 fwy_residue= (as.numeric((r$`WT.VINES(KG)`))/12)*10000,
			 variety= r$`Name of clone`, 
			 on_farm= TRUE,
			 location= ifelse(grepl("Nakasongola", j),"Nakasongola", "Mpigi"),
			 trial_id= gsub("on-farm", "", j),
			 code= r$CODE_ENTRY,
			 virus_severity= r$SPVD,
			 diseases= "alternaria blight",
			 disease_severity= r$ALT,
			 severity_scale= "1-9",
			 planting_date= ifelse(grepl("Nakasongola", j), "2006-10-12",
							ifelse(grepl("Mpigi 2006-2007", j), "2006-11-07", "2006-04")),
			 
			 harvest_date= ifelse(grepl("Nakasongola", j), "2007-03-26",
						   ifelse(grepl("Mpigi 2006-2007", j), "2007-04-12", "2006-09-28")),
			 fwy_total= NA
		)
      } else { 
		  vr= grep("Code|Clone name", r[,c(1:length(names(r)))])
		  ## dataset with variety
		  rr <- r[c(grep("Code|Clone name", r[,vr[1]]):nrow(r)),]
		  cols <- grep("(t/ha)|t/ha", r[,c(1:length(names(r)))])[1]
		  r <- r[-(grep("(t/ha)|t/ha", r[, cols])),]
		  r <- r[!is.na(r$`Sweetpotato virus disease (SPVD)`) ,] ## drop empty rows in the data
		  d <- data.frame(
			rep= as.integer(r$Rep),
			code=  r$Code,
			virus_severity= r$`Sweetpotato virus disease (SPVD)`,
			diseases= "alternaria blight",
			disease_severity= r$Alternariablight,
			plant_density= NA,
			severity_scale= "1-9",
			fwy_residue= as.numeric(r$`Vine yield`)*1000,
			yield_marketable= as.numeric(r$`Markatable yield`)*1000,
			yield= as.numeric(r$`Total root yield`)*1000,
			fwy_total= as.numeric(r$`Biomass yield`)*1000,
			on_farm= TRUE,
			location= "Mpigi",
			planting_date= ifelse(grepl("Nakasongola", j), "2006-10-12",
							ifelse(grepl("Mpigi 2006-2007", j), "2006-11-07", "2006-04")),
			
			harvest_date= ifelse(grepl("Nakasongola", j), "2007-03-26",
						  ifelse(grepl("Mpigi 2006-2007", j), "2007-04-12", "2006-09-28")),
			
			trial_id= gsub("On-farm", "", j),
			flesh_color= NA
			 
		  )
		  ### Adding variety in the data
		  rr <- rr[, vr] 
		  names(rr) <- c("code", "variety")
		  rr <- rr[-1, ][!is.na(rr[-1, ]$code),]
		  d <- merge(d, rr, by="code", all.x =TRUE)
      }
   })
   
   d2 <- do.call(rbind, d2)
   
   d <- carobiner::bindr(d1, d2)
   d$code <- NULL
   d$plot_area <- 12 # net plot size in m2
   
   ### Adding planting date
   dte <- data.frame(
      location= c("Kachwe20042005", "Kachwe2005B", "Kachwe2006B2007A", "Ngetta2004B2005A", "Ngetta2005B2006A", "SAARI2005B2006A", "SAARI2005B2006B", "SAARI2006A2007", "SAARI2006A", "NaCRRI2005B2006A"),
      planting=c("2004-09-15", "2005-10-27", "2006-06-05", "2004-09-15", "2005-12-14","2005-09-01", "2005-09-01","2006-10-25", "2006-06-08", "2005-05-20"),
      harvest= c(NA, "2006-07-26", "2007-02-08", "2005-01-12", NA, "2006-02-28", "2006-04-28", NA, "2007-01-18", "2006-01-11")
   ) 
   
   d <- merge(d, dte, by= "location", all.x = TRUE)  
   
   d$planting_date[is.na(d$planting_date)] <- d$planting[is.na(d$planting_date)]
   d$harvest_date[is.na(d$harvest_date)] <- d$harvest[is.na(d$harvest_date)]
   d$planting <- d$harvest <- NULL
   
   ## Fixing the location 
   loc <- carobiner::fix_name(d$location)
   loc <- gsub("Kachwe20042005", "Kachwekano", loc)
   loc <- gsub("Kachwe2005B", "Kachwekano", loc)
   loc <- gsub("Kachwe2006B2007A", "Kachwekano", loc)
   loc <- gsub("Ngetta2004B2005A", "Ngetta", loc)
   loc <- gsub("Ngetta2005B2006A", "Ngetta", loc)
   loc <- gsub("SAARI2005B2006A", "Saari", loc)
   loc <- gsub("SAARI2005B2006B", "Saari", loc)
   loc <- gsub("SAARI2006A2007", "Saari", loc)
   loc <- gsub("SAARI2006A", "Saari", loc)
   loc <- gsub("NaCRRI2005B2006A", "NaCRRI", loc)
   d$location <- loc
   
   ## Fixing flesh color
   p <- carobiner::fix_name(d$flesh_color)
   p <- gsub("cr", 2, p)
   p <- gsub("p/o",6, p)
   p <- gsub("y|Y|o", NA, p)
   p <- gsub("c/o|cr/o|o/cr", 7, p)
   d$flesh_color <- as.numeric(p)
   
   cols <- c("white", "cream", "dark cream", "pale yellow", "dark yellow", "pale orange", "intermediate orange", "dark orange", "strongly 	pigmented with anthocyanins")
   d$flesh_color <- cols[d$flesh_color]
   
   d$variety <- gsub(" ", "", d$variety)
   d$country <- "Uganda"
   d$crop <- "sweetpotato"
   d$irrigated <- NA
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "roots"
   
   
   ## Adding longitude and latitude
   
   geo <- data.frame(
      location=c("Saari", "Mpigi", "Nakasongola", "Kachwekano", "Ngetta", "NaCRRI"),
      latitude=c(1.1445, 0.2275, 1.3119, -1.2558, 2.3113, 0.5205),
      longitude= c(34.2071, 32.3252, 32.4630, 29.9504, 32.9265, 32.6264)
   )
   
   d$geo_from_source <- FALSE
   
   d <- merge(d, geo, by= "location", all.x = TRUE)
   
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   d$trial_id <- trimws(d$trial_id)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files (path, meta, d)
}


