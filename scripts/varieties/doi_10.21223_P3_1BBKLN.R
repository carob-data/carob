# R script for "carob"


carob_script <- function(path) {
   
"The data set for the “NASPOT 11” HortScience release paper has on-farm participatory planta breeding trials conducted in Mpigi, Luwero, Soroti, Kiboga, and Kabale districts and on-station trials conducted at Namulonge, Kachwekano, Ngetta, and Serere between 2005 and 2008 leading to official cultivar release of “NASPOT 11” in Uganda in April 2010. Descriptions of the data are in a pdf file (Submission to the Variety Release Committee for Release of Sweetpotato Varieties). The on-farm and on-station data are in Excel format for Sweetpotato Breeding Protocol (manual) http://www.sweetpotatoknowledge.org/). The on-farm and on-station data on disease resistance, dry matter content, taste test and root weight from plots used to compute root yield, and biomass yield was the basis for official cultivar release of ‘NASPOT 11’, in 2010 in Uganda. The pdf file has details on background information, pedigree, cultivar description, site description, materials and methods, analysis, production package, cultivar maintenance and availability, and relevant tables and illustrations. illustrations"
  
   uri <- "doi:10.21223/P3/1BBKLN"
   group <- "varieties" 
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1, 
      data_organization ="CIP;NARO;UGRE;NCSU", 
      publication ="doi:10.21273/HORTSCI.46.2.317",
      project =NA, 
      data_type = "experiment",
      response_vars = "yield;yield_marketable;fwy_residue",
      treatment_vars = "variety", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-10-04",
      notes= "Dry matter data are missing",
      design= "RCBD"
   )
   
   f1 <- ff[basename(ff)=="2 PPB-Data-NASPOT11-On-farm.xlsx"]
   f2 <- ff[basename(ff)=="3 PPB-Data-NASPOT11-On-station.xlsx"]
   
   ### Processing On farm data
   sheets= c("1Mpigi16Aug2005.3Sept2006", "3Luwero 3Mar.1July2006", "4PPB Mpigi 16Aug.3Sept2006", "5Soroti 5No2007-2April2008", "6PPB Kiboga 10May2006.3Jan2007", "7Kabale 23May08.11Mar2009")
  
   d1 <- lapply( sheets ,\(i){
      
      r <- carobiner::read.excel(f1, sheet = i, na=c("rts", "mkt rts"), fix_names = TRUE)
      names(r) <- r[grep("Rep", r[,1])[1],]
      names(r) <- gsub("SPVD","SP", names(r))
      names(r) <- gsub("Wt Vines|Wt vines","Vines", names(r))
      r <- r[-grep("Rep", r[,1]),] 
      r <- r[,!is.na(colnames(r))]
      c <- r[grep("Wt", names(r))] 
      colnames(c) <- c("MKt","UMKt")
      vr <- grep("CLONE|CODE|code|Code", r[,names(r)])
      r1 <- r
      r <- cbind(r, c)[-c(1:2),]
      r <- r[!is.na(r$Rep),]
      if(!grepl("1Mpigi16Aug2005|3Luwero 3Mar", i)) { r <- r[-c(grep("Post harvest|Farmer|CLONE|Code", r[,1])[1]: nrow(r)),]
      r <- r[!is.na(r$Est)  & !is.na(r$MKt) & !is.na(r$UMKt),]}
      if(grepl("3Luwero 3Mar", i))  r <- r[!is.na(r$Est),]
      if(is.null(r$Flesh)) r$Flesh <- NA
      
      d <- data.frame(
         rep= suppressWarnings(as.integer(r$Rep)),
         plant_density= (as.numeric(r$Stand)/30)*10000,
         yield_marketable= (as.numeric(r$MKt) /30)*10000,
         yield= (as.numeric(r$MKt)/30)*10000 + (as.numeric(r$UMKt)/30)*10000,
         fwy_residue= (as.numeric(r$Vine)/30)*10000,
         variety= r$Entry,
         flesh_color= as.numeric(r$Flesh),
         on_farm= TRUE,
         location= ifelse(grepl("Mpigi", i), "Mpigi",
                          ifelse(grepl("Luwero", i), "Luwero",
                          ifelse(grepl("Soroti",i), "Soroti",
                          ifelse(grepl("Kiboga", i), "Kiboga", "Kabale")))),
         trial_id= i,
         planting_date= ifelse(grepl("16Aug2005", i), "2005-08-16",
                        ifelse(grepl("3Mar.1July2006", i), "2006-03-03",
                        ifelse(grepl("16Aug.3Sept2006", i), "2006-08-16",
                        ifelse(grepl("5No2007-2April2008", i), "2007-11-05",
                        ifelse(grepl("10May2006.3Jan2007", i), "2006-05-10", "2008-05-23"))))),
         virus_severity= r$SP,
         diseases= "alternaria blight",
         disease_severity= r$Alt,
         severity_scale= "1-9"
         )
      
      if (grepl("6PPB Kiboga", i)){
         rr <- r1[c(grep("CLONE|Code", r1[,vr[1]]): nrow(r1)),]
         rr <- rr[, vr]
         names(rr) <- rr[1,]
         rr <- rr[-1,]
         colnames(rr) <- c("variety", "code")
         d$variety <- rr$variety[as.numeric(d$variety)]
      }
      
      if (grepl("7Kabale", i)){
         rr <- r1[c(grep("CLONE|Code", r1[,vr[1]]): nrow(r1)),]
         rr <- rr[, vr]
         names(rr) <- rr[1,]
         rr <- rr[-1,]
         colnames(rr) <- c("code", "variety") 
         rr <- rr[-c(grep("Palatability taste-Kabale", rr[,1]):nrow(rr)),]
         rr <- rr[!is.na(rr$code),] 
         
         d$variety <- rr$variety[as.numeric(d$variety)]
         
      }
      d
      })
   d1 <- do.call(rbind, d1)   
   
   ## Processing On station 
   
   sheets1= c("8Onstation NaCRRI 6April2006", "9NaCRRI 16Apr2008.19Jan2009", "10Kachwekano 19Dec07.4Sept2008", "11Kachwekano 21May008.11Mar2009", "12Ngetta13Oct.11Apr2007", "13Ngetta 18Jul.1Dec2008",
             "14Serere 8June2006.19Jan2007","15Serere 17June.3Dec2008")
   
   d2 <- lapply(sheets1 ,\(i){
      
      r <- carobiner::read.excel(f2, sheet = i, fix_names = TRUE)
      names(r) <- r[grep("Rep", r[,1])[1],]
      names(r) <- gsub("Wt Vines|Wt vines","Vines", names(r))
      r <- r[-grep("Rep", r[,1]),] 
      r <- r[,!is.na(colnames(r))]
      c <- r[grep("Wt", names(r))] 
      colnames(c) <- c("MKt","UMKt")
      vr <- grep("code|Code|Clone", r[,names(r)])
      rr <- r[c(grep("code|Code", r[,vr[1]]): nrow(r)),]
      r <- cbind(r, c)[-c(1:2),]
      r <- r[-c(grep("code|Code", r[,vr[1]])[1]: nrow(r)),]
      r <- r[!is.na(r$Rep)  & !is.na(r$MKt),]
      
      if(is.null(r$Flesh)) r$Flesh <- NA
      
      d <- data.frame(
         rep= suppressWarnings(as.integer(r$Rep)),
         plant_density= (as.numeric(r$Stand)/30)*10000,
         yield_marketable= (as.numeric(r$MKt) /30)*10000,
         yield= (as.numeric(r$MKt)/30)*10000 + (as.numeric(r$UMKt)/30)*10000,
         fwy_residue= (as.numeric(r$Vine)/30)*10000,
         variety= r$Entry,
         flesh_color= as.numeric(r$Flesh),
         on_farm= FALSE,
         location= ifelse(grepl("NaCRRI", i), "NaCRRI",
                   ifelse(grepl("Kachwekano", i), "Kachwekano",
                   ifelse(grepl("Ngetta",i), "Ngetta","Serere"))),  
         
         planting_date= ifelse(grepl("6April2006", i), "2006-04-06",
                        ifelse(grepl("16Apr2008.19Jan2009", i), "2008-03-16",
                        ifelse(grepl("19Dec07.4Sept2008", i), "2007-12-19",
                        ifelse(grepl("21May008.11Mar2009", i), "2008-05-21",
                        ifelse(grepl("3Oct.11Apr2007", i), "2007-10-30",
                        ifelse(grepl("18Jul.1Dec2008", i), "2008-07-18",
                        ifelse(grepl("8June2006.19Jan2007", i), "2006-06-08", "2007-06-17"))))))),
         trial_id= i,
         virus_severity= r$SPVD,
         diseases= "alternaria blight",
         disease_severity= r$Alt,
         severity_scale= "1-9"
      )
      
      
   ### Adding variety      
         rr <- rr[, vr]
         names(rr) <- rr[1,]
         rr <- rr[-1,]
         colnames(rr) <- c("code", "variety")
         d$variety <- rr$variety[as.numeric(d$variety)]
         d
      
   })
   
   
   d2 <- do.call(rbind, d2)   
   
  d  <- carobiner::bindr(d1, d2)
  
  ## Fixing flesh color
  cols <- c("white", "cream", "dark cream", "pale yellow", "dark yellow", "pale orange", "intermediate orange", "dark orange", "strongly 	pigmented with anthocyanins")
  d$flesh_color <- cols[d$flesh_color]
  
   d$country <- "Uganda"
   d$crop <- "sweetpotato"
   d$on_farm <- TRUE
   plot_area <- 30 #m2 gross plot
   d$irrigated <- NA
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "roots"
   
   ### Adding longitude and latitude 
   
   geo <- data.frame(
      location= c("Mpigi", "Luwero", "Soroti", "Kiboga", "Kabale", "NaCRRI", "Kachwekano", "Ngetta", "Serere"),
      latitude= c(0.22732, 0.83981, 1.713568, 0.920494, -1.241701, 0.534138, -1.25353, 2.30066, 1.49822),
      longitude= c(32.32546, 32.49769, 33.60588, 31.77022, 29.98544, 32.59839, 29.9420, 32.9330, 33.54958)
   )
   
   d$geo_from_source <- FALSE
   
   d <- merge(d, geo, by="location", all.x = TRUE)
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files (path, meta, d)
}


