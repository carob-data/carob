# R script for "carob"


carob_script <- function(path) {
   
"The data set has on-station advanced yied trials for 2011 to 2012 for Kachwekano, Ngetta, Serere, and Namulonge, and 2012 on-farm trials for Isingiro, Buyende, Rakai, Oyam, and Kabale districts in in Uganda. The data is in CloneSelector format or Excel and format for Sweetpotato Breeding Protocol (manual) (http://www.sweetpotatoknowledge.org/). The on-station and on-farm data on disease resistance, beta-carotene content, dry matter content, taste test and root weght from plots used to compute root yield, and biomass yield was the basis for official cultivar release in Uganda."
   
   uri <- "doi:10.21223/P3/CYD0PI"
   group <- "varieties" 
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=9, 
      data_organization ="CIP;NARO;IFPRI;NCSU", 
      publication ="doi:10.21273/HORTSCI.51.3.291", 
      project =NA, 
      data_type = "experiment",
      response_vars = "yield;yield_marketable",
      treatment_vars = "variety", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-09-19",
      notes="beta-carotene and dry matter content data are not available"
   )
   
   f1 <- ff[grepl("0FSP", basename(ff))]
   f2 <- ff[grepl("OFSP", basename(ff))]
   f3 <- ff[grepl("On farm", basename(ff))]
   
   ### Processing 2011-2012 On-station trials 
  
      d1 <- lapply(c("Kachw", "Ngetta", "Serere", "NaCRRI") ,\(i){
         
         r <- carobiner::read.excel(f1, sheet = i, fix_names = TRUE, na=c("*"))
         names(r) <- r[grepl("REP", r[,1]),]
         r <- r[-(1:2),]
         
         vr= grep("ENTRY|CODE",r[,c(1:length(names(r)))])
         ## dataset with variety
         rr <- r[c(grep("ENTRY|CODE",r[,vr[1]]):nrow(r)),]
         r <- r[!is.na(r$REP),] ## drop empty rows in the data
         d <- data.frame(
            rep= as.integer(r$REP),
            plant_density= (as.numeric(r$STANDS)/18)*10000,
            yield_marketable= (as.numeric(r$`WT.MKT RTS(KG)`)/18)*10000,
            yield= ((as.numeric(r$`WT. UN MKT RTS (KG)`) + as.numeric(r$`WT.MKT RTS(KG)`))/18)*10000,
            on_farm= FALSE,
            planting_date= ifelse(grepl("NaCRRI", i), "2011-04-07",
                                  ifelse(grepl("Serere", i), "2011-05-25",
                                  ifelse(grepl("Ngetta", i),"2011-06-16", "2011"))),
            harvest_date= ifelse(grepl("NaCRRI", i), "2011-12-20",
                                  ifelse(grepl("Serere", i), "2012-01-05", NA)),
            location= i,                           
            trial_id= paste0("2011-2012", "-",i),
            code= r$CODE_ENTRY,
            virus_severity= r$SPVD,
            diseases= "alternaria blight",
            disease_severity= r$ALT,
            severity_scale= "1-9",
            flesh_color= as.numeric(r$`FLESH COL.`),
            fwy_residue= (as.numeric((r$`WT.VINES(KG)`))/18)*10000
         )
         
         ### Adding variety in the data
         rr <- rr[, vr] 
         names(rr) <- c("code","variety","code1")
         rr <- rr[-1,][!is.na(rr[-1,]$code),]
         rr$code1 <- NULL
         d <- merge(d, rr, by="code", all.x =TRUE)
      })
      
   d1 <- do.call(rbind, d1)
  
### Processing 2012 On-station trials 
   
   d2 <- lapply(c("Serere 12", "Ngetta 12", "NaCRRI") ,\(i){ 
      r <- carobiner::read.excel(f2, sheet = i, fix_names = TRUE, na=c("*"))
      names(r) <- r[grepl("REP", r[,1]),]
      r <- r[-(1:2),]
      
      vr= grep("ENTRY|CODE",r[,c(1:length(names(r)))])
      ## dataset with variety
      rr <- r[c(grep("ENTRY|CODE",r[,vr[1]]):nrow(r)),]
      r <- r[!is.na(r$REP)& !is.na(r$STANDS) ,] ## drop empty rows in the data
      d <- data.frame(
         rep= as.integer(r$REP),
         plant_density= (as.numeric(r$STANDS)/18)*10000,
         yield_marketable= (as.numeric(r$`WT.MKT RTS(KG)`)/18)*10000,
         yield= ((as.numeric(r$`WT. UN MKT RTS (KG)`) + as.numeric(r$`WT.MKT RTS(KG)`))/18)*10000,
         on_farm= FALSE,
         planting_date= ifelse(grepl("Serere 12", i), "2012-05-10",
                        ifelse(grepl("Ngetta 12", i), "2012-05-15", "2012-06-17")),
         harvest_date= ifelse(grepl("Serere 12", i), "2012-12-20",
                       ifelse(grepl("Ngetta 12", i), "2012-11-08", "2012-12-08")),                            
         location= gsub(" 12", "", i),
         trial_id= paste0("2012", "-",i),
         code= r$CODE_ENTRY,
         virus_severity= r$SPVD,
         diseases= "alternaria blight",
         disease_severity= r$ALT,
         severity_scale= "1-9",
         flesh_color= as.numeric(r$`FLESH COL.`),
         fwy_residue= (as.numeric((r$`WT.VINES(KG)`))/18)*10000
      )
      
      ### Adding variety in the data
      rr <- rr[, vr] 
      names(rr) <- c("code", "variety")
      rr <- rr[-1, ][!is.na(rr[-1, ]$code),]
      d <- merge(d, rr, by="code", all.x =TRUE)
   })
   
   d2 <- do.call(rbind, d2)
   
   ## Processing On-farm trial
   
   r <- carobiner::read.excel(f3, fix_names = TRUE)
   
   d3 <- data.frame(
      rep= as.integer(r$REP),
      plant_density= (as.numeric(r$STANDS)/18)*10000,
      yield_marketable= (as.numeric(r$WT.MKT.RTS.KG)/18)*10000,
      yield= ((as.numeric(r$WT.UN.MKT.RTS.KG) + as.numeric(r$WT.MKT.RTS.KG))/18)*10000,
      on_farm= TRUE,
      location= r$LOCATION,
      planting_date = "2012",
      trial_id= paste0("2012", "-", r$LOCATION),
      virus_severity= r$SPVD,
      diseases= "alternaria blight",
      disease_severity= r$ALT,
      severity_scale= "1-9",
      flesh_color= r$FLESH.COL,
      fwy_residue= (r$WT.VINES.KG/18)*10000
   )
   
   ## remove duplicate rows (3). 
   d3 <- unique(d3)
   
  d <- carobiner::bindr(d1, d2, d3)
  d$code <- NULL
  d$plot_area <- 18 # m2
  d$location[d$location=="Kachw"] <- "Kachwekano"
  
	cols <- c("white", "cream", "dark cream", "pale yellow", "dark yellow", "pale orange", "intermediate orange", "dark orange", "strongly 	pigmented with anthocyanins")
	d$flesh_color[d$flesh_color==0] <- NA
	d$flesh_color <- cols[d$flesh_color]
  
   d$variety <- gsub(" ", "", d$variety)
   d$country <- "Uganda"
   d$crop <- "sweetpotato"
   d$irrigated <- NA
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "roots"
   
   ## Adding longitude ,latitude and biomass from publication (On_farm trials)   
   geo <- data.frame(
      location=c("Kachwekano", "Ngetta", "Serere", "NaCRRI", "Isingiro", "Kabale", "Oyam", "Rakai", "Buyende"),
      latitude=c(-1.2558, 2.3113, 1.5011, 0.5205, -0.7801, -1.2558, 2.4270, -0.709811, 1.146247),
      longitude= c(29.9504, 32.9265, 33.5467, 32.6264, 30.7975, 29.9504, 31.4061, 31.40809, 33.16102),
      fwy_total= c(NA, NA, NA, NA, 31.1, 29.6, 20.6, 26.7, 33.1) ## mean total biomass for On-farm trials 
   )
   
   d$geo_from_source <- FALSE
   
   d <- merge(d, geo, by= "location", all.x = TRUE)
   
   ## Adding biomass from publication (On-station trials)
   
   d$fwy_total <- (ifelse(grepl("NASPOT1", d$variety), 67,
                  ifelse(grepl("NAS7/2006/1185", d$variety), 57.7,
                  ifelse(grepl("JEWEL(OP)/2005/6", d$variety), 58.3,
                  ifelse(grepl("NASPOT8", d$variety), 66.3,
                  ifelse(grepl("NKA", d$variety), 53.2,
                  ifelse(grepl("DIMBUKA", d$variety), 57.9, d$fwy_total)))))))*1000
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files (path, meta, d)
}


