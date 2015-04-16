#### Sally Archibald
### this file contains all the functions needed to download the fire data from the ftp site, extract it, and calculate fire intervals. These scripts are used together with the 
#"getFRIdata_GBIF_finalCodeForCHPC" which actually runs the functions and produces the data

ftpsite <- "ftp://user:burnt_data@ba1.geog.umd.edu/Collection51/TIFF/" #ftp://ba1.geog.umd.edu/Collection51/TIFF/
Downloaddir <- ("E:\\ModisData\\")
#Downloaddir <- "C:\\Data_spatial\\Modis_Roy_BA\\GlobalAnalysis\\BAdat_bywindow_new\\ba1.geog.umd.edu\\Collection51\\TIFF"

"DownloadFireDat.s" <- function(ftpsite, Downloaddir, whatwins = "all", whatyears = "getyears"){
  # This aim of this script is to download the burned area  data from the .Ftp site.
  # is is a simplified version of the code to be found at "MODIS_downloadBurnedAreafromFTP.r"
  # it requires the user to input the ftpsite, the download directory, the windows required (default is all), and the years for which data are requierd (default is all years)
  require(RCurl)
  require(R.utils)
  if(whatwins == "all") windows <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20","21","22","23","24") 
  if(whatwins != "all") windows <- whatwins
  for(i in 1:length(windows)){
    win <- paste("Win", windows[i], sep = "")
    ### create the folder for the window if necessary
    if (file.exists(file.path(Downloaddir, win))== F){
      dir.create(file.path(Downloaddir, win))    
    }  
    if(whatyears == "getyears"){    # IF you want to get ALL the years
      years <- getURL(paste(ftpsite, win, "/", sep = ""),ftp.use.epsv = FALSE, dirlistonly = TRUE)
      years <- strsplit(years, "\r*\n")[[1]]
    }
    if(whatyears != "getyears") years <- whatyears
    #use getURL to download the requisite data
    for(j in 1:length(years)){
      ### test this for just one folder
      inputURL <- paste(ftpsite, win, "/", years[j], "/", sep = "")
      filenames <- try(getURL(inputURL, ftp.use.epsv = FALSE, dirlistonly = TRUE))
      files <- strsplit(filenames, "\r*\n")[[1]]
      files <- files[grep("burndate", files)]   # use this code if you only want to download one type of burned area data
      # to make sure you dont download all over again
      currentfiles <- list.files(file.path(Downloaddir, win))
      files <- setdiff(files, currentfiles)
      filenames = paste(inputURL, files, sep = "") 
      if(length(files)>0){
        for(k in 1:length(filenames)){
          try(download.file(filenames[k],  destfile = paste(Downloaddir, "/", win, "/", files[k], sep = "")))
          #x <-getURL("ftp://ftp.ba1.geog.umd.edu/TIFF/Win04/2010/MCD45monthly.A2010244.Win04.005.burndate.tif.gz", opts = opts)
          #gunzip(paste(tempdir, files[k], sep = ""), remove = FALSE)
        }    
      }
    }
  }  
  ## to check whether all the data are there....
  for(i in 1:length(windows)){  
    files <- dir(paste(Downloaddir, "/", "Win", windows[i], sep = ""))
    files <- files[grep("burndate", files)]   # use this code if you only want to download one type of burned area data
    files <- gsub(".gz", "", files)
    dates <- gsub("MCD45monthly.A", "", files); dates <- substr(dates, 1, 4)
    cat("window", windows[i], "\n")
    cat(paste(names(table(dates)), ":", table(dates), sep = ""), "\n", sep = " ")
  }  
  #NOTES
  ### from modis userguide
  #A user-friendly geotiff version of the MCD45 product is derived from the standard MCD45A1 hdf version by University of Maryland. 
  #The geotiffs are reprojected in Plate-Carrée projection and cover a set of sub-continental windows (figure 2). 
  #A table containing the regions covered and bounding coordinates of the 24 windows is available in Appendix III.
  #a) MCD45monthly = monthly Geotiff version of MCD45A1
  #b) A2000306 = year and Julian date of the starting day of the month covered by the product: 306 is the Julian date of Nov 1, hence 2000306 means that the product covers November 2000.
  #c) Win01 = spatial extent: the file covers window 1 (Alaska)
  #d) 005 = version identifier. 005 indicates Collection5
  #e) burndate/ba_qa = content of the file: unlike hdf, geotiff files contain a single layer. At the moment, two layers of the original product are available as geotiffs: "burndate" and "QA". See 3.3.3 for details.
  ##DATA
  #Burn date
  #• 0 - unburned (2 bytes): Approximate Julian day of burning from eight days before the beginning of the month to eight days after the end of the month, or a code indicating unburned areas, snow, water, or lack of data.
  #• 1-366 - approximate Julian day of burning
  #• 900 – snow or high aerosol
  #• 9998 - water bodies (internal)
  #• 9999 - water bodies (seas and oceans)
  #• 10000 - not enough data to perform inversion throughout the period
  #BA pixel QA
  #• 1 - most confidently detected pixels, regardless of direction in time (forward, backward or both), passing test (4) described in appendix 1. (1 byte): Confidence of the detection (1 (most confident) to 4 (least confident)).
  #• 2 - pixels where backward and forward direction in time predict the same change, passing test (5) described in appendix I.
  #• 3 - pixels selected in the first stage of the contextual analysis.
  #• 4 - pixels selected in the second stage of the contextual analysis.
  #Unlike the HDF version of the product, the GEOTIFFS do not include any overlap between consecutive months.  
}



extractDir <- ("E:\\ModisData_unzipped\\")
extractFireDat.s <- function(Downloaddir, extractDir, whatwins = "all"){   ## this expects to be in the format "WinX"
 ### to extract all the windows data at the same time
 ### extract .tif files from .gz files and store in temp directory
 ## then work out which files you want to download. This code assumes that there IS a subdirectory after the window
 if(whatwins[1] == "all") win <- dir(Downloaddir)
 if(whatwins[1] != "all") win <- whatwins   ## this expects to be in the format "WinX"
 for(i in 1:length(win)){
   in_files <- dir(paste(Downloaddir,"\\", win[i], sep = ""))
   in_files <- in_files[grep("burndate", in_files)]   # use this code if you only want to download one type of burned area data   
   out_files <- gsub(".gz", "", in_files)
   # to make sure you dont unzip all over again
   currentfiles <- list.files(file.path(extractDir, win))
   out_files <- setdiff(out_files, currentfiles) 
   if(length(out_files) > 0){    ## if there are some data to unzip
     in_files <- paste(Downloaddir, win[i], "\\", out_files, ".gz", sep = "")
     for(k in 1:length(in_files)){
       gunzip(in_files[k], destname = paste(extractDir, win[i], "\\", out_files[k], sep = ""), remove = FALSE)
     }
   }
 }
}







getBdates.s <- function(pnts, i, ){
  ### ok, just work with the data which are in this window  
  dat <- pnts[pnts$WinName == window[i],]
  #dat <- SpatialPointsDataFrame(dat[,c("lats_1", "longs_1")], data = dat)
  ### now comes the tricky part - to add the burn date for each month to a dataset of points. 
  ### read in the MODIS layers one at a time and extract the values for all the points to a file.
    #### create a new dataset and populate it with the beginning and end dates of the period of interest
  ### first get the year and month for each input data
  years <- unlist(strsplit(inputfiles, "MCD45monthly.A"))
  years <- years[years != ""]
  years <- unlist(strsplit(years, paste(".Win", window[i], ".051.burndate.tif", sep = "")))
  dates <- strptime(years, "%Y%j")
  #### check that all the months are there
  a <- dates[2:length(dates)] - dates[1:(length(dates)-1)]
  if(length(a[a > 31]) > 1) paste("problem with input data - missing files")  
  ### make a begining and end date
  start <- as.character(min(dates))
  end <- as.character(max(dates))
  #### create the dataset - basically we need the unique lat-long, a date, and an indication of whether it is the first/laste date or not
  Bdates <- matrix(NA, ncol = 3, nrow = nrow(dat)*length(inputfiles))  #this gives the maximum possible number of rows
  Bdates[1:nrow(dat),] <- cbind(dat$latlongs, rep(start, nrow(dat)), rep(0, nrow(dat)))   # record that this is the first/last date (i.e. an open interval)
  nn <- nrow(dat)+1   # make a counter that records the next empty row
  Bdates[nn:(nn+nrow(dat)-1),] <- cbind(dat$latlongs, rep(end, nrow(dat)), rep(0, nrow(dat)))
  nn <- nrow(dat)*2+1
  #lookup <- read.csv("D:\\Data_spatial\\Modis_Roy_BA\\MODIS_CODES_lookuptable.csv")
  getstart <- Sys.time()
  for(k in 1:length(inputfiles)){    # for every input burned area layer
    a <- raster(paste(tempdir, "Win", window[i], "\\", inputfiles[k], sep = ""))
    year <-  substring(years, 1, 4)[k]
    b <- raster::extract(a, dat)   # get the values for these points.     
    ### now you need to do some cleaning. Basically, only the points that have a date value attached need to be kept. 
    #### at the moment, ignore all other values except dates... maybe later add code to extract points that are classed as water etc
    id <- dat$latlongs[b > 0 & b < 367]
    b <- b[b > 0 & b < 367] 
    if(length(b) > 0){    #i.e. if there are some dates that are valid, add them to the new dataset. 
      bdate <- strptime(paste(rep(year, length(b)), b), "%Y %j")
      Bdates[nn:(nn+length(b)-1),] <- cbind(id, as.character(bdate), rep(1, length(b)))
      nn <- nn + length(b)
    }
    cat("at file", k,"of", length(inputfiles), "files. total time taken: ", round(Sys.time()-getstart,0), "\n")
    if(k == 40 || k == 70 || k == 90 || k == 120 || k == 130 || k == 160) save("Bdates", file = paste(whichdir, "DateInfo_Window", window[i], "_k", k,  ".Rdata", sep = ""))
  }
  t <-  max(c(1:nrow(Bdates))[!is.na(Bdates[,1])])
  Bdates <- Bdates[1:(t-1),]
  Bdates <- data.frame(Bdates)
  colnames(Bdates) <- c("latlong", "date", "burned")
  save("Bdates", file = paste(whichdir, "DateInfo_Window", window[i], ".Rdata", sep = ""))
  
  
  
  
}