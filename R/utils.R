###########################################
## UTILITY FILE FOR 
## TLS DATA PROCESSING WEBAPP
##
## (convert individual Cartesian coordinate
## files from each TLS position to the named Zc format 
## required by the TLS Webapp)
##

##
## PROBLEM STATEMENT:
## Have two files that are 3D cartesian coordinates from a set of fixed targets. 
## Each file is in its own coordinate system. 
## The order of the targets are not consistent. 
## What algorithm can I use to identify the correspondence between the targets in the two files?
##


##
## Function obtained from Gemini
##  Input: two files of Cartesian coordinates
##  Ouput: the indexes of the second file that correspond to the (ordered) indexes of the second file
##
match_targets_by_distance <- function(set_a, set_b, tolerance = 1e-5) {
  # 1. Calculate Euclidean distance matrices
  # set_a and set_b should be matrices or data frames with columns X, Y, Z
  ##   **calculate the distance between all target pairs
  ##    Column 1 contains: d(t1,t2), (t1,t3), (t1,t4), ..., (t1,t20)
  dist_a <- as.matrix(dist(set_a))
  dist_b <- as.matrix(dist(set_b))
  
  # 2. Create "Fingerprints" by sorting distances in each row
  # We sort so the order of other points doesn't matter
  fingerprints_a <- t(apply(dist_a, 1, sort))
  fingerprints_b <- t(apply(dist_b, 1, sort))
  
  # 3. Match fingerprints
  # For each point in A, find the row in B with the smallest difference
  num_points <- nrow(set_a)
  matches <- numeric(num_points)
  
  for (i in 1:num_points) {
    # Calculate the sum of absolute differences between point i in A 
    # and all candidate points in B
    diffs <- colSums(apply(fingerprints_b, 1, function(row_b) abs(row_b - fingerprints_a[i, ])))
    
    # The index of the minimum difference is our match
    matches[i] <- which.min(diffs)
  }
  
  # Return the indexes of set_b that correpond to the order of set_a
  return(matches)
  
  # Return a data frame showing the mapping
  #return(data.frame(
  #  Set_A_Row = 1:num_points,
  #  Set_B_Match = matches
  #))
}

##
## WRITE FUNCTION THAT TAKES AS INPUT THE ORIGINAL USER FILES (ONE FILE PER POSITION)
## AND RETURNS A FILE THAT IS IN THE FORMAT THAT THE TLS WEBAPP REQUIRES
## 
## INPUTS: f1, f2, f3, f4       Each file is a set of Cartesian coordinates, in their own frame of reference
##         name                 Optional string that the user can use to name the targets (will apply sequential numbering to it)
##                               e.g., "Target" if you want the target names to be [Target01, Target02,...,Target20]
##                                     "CB_0"   if you want names to be [CB_001, CB_002, ..., CB_020]
## 
## OUTPUTS: file_out            Combined file, with target coordinates in order corresponding to the order in the first file,
##                              with a "TargetID" column
##

process_tls_data <- function(f1, f2, f3, f4, name_scheme = "Target"){
  # Put them in a list for easy iteration
  data_list <- list(f1, f2, f3, f4)
  
  ###############################################
  ##**PERFORM SOME BASIC CHECKS**
  ##
  ## CHECK FOR NA VALUES
  na_check <- sapply(data_list, anyNA)
  if (any(na_check)) {
    # Find which files failed the check
    failed_na <- which(na_check)
    stop(paste("Data check failed: NA values found in file(s):", 
               paste(failed_na, collapse = ", ")))
  }
  ## CHECK THAT EACH FILE HAS 3 COLUMNS
  ncol_check <- sapply(data_list, ncol)
  if (!all(ncol_check == 3)) {
    # Optional: Identify which files specifically have the wrong count
    failed_ncol <- which(ncol_check != 3)
    stop(paste("Data check failed: File(s)", 
               paste(failed_ncol, collapse = ", "), 
               "do not have exactly 3 columns."))
  }
  ## CHECK THAT THE FILES HAVE THE SAME NUMBER OF TARGETS
  nrow_check <- sapply(data_list, nrow)
  if (length(unique(nrow_check)) != 1) {
    
    # Optional: Provide feedback on what the different counts are
    stop(paste("Data check failed: Files must have the same number of targets (the number of rows in each file must be the same)"))
  }
  ######################################################
  ##
  ##**PROVIDED THE DATA PASSED THE CHECKS, NOW DO SOME STUFF*
  ##
  ## Extract the (optional) user-provided target name 
  base_name <- name_scheme
  ## 
  ## Create the TargetID names
  TargetID <- paste0(base_name, sprintf("%02d", 1:nrow(f1)))
  
  ## 
  ## Using the 'match_targets_by_distance' function, determine the 
  ## row indices in files 2-4 that correponds to file 1
  ##
  f2_order <- match_targets_by_distance(f1, f2)
  f3_order <- match_targets_by_distance(f1, f3)
  f4_order <- match_targets_by_distance(f1, f4)
  
  ##
  ## Put the rearranged data together with a TargetID column
  ## 
  cdat <- data.frame(
    TargetID, 
    cbind(f1, f2[f2_order,], f3[f3_order,], f4[f4_order,])
  )
  ## do some cleaning
  colnames(cdat)[2:ncol(cdat)] <- rep(c("X", "Y","Z"), length(data_list))
  rownames(cdat) <- NULL
  
  return(cdat)
}

################################################
## FUNCTION THAT CHECKS THAT THE FOUR DATA FILES
## 1. do not contain NA values
## 2. have only 3 columns
## 3. have the same number of rows
##
check_data_integrity <- function(datasets, header_setting) {
  datasets <- Filter(Negate(is.null), datasets)
  if (length(datasets) < 4) return(NULL) 
  
  # --- CHECK 1: Header Mismatch ---
  # If User says 'No' but R sees characters in row 1 and numbers in row 2
  header_mismatch <- any(sapply(datasets, function(df) {
    is.character(df[1,1]) && !is.na(as.numeric(df[2,1]))
  }))
  
  if (header_setting == "No" && header_mismatch) {
    return("It looks like your file has a header, but you selected 'No'. Please check your settings.")
  }
  
  # --- CHECK 2: NA Values ---
  if (any(sapply(datasets, function(df) any(is.na(df))))) {
    return("One or more files contain missing (NA) values. Please clean your data.")
  }
  
  # --- CHECK 3: Column Count ---
  if (any(sapply(datasets, ncol) != 3)) {
    return("All uploaded files must have exactly 3 columns.")
  }
  
  # --- CHECK 4: Row Count ---
  rows <- sapply(datasets, nrow)
  if (length(unique(rows)) > 1) {
    return("The uploaded files do not have the same number of rows.")
  }
  
  return(NULL) 
}

############################################
## Function to check for unique file names

check_unique_filenames <- function(files) {
  # Get the names of the files that are not NULL
  names <- sapply(files, function(f) f$name)
  names <- names[!sapply(names, is.null)]
  
  if (length(names) < 4) return(NULL) # Only check when all 4 are there
  
  if (length(unique(names)) < 4) {
    return("Error: All four uploaded files must have unique names. Please check for duplicates.")
  }
  return(NULL)
}