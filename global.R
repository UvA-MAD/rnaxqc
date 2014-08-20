library(stringr)
library(reshape)
library(dplyr)

data_dir <- "data"
design_file <- "design.txt"
count_file_prefix <- "CountTable_"
count_file_suffix <- ".txt"


# fetch count files
count_files <- list.files(data_dir, pattern = count_file_prefix)

# read in count data as dataframes
count_tables <- lapply(count_files, function(fin) {
  count_table <- read.table(file.path(data_dir, fin), header=T, row.names=1)

  # strip off "X" added by R in column names
  names(count_table) <- str_replace(names(count_table), "^X", "")

  # calculate log2(cmp + 05)
  # where cpm is counts per milion totall counts for species for sample
  count_table <- log2((count_table / (colSums(count_table) / 1000000)) + 0.5)

  return(count_table)
})

# strip file names off prefix and file extension
species_list <- count_files %>%
  str_replace_all(paste0("^", count_file_prefix), "") %>%
  str_replace_all(paste0(count_file_suffix, "$"), "")
names(count_tables) <- species_list

# read in design file as dataframe
design_table <- read.table(file.path(data_dir, design_file), header=T)
names(design_table) <- tolower(names(design_table))

group_factors <- names(design_table)
group_factors <- group_factors[group_factors != 'sampleid']

## boxplot ####################################################################
# for each species melt count table and merge with design table
count_design_molten <- lapply(count_tables, function(count_table){
  count_table$seqid <- rownames(count_table)
  molten_table <- melt(count_table)
  names(molten_table) <- c("seqid", "sampleid", "count")
  merged_table <- merge(molten_table, design_table, by="sampleid")
})

## PCA ########################################################################
# run principal component analysis on all species count tables
pca <- lapply(count_tables, prcomp)

# convert pca rotations to dataframes
# add experimental design information too
pca_rot <- lapply(pca, function(species_pca) {
  # get rotation as dataframe
  df <- data.frame(species_pca$r)
  # create column with sample ids for indexing later in plots
  df$sampleid <- rownames(df)
  # add exeperiment design details
  df <- merge(df, design_table, by='sampleid')
})

calc_proportion <- function(species_pca) {
  data.frame(proportion = species_pca$sdev^2/sum(species_pca$sdev^2),
             pc = 1:length(species_pca$sdev))
}

pca_prop <- lapply(pca, calc_proportion)

## MA #########################################################################
ma_tables <- count_tables

ma_active = FALSE
