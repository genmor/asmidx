#' Convert structural_error.BED output from Inspector into a dataframe
#'
#' The python program Inspector (available from: https://github.com/Maggi-Chen/Inspector)
#' can be used to identify structural errors (>50 bp) in hifi genome assemblies.
#' This function takes the structural_error.BED output and converts it into
#' a dataframe for easy manipulation in R.
#'
#' As of this writing (Feb. 3, 2023), Inspector does not name its output files with identifying information (by design).
#' For use with this function, the file must be copied and renamed with identifying information separated by underscores (_).
#' For example: specimen_assembly_structural_error.BED
#' Note that if you need to use inspector-correct, then the files cannot be renamed (hence copying and renaming).
#'
#'@author Gen Morinaga
#'
#' @param path_to_bed The absolute file path to the *_structural_error.BED file.
#' The \* should be a some sort of ID string
#' @param summarise Logical (T/F) default: summarise = T. The contents of structural_error.bed has lists each error as a row with columns
#' for the start and end of positions of the error, the number of reads that support that the error is there, the type of error, and the size of the error (in bp).
#' summarising will calculate the size (in bp) of each type of error for the assembly.
#' @details If there are multiple structural_error.bed files and the goal is to summarise each file,
#' it may be best if this function is used in lapply (see example). This will generate a list of dataframes
#' which can made into a single dataframe using bind_rows (in dplyr) or do.call and rbind (see example). To do this, first create
#' a vector of absolute paths to each structural_error.bed file or aggregate all structural_error.bed
#' files into a single directory and make that the working directory. Then, use
#' list.files() in lapply with this function to generate a list of dataframes with summarised
#' error lengths. Finally, use bind_rows on this list object to create a single dataframe
#' with each row representing an assembly.
#' @examples
#' #write example output of a structural_error.bed output from Inspector to the
#' #current working directory
#' cat(`Nmex03_ipa-np_insp_structural_error.bed`,
#' file = "Nmex03_ipa-np_insp_structural_error.bed")
#' cat(`Nmex03_ipa-np_pd_insp_structural_error.bed`,
#' file = "Nmex03_ipa-np_pd_insp_structural_error.bed")
#'
#' #for a single file, summarised
#' insp_stbed2df("Nmex03_ipa-np_insp_structural_error.bed", summarise = T)
#'
#' #for a single file, unsummarised output
#' insp_stbed2df("Nmex03_ipa-np_insp_structural_error.bed", summarise = F)
#'
#' #for multiple files, summarised. For unsummarised output, simply set summarise = F
#' do.call('rbind', lapply(list.files(pattern = '.bed', insp_stbed2df, summarise = T)))
#' @export
insp_stbed2df<-function(path_to_bed, summarise = T) {
  dat <- tryCatch(read.table(path_to_bed, header = F, sep = "\t", quote = "", 
                    fill = T)[, 1:6], 
                  error = function(e) {
                    message(paste('The following file does not contain rows:', path_to_bed))
                    message('We will assume that no errors were detected in this assembly')
                    message('Creating a dataframe with 0s for each error type')
                    data.frame(contig = rep('x', 4), start = rep(0, 4), end = rep(0, 4),
                               n.support.reads = rep(0, 4), type = c('Collapse', 'Expansion', 'HaplotypeSwitch', 'Inversion'),
                               size = rep(0, 4))
                  })
  colnames(dat)<-c('contig', 'start', 'end', 'n.support.reads', 'type', 'size')
  dat$size<-as.numeric(gsub('(Size=)([0-9]+)(;?[0-9]+)', '\\2', dat$size))
  dat$start<-as.integer(gsub('([0-9]+);([0-9]+)', '\\1', dat$start))
  dat$end<-as.integer(gsub('([0-9]+);([0-9]+)', '\\1', dat$end))
  dat$type<-factor(dat$type, levels = c('Collapse', 'Expansion', 'HaplotypeSwitch', 'Inversion'))
  dat$assembly<-gsub('_structural_error','', tools::file_path_sans_ext(fs::path_file((path_to_bed))))
  if(summarise == F) {
    return(dat)
  }
  else {
    dat<-aggregate(size ~ type + assembly, data = dat, sum, drop = F)
    dat$size[is.na(dat$size)]<-0
    dat<-reshape(dat, idvar = 'assembly', timevar = 'type', direction = 'wide')
    colnames(dat)[2:5]<-c('Collapse', 'Expansion', 'HaplotypeSwitch', 'Inversion')
    return(dat)
  }
}
