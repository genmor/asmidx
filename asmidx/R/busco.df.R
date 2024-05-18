#'Convert BUSCO short summary output into a dataframe
#'This function takes short_summary output of BUSCO (found at https://busco.ezlab.org/)
#'and converts it into a dataframe.
#'
#'@author Gen Morinaga
#'
#'@param path_to_busco_short_summary The absolute path to the short_summary file output from BUSCO
#'@param assembly.name Optional. A character string used as ID. If it isn't supplied then the function
#'will attempt to use the file name as ID. If the default BUSCO file output is kept, (short_summary.*.txt),
#'the the function by default will use the string in place of * for assembly.name.
#'
#'@examples
#'  #write example output of a short_summary output from BUSCO to the current working directory
#'  cat(`short_summary.specific.diptera_odb10.Nmex03_ipa-np.fasta.txt`,
#'  "short_summary.specific.diptera_odb10.Nmex03_ipa-np.fasta.txt")
#'  cat(`short_summary.specific.diptera_odb10.Nmex03_ipa-np_pd.fa.txt`,
#'     "short_summary.specific.diptera_odb10.Nmex03_ipa-np_pd.fa.txt")
#'
#'  #for a single file, name unspecified
#'  busco.df("short_summary.specific.diptera_odb10.Nmex03_ipa-np_pd.fa.txt")
#'
#'  #for a single file, name specified
#'  busco.df("short_summary.specific.diptera_odb10.Nmex03_ipa-np_pd.fa.txt",
#'  assembly.name = "Example_name")
#'
#'  #for multiple files
#'  do.call('rbind', lapply(list.files(pattern = 'short_summary'), busco.df))
#'@export
busco.df<-function(path_to_busco_short_summary, assembly.name = NULL) {
  tmp<-unlist(strsplit(readLines(path_to_busco_short_summary)[10:15], split = '\t'))
  tmp<-tmp[!(tmp %in% '')]
  tmp<-tmp[!(tmp %in% '   ')]
  dat<-matrix(tmp, ncol = 2, byrow = T)
  rm(tmp)
  dat<-data.frame(dat, cat = c('c', 's', 'd', 'f', 'm', 't'))
  colnames(dat)[1]<- 'n'
  dat<-dat[, -2]
  dat$n<-as.numeric(dat$n)
  dat$cat<-factor(c('c', 's', 'd', 'f', 'm', 't'),
                  levels = c('d', 'f', 'm', 's', 't','c'))
  if(is.null(assembly.name) == F) {
    dat$assembly<-assembly.name
  }
  else {
    dat$assembly<-fs::path_file(
      tools::file_path_sans_ext(
        tools::file_path_sans_ext(
          gsub('(short_summary\\.[a-z]+\\.[a-z]+_[a-z]+[0-9]+\\.)(.+)',
               '\\2',
               path_to_busco_short_summary)
        )
      )
    )
  }
  dat<-reshape(dat, idvar = 'assembly', timevar = 'cat', direction = 'wide')
  colnames(dat)[-1]<-c('c', 's', 'd', 'f', 'm', 't')
  dat<-transform(dat,
                 c_percent = round(c/t  * 100, 1),
                 s_percent = round(s/t * 100, 1),
                 d_percent = round(d/t * 100, 1),
                 f_percent = round(f/t * 100, 1),
                 m_percent = round(m/t * 100, 1))
  return(dat)
}
