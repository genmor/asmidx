#' read in compleasm short summary
#'
#' @author Gen Morinaga
#'
#' @param path_to_compleasm file path to compleasm summary output
#' @param assembly.name name of the assembly on which the compleasm output is based.
#' If no name is provided, the outputted dataframe will not have an "assembly" column
#'
#' @details compleasm is a faster, more sensitive rewrite of BUSCO. The short summary
#' is similar to that of BUSCO, but it's more streamlined, and contains a new category
#' of fragmented single copy ortholog (I)
#'
#' @examples
#' # write sample compleasm summary outputs to the current
#' cat('compleasm_summary1.txt', file = 'compleasm_summary1.txt')
#' cat('compleasm_summary2.txt', file = 'compleasm_summary2.txt')
#'
#' #to read in single compleasm summary output
#' comp.df('compleasm_summary1.txt')
#'
#' comp.df('compleasm_summary1.txt', asssembly.name = 'TEST')
#'
#' #to read in multiple, simply wrap comp.df() in lapply() and use do.call('rbind')
#' #to output a single dataframe with multiple rows.
#' #Assuming that compleasm summary files are in the current working directory:
#'
#' #note that the example compleasm summary outputs are named 'compleasm_summary'
#' #read in all compleasm summary files as a list
#' df<-lapply(list.files(pattern = 'compleasm_'), comp.df)
#'
#' #bind the list of compleasm summary dataframes into a single dataframe
#' df<-do.call('rbind', df)
#'
#' #since it would be useful to know which summary output each row corresponds to
#' #create a new column that contains file names
#' df$file<-list.files(pattern = 'compleasm_')
#'
#' @export
comp.df<-function(path_to_compleasm_summary, assembly.name = NULL) {
  str<-unlist(strsplit(unlist(strsplit(readLines(path_to_compleasm_summary)[2:7], split = ', ')), split = ':'))
  str<-c(str[1:16],NA, str[length(str)])
  str<-gsub('\\%', '', str)
  df<-data.frame(matrix(data = str, ncol = 3, byrow = T)[, c(1,3)])
  df$X2<-as.numeric(df$X2)
  df<-dcast(df, NULL ~ X1, value.var = 'X2')[, -1]
  df<-transform(df, S_percent = round(S/N * 100, 2),
                D_percent = round(D/N * 100, 2),
                F_percent = round(F/N * 100, 2),
                I_percent  = round(I/N * 100, 2),
                M_percent = round(M/N * 100, 2))
  if(is.null(assembly.name)) {
    return(df)
  } else {
    df$assembly<-assembly.name
    return(df)
  }
}
