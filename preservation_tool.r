library(dplyr)
library(haven)
library(stringr)
library(labelled)
library(tibble)
library(readr)


spss_val_syntax_v1 <- function(var_name,val_num,val_names){
  t_str <- paste0(var_name)
  
  for (v in 1:length(val_num)){
    t_str <- paste0(t_str,'\n    ', val_num[v], ' "',val_names[v],'"')
    
  }
  t_str <- paste0(t_str,' /\n')
  t_str
}



spss_data <- read_sav("./aes22_unrestricted_v2.sav")


####################################### delimtaed format dat

write_delim(as.data.frame(spss_data),na = '', "sample_data1.dat", delim = "\t")


dat_generator<- function(spss_data,dat_filepath=''){
  spss_syntax <- paste0("GET DATA /TYPE=TXT
  /FILE='",dat_filepath,"'
  /DELCASE=LINE
  /DELIMITERS='\\t'
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE=ALL
  /VARIABLES=\n")
  
  varlabel_syntax <- ""
  vallabel_syntax <- ""
  for (i in names(spss_data)) {
    spss_syntax <- paste0(spss_syntax,'    ',i,' ',attributes(spss_data[[i]])$format.spss,'\n')
    varlabel_syntax <- paste0(varlabel_syntax,'    ',i,' "',attributes(spss_data[[i]])$label,'"\n' )
    value_lables <- attributes(spss_data[[i]])$labels
    
    if (!is.null(value_lables)){
      vallabel_syntax <- paste0(vallabel_syntax,spss_val_syntax_v1(i,value_lables,names(value_lables)))
    }
    
    # spss_val_syntax
  }
  
  spss_syntax <- paste0(spss_syntax,'.\n\n\n')
  spss_syntax <- paste0(spss_syntax,'VARIABLE LABELS\n')
  spss_syntax <- paste0(spss_syntax,varlabel_syntax)
  
  spss_syntax <- paste0(spss_syntax,'.\n\n\n')
  spss_syntax <- paste0(spss_syntax,'VALUE LABELS\n')
  spss_syntax <- paste0(spss_syntax,vallabel_syntax)
  spss_syntax <- paste0(spss_syntax,'.\n\n\n')
  
  spss_syntax <- paste0(spss_syntax,paste0("SAVE OUTFILE='",tools::file_path_sans_ext(dat_filepath),".sav'") )
  
  spss_syntax
  
}

spss_syntax <- dat_generator(spss_data)
write(spss_syntax,'./t.sps')


