

#####JSTENZEL: To be part of a field or field TH package in the future


### TH Field Troubleshooting script
# Author: JStenzel


## This function ("set()") provides a logger id, time, and interval set. 
### ID: Logger id will only be reset if the argument "name" is set to a 5 digit # (e.g. "set(name=16500)' ) #
### Time: I am looking to consistently set times to non-Daylight Savings without user input.
### Interval: Interval will default to 30 min. If different interval is needed, set argument "int" to the 
###           number of minutes (e.g. 'set(int=15)')


### Set input/output folder for TH download

#' Set input/output folder for TH download/upload
#' 
#' This function sets paths for the set() and download() functions and should be used at the beginning of a session. The assigned input drive and output folder are printed in the R console. Please create an download output folder specific to the download day and site. Note: No parameters are necessary.
#' 
#' 
#'@export
set.folders=function(){
  input.drive<<-paste0(toupper(readline("What drive are SD cards opening under on this computer? Choose 'D','E', or 'F'___")),":\\") 
  readline("Hit ENTER and then select the desktop output folder")
  output.folder<<-choose.dir()
  print(paste0("Input Drive=  ",input.drive))
  print(paste0("Output folder=  ",output.folder))
 print("If these locations are incorrect, run 'set.folders()' again!")
}



####Imported set() function


#' Logger settings
#'
#' Sets key logger files: logging interval, time, logger ID. Setting the logger time is automatic (and will be to non-daylight savings time). Setting the logging interval and changing the ID will require USER responses. Note: In the current version, you can set parameter values for each of the logger setting files; alternatively, you can set these values with the prompts that result from running the function w/o parameters. Following the prompts will override any parameters set initially.
#'
#' @param int The logging interval; a number in minutes. Typically, do not change this from 30. 
#' @param name The logger ID. A five digit number. For "old" loggers (purple board), the convention is 14XXX. For "new" loggers (green board), the convention is 16XXX.
#' @param logger 'old' vs 'new' Old= a purple board, New = a green board. Differentiation is necessary because the logger versions require different time file names and time formats.
#'
#' @return Up to three different setting files to the selected removeable drive depending on user inputs.
#' @examples set("16999", 30, "new")
#'
#'@export
set=function(name="N",int=30,logger="new"){
  #if(dir.exists("D:/")==TRUE&dir.exists("E:/")==FALSE)drive="D:\\"
  #if(dir.exists("D:/")==FALSE&dir.exists("E:/")==TRUE)drive="E:\\"
  drive=paste0(readline("What drive is the SD card? Enter 'D', 'E', or 'F'___"),":\\")
  logger=readline("What logger verion? Enter 'old' or 'new?'Old has a purple board, new is green___")
  if(readline("do you need to change the logger id? Enter 'Y' or 'N'___")%in%c("Y","y","yes","'Y'")){
    name=as.numeric(readline("Enter the logger name (xxxxx; e.g. 16999)___")) 
  }
  write(int,paste0(drive,"INTSET.txt"))
  if(logger=="new")write(format(Sys.time()-3590,"%d/%m/%Y,%H:%M:%S,"),paste0(drive,"TIMESET.txt"))
  if(logger=="old")write(format(Sys.time()-3590,"%m/%d/%Y,%H:%M:%S,"),paste0(drive,"CLKSET.txt"))
  if(name!="N")write(paste0(" ",name),paste0(drive,"SERSET.txt"))
}


## This function ("d()") initiates a file copy from SD to desktop download folder of the day & lets the user know if there are deviations
## from the expected copying of 1 .csv

#' Download from SD to Laptop
#'
#' Downloads TreeHugger .CSV from chosen drive to a desktop folder. Relies on prompted user input to determine appropriate download and upload paths. Warns user if download or upload conditions are not as expected. Outputs the copied file name to the console.
#'
#'
#'
#'@export
download=function(input=input.drive,output=output.folder){
  if(toupper(readline("Have you set the input and output folders yet? 'y' or 'n'___"))%in%c("NO","N")){
    input.drive<<-paste0(toupper(readline("What drive are SD cards opening under on this computer? Choose 'D','E', or 'F'___")),":\\") 
    readline("Hit ENTER and then select the desktop output folder")
    output.folder<<-choose.dir()
    print(paste0("Input Drive=  ",input.drive))
    print(paste0("Output folder=  ",output.folder))
    print("If these locations are incorrect, run 'set.folders()' again!")
  }
  start.length=length(list.files(output))
  start.contents=list.files(output)
  input.file.full=list.files(input,pattern=".csv|.CSV",full.names=T)
  input.file.short=list.files(input,pattern=".csv|.CSV")
  if(length(input.file.short)==0){warning("There are no .CSV files to be copied. Check SD card contents and proceed manually",call.=FALSE)
  }else if(length(input.file.short)>1){warning("There is more than 1 .CSV. Check SD card contents and proceed manually",call.=FALSE)
    }else{
  file.copy(input.file.full,paste0(output,"\\",input.file.short),overwrite=F)
  end.length=length(list.files(output))-start.length
  end.contents=list.files(output)
  if(end.length==1)print("1 file(s) copied")
  if(end.length==1)print(end.contents[!(end.contents%in%start.contents)])
  if(end.length==0){
    warning("0 files copied",call.=FALSE)
  }else{
    new.file=list.files(output,full.names=T)[!(end.contents%in%start.contents)]
    tail.copied=tail(read.csv(new.file,1))
    print(paste0("Last record date:   ",tail.copied$date))
    print(paste0("Last record mm :   ",tail.copied$mm))
  }
  }
}

















