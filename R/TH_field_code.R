

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
#' This function sets paths for the set() and download() functions and should be used at the beginning of a session
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
  if(length(input.file.short)==0)stop("There are no .CSV files to be copied. Check SD card contents and proceed manually")
  if(length(input.file.short)>1)stop("There is more than 1 .CSV. Check SD card contents and proceed manually")
  file.copy(input.file.full,paste0(output,"\\",input.file.short),overwrite=F)
  end.length=length(list.files(output))-start.length
  end.contents=list.files(output)
  if(end.length==1)print("1 file(s) copied")
  if(end.length==1)print(end.contents[!(end.contents%in%start.contents)])
  if(end.length==0){
    warning("0 files copied")
  }else{
    new.file=list.files(output,full.names=T)[!(end.contents%in%start.contents)]
    tail.copied=tail(read.csv(new.file,1))
    print(paste0("Last record date:   ",tail.copied$date))
    print(paste0("Last record mm :   ",tail.copied$mm))
  }
}














####OLD
d=function(input="C:\\Users\\User\\Desktop\\sd",output="C:\\Users\\User\\Desktop\\th_download"){
  first.time=toupper(readline("Is this your first time using this function today? Enter 'y' or 'n'"))
  if(first.time%in%c("YES","Y")){
    drive=toupper(readline("What drive are SD cards opening under on this computer? Choose 'D','E', or 'F'___")) 
  }  
  start.length=length(list.files(output))
  start.contents=list.files(output)
  input.file.full=list.files(input,pattern=".csv",full.names=T)
  input.file.short=list.files(input,pattern=".csv")
  if(length(input.file.short)==0)stop("There are no .CSV files to be copied. Check SD card contents and proceed manually")
  if(length(input.file.short)>1)stop("There is more than 1 .CSV. Check SD card contents and proceed manually")
  file.copy(input.file.full,paste0(output,"\\",input.file.short),overwrite=F)
  end.length=length(list.files(output))-start.length
  end.contents=list.files(output)
  if(end.length==1)print("1 file(s) copied")
  if(end.length==1)print(end.contents[!(end.contents%in%start.contents)])
  if(end.length==0){
    warning("0 files copied")
  }else{
    new.file=list.files(output,full.names=T)[!(end.contents%in%start.contents)]
    tail.copied=tail(read.csv(new.file,1))
    print(paste0("Last record date:   ",tail.copied$date))
    print(paste0("Last record mm :   ",tail.copied$mm))
  }
}
download=d



