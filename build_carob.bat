@ECHO OFF

IF %1.==. (GOTO nocombine) else (GOTO combine)

:nocombine
Rscript.exe --vanilla -e "x <- carobiner::make_carob(path='.', cache=TRUE, split_license=TRUE, report=TRUE, combine=FALSE, purge=TRUE)" 
GOTO done

:combine
Rscript.exe --vanilla -e "x <- carobiner::make_carob(path='.', cache=TRUE, split_license=TRUE, report=TRUE, combine=TRUE, purge=TRUE)"
GOTO done

:done
	