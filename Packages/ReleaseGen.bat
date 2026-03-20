call Gen.bat

rem fix vclwinx package requires (need before Sydney)
set package=XE2
call :vclwinx_before_fix
set package=XE3
call :vclwinx_before_fix
set package=XE4
call :vclwinx_before_fix
set package=XE5
call :vclwinx_before_fix
set package=XE6
call :vclwinx_before_fix
set package=XE7
call :vclwinx_before_fix
set package=XE8
call :vclwinx_before_fix
set package=RX10Seattle
call :vclwinx_before_fix
set package=RX10Berlin
call :vclwinx_before_fix
set package=RX10Tokyo
call :vclwinx_before_fix
set package=RX10Rio
call :vclwinx_before_fix

goto :end

:vclwinx_before_fix
FileFix -path %package%/EsVclComponents.dpk -delete-with "vclwinx,"
FileFix -path %package%/EsVclComponents.dproj -delete-with "vclwinx.dcp"
exit /b

:end