PackagesGenerator -config PackagesGenerator.ini -hide -skip

rem fix vclwinx package requires (need before Sydney)
set package=XE2
call :vclwinx_fix
set package=XE3
call :vclwinx_fix
set package=XE4
call :vclwinx_fix
set package=XE5
call :vclwinx_fix
set package=XE6
call :vclwinx_fix
set package=XE7
call :vclwinx_fix
set package=XE8
call :vclwinx_fix
set package=RX10Seattle
call :vclwinx_fix
set package=RX10Berlin
call :vclwinx_fix
set package=RX10Tokyo
call :vclwinx_fix
set package=RX10Rio
call :vclwinx_fix

goto :end

:vclwinx_fix
rem set %package% var!
type %package%\EsVclComponents.dpk | findstr /v vclwinx > %package%\temp.dpk
del %package%\EsVclComponents.dpk
rename %package%\temp.dpk EsVclComponents.dpk
exit /b

:end