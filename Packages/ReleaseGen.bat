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

rem fix vclwinx package requires (need after Sydney)
set package=RX10Sydney
call :vclwinx_after_fix
set package=RX11Alexandria
call :vclwinx_after_fix
set package=RX12Athens
call :vclwinx_after_fix

goto :end

:vclwinx_before_fix
FileFix -path %package%/EsVclComponents.dpk -delete-with "{$IF not(Defined(VER210) or Defined(VER220) or Defined(VER230) or Defined(VER240) or Defined(VER250) or Defined(VER260) or Defined(VER270) or Defined(VER280) or Defined(VER290) or Defined(VER300) or Defined(VER310) or Defined(VER320) or Defined(VER330))}vclwinx,{$IFEND}"
exit /b

rem set %package% var!
type %package%\EsVclComponents.dpk | findstr /v vclwinx > %package%\temp.dpk
del %package%\EsVclComponents.dpk
rename %package%\temp.dpk EsVclComponents.dpk
exit /b

:vclwinx_after_fix
FileFix -path %package%/EsVclComponents.dpk -replace-with "{$IF not(Defined(VER210) or Defined(VER220) or Defined(VER230) or Defined(VER240) or Defined(VER250) or Defined(VER260) or Defined(VER270) or Defined(VER280) or Defined(VER290) or Defined(VER300) or Defined(VER310) or Defined(VER320) or Defined(VER330))}vclwinx,{$IFEND}" -to "vclwinx,"
set lineto=/"RX10Sydney/"
FileFix -path %package%/EsVclComponents.dproj -insert-after "<DCCReference Include=^^EsCore.dcp^^/>" -line "<DCCReference Include=^^vclwinx.dcp^^/>"
exit /b

rem set %package% var!
powershell -executionpolicy bypass -command "(Get-Content -Path %package%\EsVclComponents.dpk) -replace [regex]::Escape('{$IF not(Defined(VER210) or Defined(VER220) or Defined(VER230) or Defined(VER240) or Defined(VER250) or Defined(VER260) or Defined(VER270) or Defined(VER280) or Defined(VER290) or Defined(VER300) or Defined(VER310) or Defined(VER320) or Defined(VER330))}vclwinx,{$IFEND}'), 'vclwinx,' | Set-Content -Path %package%\EsVclComponents.dpk -Encoding UTF8"
powershell -executionpolicy bypass -command "(Get-Content -Path %package%\EsVclComponents.dproj) -replace [regex]::Escape('        <DCCReference Include=\"EsCore.dcp\"/>'), ('        <DCCReference Include=\"EsCore.dcp\"/>' + \"`r`n\" + '        <DCCReference Include=\"vclwinx.dcp\"/>') | Set-Content -Path %package%\EsVclComponents.dproj -Encoding UTF8"
exit /b

:end