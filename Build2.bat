:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::Checks and builds R2MLwiN
::R_PATH is set manually by calling Rpathset.bat
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
@setlocal
@pushd %~dp0

@set BuildError=0
@echo.
@echo "Adds R_PATH manually"
@call Rpathset.bat %*
@if not %errorlevel% == 0 set BuildError=%errorlevel%
@echo.

@echo.
@echo "Checks the package"
@call "%R_PATH%\Rcmd.exe" check  R2MLwiN %*
@if not %errorlevel% == 0 set BuildError=%errorlevel%
@echo.

@echo.
@echo "Builds the source package"
@call "%R_PATH%\Rcmd.exe" build R2MLwiN %*
@if not %errorlevel% == 0 set BuildError=%errorlevel%
@echo.

@echo.
@echo "Builds the windows binary package"
@call "%R_PATH%\Rcmd.exe" INSTALL --build R2MLwiN %*
@if not %errorlevel% == 0 set BuildError=%errorlevel%
@echo.

@if not "%1" == "" goto :ParamsPresent
@echo.
@if %BuildError% == 0 echo ">>>>>>>>>>>>>>>> Build Succeeded <<<<<<<<<<<<<<<<"
@if not %BuildError% == 0 echo ">>>>>>>>>>>>>>>> Build Failed <<<<<<<<<<<<<<<<"
@echo.
@pause

:ParamsPresent
@popd
@exit /b %BuildError%
