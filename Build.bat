:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::Checks and builds R Analysis Pipelines (RAP) for Breeding View
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
@setlocal
@pushd %~dp0

@echo.
@set BuildError=0
@echo "Checks the package"
@call R.bat CMD check R2MLwiN %*
@if not %errorlevel% == 0 set BuildError=%errorlevel%
@echo.

@echo.
@echo "Builds the source package"
@call R.bat CMD build R2MLwiN %*
@if not %errorlevel% == 0 set BuildError=%errorlevel%
@echo.

@echo.
@echo "Builds the windows binary package"
@call R.bat CMD INSTALL --build R2MLwiN %*
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
