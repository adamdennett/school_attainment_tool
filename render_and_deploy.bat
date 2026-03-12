@echo off
REM render_and_deploy.bat
REM -------------------------------------------------------------------
REM Renders all Quarto (.qmd) reports in output\ and copies the
REM resulting HTML files to docs\ for GitHub Pages.
REM
REM Usage:
REM   render_and_deploy           REM render all .qmd files then copy
REM   render_and_deploy --quick   REM skip rendering, just copy HTML
REM -------------------------------------------------------------------

setlocal enabledelayedexpansion

set "OUTPUT_DIR=%~dp0output"
set "DOCS_DIR=%~dp0docs"

REM Ensure docs\ exists
if not exist "%DOCS_DIR%" mkdir "%DOCS_DIR%"
if not exist "%DOCS_DIR%\.nojekyll" type nul > "%DOCS_DIR%\.nojekyll"

REM ------------------------------------------------------------------
REM Step 1: Render .qmd files (unless --quick)
REM ------------------------------------------------------------------
if "%~1"=="--quick" (
    echo === Skipping render [--quick mode] ===
    echo.
    goto :copy
)

echo === Rendering Quarto reports ===
set "found=0"
for %%f in ("%OUTPUT_DIR%\*.qmd") do (
    set "found=1"
    echo   Rendering %%~nxf ...
    quarto render "%%f" --to html
    if errorlevel 1 (
        echo   ERROR: Failed to render %%~nxf
        exit /b 1
    )
)

if "!found!"=="0" (
    echo No .qmd files found in %OUTPUT_DIR%
    exit /b 1
)
echo.

REM ------------------------------------------------------------------
REM Step 2: Copy HTML files from output\ to docs\
REM ------------------------------------------------------------------
:copy
echo === Syncing HTML to docs\ ===
set "copied=0"

for %%f in ("%OUTPUT_DIR%\*.html") do (
    REM Skip the standalone map
    if "%%~nxf"=="la_cluster_map.html" (
        echo   Skipping %%~nxf [standalone map]
    ) else (
        copy /y "%%f" "%DOCS_DIR%\%%~nxf" >nul
        echo   Copied %%~nxf
        set /a copied+=1
    )
)

echo.
echo === Done: !copied! file(s) copied to docs\ ===
echo.
echo Next steps:
echo   git add docs/
echo   git commit -m "Update GitHub Pages reports"
echo   git push

endlocal
