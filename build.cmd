@echo off

call "%~dp0paket.cmd" restore
if errorlevel 1 (
  exit /b %errorlevel%
)

dotnet run --project "%~dp0build\BlackFox.FsGetLine.Build.fsproj" -- %*
