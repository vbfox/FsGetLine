@echo off

call paket.cmd restore
if errorlevel 1 (
  exit /b %errorlevel%
)

dotnet run --project build\BlackFox.FsGetLine.Build.fsproj -- %*
