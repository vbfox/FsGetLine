#!/bin/bash
set -eo pipefail

./paket.sh restore || { exit $?; }

dotnet run --project build/BlackFox.FsGetLine.Build.fsproj -- "$@"
