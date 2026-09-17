FsGetLine
=========

[![CI](https://github.com/vbfox/FsGetLine/actions/workflows/ci.yml/badge.svg)](https://github.com/vbfox/FsGetLine/actions/workflows/ci.yml)

[![Nuget Package](https://img.shields.io/nuget/v/BlackFox.FsGetLine.svg)](https://www.nuget.org/packages/BlackFox.FsGetLine)

This repository contains an implementation of 'getline' in F#.

The original version is [getline.cs][1] by Miguel de Icaza as described
in his blog post [getline.cs: Partying like its 1988][2].

The version in this repository was created as an exercise of porting C#
code to F# and making it more functional.

All bugs are mine.

Releasing
---------

The version and notes come from `Release Notes.md`. Once a new entry is at the top of that file
and merged into `main`, run from a clean, up-to-date checkout of `main`:

```bash
./build.sh Release
```

It checks the preconditions, then tags and pushes the tag, which triggers the `Publish` GitHub
Actions workflow. That workflow builds, tests, packs, creates the GitHub Release and pushes the
package to NuGet.org using trusted publishing (no API key stored in the repository).

[1]: https://github.com/mono/mono/blob/master/mcs/tools/csharp/getline.cs
[2]: http://tirania.org/blog/archive/2008/Aug-26.html
