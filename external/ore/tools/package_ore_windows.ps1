#requires -Version 7.0
<#
.SYNOPSIS
Package and verify a Windows x64 ORE release build. See packages/README.md.
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory)][string]$BuildDir,
    [string]$OutDir = "$PSScriptRoot/../packages",
    [string]$Version = '1.8.17.0',
    [string]$Sample = "$PSScriptRoot/../packages/TA002_IR_Swap.tar.gz",
    [string]$ExpectedOutput = "$PSScriptRoot/../examples/Academy/TA002_IR_Swap/ExpectedOutput",
    [double]$Rtol = 1e-6
)
$ErrorActionPreference = 'Stop'
if (!(Get-Command dumpbin.exe -ErrorAction SilentlyContinue)) {
    $vswhere = Join-Path ${env:ProgramFiles(x86)} 'Microsoft Visual Studio/Installer/vswhere.exe'
    $vs = & $vswhere -latest -products '*' -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath
    if (!$vs) { throw 'Visual Studio C++ tools are required for dependency inspection.' }
    & "$vs/Common7/Tools/Launch-VsDevShell.ps1" -Arch amd64 -HostArch amd64 -SkipAutomaticLocation
}
& python -X utf8 "$PSScriptRoot/package_ore_windows.py" --build-dir $BuildDir --out-dir $OutDir --version $Version --sample $Sample --expected-output $ExpectedOutput --rtol $Rtol
if ($LASTEXITCODE -ne 0) { throw "Windows package verification failed ($LASTEXITCODE)." }
