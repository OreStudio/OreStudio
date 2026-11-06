#requires -Version 7.0
<#
.SYNOPSIS
Build the pinned ORE CLI from a separate checkout of OreStudio/Engine.
.DESCRIPTION
Run from PowerShell with VS 2022 C++ tools, CMake, Git and Python installed.
SourceDir must already be checked out, including recursive submodules. No
checkout, upstream merge or push is performed. Dependencies have a dedicated
vcpkg manifest and install tree; existing OreStudio dependencies are untouched.
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory)][string]$SourceDir,
    [string]$BuildDir,
    [string]$VcpkgRoot,
    [string]$EngineCommit = '3d75a69087911a7e2cf1e6882da7bc27135c0762',
    [ValidateRange(1,64)][int]$Jobs = 4
)
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
function Invoke-Checked([string]$Program, [string[]]$Arguments) {
    & $Program @Arguments
    if ($LASTEXITCODE -ne 0) { throw "$Program failed ($LASTEXITCODE)" }
}
$repo = (Resolve-Path "$PSScriptRoot/../../..").Path
$SourceDir = (Resolve-Path -LiteralPath $SourceDir).Path
if (!$BuildDir) { $BuildDir = Join-Path $SourceDir 'build/orestudio-windows-x64-release' }
$BuildDir = [IO.Path]::GetFullPath($BuildDir)
if (!$VcpkgRoot) { $VcpkgRoot = Join-Path $repo 'vcpkg' }
$VcpkgRoot = (Resolve-Path -LiteralPath $VcpkgRoot).Path
$actualCommit = (& git -C $SourceDir rev-parse HEAD).Trim()
if ($LASTEXITCODE -ne 0 -or $actualCommit -ne $EngineCommit) {
    throw "Expected Engine commit $EngineCommit; found $actualCommit"
}
$dirty = & git -C $SourceDir status --porcelain --untracked-files=no --ignore-submodules=none
if ($LASTEXITCODE -ne 0 -or $dirty) { throw 'Engine source must be clean.' }
$submodules = & git -C $SourceDir submodule status --recursive
if ($LASTEXITCODE -ne 0 -or ($submodules | Where-Object { $_ -match '^[+\-U]' })) {
    throw 'Initialise the exact pinned submodules: git submodule update --init --recursive'
}
if (!(Get-Command cl.exe -ErrorAction SilentlyContinue)) {
    $vswhere = Join-Path ${env:ProgramFiles(x86)} 'Microsoft Visual Studio/Installer/vswhere.exe'
    $vs = & $vswhere -latest -products '*' -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath
    if (!$vs) { throw 'Visual Studio C++ x64 build tools are required.' }
    & "$vs/Common7/Tools/Launch-VsDevShell.ps1" -Arch amd64 -HostArch amd64 -SkipAutomaticLocation
}
if ($env:VSCMD_ARG_TGT_ARCH -ne 'x64') { throw 'Use the x64 Visual Studio developer environment.' }
if (!(Get-Command ninja.exe -ErrorAction SilentlyContinue)) {
    $ninjaDir = Join-Path $env:VSINSTALLDIR 'Common7/IDE/CommonExtensions/Microsoft/CMake/Ninja'
    if (!(Test-Path "$ninjaDir/ninja.exe")) { throw 'Ninja is required.' }
    $env:PATH = "$ninjaDir;$env:PATH"
}
New-Item -ItemType Directory -Force -Path $BuildDir | Out-Null
$env:VCPKG_ROOT = $VcpkgRoot
$installed = Join-Path $BuildDir 'vcpkg_installed'
Invoke-Checked "$VcpkgRoot/vcpkg.exe" @('install', "--x-manifest-root=$PSScriptRoot/windows", "--x-install-root=$installed", "--overlay-ports=$PSScriptRoot/windows/overlays", '--triplet=x64-windows', '--host-triplet=x64-windows')
Invoke-Checked cl.exe @('/nologo', '/std:c++20', '/EHsc', '/MD', '/DNDEBUG',
    "/I$installed/x64-windows/include", '/c', "$PSScriptRoot/windows/tests/ublas_msvc_probe.cpp",
    "/Fo$BuildDir/ublas_msvc_probe.obj")
$configure = @('--preset', 'windows-ninja-x64-release', '-S', $SourceDir, '-B', $BuildDir,
    "-DVCPKG_INSTALLED_DIR=$installed", '-DVCPKG_MANIFEST_MODE=OFF', '-DVCPKG_TARGET_TRIPLET=x64-windows',
    '-DORE_BUILD_APP=ON', '-DORE_BUILD_SWIG=OFF', '-DORE_BUILD_TESTS=OFF', '-DORE_BUILD_EXAMPLES=OFF',
    '-DORE_BUILD_DOC=OFF', '-DQL_BUILD_EXAMPLES=OFF', '-DQL_BUILD_TEST_SUITE=OFF', '-DQL_BUILD_BENCHMARK=OFF',
    '-DMSVC_WHOLE_PROGRAM_OPTIMIZATION=OFF', '-DMSVC_PARALLELBUILD=OFF',
    '-DBoost_USE_STATIC_LIBS=OFF', '-DBoost_USE_STATIC_RUNTIME=OFF', '-DMSVC_LINK_DYNAMIC_RUNTIME=ON')
Invoke-Checked cmake $configure
Invoke-Checked cmake @('--build', $BuildDir, '--config', 'Release', '--target', 'ore', '--parallel', "$Jobs")
$record = [ordered]@{
    engine_repository = 'https://github.com/OreStudio/Engine'
    engine_commit = $actualCommit
    engine_commit_date = (& git -C $SourceDir log -1 --format=%cI).Trim()
    submodules = @($submodules)
    built_at = [DateTime]::UtcNow.ToString('o')
    platform = 'x64-windows'
    configuration = 'Release'
    compiler = (Get-Command cl.exe).Source
    compiler_version = (Get-Item (Get-Command cl.exe).Source).VersionInfo.FileVersion
    cmake_version = @(& cmake --version)[0]
    vcpkg_commit = (& git -C $VcpkgRoot rev-parse HEAD).Trim()
    dependency_manifest_sha256 = (Get-FileHash "$PSScriptRoot/windows/vcpkg.json" -Algorithm SHA256).Hash.ToLowerInvariant()
    dependency_overlay_sha256 = @(Get-ChildItem "$PSScriptRoot/windows/overlays" -Recurse -File | Sort-Object FullName | ForEach-Object {
        @{ file = [IO.Path]::GetRelativePath("$PSScriptRoot/windows/overlays", $_.FullName); sha256 = (Get-FileHash $_.FullName -Algorithm SHA256).Hash.ToLowerInvariant() }
    })
    configure_arguments = $configure
}
[IO.File]::WriteAllText((Join-Path $BuildDir 'orestudio-build.json'), ($record | ConvertTo-Json -Depth 5) + "`n", [Text.UTF8Encoding]::new($false))
Write-Host "Built ORE: $BuildDir/App/Release/ore.exe"
