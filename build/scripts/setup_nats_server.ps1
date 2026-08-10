# setup_nats_server.ps1 - put the vendored nats-server binary on PATH.
#
# Windows equivalent of setup_nats_server.sh: unpacks the pinned
# nats-server release under external/nats/packages/ into
# build/nats/vendored/ and prepends that directory to PATH via
# GITHUB_PATH, so Windows CI runs the same pinned binary as Linux and
# macOS instead of a GitHub-release download.
#
# Usage (from the checkout root):
#   ./build/scripts/setup_nats_server.ps1
#

$ErrorActionPreference = 'Stop'

$root = Split-Path -Parent (Split-Path -Parent $PSScriptRoot)
$version = 'v2.14.3'
$pkg = 'windows-amd64'
$archive = Join-Path $root "external/nats/packages/nats-server-$version-$pkg.zip"
$dest = Join-Path $root "build/nats/vendored/$pkg"

if (-not (Test-Path (Join-Path $dest 'nats-server.exe'))) {
    $scratch = Join-Path ([System.IO.Path]::GetTempPath()) "nats-server-$([guid]::NewGuid().ToString('N'))"
    Expand-Archive -Path $archive -DestinationPath $scratch -Force
    if (Test-Path $dest) {
        Remove-Item -Recurse -Force $dest
    }
    New-Item -ItemType Directory -Path (Split-Path -Parent $dest) -Force | Out-Null
    Move-Item -Path (Join-Path $scratch 'nats-server-*') -Destination $dest
    Remove-Item -Recurse -Force $scratch
}

if (-not (Test-Path (Join-Path $dest 'nats-server.exe'))) {
    Write-Error "nats-server binary not found after unpacking: $dest"
    exit 1
}

if (-not $env:GITHUB_PATH) {
    Write-Warning "GITHUB_PATH is not set; $dest is not on PATH"
}
else {
    Add-Content -Path $env:GITHUB_PATH -Value $dest -Encoding utf8NoBOM
}
