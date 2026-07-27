# Opens the Windows Defender Firewall inbound rules needed for the LAN to
# reach services running inside WSL2 (mirrored networking mode) on this
# host. Idempotent: safe to re-run, existing rules with the same
# -DisplayName are left as-is by New-NetFirewallRule only if absent --
# remove a rule first (Remove-NetFirewallRule -DisplayName "...") to
# recreate it with different ports.
#
# Run from an elevated (Administrator) PowerShell prompt:
#   powershell -ExecutionPolicy Bypass -File open_wsl_firewall_ports.ps1
#
# Context: doc/agile/versions/v0/sprint_24/offload-services-to-wsl-host/

# ---------------------------------------------------------------------------
# PostgreSQL (shared instance, all environments connect to the same cluster)
# ---------------------------------------------------------------------------
New-NetFirewallRule -DisplayName "WSL PostgreSQL" -Direction Inbound -LocalPort 5433 -Protocol TCP -Action Allow

# ---------------------------------------------------------------------------
# Per-environment service ports (http, wt, site, nats, nats_monitor) --
# one 1000-wide block per ores_dev_* worktree, base_port + {0,2,4,5,6}.
# See projects/ores.compass/src/env_init.py (BASE_PORT_START/STEP and the
# *_PORT_OFFSET constants) for the single source of truth on this layout.
# ---------------------------------------------------------------------------
New-NetFirewallRule -DisplayName "WSL ORE Studio (eager_maxwell)"   -Direction Inbound -LocalPort 20000,20002,20004,20005,20006 -Protocol TCP -Action Allow
New-NetFirewallRule -DisplayName "WSL ORE Studio (brave_hopper)"    -Direction Inbound -LocalPort 21000,21002,21004,21005,21006 -Protocol TCP -Action Allow
New-NetFirewallRule -DisplayName "WSL ORE Studio (bright_faraday)"  -Direction Inbound -LocalPort 22000,22002,22004,22005,22006 -Protocol TCP -Action Allow
New-NetFirewallRule -DisplayName "WSL ORE Studio (clever_dijkstra)" -Direction Inbound -LocalPort 23000,23002,23004,23005,23006 -Protocol TCP -Action Allow
New-NetFirewallRule -DisplayName "WSL ORE Studio (jolly_knuth)"     -Direction Inbound -LocalPort 24000,24002,24004,24005,24006 -Protocol TCP -Action Allow
New-NetFirewallRule -DisplayName "WSL ORE Studio (merry_newton)"    -Direction Inbound -LocalPort 25000,25002,25004,25005,25006 -Protocol TCP -Action Allow
New-NetFirewallRule -DisplayName "WSL ORE Studio (solid_dirac)"     -Direction Inbound -LocalPort 26000,26002,26004,26005,26006 -Protocol TCP -Action Allow
New-NetFirewallRule -DisplayName "WSL ORE Studio (prime_origin)"    -Direction Inbound -LocalPort 27000,27002,27004,27005,27006 -Protocol TCP -Action Allow
New-NetFirewallRule -DisplayName "WSL ORE Studio (swift_curie)"     -Direction Inbound -LocalPort 28000,28002,28004,28005,28006 -Protocol TCP -Action Allow

Write-Host "Done. Verify with: Get-NetFirewallRule -DisplayName 'WSL*' | Format-Table DisplayName,Enabled,Direction,Action"
