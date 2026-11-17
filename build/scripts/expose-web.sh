#!/usr/bin/env bash
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
# Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# Opens and closes the public HTTPS view of the web service on a deployed host.
#
# The exposure goes through Tailscale Funnel rather than the host firewall, and
# that is deliberate: Funnel terminates TLS with a certificate for the node's
# tailnet name, reaches the service over loopback, and needs no inbound port at
# all. Opening 443 in nftables would instead require a certificate for an
# address the host does not own and a TLS terminator in front of a plain HTTP
# service, and would leave the port open for as long as the rule stayed.
#
# What Funnel publishes is public: anyone with the URL reaches the service. The
# service authenticates its users, so the exposure is the login page; the point
# of the script is that the window is opened deliberately and closed again.
#
# Funnel has to be allowed for the tailnet once, in the Tailscale admin
# console. On a tailnet that has not enabled it, `on` prints the URL that
# enables it.
#
# Usage:
#   build/scripts/expose-web.sh <ssh-host> on
#   build/scripts/expose-web.sh <ssh-host> off
#   build/scripts/expose-web.sh <ssh-host> status
#
# Environment:
#   WEB_PORT     port the BFF listens on locally (default 21402)
#   HTTPS_PORT   public HTTPS port Funnel serves on (default 443)
set -euo pipefail

host="${1:?usage: expose-web.sh <ssh-host> <on|off|status>}"
action="${2:-status}"
web_port="${WEB_PORT:-21402}"
https_port="${HTTPS_PORT:-443}"
target="http://127.0.0.1:${web_port}"

case "$action" in
    on)
        # --bg leaves the funnel running after the ssh session ends, which is
        # what makes an explicit `off` meaningful.
        if output=$(ssh "$host" \
            "timeout 25 tailscale funnel --bg --https=${https_port} ${target} 2>&1"); then
            printf '%s\n' "$output"
            echo "Open to the internet on https://<node>.<tailnet>.ts.net/"
            echo "Close it with: $0 $host off"
        else
            printf '%s\n' "$output" >&2
            echo "Funnel is not available; the message above says how to enable it." >&2
            exit 1
        fi
        ;;
    off)
        # Closing a funnel that is not open waits for a confirmation that
        # never comes, so the status decides whether there is anything to
        # close — and, unlike the close itself, it answers at once.
        if ssh "$host" "timeout 10 tailscale funnel status 2>/dev/null" \
                | grep -q 'https://'; then
            ssh "$host" "timeout 20 tailscale funnel --https=${https_port} off"
        else
            echo "Nothing was being served on ${https_port}."
        fi
        echo "The web service is no longer reachable from the internet."
        ;;
    status)
        ssh "$host" "tailscale funnel status"
        ;;
    *)
        echo "unknown action: ${action} (expected on, off or status)" >&2
        exit 2
        ;;
esac
