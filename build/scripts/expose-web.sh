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
# Opens and closes the public HTTPS view of the web service on a deployed host,
# through the host's own address.
#
# The host terminates TLS with Caddy, which obtains and renews a Let's Encrypt
# certificate for the name it is given and reverse-proxies to the BFF bound to
# loopback. While closed the machine accepts no traffic at all from the
# internet: closing deletes the one rule that opens 443 and stops Caddy, so
# nothing is left listening and the host's other rules are untouched.
#
# Only 443 is opened. The certificate is validated with TLS-ALPN-01, which
# happens on 443, so port 80 is never needed.
#
# The name must resolve to the host's public address before `on`.
#
# Usage:
#   build/scripts/expose-web.sh <ssh-host> setup <hostname>
#   build/scripts/expose-web.sh <ssh-host> on [--for 2h]
#   build/scripts/expose-web.sh <ssh-host> off
#   build/scripts/expose-web.sh <ssh-host> status
#
# Environment:
#   WEB_PORT   port the BFF listens on locally. Unset, it is read from the
#              host profile (.env.<ssh-host>), then from .env, so the proxy
#              follows the environment instead of a port written twice.
set -euo pipefail

host="${1:?usage: expose-web.sh <ssh-host> <setup|on|off|status> [args]}"
action="${2:?usage: expose-web.sh <ssh-host> <setup|on|off|status> [args]}"
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
helper="/usr/local/bin/ores-web-close"

die() {
    echo "expose-web: $*" >&2
    exit 1
}

# The web port one environment file states, or nothing.
port_of() {
    [ -f "$1" ] || return 1
    local value
    value="$(sed -n 's/^ORES_WEB_PORT=//p' "$1" | tail -1 | sed 's/["[:space:]]//g')"
    [ -n "$value" ] && printf '%s' "$value"
}

web_port="${WEB_PORT:-}"
if [ -z "$web_port" ]; then
    web_port="$(port_of "$root/.env.$host" || port_of "$root/.env" || true)"
fi
[ -n "$web_port" ] || die "cannot tell which port the web listens on: set WEB_PORT, or ORES_WEB_PORT in .env.$host or .env"

# The port the host's Caddyfile proxies to, or nothing when it has none.
caddy_port() {
    ssh "$host" "sed -n 's/.*reverse_proxy 127.0.0.1:\([0-9][0-9]*\).*/\1/p' /etc/caddy/Caddyfile 2>/dev/null | head -1"
}

# One rule, inserted at the head of the host's own input chain, and removed by
# the handle nftables gives it. A separate table does not work here: every base
# chain on a hook sees the packet, so an accept in a chain of its own is
# followed by the host's chain and its drop policy, and the packet is dropped
# anyway. The rule carries a comment, so the close removes that rule and never
# a rule the host wrote: the host may already accept 443 somewhere in the same
# chain, and a handle read back by port would name the wrong one.
handle_file="/run/ores-web-expose.handle"
rule_comment="ores-web-expose"

# The handle of the rule this script adds, from its comment, or nothing.
find_handle='nft -a list chain inet filter input 2>/dev/null \
    | grep "comment \"ores-web-expose\"" | tail -1 | sed "s/.*# handle //"'

open_rules() {
    ssh "$host" "
        nft insert rule inet filter input tcp dport 443 accept comment \"${rule_comment}\"
        ${find_handle} > ${handle_file}
        cat ${handle_file}
    "
}

close_rules() {
    ssh "$host" "
        handle=\$(cat ${handle_file} 2>/dev/null || true)
        if [ -z \"\$handle\" ]; then handle=\$(${find_handle}); fi
        if [ -n \"\$handle\" ]; then
            nft delete rule inet filter input handle \"\$handle\" 2>/dev/null || true
        fi
        rm -f ${handle_file}
    "
}

# The same close, runnable on the host by a timer, so an exposure that is
# forgotten ends by itself.
install_helper() {
    ssh "$host" "cat > ${helper} <<'EOF'
#!/bin/sh
# Written by build/scripts/expose-web.sh. Ends a scheduled exposure.
systemctl stop caddy >/dev/null 2>&1 || true
handle=\$(cat ${handle_file} 2>/dev/null || true)
if [ -z \"\$handle\" ]; then
    handle=\$(nft -a list chain inet filter input 2>/dev/null \
        | grep 'comment \"${rule_comment}\"' | tail -1 | sed 's/.*# handle //')
fi
if [ -n \"\$handle\" ]; then
    nft delete rule inet filter input handle \"\$handle\" 2>/dev/null || true
fi
rm -f ${handle_file}
EOF
chmod 755 ${helper}"
}

write_caddyfile() {
    ssh "$host" "cat > /etc/caddy/Caddyfile <<'EOF'
{
	# No admin endpoint: the configuration is written here, not over HTTP.
	admin off
}

${hostname} {
	tls {
		# 443 is the only port the exposure opens, and TLS-ALPN-01 is the
		# challenge that runs on it.
		issuer acme {
			disable_http_challenge
		}
	}

	header {
		Strict-Transport-Security \"max-age=31536000\"
		X-Content-Type-Options \"nosniff\"
		X-Frame-Options \"DENY\"
		Referrer-Policy \"strict-origin-when-cross-origin\"
	}

	reverse_proxy 127.0.0.1:${web_port}
}
EOF
caddy validate --config /etc/caddy/Caddyfile --adapter caddyfile >/dev/null 2>&1 || {
    echo 'Caddyfile is invalid:' >&2
    caddy validate --config /etc/caddy/Caddyfile --adapter caddyfile >&2
    exit 1
}"
}

case "$action" in
    setup)
        hostname="${3:?setup needs the hostname the certificate is issued for}"
        # The name reaches a heredoc that writes the host's configuration, so it
        # may hold only what a DNS name holds.
        if [[ ! "$hostname" =~ ^[A-Za-z0-9.-]+$ ]]; then
            echo "the hostname may hold only letters, digits, dots and dashes: $hostname" >&2
            exit 2
        fi
        ssh "$host" "command -v caddy >/dev/null 2>&1 || DEBIAN_FRONTEND=noninteractive apt-get install -y caddy >/dev/null"
        write_caddyfile
        # Closed is the default, so the service must not start with the machine.
        ssh "$host" "systemctl disable caddy >/dev/null 2>&1 || true; systemctl stop caddy >/dev/null 2>&1 || true"
        install_helper
        echo "Caddy is configured for ${hostname}, proxying 127.0.0.1:${web_port}."
        echo "Open it with: $0 ${host} on"
        ;;
    on)
        shift 2
        duration=""
        while [[ $# -gt 0 ]]; do
            case "$1" in
                --for) duration="${2:?--for needs a duration, e.g. 2h}"; shift 2 ;;
                *) echo "unknown argument: $1" >&2; exit 2 ;;
            esac
        done
        # An exposure that proxies to a port nothing listens on answers 502 to
        # every visitor, so the two ports are compared before the port opens.
        proxied="$(caddy_port)"
        [ -n "$proxied" ] || die "no Caddyfile on $host: run $0 $host setup <hostname>"
        if [ "$proxied" != "$web_port" ]; then
            die "$host proxies 127.0.0.1:$proxied but the web listens on $web_port: run $0 $host setup <hostname>"
        fi
        open_rules
        ssh "$host" "systemctl start caddy"
        ssh "$host" "systemctl stop ores-web-close.service 2>/dev/null || true"
        if [[ -n "$duration" ]]; then
            install_helper
            # An earlier exposure may still hold the timer and its unit name,
            # so both are cancelled before this one is scheduled.
            ssh "$host" "systemctl stop ores-web-close.timer ores-web-close.service >/dev/null 2>&1 || true; systemctl reset-failed ores-web-close.timer ores-web-close.service >/dev/null 2>&1 || true"
            # A timer that did not start leaves the exposure open for good, so
            # a failure closes it now rather than leaving it to a person.
            if ! ssh "$host" "systemd-run --on-active='${duration}' --unit=ores-web-close ${helper} >/dev/null 2>&1"; then
                echo "the close timer did not start; closing now" >&2
                ssh "$host" "systemctl stop caddy >/dev/null 2>&1 || true" || true
                close_rules || true
                exit 1
            fi
        fi
        hostname="$(ssh "$host" "awk '\$2 == \"{\" {print \$1; exit}' /etc/caddy/Caddyfile")"
        echo "Open on https://${hostname}/"
        [[ -n "$duration" ]] && echo "It closes itself in ${duration}."
        echo "Close it now with: $0 ${host} off"
        ;;
    off)
        ssh "$host" "systemctl stop caddy >/dev/null 2>&1 || true"
        close_rules
        ssh "$host" "systemctl stop ores-web-close.timer ores-web-close.service >/dev/null 2>&1 || true; systemctl reset-failed ores-web-close.timer ores-web-close.service >/dev/null 2>&1 || true"
        echo "Caddy stopped and port 443 closed."
        ;;
    status)
        printf 'caddy      : %s\n' "$(ssh "$host" "systemctl is-active caddy 2>/dev/null || true")"
        printf 'firewall   : %s\n' "$(ssh "$host" "nft -a list chain inet filter input 2>/dev/null | grep -q 'comment \"${rule_comment}\"' && echo 'open (443)' || echo closed")"
        proxied="$(caddy_port)"
        if [ -z "$proxied" ]; then
            printf 'proxy      : no Caddyfile: run %s %s setup <hostname>\n' "$0" "$host"
        elif [ "$proxied" = "$web_port" ]; then
            printf 'proxy      : 127.0.0.1:%s (matches the environment)\n' "$proxied"
        else
            printf 'proxy      : 127.0.0.1:%s but the web listens on %s: run setup again\n' "$proxied" "$web_port"
        fi
        # Caddy keeps one directory per issuer and one per name under it.
        printf 'certificate: %s\n' "$(ssh "$host" "ls -1 /var/lib/caddy/.local/share/caddy/certificates/*/*/*.crt 2>/dev/null | xargs -r -n1 basename | tr '\n' ' ' || true")"
        ;;
    *)
        echo "unknown action: ${action} (expected setup, on, off or status)" >&2
        exit 2
        ;;
esac
