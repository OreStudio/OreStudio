"""
Tests for nats_certs address discovery.

Run with:  python -m pytest projects/ores.compass/tests/test_nats_certs.py -v
No live database or file system access required.
"""

import ipaddress
import sys
from pathlib import Path

# Allow importing from the src directory without installing the package.
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import nats_certs as nc


def test_local_ipv4_addresses_returns_non_loopback():
    addrs = nc._local_ipv4_addresses()
    assert isinstance(addrs, list)
    assert all(isinstance(a, str) for a in addrs)
    assert "127.0.0.1" not in addrs
    for addr in addrs:
        ip = ipaddress.ip_address(addr)
        assert not ip.is_loopback


def test_local_ipv4_addresses_falls_back_when_ip_missing(monkeypatch):
    # macOS/Windows have no `ip` binary: subprocess.run raises
    # FileNotFoundError at exec time, before any exit code exists.
    def missing_ip(*args, **kwargs):
        raise FileNotFoundError(2, "No such file or directory: 'ip'")

    monkeypatch.setattr(nc.subprocess, "run", missing_ip)
    addrs = nc._local_ipv4_addresses()
    assert isinstance(addrs, list)
    for addr in addrs:
        ip = ipaddress.ip_address(addr)
        assert not ip.is_loopback
        assert not ip.is_link_local


def test_fallback_filters_loopback_and_link_local(monkeypatch):
    # gethostname may resolve to loopback (Debian maps it to 127.0.1.1)
    # or link-local addresses; only real addresses may survive.
    def missing_ip(*args, **kwargs):
        raise FileNotFoundError(2, "No such file or directory: 'ip'")

    def fake_getaddrinfo(host, port, family):
        return [
            (2, 1, 6, "", ("127.0.1.1", 0)),
            (2, 1, 6, "", ("169.254.1.5", 0)),
            (2, 1, 6, "", ("10.0.0.7", 0)),
        ]

    monkeypatch.setattr(nc.subprocess, "run", missing_ip)
    monkeypatch.setattr(nc.socket, "getaddrinfo", fake_getaddrinfo)
    assert nc._local_ipv4_addresses() == ["10.0.0.7"]
