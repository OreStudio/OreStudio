/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.platform/net/network_info.hpp"
#include <algorithm>
#include <array>
#include <climits>
#include <cstdint>
#include <functional>
#include <iomanip>
#include <sstream>
#include <vector>

#if defined(__linux__)
#    include <ifaddrs.h>
#    include <net/if.h>
#    include <netpacket/packet.h>
#    include <sys/types.h>
#    include <unistd.h>
#elif defined(__APPLE__)
#    include <ifaddrs.h>
#    include <net/if.h>
#    include <net/if_dl.h>
#    include <sys/param.h>
#    include <sys/types.h>
#    include <unistd.h>
#elif defined(_WIN32)
#    include <windows.h>
#    include <iphlpapi.h>
#    pragma comment(lib, "iphlpapi.lib")
#endif

namespace ores::platform::net {

namespace {

std::string format_mac_address(const std::string& mac_bytes) {
    std::ostringstream oss;
    oss << std::hex << std::setfill('0');
    for (std::size_t i = 0; i < mac_bytes.size(); ++i) {
        if (i > 0)
            oss << ':';
        oss << std::setw(2) << static_cast<unsigned>(static_cast<unsigned char>(mac_bytes[i]));
    }
    return oss.str();
}

/**
 * Returns every non-loopback 6-byte hardware address as sorted raw bytes.
 * The lexicographic ordering of the raw bytes is what makes the primary
 * address stable across reboots.
 */
std::vector<std::string> enumerate_mac_addresses() {
    std::vector<std::string> macs;

#if defined(__linux__)
    struct ifaddrs* ifaddr = nullptr;
    if (getifaddrs(&ifaddr) == -1)
        return macs;

    for (auto* ifa = ifaddr; ifa != nullptr; ifa = ifa->ifa_next) {
        if (ifa->ifa_addr == nullptr || (ifa->ifa_flags & IFF_LOOPBACK) != 0)
            continue;

        if (ifa->ifa_addr->sa_family == AF_PACKET) {
            auto* s = reinterpret_cast<struct sockaddr_ll*>(ifa->ifa_addr);
            if (s->sll_halen == 6)
                macs.emplace_back(reinterpret_cast<const char*>(s->sll_addr), 6);
        }
    }

    freeifaddrs(ifaddr);
#elif defined(__APPLE__)
    struct ifaddrs* ifaddr = nullptr;
    if (getifaddrs(&ifaddr) == -1)
        return macs;

    for (auto* ifa = ifaddr; ifa != nullptr; ifa = ifa->ifa_next) {
        if (ifa->ifa_addr == nullptr || (ifa->ifa_flags & IFF_LOOPBACK) != 0)
            continue;

        if (ifa->ifa_addr->sa_family == AF_LINK) {
            auto* sdl = reinterpret_cast<struct sockaddr_dl*>(ifa->ifa_addr);
            if (sdl->sdl_alen == 6)
                macs.emplace_back(LLADDR(sdl), 6);
        }
    }

    freeifaddrs(ifaddr);
#elif defined(_WIN32)
    ULONG buf_len = 0;
    GetAdaptersInfo(nullptr, &buf_len);
    if (buf_len == 0)
        return macs;

    std::vector<std::byte> buffer(buf_len);
    auto* adapter_info = reinterpret_cast<PIP_ADAPTER_INFO>(buffer.data());

    if (GetAdaptersInfo(adapter_info, &buf_len) != ERROR_SUCCESS)
        return macs;

    for (auto* adapter = adapter_info; adapter != nullptr; adapter = adapter->Next) {
        if (adapter->AddressLength == 6)
            macs.emplace_back(reinterpret_cast<const char*>(adapter->Address), 6);
    }
#endif

    std::sort(macs.begin(), macs.end());
    return macs;
}

}

std::string get_hostname() {
#if defined(__linux__)
    std::array<char, HOST_NAME_MAX + 1> buffer{};
    if (gethostname(buffer.data(), buffer.size()) == 0) {
        return std::string(buffer.data());
    }
#elif defined(__APPLE__)
    std::array<char, MAXHOSTNAMELEN + 1> buffer{};
    if (gethostname(buffer.data(), buffer.size()) == 0) {
        return std::string(buffer.data());
    }
#elif defined(_WIN32)
    std::array<char, MAX_COMPUTERNAME_LENGTH + 1> buffer{};
    DWORD size = static_cast<DWORD>(buffer.size());
    if (GetComputerNameA(buffer.data(), &size)) {
        return std::string(buffer.data());
    }
#endif
    return "unknown";
}

std::optional<std::string> get_primary_mac_address() {
    const auto macs = enumerate_mac_addresses();
    if (macs.empty())
        return std::nullopt;

    return format_mac_address(macs.front());
}

std::optional<std::string> get_primary_mac_address_bytes() {
    const auto macs = enumerate_mac_addresses();
    if (macs.empty())
        return std::nullopt;

    return macs.front();
}

std::string derive_machine_id() {
    const auto hostname = get_hostname();
    const auto mac = get_primary_mac_address();

    std::string combined = hostname + ":" + mac.value_or("");
    const std::size_t hash = std::hash<std::string>{}(combined);

    std::ostringstream oss;
    oss << std::hex << std::setfill('0') << std::setw(16) << hash;
    return oss.str();
}

std::uint16_t derive_machine_id_hash() {
    const auto hostname = get_hostname();
    const auto mac = get_primary_mac_address_bytes();

    std::string combined = hostname + mac.value_or("");
    const std::size_t hash = std::hash<std::string>{}(combined);

    return static_cast<std::uint16_t>(hash & 0xFFFF);
}

}
