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
#include "ores.utility/net/network_info.hpp"

#include <algorithm>
#include <array>
#include <functional>
#include <iomanip>
#include <sstream>
#include <vector>

#include <boost/process/environment.hpp>

#if defined(__linux__)
#include <unistd.h>
#include <limits.h>
#include <sys/types.h>
#include <ifaddrs.h>
#include <net/if.h>
#include <netpacket/packet.h>
#elif defined(__APPLE__)
#include <unistd.h>
#include <sys/types.h>
#include <sys/param.h>
#include <ifaddrs.h>
#include <net/if.h>
#include <net/if_dl.h>
#elif defined(_WIN32)
#include <windows.h>
#include <iphlpapi.h>
#pragma comment(lib, "iphlpapi.lib")
#endif

namespace ores::utility::net {

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
#if defined(__linux__)
    struct ifaddrs* ifaddr = nullptr;
    if (getifaddrs(&ifaddr) == -1) {
        return std::nullopt;
    }

    std::vector<std::string> macs;
    for (auto* ifa = ifaddr; ifa != nullptr; ifa = ifa->ifa_next) {
        if (ifa->ifa_addr == nullptr || (ifa->ifa_flags & IFF_LOOPBACK) != 0) {
            continue;
        }

        if (ifa->ifa_addr->sa_family == AF_PACKET) {
            auto* s = reinterpret_cast<struct sockaddr_ll*>(ifa->ifa_addr);
            if (s->sll_halen == 6) {
                std::ostringstream oss;
                oss << std::hex << std::setfill('0');
                for (int i = 0; i < 6; ++i) {
                    if (i > 0) oss << ':';
                    oss << std::setw(2)
                        << static_cast<unsigned>(s->sll_addr[i]);
                }
                macs.push_back(oss.str());
            }
        }
    }

    freeifaddrs(ifaddr);

    if (macs.empty()) {
        return std::nullopt;
    }

    std::sort(macs.begin(), macs.end());
    return macs.front();

#elif defined(__APPLE__)
    struct ifaddrs* ifaddr = nullptr;
    if (getifaddrs(&ifaddr) == -1) {
        return std::nullopt;
    }

    std::vector<std::string> macs;
    for (auto* ifa = ifaddr; ifa != nullptr; ifa = ifa->ifa_next) {
        if (ifa->ifa_addr == nullptr || (ifa->ifa_flags & IFF_LOOPBACK) != 0) {
            continue;
        }

        if (ifa->ifa_addr->sa_family == AF_LINK) {
            auto* sdl = reinterpret_cast<struct sockaddr_dl*>(ifa->ifa_addr);
            if (sdl->sdl_alen == 6) {
                auto* mac_ptr = reinterpret_cast<unsigned char*>(
                    LLADDR(sdl));
                std::ostringstream oss;
                oss << std::hex << std::setfill('0');
                for (int i = 0; i < 6; ++i) {
                    if (i > 0) oss << ':';
                    oss << std::setw(2) << static_cast<unsigned>(mac_ptr[i]);
                }
                macs.push_back(oss.str());
            }
        }
    }

    freeifaddrs(ifaddr);

    if (macs.empty()) {
        return std::nullopt;
    }

    std::sort(macs.begin(), macs.end());
    return macs.front();

#elif defined(_WIN32)
    ULONG buf_len = 0;
    GetAdaptersInfo(nullptr, &buf_len);
    if (buf_len == 0) {
        return std::nullopt;
    }

    std::vector<std::byte> buffer(buf_len);
    auto* adapter_info = reinterpret_cast<PIP_ADAPTER_INFO>(buffer.data());

    if (GetAdaptersInfo(adapter_info, &buf_len) != ERROR_SUCCESS) {
        return std::nullopt;
    }

    std::vector<std::string> macs;
    for (auto* adapter = adapter_info; adapter != nullptr;
         adapter = adapter->Next) {
        if (adapter->AddressLength == 6) {
            std::ostringstream oss;
            oss << std::hex << std::setfill('0');
            for (UINT i = 0; i < adapter->AddressLength; ++i) {
                if (i > 0) oss << ':';
                oss << std::setw(2)
                    << static_cast<unsigned>(adapter->Address[i]);
            }
            macs.push_back(oss.str());
        }
    }

    if (macs.empty()) {
        return std::nullopt;
    }

    std::sort(macs.begin(), macs.end());
    return macs.front();
#else
    return std::nullopt;
#endif
}

std::optional<std::string> get_primary_mac_address_bytes() {
#if defined(__linux__)
    struct ifaddrs* ifaddr = nullptr;
    if (getifaddrs(&ifaddr) == -1) {
        return std::nullopt;
    }

    std::vector<std::string> macs;
    for (auto* ifa = ifaddr; ifa != nullptr; ifa = ifa->ifa_next) {
        if (ifa->ifa_addr == nullptr || (ifa->ifa_flags & IFF_LOOPBACK) != 0) {
            continue;
        }

        if (ifa->ifa_addr->sa_family == AF_PACKET) {
            auto* s = reinterpret_cast<struct sockaddr_ll*>(ifa->ifa_addr);
            if (s->sll_halen == 6) {
                std::string mac_bytes;
                mac_bytes.reserve(6);
                for (int i = 0; i < 6; ++i) {
                    mac_bytes += static_cast<char>(s->sll_addr[i]);
                }
                macs.push_back(mac_bytes);
            }
        }
    }

    freeifaddrs(ifaddr);

    if (macs.empty()) {
        return std::nullopt;
    }

    std::sort(macs.begin(), macs.end());
    return macs.front();

#elif defined(__APPLE__)
    struct ifaddrs* ifaddr = nullptr;
    if (getifaddrs(&ifaddr) == -1) {
        return std::nullopt;
    }

    std::vector<std::string> macs;
    for (auto* ifa = ifaddr; ifa != nullptr; ifa = ifa->ifa_next) {
        if (ifa->ifa_addr == nullptr || (ifa->ifa_flags & IFF_LOOPBACK) != 0) {
            continue;
        }

        if (ifa->ifa_addr->sa_family == AF_LINK) {
            auto* sdl = reinterpret_cast<struct sockaddr_dl*>(ifa->ifa_addr);
            if (sdl->sdl_alen == 6) {
                auto* mac_ptr = reinterpret_cast<unsigned char*>(LLADDR(sdl));
                std::string mac_bytes;
                mac_bytes.reserve(6);
                for (int i = 0; i < 6; ++i) {
                    mac_bytes += static_cast<char>(mac_ptr[i]);
                }
                macs.push_back(mac_bytes);
            }
        }
    }

    freeifaddrs(ifaddr);

    if (macs.empty()) {
        return std::nullopt;
    }

    std::sort(macs.begin(), macs.end());
    return macs.front();

#elif defined(_WIN32)
    ULONG buf_len = 0;
    GetAdaptersInfo(nullptr, &buf_len);
    if (buf_len == 0) {
        return std::nullopt;
    }

    std::vector<std::byte> buffer(buf_len);
    auto* adapter_info = reinterpret_cast<PIP_ADAPTER_INFO>(buffer.data());

    if (GetAdaptersInfo(adapter_info, &buf_len) != ERROR_SUCCESS) {
        return std::nullopt;
    }

    std::vector<std::string> macs;
    for (auto* adapter = adapter_info; adapter != nullptr;
         adapter = adapter->Next) {
        if (adapter->AddressLength == 6) {
            std::string mac_bytes;
            mac_bytes.reserve(6);
            for (UINT i = 0; i < adapter->AddressLength; ++i) {
                mac_bytes += static_cast<char>(adapter->Address[i]);
            }
            macs.push_back(mac_bytes);
        }
    }

    if (macs.empty()) {
        return std::nullopt;
    }

    std::sort(macs.begin(), macs.end());
    return macs.front();
#else
    return std::nullopt;
#endif
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

std::int64_t get_process_id() {
    return static_cast<std::int64_t>(
        boost::this_process::get_id());
}

}
