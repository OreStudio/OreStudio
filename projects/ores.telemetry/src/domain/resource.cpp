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
#include "ores.telemetry/domain/resource.hpp"
#include <algorithm>
#include <array>
#include <fstream>
#include <functional>
#include <iomanip>
#include <sstream>
#include <vector>

#ifdef __linux__
#include <unistd.h>
#include <limits.h>
#include <sys/types.h>
#include <ifaddrs.h>
#include <net/if.h>
#include <netpacket/packet.h>
#endif

namespace ores::telemetry::domain {

namespace {

std::string get_hostname() {
#ifdef __linux__
    std::array<char, HOST_NAME_MAX + 1> buffer{};
    if (gethostname(buffer.data(), buffer.size()) == 0) {
        return std::string(buffer.data());
    }
#endif
    return "unknown";
}

std::string get_first_mac_address() {
#ifdef __linux__
    struct ifaddrs* ifaddr = nullptr;
    if (getifaddrs(&ifaddr) == -1) {
        return "";
    }

    // Collect all non-loopback MAC addresses
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
        return "";
    }

    // Sort to ensure stable ordering across reboots
    std::sort(macs.begin(), macs.end());
    return macs.front();
#else
    return "";
#endif
}

std::string derive_host_id(const std::string& hostname,
                           const std::string& mac_address) {
    // Combine hostname and MAC address, then hash
    std::string combined = hostname + ":" + mac_address;
    std::size_t hash = std::hash<std::string>{}(combined);

    std::ostringstream oss;
    oss << std::hex << std::setfill('0') << std::setw(16) << hash;
    return oss.str();
}

}

std::optional<std::string> resource::service_name() const {
    auto it = attrs.find("service.name");
    if (it != attrs.end()) {
        if (auto* str = std::get_if<std::string>(&it->second)) {
            return *str;
        }
    }
    return std::nullopt;
}

std::optional<std::string> resource::host_name() const {
    auto it = attrs.find("host.name");
    if (it != attrs.end()) {
        if (auto* str = std::get_if<std::string>(&it->second)) {
            return *str;
        }
    }
    return std::nullopt;
}

std::optional<std::string> resource::host_id() const {
    auto it = attrs.find("host.id");
    if (it != attrs.end()) {
        if (auto* str = std::get_if<std::string>(&it->second)) {
            return *str;
        }
    }
    return std::nullopt;
}

resource resource::from_environment(std::string_view service_name,
                                    std::string_view service_version) {
    resource res;

    // Service attributes
    res.attrs["service.name"] = std::string(service_name);
    res.attrs["service.version"] = std::string(service_version);

    // Host attributes (locally derived)
    const auto hostname = get_hostname();
    res.attrs["host.name"] = hostname;

    const auto mac = get_first_mac_address();
    if (!mac.empty()) {
        res.attrs["host.id"] = derive_host_id(hostname, mac);
    } else {
        // Fallback to just hostname hash if no MAC available
        res.attrs["host.id"] = derive_host_id(hostname, "");
    }

    // Process attributes
#ifdef __linux__
    res.attrs["process.pid"] = static_cast<std::int64_t>(getpid());
#endif

    return res;
}

}
