/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.http.api/net/client_address.hpp"
#include <boost/asio/ip/address_v6.hpp>
#include <stdexcept>

namespace ores::http::net {

namespace {

namespace ip = boost::asio::ip;

std::string_view trimmed_view(std::string_view text) {
    while (!text.empty() && (text.front() == ' ' || text.front() == '\t'))
        text.remove_prefix(1);
    while (!text.empty() && (text.back() == ' ' || text.back() == '\t'))
        text.remove_suffix(1);
    return text;
}

/// An IPv4-mapped IPv6 address is the IPv4 client it maps.
ip::address unmapped(const ip::address& a) {
    if (a.is_v6() && a.to_v6().is_v4_mapped())
        return ip::make_address_v4(ip::v4_mapped, a.to_v6());
    return a;
}

std::string key_of(const ip::address& a) {
    if (a.is_v4())
        return a.to_string();
    auto bytes = a.to_v6().to_bytes();
    std::fill(bytes.begin() + 8, bytes.end(), 0);
    return ip::address_v6(bytes).to_string() + "/64";
}

}

trusted_proxies trusted_proxies::parse(std::string_view list) {
    trusted_proxies r;
    while (!list.empty()) {
        const auto comma = list.find(',');
        const auto entry = trimmed_view(list.substr(0, comma));
        list = comma == std::string_view::npos ? std::string_view{} : list.substr(comma + 1);
        if (entry.empty())
            continue;
        const std::string text(entry);
        boost::system::error_code ec;
        if (text.find(':') != std::string::npos) {
            const auto net = text.find('/') == std::string::npos ?
                                 ip::network_v6(ip::make_address_v6(text, ec), 128) :
                                 ip::make_network_v6(text, ec);
            if (ec)
                throw std::invalid_argument("A trusted proxy is not a network: " + text);
            r.v6_.push_back(net);
        } else {
            const auto net = text.find('/') == std::string::npos ?
                                 ip::network_v4(ip::make_address_v4(text, ec), 32) :
                                 ip::make_network_v4(text, ec);
            if (ec)
                throw std::invalid_argument("A trusted proxy is not a network: " + text);
            r.v4_.push_back(net);
        }
    }
    return r;
}

bool trusted_proxies::contains(const ip::address& address) const {
    const auto a = unmapped(address);
    if (a.is_v4()) {
        const ip::network_v4 host(a.to_v4(), 32);
        for (const auto& net : v4_)
            if (host == net || host.is_subnet_of(net))
                return true;
        return false;
    }
    const ip::network_v6 host(a.to_v6(), 128);
    for (const auto& net : v6_)
        if (host == net || host.is_subnet_of(net))
            return true;
    return false;
}

std::string
client_of(const ip::address& peer, std::string_view forwarded_for, const trusted_proxies& trusted) {
    auto client = unmapped(peer);
    if (!trusted.contains(client))
        return key_of(client);
    while (!forwarded_for.empty()) {
        const auto comma = forwarded_for.rfind(',');
        const auto entry = trimmed_view(
            comma == std::string_view::npos ? forwarded_for : forwarded_for.substr(comma + 1));
        forwarded_for =
            comma == std::string_view::npos ? std::string_view{} : forwarded_for.substr(0, comma);
        boost::system::error_code ec;
        const auto hop = ip::make_address(std::string(entry), ec);
        if (ec)
            break;
        client = unmapped(hop);
        if (!trusted.contains(client))
            return key_of(client);
    }
    return key_of(client);
}

}
