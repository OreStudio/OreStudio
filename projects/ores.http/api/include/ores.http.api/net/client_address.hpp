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
#ifndef ORES_HTTP_NET_CLIENT_ADDRESS_HPP
#define ORES_HTTP_NET_CLIENT_ADDRESS_HPP

#include "ores.http.api/export.hpp"
#include <boost/asio/ip/address.hpp>
#include <boost/asio/ip/network_v4.hpp>
#include <boost/asio/ip/network_v6.hpp>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::net {

/**
 * @brief The proxies whose forwarded address the server believes.
 *
 * A forwarded address is only a claim, and any client can make it. It names
 * the client only when the peer that made it is a proxy the deployment runs.
 */
class ORES_HTTP_API_EXPORT trusted_proxies final {
public:
    trusted_proxies() = default;

    /**
     * @brief The networks a comma separated list states, such as
     * =127.0.0.1/32,10.0.0.0/8=. An address with no prefix is one host.
     *
     * @throws std::invalid_argument naming the entry that is not a network.
     */
    [[nodiscard]] static trusted_proxies parse(std::string_view list);

    [[nodiscard]] bool contains(const boost::asio::ip::address& a) const;

    [[nodiscard]] bool empty() const {
        return v4_.empty() && v6_.empty();
    }

private:
    std::vector<boost::asio::ip::network_v4> v4_;
    std::vector<boost::asio::ip::network_v6> v6_;
};

/**
 * @brief The client a request comes from, as the rate limiter counts it.
 *
 * The socket's peer, unless the peer is a trusted proxy: then the right-most
 * address in =X-Forwarded-For= that is not a trusted proxy, because each proxy
 * appends the address it was called from, and only the ones the deployment runs
 * can be believed. An IPv6 client is counted by its /64, which is what one
 * subscriber is given, so rotating addresses inside it does not reset a budget.
 *
 * @param forwarded_for The request's X-Forwarded-For header, or empty.
 */
[[nodiscard]] ORES_HTTP_API_EXPORT std::string client_of(const boost::asio::ip::address& peer,
                                                         std::string_view forwarded_for,
                                                         const trusted_proxies& trusted);

}

#endif
