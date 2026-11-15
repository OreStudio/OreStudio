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
#include <boost/asio/ip/address.hpp>
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>

using ores::http::net::client_of;
using ores::http::net::trusted_proxies;

namespace {

boost::asio::ip::address ip(const char* text) {
    return boost::asio::ip::make_address(text);
}

}

TEST_CASE("an_untrusted_peer_is_the_client_whatever_it_forwards", "[client_address]") {
    const auto none = trusted_proxies::parse("");
    CHECK(none.empty());
    CHECK(client_of(ip("203.0.113.9"), "198.51.100.1", none) == "203.0.113.9");
    const auto proxies = trusted_proxies::parse("10.0.0.0/8");
    CHECK(client_of(ip("203.0.113.9"), "198.51.100.1", proxies) == "203.0.113.9");
}

TEST_CASE("a_trusted_proxy_names_the_right_most_untrusted_hop", "[client_address]") {
    const auto proxies = trusted_proxies::parse(" 127.0.0.1 , 10.0.0.0/8 ");
    CHECK(client_of(ip("127.0.0.1"), "198.51.100.1", proxies) == "198.51.100.1");
    // A client can write anything on the left; only the hops the proxies added count.
    CHECK(client_of(ip("127.0.0.1"), "1.2.3.4, 198.51.100.1, 10.1.1.1", proxies) == "198.51.100.1");
    // A proxy that forwards nothing is the client.
    CHECK(client_of(ip("10.2.2.2"), "", proxies) == "10.2.2.2");
    // A hop that is not an address ends the walk at the last trusted hop.
    CHECK(client_of(ip("127.0.0.1"), "198.51.100.1, garbage", proxies) == "127.0.0.1");
}

TEST_CASE("an_ipv6_client_counts_by_its_slash_64", "[client_address]") {
    const auto none = trusted_proxies::parse("");
    CHECK(client_of(ip("2001:db8:1:2:aaaa::1"), "", none) == "2001:db8:1:2::/64");
    CHECK(client_of(ip("2001:db8:1:2:bbbb::9"), "", none) == "2001:db8:1:2::/64");
    CHECK(client_of(ip("::ffff:203.0.113.9"), "", none) == "203.0.113.9");
    const auto v6 = trusted_proxies::parse("::1");
    CHECK(client_of(ip("::1"), "2001:db8::7", v6) == "2001:db8::/64");
}

TEST_CASE("a_trusted_proxy_that_is_not_a_network_is_refused", "[client_address]") {
    CHECK_THROWS_AS(trusted_proxies::parse("not-an-address"), std::invalid_argument);
    CHECK_THROWS_AS(trusted_proxies::parse("10.0.0.0/99"), std::invalid_argument);
}
