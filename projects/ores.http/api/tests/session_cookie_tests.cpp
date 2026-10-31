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
#include "ores.http.api/domain/session_cookie.hpp"
#include "ores.logging/make_logger.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using namespace ores::http::domain;
using namespace ores::logging;

namespace {

cookie_settings plain_settings() {
    cookie_settings settings;
    settings.name = "ores_web_session_clever_dijkstra";
    settings.secure = false;
    return settings;
}

cookie_settings tls_settings() {
    auto settings = plain_settings();
    settings.secure = true;
    return settings;
}

}

TEST_CASE("a_session_cookie_is_httponly_and_same_site", "[session_cookie]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the flags a session cookie carries";

    const auto issued = session_cookie::issue(plain_settings(), "the-identifier");

    REQUIRE(issued.find("ores_web_session_clever_dijkstra=the-identifier") == 0);
    REQUIRE(issued.find("HttpOnly") != std::string::npos);
    REQUIRE(issued.find("SameSite=Lax") != std::string::npos);
    REQUIRE(issued.find("Path=/") != std::string::npos);
    // No TLS, so no Secure: a browser would refuse a Secure cookie over plain HTTP.
    REQUIRE(issued.find("Secure") == std::string::npos);
}

TEST_CASE("over_tls_the_cookie_is_secure_and_prefixed_for_the_host", "[session_cookie]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the prefix a Secure cookie requires";

    const auto settings = tls_settings();
    const auto issued = session_cookie::issue(settings, "the-identifier");

    REQUIRE(session_cookie::issue_name(settings) == "__Host-ores_web_session_clever_dijkstra");
    REQUIRE(issued.find("__Host-ores_web_session_clever_dijkstra=the-identifier") == 0);
    REQUIRE(issued.find("Secure") != std::string::npos);
    REQUIRE(issued.find("Path=/") != std::string::npos);
    // The prefix forbids a Domain attribute, so none is written.
    REQUIRE(issued.find("Domain") == std::string::npos);
}

TEST_CASE("the_identifier_is_read_back_from_a_cookie_header", "[session_cookie]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the parse, which has to survive other cookies";

    const auto settings = plain_settings();

    REQUIRE(session_cookie::read("ores_web_session_clever_dijkstra=the-identifier", settings) ==
            "the-identifier");
    REQUIRE(session_cookie::read("theme=dark; ores_web_session_clever_dijkstra=abc; locale=en",
                                 settings) == "abc");
    REQUIRE(session_cookie::read(" ores_web_session_clever_dijkstra = spaced ", settings) ==
            "spaced");
    REQUIRE(!session_cookie::read("theme=dark; locale=en", settings).has_value());
    REQUIRE(!session_cookie::read("", settings).has_value());
    REQUIRE(!session_cookie::read("ores_web_session_clever_dijkstra=", settings).has_value());
}

TEST_CASE("a_cookie_for_another_deployment_is_not_this_session", "[session_cookie]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that two deployments on a host cannot swap sessions";

    auto settings = plain_settings();
    REQUIRE(!session_cookie::read("ores_web_session_other_environment=abc", settings).has_value());

    // And a Secure deployment does not read the unprefixed name, because the browser
    // would not have sent that one to it.
    REQUIRE(
        !session_cookie::read("ores_web_session_clever_dijkstra=abc", tls_settings()).has_value());
}

TEST_CASE("clearing_a_cookie_expires_it_with_the_same_flags", "[session_cookie]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that signing out removes what signing in set";

    for (const auto& settings : {plain_settings(), tls_settings()}) {
        const auto cleared = session_cookie::clear(settings);
        REQUIRE(cleared.find(session_cookie::issue_name(settings) + "=;") == 0);
        REQUIRE(cleared.find("Max-Age=0") != std::string::npos);
        REQUIRE(cleared.find("HttpOnly") != std::string::npos);
        REQUIRE(cleared.find("SameSite=Lax") != std::string::npos);
        REQUIRE((cleared.find("Secure") != std::string::npos) == settings.secure);
    }
}
