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
#include "ores.http/routes/iam/auth_session_routes.hpp"
#include "ores.iam.api/messaging/login_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/jwks.hpp"
#include "ores.nats/service/timeouts.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cstdlib>
#include <optional>
#include <rfl/json.hpp>
#include <string>

using namespace ores::http::domain;
using namespace ores::http::routes::iam;
using namespace ores::logging;

namespace {

/// An environment variable, or nothing when the deployment has not set it.
std::optional<std::string> env(const char* name) {
    if (const auto* value = std::getenv(name); value != nullptr && *value != '\0')
        return std::string(value);
    return std::nullopt;
}

/**
 * @brief A client for the running fleet, or nothing when there is none.
 *
 * The test needs the fleet this checkout's `.env` describes: NATS, a client
 * certificate, and the account a person signs in with.
 */
std::optional<ores::nats::config::nats_options> fleet_options() {
    auto url = env("ORES_NATS_URL");
    auto ca = env("ORES_SHELL_NATS_TLS_CA");
    auto cert = env("ORES_SHELL_NATS_TLS_CERT");
    auto key = env("ORES_SHELL_NATS_TLS_KEY");
    auto prefix = env("ORES_SHELL_NATS_SUBJECT_PREFIX");
    if (!url || !ca || !cert || !key)
        return std::nullopt;

    ores::nats::config::nats_options options;
    options.url = *url;
    options.tls_ca_cert = *ca;
    options.tls_client_cert = *cert;
    options.tls_client_key = *key;
    options.subject_prefix = prefix.value_or(std::string{});
    return options;
}

/// The public key IAM publishes, so this process can verify what IAM mints.
std::optional<std::string> fetch_public_key(ores::nats::service::nats_client& session) {
    std::optional<std::string> key;
    asio::io_context io;
    asio::co_spawn(
        io,
        [&]() -> asio::awaitable<void> {
            key = co_await ores::nats::service::fetch_jwks_public_key(session.transport(),
                                                                      std::chrono::seconds(5));
        },
        asio::detached);
    io.run();
    return key;
}

}

TEST_CASE("a_person_signs_in_and_the_token_stays_with_the_server", "[auth_session][live]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the sign-in round trip against the running fleet";

    const auto options = fleet_options();
    const auto username = env("ORES_SHELL_LOGIN_USERNAME");
    const auto password = env("ORES_SHELL_LOGIN_PASSWORD");
    if (!options || !username || !password)
        SKIP("This checkout has no fleet to sign in to");

    ores::nats::service::nats_client session;
    try {
        session.connect(*options);
    } catch (const std::exception& e) {
        // A checkout with no fleet running is the normal state of a working tree,
        // and a sign-in test that failed there would be noise rather than a signal.
        SKIP(std::string("The fleet is not reachable: ") + e.what());
    }

    const auto public_key = fetch_public_key(session);
    REQUIRE(public_key.has_value());

    auto store = std::make_shared<session_store>(std::chrono::hours(1));
    cookie_settings settings;
    settings.name = "ores_web_session_test";

    auto verifier = std::make_shared<ores::security::jwt::jwt_authenticator>(
        ores::security::jwt::jwt_authenticator::create_rs256_verifier(*public_key));
    auth_session_routes routes(session, store, verifier, settings);

    ores::iam::messaging::login_request credentials;
    credentials.principal = *username;
    credentials.password = *password;

    http_request req;
    req.method = http_method::post;
    req.body = rfl::json::write(credentials);

    std::optional<http_response> answer;
    asio::io_context io;
    asio::co_spawn(
        io,
        [&]() -> asio::awaitable<void> { answer = co_await routes.sign_in(req); },
        asio::detached);
    io.run();

    REQUIRE(answer.has_value());
    INFO("the door answered: " << static_cast<int>(answer->status) << " " << answer->body);
    REQUIRE(answer->status == http_status::ok);

    // The browser holds a cookie, and the token is somewhere else.
    const auto cookie = answer->headers.find("Set-Cookie");
    REQUIRE(cookie != answer->headers.end());
    REQUIRE(cookie->second.find(settings.name + "=") == 0);
    REQUIRE(answer->body.find("token") == std::string::npos);

    // The session it named is the one the gateway holds.
    REQUIRE(store->size() == 1);
    const auto opened = answer->body.find(R"("subject":")");
    REQUIRE(opened != std::string::npos);
    REQUIRE(answer->body.find(*username) != std::string::npos);

    session.disconnect();
}
