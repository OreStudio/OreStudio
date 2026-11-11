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
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/domain/session_cookie.hpp"
#include "ores.http.api/domain/session_store.hpp"
#include "ores.http/routes/iam/browser_session.hpp"
#include "ores.http/routes/iam/browser_session_routes.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <memory>
#include <optional>
#include <string>

using namespace std::chrono_literals;
using ores::http::domain::cookie_settings;
using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::domain::session_record;
using ores::http::domain::session_store;
using ores::http::net::router;
using ores::http::openapi::endpoint_registry;
using ores::http::routes::iam::browser_party;
using ores::http::routes::iam::browser_session;
using ores::http::routes::iam::browser_session_routes;
using ores::nats::service::nats_client;
namespace iam = ores::iam::messaging;

namespace {

const std::string acme_tenant = "00000000-0000-0000-0000-0000000000a1";
const std::string trading_party = "00000000-0000-0000-0000-0000000000b1";

/// The browser session routes over a store a test can fill, on a client that is
/// not connected, so any forward fails.
struct browser_fixture final {
    nats_client session;
    std::shared_ptr<session_store> sessions = std::make_shared<session_store>(1h);
    cookie_settings cookie;
    std::shared_ptr<router> table = std::make_shared<router>();
    // The handlers hold the routes object, so it lives as long as the table.
    std::unique_ptr<browser_session_routes> routes;

    browser_fixture() {
        cookie.name = "ores_test_session";
        routes = std::make_unique<browser_session_routes>(
            session, sessions, [] { return nullptr; }, cookie);
        routes->register_routes(table, std::make_shared<endpoint_registry>());
    }

    std::string open(const browser_session& view) {
        session_record record;
        record.token = "the-token";
        record.claims.subject = "acc-1";
        record.view = ores::utility::codec::json::write(view);
        return sessions->open(std::move(record));
    }

    http_response answer(http_method method,
                         const std::string& pattern,
                         std::optional<std::string> identifier = std::nullopt,
                         std::string body = {}) {
        const auto route = std::ranges::find_if(table->routes(), [&](const auto& r) {
            return r.method == method && r.pattern == pattern;
        });
        REQUIRE(route != table->routes().end());
        http_request req;
        req.body = std::move(body);
        if (identifier)
            req.headers["cookie"] = cookie.name + "=" + *identifier;
        boost::asio::io_context io;
        std::optional<http_response> out;
        boost::asio::co_spawn(
            io,
            [&]() -> boost::asio::awaitable<void> { out = co_await route->handler(req); },
            boost::asio::detached);
        io.run();
        REQUIRE(out.has_value());
        return *out;
    }
};

iam::login_response a_login(bool with_party) {
    iam::login_response login;
    login.success = true;
    login.token = "t";
    login.account_id = "acc-1";
    login.tenant_id = acme_tenant;
    login.tenant_name = "Acme";
    login.username = "jo";
    login.email = "jo@acme.io";
    login.version = "0.0.30";
    login.database.fingerprint = "fp";
    login.available_parties.push_back({trading_party, "Trading", "Operational", "GBLO"});
    login.access_lifetime_s = 1800;
    login.password_reset_required = true;
    if (with_party)
        login.selected_party_id = trading_party;
    else
        login.default_party_id = trading_party;
    return login;
}

const std::string acme_view =
    R"("username":"jo","email":"jo@acme.io","accountId":"acc-1","tenantId":")" + acme_tenant +
    R"(","tenantName":"Acme","mode":"application","tenantBootstrapping":false,)"
    R"("version":"0.0.30","database":{"fingerprint":"fp","environment":"","commit":"",)"
    R"("created":""},"party":{"id":")" +
    trading_party +
    R"(","name":"Trading","partyCategory":"Operational","businessCenterCode":"GBLO"},)"
    R"("availableParties":[{"id":")" +
    trading_party +
    R"(","name":"Trading","partyCategory":"Operational","businessCenterCode":"GBLO"}],)"
    R"("accessLifetimeSeconds":1800,"passwordResetRequired":true)";

}

TEST_CASE("browser_session_routes_answer_a_login_as_the_bff_did", "[browser_session]") {
    CHECK(browser_session_routes::login_json(a_login(true)) ==
          R"({"outcome":"active","session":{)" + acme_view + "}}");
    CHECK(browser_session_routes::login_json(a_login(false)) ==
          R"({"outcome":"party-required","username":"jo","email":"jo@acme.io",)"
          R"("accountId":"acc-1","tenantName":"Acme","tenantBootstrapping":false,)"
          R"("version":"0.0.30","database":{"fingerprint":"fp","environment":"","commit":"",)"
          R"("created":""},"availableParties":[{"id":")" +
              trading_party +
              R"(","name":"Trading","partyCategory":"Operational","businessCenterCode":"GBLO"}],)"
              R"("defaultPartyId":")" +
              trading_party + R"(","passwordResetRequired":true,"accessLifetimeSeconds":1800})");
    auto system = a_login(true);
    system.tenant_id = "ffffffff-ffff-ffff-ffff-ffffffffffff";
    CHECK(ores::http::routes::iam::session_of(system).mode == "system-administration");
}

TEST_CASE("browser_session_routes_read_the_session_the_cookie_names", "[browser_session]") {
    browser_fixture f;
    const auto no_cookie = f.answer(http_method::get, "/api/session");
    CHECK(no_cookie.status == http_status::unauthorized);
    CHECK(no_cookie.body.find("not-authenticated") != std::string::npos);

    auto waiting = ores::http::routes::iam::session_of(a_login(false));
    CHECK(f.answer(http_method::get, "/api/session", f.open(waiting)).status ==
          http_status::unauthorized);

    const auto active = f.answer(http_method::get,
                                 "/api/session",
                                 f.open(ores::http::routes::iam::session_of(a_login(true))));
    CHECK(active.status == http_status::ok);
    CHECK(active.body == "{" + acme_view + "}");
}

TEST_CASE("browser_session_routes_sign_out_ends_the_session_and_clears_the_cookie",
          "[browser_session]") {
    browser_fixture f;
    const auto id = f.open(ores::http::routes::iam::session_of(a_login(true)));
    const auto out = f.answer(http_method::delete_, "/api/session", id);
    CHECK(out.status == http_status::ok);
    CHECK(out.body == R"({"ok":true})");
    CHECK(out.headers.at("Set-Cookie").find("ores_test_session=;") != std::string::npos);
    CHECK_FALSE(f.sessions->find(id).has_value());
    // A browser with no session is still told to drop its cookie.
    CHECK(f.answer(http_method::delete_, "/api/session").status == http_status::ok);
}

TEST_CASE("browser_session_routes_refuse_before_forwarding", "[browser_session]") {
    browser_fixture f;
    CHECK(
        f.answer(http_method::post, "/api/session", std::nullopt, R"({"username":"jo"})").status ==
        http_status::bad_request);
    CHECK(f.answer(http_method::post, "/api/session/party", std::nullopt, R"({"partyId":"p"})")
              .status == http_status::unauthorized);
    const auto id = f.open(ores::http::routes::iam::session_of(a_login(false)));
    CHECK(f.answer(http_method::post, "/api/session/party", id, R"({"partyId":""})").status ==
          http_status::bad_request);
    CHECK(f.answer(http_method::post, "/api/session/switch-party", id, "{}").status ==
          http_status::bad_request);
    CHECK(f.answer(http_method::post, "/api/account/password", id, R"({"currentPassword":"a"})")
              .status == http_status::bad_request);
    // The client is not connected, so an accepted request fails at the forward step.
    CHECK(f.answer(http_method::post,
                   "/api/session",
                   std::nullopt,
                   R"({"username":"jo","password":"pw"})")
              .status == http_status::bad_gateway);
    CHECK(f.answer(http_method::post, "/api/session/party", id, R"({"partyId":"p"})").status ==
          http_status::bad_gateway);
}

TEST_CASE("browser_session_routes_open_only_the_door", "[browser_session]") {
    browser_fixture f;
    const auto find = [&](http_method m, const std::string& p) {
        return *std::ranges::find_if(
            f.table->routes(), [&](const auto& r) { return r.method == m && r.pattern == p; });
    };
    const auto sign_in = find(http_method::post, "/api/session");
    CHECK_FALSE(sign_in.requires_auth);
    CHECK(sign_in.rate_policy == "credential");
    CHECK_FALSE(find(http_method::get, "/api/session").requires_auth);
    CHECK_FALSE(find(http_method::delete_, "/api/session").requires_auth);
    CHECK(find(http_method::post, "/api/session/party").requires_auth);
    CHECK(find(http_method::post, "/api/session/switch-party").requires_auth);
    CHECK(find(http_method::post, "/api/account/password").requires_auth);
}
