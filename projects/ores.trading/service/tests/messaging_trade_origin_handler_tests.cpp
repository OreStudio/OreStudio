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
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/generators/book_generator.hpp"
#include "ores.refdata.api/generators/currency_generator.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.api/generators/portfolio_generator.hpp"
#include "ores.refdata.core/repository/book_repository.hpp"
#include "ores.refdata.core/repository/currency_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.refdata.core/repository/portfolio_repository.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.trading.api/domain/origin.hpp"
#include "ores.trading.api/generators/trade_booking_generator.hpp"
#include "ores.trading.api/generators/trade_generator.hpp"
#include "ores.trading.core/messaging/trade_operations_handler.hpp"
#include "ores.trading.core/repository/trade_repository.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

/*
 * Proves the origin gate end to end over real NATS: a request with a bearer
 * token goes to the handler on its own subject, the handler resolves the
 * caller's permissions from the token and decides whether the origin it states
 * is one it may state. The subject is test-scoped, because the running service
 * subscribes to the real subject on the same queue group and its own verifier
 * would refuse the test token.
 */

namespace {

const std::string tags("[trade_origin_handler][integration]");

const std::string test_subject("ores.test.trade_operations.book_trade");
const std::string write_permission("trading::trades:write");
const std::string service_origin_permission("trading::trades:write_service_origin");
const std::string test_secret("trading-origin-handler-test-secret");
const std::string test_issuer("ores.iam.test");
const std::string test_audience("ores.trading.service");

using ores::trading::messaging::book_trade_request;
using ores::trading::messaging::book_trade_response;
using ores::utility::domain::outcome;

/// A tenant with a party, a currency, a portfolio and a book of that party.
struct handler_fixture {
    ores::testing::scoped_database_helper db;
    ores::utility::generation::generation_context gen;
    ores::database::context ctx = db.context();
    ores::nats::service::client nats;
    std::optional<ores::security::jwt::jwt_authenticator> verifier;
    std::optional<ores::nats::service::subscription> sub;
    boost::uuids::uuid party_id{};
    boost::uuids::uuid book_id{};
    std::string actor;

    handler_fixture()
        : gen(ores::testing::make_generation_context(db))
        , nats(ores::testing::make_nats_options()) {
        namespace gen_rd = ores::refdata::generators;
        namespace repo = ores::refdata::repository;

        actor = db.db_user();

        auto party = gen_rd::generate_synthetic_party(gen);
        party.change_reason_code = "system.test";
        for (const auto& e : repo::party_repository().read_latest(db.context())) {
            if (e.tenant_id == party.tenant_id) {
                party.parent_party_id = e.id;
                break;
            }
        }
        repo::party_repository().write(db.context(), party);
        party_id = party.id;
        ctx = db.context().with_party(db.tenant_id(), party.id, {party.id}, actor);

        auto currency = gen_rd::generate_synthetic_currency(gen);
        repo::currency_repository().write(ctx, {currency});

        auto portfolio = gen_rd::generate_synthetic_portfolio(gen);
        portfolio.party_id = party.id;
        portfolio.change_reason_code = "system.test";
        repo::portfolio_repository().write(ctx, portfolio);

        auto book = gen_rd::generate_synthetic_book(gen);
        book.party_id = party.id;
        book.parent_portfolio_id = portfolio.id;
        book.functional_currency = currency.iso_code;
        book.change_reason_code = "system.test";
        repo::book_repository().write(ctx, book);
        book_id = book.id;

        nats.connect();
        REQUIRE(nats.is_connected());
        verifier = ores::security::jwt::jwt_authenticator::create_hs256(
            test_secret, test_issuer, test_audience);
        sub = nats.queue_subscribe(
            test_subject, "ores.trading.service", [this](ores::nats::message msg) {
                ores::trading::messaging::trade_operations_handler h(
                    nats, db.context(), verifier);
                h.book_trade(std::move(msg));
            });
    }

    /// A token the handler's verifier accepts, holding exactly @p roles.
    std::string token(const std::vector<std::string>& roles) {
        auto signer = ores::security::jwt::jwt_authenticator::create_hs256(
            test_secret, test_issuer, test_audience);
        auto claims = ores::security::jwt::jwt_claims::with_ttl(std::chrono::minutes(5));
        claims.subject = "trade-origin-test";
        claims.issuer = test_issuer;
        claims.audience = test_audience;
        claims.tenant_id = db.tenant_id().to_string();
        claims.party_id = boost::uuids::to_string(party_id);
        claims.visible_party_ids = {boost::uuids::to_string(party_id)};
        claims.roles = roles;
        return *signer.create_token(claims);
    }

    book_trade_request request() {
        book_trade_request r;
        r.anchor = ores::trading::generators::generate_synthetic_trade(gen);
        r.booking = ores::trading::generators::generate_synthetic_trade_booking(gen);
        r.booking.book_id = book_id;
        r.booking.change_reason_code = "system.test";
        r.activity_type_code = "new_booking";
        return r;
    }

    ores::nats::message send(const book_trade_request& req, const std::string& bearer) {
        std::unordered_map<std::string, std::string> headers;
        headers[std::string(ores::nats::headers::authorization)] =
            std::string(ores::nats::headers::bearer_prefix) + bearer;
        return nats.request_sync(
            test_subject, ores::nats::default_wire_codec().encode(req), headers);
    }

    bool stored(const boost::uuids::uuid& id) const {
        return !ores::trading::repository::trade_repository()
                    .read_latest(ctx, boost::uuids::to_string(id))
                    .empty();
    }
};

bool refused_forbidden(const ores::nats::message& reply) {
    const auto it = reply.headers.find(std::string(ores::nats::headers::x_error));
    return it != reply.headers.end() && it->second == "forbidden";
}

}

TEST_CASE("a_token_without_the_service_origin_permission_may_not_state_synthetic", tags) {
    handler_fixture f;
    auto req = f.request();
    req.anchor.origin = ores::trading::domain::origin::synthetic;
    req.anchor.origin_reference = "11111111-2222-3333-4444-555555555555";

    const auto reply = f.send(req, f.token({write_permission}));

    CHECK(refused_forbidden(reply));
    CHECK_FALSE(f.stored(req.anchor.id));
}

TEST_CASE("a_token_without_the_service_origin_permission_may_not_state_ore_import", tags) {
    handler_fixture f;
    auto req = f.request();
    req.anchor.origin = ores::trading::domain::origin::ore_import;
    req.anchor.origin_reference = "ORE-TRADE-1";

    const auto reply = f.send(req, f.token({write_permission}));

    CHECK(refused_forbidden(reply));
    CHECK_FALSE(f.stored(req.anchor.id));
}

TEST_CASE("a_token_with_the_service_origin_permission_may_state_synthetic", tags) {
    handler_fixture f;
    auto req = f.request();
    req.anchor.origin = ores::trading::domain::origin::synthetic;
    req.anchor.origin_reference = "11111111-2222-3333-4444-555555555555";

    const auto reply = f.send(req, f.token({write_permission, service_origin_permission}));

    REQUIRE_FALSE(refused_forbidden(reply));
    const auto decoded = ores::nats::default_wire_codec().decode<book_trade_response>(reply.data);
    REQUIRE(decoded);
    REQUIRE(decoded->result.outcome == outcome::ok);

    const auto rows = ores::trading::repository::trade_repository().read_latest(
        f.ctx, boost::uuids::to_string(req.anchor.id));
    REQUIRE(rows.size() == 1);
    CHECK(rows.front().origin == ores::trading::domain::origin::synthetic);
    CHECK(rows.front().origin_reference == "11111111-2222-3333-4444-555555555555");
}

TEST_CASE("a_token_without_the_service_origin_permission_may_state_api", tags) {
    handler_fixture f;
    auto req = f.request();
    req.anchor.origin = ores::trading::domain::origin::api;

    const auto reply = f.send(req, f.token({write_permission}));

    REQUIRE_FALSE(refused_forbidden(reply));
    const auto decoded = ores::nats::default_wire_codec().decode<book_trade_response>(reply.data);
    REQUIRE(decoded);
    REQUIRE(decoded->result.outcome == outcome::ok);
}
