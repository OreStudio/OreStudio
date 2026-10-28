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
#include "ores.marketdata.api/generators/market_fixing_generator.hpp"
#include "ores.marketdata.api/generators/market_series_generator.hpp"
#include "ores.marketdata.api/messaging/fixing_read_operations_protocol.hpp"
#include "ores.marketdata.core/messaging/fixing_read_handler.hpp"
#include "ores.marketdata.core/repository/market_fixing_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include "tick_handler_support.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

// Proves the fixing read end to end over real NATS: the handler checks the read permission of
// the fixings and the reply decodes on the wire. The subject is test-scoped, as the running
// service holds the real one.

namespace fixing_read_handler_test {

using namespace std::chrono;
using ores::marketdata::messaging::get_fixing_request;
using ores::marketdata::messaging::get_fixing_response;

const std::string fixing_subject("ores.test.fixing_read.get");
const std::string fixing_read_permission("marketdata::market_fixings:read");

struct fixing_handler_fixture {
    ores::testing::scoped_database_helper db;
    ores::nats::service::client nats;
    std::optional<ores::security::jwt::jwt_authenticator> verifier;
    std::optional<ores::nats::service::subscription> sub;
    boost::uuids::uuid party;
    std::string item;

    fixing_handler_fixture()
        : nats(ores::testing::make_nats_options())
        , party(ores::marketdata::tests::system_party_of_the_test_tenant(db))
        , item("oresmd://fx/EUR?type=fixing&index=fx&source=NATS" +
               boost::uuids::to_string(boost::uuids::random_generator{}()).substr(0, 8) +
               "&ccy=USD") {
        auto generation = ores::testing::make_generation_context(db);
        auto s = ores::marketdata::generators::generate_synthetic_market_series(generation);
        s.party_id = party;
        s.oresmd_uri = item;
        ores::marketdata::repository::market_series_repository{}.write(db.context(), s);
        auto f = ores::marketdata::generators::generate_synthetic_market_fixing(generation);
        f.party_id = party;
        f.series_id = s.id;
        f.fixing_date = year_month_day{year{2026}, October, day{23}};
        f.value = "1.1500";
        ores::marketdata::repository::market_fixing_repository{}.insert(db.context(), f);

        nats.connect();
        REQUIRE(nats.is_connected());
        verifier = ores::security::jwt::jwt_authenticator::create_hs256(
            ores::marketdata::tests::test_secret,
            ores::marketdata::tests::test_issuer,
            ores::marketdata::tests::test_audience);
        sub = nats.queue_subscribe(
            fixing_subject, "ores.marketdata.service", [this](ores::nats::message msg) {
                ores::marketdata::messaging::fixing_read_handler h(nats, db.context(), verifier);
                h.get_fixing(std::move(msg));
            });
    }

    ores::nats::message ask(const std::vector<std::string>& roles) {
        get_fixing_request req;
        req.item = item;
        req.on_or_before = year_month_day{year{2026}, October, day{23}};
        const auto token = ores::marketdata::tests::mint_token(
            db.tenant_id().to_string(), party, {boost::uuids::to_string(party)}, roles);
        std::unordered_map<std::string, std::string> headers;
        headers[std::string(ores::nats::headers::authorization)] =
            std::string(ores::nats::headers::bearer_prefix) + token;
        return nats.request_sync(
            fixing_subject, ores::nats::default_wire_codec().encode(req), headers);
    }
};

}

using fixing_read_handler_test::fixing_handler_fixture;
using fixing_read_handler_test::fixing_read_permission;

TEST_CASE("a_token_with_the_fixing_read_permission_gets_the_fixing_over_nats",
          "[fixing_read_handler][integration]") {
    fixing_handler_fixture f;

    const auto reply = f.ask({fixing_read_permission});

    REQUIRE_FALSE(reply.headers.contains(std::string(ores::nats::headers::x_error)));
    const auto decoded =
        ores::nats::default_wire_codec().decode<fixing_read_handler_test::get_fixing_response>(
            reply.data);
    REQUIRE(decoded);
    REQUIRE(decoded->fixing_date.has_value());
    CHECK(decoded->value == "1.1500");
}

TEST_CASE("a_token_without_the_fixing_read_permission_is_refused_forbidden",
          "[fixing_read_handler][integration]") {
    fixing_handler_fixture f;

    const auto reply = f.ask({"marketdata::market_observations:read"});

    REQUIRE(reply.headers.contains(std::string(ores::nats::headers::x_error)));
    CHECK(reply.headers.at(std::string(ores::nats::headers::x_error)) == "forbidden");
}
