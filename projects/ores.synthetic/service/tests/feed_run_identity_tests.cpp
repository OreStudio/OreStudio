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
#include "../src/feed_run_identity.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.testing/test_database_manager.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <vector>

namespace {

const std::string tags("[feed_run_identity]");

using ores::service::service::cache::run_token;
using ores::service::service::cache::run_token_key;
using ores::synthetic::service::feed_kind_candidate;
using ores::synthetic::service::feed_run_identity;
using ores::synthetic::service::feed_run_identity_ports;
using ores::synthetic::service::feed_start_target;

// A real context over the test database. The identity only hands it to the
// record port, which the fake never reads.
ores::database::context make_test_context() {
    const auto opts = ores::testing::test_database_manager::make_database_options();
    return ores::database::context_factory::make_context(
        ores::database::context_factory::configuration{.database_options = opts,
                                                       .pool_size = 1,
                                                       .num_attempts = 1,
                                                       .wait_time_in_seconds = 1,
                                                       .service_account = opts.user});
}

// The three ports as a test sees them: what each was asked, and what each says
// next.
struct fake_ports {
    std::vector<run_token_key> exchanges;
    std::vector<std::string> grants_created;
    std::vector<std::string> bearers;
    std::vector<boost::uuids::uuid> recorded;
    std::string grant_to_give = "11111111-1111-1111-1111-111111111111";
    std::optional<std::string> grant_error;
    std::string refused_grant;
    int next_token = 0;

    feed_run_identity_ports ports() {
        feed_run_identity_ports p;
        p.mint = [this](const run_token_key& key, std::string_view) -> std::optional<run_token> {
            exchanges.push_back(key);
            if (key.grant_id == refused_grant)
                return std::nullopt;
            return run_token{.token = "run-token-" + std::to_string(++next_token),
                             .expires_at =
                                 std::chrono::system_clock::now() + std::chrono::minutes(5)};
        };
        p.create_grant =
            [this](const std::string& resource,
                   const std::string& bearer) -> std::expected<std::string, std::string> {
            grants_created.push_back(resource);
            bearers.push_back(bearer);
            if (grant_error)
                return std::unexpected(*grant_error);
            return grant_to_give;
        };
        p.record_grant =
            [this](const ores::database::context&,
                   const boost::uuids::uuid&,
                   const boost::uuids::uuid& grant) -> std::expected<void, std::string> {
            recorded.push_back(grant);
            return {};
        };
        return p;
    }
};

feed_start_target target(bool vintage,
                         std::optional<boost::uuids::uuid> grant = std::nullopt,
                         const std::string& config_id = "feed-1") {
    feed_start_target t;
    t.row.candidate.feed_config_id = config_id;
    t.row.candidate.display_name = "oresmd://fx/USD";
    t.row.candidate.container_id = boost::uuids::random_generator()();
    t.row.candidate.vintage_priced = vintage;
    t.run_grant_id = grant;
    return t;
}

const boost::uuids::uuid held_grant =
    boost::uuids::string_generator()("22222222-2222-2222-2222-222222222222");

}

TEST_CASE("a_feed_with_a_fixed_price_needs_no_token_and_asks_nobody", tags) {
    fake_ports fake;
    feed_run_identity identity(fake.ports());

    const auto token = identity.run_token_for(target(false), make_test_context(), "person");

    REQUIRE(token);
    CHECK(token->empty());
    CHECK(fake.exchanges.empty());
    CHECK(fake.grants_created.empty());
}

TEST_CASE("a_vintage_feed_exchanges_the_grant_its_collection_holds", tags) {
    fake_ports fake;
    feed_run_identity identity(fake.ports());

    const auto token =
        identity.run_token_for(target(true, held_grant, "feed-7"), make_test_context(), {});

    REQUIRE(token);
    CHECK(*token == "run-token-1");
    REQUIRE(fake.exchanges.size() == 1);
    CHECK(fake.exchanges[0].grant_id == boost::uuids::to_string(held_grant));
    CHECK(fake.exchanges[0].run_id == "feed-7");
    CHECK(fake.grants_created.empty());
}

TEST_CASE("a_restore_with_no_grant_and_no_person_refuses_and_says_why", tags) {
    fake_ports fake;
    feed_run_identity identity(fake.ports());

    const auto token = identity.run_token_for(target(true), make_test_context(), {});

    REQUIRE(!token);
    CHECK(token.error().find("no run grant") != std::string::npos);
    CHECK(fake.grants_created.empty());
    CHECK(fake.exchanges.empty());
}

TEST_CASE("a_start_by_a_person_gives_the_consent_and_records_it", tags) {
    fake_ports fake;
    feed_run_identity identity(fake.ports());
    const auto start = target(true);

    const auto token = identity.run_token_for(start, make_test_context(), "person-token");

    REQUIRE(token);
    REQUIRE(fake.grants_created.size() == 1);
    CHECK(fake.grants_created[0] == "synthetic.market_data_generation_config/" +
                                        boost::uuids::to_string(start.row.candidate.container_id));
    CHECK(fake.bearers[0] == "person-token");
    CHECK(fake.recorded.size() == 1);
    REQUIRE(fake.exchanges.size() == 1);
    CHECK(fake.exchanges[0].grant_id == fake.grant_to_give);
}

TEST_CASE("the_feeds_after_the_first_of_a_collection_reuse_the_grant_just_given", tags) {
    fake_ports fake;
    feed_run_identity identity(fake.ports());
    auto first = target(true, std::nullopt, "feed-1");
    auto second = target(true, std::nullopt, "feed-2");
    second.row.candidate.container_id = first.row.candidate.container_id;
    const auto ctx = make_test_context();

    REQUIRE(identity.run_token_for(first, ctx, "person-token"));
    REQUIRE(identity.run_token_for(second, ctx, "person-token"));

    CHECK(fake.grants_created.size() == 1);
    CHECK(fake.recorded.size() == 1);
    CHECK(fake.exchanges.size() == 2);
}

TEST_CASE("a_lapsed_grant_stops_a_restore_and_a_person_replaces_it", tags) {
    fake_ports fake;
    fake.refused_grant = boost::uuids::to_string(held_grant);
    feed_run_identity identity(fake.ports());
    const auto ctx = make_test_context();

    const auto restore = identity.run_token_for(target(true, held_grant), ctx, {});
    REQUIRE(!restore);
    CHECK(restore.error().find("lapsed or revoked") != std::string::npos);
    CHECK(fake.grants_created.empty());

    const auto start = identity.run_token_for(target(true, held_grant), ctx, "person-token");
    REQUIRE(start);
    CHECK(fake.grants_created.size() == 1);
    CHECK(fake.exchanges.back().grant_id == fake.grant_to_give);
}

TEST_CASE("a_refusal_by_iam_to_give_the_consent_reaches_the_caller", tags) {
    fake_ports fake;
    fake.grant_error = "The person lacks marketdata::market_series:read.";
    feed_run_identity identity(fake.ports());

    const auto token = identity.run_token_for(target(true), make_test_context(), "person-token");

    REQUIRE(!token);
    CHECK(token.error() == "The person lacks marketdata::market_series:read.");
    CHECK(fake.recorded.empty());
    CHECK(fake.exchanges.empty());
}
