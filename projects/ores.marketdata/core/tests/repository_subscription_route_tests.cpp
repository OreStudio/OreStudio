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
#include "ores.marketdata.api/domain/catalogue_ids.hpp"
#include "ores.marketdata.api/domain/route.hpp"
#include "ores.marketdata.api/domain/subscription.hpp"
#include "ores.marketdata.api/generators/route_generator.hpp"
#include "ores.marketdata.api/generators/subscription_generator.hpp"
#include "ores.marketdata.core/repository/route_repository.hpp"
#include "ores.marketdata.core/repository/subscription_repository.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>

namespace {

const std::string tags("[repository][subscription]");
const std::string route_tags("[repository][route]");

constexpr auto unique_violation = "duplicate key value violates unique constraint";
constexpr auto check_violation = "violates check constraint";

}

using namespace ores::marketdata::generators;

namespace catalogue = ores::marketdata::domain::catalogue_ids;
using ores::marketdata::repository::route_repository;
using ores::marketdata::repository::subscription_repository;
using ores::testing::database_helper;

namespace {

ores::marketdata::domain::subscription
subscribed(database_helper& h,
           ores::utility::generation::generation_context& ctx,
           subscription_repository& repo,
           const std::string& provider) {
    auto s = generate_synthetic_subscription(ctx);
    s.provider_code = provider;
    s.display = true;
    repo.write(h.context(), s);
    return s;
}

}

TEST_CASE("write_and_read_a_subscription", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    subscription_repository repo;
    auto s = subscribed(h, ctx, repo, "bloomberg");

    const auto read = repo.read_latest(h.context(), boost::uuids::to_string(s.id));
    REQUIRE(read.size() == 1);
    CHECK(read.front().party_id == s.party_id);
    CHECK(read.front().provider_code == "bloomberg");
    CHECK(read.front().display);
}

TEST_CASE("a_subscription_holds_at_least_one_right", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    subscription_repository repo;
    auto s = generate_synthetic_subscription(ctx);
    s.display = false;
    s.non_display = false;
    s.derived = false;
    CHECK_THROWS_WITH(repo.write(h.context(), s),
                      Catch::Matchers::ContainsSubstring(check_violation));
}

TEST_CASE("a_party_holds_one_subscription_to_a_provider", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    subscription_repository repo;
    auto first = subscribed(h, ctx, repo, "bloomberg");

    auto other_provider = first;
    other_provider.id = ctx.generate_uuid();
    other_provider.provider_code = "lseg";
    CHECK_NOTHROW(repo.write(h.context(), other_provider));

    auto duplicate = first;
    duplicate.id = ctx.generate_uuid();
    CHECK_THROWS_WITH(repo.write(h.context(), duplicate),
                      Catch::Matchers::ContainsSubstring(unique_violation));
}

TEST_CASE("a_subscription_names_a_provider_in_the_catalogue", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    subscription_repository repo;
    auto s = generate_synthetic_subscription(ctx);
    s.provider_code = "not_a_provider";
    CHECK_THROWS_WITH(repo.write(h.context(), s),
                      Catch::Matchers::ContainsSubstring("Invalid provider"));
}

TEST_CASE("a_route_needs_a_subscription", route_tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    route_repository routes;
    auto r = generate_synthetic_route(ctx);
    r.provider_code = "ore";
    r.source_id = catalogue::ore_import_source_id();
    CHECK_THROWS_WITH(routes.write(h.context(), r),
                      Catch::Matchers::ContainsSubstring("holds no subscription"));
}

TEST_CASE("a_route_names_a_source_of_its_provider", route_tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    subscription_repository subscriptions;
    route_repository routes;
    auto s = subscribed(h, ctx, subscriptions, "bloomberg");

    auto r = generate_synthetic_route(ctx);
    r.party_id = s.party_id;
    r.provider_code = "bloomberg";
    r.source_id = catalogue::ore_import_source_id();
    CHECK_THROWS_WITH(routes.write(h.context(), r),
                      Catch::Matchers::ContainsSubstring("is not a source of provider"));
}

TEST_CASE("a_party_has_one_route_for_a_provider", route_tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    subscription_repository subscriptions;
    route_repository routes;
    auto s = subscribed(h, ctx, subscriptions, "ore");

    auto first = generate_synthetic_route(ctx);
    first.party_id = s.party_id;
    first.provider_code = "ore";
    first.source_id = catalogue::ore_import_source_id();
    routes.write(h.context(), first);

    auto second = first;
    second.id = ctx.generate_uuid();
    second.source_id = catalogue::ore_simulated_source_id();
    CHECK_THROWS_WITH(routes.write(h.context(), second),
                      Catch::Matchers::ContainsSubstring(unique_violation));
}

TEST_CASE("a_party_changes_its_route_and_keeps_the_history", route_tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    subscription_repository subscriptions;
    route_repository routes;
    auto s = subscribed(h, ctx, subscriptions, "ore");

    auto r = generate_synthetic_route(ctx);
    r.party_id = s.party_id;
    r.provider_code = "ore";
    r.source_id = catalogue::ore_import_source_id();
    routes.write(h.context(), r);

    const auto id = boost::uuids::to_string(r.id);
    auto current = routes.read_latest(h.context(), id);
    REQUIRE(current.size() == 1);
    current.front().source_id = catalogue::ore_simulated_source_id();
    current.front().change_reason_code = "system.test";
    routes.write(h.context(), current.front());

    const auto latest = routes.read_latest(h.context(), id);
    REQUIRE(latest.size() == 1);
    CHECK(latest.front().source_id == catalogue::ore_simulated_source_id());
    CHECK(routes.read_all(h.context(), id).size() == 2);
}

TEST_CASE("a_route_cannot_move_to_a_provider_without_a_subscription", route_tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    subscription_repository subscriptions;
    route_repository routes;
    auto s = subscribed(h, ctx, subscriptions, "ore");

    auto r = generate_synthetic_route(ctx);
    r.party_id = s.party_id;
    r.provider_code = "ore";
    r.source_id = catalogue::ore_import_source_id();
    routes.write(h.context(), r);

    const auto id = boost::uuids::to_string(r.id);
    auto current = routes.read_latest(h.context(), id);
    REQUIRE(current.size() == 1);
    current.front().provider_code = "lseg";
    current.front().change_reason_code = "system.test";
    CHECK_THROWS_WITH(routes.write(h.context(), current.front()),
                      Catch::Matchers::ContainsSubstring("holds no subscription"));
}
