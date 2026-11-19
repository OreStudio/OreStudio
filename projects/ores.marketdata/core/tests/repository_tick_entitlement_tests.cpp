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
#include "ores.database/service/tenant_context.hpp"
#include "ores.marketdata.api/domain/catalogue_ids.hpp"
#include "ores.marketdata.api/generators/route_generator.hpp"
#include "ores.marketdata.core/repository/feed_repository.hpp"
#include "ores.marketdata.core/repository/route_repository.hpp"
#include "ores.marketdata.core/repository/tick_entitlement.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "tick_test_support.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

namespace {

namespace catalogue = ores::marketdata::domain::catalogue_ids;
using ores::marketdata::messaging::watchable_feed;
using ores::marketdata::repository::watchable_feeds;
using ores::marketdata::tests::party_context;
using ores::marketdata::tests::subscribe;
using ores::testing::database_helper;

const std::string tick_entitlement_tags("[repository][tick_entitlement]");

/// A new party of @p h, subscribed to the provider ore with the rights given and routed to
/// @p feed.
boost::uuids::uuid routed_party(database_helper& h,
                                const boost::uuids::uuid& feed,
                                bool display,
                                bool non_display,
                                bool derived) {
    auto generation = ores::testing::make_generation_context(h);
    const auto party = generation.generate_uuid();
    subscribe(h, party, "ore", display, non_display, derived);
    auto r = ores::marketdata::generators::generate_synthetic_route(generation);
    r.party_id = party;
    r.publisher_code = "ore";
    r.asset_class = "fx";
    r.feed_id = feed;
    r.modified_by = h.db_user();
    r.performed_by = h.db_user();
    r.change_reason_code = "system.test";
    r.change_commentary = "Test route";
    ores::marketdata::repository::route_repository{}.write(h.context(), r);
    return party;
}

/// The ore feed as a watcher sees it, with the rank the providers table gives ore.
watchable_feed ore_feed(const boost::uuids::uuid& feed) {
    return {"ore", boost::uuids::to_string(feed), 5};
}

}

TEST_CASE("a_party_with_the_display_right_may_watch_its_routed_feed_on_a_screen",
          tick_entitlement_tags) {
    database_helper h;
    const auto party = routed_party(h, catalogue::ore_import_feed_id(), true, false, false);

    const auto feeds = watchable_feeds(party_context(h, party), true);

    CHECK(feeds == std::vector{ore_feed(catalogue::ore_import_feed_id())});
}

TEST_CASE("a_party_with_only_the_non_display_right_may_watch_for_a_service_and_not_a_screen",
          tick_entitlement_tags) {
    database_helper h;
    const auto party = routed_party(h, catalogue::ore_import_feed_id(), false, true, false);
    const auto ctx = party_context(h, party);

    CHECK(watchable_feeds(ctx, true).empty());
    CHECK(watchable_feeds(ctx, false) == std::vector{ore_feed(catalogue::ore_import_feed_id())});
}

TEST_CASE("a_party_with_only_the_derived_right_may_watch_nothing", tick_entitlement_tags) {
    database_helper h;
    const auto party = routed_party(h, catalogue::ore_import_feed_id(), false, false, true);
    const auto ctx = party_context(h, party);

    CHECK(watchable_feeds(ctx, true).empty());
    CHECK(watchable_feeds(ctx, false).empty());
}

TEST_CASE("a_party_watches_the_feed_its_own_route_picks_and_not_another_partys",
          tick_entitlement_tags) {
    database_helper h;
    const auto first = routed_party(h, catalogue::ore_import_feed_id(), true, false, false);
    const auto second = routed_party(h, catalogue::ore_derived_feed_id(), true, false, false);

    CHECK(watchable_feeds(party_context(h, first), true) ==
          std::vector{ore_feed(catalogue::ore_import_feed_id())});
    CHECK(watchable_feeds(party_context(h, second), true) ==
          std::vector{ore_feed(catalogue::ore_derived_feed_id())});
}

TEST_CASE("the_system_tenant_may_watch_every_feed_of_the_catalogue", tick_entitlement_tags) {
    database_helper h;
    const auto system_ctx =
        ores::database::service::tenant_context::with_system_tenant(h.context());

    const auto feeds = watchable_feeds(system_ctx, true);

    const auto catalogue_feeds =
        ores::marketdata::repository::feed_repository{}.read_latest(system_ctx);
    CHECK(feeds.size() == catalogue_feeds.size());
    CHECK(std::ranges::find(feeds, ore_feed(catalogue::ore_import_feed_id())) != feeds.end());
    CHECK(std::ranges::find(feeds, ore_feed(catalogue::ore_derived_feed_id())) != feeds.end());
}
