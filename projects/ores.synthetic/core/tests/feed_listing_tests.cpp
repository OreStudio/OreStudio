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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.logging/logger.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.synthetic.api/feeds/feed_filter.hpp"
#include "ores.synthetic.api/generators/fx_vol_surface_feed_generator.hpp"
#include "ores.synthetic.api/generators/market_data_generation_config_generator.hpp"
#include "ores.synthetic.core/repository/fx_vol_surface_feed_repository.hpp"
#include "ores.synthetic.core/repository/market_data_generation_config_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "test_suite.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

ores::database::context party_scope(ores::testing::scoped_database_helper& h,
                                    ores::utility::generation::generation_context& ctx) {
    ores::refdata::repository::party_repository parties;
    auto party = ores::refdata::generators::generate_synthetic_party(ctx);
    party.change_reason_code = "system.test";
    for (const auto& e : parties.read_latest(h.context()))
        if (e.tenant_id == party.tenant_id) {
            party.parent_party_id = e.id;
            break;
        }
    parties.write(h.context(), party);
    return h.context().with_party(h.tenant_id(), party.id, {party.id}, h.db_user());
}

}

TEST_CASE("the feed list holds a vol surface feed beside the spot and curve feeds",
          "[feed_listing][integration]") {
    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = party_scope(h, gen);

    auto collection =
        ores::synthetic::generators::generate_synthetic_market_data_generation_config(gen);
    ores::synthetic::repository::market_data_generation_config_repository().write(ctx, collection);

    auto feed = ores::synthetic::generators::generate_synthetic_fx_vol_surface_feed(gen);
    feed.change_reason_code = "system.test";
    feed.party_id = *ctx.party_id();
    feed.config_id = collection.id;
    ores::synthetic::repository::fx_vol_surface_feed_repository().write(ctx, feed);

    const auto rows = ores::database::query_builder::execute_raw_multi_column_query(
        ctx,
        "select kind, label, underlying, engine, provider_code, source_name from (" +
            ores::synthetic::feed::feeds_query() + ") f where id = '" +
            boost::uuids::to_string(feed.id) + "'",
        lg,
        "Reading the vol surface feed from the feed list");

    REQUIRE(rows.size() == 1);
    const auto pair = feed.base_currency_code + "/" + feed.quote_currency_code;
    CHECK(rows[0][0] == std::optional<std::string>("fx_vol_surface"));
    CHECK(rows[0][1] == std::optional(pair));
    CHECK(rows[0][2] == std::optional(pair));
    CHECK(rows[0][3] == std::optional<std::string>("ssvi"));
    CHECK(rows[0][4] == std::optional(collection.provider_code));
    CHECK(rows[0][5] == std::optional(feed.source_name));
}
