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
#include "ores.marketdata.api/domain/market_series_asset_class.hpp"
#include "ores.marketdata.api/generators/feed_binding_generator.hpp"
#include "ores.marketdata.api/generators/market_series_generator.hpp"
#include "ores.marketdata.api/generators/observation_lineage_generator.hpp"
#include "ores.marketdata.api/generators/series_classification_rule_generator.hpp"
#include "ores.marketdata.core/repository/feed_binding_repository.hpp"
#include "ores.marketdata.core/repository/market_series_asset_class_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/repository/observation_lineage_repository.hpp"
#include "ores.marketdata.core/repository/series_classification_rule_repository.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

namespace {

const std::string tags("[repository]");

}

using namespace ores::marketdata::generators;
using namespace ores::marketdata::repository;
using ores::testing::database_helper;

TEST_CASE("feed_binding_reads_back_by_id_and_source_until_removed", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    feed_binding_repository repo;

    const auto b = generate_synthetic_feed_binding(ctx);
    repo.write(h.context(), b);
    const auto id = boost::uuids::to_string(b.id);

    const auto by_id = repo.read_latest(h.context(), id);
    REQUIRE(by_id.size() == 1);
    CHECK(by_id.front().source_name == b.source_name);
    CHECK(by_id.front().party_id == b.party_id);

    const auto by_source = repo.read_latest_by_source_name(h.context(), b.source_name);
    REQUIRE(by_source.size() == 1);
    CHECK(by_source.front().id == b.id);

    repo.remove(h.context(), id);
    CHECK(repo.read_latest(h.context(), id).empty());
    CHECK(repo.read_any_by_source_name(h.context(), b.source_name).size() == 1);
}

TEST_CASE("observation_lineage_reads_back_by_id_until_removed", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    observation_lineage_repository repo;

    const auto l = generate_synthetic_observation_lineage(ctx);
    repo.write(h.context(), l);
    const auto id = boost::uuids::to_string(l.id);

    const auto read = repo.read_latest(h.context(), id);
    REQUIRE(read.size() == 1);
    CHECK(read.front().series_id == l.series_id);
    CHECK(read.front().derivation_config_id == l.derivation_config_id);
    CHECK(read.front().source_series_ids == l.source_series_ids);

    repo.remove(h.context(), id);
    CHECK(repo.read_latest(h.context(), id).empty());
}

TEST_CASE("series_classification_rule_reads_back_by_key_until_removed", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    series_classification_rule_repository repo;

    const auto r = generate_synthetic_series_classification_rule(ctx);
    repo.write(h.context(), r);

    const auto read = repo.read_latest(h.context(), r.series_type, r.metric);
    REQUIRE(read.size() == 1);
    CHECK(read.front().series_subclass_code == r.series_subclass_code);

    repo.remove(h.context(), r.series_type, r.metric);
    CHECK(repo.read_latest(h.context(), r.series_type, r.metric).empty());
}

TEST_CASE("market_series_asset_class_pages_the_classes_of_one_series", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    const auto series = generate_synthetic_market_series(ctx);
    market_series_repository{}.write(h.context(), series);

    market_series_asset_class_repository repo(h.context());
    std::vector<ores::marketdata::domain::market_series_asset_class> rows;
    for (const std::string code : {"fx", "interest_rates", "equity"}) {
        ores::marketdata::domain::market_series_asset_class row;
        row.tenant_id = h.tenant_id().to_string();
        row.market_series_id = series.id;
        row.asset_class_code = code;
        row.modified_by = h.db_user();
        row.performed_by = h.db_user();
        row.change_reason_code = "system.test";
        row.change_commentary = "repository test";
        rows.push_back(row);
    }
    repo.write(rows);

    CHECK(repo.get_total_asset_class_count_by_series(series.id) == 3);

    const auto first = repo.read_latest_by_series(series.id, 0, 2);
    const auto second = repo.read_latest_by_series(series.id, 2, 2);
    REQUIRE(first.size() == 2);
    REQUIRE(second.size() == 1);

    std::vector<std::string> codes;
    for (const auto& r : first)
        codes.push_back(r.asset_class_code);
    codes.push_back(second.front().asset_class_code);
    CHECK(codes == std::vector<std::string>{"equity", "fx", "interest_rates"});

    repo.remove_by_series(series.id);
    CHECK(repo.get_total_asset_class_count_by_series(series.id) == 0);
    CHECK(repo.read_latest_by_series(series.id).empty());
}
