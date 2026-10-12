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
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.core/repository/market_series_identity_projector.hpp"
#include "ores.marketdata.core/repository/market_series_identity_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.testing/database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

using namespace ores::logging;

namespace {

const std::string_view test_suite("ores.marketdata.core.tests");
const std::string tags("[repository][series_identity][backfill]");

using ores::marketdata::repository::market_series_identity_projector;
using ores::marketdata::repository::market_series_identity_repository;
using ores::marketdata::repository::market_series_repository;
using ores::testing::database_helper;

/// A curve id no earlier run used, so the series is this test's alone.
std::string fresh_curve_id() {
    auto tag = boost::uuids::to_string(boost::uuids::random_generator{}());
    tag.erase(std::remove(tag.begin(), tag.end(), '-'), tag.end());
    return tag;
}

std::string zero_uri(const std::string& curve_id) {
    return "oresmd://ir/EUR?type=series&instrument=zero&quote=rate&curve_id=" + curve_id +
           "&day_counter=A365";
}

/// Inserts a series the way the data quality publish does: in SQL, which cannot run the
/// codec, so the series has no identity row. Returns its id.
std::string insert_without_identity(database_helper& h,
                                    const boost::uuids::uuid& party,
                                    const std::string& uri) {
    const auto id = boost::uuids::to_string(boost::uuids::random_generator{}());
    auto lg(make_logger(test_suite));
    ores::database::repository::execute_parameterized_multi_column_query(
        h.context(),
        "insert into ores_marketdata_market_series_tbl ("
        "    tenant_id, id, version, party_id, oresmd_uri, series_subclass,"
        "    producer_kind, derivation_kind, derivation_config_id, derivation_config_version,"
        "    modified_by, performed_by, change_reason_code, change_commentary)"
        " values ($1::uuid, $2::uuid, 0, $3::uuid, $4, 'yield',"
        "    'VENDOR', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,"
        "    current_user, current_user, 'system.external_data_import',"
        "    'inserted in SQL by a backfill test')"
        " returning id::text",
        {h.tenant_id().to_string(), id, boost::uuids::to_string(party), uri},
        lg,
        "Inserting a series without its identity row");
    return id;
}

std::size_t identity_rows(database_helper& h, const std::string& series_id) {
    return market_series_identity_repository{}
        .read_latest(h.context(), std::vector<std::string>{series_id})
        .size();
}

}

TEST_CASE("a_series_inserted_in_sql_has_no_identity_until_it_is_projected", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto party = boost::uuids::random_generator{}();
    const auto bare = insert_without_identity(h, party, zero_uri(fresh_curve_id()));
    REQUIRE(identity_rows(h, bare) == 0);

    const auto done = market_series_identity_projector::project_missing(h.context());

    CHECK(done.projected >= 1);
    CHECK(identity_rows(h, bare) == 1);
}

TEST_CASE("projecting_missing_identities_leaves_a_projected_series_alone", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto party = boost::uuids::random_generator{}();
    // A series written through the repository is projected as it is written.
    ores::marketdata::domain::market_series written;
    written.id = boost::uuids::random_generator{}();
    written.tenant_id = h.tenant_id();
    written.party_id = party;
    written.oresmd_uri = zero_uri(fresh_curve_id());
    written.series_subclass = "yield";
    written.derivation_kind = "OBSERVED";
    written.derivation_config_id = boost::uuids::nil_uuid();
    written.derivation_config_version = 0;
    written.modified_by = h.db_user();
    written.performed_by = h.db_user();
    written.change_reason_code = "system.test";
    written.change_commentary = "series identity backfill test";
    market_series_repository{}.write(h.context(), written);
    const auto written_id = boost::uuids::to_string(written.id);
    REQUIRE(identity_rows(h, written_id) == 1);

    market_series_identity_projector::project_missing(h.context());

    CHECK(identity_rows(h, written_id) == 1);
}

TEST_CASE("a_second_backfill_finds_nothing_to_project", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto party = boost::uuids::random_generator{}();
    insert_without_identity(h, party, zero_uri(fresh_curve_id()));

    market_series_identity_projector::project_missing(h.context());
    const auto again = market_series_identity_projector::project_missing(h.context());

    CHECK(again.projected == 0);
    CHECK(again.already_projected >= 1);
}

TEST_CASE("a_backfill_for_one_party_leaves_the_other_partys_series_unprojected", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto first = boost::uuids::random_generator{}();
    const auto second = boost::uuids::random_generator{}();
    const auto curve = fresh_curve_id();
    const auto first_id = insert_without_identity(h, first, zero_uri(curve));
    const auto second_id = insert_without_identity(h, second, zero_uri(curve));

    market_series_identity_projector::project_missing(h.context(), first);

    CHECK(identity_rows(h, first_id) == 1);
    CHECK(identity_rows(h, second_id) == 0);

    market_series_identity_projector::project_missing(h.context());
    CHECK(identity_rows(h, second_id) == 1);
}
