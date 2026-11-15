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
#include "ores.database/query_builder/execute.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.core/repository/market_series_identity_projector.hpp"
#include "ores.marketdata.core/repository/market_series_identity_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.testing/database_helper.hpp"
#include "test_helpers.hpp"
#include "test_suite.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <string>
#include <vector>

using namespace ores::logging;

namespace {

using ores::marketdata::repository::market_series_identity_projector;
using ores::marketdata::repository::market_series_identity_repository;
using ores::marketdata::repository::market_series_repository;
using ores::testing::database_helper;

/// Inserts a series the way the data quality publish does: in SQL, which cannot run the
/// codec, so the series has no identity row. Returns its id.
std::string insert_without_identity(const ores::database::context& ctx,
                                    const boost::uuids::uuid& party,
                                    const std::string& uri) {
    const auto id = boost::uuids::to_string(boost::uuids::random_generator{}());
    auto lg(make_logger(test_suite));
    ores::database::query_builder::execute_parameterized_multi_column_query(
        ctx,
        "insert into ores_marketdata_market_series_tbl ("
        "    tenant_id, id, version, party_id, oresmd_uri, series_subclass,"
        "    provider_code, derivation_kind, derivation_config_id, derivation_config_version,"
        "    modified_by, performed_by, change_reason_code, change_commentary)"
        " values ($1::uuid, $2::uuid, 0, $3::uuid, $4, 'yield',"
        "    'ore', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,"
        "    current_user, current_user, 'system.external_data_import',"
        "    'inserted in SQL by a backfill test')"
        " returning id::text",
        {ctx.tenant_id().to_string(), id, boost::uuids::to_string(party), uri},
        lg,
        "Inserting a series without its identity row");
    return id;
}

/// States one field of one family's shape, as the catalogue does.
void state_shape(const ores::database::context& ctx,
                 const std::string& subclass,
                 const std::string& index_family,
                 const std::string& field,
                 const std::string& requirement) {
    auto lg(make_logger(test_suite));
    ores::database::query_builder::execute_parameterized_multi_column_query(
        ctx,
        "insert into public.ores_marketdata_market_series_identity_shapes_tbl ("
        "    tenant_id, asset_class, series_subclass, index_family, field, requirement,"
        "    description, version, modified_by, performed_by,"
        "    change_reason_code, change_commentary)"
        " values ($1::uuid, 'ir', $2, $3, $4, $5, 'a test states the shape it asserts',"
        "    0, current_user, current_user, 'system.test', 'stated by a test')",
        {ctx.tenant_id().to_string(), subclass, index_family, field, requirement},
        lg,
        "Stating a family's identity shape");
}

/// Inserts a series of the given subclass and URI, as the ingest does.
std::string insert_series(const ores::database::context& ctx,
                          const boost::uuids::uuid& party,
                          const std::string& uri,
                          const std::string& subclass) {
    const auto id = boost::uuids::to_string(boost::uuids::random_generator{}());
    auto lg(make_logger(test_suite));
    ores::database::query_builder::execute_parameterized_multi_column_query(
        ctx,
        "insert into ores_marketdata_market_series_tbl ("
        "    tenant_id, id, version, party_id, oresmd_uri, series_subclass,"
        "    provider_code, derivation_kind, derivation_config_id, derivation_config_version,"
        "    modified_by, performed_by, change_reason_code, change_commentary)"
        " values ($1::uuid, $2::uuid, 0, $3::uuid, $4, $5,"
        "    'ore', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,"
        "    current_user, current_user, 'system.external_data_import',"
        "    'inserted in SQL by a shape test')"
        " returning id::text",
        {ctx.tenant_id().to_string(), id, boost::uuids::to_string(party), uri, subclass},
        lg,
        "Inserting a series with its identity shape under test");
    return id;
}

std::size_t identity_rows(database_helper& h, const std::string& series_id) {
    return market_series_identity_repository{}
        .read_latest(h.context(), std::vector<std::string>{series_id})
        .size();
}

}

TEST_CASE("a_series_inserted_in_sql_has_no_identity_until_it_is_projected",
          "[repository][series_identity][backfill]") {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto party = boost::uuids::random_generator{}();
    const auto bare = insert_without_identity(h.context(), party, zero_uri(fresh_curve_id()));
    REQUIRE(identity_rows(h, bare) == 0);

    const auto done = market_series_identity_projector::project_missing(h.context());

    CHECK(done.projected >= 1);
    CHECK(identity_rows(h, bare) == 1);
}

TEST_CASE("projecting_missing_identities_leaves_a_projected_series_alone",
          "[repository][series_identity][backfill]") {
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
    written.provider_code = "ore";
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

TEST_CASE("a_second_backfill_finds_nothing_to_project", "[repository][series_identity][backfill]") {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto party = boost::uuids::random_generator{}();
    insert_without_identity(h.context(), party, zero_uri(fresh_curve_id()));

    market_series_identity_projector::project_missing(h.context());
    const auto again = market_series_identity_projector::project_missing(h.context());

    CHECK(again.projected == 0);
    CHECK(again.already_projected >= 1);
}

TEST_CASE("a_series_with_an_unreadable_uri_is_projected_once_and_not_retried",
          "[repository][series_identity][backfill]") {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto party = boost::uuids::random_generator{}();
    // No codec reads this URI. It still gets a row, with its kind and no field value.
    const auto odd = insert_without_identity(
        h.context(),
        party,
        "oresmd://ir/EUR?type=series&instrument=no_such_instrument&quote=rate&x=" +
            fresh_curve_id());

    const auto first = market_series_identity_projector::project_missing(h.context());
    CHECK(first.unreadable >= 1);
    CHECK(identity_rows(h, odd) == 1);

    // Having a row, it is not missing, so the next call neither retries nor counts it.
    const auto again = market_series_identity_projector::project_missing(h.context());
    CHECK(again.projected == 0);
    CHECK(again.unreadable == 0);
    CHECK(identity_rows(h, odd) == 1);
}

TEST_CASE("a_backfill_for_one_party_leaves_the_other_partys_series_unprojected",
          "[repository][series_identity][backfill]") {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto first = boost::uuids::random_generator{}();
    const auto second = boost::uuids::random_generator{}();
    const auto curve = fresh_curve_id();
    const auto first_id = insert_without_identity(h.context(), first, zero_uri(curve));
    const auto second_id = insert_without_identity(h.context(), second, zero_uri(curve));

    market_series_identity_projector::project_missing(h.context(), first);

    CHECK(identity_rows(h, first_id) == 1);
    CHECK(identity_rows(h, second_id) == 0);

    market_series_identity_projector::project_missing(h.context());
    CHECK(identity_rows(h, second_id) == 1);
}

TEST_CASE("a_backfill_leaves_the_system_tenants_series_to_the_system",
          "[repository][series_identity][backfill]") {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto system = h.context().with_tenant(ores::utility::uuid::tenant_id::system(), "test");
    const auto party = boost::uuids::random_generator{}();
    const auto shared = insert_without_identity(system, party, zero_uri(fresh_curve_id()));

    market_series_identity_projector::project_missing(system);
    REQUIRE(market_series_identity_repository{}
                .read_latest(system, std::vector<std::string>{shared})
                .size() == 1);

    // The tenant reads the shared series but not its identity row. A backfill
    // must leave it to the system: projecting it would collide with the row the
    // tenant cannot see.
    CHECK_NOTHROW(market_series_identity_projector::project_missing(h.context()));
    CHECK(identity_rows(h, shared) == 0);
}

TEST_CASE("an_ibor_fixing_states_its_currency_its_index_and_its_name",
          "[repository][series_identity][shape]") {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto ctx = h.context();
    const auto series_id =
        insert_series(ctx,
                      boost::uuids::random_generator{}(),
                      "oresmd://ir/USD?type=fixing&index=ibor&name=SOFR",
                      "index_fixing");
    // The party gets the family's shape from its tenant's catalogue, so the write
    // path can refuse a row that is thin.
    for (const auto& field : {"ccy", "index", "index_name"})
        state_shape(ctx, "index_fixing", "ibor", field, "required");
    state_shape(ctx, "index_fixing", "ibor", "unit_ccy", "forbidden");

    const auto rows = market_series_identity_repository{}
                          .read_latest(ctx, std::vector<std::string>{series_id});
    REQUIRE(rows.size() == 1);
    CHECK(rows.front().asset_class == "ir");
    CHECK(rows.front().index == "ibor");
    CHECK(rows.front().index_name == "SOFR");
    CHECK(rows.front().ccy == "USD");
    CHECK(rows.front().unit_ccy.empty());
}

TEST_CASE("an_ibor_fixing_that_misses_a_required_field_is_refused_by_name",
          "[repository][series_identity][shape]") {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto ctx = h.context();
    const auto series_id =
        insert_series(ctx, boost::uuids::random_generator{}(), "oresmd://ir/USD?type=fixing&index=ibor", "index_fixing");
    state_shape(ctx, "index_fixing", "ibor", "index_name", "required");

    const auto series = market_series_repository{}.read_latest(ctx);
    std::vector<ores::marketdata::domain::market_series> one;
    for (const auto& s : series) {
        if (boost::uuids::to_string(s.id) == series_id)
            one.push_back(s);
    }
    REQUIRE(one.size() == 1);

    CHECK_THROWS_WITH(market_series_identity_projector::project(ctx, one),
                      Catch::Matchers::ContainsSubstring("field 'index_name' is required"));
}
