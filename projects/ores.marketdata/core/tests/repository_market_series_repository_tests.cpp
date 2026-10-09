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
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.api/domain/market_series_identity.hpp"
#include "ores.marketdata.api/domain/market_series_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.api/generators/market_series_generator.hpp"
#include "ores.marketdata.core/repository/market_series_identity_projector.hpp"
#include "ores.marketdata.core/repository/market_series_identity_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string_view test_suite("ores.marketdata.core.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;
using namespace ores::marketdata::generators;

using ores::testing::database_helper;
using ores::marketdata::repository::market_series_repository;

// A series built for the identity cases rather than generated: they need two rows
// that share an identity and nothing else, and a generated identity can collide
// with another case's row.
ores::marketdata::domain::market_series make_identity_test_series(database_helper& h,
                                                                  const boost::uuids::uuid& party) {
    ores::marketdata::domain::market_series s;
    s.id = boost::uuids::random_generator{}();
    s.version = 0;
    s.tenant_id = h.tenant_id();
    s.party_id = party;
    s.oresmd_uri = std::string("oresmd://generic/identity-test-") + boost::uuids::to_string(s.id) +
                   "?type=fixing";
    s.series_subclass = "yield";
    s.derivation_kind = "OBSERVED";
    s.derivation_config_id = boost::uuids::nil_uuid();
    s.derivation_config_version = 0;
    s.modified_by = h.db_user();
    s.performed_by = h.db_user();
    s.change_reason_code = "system.test";
    s.change_commentary = "repository identity test";
    return s;
}


TEST_CASE("write_single_market_series", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository repo;
    auto s = generate_synthetic_market_series(ctx);

    BOOST_LOG_SEV(lg, debug) << "Market series: " << s;
    CHECK_NOTHROW(repo.write(h.context(), s));
}

TEST_CASE("write_multiple_market_series", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository repo;
    auto series = generate_synthetic_market_series(5, ctx);
    BOOST_LOG_SEV(lg, debug) << "Market series: " << series;

    CHECK_NOTHROW(repo.write(h.context(), series));
}

TEST_CASE("read_latest_market_series", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository repo;
    auto written = generate_synthetic_market_series(3, ctx);
    BOOST_LOG_SEV(lg, debug) << "Written market series: " << written;
    repo.write(h.context(), written);

    auto read = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read market series: " << read;

    CHECK(!read.empty());
    CHECK(read.size() >= written.size());
}

TEST_CASE("read_latest_market_series_by_id", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository repo;
    auto series = generate_synthetic_market_series(3, ctx);
    const auto target = series.front();
    repo.write(h.context(), series);

    auto read = repo.read_latest(h.context(), boost::uuids::to_string(target.id));
    BOOST_LOG_SEV(lg, debug) << "Read market series: " << read;

    REQUIRE(read.size() == 1);
    CHECK(read[0].oresmd_uri == target.oresmd_uri);
    CHECK(read[0].series_subclass == target.series_subclass);
}

TEST_CASE("read_latest_market_series_by_identity", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository repo;
    auto s = generate_synthetic_market_series(ctx);
    repo.write(h.context(), s);

    // Scoped to the row's own party: the identity is unique per party, so one
    // identity can name a series in each of several parties.
    auto read =
        repo.read_latest_by_uri(h.context(), s.oresmd_uri, boost::uuids::to_string(s.party_id));
    BOOST_LOG_SEV(lg, debug) << "Read by identity: " << read;

    REQUIRE(read.size() == 1);
    CHECK(read[0].oresmd_uri == s.oresmd_uri);
}

TEST_CASE("read_all_versions_of_market_series", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository repo;
    auto s = generate_synthetic_market_series(ctx);
    repo.write(h.context(), s);

    s.change_commentary = "second version";
    repo.write(h.context(), s);

    auto all = repo.read_all(h.context(), boost::uuids::to_string(s.id));
    BOOST_LOG_SEV(lg, debug) << "All versions: " << all;

    CHECK(all.size() >= 2);
}

TEST_CASE("remove_market_series", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository repo;
    auto s = generate_synthetic_market_series(ctx);
    repo.write(h.context(), s);

    auto before = repo.read_latest(h.context(), boost::uuids::to_string(s.id));
    REQUIRE(!before.empty());

    CHECK_NOTHROW(repo.remove(h.context(), boost::uuids::to_string(s.id)));

    auto after = repo.read_latest(h.context(), boost::uuids::to_string(s.id));
    BOOST_LOG_SEV(lg, debug) << "After remove count: " << after.size();
    CHECK(after.empty());
}

TEST_CASE("two_series_in_one_party_cannot_share_an_identity", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto party = boost::uuids::random_generator{}();

    market_series_repository repo;
    const auto first = make_identity_test_series(h, party);
    repo.write(h.context(), first);

    // The same party and the same identity, with a different id: the identity is
    // what the row is keyed by, so the second write is refused.
    auto second = first;
    second.id = boost::uuids::random_generator{}();

    CHECK_THROWS(repo.write(h.context(), second));
}

TEST_CASE("two_parties_may_each_hold_one_identity", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto party = boost::uuids::random_generator{}();

    market_series_repository repo;
    const auto first = make_identity_test_series(h, party);
    repo.write(h.context(), first);

    // The identity is unique per party, and a market series belongs to one party,
    // so a second party's row of the same identity is a series of its own.
    auto second = first;
    second.id = boost::uuids::random_generator{}();
    second.party_id = boost::uuids::random_generator{}();

    CHECK_NOTHROW(repo.write(h.context(), second));
}

// A series whose URI names a fixing. It carries no instrument type, so the
// projection reads it through the index codec rather than the instrument
// schema, and its identity fields come from the index grammar.
ores::marketdata::domain::market_series make_fixing_test_series(database_helper& h,
                                                                const boost::uuids::uuid& party,
                                                                const std::string& uri) {
    ores::marketdata::domain::market_series s;
    s.id = boost::uuids::random_generator{}();
    s.version = 0;
    s.tenant_id = h.tenant_id();
    s.party_id = party;
    s.oresmd_uri = uri;
    s.series_subclass = "yield";
    s.derivation_kind = "OBSERVED";
    s.derivation_config_id = boost::uuids::nil_uuid();
    s.derivation_config_version = 0;
    s.modified_by = h.db_user();
    s.performed_by = h.db_user();
    s.change_reason_code = "system.test";
    s.change_commentary = "fixing identity projection test";
    return s;
}

TEST_CASE("a_fixing_series_projects_its_identity_fields", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository repo;
    const auto s =
        make_fixing_test_series(h,
                                boost::uuids::random_generator{}(),
                                "oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=6M");
    repo.write(h.context(), s);

    ores::marketdata::repository::market_series_identity_repository identities;
    const auto rows = identities.read_latest(h.context(), boost::uuids::to_string(s.id));
    REQUIRE(rows.size() == 1);

    // The row decomposes the fixing URI onto the same columns a convention's
    // fixing URI decomposes onto: the authority is the asset class, the URI's
    // index key is the family, and name and tenor are the family's fields.
    CHECK(rows[0].identity_kind == "index");
    CHECK(rows[0].asset_class == "ir");
    CHECK(rows[0].ccy == "EUR");
    CHECK(rows[0].index == "ibor");
    CHECK(rows[0].index_name == "EURIBOR");
    CHECK(rows[0].tenor == "6M");
    CHECK(rows[0].instrument_type.empty());
    CHECK(rows[0].quote_type.empty());
}

TEST_CASE("reprojecting_repairs_a_fixing_row_and_changes_nothing_after", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository repo;
    const auto s =
        make_fixing_test_series(h,
                                boost::uuids::random_generator{}(),
                                "oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=6M");
    repo.write(h.context(), s);

    // A row an earlier projector wrote: the kind and the asset class and no
    // field value. Re-projecting has to correct it, because the row the write
    // path leaves is not the only one the table can hold.
    ores::marketdata::domain::market_series_identity stale;
    stale.tenant_id = h.tenant_id();
    stale.series_id = s.id;
    stale.party_id = s.party_id;
    stale.identity_kind = "index";
    stale.asset_class = "ir";
    ores::marketdata::repository::market_series_identity_repository{}.write(h.context(), stale);

    const auto first =
        ores::marketdata::repository::market_series_identity_projector::reproject(h.context(), {s});
    CHECK(first.written == 1);
    CHECK(first.unchanged == 0);
    CHECK(first.unreadable == 0);

    const auto second =
        ores::marketdata::repository::market_series_identity_projector::reproject(h.context(), {s});
    CHECK(second.written == 0);
    CHECK(second.unchanged == 1);

    const auto rows = ores::marketdata::repository::market_series_identity_repository{}.read_latest(
        h.context(), boost::uuids::to_string(s.id));
    REQUIRE(rows.size() == 1);
    CHECK(rows[0].ccy == "EUR");
    CHECK(rows[0].index == "ibor");
    CHECK(rows[0].index_name == "EURIBOR");
    CHECK(rows[0].tenor == "6M");
}

TEST_CASE("an_fx_fixing_projects_its_source", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    const auto s =
        make_fixing_test_series(h,
                                boost::uuids::random_generator{}(),
                                "oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=GBP");
    market_series_repository{}.write(h.context(), s);

    const auto rows = ores::marketdata::repository::market_series_identity_repository{}.read_latest(
        h.context(), boost::uuids::to_string(s.id));
    REQUIRE(rows.size() == 1);
    CHECK(rows[0].identity_kind == "index");
    CHECK(rows[0].asset_class == "fx");
    CHECK(rows[0].index == "fx");
    CHECK(rows[0].unit_ccy == "EUR");
    CHECK(rows[0].ccy == "GBP");
    CHECK(rows[0].source == "ECB");
}

TEST_CASE("two_fx_fixings_that_differ_only_in_source_project_apart", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    const auto party = boost::uuids::random_generator{}();

    market_series_repository repo;
    const auto ecb = make_fixing_test_series(
        h, party, "oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=GBP");
    const auto tr20h = make_fixing_test_series(
        h, party, "oresmd://fx/EUR?type=fixing&index=fx&source=TR20H&ccy=GBP");
    repo.write(h.context(), ecb);
    repo.write(h.context(), tr20h);

    ores::marketdata::repository::market_series_identity_repository identities;
    const auto a = identities.read_latest(h.context(), boost::uuids::to_string(ecb.id));
    const auto b = identities.read_latest(h.context(), boost::uuids::to_string(tr20h.id));
    REQUIRE(a.size() == 1);
    REQUIRE(b.size() == 1);

    // The two identities are equal but for the source, so before the source had
    // a column they projected alike and a join on the shared columns matched
    // both. The source is what tells them apart.
    CHECK(a[0].unit_ccy == b[0].unit_ccy);
    CHECK(a[0].ccy == b[0].ccy);
    CHECK(a[0].index == b[0].index);
    CHECK(a[0].asset_class == b[0].asset_class);
    CHECK(a[0].source == "ECB");
    CHECK(b[0].source == "TR20H");
}

TEST_CASE("a_dated_fixing_projects_its_expiry", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    const auto party = boost::uuids::random_generator{}();

    market_series_repository repo;
    const auto december = make_fixing_test_series(
        h, party, "oresmd://commodity/ICE:B?type=fixing&index=commodity&expiry=2024-12");
    const auto january = make_fixing_test_series(
        h, party, "oresmd://commodity/ICE:B?type=fixing&index=commodity&expiry=2025-01");
    repo.write(h.context(), december);
    repo.write(h.context(), january);

    ores::marketdata::repository::market_series_identity_repository identities;
    const auto a = identities.read_latest(h.context(), boost::uuids::to_string(december.id));
    const auto b = identities.read_latest(h.context(), boost::uuids::to_string(january.id));
    REQUIRE(a.size() == 1);
    REQUIRE(b.size() == 1);

    // ORE reads the date into the index name, so the two contracts are two
    // series. Without the expiry column they projected to one identity.
    CHECK(a[0].index_name == "ICE:B");
    CHECK(b[0].index_name == "ICE:B");
    CHECK(a[0].expiry == "2024-12");
    CHECK(b[0].expiry == "2025-01");
}

TEST_CASE("a_power_fixing_projects_its_delivery_window", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    const auto s = make_fixing_test_series(
        h,
        boost::uuids::random_generator{}(),
        "oresmd://commodity/"
        "ICE:PDQ?type=fixing&index=power&delivery=2021-01-01&start=10800&end=14400");
    market_series_repository{}.write(h.context(), s);

    const auto rows = ores::marketdata::repository::market_series_identity_repository{}.read_latest(
        h.context(), boost::uuids::to_string(s.id));
    REQUIRE(rows.size() == 1);
    CHECK(rows[0].index == "power");
    CHECK(rows[0].index_name == "ICE:PDQ");
    CHECK(rows[0].delivery == "2021-01-01");
    CHECK(rows[0].delivery_start == "10800");
    CHECK(rows[0].delivery_end == "14400");
}

TEST_CASE("a_cmb_fixing_projects_its_family", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    const auto s =
        make_fixing_test_series(h,
                                boost::uuids::random_generator{}(),
                                "oresmd://security/GOVT?type=fixing&index=cmb&tenor=10Y");
    market_series_repository{}.write(h.context(), s);

    const auto rows = ores::marketdata::repository::market_series_identity_repository{}.read_latest(
        h.context(), boost::uuids::to_string(s.id));
    REQUIRE(rows.size() == 1);
    CHECK(rows[0].index == "cmb");
    CHECK(rows[0].family == "GOVT");
    CHECK(rows[0].tenor == "10Y");
}

TEST_CASE("two_market_series_identities_cannot_spell_one_identity", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    ores::marketdata::domain::market_series_identity first;
    first.tenant_id = h.tenant_id();
    first.series_id = boost::uuids::random_generator{}();
    first.party_id = boost::uuids::random_generator{}();
    first.identity_kind = "index";
    first.asset_class = "fx";
    first.index = "fx";
    first.unit_ccy = "EUR";
    first.ccy = "GBP";
    first.source = "ECB";

    ores::marketdata::repository::market_series_identity_repository repo;
    repo.write(h.context(), first);

    // The same identity spelled by another series: the natural key refuses it,
    // so a join on the decomposed columns can never match two rows.
    auto second = first;
    second.series_id = boost::uuids::random_generator{}();
    CHECK_THROWS(repo.write(h.context(), second));
}

TEST_CASE("reprojecting_fills_a_new_column_an_earlier_projection_left_empty", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository repo;
    const auto s =
        make_fixing_test_series(h,
                                boost::uuids::random_generator{}(),
                                "oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=GBP");
    repo.write(h.context(), s);

    // The row an earlier projector wrote: every field that had a column then,
    // and nothing for the source, which had none.
    ores::marketdata::domain::market_series_identity stale;
    stale.tenant_id = h.tenant_id();
    stale.series_id = s.id;
    stale.party_id = s.party_id;
    stale.identity_kind = "index";
    stale.asset_class = "fx";
    stale.index = "fx";
    stale.unit_ccy = "EUR";
    stale.ccy = "GBP";
    ores::marketdata::repository::market_series_identity_repository{}.write(h.context(), stale);

    const auto first =
        ores::marketdata::repository::market_series_identity_projector::reproject(h.context(), {s});
    CHECK(first.written == 1);
    CHECK(first.unchanged == 0);

    const auto rows = ores::marketdata::repository::market_series_identity_repository{}.read_latest(
        h.context(), boost::uuids::to_string(s.id));
    REQUIRE(rows.size() == 1);
    CHECK(rows[0].source == "ECB");

    const auto second =
        ores::marketdata::repository::market_series_identity_projector::reproject(h.context(), {s});
    CHECK(second.written == 0);
    CHECK(second.unchanged == 1);
}
