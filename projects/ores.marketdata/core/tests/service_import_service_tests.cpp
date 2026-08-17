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
#include "ores.marketdata.core/repository/market_observations_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/service/import_service.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.testing/database_helper.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string_view test_suite("ores.marketdata.core.tests");
const std::string tags("[service][import_service]");

}

using namespace ores::logging;
using ores::marketdata::service::import_service;
using ores::testing::database_helper;

TEST_CASE("import_dedupes_duplicate_observation_and_reports_warning", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);

    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = "20160205 FX/RATE/EUR/CHF 1.0\n"
                              "20160205 FX/RATE/EUR/CHF 1.5\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    CHECK(resp.success);
    CHECK(resp.observation_count == 1);
    REQUIRE(resp.warnings.size() == 1);
    CHECK(resp.errors.empty());
    CHECK(resp.warnings[0].find("FX/RATE/EUR/CHF") != std::string::npos);
}

TEST_CASE("import_with_duplicates_are_errors_skips_only_the_affected_section", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);

    ores::marketdata::messaging::import_market_data_request req;
    // Market data has a duplicate; fixings does not.
    req.market_data_content = "20160205 FX/RATE/EUR/CHF 1.0\n"
                              "20160205 FX/RATE/EUR/CHF 1.5\n";
    req.fixings_content = "2016-02-05 EUR-ESTR 0.001\n";
    req.source = "test.import_service";
    req.duplicates_are_errors = true;

    const auto resp = svc.import(req);

    // Overall failure...
    CHECK_FALSE(resp.success);
    REQUIRE(resp.errors.size() == 1);
    CHECK(resp.warnings.empty());

    // ...but the clean fixings section was still persisted, and its count
    // is still surfaced rather than being silently dropped alongside the
    // errored market-data section.
    CHECK(resp.observation_count == 0);
    CHECK(resp.fixing_count == 1);
}

TEST_CASE("import_defaults_point_id_to_spot_for_scalar_series", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);
    ores::marketdata::repository::market_series_repository series_repo;
    ores::marketdata::repository::market_observations_repository obs_repo;

    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = "20160205 FX/RATE/EUR/USD 1.132337\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    REQUIRE(resp.success);
    REQUIRE(resp.observation_count == 1);

    const auto series =
        series_repo.read_latest_by_uri(h.context(), "oresmd://fx/eurusd?type=quote&quote=spot");
    REQUIRE(series.size() == 1);

    const auto observations = obs_repo.read_latest(h.context(), series.front().id);
    REQUIRE(observations.size() == 1);
    CHECK(observations.front().point_id == "SPOT");
}

TEST_CASE("import_preserves_the_point_id_of_a_non_scalar_series", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);
    ores::marketdata::repository::market_series_repository series_repo;
    ores::marketdata::repository::market_observations_repository obs_repo;

    ores::marketdata::messaging::import_market_data_request req;
    // A full IR_SWAP key (6 segments) maps to an identifier carrying a
    // point; the observation keeps the key's point rather than being
    // mislabelled as "SPOT".
    req.market_data_content = "20160205 IR_SWAP/RATE/EUR/2D/3M/PAR_RATE 0.01\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    REQUIRE(resp.success);
    REQUIRE(resp.observation_count == 1);

    const auto series = series_repo.read_latest_by_uri(
        h.context(), "oresmd://ir/eur?tenor=3m&type=quote&metric=rate&quote=ir_swap&point=par_rate");
    REQUIRE(series.size() == 1);

    const auto observations = obs_repo.read_latest(h.context(), series.front().id);
    REQUIRE(observations.size() == 1);
    CHECK(observations.front().point_id == "PAR_RATE");
}

TEST_CASE("import_drops_unmappable_ore_keys_with_a_warning", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);
    ores::marketdata::repository::market_series_repository series_repo;

    ores::marketdata::messaging::import_market_data_request req;
    // A short IR_SWAP key (5 segments) has no oresmd identifier (the
    // inverse projection needs all six), and BOND has no oresmd mapping at
    // all. Neither row can be represented as a URI, so both are dropped --
    // visibly, via the warning list -- rather than guessed at.
    req.market_data_content = "20160205 IR_SWAP/RATE/EUR/2D/1D 0.01\n"
                              "20160205 BOND/RATE/USD/1.5 99.5\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    REQUIRE(resp.success);
    REQUIRE(resp.observation_count == 0);
    REQUIRE(resp.warnings.size() == 2);
    CHECK(resp.warnings[0].find("IR_SWAP/RATE/EUR/2D/1D") != std::string::npos);
    CHECK(resp.warnings[1].find("BOND/RATE/USD/1.5") != std::string::npos);
}

TEST_CASE("import_leaves_fx_qualifier_untouched_when_currency_pairs_unreachable", tags) {
    auto lg(make_logger(test_suite));

    // auth_nats is unconnected (no live refdata to fetch currency_pair
    // reference data from) — fetch_known_currency_pairs must degrade to an
    // empty known-pairs set rather than throwing, and with no known pairs
    // fx_quote_convention_checker never corrects anything: a genuinely
    // reversed key like FX/RATE/USD/GBP is persisted exactly as given,
    // which is the safe behaviour (never guess without reference data).
    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);
    ores::marketdata::repository::market_series_repository series_repo;

    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = "20160205 FX/RATE/USD/GBP 1.394610179594994\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    REQUIRE(resp.success);
    REQUIRE(resp.observation_count == 1);
    CHECK(resp.warnings.empty());

    const auto series =
        series_repo.read_latest_by_uri(h.context(), "oresmd://fx/usdgbp?type=quote&quote=spot");
    REQUIRE(series.size() == 1);
}
