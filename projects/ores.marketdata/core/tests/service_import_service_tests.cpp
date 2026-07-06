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
    import_service svc(h.context());

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
    import_service svc(h.context());

    ores::marketdata::messaging::import_market_data_request req;
    // Market data has a duplicate; fixings does not.
    req.market_data_content = "20160205 FX/RATE/EUR/CHF 1.0\n"
                              "20160205 FX/RATE/EUR/CHF 1.5\n";
    req.fixings_content = "2016-02-05 EUR-EONIA 0.001\n";
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
    import_service svc(h.context());
    ores::marketdata::repository::market_series_repository series_repo;
    ores::marketdata::repository::market_observations_repository obs_repo;

    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = "20160205 FX/RATE/EUR/USD 1.132337\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    REQUIRE(resp.success);
    REQUIRE(resp.observation_count == 1);

    const auto series = series_repo.read_latest_by_type(h.context(), "FX", "RATE", "EUR/USD");
    REQUIRE(series.size() == 1);
    CHECK(series.front().is_scalar);

    const auto observations = obs_repo.read_latest(h.context(), series.front().id);
    REQUIRE(observations.size() == 1);
    CHECK(observations.front().point_id == "SPOT");
}

TEST_CASE("import_leaves_point_id_empty_for_non_scalar_series_with_short_key", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    import_service svc(h.context());
    ores::marketdata::repository::market_series_repository series_repo;
    ores::marketdata::repository::market_observations_repository obs_repo;

    ores::marketdata::messaging::import_market_data_request req;
    // IR_SWAP has qualifier_depth 3 (currency/index_tenor/fixed_freq), so a
    // key with only 2 qualifier segments is short: decompose_key treats it
    // as a malformed/no-point_id key (not a scalar quote), and the import
    // must not mislabel it as "SPOT".
    req.market_data_content = "20160205 IR_SWAP/RATE/EUR/2D/1D 0.01\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    REQUIRE(resp.success);
    REQUIRE(resp.observation_count == 1);

    const auto series = series_repo.read_latest_by_type(h.context(), "IR_SWAP", "RATE", "EUR/2D/1D");
    REQUIRE(series.size() == 1);
    CHECK_FALSE(series.front().is_scalar);

    const auto observations = obs_repo.read_latest(h.context(), series.front().id);
    REQUIRE(observations.size() == 1);
    CHECK(observations.front().point_id.empty());
}
