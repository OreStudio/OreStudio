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
