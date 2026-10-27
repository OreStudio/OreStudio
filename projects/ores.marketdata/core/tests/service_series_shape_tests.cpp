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
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/service/import_service.hpp"
#include "ores.marketdata.core/service/series_shape.hpp"
#include "ores.marketdata.core/service/series_snapshot_reader.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.testing/database_helper.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

using namespace ores::logging;

namespace {

using ores::marketdata::service::in_term_order;
using ores::testing::database_helper;

using strings = std::vector<std::string>;

}

TEST_CASE("terms_are_ordered_by_length_whatever_order_a_source_stated_them", "[service][series_shape]") {
    auto lg(make_logger(test_suite));

    CHECK(in_term_order({"2M",
                         "10Y",
                         "30Y",
                         "5Y",
                         "7Y",
                         "3M",
                         "1Y",
                         "9M",
                         "4Y",
                         "1M",
                         "1W",
                         "6M",
                         "2Y",
                         "3Y",
                         "18M"}) == strings{"1W",
                                            "1M",
                                            "2M",
                                            "3M",
                                            "6M",
                                            "9M",
                                            "1Y",
                                            "18M",
                                            "2Y",
                                            "3Y",
                                            "4Y",
                                            "5Y",
                                            "7Y",
                                            "10Y",
                                            "30Y"});
}

TEST_CASE("a_compound_period_sits_by_its_length", "[service][series_shape]") {
    auto lg(make_logger(test_suite));

    CHECK(in_term_order({"2Y", "1Y6M", "1Y", "13M"}) == strings{"1Y", "13M", "1Y6M", "2Y"});
}

TEST_CASE("overnight_tenors_come_before_periods_and_dates_and_continuations_after", "[service][series_shape]") {
    auto lg(make_logger(test_suite));

    CHECK(in_term_order({"c2", "2030-01-15", "1W", "SN", "ON", "c1", "TN", "1M"}) ==
          strings{"ON", "TN", "SN", "1W", "1M", "2030-01-15", "c1", "c2"});
}

TEST_CASE("a_value_that_is_no_term_follows_the_terms_in_its_stated_order", "[service][series_shape]") {
    auto lg(make_logger(test_suite));

    CHECK(in_term_order({"odd", "1Y", "other", "1M"}) == strings{"1M", "1Y", "odd", "other"});
}

TEST_CASE("terms_a_few_days_apart_are_ordered_by_their_days", "[service][series_shape]") {
    auto lg(make_logger(test_suite));

    // A month counts as 30 days and a year as 365, so 12M is 360 days, 52W is 364
    // and 1Y is 365. They are three lengths and not a tie.
    CHECK(in_term_order({"1Y", "52W", "12M"}) == strings{"12M", "52W", "1Y"});
}

TEST_CASE("terms_of_equal_length_keep_their_stated_order", "[service][series_shape]") {
    auto lg(make_logger(test_suite));

    // 7D and 1W are two spellings of seven days.
    CHECK(in_term_order({"7D", "1W"}) == strings{"7D", "1W"});
    CHECK(in_term_order({"1W", "7D"}) == strings{"1W", "7D"});
}

TEST_CASE("an_imported_surface_is_read_with_its_expiries_in_order", "[service][series_shape]") {
    auto lg(make_logger(test_suite));

    database_helper h;
    ores::nats::service::nats_client auth_nats;
    ores::marketdata::service::import_service svc(h.context(), auth_nats);
    ores::marketdata::messaging::import_market_data_request req;
    req.source = "test.series_shape";
    req.market_data_content = "20180110 FX_OPTION/RATE_LNVOL/NZD/RUB/2Y/ATM 0.085\n"
                              "20180110 FX_OPTION/RATE_LNVOL/NZD/RUB/1M/ATM 0.070\n"
                              "20180110 FX_OPTION/RATE_LNVOL/NZD/RUB/10Y/ATM 0.120\n"
                              "20180110 FX_OPTION/RATE_LNVOL/NZD/RUB/3M/ATM 0.075\n";
    REQUIRE(svc.import(req).success);

    ores::marketdata::messaging::get_curve_snapshot_request snapshot_req;
    snapshot_req.identity.asset = "fx";
    snapshot_req.identity.scope = "NZD";
    snapshot_req.identity.instrument_type = "fx_option";
    snapshot_req.identity.quote_type = "rate_lnvol";
    snapshot_req.identity.fields.push_back({"ccy", "RUB"});
    const auto resp = ores::marketdata::service::series_snapshot_reader::read(
        h.context(),
        snapshot_req,
        ores::platform::time::datetime::from_iso8601_utc("2018-01-11T00:00:00Z"));

    REQUIRE(resp.success);
    REQUIRE(resp.nodes.size() == 4);
    CHECK(resp.nodes[0].coordinates.front() == "1M");
    CHECK(resp.nodes[1].coordinates.front() == "3M");
    CHECK(resp.nodes[2].coordinates.front() == "2Y");
    CHECK(resp.nodes[3].coordinates.front() == "10Y");
    CHECK(resp.values == strings{"0.070", "0.075", "0.085", "0.120"});
}

TEST_CASE("an_axis_that_is_no_term_keeps_the_order_its_source_stated", "[service][series_shape]") {
    auto lg(make_logger(test_suite));

    database_helper h;
    ores::nats::service::nats_client auth_nats;
    ores::marketdata::service::import_service svc(h.context(), auth_nats);
    ores::marketdata::messaging::import_market_data_request req;
    req.source = "test.series_shape";
    // One expiry, and strikes stated in an order no sort by name or length gives.
    req.market_data_content = "20180111 FX_OPTION/RATE_LNVOL/NZD/CNH/1Y/25RR 0.011\n"
                              "20180111 FX_OPTION/RATE_LNVOL/NZD/CNH/1Y/ATM 0.080\n"
                              "20180111 FX_OPTION/RATE_LNVOL/NZD/CNH/1Y/25BF 0.004\n";
    REQUIRE(svc.import(req).success);

    ores::marketdata::messaging::get_curve_snapshot_request snapshot_req;
    snapshot_req.identity.asset = "fx";
    snapshot_req.identity.scope = "NZD";
    snapshot_req.identity.instrument_type = "fx_option";
    snapshot_req.identity.quote_type = "rate_lnvol";
    snapshot_req.identity.fields.push_back({"ccy", "CNH"});
    const auto resp = ores::marketdata::service::series_snapshot_reader::read(
        h.context(),
        snapshot_req,
        ores::platform::time::datetime::from_iso8601_utc("2018-01-12T00:00:00Z"));

    REQUIRE(resp.success);
    REQUIRE(resp.nodes.size() == 3);
    CHECK(resp.values == strings{"0.011", "0.080", "0.004"});
}
