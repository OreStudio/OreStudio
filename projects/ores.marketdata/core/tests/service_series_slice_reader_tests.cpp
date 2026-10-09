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
#include "ores.marketdata.core/service/series_slice_reader.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.platform/time/datetime.hpp"
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
const std::string tags("[service][series_slice_reader]");

using ores::marketdata::messaging::get_series_slice_request;
using ores::marketdata::messaging::get_series_slice_response;
using ores::marketdata::messaging::resolve_series_identity_request;
using ores::marketdata::service::series_slice_reader;
using ores::testing::database_helper;

/// Writes @p content through the import, so the reader reads stored data.
void import(database_helper& h, const std::string& content) {
    ores::nats::service::nats_client auth_nats;
    ores::marketdata::service::import_service svc(h.context(), auth_nats);
    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = content;
    req.source = "test.series_slice_reader";
    const auto resp = svc.import(req);
    REQUIRE(resp.success);
}

/// The FX option surface on a pair, as the resolver names one.
resolve_series_identity_request fx_option_identity(const std::string& unit_ccy,
                                                   const std::string& ccy) {
    resolve_series_identity_request id;
    id.asset = "fx";
    id.scope = unit_ccy;
    id.instrument_type = "fx_option";
    id.quote_type = "rate_lnvol";
    id.fields.push_back({"ccy", ccy});
    return id;
}

/// A curve id no earlier run used, so the series is this test's alone.
std::string fresh_curve_id() {
    auto tag = boost::uuids::to_string(boost::uuids::random_generator{}());
    tag.erase(std::remove(tag.begin(), tag.end(), '-'), tag.end());
    return tag;
}

/// A zero coupon curve, as the resolver names one.
resolve_series_identity_request zero_identity(const std::string& curve_id) {
    resolve_series_identity_request id;
    id.asset = "ir";
    id.scope = "EUR";
    id.instrument_type = "zero";
    id.quote_type = "rate";
    id.fields.push_back({"curve_id", curve_id});
    id.fields.push_back({"day_counter", "A365"});
    return id;
}

get_series_slice_request slice_request(resolve_series_identity_request identity,
                                       std::string component_field,
                                       std::string component_value,
                                       const std::string& from,
                                       const std::string& to) {
    get_series_slice_request req;
    req.identity = std::move(identity);
    req.component_field = std::move(component_field);
    req.component_value = std::move(component_value);
    req.from_instant = ores::platform::time::datetime::from_iso8601_utc(from);
    req.to_instant = ores::platform::time::datetime::from_iso8601_utc(to);
    return req;
}

}

TEST_CASE("a_slice_returns_one_component_as_a_ladder_at_each_instant", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    import(h,
           "20160210 FX_OPTION/RATE_LNVOL/NZD/USD/1Y/ATM 0.080\n"
           "20160210 FX_OPTION/RATE_LNVOL/NZD/USD/2Y/ATM 0.085\n"
           "20160210 FX_OPTION/RATE_LNVOL/NZD/USD/1Y/25RR 0.011\n");
    import(h, "20160211 FX_OPTION/RATE_LNVOL/NZD/USD/1Y/ATM 0.090\n");
    import(h, "20160212 FX_OPTION/RATE_LNVOL/NZD/USD/2Y/25RR 0.012\n");

    const auto resp = series_slice_reader::read(h.context(),
                                                slice_request(fx_option_identity("NZD", "USD"),
                                                              "strike_label",
                                                              "ATM",
                                                              "2016-02-10T00:00:00Z",
                                                              "2016-02-12T00:00:00Z"));

    REQUIRE(resp.success);
    CHECK(resp.message.empty());
    // The ladder is the shape's: both expiries the object declares, in the order
    // the shape stores them.
    CHECK(resp.coordinate_field == "expiry");
    CHECK(resp.coordinates == std::vector<std::string>{"1Y", "2Y"});
    REQUIRE(resp.instants.size() == 3);

    CHECK(resp.instants[0].values == std::vector<std::string>{"0.080", "0.085"});
    // The second instant states only the 1Y at-the-money point, so the 2Y node is
    // a hole there: the read reports what the object states at an instant and
    // not what it last held.
    CHECK(resp.instants[1].values == std::vector<std::string>{"0.090", ""});
    // The third instant states no at-the-money point at all, so both nodes are
    // holes and the instant is still a row.
    CHECK(resp.instants[2].values == std::vector<std::string>{"", ""});
}

TEST_CASE("a_slice_names_a_node_the_shape_declares_and_no_point_fills_as_a_hole", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    import(h,
           "20160310 FX_OPTION/RATE_LNVOL/NZD/JPY/1Y/25RR 0.011\n"
           "20160310 FX_OPTION/RATE_LNVOL/NZD/JPY/2Y/ATM 0.085\n");

    const auto resp = series_slice_reader::read(h.context(),
                                                slice_request(fx_option_identity("NZD", "JPY"),
                                                              "strike_label",
                                                              "25RR",
                                                              "2016-03-10T00:00:00Z",
                                                              "2016-03-10T00:00:00Z"));

    REQUIRE(resp.success);
    CHECK(resp.coordinates == std::vector<std::string>{"1Y", "2Y"});
    REQUIRE(resp.instants.size() == 1);
    // 2Y is a declared expiry and the risk reversal holds no point there, so the
    // node is a hole and not a dropped node.
    CHECK(resp.instants[0].values == std::vector<std::string>{"0.011", ""});
}

TEST_CASE("a_slice_of_a_one_axis_object_is_the_whole_object", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto curve = fresh_curve_id();
    import(h,
           "20160410 ZERO/RATE/EUR/" + curve +
               "/A365/1Y 0.010\n"
               "20160410 ZERO/RATE/EUR/" +
               curve + "/A365/2Y 0.020\n");

    const auto resp = series_slice_reader::read(
        h.context(),
        slice_request(
            zero_identity(curve), "", "", "2016-04-10T00:00:00Z", "2016-04-10T00:00:00Z"));

    REQUIRE(resp.success);
    CHECK(resp.coordinate_field == "term");
    CHECK(resp.coordinates == std::vector<std::string>{"1Y", "2Y"});
    REQUIRE(resp.instants.size() == 1);
    CHECK(resp.instants[0].values == std::vector<std::string>{"0.010", "0.020"});
}

TEST_CASE("a_slice_refuses_a_component_the_shape_does_not_declare", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    import(h, "20160510 FX_OPTION/RATE_LNVOL/NZD/SEK/1Y/ATM 0.080\n");
    const auto identity = fx_option_identity("NZD", "SEK");
    const auto read = [&](const std::string& field, const std::string& value) {
        return series_slice_reader::read(
            h.context(),
            slice_request(identity, field, value, "2016-05-10T00:00:00Z", "2016-05-10T00:00:00Z"));
    };

    // A value the axis does not hold.
    const auto bad_value = read("strike_label", "FLY");
    CHECK_FALSE(bad_value.success);
    CHECK(bad_value.message.find("FLY") != std::string::npos);
    CHECK(bad_value.message.find("strike_label") != std::string::npos);

    // A field the object does not declare as an axis.
    const auto bad_field = read("curve_id", "ATM");
    CHECK_FALSE(bad_field.success);
    CHECK(bad_field.message.find("curve_id") != std::string::npos);

    // A two-axis object needs the axis the component fixes.
    const auto unnamed = read("", "");
    CHECK_FALSE(unnamed.success);
    CHECK(unnamed.message.find("strike_label") != std::string::npos);
    CHECK(unnamed.message.find("expiry") != std::string::npos);

    // A range that ends before it starts.
    const auto reversed = series_slice_reader::read(
        h.context(),
        slice_request(
            identity, "strike_label", "ATM", "2016-05-11T00:00:00Z", "2016-05-10T00:00:00Z"));
    CHECK_FALSE(reversed.success);
    CHECK(reversed.message.find("range") != std::string::npos);
}

TEST_CASE("a_slice_of_an_identity_no_series_carries_is_empty_and_not_an_error", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto resp = series_slice_reader::read(h.context(),
                                                slice_request(fx_option_identity("NZD", "XXX"),
                                                              "strike_label",
                                                              "ATM",
                                                              "2016-06-10T00:00:00Z",
                                                              "2016-06-10T00:00:00Z"));

    CHECK(resp.success);
    CHECK(resp.instants.empty());
    CHECK(resp.coordinates.empty());
}
