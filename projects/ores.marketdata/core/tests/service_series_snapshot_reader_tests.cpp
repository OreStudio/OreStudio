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
#include "ores.marketdata.core/service/series_evolution_reader.hpp"
#include "ores.marketdata.core/service/series_snapshot_reader.hpp"
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
const std::string tags("[service][series_snapshot_reader]");

using ores::marketdata::messaging::get_curve_snapshot_request;
using ores::marketdata::messaging::get_series_evolution_request;
using ores::marketdata::messaging::resolve_series_identity_request;
using ores::marketdata::service::series_evolution_reader;
using ores::marketdata::service::series_snapshot_reader;
using ores::testing::database_helper;

/// Writes @p content through the import, so the reader reads stored data.
void import(database_helper& h, const std::string& content) {
    ores::nats::service::nats_client auth_nats;
    ores::marketdata::service::import_service svc(h.context(), auth_nats);
    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = content;
    req.source = "test.series_snapshot_reader";
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

/// A spot rate on a pair, as the resolver names one. It carries no coordinate,
/// so it has no grid.
resolve_series_identity_request fx_spot_identity(const std::string& unit_ccy,
                                                 const std::string& ccy) {
    resolve_series_identity_request id;
    id.asset = "fx";
    id.scope = unit_ccy;
    id.instrument_type = "fx_spot";
    id.quote_type = "rate";
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

std::chrono::system_clock::time_point instant(const std::string& iso) {
    return ores::platform::time::datetime::from_iso8601_utc(iso);
}

get_curve_snapshot_request snapshot_request(resolve_series_identity_request identity) {
    get_curve_snapshot_request req;
    req.identity = std::move(identity);
    return req;
}

}

TEST_CASE("a_snapshot_states_the_declared_grid_and_the_value_at_each_node", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    import(h,
           "20170410 FX_OPTION/RATE_LNVOL/NZD/SEK/1Y/ATM 0.080\n"
           "20170410 FX_OPTION/RATE_LNVOL/NZD/SEK/2Y/ATM 0.085\n"
           "20170410 FX_OPTION/RATE_LNVOL/NZD/SEK/1Y/25RR 0.011\n");
    import(h, "20170411 FX_OPTION/RATE_LNVOL/NZD/SEK/2Y/25RR 0.012\n");

    const auto resp =
        series_snapshot_reader::read(h.context(),
                                     snapshot_request(fx_option_identity("NZD", "SEK")),
                                     instant("2017-04-11T12:00:00Z"));

    REQUIRE(resp.success);
    CHECK(resp.message.empty());
    CHECK(resp.coordinate_fields == std::vector<std::string>{"expiry", "strike_label"});
    REQUIRE(resp.nodes.size() == 4);
    CHECK(resp.nodes[0].coordinates == std::vector<std::string>{"1Y", "ATM"});
    CHECK(resp.nodes[3].coordinates == std::vector<std::string>{"2Y", "25RR"});
    // Each node holds the latest value at or before the instant, and a node no
    // point states is a hole.
    CHECK(resp.values == std::vector<std::string>{"0.080", "0.011", "0.085", "0.012"});
    REQUIRE(resp.recorded_at.size() == resp.nodes.size());
    CHECK(resp.as_of == instant("2017-04-11T12:00:00Z"));
}

TEST_CASE("a_node_no_point_states_is_a_hole_with_no_record_time", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    import(h,
           "20170510 FX_OPTION/RATE_LNVOL/NZD/NOK/1Y/ATM 0.080\n"
           "20170510 FX_OPTION/RATE_LNVOL/NZD/NOK/2Y/ATM 0.085\n");
    import(h, "20170511 FX_OPTION/RATE_LNVOL/NZD/NOK/1Y/25RR 0.011\n");

    const auto resp =
        series_snapshot_reader::read(h.context(),
                                     snapshot_request(fx_option_identity("NZD", "NOK")),
                                     instant("2017-05-10T12:00:00Z"));

    REQUIRE(resp.success);
    // 1Y/25RR is declared by the later import but not yet stated at the instant.
    REQUIRE(resp.nodes.size() == 4);
    CHECK(resp.values == std::vector<std::string>{"0.080", "", "0.085", ""});
    CHECK(resp.recorded_at[1] == std::chrono::system_clock::time_point{});
    CHECK(resp.recorded_at[3] == std::chrono::system_clock::time_point{});
    CHECK(resp.recorded_at[0] != std::chrono::system_clock::time_point{});
}

TEST_CASE("a_snapshot_and_an_evolution_agree_on_the_nodes_and_on_a_stated_value", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    import(h,
           "20170610 FX_OPTION/RATE_LNVOL/NZD/DKK/1Y/ATM 0.080\n"
           "20170610 FX_OPTION/RATE_LNVOL/NZD/DKK/2Y/ATM 0.085\n"
           "20170610 FX_OPTION/RATE_LNVOL/NZD/DKK/1Y/10RR 0.013\n");
    import(h, "20170611 FX_OPTION/RATE_LNVOL/NZD/DKK/2Y/25RR 0.012\n");

    const auto at = instant("2017-06-11T00:00:00Z");
    const auto snapshot = series_snapshot_reader::read(
        h.context(), snapshot_request(fx_option_identity("NZD", "DKK")), at);

    get_series_evolution_request evolution_req;
    evolution_req.identity = fx_option_identity("NZD", "DKK");
    evolution_req.instants.push_back(at);
    const auto evolution = series_evolution_reader::read(h.context(), evolution_req);

    REQUIRE(snapshot.success);
    REQUIRE(evolution.success);
    CHECK(snapshot.coordinate_fields == evolution.coordinate_fields);
    REQUIRE(snapshot.nodes.size() == evolution.nodes.size());
    for (std::size_t n = 0; n < snapshot.nodes.size(); ++n)
        CHECK(snapshot.nodes[n].coordinates == evolution.nodes[n].coordinates);
    REQUIRE(evolution.instants.size() == 1);
    // The evolution states only what the instant states, and the snapshot carries
    // the latest value forward. So the snapshot may hold a value where the
    // evolution holds a hole, and wherever the evolution holds one the two agree.
    REQUIRE(snapshot.values.size() == evolution.instants[0].values.size());
    CHECK(evolution.instants[0].values == std::vector<std::string>{"", "", "", "", "", "0.012"});
    for (std::size_t n = 0; n < snapshot.values.size(); ++n)
        if (!evolution.instants[0].values[n].empty())
            CHECK(snapshot.values[n] == evolution.instants[0].values[n]);
    CHECK(snapshot.values == std::vector<std::string>{"0.080", "0.013", "", "0.085", "", "0.012"});
}

TEST_CASE("a_snapshot_of_an_object_with_no_axis_is_refused", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    import(h, "20170710 FX/RATE/NZD/DKK 4.5\n");

    const auto resp = series_snapshot_reader::read(h.context(),
                                                   snapshot_request(fx_spot_identity("NZD", "DKK")),
                                                   instant("2017-07-10T12:00:00Z"));

    CHECK_FALSE(resp.success);
    CHECK_FALSE(resp.message.empty());
}

TEST_CASE("a_snapshot_of_an_identity_no_series_carries_is_empty", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto at = instant("2017-08-10T12:00:00Z");
    const auto resp = series_snapshot_reader::read(
        h.context(), snapshot_request(zero_identity(fresh_curve_id())), at);

    REQUIRE(resp.success);
    CHECK(resp.nodes.empty());
    CHECK(resp.values.empty());
    CHECK(resp.as_of == at);
}
