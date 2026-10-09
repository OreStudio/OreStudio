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
const std::string tags("[service][series_evolution_reader]");

using ores::marketdata::messaging::get_series_evolution_request;
using ores::marketdata::messaging::get_series_evolution_response;
using ores::marketdata::messaging::resolve_series_identity_request;
using ores::marketdata::service::series_evolution_reader;
using ores::testing::database_helper;

/// Writes @p content through the import, so the reader reads stored data.
void import(database_helper& h, const std::string& content) {
    ores::nats::service::nats_client auth_nats;
    ores::marketdata::service::import_service svc(h.context(), auth_nats);
    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = content;
    req.source = "test.series_evolution_reader";
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

get_series_evolution_request evolution_request(resolve_series_identity_request identity,
                                               const std::string& from,
                                               const std::string& to) {
    get_series_evolution_request req;
    req.identity = std::move(identity);
    req.from_instant = ores::platform::time::datetime::from_iso8601_utc(from);
    req.to_instant = ores::platform::time::datetime::from_iso8601_utc(to);
    return req;
}

/// The coordinate labels of every node, flattened so a test reads them as one
/// list.
std::vector<std::string> labels_of(const get_series_evolution_response& resp) {
    std::vector<std::string> labels;
    for (const auto& node : resp.nodes) {
        std::string joined;
        for (const auto& part : node.coordinates)
            joined += (joined.empty() ? "" : "/") + part;
        labels.push_back(std::move(joined));
    }
    return labels;
}

std::vector<std::string> statuses_of(const get_series_evolution_response& resp) {
    std::vector<std::string> statuses;
    for (const auto& node : resp.nodes)
        statuses.push_back(node.status);
    return statuses;
}

}

TEST_CASE("an_evolution_marks_every_node_of_the_declared_grid", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    // 2Y/10RR is never stated, and the shape declares it all the same, because
    // the axes are declared value by value and not cell by cell.
    import(h,
           "20160410 FX_OPTION/RATE_LNVOL/NZD/CHF/1Y/ATM 0.080\n"
           "20160410 FX_OPTION/RATE_LNVOL/NZD/CHF/2Y/ATM 0.085\n"
           "20160410 FX_OPTION/RATE_LNVOL/NZD/CHF/1Y/25RR 0.011\n"
           "20160410 FX_OPTION/RATE_LNVOL/NZD/CHF/1Y/10RR 0.013\n");
    import(h,
           "20160411 FX_OPTION/RATE_LNVOL/NZD/CHF/1Y/ATM 0.090\n"
           "20160411 FX_OPTION/RATE_LNVOL/NZD/CHF/2Y/25RR 0.012\n");

    const auto resp = series_evolution_reader::read(
        h.context(),
        evolution_request(
            fx_option_identity("NZD", "CHF"), "2016-04-10T00:00:00Z", "2016-04-11T00:00:00Z"));

    REQUIRE(resp.success);
    CHECK(resp.message.empty());
    // The axes, in the order the shape stores them.
    CHECK(resp.coordinate_fields == std::vector<std::string>{"expiry", "strike_label"});
    // The declared grid, with the last axis varying fastest.
    CHECK(labels_of(resp) ==
          std::vector<std::string>{"1Y/ATM", "1Y/25RR", "1Y/10RR", "2Y/ATM", "2Y/25RR", "2Y/10RR"});

    REQUIRE(resp.instants.size() == 2);
    CHECK(resp.instants[0].values ==
          std::vector<std::string>{"0.080", "0.011", "0.013", "0.085", "", ""});
    CHECK(resp.instants[1].values == std::vector<std::string>{"0.090", "", "", "", "0.012", ""});

    // The first instant states three cells, the second states two, and the shape
    // declares six, so every node takes the word its two cells earn it.
    CHECK(statuses_of(resp) ==
          std::vector<std::string>{
              "persistent", "dropped", "dropped", "dropped", "added", "never_quoted"});
}

TEST_CASE("a_node_an_instant_between_the_ends_alone_carries_is_intermittent", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    import(h, "20160510 FX_OPTION/RATE_LNVOL/NZD/CAD/1Y/ATM 0.080\n");
    import(h, "20160511 FX_OPTION/RATE_LNVOL/NZD/CAD/2Y/ATM 0.085\n");
    import(h, "20160512 FX_OPTION/RATE_LNVOL/NZD/CAD/1Y/ATM 0.090\n");

    const auto resp = series_evolution_reader::read(
        h.context(),
        evolution_request(
            fx_option_identity("NZD", "CAD"), "2016-05-10T00:00:00Z", "2016-05-12T00:00:00Z"));

    REQUIRE(resp.success);
    CHECK(labels_of(resp) == std::vector<std::string>{"1Y/ATM", "2Y/ATM"});
    REQUIRE(resp.instants.size() == 3);
    CHECK(resp.instants[0].values == std::vector<std::string>{"0.080", ""});
    CHECK(resp.instants[1].values == std::vector<std::string>{"", "0.085"});
    CHECK(resp.instants[2].values == std::vector<std::string>{"0.090", ""});
    CHECK(statuses_of(resp) == std::vector<std::string>{"intermittent", "intermittent"});
}

TEST_CASE("an_evolution_of_a_one_axis_object_is_the_whole_ladder", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto curve = fresh_curve_id();
    import(h,
           "20160610 ZERO/RATE/EUR/" + curve +
               "/A365/1Y 0.010\n"
               "20160610 ZERO/RATE/EUR/" +
               curve + "/A365/2Y 0.020\n");
    import(h, "20160611 ZERO/RATE/EUR/" + curve + "/A365/2Y 0.025\n");

    const auto resp = series_evolution_reader::read(
        h.context(),
        evolution_request(zero_identity(curve), "2016-06-10T00:00:00Z", "2016-06-11T00:00:00Z"));

    REQUIRE(resp.success);
    CHECK(resp.coordinate_fields == std::vector<std::string>{"term"});
    CHECK(labels_of(resp) == std::vector<std::string>{"1Y", "2Y"});
    REQUIRE(resp.instants.size() == 2);
    CHECK(resp.instants[0].values == std::vector<std::string>{"0.010", "0.020"});
    CHECK(resp.instants[1].values == std::vector<std::string>{"", "0.025"});
    CHECK(statuses_of(resp) == std::vector<std::string>{"dropped", "persistent"});
}

TEST_CASE("a_set_of_instants_the_caller_states_replaces_the_range", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    import(h, "20160710 FX_OPTION/RATE_LNVOL/NZD/EUR/1Y/ATM 0.080\n");
    import(h, "20160711 FX_OPTION/RATE_LNVOL/NZD/EUR/1Y/ATM 0.085\n");
    import(h, "20160712 FX_OPTION/RATE_LNVOL/NZD/EUR/1Y/ATM 0.090\n");

    // The range is not stated, so only the set decides what is read.
    get_series_evolution_request req;
    req.identity = fx_option_identity("NZD", "EUR");
    req.instants.push_back(
        ores::platform::time::datetime::from_iso8601_utc("2016-07-10T00:00:00Z"));
    req.instants.push_back(
        ores::platform::time::datetime::from_iso8601_utc("2016-07-12T00:00:00Z"));

    const auto resp = series_evolution_reader::read(h.context(), req);

    REQUIRE(resp.success);
    CHECK(labels_of(resp) == std::vector<std::string>{"1Y/ATM"});
    // The middle instant is not stated, so it is not in the answer, and the
    // range is not consulted at all.
    REQUIRE(resp.instants.size() == 2);
    CHECK(resp.instants[0].values == std::vector<std::string>{"0.080"});
    CHECK(resp.instants[1].values == std::vector<std::string>{"0.090"});
}

TEST_CASE("an_evolution_is_refused_when_it_cannot_be_read", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    // A spot rate has no coordinate, so it has no grid to read.
    import(h, "20160810 FX/RATE/NZD/AUD 1.07\n");

    const auto no_axis = series_evolution_reader::read(
        h.context(),
        evolution_request(
            fx_spot_identity("NZD", "AUD"), "2016-08-10T00:00:00Z", "2016-08-10T00:00:00Z"));
    CHECK_FALSE(no_axis.success);
    CHECK(no_axis.message.find("no axis") != std::string::npos);

    import(h, "20160810 FX_OPTION/RATE_LNVOL/NZD/GBP/1Y/ATM 0.080\n");
    const auto reversed = series_evolution_reader::read(
        h.context(),
        evolution_request(
            fx_option_identity("NZD", "GBP"), "2016-08-11T00:00:00Z", "2016-08-10T00:00:00Z"));
    CHECK_FALSE(reversed.success);
    CHECK(reversed.message.find("range") != std::string::npos);

    const auto absent = series_evolution_reader::read(
        h.context(),
        evolution_request(
            fx_option_identity("NZD", "XXX"), "2016-08-10T00:00:00Z", "2016-08-10T00:00:00Z"));
    CHECK(absent.success);
    CHECK(absent.instants.empty());
    CHECK(absent.nodes.empty());
}
