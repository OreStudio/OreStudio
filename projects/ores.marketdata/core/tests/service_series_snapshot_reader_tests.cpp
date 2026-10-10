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
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_identity_reader.hpp"
#include "ores.marketdata.core/service/import_service.hpp"
#include "ores.marketdata.core/service/series_evolution_reader.hpp"
#include "ores.marketdata.core/service/series_shape.hpp"
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

TEST_CASE("a_snapshot_asked_for_provenance_names_the_source_of_each_value", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    import(h,
           "20170910 FX_OPTION/RATE_LNVOL/NZD/PLN/1Y/ATM 0.080\n"
           "20170910 FX_OPTION/RATE_LNVOL/NZD/PLN/2Y/ATM 0.085\n"
           "20170910 FX_OPTION/RATE_LNVOL/NZD/PLN/1Y/25RR 0.011\n");

    const auto identity = fx_option_identity("NZD", "PLN");
    const auto series =
        ores::marketdata::repository::market_series_identity_reader{}.read(h.context(), identity);
    REQUIRE(series.size() == 1);
    ores::marketdata::repository::market_observation_repository obs_repo;
    // The point to over-key is the one whose expiry coordinate is 2Y at the ATM
    // strike, found by its coordinates and not by the layout of its URI.
    std::string over_keyed_uri;
    for (const auto& obs : obs_repo.read_latest_for_series(h.context(), series.front().id)) {
        const auto parsed = ores::marketdata::datum::oresmd_uri_codec::read(obs.oresmd_uri);
        REQUIRE(parsed);
        const auto expiry = ores::marketdata::service::coordinate_of(
            *parsed, *ores::marketdata::datum::field_named("expiry"));
        const auto strike = ores::marketdata::service::coordinate_of(
            *parsed, *ores::marketdata::datum::field_named("strike_label"));
        if (expiry == "2Y" && strike == "ATM")
            over_keyed_uri = obs.oresmd_uri;
    }
    REQUIRE_FALSE(over_keyed_uri.empty());

    const auto at = instant("2017-09-11T12:00:00Z");
    obs_repo.write_manual_point(
        h.context().with_party(
            h.tenant_id(), series.front().party_id, {series.front().party_id}, h.db_user()),
        series.front().id,
        over_keyed_uri,
        instant("2017-09-10T00:00:00Z"),
        "0.090",
        "system.test",
        "operator over-key");

    auto req = snapshot_request(identity);
    req.include_provenance = true;
    const auto resp = series_snapshot_reader::read(h.context(), req, at);

    REQUIRE(resp.success);
    // Nodes: 1Y/ATM, 1Y/25RR, 2Y/ATM, 2Y/25RR.
    REQUIRE(resp.provenance.size() == resp.nodes.size());
    CHECK(resp.values == std::vector<std::string>{"0.080", "0.011", "0.090", ""});
    CHECK(resp.provenance[0].source_kind == "quoted");
    CHECK(resp.provenance[0].modified_by.empty());
    CHECK(resp.provenance[1].source_kind == "quoted");
    CHECK(resp.provenance[2].source_kind == "manual");
    CHECK(resp.provenance[2].modified_by == h.db_user());
    CHECK(resp.provenance[2].change_reason_code == "system.test");
    CHECK(resp.provenance[2].change_commentary == "operator over-key");
    CHECK(resp.provenance[2].recorded_at != std::chrono::system_clock::time_point{});
    // A hole has no source.
    CHECK(resp.provenance[3].source_kind.empty());
}

TEST_CASE("a_snapshot_not_asked_for_provenance_carries_none", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    import(h, "20170911 FX_OPTION/RATE_LNVOL/NZD/CZK/1Y/ATM 0.080\n");

    const auto resp =
        series_snapshot_reader::read(h.context(),
                                     snapshot_request(fx_option_identity("NZD", "CZK")),
                                     instant("2017-09-12T12:00:00Z"));

    REQUIRE(resp.success);
    CHECK(resp.values == std::vector<std::string>{"0.080"});
    CHECK(resp.provenance.empty());
}
