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
#include "ores.reporting.api/messaging/report_address_market_readiness_protocol.hpp"
#include "ores.reporting.api/messaging/report_run_market_readiness_protocol.hpp"
#include "ores.reporting.api/messaging/report_trade_market_gap_protocol.hpp"
#include "ores.reporting.api/messaging/report_trade_market_readiness_protocol.hpp"
#include "ores.reporting.core/repository/report_address_market_readiness_repository.hpp"
#include "ores.reporting.core/repository/report_run_market_readiness_repository.hpp"
#include "ores.reporting.core/repository/report_trade_market_gap_repository.hpp"
#include "ores.reporting.core/repository/report_trade_market_readiness_repository.hpp"
#include "ores.reporting.core/service/market_readiness.hpp"
#include "ores.reporting.core/service/market_readiness_recorder.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "report_instance_test_fixture.hpp"
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <vector>

namespace {

using namespace ores::reporting::service;

address_readiness an_answer_for(const std::string& uri) {
    return address_readiness{.oresmd_uri = uri, .series_id = "series-" + uri, .gap_cause = ""};
}

address_readiness an_absent_series_for(const std::string& uri) {
    return address_readiness{
        .oresmd_uri = uri, .series_id = std::nullopt, .gap_cause = "no_series"};
}

trade_requirement a_trade_needing(const std::string& trade_id, std::vector<std::string> uris) {
    return trade_requirement{.trade_id = trade_id, .oresmd_uris = std::move(uris)};
}

}

TEST_CASE("a_run_records_its_readiness_at_the_three_grains_and_the_trade_gaps",
          "[market_readiness][recorder]") {
    ores::testing::scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = ores::reporting::tests::write_party_and_scope_context(h, gen);

    const auto readiness = decide_market_readiness(
        {"trade-1", "trade-2"},
        {a_trade_needing("trade-1", {"EUR-ESTR-6M"}), a_trade_needing("trade-2", {"USD-SOFR-3M"})},
        {an_answer_for("EUR-ESTR-6M"), an_absent_series_for("USD-SOFR-3M")});
    REQUIRE(readiness.complete == 1);

    const auto instance_id = boost::uuids::random_generator()();
    market_readiness_recorder recorder(ctx);
    const auto refusal =
        recorder.record(readiness_subject{.party_id = *ctx.party_id(),
                                          .report_instance_id = instance_id,
                                          .as_of = std::chrono::system_clock::now()},
                        readiness);
    REQUIRE(refusal.empty());

    const ores::reporting::messaging::report_run_market_readinesses_filter run_filter{
        .report_instance_id = instance_id};
    ores::reporting::repository::report_run_market_readiness_repository runs;
    const auto run = runs.read_latest(ctx, 0, 10, {}, run_filter);
    REQUIRE(run.size() == 1);
    CHECK(run.at(0).booked_count == 2);
    CHECK(run.at(0).complete_count == 1);
    CHECK(run.at(0).excluded_count == 1);

    const ores::reporting::messaging::report_trade_market_readinesses_filter trade_filter{
        .report_instance_id = instance_id};
    ores::reporting::repository::report_trade_market_readiness_repository trades;
    const auto trade_rows = trades.read_latest(ctx, 0, 10, {}, trade_filter);
    REQUIRE(trade_rows.size() == 2);
    const auto complete_of = [&](const std::string& trade_id) {
        for (const auto& row : trade_rows)
            if (row.trade_id == trade_id)
                return row.complete;
        return false;
    };
    CHECK(complete_of("trade-1"));
    CHECK_FALSE(complete_of("trade-2"));

    // The address grain states the cause once, and the gap grain names the trade that lost it.
    const ores::reporting::messaging::report_address_market_readinesses_filter address_filter{
        .report_instance_id = instance_id};
    ores::reporting::repository::report_address_market_readiness_repository addresses;
    const auto address_rows = addresses.read_latest(ctx, 0, 10, {}, address_filter);
    REQUIRE(address_rows.size() == 2);
    const auto cause_of = [&](const std::string& uri) {
        for (const auto& row : address_rows)
            if (row.oresmd_uri == uri)
                return row.gap_cause;
        return std::string("not asked about");
    };
    CHECK(cause_of("EUR-ESTR-6M").empty());
    CHECK(cause_of("USD-SOFR-3M") == "no_series");

    const ores::reporting::messaging::report_trade_market_gaps_filter gap_filter{
        .report_instance_id = instance_id};
    ores::reporting::repository::report_trade_market_gap_repository gaps;
    const auto gap_rows = gaps.read_latest(ctx, 0, 10, {}, gap_filter);
    REQUIRE(gap_rows.size() == 1);
    CHECK(gap_rows.at(0).trade_id == "trade-2");
    CHECK(gap_rows.at(0).oresmd_uri == "USD-SOFR-3M");
}

TEST_CASE("a_run_retried_keeps_one_readiness_record_and_not_two", "[market_readiness][recorder]") {
    ores::testing::scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = ores::reporting::tests::write_party_and_scope_context(h, gen);

    const auto readiness = decide_market_readiness(
        {"trade-1"}, {a_trade_needing("trade-1", {"EUR-ESTR-6M"})}, {an_answer_for("EUR-ESTR-6M")});
    const auto instance_id = boost::uuids::random_generator()();
    const readiness_subject subject{.party_id = *ctx.party_id(),
                                    .report_instance_id = instance_id,
                                    .as_of = std::chrono::system_clock::now()};

    market_readiness_recorder recorder(ctx);
    REQUIRE(recorder.record(subject, readiness).empty());
    // A step that failed after it committed is dispatched again; the second write must land.
    REQUIRE(recorder.record(subject, readiness).empty());

    const ores::reporting::messaging::report_run_market_readinesses_filter run_filter{
        .report_instance_id = instance_id};
    ores::reporting::repository::report_run_market_readiness_repository runs;
    CHECK(runs.read_latest(ctx, 0, 10, {}, run_filter).size() == 1);

    const ores::reporting::messaging::report_trade_market_readinesses_filter trade_filter{
        .report_instance_id = instance_id};
    ores::reporting::repository::report_trade_market_readiness_repository trades;
    CHECK(trades.read_latest(ctx, 0, 10, {}, trade_filter).size() == 1);
}
