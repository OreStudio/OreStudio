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
#include "ores.http/routes/synthetic/ir_curve_coverage.hpp"
#include "ores.logging/logger.hpp"
#include "ores.platform/time/datetime.hpp"
#include "test_suite.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <vector>

using namespace ores::logging;
using ores::http::routes::synthetic::ir_curve_coverage;
using ores::http::routes::synthetic::ir_curve_coverage_input;
using ores::http::routes::synthetic::ir_curve_coverage_row;
using ores::http::routes::synthetic::published_curve_fact;
using ores::refdata::domain::ir_curve_bootstrap_config;
using ores::synthetic::domain::ir_curve_generation_config;
using ores::synthetic::messaging::running_feed;

namespace {

boost::uuids::uuid uuid_of(const std::string& text) {
    static const boost::uuids::string_generator gen;
    return gen(text);
}

/** The clock every case measures a publish against: 2026-11-13 12:00 UTC. */
std::chrono::system_clock::time_point now() {
    return ores::platform::time::datetime::from_db_string("2026-11-13 12:00:00+00");
}

const std::string usd_config_id = "55555555-5555-4555-8555-555555555555";
const std::string usd_bootstrap_id = "66666666-6666-4666-8666-666666666666";
const std::string usd_series_id = "77777777-7777-4777-8777-777777777777";
const std::string usd_source = "synthetic.bloomberg.usdsofr";

ir_curve_generation_config config_of(const std::string& id,
                                     const std::string& currency,
                                     const std::string& source_name,
                                     const std::string& price_source = "fixing") {
    ir_curve_generation_config config;
    config.id = uuid_of(id);
    config.currency_code = currency;
    config.source_name = source_name;
    config.price_source = price_source;
    return config;
}

ir_curve_bootstrap_config bootstrap_of(const std::string& id,
                                       const std::string& currency,
                                       const std::string& name,
                                       const std::string& series_id) {
    ir_curve_bootstrap_config config;
    config.id = uuid_of(id);
    config.currency_code = currency;
    config.name = name;
    config.output_series_id = uuid_of(series_id);
    return config;
}

running_feed run_of(const std::string& source_name,
                    bool from_fixing = true,
                    const std::string& provider_code = "bloomberg",
                    const std::string& note = std::string{}) {
    running_feed run;
    run.source_name = source_name;
    ores::synthetic::messaging::feed_start start;
    start.from_fixing = from_fixing;
    start.provider_code = provider_code;
    start.note = note;
    run.start = start;
    return run;
}

published_curve_fact publish_of(const std::string& as_of,
                                const std::string& state_code = "published") {
    return published_curve_fact{as_of, state_code};
}

ir_curve_generation_config usd_config() {
    return config_of(usd_config_id, "USD", usd_source);
}

ir_curve_bootstrap_config usd_bootstrap() {
    return bootstrap_of(usd_bootstrap_id, "USD", "USD-SOFR", usd_series_id);
}

/** The coverage of the class, with the case's clock. */
std::vector<ir_curve_coverage_row> coverage(ir_curve_coverage_input input) {
    input.now = now();
    return ir_curve_coverage(input);
}

ir_curve_coverage_input usd_input() {
    ir_curve_coverage_input input;
    input.runs = {run_of(usd_source)};
    input.generation_configs = {usd_config()};
    input.bootstrap_configs = {usd_bootstrap()};
    input.published_by_series[usd_series_id] = publish_of("2026-11-13 11:30:00+00");
    return input;
}

}

TEST_CASE("ir_coverage_covers_a_currency_whose_feed_curve_and_publish_are_in_place",
          "[ir_coverage]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the IR coverage of a covered currency";

    const auto rows = coverage(usd_input());
    REQUIRE(rows.size() == 1);
    const auto& row = rows.front();
    CHECK(row.currency_code == "USD");
    CHECK(row.config_id == usd_config_id);
    CHECK(row.source_name == usd_source);
    CHECK(row.provider_code == "bloomberg");
    CHECK(row.bootstrap_config_id == usd_bootstrap_id);
    CHECK(row.bootstrap_config_name == "USD-SOFR");
    CHECK(row.published_as_of == "2026-11-13 11:30:00+00");
    CHECK(row.published_state == "published");
    CHECK(row.gap.empty());
    CHECK(row.gap_reason.empty());
    REQUIRE(row.curves.size() == 1);
    CHECK(row.curves.front().role == "FUNDING");
}

TEST_CASE("ir_coverage_states_no_feed_for_a_currency_only_a_curve_config_holds", "[ir_coverage]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the no_feed gap of the IR coverage";

    auto input = usd_input();
    input.runs.clear();
    input.generation_configs.clear();

    const auto rows = coverage(std::move(input));
    REQUIRE(rows.size() == 1);
    const auto& row = rows.front();
    CHECK(row.config_id.empty());
    CHECK(row.source_name.empty());
    CHECK(row.provider_code.empty());
    CHECK(row.bootstrap_config_id == usd_bootstrap_id);
    CHECK(row.published_as_of == "2026-11-13 11:30:00+00");
    CHECK(row.gap == "no_feed");
    CHECK(row.gap_reason == "No synthetic feed holds this currency.");
}

TEST_CASE("ir_coverage_states_no_source_in_the_note_the_service_recorded", "[ir_coverage]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the no_source gap a run's note states";

    const std::string note = "No series of the class policy publishes an overnight rate in USD.";
    auto input = usd_input();
    input.runs = {run_of(usd_source, false, "", note)};

    const auto rows = coverage(std::move(input));
    REQUIRE(rows.size() == 1);
    CHECK(rows.front().provider_code.empty());
    CHECK(rows.front().gap == "no_source");
    CHECK(rows.front().gap_reason == note);
}

TEST_CASE("ir_coverage_states_no_source_for_a_run_that_kept_its_level_without_a_note",
          "[ir_coverage]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the no_source gap a silent run states";

    auto input = usd_input();
    input.runs = {run_of(usd_source, false, "")};

    const auto rows = coverage(std::move(input));
    REQUIRE(rows.size() == 1);
    CHECK(rows.front().gap == "no_source");
    CHECK(rows.front().gap_reason ==
          "The feed keeps a fixed level, because no public source publishes this currency.");
}

TEST_CASE("ir_coverage_states_no_source_for_a_config_that_starts_from_its_fixed_level",
          "[ir_coverage]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the no_source gap a fixed config states";

    for (const bool running : {false, true}) {
        auto input = usd_input();
        input.generation_configs = {config_of(usd_config_id, "USD", usd_source, "fixed")};
        input.runs =
            running ? std::vector<running_feed>{run_of(usd_source)} : std::vector<running_feed>{};

        const auto rows = coverage(std::move(input));
        REQUIRE(rows.size() == 1);
        CHECK(rows.front().gap == "no_source");
        CHECK(rows.front().gap_reason ==
              "The feed config starts from its fixed level, not from a fixing.");
    }
}

TEST_CASE("ir_coverage_names_the_provider_of_a_run_but_keeps_the_config's_gap", "[ir_coverage]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a fixed config beside a run that started from a fixing";

    auto input = usd_input();
    input.generation_configs = {config_of(usd_config_id, "USD", usd_source, "fixed")};

    const auto rows = coverage(std::move(input));
    REQUIRE(rows.size() == 1);
    CHECK(rows.front().provider_code == "bloomberg");
    CHECK(rows.front().gap == "no_source");
    CHECK(rows.front().gap_reason ==
          "The feed config starts from its fixed level, not from a fixing.");
}

TEST_CASE("ir_coverage_covers_a_currency_whose_feed_takes_a_fixing_but_does_not_run_now",
          "[ir_coverage]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a currency whose feed is not running";

    auto input = usd_input();
    input.runs.clear();

    const auto rows = coverage(std::move(input));
    REQUIRE(rows.size() == 1);
    CHECK(rows.front().provider_code.empty());
    CHECK(rows.front().gap.empty());
}

TEST_CASE("ir_coverage_states_no_curve_config_for_a_source_with_no_bootstrap_config",
          "[ir_coverage]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the no_curve_config gap of the IR coverage";

    auto input = usd_input();
    input.bootstrap_configs.clear();

    const auto rows = coverage(std::move(input));
    REQUIRE(rows.size() == 1);
    const auto& row = rows.front();
    CHECK(row.bootstrap_config_id.empty());
    CHECK(row.bootstrap_config_name.empty());
    CHECK(row.published_as_of.empty());
    CHECK(row.published_state.empty());
    CHECK(row.gap == "no_curve_config");
    CHECK(row.gap_reason == "No bootstrap config holds this currency.");
    CHECK(row.curves.empty());
}

TEST_CASE("ir_coverage_states_no_publish_for_a_curve_never_published", "[ir_coverage]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the no_publish gap of the IR coverage";

    auto input = usd_input();
    input.published_by_series.clear();

    const auto rows = coverage(std::move(input));
    REQUIRE(rows.size() == 1);
    const auto& row = rows.front();
    CHECK(row.published_as_of.empty());
    CHECK(row.published_state.empty());
    CHECK(row.gap == "no_publish");
    CHECK(row.gap_reason == "The curve has never been published.");
}

TEST_CASE("ir_coverage_states_not_live_for_a_latest_set_no_longer_in_use", "[ir_coverage]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the not_live gap of the IR coverage";

    auto input = usd_input();
    input.published_by_series[usd_series_id] = publish_of("2026-11-13 11:55:00+00", "withdrawn");

    const auto rows = coverage(std::move(input));
    REQUIRE(rows.size() == 1);
    CHECK(rows.front().gap == "not_live");
    CHECK(rows.front().gap_reason == "The latest set is not the set in use. It is withdrawn.");
}

TEST_CASE("ir_coverage_measures_staleness_at_thirty_minutes_exactly", "[ir_coverage]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the stale_publish boundary of the IR coverage";

    auto stale = usd_input();
    stale.published_by_series[usd_series_id] = publish_of("2026-11-13 11:29:00+00");
    const auto stale_rows = coverage(std::move(stale));
    REQUIRE(stale_rows.size() == 1);
    CHECK(stale_rows.front().gap == "stale_publish");
    CHECK(stale_rows.front().gap_reason ==
          "The latest curve was published more than 30 minutes ago.");

    auto fresh = usd_input();
    fresh.published_by_series[usd_series_id] = publish_of("2026-11-13 11:30:00+00");
    const auto fresh_rows = coverage(std::move(fresh));
    REQUIRE(fresh_rows.size() == 1);
    CHECK(fresh_rows.front().gap.empty());
    CHECK(fresh_rows.front().gap_reason.empty());
}

TEST_CASE("ir_coverage_reads_a_currency_through_the_feed_that_starts_from_a_public_source",
          "[ir_coverage]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing which of two configs names the currency's source";

    const std::string other_id = "88888888-8888-4888-8888-888888888888";
    const std::string other_source = "synthetic.demo.usdsofr";
    auto input = usd_input();
    input.runs = {run_of(other_source, false, "The config starts from its fixed level."),
                  run_of(usd_source)};
    input.generation_configs = {config_of(other_id, "USD", other_source), usd_config()};

    const auto rows = coverage(std::move(input));
    REQUIRE(rows.size() == 1);
    CHECK(rows.front().config_id == usd_config_id);
    CHECK(rows.front().source_name == usd_source);
    CHECK(rows.front().gap.empty());
}

TEST_CASE("ir_coverage_holds_one_row_per_currency_in_currency_order", "[ir_coverage]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the currency universe and its order";

    const std::string eur_config_id = "99999999-9999-4999-8999-999999999999";
    const std::string eur_bootstrap_id = "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa";
    const std::string eur_series_id = "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb";
    const std::string eur_source = "synthetic.bloomberg.eurestr";

    auto input = usd_input();
    input.runs.push_back(run_of(eur_source));
    input.generation_configs.push_back(config_of(eur_config_id, "EUR", eur_source));
    input.bootstrap_configs.push_back(
        bootstrap_of(eur_bootstrap_id, "EUR", "EUR-ESTR", eur_series_id));
    input.published_by_series[eur_series_id] = publish_of("2026-11-13 11:30:00+00");

    const auto rows = coverage(std::move(input));
    REQUIRE(rows.size() == 2);
    CHECK(rows[0].currency_code == "EUR");
    CHECK(rows[1].currency_code == "USD");
    CHECK(rows[0].gap.empty());
    CHECK(rows[1].gap.empty());
}

TEST_CASE(
    "ir_coverage_lists_every_curve_funding_first_and_each_projection_naming_its_discount_curve",
    "[ir_coverage]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the curves of a currency that has a projection too";

    const std::string libor_id = "88888888-8888-4888-8888-888888888888";
    const std::string libor_series_id = "99999999-9999-4999-8999-999999999999";
    auto projection = bootstrap_of(libor_id, "USD", "USD-LIBOR-3M", libor_series_id);
    projection.curve_family_role = "PROJECTION";
    projection.index_tenor = "3M";
    projection.discount_curve_config_id = uuid_of(usd_bootstrap_id);

    auto funding = usd_bootstrap();
    funding.curve_family_role = "FUNDING";

    auto input = usd_input();
    input.bootstrap_configs = {projection, funding};
    input.published_by_series[libor_series_id] = publish_of("2026-11-13 11:45:00+00");

    const auto rows = coverage(std::move(input));
    REQUIRE(rows.size() == 1);
    const auto& curves = rows.front().curves;
    REQUIRE(curves.size() == 2);

    CHECK(curves[0].bootstrap_config_id == usd_bootstrap_id);
    CHECK(curves[0].name == "USD-SOFR");
    CHECK(curves[0].role == "FUNDING");
    CHECK(curves[0].index_tenor.empty());
    CHECK(curves[0].discount_curve_name.empty());
    CHECK(curves[0].published_as_of == "2026-11-13 11:30:00+00");
    CHECK(curves[0].gap.empty());

    CHECK(curves[1].bootstrap_config_id == libor_id);
    CHECK(curves[1].name == "USD-LIBOR-3M");
    CHECK(curves[1].role == "PROJECTION");
    CHECK(curves[1].index_tenor == "3M");
    CHECK(curves[1].discount_curve_name == "USD-SOFR");
    CHECK(curves[1].published_as_of == "2026-11-13 11:45:00+00");
    CHECK(curves[1].gap.empty());
}

TEST_CASE("ir_coverage_states_each_curve_by_its_own_publish", "[ir_coverage]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a live funding curve beside a projection never published";

    const std::string libor_id = "88888888-8888-4888-8888-888888888888";
    const std::string libor_series_id = "99999999-9999-4999-8999-999999999999";
    auto projection = bootstrap_of(libor_id, "USD", "USD-LIBOR-3M", libor_series_id);
    projection.curve_family_role = "PROJECTION";
    projection.index_tenor = "3M";
    projection.discount_curve_config_id = uuid_of(usd_bootstrap_id);

    auto funding = usd_bootstrap();
    funding.curve_family_role = "FUNDING";

    auto input = usd_input();
    input.bootstrap_configs = {projection, funding};
    input.published_by_series[usd_series_id] = publish_of("2026-11-13 11:45:00+00");

    const auto rows = coverage(std::move(input));
    REQUIRE(rows.size() == 1);
    REQUIRE(rows.front().curves.size() == 2);
    CHECK(rows.front().curves[0].name == "USD-SOFR");
    CHECK(rows.front().curves[0].gap.empty());
    CHECK(rows.front().curves[1].name == "USD-LIBOR-3M");
    CHECK(rows.front().curves[1].gap == "no_publish");
    CHECK(rows.front().gap.empty());
}

TEST_CASE("ir_coverage_covers_a_currency_by_its_funding_curve_not_a_projection", "[ir_coverage]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing which curve names the currency's coverage";

    const std::string libor_id = "88888888-8888-4888-8888-888888888888";
    const std::string libor_series_id = "99999999-9999-4999-8999-999999999999";
    auto projection = bootstrap_of(libor_id, "USD", "USD-LIBOR-3M", libor_series_id);
    projection.curve_family_role = "PROJECTION";
    projection.index_tenor = "3M";
    projection.discount_curve_config_id = uuid_of(usd_bootstrap_id);

    auto funding = usd_bootstrap();
    funding.curve_family_role = "FUNDING";

    auto input = usd_input();
    input.bootstrap_configs = {projection, funding};
    input.published_by_series[usd_series_id] = publish_of("2026-11-13 10:00:00+00");
    input.published_by_series[libor_series_id] = publish_of("2026-11-13 11:45:00+00");

    const auto rows = coverage(std::move(input));
    REQUIRE(rows.size() == 1);
    CHECK(rows.front().bootstrap_config_name == "USD-SOFR");
    CHECK(rows.front().gap == "stale_publish");
}

TEST_CASE("ir_coverage_reads_a_config_with_no_role_as_the_funding_curve", "[ir_coverage]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a bootstrap config with no role";

    const auto rows = coverage(usd_input());
    REQUIRE(rows.size() == 1);
    REQUIRE(rows.front().curves.size() == 1);
    CHECK(rows.front().curves.front().role == "FUNDING");
}
