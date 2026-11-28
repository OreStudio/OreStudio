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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.analytics.quant/math/ir_vol_derivation.hpp"
#include "ores.platform/environment/environment.hpp"
#include <algorithm>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <cstdint>
#include <ctime>
#include <fstream>
#include <iostream>
#include <iterator>
#include <random>
#include <string>
#include <string_view>
#include <vector>

using namespace ores::analytics::quant::math;

namespace {

using std::chrono::January;
using std::chrono::sys_days;
using std::chrono::year_month_day;

/// A day the projection's histories are dated on, clear of the other suites' helpers.
year_month_day ip_observation_day(int day) {
    return year_month_day{sys_days{std::chrono::year{2026} / January / day}};
}

/// A rate that wanders from four percent, one observation a day, on a seed of its own.
std::vector<dated_level> ip_wandering_rate(int count, std::uint32_t seed) {
    std::mt19937 generator(seed);
    std::normal_distribution<double> step(0.0, 0.0002);
    std::vector<dated_level> out;
    out.reserve(static_cast<std::size_t>(count));
    double rate = 0.04;
    for (int i = 0; i < count; ++i) {
        rate = std::clamp(rate + step(generator), 0.005, 0.12);
        out.push_back({.date = ip_observation_day(i + 1), .level = rate});
    }
    return out;
}

short_rate_projection ip_reversion(double mean_reversion) {
    return {.model = short_rate_model::hull_white, .mean_reversion = mean_reversion};
}

std::vector<std::span<const dated_level>>
ip_histories(const std::vector<std::vector<dated_level>>& levels) {
    std::vector<std::span<const dated_level>> out;
    out.reserve(levels.size());
    for (const auto& history : levels)
        out.emplace_back(history);
    return out;
}

}

TEST_CASE("the projection carries a batch to swaptions, cell for cell like the day-at-a-time "
          "derivation",
          "[ir_vol_projection]") {
    const std::vector<double> ladder{0.25, 1.0};
    const std::vector<double> terms{1.0, 2.0, 10.0};
    const std::vector<std::vector<dated_level>> histories{
        ip_wandering_rate(400, 1), ip_wandering_rate(400, 2), ip_wandering_rate(400, 3)};

    const auto batch =
        measure_batch(ip_histories(histories), ladder, volatility_transform::difference, {});
    REQUIRE(batch);
    const auto projection = project_ir_vols(*batch, terms, ip_reversion(0.05));
    REQUIRE(projection);
    CHECK(projection->atms.size() == batch->states.size());

    for (std::size_t row = 0; row < histories.size(); ++row) {
        const auto matrix =
            derive_term_matrix(histories[row], terms[row], ladder, {}, ip_reversion(0.05));
        REQUIRE(matrix);
        const std::size_t first = batch->offset_of(row);
        for (std::size_t cell = 0; cell < ladder.size(); ++cell) {
            INFO("series " << row << " cell " << cell);
            REQUIRE(batch->states[first + cell] == vol_cell_state::quoted);
            REQUIRE(matrix->cells[cell].has_value());
            CHECK(projection->factors[first + cell] ==
                  Catch::Approx(matrix->cells[cell]->factor).epsilon(1e-13));
            CHECK(projection->atms[first + cell] ==
                  Catch::Approx(matrix->cells[cell]->atm).epsilon(1e-9));
            CHECK(projection->smile_skewnesses[first + cell] ==
                  Catch::Approx(matrix->cells[cell]->smile.skewness).epsilon(1e-9));
        }
    }
}

TEST_CASE("the projection answers nothing where the batch quoted nothing", "[ir_vol_projection]") {
    const std::vector<double> ladder{3.0, 0.25};
    const std::vector<double> terms{1.0};
    const std::vector<std::vector<dated_level>> histories{ip_wandering_rate(100, 10)};

    const auto batch =
        measure_batch(ip_histories(histories), ladder, volatility_transform::difference, {});
    REQUIRE(batch);
    REQUIRE(batch->states[0] == vol_cell_state::unreached);
    REQUIRE(batch->states[1] == vol_cell_state::quoted);

    const auto projection = project_ir_vols(*batch, terms, ip_reversion(0.05));
    REQUIRE(projection);
    CHECK(projection->factors[0] == 0.0);
    CHECK(projection->atms[0] == 0.0);
    CHECK(projection->smile_skewnesses[0] == 0.0);
    CHECK(projection->atms[1] > 0.0);
}

TEST_CASE("the projection refuses a model and a tenor it cannot read, and states each",
          "[ir_vol_projection]") {
    const std::vector<double> ladder{1.0};
    const std::vector<std::vector<dated_level>> histories{ip_wandering_rate(400, 20)};
    const auto batch =
        measure_batch(ip_histories(histories), ladder, volatility_transform::difference, {});
    REQUIRE(batch);

    const std::vector<double> terms{1.0};
    const auto refused = project_ir_vols(
        *batch, terms, {.model = short_rate_model::cox_ingersoll_ross, .mean_reversion = 0.05});
    REQUIRE_FALSE(refused);
    CHECK(refused.error().find("cox_ingersoll_ross") != std::string::npos);

    const auto negative = project_ir_vols(*batch, terms, ip_reversion(-0.01));
    REQUIRE_FALSE(negative);
    CHECK(negative.error() == "the mean reversion must be finite and not negative");

    const std::vector<double> no_tenor{};
    const auto missing = project_ir_vols(*batch, no_tenor, ip_reversion(0.05));
    REQUIRE_FALSE(missing);
    CHECK(missing.error() == "the batch holds 1 series and 0 tenor(s) were stated");

    const std::vector<double> zero_tenor{0.0};
    const auto zero = project_ir_vols(*batch, zero_tenor, ip_reversion(0.05));
    REQUIRE_FALSE(zero);
    CHECK(zero.error() == "the term of series 0 must be positive and finite");
}

namespace {

/**
 * The rate series a seed dataset carries, read from its curve quotes.
 *
 * A quote's text is the key the stored series is named by, so the corpus is the distinct
 * =IR_SWAP/RATE/= keys of the file: one key is one series, however many curves quote it. The file
 * is scanned rather than parsed, because the quotes are one field of a dataset this suite does not
 * own and a JSON dependency for one reading would be a dependency the arithmetic does not need.
 */
std::vector<std::string> ip_rate_series_keys(const std::string& path) {
    std::ifstream stream(path);
    REQUIRE(stream);
    const std::string text{std::istreambuf_iterator<char>(stream),
                           std::istreambuf_iterator<char>()};
    const std::string marker = R"("quote_text": ")";
    std::vector<std::string> keys;
    for (auto at = text.find(marker); at != std::string::npos; at = text.find(marker, at + 1)) {
        const auto first = at + marker.size();
        const auto last = text.find('"', first);
        if (last == std::string::npos)
            break;
        const std::string key = text.substr(first, last - first);
        if (key.starts_with("IR_SWAP/RATE/"))
            keys.push_back(key);
    }
    std::ranges::sort(keys);
    keys.erase(std::unique(keys.begin(), keys.end()), keys.end());
    return keys;
}

/// A tenor's length in years: =10Y= is ten, =3M= a quarter, and =1D= a day over a year.
double ip_tenor_years(std::string_view tenor) {
    if (tenor.size() < 2)
        return 0.0;
    const double count = std::stod(std::string(tenor.substr(0, tenor.size() - 1)));
    switch (tenor.back()) {
        case 'D':
            return count / 365.0;
        case 'M':
            return count / 12.0;
        case 'Y':
            return count;
        default:
            return 0.0;
    }
}

/// The currency a quote key names, its third field.
std::string_view ip_key_currency(const std::string& key) {
    const std::string_view view(key);
    const auto second = view.find('/', view.find('/') + 1);
    const auto third = view.find('/', second + 1);
    return view.substr(second + 1, third - second - 1);
}

}

/*
 * The cost of the pass over the corpus the seed configures, and not a synthetic one: the series
 * are the seed's rate quotes, and each carries a history of its own. Hidden from the suite;
 * run it with "[.benchmark]" and the dataset's path.
 */
TEST_CASE("the batch over every rate series the seed carries", "[.benchmark]") {
    const auto path = ores::platform::environment::environment::get_value("ORES_SEED_CURVE_QUOTES");
    if (!path)
        SKIP("set ORES_SEED_CURVE_QUOTES to the seed's curve_quotes.json");
    const auto keys = ip_rate_series_keys(*path);
    REQUIRE(keys.size() > 0);

    // The seed's own ladder, and a history long enough for the longest pillar's window: 8000
    // observations are 31 years of returns, and the thirty-year window asks for 7,560.
    const std::vector<double> ladder{
        1.0 / 12.0, 0.25, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 10.0, 15.0, 20.0, 30.0};
    constexpr int observations = 8000;

    std::vector<std::vector<dated_level>> histories;
    histories.reserve(keys.size());
    std::vector<double> terms;
    terms.reserve(keys.size());
    std::vector<std::string> currencies;
    for (std::size_t i = 0; i < keys.size(); ++i) {
        histories.push_back(ip_wandering_rate(observations, static_cast<std::uint32_t>(i + 1)));
        terms.push_back(ip_tenor_years(keys[i].substr(keys[i].rfind('/') + 1)));
        currencies.emplace_back(ip_key_currency(keys[i]));
    }
    std::ranges::sort(currencies);
    currencies.erase(std::unique(currencies.begin(), currencies.end()), currencies.end());
    const auto series = ip_histories(histories);

    // Processor time and not wall clock: the machine builds other worktrees while this runs, and
    // the answer to "what does the pass cost" is not a function of who else is compiling.
    constexpr int passes = 5;
    double best = 0.0;
    for (int pass = 0; pass < passes; ++pass) {
        const auto started = std::clock();
        const auto batch = measure_batch(series, ladder, volatility_transform::difference, {});
        const auto projection = project_ir_vols(*batch, terms, ip_reversion(0.05));
        const auto took = 1000.0 * static_cast<double>(std::clock() - started) / CLOCKS_PER_SEC;
        REQUIRE(batch);
        REQUIRE(projection);
        if (pass == 0)
            best = took;
        best = std::min(best, took);
    }

    // The same corpus one series at a time, which is the call the batch replaces: a vector per
    // cell, a window walked per cell, and the shape re-read for every series.
    double one_at_a_time = 0.0;
    for (int pass = 0; pass < passes; ++pass) {
        const auto started = std::clock();
        for (std::size_t i = 0; i < keys.size(); ++i) {
            const auto matrix =
                derive_term_matrix(histories[i], terms[i], ladder, {}, ip_reversion(0.05));
            REQUIRE(matrix);
        }
        const auto took = 1000.0 * static_cast<double>(std::clock() - started) / CLOCKS_PER_SEC;
        if (pass == 0)
            one_at_a_time = took;
        one_at_a_time = std::min(one_at_a_time, took);
    }

    const std::size_t cells = keys.size() * ladder.size();
    std::cout << "corpus: " << keys.size() << " series over " << currencies.size()
              << " currencies, " << ladder.size() << " expiries, " << observations
              << " observations each\n"
              << "cells: " << cells << ", batch best of " << passes << " passes: " << best
              << " ms, one series at a time: " << one_at_a_time << " ms\n";
}
