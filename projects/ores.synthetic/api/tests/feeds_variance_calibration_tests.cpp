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
#include "ores.analytics.quant/math/gjr_garch.hpp"
#include "ores.synthetic.api/feeds/fixing_calibration.hpp"
#include "ores.synthetic.api/feeds/variance_calibration.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cmath>
#include <format>
#include <numbers>
#include <random>
#include <string>
#include <vector>

namespace {

const std::string variance_calibration_tags("[feeds][variance_calibration]");

using Catch::Approx;
using namespace ores::synthetic::feed;

std::chrono::year_month_day variance_day(int offset) {
    using namespace std::chrono;
    return year_month_day{sys_days{2020y / January / 1} + days{offset}};
}

std::vector<std::chrono::year_month_day> variance_days(std::size_t n) {
    std::vector<std::chrono::year_month_day> out;
    for (std::size_t i = 0; i < n; ++i)
        out.push_back(variance_day(static_cast<int>(i)));
    return out;
}

/// Fixings of a pair whose daily log returns follow a GJR-GARCH(1,1) with normal shocks.
std::vector<std::string> gjr_garch_fixings(
    double alpha, double gamma, double beta, double long_run, std::size_t n, unsigned seed) {
    std::mt19937 rng(seed);
    std::normal_distribution<double> z(0.0, 1.0);
    const double omega = long_run * (1.0 - alpha - gamma / 2.0 - beta);
    double variance = long_run;
    double level = 1.1;
    std::vector<std::string> out{std::format("{:.10f}", level)};
    for (std::size_t i = 1; i < n; ++i) {
        const double shock = std::sqrt(variance) * z(rng);
        level *= std::exp(shock);
        out.push_back(std::format("{:.10f}", level));
        variance = omega + (alpha + (shock < 0.0 ? gamma : 0.0)) * shock * shock + beta * variance;
    }
    return out;
}

}

TEST_CASE("every date of the window is a point and a bad value is kept with its reason",
          variance_calibration_tags) {
    const std::vector<std::string> texts{"abc", "1.10", "-1", "1.21", "n/a", "1.331"};
    const auto points = capture_variance_points(variance_days(texts.size()), texts, false);
    REQUIRE(points.size() == texts.size());
    CHECK_FALSE(points[0].used);
    CHECK(points[0].exclusion_reason == "the value is not a number");
    CHECK_FALSE(points[0].value);
    CHECK_FALSE(points[1].used);
    CHECK(points[1].exclusion_reason == "the first readable point has no return");
    CHECK(points[2].exclusion_reason == "the value is not positive");
    CHECK(points[3].used);
    CHECK(points[3].exclusion_reason.empty());
    // The return skips the value that could not be used, so it runs from 1.10 to 1.21.
    CHECK(*points[3].log_return == Approx(std::log(1.1)).epsilon(1e-12));
    CHECK_FALSE(points[4].used);
    CHECK(points[5].used);
    CHECK(*points[5].log_return == Approx(std::log(1.1)).epsilon(1e-12));
    CHECK(points[5].date == variance_day(5));
}

TEST_CASE("an inverted item's returns are the pair's in market convention",
          variance_calibration_tags) {
    const std::vector<std::string> texts{"1.0", "1.1"};
    const auto direct = capture_variance_points(variance_days(2), texts, false);
    const auto inverted = capture_variance_points(variance_days(2), texts, true);
    CHECK(*direct[1].log_return == Approx(std::log(1.1)).epsilon(1e-12));
    CHECK(*inverted[1].log_return == Approx(-std::log(1.1)).epsilon(1e-12));
    CHECK(inverted[1].text == "1.1");
    CHECK(*inverted[1].value == Approx(1.1));
}

TEST_CASE("an input too short to fit is rejected with its reason and no shape",
          variance_calibration_tags) {
    const auto texts = gjr_garch_fixings(0.05, 0.04, 0.9, 4e-5, 100, 3);
    const auto points = capture_variance_points(variance_days(texts.size()), texts, false);
    const auto fit = fit_variance(points, 0.5);
    CHECK_FALSE(fit.fitted);
    CHECK(fit.returns_used == 99);
    CHECK(fit.reason == "the input holds 99 daily returns and a fit needs " +
                            std::to_string(minimum_garch_returns));
    CHECK_FALSE(fit.alpha);
    CHECK_FALSE(fit.persistence);
    CHECK_FALSE(fit.log_likelihood);
}

TEST_CASE("a fit recovers the shape that drew the fixings and states its evidence",
          variance_calibration_tags) {
    const auto texts = gjr_garch_fixings(0.05, 0.08, 0.86, 4e-5, 8'000, 17);
    const auto points = capture_variance_points(variance_days(texts.size()), texts, false);
    const double p_minus = 0.5;
    const auto fit = fit_variance(points, p_minus);
    REQUIRE(fit.fitted);
    CHECK(fit.reason.empty());
    CHECK(fit.returns_used == 7'999);
    CHECK(std::abs(*fit.alpha - 0.05) < 0.03);
    CHECK(std::abs(*fit.gamma - 0.08) < 0.04);
    CHECK(std::abs(*fit.beta - 0.86) < 0.04);
    // The persistence a calibration stores is the one its parameters make under p_minus.
    CHECK(*fit.persistence == Approx(*fit.alpha + *fit.gamma * p_minus + *fit.beta).epsilon(1e-12));
    CHECK(*fit.persistence < 1.0);
    CHECK(*fit.long_run_variance > 0.0);
    CHECK(*fit.starting_variance_ratio > 0.0);
    CHECK(std::isfinite(*fit.log_likelihood));
    CHECK(fit.starts > 0);
    CHECK(fit.starts_agreed);
}

TEST_CASE("the same input always gives the same fit", variance_calibration_tags) {
    const auto texts = gjr_garch_fixings(0.04, 0.05, 0.9, 4e-5, 1'500, 29);
    const auto points = capture_variance_points(variance_days(texts.size()), texts, false);
    const auto first = fit_variance(points, 0.5);
    const auto second = fit_variance(points, 0.5);
    REQUIRE(first.fitted);
    CHECK(*first.alpha == *second.alpha);
    CHECK(*first.gamma == *second.gamma);
    CHECK(*first.beta == *second.beta);
    CHECK(*first.log_likelihood == *second.log_likelihood);
}

TEST_CASE("an inverted item fits the asymmetry with the pair's sign", variance_calibration_tags) {
    // The fixings are drawn with falls exciting the variance; read inverted, those falls are
    // the pair's rises, so the pair's own gamma must not keep the item's sign.
    const auto texts = gjr_garch_fixings(0.03, 0.12, 0.84, 4e-5, 8'000, 41);
    const auto direct =
        fit_variance(capture_variance_points(variance_days(texts.size()), texts, false), 0.5);
    const auto inverted =
        fit_variance(capture_variance_points(variance_days(texts.size()), texts, true), 0.5);
    REQUIRE(direct.fitted);
    REQUIRE(inverted.fitted);
    CHECK(*direct.gamma > 0.05);
    CHECK(*inverted.gamma < *direct.gamma);
}

TEST_CASE("the likelihood a fit states is the Gaussian one of its returns at its shape",
          variance_calibration_tags) {
    namespace qm = ores::analytics::quant::math;
    const auto texts = gjr_garch_fixings(0.05, 0.06, 0.88, 4e-5, 1'000, 53);
    const auto points = capture_variance_points(variance_days(texts.size()), texts, false);
    const double p_minus = 0.5;
    const auto fit = fit_variance(points, p_minus);
    REQUIRE(fit.fitted);

    std::vector<double> returns;
    for (const auto& p : points)
        if (p.used)
            returns.push_back(*p.log_return);
    double mean = 0.0;
    for (const double r : returns)
        mean += r;
    mean /= static_cast<double>(returns.size());
    std::vector<double> shocks;
    for (const double r : returns)
        shocks.push_back(r - mean);
    const auto model =
        qm::gjr_garch_model::make({.alpha = *fit.alpha, .gamma = *fit.gamma, .beta = *fit.beta},
                                  *fit.long_run_variance,
                                  p_minus);
    REQUIRE(model.has_value());
    const auto variances = qm::filter(*model, shocks, *fit.long_run_variance);
    double expected = 0.0;
    for (std::size_t t = 0; t < shocks.size(); ++t)
        expected -= 0.5 * (std::log(2.0 * std::numbers::pi) + std::log(variances[t]) +
                           shocks[t] * shocks[t] / variances[t]);
    CHECK(*fit.log_likelihood == Approx(expected).epsilon(1e-9));
    // The first day's variance is the filter's forecast after the last return.
    CHECK(*fit.starting_variance_ratio ==
          Approx(variances.back() / *fit.long_run_variance).epsilon(1e-12));
}

namespace {

ores::synthetic::domain::fx_spot_generation_config eurusd_feed() {
    ores::synthetic::domain::fx_spot_generation_config cfg;
    cfg.base_currency_code = "EUR";
    cfg.quote_currency_code = "USD";
    cfg.ticks_per_hour = 3600;
    cfg.variance_model = "gjr_garch";
    return cfg;
}

std::vector<ores::synthetic::domain::gmm_component> one_component() {
    ores::synthetic::domain::gmm_component c;
    c.mean = 0.0;
    c.stdev = 0.0005;
    c.weight = 1.0;
    return {c};
}

ores::synthetic::domain::variance_calibration approved_fit() {
    ores::synthetic::domain::variance_calibration c;
    c.version = 2;
    c.base_currency_code = "EUR";
    c.quote_currency_code = "USD";
    c.method = "gjr_garch_qmle";
    c.alpha = 0.011;
    c.gamma = 0.023;
    c.beta = 0.932;
    c.starting_variance_ratio = 0.92;
    c.returns_used = 279;
    c.state = "approved";
    c.state_reason = "Starts agree.";
    return c;
}

}

TEST_CASE("a feed runs the shape of the approved calibration of its pair, and names it",
          variance_calibration_tags) {
    const auto run = calibrated_variance(eurusd_feed(), approved_fit(), one_component());
    REQUIRE(run.has_value());
    CHECK(run->shape.alpha == 0.011);
    CHECK(run->shape.gamma == 0.023);
    CHECK(run->shape.beta == 0.932);
    CHECK(run->record.calibration_version == 2);
    CHECK(run->record.fitted);
    CHECK(run->record.returns == 279);
    CHECK(run->record.initial_ratio == 0.92);
    // A single normal component is symmetric, so a day's p_minus is one half.
    CHECK(run->record.persistence == Approx(0.011 + 0.932 + 0.023 * 0.5).epsilon(1e-9));
    CHECK(run->record.note == "Fitted from 279 daily returns and approved: Starts agree.");
}

TEST_CASE("a feed refuses to start on no calibration, another pair's, an unapproved or a "
          "shapeless one",
          variance_calibration_tags) {
    const auto feed = eurusd_feed();
    const auto mixture = one_component();
    const auto none = calibrated_variance(feed, std::nullopt, mixture);
    REQUIRE_FALSE(none.has_value());
    CHECK(none.error().find("names no variance calibration") != std::string::npos);

    auto other = approved_fit();
    other.base_currency_code = "GBP";
    const auto wrong_pair = calibrated_variance(feed, other, mixture);
    REQUIRE_FALSE(wrong_pair.has_value());
    CHECK(wrong_pair.error().find("is of GBP/USD, not of EUR/USD") != std::string::npos);

    for (const std::string state : {"proposed", "rejected", "superseded"}) {
        auto undecided = approved_fit();
        undecided.state = state;
        undecided.state_reason = "the reviewer said so";
        const auto refused = calibrated_variance(feed, undecided, mixture);
        REQUIRE_FALSE(refused.has_value());
        CHECK(refused.error().find("is " + state + " (the reviewer said so), not approved") !=
              std::string::npos);
    }

    auto shapeless = approved_fit();
    shapeless.beta = std::nullopt;
    const auto empty = calibrated_variance(feed, shapeless, mixture);
    REQUIRE_FALSE(empty.has_value());
    CHECK(empty.error().find("has no shape") != std::string::npos);
}
