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
#include "ores.analytics.quant/math/gjr_garch.hpp"
#include "price_stream.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <cstddef>
#include <limits>
#include <random>
#include <vector>

using namespace ores::analytics::quant::math;

namespace {

gjr_garch_model
garch_model_of(gjr_garch_shape shape, double long_run_variance, double p_minus = 0.5) {
    const auto made = gjr_garch_model::make(shape, long_run_variance, p_minus);
    REQUIRE(made.has_value());
    return *made;
}

/// A path of shocks drawn with normal innovations, the variance starting at its long-run
/// level. The recursion is written out here rather than taken from the model, so a defect in
/// the model's recursion cannot also draw the data the fit is checked against.
std::vector<double>
simulate_garch_shocks(const gjr_garch_model& model, std::size_t n, std::uint64_t seed) {
    const auto [alpha, gamma, beta] = model.shape();
    const double omega = model.long_run_variance() * (1.0 - alpha - beta - gamma * model.p_minus());
    auto rng = price_stream(seed);
    std::normal_distribution<double> normal(0.0, 1.0);
    std::vector<double> shocks;
    shocks.reserve(n);
    double variance = model.long_run_variance();
    for (std::size_t i = 0; i < n; ++i) {
        const double shock = std::sqrt(variance) * normal(rng);
        shocks.push_back(shock);
        const double reaction = shock < 0.0 ? alpha + gamma : alpha;
        variance = omega + reaction * shock * shock + beta * variance;
    }
    return shocks;
}

/// QuantLib's Garch11::costFunction (ql/models/volatility/garch.hpp), ported as written,
/// as an expectation independent of the code under test.
double quantlib_cost(const std::vector<double>& series, double alpha, double beta, double omega) {
    double retval = 0.0;
    double u2 = 0.0;
    double sigma2 = 0.0;
    std::size_t n = 0;
    for (const double x : series) {
        sigma2 = omega + alpha * u2 + beta * sigma2;
        u2 = x * x;
        retval += std::log(sigma2) + u2 / sigma2;
        ++n;
    }
    return retval / (2.0 * static_cast<double>(n));
}

}

TEST_CASE("the filter reproduces QuantLib's GARCH calculation test", "[gjr_garch]") {
    // QuantLib test-suite/garch.cpp, testCalculation: Garch11(0.2, 0.3, 0.4) on ten
    // returns of 0.1, the variance starting at the first return squared.
    const auto model = garch_model_of({.alpha = 0.2, .gamma = 0.0, .beta = 0.3}, 0.4);
    CHECK(model.omega() == Catch::Approx(0.2).epsilon(1e-15));
    const std::vector<double> returns(10, 0.1);
    const auto variances = filter(model, returns, 0.1 * 0.1);
    const std::vector<double> expected = {0.452769,
                                          0.513323,
                                          0.530141,
                                          0.5350841,
                                          0.536558,
                                          0.536999,
                                          0.537132,
                                          0.537171,
                                          0.537183,
                                          0.537187};
    REQUIRE(variances.size() == returns.size() + 1);
    for (std::size_t i = 0; i < expected.size(); ++i)
        CHECK(std::abs(std::sqrt(variances[i + 1]) - expected[i]) < 1e-6);
}

TEST_CASE("the cost is QuantLib's Garch11 cost when gamma is zero", "[gjr_garch]") {
    const auto model = garch_model_of({.alpha = 0.07, .gamma = 0.0, .beta = 0.9}, 1.5e-4);
    const auto series = simulate_garch_shocks(model, 2000, 7);
    CHECK(mean_cost(model, series, model.omega()) ==
          Catch::Approx(quantlib_cost(series, 0.07, 0.9, model.omega())).epsilon(1e-13));
}

TEST_CASE("a fall raises the variance by alpha plus gamma and a rise by alpha alone",
          "[gjr_garch]") {
    const auto model = garch_model_of({.alpha = 0.05, .gamma = 0.10, .beta = 0.80}, 1e-4);
    const double v = 2e-4;
    CHECK(model.next_variance(v, -0.01) == Catch::Approx(model.omega() + 0.15e-4 + 0.8 * v));
    CHECK(model.next_variance(v, 0.01) == Catch::Approx(model.omega() + 0.05e-4 + 0.8 * v));

    const auto reversed = garch_model_of({.alpha = 0.15, .gamma = -0.10, .beta = 0.80}, 1e-4);
    CHECK(reversed.next_variance(v, 0.01) > reversed.next_variance(v, -0.01));
}

TEST_CASE("omega follows from the long-run variance and the persistence", "[gjr_garch]") {
    const auto model = garch_model_of({.alpha = 0.05, .gamma = 0.08, .beta = 0.85}, 2e-4, 0.45);
    CHECK(model.persistence() == Catch::Approx(0.05 + 0.85 + 0.08 * 0.45));
    CHECK(model.omega() == Catch::Approx(2e-4 * (1.0 - model.persistence())));

    // The long-run variance is the fixed point of the expected recursion:
    // E[sigma2'] = omega + persistence * sigma2.
    const double v = model.long_run_variance();
    CHECK(model.omega() + model.persistence() * v == Catch::Approx(v));
}

TEST_CASE("a model outside the admissible region is refused, with the reason", "[gjr_garch]") {
    const auto refused = [](gjr_garch_shape s, double lrv, double p) {
        return !gjr_garch_model::make(s, lrv, p).has_value();
    };
    CHECK(refused({.alpha = -0.01, .gamma = 0.0, .beta = 0.9}, 1e-4, 0.5));
    CHECK(refused({.alpha = 0.05, .gamma = 0.0, .beta = -0.1}, 1e-4, 0.5));
    CHECK(refused({.alpha = 0.05, .gamma = -0.06, .beta = 0.9}, 1e-4, 0.5));
    CHECK(refused({.alpha = 0.10, .gamma = 0.0, .beta = 0.90}, 1e-4, 0.5));
    CHECK(refused({.alpha = 0.05, .gamma = 0.0, .beta = 0.9}, 0.0, 0.5));
    CHECK(refused({.alpha = 0.05, .gamma = 0.0, .beta = 0.9}, 1e-4, 1.5));
    CHECK(refused({.alpha = NAN, .gamma = 0.0, .beta = 0.9}, 1e-4, 0.5));

    // The boundary alpha + gamma = 0 is allowed: a fall then leaves the variance unmoved.
    CHECK_FALSE(refused({.alpha = 0.05, .gamma = -0.05, .beta = 0.9}, 1e-4, 0.5));
    const auto message =
        gjr_garch_model::make({.alpha = 0.1, .gamma = 0.0, .beta = 0.9}, 1e-4, 0.5);
    REQUIRE_FALSE(message.has_value());
    CHECK(message.error().find("persistence") != std::string::npos);
}

TEST_CASE("admissible agrees with make on every shape of a grid across the region's edges",
          "[gjr_garch]") {
    const std::vector<double> alphas{-0.01, 0.0, 0.05, 0.2, 1.1};
    const std::vector<double> gammas{-0.3, -0.05, 0.0, 0.1, 0.4};
    const std::vector<double> betas{-0.01, 0.0, 0.7, 0.9, 0.99};
    const std::vector<double> p_minuses{-0.1, 0.0, 0.5, 0.62, 1.0, 1.1};
    int accepted = 0;
    for (const double a : alphas)
        for (const double g : gammas)
            for (const double b : betas)
                for (const double p : p_minuses) {
                    const gjr_garch_shape shape{.alpha = a, .gamma = g, .beta = b};
                    const bool made = gjr_garch_model::make(shape, 1e-4, p).has_value();
                    CHECK(gjr_garch_model::admissible(shape, p) == made);
                    accepted += made ? 1 : 0;
                }
    CHECK(accepted > 0);
    CHECK_FALSE(gjr_garch_model::admissible(
        {.alpha = std::numeric_limits<double>::quiet_NaN(), .gamma = 0.0, .beta = 0.9}, 0.5));
}

TEST_CASE("p_minus is one half for any symmetric mixture", "[gjr_garch]") {
    CHECK(negative_second_moment({.means = {0.0}, .stdevs = {0.01}, .weights = {1.0}}) ==
          Catch::Approx(0.5).epsilon(1e-12));
    CHECK(negative_second_moment(
              {.means = {-0.002, 0.002}, .stdevs = {0.001, 0.001}, .weights = {0.5, 0.5}}) ==
          Catch::Approx(0.5).epsilon(1e-12));
    CHECK(negative_second_moment(
              {.means = {0.0, 0.0}, .stdevs = {0.001, 0.004}, .weights = {0.8, 0.2}}) ==
          Catch::Approx(0.5).epsilon(1e-12));
}

TEST_CASE("p_minus of a skewed mixture matches its sample estimate", "[gjr_garch]") {
    const mixture skewed{
        .means = {0.0004, -0.0016}, .stdevs = {0.001, 0.003}, .weights = {0.8, 0.2}};
    const double closed = negative_second_moment(skewed);
    CHECK(closed > 0.5);

    const auto [mean, variance] = moments_of(skewed);
    auto rng = price_stream(11);
    std::discrete_distribution<int> pick(skewed.weights.begin(), skewed.weights.end());
    double total = 0.0;
    constexpr int n = 1'000'000;
    for (int i = 0; i < n; ++i) {
        const int k = pick(rng);
        const double x = std::normal_distribution<double>(skewed.means[k], skewed.stdevs[k])(rng);
        const double z = (x - mean) / std::sqrt(variance);
        if (z < 0.0)
            total += z * z;
    }
    CHECK(std::abs(total / n - closed) < 3e-3);
}

TEST_CASE("p_minus of a day matches a sample of standardised daily sums", "[long_running][gjr_garch]") {
    const mixture skewed{
        .means = {0.0004, -0.0016}, .stdevs = {0.001, 0.003}, .weights = {0.8, 0.2}};
    constexpr int ticks = 50;
    const double daily = daily_negative_second_moment(skewed, ticks);
    CHECK(daily > 0.5);
    CHECK(daily < negative_second_moment(skewed) - 0.05);

    const auto [mean, variance] = moments_of(skewed);
    const double scale = std::sqrt(variance * ticks);
    auto rng = price_stream(12);
    std::discrete_distribution<int> pick(skewed.weights.begin(), skewed.weights.end());
    double total = 0.0;
    constexpr int days = 200'000;
    for (int d = 0; d < days; ++d) {
        double sum = 0.0;
        for (int t = 0; t < ticks; ++t) {
            const int k = pick(rng);
            sum += std::normal_distribution<double>(skewed.means[k], skewed.stdevs[k])(rng) - mean;
        }
        const double z = sum / scale;
        if (z < 0.0)
            total += z * z;
    }
    CHECK(std::abs(total / days - daily) < 5e-3);
}

TEST_CASE("p_minus of a day is one tick's for a one-tick day and one half for a long one",
          "[gjr_garch]") {
    const mixture skewed{
        .means = {0.0004, -0.0016}, .stdevs = {0.001, 0.003}, .weights = {0.8, 0.2}};
    CHECK(daily_negative_second_moment(skewed, 1.0) == negative_second_moment(skewed));
    CHECK(daily_negative_second_moment(skewed, 86'400.0) == Catch::Approx(0.5).margin(1e-3));
    const mixture symmetric{
        .means = {-0.002, 0.002}, .stdevs = {0.001, 0.001}, .weights = {0.5, 0.5}};
    CHECK(daily_negative_second_moment(symmetric, 24.0) == Catch::Approx(0.5).epsilon(1e-12));
}

TEST_CASE("a component with no spread contributes its square only below the mean", "[gjr_garch]") {
    const mixture point_mass{.means = {-0.001, 0.001}, .stdevs = {0.0, 0.0}, .weights = {0.5, 0.5}};
    CHECK(negative_second_moment(point_mass) == Catch::Approx(0.5).epsilon(1e-12));
}

TEST_CASE("the fit recovers a GARCH(1,1) as QuantLib's calibration test does", "[gjr_garch]") {
    // QuantLib simulates Garch11(0.2, 0.3, 0.4) for 50000 steps and calibrates back to
    // alpha 0.2076, beta 0.2820. The same experiment with our own random numbers.
    const auto truth = garch_model_of({.alpha = 0.2, .gamma = 0.0, .beta = 0.3}, 0.4);
    const auto series = simulate_garch_shocks(truth, 50'000, 48);
    const auto fit = fit_gjr_garch(series, 0.5);
    REQUIRE(fit.has_value());
    CHECK(std::abs(fit->model.shape().alpha - 0.2) < 0.02);
    CHECK(std::abs(fit->model.shape().beta - 0.3) < 0.03);
    // gamma is estimated from the falls alone, about half the data, so its error is about
    // twice alpha's.
    CHECK(std::abs(fit->model.shape().gamma) < 0.04);
}

TEST_CASE("the fit recovers a persistent GJR-GARCH with a leverage effect", "[gjr_garch]") {
    const auto truth = garch_model_of({.alpha = 0.05, .gamma = 0.08, .beta = 0.88}, 1e-4);
    const auto series = simulate_garch_shocks(truth, 20'000, 21);
    const auto fit = fit_gjr_garch(series, 0.5);
    REQUIRE(fit.has_value());
    CHECK(std::abs(fit->model.shape().alpha - 0.05) < 0.02);
    CHECK(std::abs(fit->model.shape().gamma - 0.08) < 0.03);
    CHECK(std::abs(fit->model.shape().beta - 0.88) < 0.02);
    CHECK(fit->model.persistence() < 1.0);
    // The fitted cost is no worse than the truth's on the same series.
    CHECK(fit->cost <= mean_cost(truth, series, fit->model.long_run_variance()) + 1e-6);
}

TEST_CASE("every start of the fit reaches the same optimum", "[gjr_garch]") {
    // QuantLib checks that each of its initial guesses calibrates to one answer.
    const auto truth = garch_model_of({.alpha = 0.06, .gamma = 0.04, .beta = 0.90}, 1e-4);
    const auto series = simulate_garch_shocks(truth, 10'000, 5);
    const auto all = fit_gjr_garch(series, 0.5);
    REQUIRE(all.has_value());
    for (const auto& start : default_fit_starts()) {
        const std::vector<gjr_garch_shape> one{start};
        const auto fit = fit_gjr_garch(series, 0.5, one);
        REQUIRE(fit.has_value());
        CHECK(fit->cost == Catch::Approx(all->cost).epsilon(1e-9));
        CHECK(std::abs(fit->model.shape().alpha - all->model.shape().alpha) < 2e-3);
        CHECK(std::abs(fit->model.shape().gamma - all->model.shape().gamma) < 2e-3);
        CHECK(std::abs(fit->model.shape().beta - all->model.shape().beta) < 2e-3);
    }
}

TEST_CASE("a well identified fit reports every start tried and agreeing", "[gjr_garch]") {
    const auto truth = garch_model_of({.alpha = 0.06, .gamma = 0.04, .beta = 0.90}, 1e-4);
    const auto series = simulate_garch_shocks(truth, 10'000, 5);
    const auto fit = fit_gjr_garch(series, 0.5);
    REQUIRE(fit.has_value());
    CHECK(fit->starts_tried == static_cast<int>(default_fit_starts().size()));
    CHECK(fit->starts_agreed);
}

TEST_CASE("starts that end in different basins do not agree", "[gjr_garch]") {
    // Thirty returns of near white noise leave the likelihood flat, so the starts settle in
    // different places, which the fit must report rather than hide behind the best one.
    const auto truth = garch_model_of({.alpha = 0.01, .gamma = 0.0, .beta = 0.0}, 1e-4);
    const auto series = simulate_garch_shocks(truth, 30, 1);
    const auto fit = fit_gjr_garch(series, 0.5);
    REQUIRE(fit.has_value());
    CHECK_FALSE(fit->starts_agreed);
    CHECK(fit->starts_tried == static_cast<int>(default_fit_starts().size()));
}

TEST_CASE("the fit targets the sample variance of the demeaned returns", "[gjr_garch]") {
    const auto truth = garch_model_of({.alpha = 0.05, .gamma = 0.0, .beta = 0.9}, 1e-4);
    auto series = simulate_garch_shocks(truth, 5'000, 3);
    for (auto& r : series)
        r += 0.001;
    double mean = 0.0;
    for (const double r : series)
        mean += r;
    mean /= static_cast<double>(series.size());
    double variance = 0.0;
    for (const double r : series)
        variance += (r - mean) * (r - mean);
    variance /= static_cast<double>(series.size());
    const auto fit = fit_gjr_garch(series, 0.5);
    REQUIRE(fit.has_value());
    CHECK(fit->model.long_run_variance() == Catch::Approx(variance).epsilon(1e-12));
}

TEST_CASE("a series too short or with no variance is refused", "[gjr_garch]") {
    CHECK_FALSE(fit_gjr_garch(std::vector<double>{0.01, -0.01}, 0.5).has_value());
    CHECK_FALSE(fit_gjr_garch(std::vector<double>(100, 0.002), 0.5).has_value());
    CHECK_FALSE(
        fit_gjr_garch(std::vector<double>{0.01, -0.01, 0.02}, 0.5, std::vector<gjr_garch_shape>{})
            .has_value());
}
