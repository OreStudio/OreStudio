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
#include "ores.analytics.quant/service/process_factory.hpp"
#include "ores.analytics.quant/service/processes/arithmetic_gaussian_mixture_model_process.hpp"
#include "ores.analytics.quant/service/processes/gaussian_mixture_model_process.hpp"
#include "ores.analytics.quant/service/processes/gjr_garch_variance_process.hpp"
#include "ores.analytics.quant/service/processes/ornstein_uhlenbeck_process.hpp"
#include "price_stream.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cmath>
#include <cstddef>
#include <limits>
#include <memory>
#include <stdexcept>
#include <vector>

using namespace ores::analytics::quant;
using service::daily_variance_frame;
using service::gjr_garch_variance_process;
using service::return_measure;

namespace {

/// An inner process that moves by a scripted log return per tick and records what it is told.
class scripted_process final : public domain::i_stochastic_process {
public:
    explicit scripted_process(std::vector<double> log_returns)
        : returns_(std::move(log_returns)) {}

    double next() override {
        return next(1.0);
    }
    double next(double elapsed) override {
        segments.push_back(elapsed);
        price_ *= std::exp(returns_[step_++ % returns_.size()] * elapsed);
        return price_;
    }
    bool supports_variable_step() const override {
        return true;
    }
    void set_tail_tilt(double tilt) override {
        tilts.push_back(tilt);
    }
    void set_volatility_multiplier(double m) override {
        multipliers.push_back(m);
    }
    bool supports_volatility_multiplier() const override {
        return true;
    }
    double current() const override {
        return price_;
    }

    std::vector<double> segments;
    std::vector<double> multipliers;
    std::vector<double> tilts;

private:
    std::vector<double> returns_;
    std::size_t step_ = 0;
    double price_ = 100.0;
};

math::gjr_garch_model
decorated_model_of(math::gjr_garch_shape shape, double lrv, double p_minus = 0.5) {
    const auto m = math::gjr_garch_model::make(shape, lrv, p_minus);
    REQUIRE(m.has_value());
    return *m;
}

std::vector<double> decorated_tick_returns(domain::i_stochastic_process& p, std::size_t n) {
    std::vector<double> out;
    out.reserve(n);
    double last = p.current();
    for (std::size_t i = 0; i < n; ++i) {
        const double now = p.next(1.0);
        out.push_back(std::log(now / last));
        last = now;
    }
    return out;
}

double decorated_mean(const std::vector<double>& xs) {
    double m = 0.0;
    for (const double x : xs)
        m += x;
    return m / static_cast<double>(xs.size());
}

double decorated_variance(const std::vector<double>& xs) {
    const double m = decorated_mean(xs);
    double v = 0.0;
    for (const double x : xs)
        v += (x - m) * (x - m);
    return v / static_cast<double>(xs.size());
}

/// The lag-one autocorrelation of the squares: the signature of volatility clustering.
double squared_autocorrelation(const std::vector<double>& xs) {
    std::vector<double> sq;
    sq.reserve(xs.size());
    for (const double x : xs)
        sq.push_back(x * x);
    const double m = decorated_mean(sq);
    double num = 0.0;
    double den = 0.0;
    for (std::size_t i = 0; i < sq.size(); ++i) {
        den += (sq[i] - m) * (sq[i] - m);
        if (i > 0)
            num += (sq[i] - m) * (sq[i - 1] - m);
    }
    return num / den;
}

}

TEST_CASE("a multiplier scales a mixture's variance and keeps its mean", "[gjr_garch_variance]") {
    const std::vector<double> means{0.0003, -0.0009};
    const std::vector<double> stdevs{0.001, 0.003};
    const std::vector<double> weights{0.75, 0.25};
    service::gaussian_mixture_model_process plain(means, stdevs, weights, 100.0, price_stream(4));
    service::gaussian_mixture_model_process scaled(means, stdevs, weights, 100.0, price_stream(4));
    CHECK(scaled.supports_volatility_multiplier());
    scaled.set_volatility_multiplier(2.0);
    const auto a = decorated_tick_returns(plain, 400'000);
    const auto b = decorated_tick_returns(scaled, 400'000);
    CHECK(decorated_variance(b) / decorated_variance(a) == Catch::Approx(4.0).epsilon(0.02));
    CHECK(std::abs(decorated_mean(b) - decorated_mean(a)) < 2e-5);
}

TEST_CASE("a multiplier of one draws exactly the path a mixture always drew",
          "[gjr_garch_variance]") {
    const std::vector<double> means{0.0003, -0.0009};
    const std::vector<double> stdevs{0.001, 0.003};
    const std::vector<double> weights{0.75, 0.25};
    service::gaussian_mixture_model_process untouched(
        means, stdevs, weights, 100.0, price_stream(9));
    service::gaussian_mixture_model_process one(means, stdevs, weights, 100.0, price_stream(9));
    one.set_volatility_multiplier(1.0);
    for (int i = 0; i < 1000; ++i)
        REQUIRE(one.next(0.7) == untouched.next(0.7));

    service::arithmetic_gaussian_mixture_model_process add_a(
        means, stdevs, weights, 1.0, price_stream(9));
    service::arithmetic_gaussian_mixture_model_process add_b(
        means, stdevs, weights, 1.0, price_stream(9));
    add_b.set_volatility_multiplier(1.0);
    for (int i = 0; i < 1000; ++i)
        REQUIRE(add_b.next() == add_a.next());
}

TEST_CASE("a process that cannot scale refuses a multiplier but accepts one",
          "[gjr_garch_variance]") {
    service::ornstein_uhlenbeck_process ou(1.0, 1.0, 0.1, 1.0, price_stream(1));
    CHECK_FALSE(ou.supports_volatility_multiplier());
    CHECK_NOTHROW(ou.set_volatility_multiplier(1.0));
    CHECK_THROWS_AS(ou.set_volatility_multiplier(1.5), std::invalid_argument);

    service::gaussian_mixture_model_process gmm({0.0}, {0.001}, {1.0}, 1.0, price_stream(1));
    CHECK_THROWS_AS(gmm.set_volatility_multiplier(0.0), std::invalid_argument);
    CHECK_THROWS_AS(gmm.set_volatility_multiplier(NAN), std::invalid_argument);
}

TEST_CASE("the variance updates at the end of each day from the day's return less the drift",
          "[gjr_garch_variance]") {
    const auto model = decorated_model_of({.alpha = 0.1, .gamma = 0.2, .beta = 0.6}, 1e-4);
    const daily_variance_frame frame{.ticks_per_day = 4.0,
                                     .inner_daily_variance = 1e-4,
                                     .inner_daily_drift = 0.0004,
                                     .measure = return_measure::log_return};
    auto inner = std::make_unique<scripted_process>(std::vector<double>{-0.002, 0.001});
    auto* script = inner.get();
    gjr_garch_variance_process p(std::move(inner), model, frame, 2e-4);

    REQUIRE(script->multipliers.size() == 1);
    CHECK(script->multipliers[0] == Catch::Approx(std::sqrt(2.0)));
    for (int i = 0; i < 4; ++i)
        p.next(1.0);
    // The day's log return is -0.002 + 0.001 - 0.002 + 0.001; the shock is that less the drift.
    const double shock = -0.002 - 0.0004;
    const double expected = model.omega() + (0.1 + 0.2) * shock * shock + 0.6 * 2e-4;
    CHECK(p.daily_variance() == Catch::Approx(expected).epsilon(1e-12));
    REQUIRE(script->multipliers.size() == 2);
    CHECK(script->multipliers[1] == Catch::Approx(std::sqrt(p.daily_variance() / 1e-4)));
}

TEST_CASE("a day that closes at a price no log return reads fails loudly", "[gjr_garch_variance]") {
    const auto model = decorated_model_of({.alpha = 0.1, .gamma = 0.0, .beta = 0.8}, 1e-4);
    const daily_variance_frame frame{.ticks_per_day = 2.0,
                                     .inner_daily_variance = 1e-4,
                                     .inner_daily_drift = 0.0,
                                     .measure = return_measure::log_return};
    auto inner = std::make_unique<scripted_process>(
        std::vector<double>{0.001, -std::numeric_limits<double>::infinity()});
    gjr_garch_variance_process p(std::move(inner), model, frame, 1e-4);
    p.next(1.0);
    CHECK_THROWS_AS(p.next(1.0), std::runtime_error);
}

TEST_CASE("a step that crosses the end of a day is split there", "[gjr_garch_variance]") {
    const auto model = decorated_model_of({.alpha = 0.05, .gamma = 0.0, .beta = 0.9}, 1e-4);
    const daily_variance_frame frame{.ticks_per_day = 2.0,
                                     .inner_daily_variance = 1e-4,
                                     .inner_daily_drift = 0.0,
                                     .measure = return_measure::log_return};
    auto inner = std::make_unique<scripted_process>(std::vector<double>{0.001});
    auto* script = inner.get();
    gjr_garch_variance_process p(std::move(inner), model, frame, 1e-4);
    p.next(1.5);
    p.next(2.0);
    // The second step crosses the day's end after half a tick, so it is cut there.
    REQUIRE(script->segments.size() == 3);
    CHECK(script->segments[0] == Catch::Approx(1.5));
    CHECK(script->segments[1] == Catch::Approx(0.5));
    CHECK(script->segments[2] == Catch::Approx(1.5));
    // One roll at 2.0 ticks; the next falls at 4.0, which is still pending.
    CHECK(script->multipliers.size() == 2);
}

TEST_CASE("the tilt and the step bound pass through to the inner process", "[gjr_garch_variance]") {
    const auto model = decorated_model_of({.alpha = 0.05, .gamma = 0.0, .beta = 0.9}, 1e-4);
    auto inner = std::make_unique<scripted_process>(std::vector<double>{0.0});
    auto* script = inner.get();
    gjr_garch_variance_process p(
        std::move(inner), model, {1.0, 1e-4, 0.0, return_measure::log_return}, 1e-4);
    p.set_tail_tilt(0.4);
    REQUIRE(script->tilts.size() == 1);
    CHECK(script->tilts[0] == 0.4);
    CHECK(p.supports_variable_step());
    CHECK(std::isinf(p.max_step()));
}

TEST_CASE("a stressed start decays to the long-run variance", "[gjr_garch_variance]") {
    // With no reaction and no carry-over the variance is omega after the first day, so the
    // first day carries four times the long-run variance and every later day the long-run.
    constexpr std::size_t paths = 4000;
    constexpr double ticks_per_day = 8.0;
    std::vector<double> first;
    std::vector<double> second;
    for (std::size_t s = 0; s < paths; ++s) {
        auto p = service::process_factory::make_gjr_garch_process(
            "geometric",
            {0.0},
            {0.001},
            {1.0},
            100.0,
            price_stream(100 + s),
            {.alpha = 0.0, .gamma = 0.0, .beta = 0.0},
            ticks_per_day,
            4.0);
        const double p0 = p->current();
        p->next(ticks_per_day);
        const double p1 = p->current();
        p->next(ticks_per_day);
        first.push_back(std::log(p1 / p0));
        second.push_back(std::log(p->current() / p1));
    }
    const double long_run = 0.001 * 0.001 * ticks_per_day;
    CHECK(decorated_variance(first) / long_run == Catch::Approx(4.0).epsilon(0.08));
    CHECK(decorated_variance(second) / long_run == Catch::Approx(1.0).epsilon(0.08));
}

TEST_CASE("a persistent variance makes the daily moves cluster, and a mixture alone does not",
          "[gjr_garch_variance]") {
    constexpr double ticks_per_day = 4.0;
    constexpr std::size_t days = 20'000;
    const auto daily = [&](domain::i_stochastic_process& p) {
        std::vector<double> out;
        out.reserve(days);
        for (std::size_t d = 0; d < days; ++d) {
            const double before = p.current();
            p.next(ticks_per_day);
            out.push_back(std::log(p.current() / before));
        }
        return out;
    };
    auto garch = service::process_factory::make_gjr_garch_process(
        "geometric",
        {0.0},
        {0.002},
        {1.0},
        100.0,
        price_stream(77),
        {.alpha = 0.10, .gamma = 0.05, .beta = 0.85},
        ticks_per_day);
    service::gaussian_mixture_model_process plain({0.0}, {0.002}, {1.0}, 100.0, price_stream(77));
    CHECK(squared_autocorrelation(daily(*garch)) > 0.1);
    CHECK(std::abs(squared_autocorrelation(daily(plain))) < 0.03);
}

TEST_CASE("a GJR-GARCH path is a pure function of its seed", "[gjr_garch_variance]") {
    const auto make = [] {
        return service::process_factory::make_gjr_garch_process(
            "geometric",
            {0.0001, -0.0004},
            {0.001, 0.003},
            {0.8, 0.2},
            1.1,
            price_stream(5),
            {.alpha = 0.05, .gamma = 0.05, .beta = 0.9},
            24.0);
    };
    auto a = make();
    auto b = make();
    for (int i = 0; i < 500; ++i)
        REQUIRE(a->next(0.8) == b->next(0.8));
}

TEST_CASE("the factory makes the mixture's daily law the GARCH long-run level",
          "[gjr_garch_variance]") {
    auto p =
        service::process_factory::make_gjr_garch_process("arithmetic",
                                                         {0.0002, -0.0006},
                                                         {0.001, 0.003},
                                                         {3.0, 1.0},
                                                         1.0,
                                                         price_stream(2),
                                                         {.alpha = 0.05, .gamma = 0.0, .beta = 0.9},
                                                         10.0,
                                                         1.0);
    const auto* g = dynamic_cast<const gjr_garch_variance_process*>(p.get());
    REQUIRE(g != nullptr);
    // Weights 3:1 normalise to 0.75 and 0.25.
    const double mean = 0.75 * 0.0002 + 0.25 * -0.0006;
    const double var = 0.75 * (1e-6 + (0.0002 - mean) * (0.0002 - mean)) +
                       0.25 * (9e-6 + (-0.0006 - mean) * (-0.0006 - mean));
    CHECK(g->model().long_run_variance() == Catch::Approx(var * 10.0).epsilon(1e-12));
    CHECK(g->daily_variance() == Catch::Approx(var * 10.0).epsilon(1e-12));
    const math::mixture normalised{
        .means = {0.0002, -0.0006}, .stdevs = {0.001, 0.003}, .weights = {0.75, 0.25}};
    CHECK(g->model().p_minus() ==
          Catch::Approx(math::daily_negative_second_moment(normalised, 10.0)).epsilon(1e-12));
    CHECK(g->model().p_minus() < math::negative_second_moment(normalised));
}

TEST_CASE("a skewed mixture with a leverage effect holds its long-run daily variance",
          "[gjr_garch_variance]") {
    constexpr double ticks_per_day = 10.0;
    constexpr std::size_t days = 60'000;
    auto p = service::process_factory::make_gjr_garch_process(
        "arithmetic",
        {0.0004, -0.0016},
        {0.001, 0.003},
        {0.8, 0.2},
        1.0,
        price_stream(31),
        {.alpha = 0.03, .gamma = 0.15, .beta = 0.85},
        ticks_per_day,
        1.0);
    std::vector<double> moves;
    moves.reserve(days);
    for (std::size_t d = 0; d < days; ++d) {
        const double before = p->current();
        p->next(ticks_per_day);
        moves.push_back(p->current() - before);
    }
    const double long_run = 3.24e-6 * ticks_per_day;
    CHECK(decorated_variance(moves) / long_run == Catch::Approx(1.0).epsilon(0.1));
}

TEST_CASE("the factory and the decorator refuse what they cannot honour", "[gjr_garch_variance]") {
    const math::gjr_garch_shape ok{.alpha = 0.05, .gamma = 0.0, .beta = 0.9};
    CHECK_THROWS_AS(service::process_factory::make_gjr_garch_process(
                        "ornstein_uhlenbeck", {0.0}, {0.1}, {1.0}, 1.0, price_stream(1), ok, 24.0),
                    std::invalid_argument);
    CHECK_THROWS_AS(
        service::process_factory::make_gjr_garch_process("geometric",
                                                         {0.0},
                                                         {0.001},
                                                         {1.0},
                                                         1.0,
                                                         price_stream(1),
                                                         {.alpha = 0.2, .gamma = 0.0, .beta = 0.9},
                                                         24.0),
        std::invalid_argument);
    CHECK_THROWS_AS(service::process_factory::make_gjr_garch_process(
                        "geometric", {0.0}, {0.001}, {1.0}, 1.0, price_stream(1), ok, 0.0),
                    std::invalid_argument);

    const auto model = decorated_model_of(ok, 1e-4);
    const daily_variance_frame frame{1.0, 1e-4, 0.0, return_measure::log_return};
    CHECK_THROWS_AS(gjr_garch_variance_process(nullptr, model, frame, 1e-4), std::invalid_argument);
    CHECK_THROWS_AS(
        gjr_garch_variance_process(std::make_unique<service::ornstein_uhlenbeck_process>(
                                       1.0, 1.0, 0.1, 1.0, price_stream(1)),
                                   model,
                                   frame,
                                   1e-4),
        std::invalid_argument);
    CHECK_THROWS_AS(
        gjr_garch_variance_process(
            std::make_unique<scripted_process>(std::vector<double>{0.0}), model, frame, 0.0),
        std::invalid_argument);
}

TEST_CASE("the per-tick cost of the GJR-GARCH decorator against the plain mixture",
          "[.benchmark][gjr_garch_variance]") {
    constexpr int ticks = 5'000'000;
    const auto time = [&](domain::i_stochastic_process& p) {
        const auto start = std::chrono::steady_clock::now();
        double sink = 0.0;
        for (int i = 0; i < ticks; ++i)
            sink += p.next(1.0);
        const auto ns =
            std::chrono::duration<double, std::nano>(std::chrono::steady_clock::now() - start)
                .count();
        CHECK(std::isfinite(sink));
        return ns / ticks;
    };
    service::gaussian_mixture_model_process plain(
        {0.0001, -0.0004}, {0.001, 0.003}, {0.8, 0.2}, 1.1, price_stream(8));
    auto garch = service::process_factory::make_gjr_garch_process(
        "geometric",
        {0.0001, -0.0004},
        {0.001, 0.003},
        {0.8, 0.2},
        1.1,
        price_stream(8),
        {.alpha = 0.05, .gamma = 0.05, .beta = 0.9},
        24.0 * 3600.0);
    const double plain_ns = time(plain);
    const double garch_ns = time(*garch);
    WARN("plain mixture: " << plain_ns << " ns/tick; with GJR-GARCH: " << garch_ns << " ns/tick");
}

TEST_CASE("a GJR-GARCH process hands the market's normal to the inner law",
          "[gjr_garch_variance]") {
    const auto model = decorated_model_of({.alpha = 0.1, .gamma = 0.2, .beta = 0.6}, 1e-4);
    const daily_variance_frame frame{.ticks_per_day = 8.0,
                                     .inner_daily_variance = 1e-4,
                                     .inner_daily_drift = 0.0,
                                     .measure = return_measure::log_return};
    auto inner = std::make_unique<service::gaussian_mixture_model_process>(
        std::vector<double>{0.0}, std::vector<double>{0.01}, std::vector<double>{1.0}, 100.0,
        price_stream(17));
    service::gaussian_mixture_model_process plain(
        std::vector<double>{0.0}, std::vector<double>{0.01}, std::vector<double>{1.0}, 100.0,
        price_stream(17));
    gjr_garch_variance_process gjr(std::move(inner), model, frame, 1e-4);
    CHECK(gjr.supports_external_normal());
    // The initial variance is the inner law's own, so the day's multiplier is one
    // and the driven path is the inner's own driven path, bit for bit.
    const std::vector<double> normals{0.5, -1.0, 0.0, 1.5, -0.25};
    for (const double z : normals)
        CHECK(gjr.next_with_normal(z) == plain.next_with_normal(z));
}

TEST_CASE("a driven GJR-GARCH day closes on the tick that completes it",
          "[gjr_garch_variance]") {
    const auto model = decorated_model_of({.alpha = 0.1, .gamma = 0.2, .beta = 0.6}, 1e-4);
    const daily_variance_frame frame{.ticks_per_day = 2.0,
                                     .inner_daily_variance = 1e-4,
                                     .inner_daily_drift = 0.0,
                                     .measure = return_measure::log_return};
    auto inner = std::make_unique<service::gaussian_mixture_model_process>(
        std::vector<double>{0.0}, std::vector<double>{0.01}, std::vector<double>{1.0}, 100.0,
        price_stream(21));
    gjr_garch_variance_process gjr(std::move(inner), model, frame, 1e-4);
    const double open = gjr.daily_variance();
    gjr.next_with_normal(3.0);
    CHECK(gjr.daily_variance() == open);
    gjr.next_with_normal(3.0);
    CHECK(gjr.daily_variance() > open);
}
