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
#include "ores.analytics.quant/math/fx_smile_quotes.hpp"
#include "ores.analytics.quant/math/root.hpp"
#include <algorithm>
#include <cmath>
#include <initializer_list>
#include <limits>
#include <utility>

namespace ores::analytics::quant::math {

namespace {

constexpr double nan = std::numeric_limits<double>::quiet_NaN();

/// How many times a bracket may double before the search gives up.
constexpr int bracket_doublings = 40;

/**
 * @brief A bracket of a sign change of g within doublings of x0, nearest first.
 *
 * Gives up on a value that is not finite, which is where the smile stops being positive.
 */
template <class G>
std::expected<std::pair<double, double>, std::string>
bracket_near(G&& g, double x0, double width, double floor = -std::numeric_limits<double>::max()) {
    const double g0 = g(x0);
    if (!std::isfinite(g0))
        return std::unexpected("the smile is not positive where the search starts");
    if (g0 == 0.0)
        return std::pair{x0, x0};
    double w = width;
    for (int i = 0; i < bracket_doublings; ++i, w *= 2.0) {
        const double lo = std::max(x0 - w, floor);
        const double glo = g(lo);
        const double hi = x0 + w;
        const double ghi = g(hi);
        if (std::isfinite(glo) && (glo > 0.0) != (g0 > 0.0))
            return std::pair{lo, x0};
        if (std::isfinite(ghi) && (ghi > 0.0) != (g0 > 0.0))
            return std::pair{x0, hi};
        if (!std::isfinite(glo) && !std::isfinite(ghi))
            break;
    }
    return std::unexpected("no sign change near the start; the smile is not positive there");
}

class smile_reader final {
public:
    smile_reader(const gram_charlier_smile& smile,
                 const fx_market& market,
                 const fx_quote_convention& convention)
        : smile_(smile)
        , market_(market)
        , convention_(convention)
        , forward_(market.forward())
        , root_years_(std::sqrt(smile.years)) {}

    [[nodiscard]] double forward() const noexcept {
        return forward_;
    }

    /// The smile's volatility at log(K / F), or NaN where it is not positive.
    [[nodiscard]] double volatility(double x) const noexcept {
        const double v = smile_.volatility_at_strike(forward_, forward_ * std::exp(x));
        return v > 0.0 ? v : nan;
    }

    [[nodiscard]] std::expected<double, std::string> atm() const {
        if (convention_.atm == fx_atm_type::forward)
            return 0.0;
        const double sign = premium_adjusted(convention_.delta) ? -1.0 : 1.0;
        const auto h = [&](double x) {
            const double s = volatility(x) * root_years_;
            return x - sign * 0.5 * s * s;
        };
        const double s0 = smile_.sigma * root_years_;
        const auto bracket = bracket_near(h, sign * 0.5 * s0 * s0, 0.1 * s0 + 1e-8);
        if (!bracket)
            return std::unexpected("the ATM strike: " + bracket.error());
        return solve(h, *bracket, "the ATM strike");
    }

    [[nodiscard]] std::expected<double, std::string>
    pillar(option_side side, double delta, double atm_volatility) const {
        const double target = side == option_side::call ? delta : -delta;
        const auto g = [&](double x) {
            const double s = volatility(x) * root_years_;
            return fx_delta(convention_.delta, side, market_, s, forward_ * std::exp(x)) - target;
        };
        const double s_atm = atm_volatility * root_years_;
        const auto flat = fx_strike_from_delta(convention_.delta, side, market_, s_atm, target);
        if (!flat)
            return std::unexpected("the pillar strike: " + flat.error());
        const auto bracket = bracket_near(g, std::log(*flat / forward_), 0.05 * s_atm);
        if (!bracket)
            return std::unexpected("the pillar strike: " + bracket.error());
        return solve(g, *bracket, "the pillar strike");
    }

    [[nodiscard]] std::expected<double, std::string>
    broker_butterfly(double delta, double atm_volatility, double smile_butterfly) const {
        const auto mismatch = [&](double b) {
            const double flat = atm_volatility + b;
            if (!(flat > 0.0))
                return nan;
            const double s = flat * root_years_;
            const auto kc =
                fx_strike_from_delta(convention_.delta, option_side::call, market_, s, delta);
            const auto kp =
                fx_strike_from_delta(convention_.delta, option_side::put, market_, s, -delta);
            if (!kc || !kp)
                return nan;
            const double sc = volatility(std::log(*kc / forward_)) * root_years_;
            const double sp = volatility(std::log(*kp / forward_)) * root_years_;
            const double broker = black_forward_price(option_side::call, forward_, *kc, s) +
                                  black_forward_price(option_side::put, forward_, *kp, s);
            const double on_smile = black_forward_price(option_side::call, forward_, *kc, sc) +
                                    black_forward_price(option_side::put, forward_, *kp, sp);
            return broker - on_smile;
        };
        const auto bracket = bracket_near(mismatch,
                                          smile_butterfly,
                                          std::max(1e-4, 0.1 * std::abs(smile_butterfly)),
                                          -atm_volatility * (1.0 - 1e-9));
        if (!bracket)
            return std::unexpected("the broker butterfly: " + bracket.error());
        return solve(mismatch, *bracket, "the broker butterfly");
    }

private:
    template <class G>
    static std::expected<double, std::string>
    solve(G&& g, std::pair<double, double> bracket, const char* what) {
        if (bracket.first == bracket.second)
            return bracket.first;
        const auto root = find_root(g, bracket.first, bracket.second, {.tolerance = 1e-14});
        if (!root)
            return std::unexpected(std::string(what) + ": " + root.error());
        return *root;
    }

    gram_charlier_smile smile_;
    fx_market market_;
    fx_quote_convention convention_;
    double forward_;
    double root_years_;
};

}

double gram_charlier_smile::volatility_at_d(double d) const noexcept {
    return sigma * (1.0 - skewness * d / 6.0 - excess_kurtosis * (1.0 - d * d) / 24.0);
}

double gram_charlier_smile::volatility_at_strike(double forward, double strike) const noexcept {
    const double s = sigma * std::sqrt(years);
    return volatility_at_d((std::log(forward / strike) + 0.5 * s * s) / s);
}

std::expected<fx_smile_quotes, std::string> quote_smile(const gram_charlier_smile& smile,
                                                        const fx_market& market,
                                                        const fx_quote_convention& convention) {
    if (!(smile.sigma > 0.0) || !(smile.years > 0.0) || !std::isfinite(smile.sigma) ||
        !std::isfinite(smile.years) || !std::isfinite(smile.skewness) ||
        !std::isfinite(smile.excess_kurtosis))
        return std::unexpected("the smile needs a positive, finite level and expiry and finite "
                               "moments");
    const smile_reader reader(smile, market, convention);
    const auto x_atm = reader.atm();
    if (!x_atm)
        return std::unexpected(x_atm.error());
    const double atm = reader.volatility(*x_atm);

    fx_smile_quotes out{.atm = atm, .rr25 = 0.0, .bf25 = 0.0, .rr10 = 0.0, .bf10 = 0.0};
    struct pillar final {
        double delta;
        double* risk_reversal;
        double* butterfly;
    };
    for (const auto& [delta, risk_reversal, butterfly] :
         {pillar{0.25, &out.rr25, &out.bf25}, pillar{0.10, &out.rr10, &out.bf10}}) {
        const auto xc = reader.pillar(option_side::call, delta, atm);
        if (!xc)
            return std::unexpected(xc.error());
        const auto xp = reader.pillar(option_side::put, delta, atm);
        if (!xp)
            return std::unexpected(xp.error());
        const double vc = reader.volatility(*xc);
        const double vp = reader.volatility(*xp);
        *risk_reversal = convention.favour == fx_risk_reversal_favour::call ? vc - vp : vp - vc;
        *butterfly = 0.5 * (vc + vp) - atm;
        if (convention.butterfly == fx_butterfly_style::broker) {
            const auto broker = reader.broker_butterfly(delta, atm, *butterfly);
            if (!broker)
                return std::unexpected(broker.error());
            *butterfly = *broker;
        }
    }
    return out;
}

}
