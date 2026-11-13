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
#include "ores.synthetic.api/feeds/fx_vol_surface_feed.hpp"
#include "ores.analytics.quant/math/rng_stream.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include <algorithm>
#include <charconv>
#include <chrono>
#include <format>
#include <stdexcept>
#include <string>
#include <utility>

namespace ores::synthetic::feed {

using ores::analytics::quant::math::fx_market;
using ores::analytics::quant::math::fx_quote_convention;
using ores::analytics::quant::math::rng_stream;
using ores::analytics::quant::math::series_key;
using ores::analytics::quant::math::stream_key;
using ores::analytics::quant::service::fx_vol_slice_market;
using ores::analytics::quant::service::fx_vol_surface_dynamics;
using ores::analytics::quant::service::ssvi_parameters;
using ores::analytics::quant::service::tick_context;
using ores::analytics::quant::service::tick_scheduler;

namespace {

constexpr int days_in_a_year = 365;
constexpr int months_in_a_year = 12;

double year_fraction(std::string_view count, char unit) {
    int n = 0;
    const auto parsed = std::from_chars(count.data(), count.data() + count.size(), n);
    static_cast<void>(parsed);
    switch (unit) {
        case 'D':
            return static_cast<double>(n) / days_in_a_year;
        case 'W':
            return 7.0 * static_cast<double>(n) / days_in_a_year;
        case 'M':
            return static_cast<double>(n) / months_in_a_year;
        default:
            return static_cast<double>(n);
    }
}

/// The emitter a row describes, or the exception a feed cannot start without.
fx_vol_surface_emitter
surface_or_throw(const fx_vol_surface_feed_build_input& input,
                 std::uint64_t run_seed,
                 const std::string& source_name) {
    auto built = make_fx_vol_surface_emitter(input, run_seed);
    if (!built)
        throw std::invalid_argument("Feed " + source_name + " did not start: " + built.error());
    return std::move(*built);
}

}

std::optional<double> fx_vol_tenor_years(std::string_view tenor) {
    // A tenor is a count of one or two digits and one unit, as the column's check states.
    if (tenor.size() < 2 || tenor.size() > 3)
        return std::nullopt;
    const char unit = tenor.back();
    if (unit != 'D' && unit != 'W' && unit != 'M' && unit != 'Y')
        return std::nullopt;
    if (!std::all_of(tenor.begin(), tenor.end() - 1, [](char c) { return c >= '0' && c <= '9'; }))
        return std::nullopt;
    const double years = year_fraction(tenor.substr(0, tenor.size() - 1), unit);
    if (!(years > 0.0))
        return std::nullopt;
    return years;
}

std::expected<fx_vol_surface_emitter, std::string>
make_fx_vol_surface_emitter(const fx_vol_surface_feed_build_input& input,
                            std::uint64_t run_seed) {
    if (input.expiries.empty())
        return std::unexpected(std::string("A surface has no expiry."));
    if (input.contributors.empty())
        return std::unexpected(std::string("A surface needs at least one contributor."));
    if (input.deltas.empty())
        return std::unexpected(std::string("A surface quotes no delta besides ATM."));
    if (!input.contributor_profiles.empty() &&
        input.contributor_profiles.size() != input.contributors.size())
        return std::unexpected(
            std::format("The surface names {} contributor(s) and {} noise profile(s).",
                        input.contributors.size(),
                        input.contributor_profiles.size()));

    // The expiries in year fraction order, because a segment is the gap between two of
    // them and the generator reads them in order.
    std::vector<std::pair<double, const domain::fx_vol_surface_feed_expiry*>> ordered;
    ordered.reserve(input.expiries.size());
    for (const auto& expiry : input.expiries) {
        const auto years = fx_vol_tenor_years(expiry.tenor);
        if (!years)
            return std::unexpected(
                std::format("The tenor '{}' is not a count and a unit.", expiry.tenor));
        ordered.emplace_back(*years, &expiry);
    }
    std::ranges::sort(ordered, [](const auto& a, const auto& b) { return a.first < b.first; });

    const auto& config = input.config;
    ssvi_parameters start;
    start.rho = config.start_skew;
    start.eta = config.start_curvature;
    start.gamma = 0.5;
    fx_vol_surface_dynamics dynamics;
    dynamics.skew_reversion = config.skew_reversion;
    dynamics.skew_volatility = config.skew_volatility;
    dynamics.curvature_reversion = config.curvature_reversion;
    dynamics.curvature_volatility = config.curvature_volatility;

    double previous_years = 0.0;
    double previous_theta = 0.0;
    for (std::size_t i = 0; i < ordered.size(); ++i) {
        const auto& [years, expiry] = ordered[i];
        if (!(years > previous_years))
            return std::unexpected(
                std::format("Two expiries share the tenor '{}'.", expiry->tenor));
        const double theta = expiry->atm_vol * expiry->atm_vol * years;
        if (!(theta > previous_theta))
            return std::unexpected(std::format(
                "The ATM total variance of {} does not rise from the expiry before it, so its "
                "forward variance would not be positive.",
                expiry->tenor));
        start.expiries.push_back(
            {.expiry_id = expiry->tenor, .years = years, .atm_total_variance = theta});
        dynamics.variance_reversion.push_back(expiry->variance_reversion);
        dynamics.variance_volatility.push_back(expiry->variance_volatility);
        previous_years = years;
        previous_theta = theta;
    }

    // The latent market correlates the shocks, so the surface's own matrix is the identity.
    const auto size = static_cast<Eigen::Index>(ordered.size() + 3);
    dynamics.correlation = Eigen::MatrixXd::Identity(size, size);

    std::vector<fx_vol_slice_market> markets;
    markets.reserve(ordered.size());
    for (const auto& expiry : start.expiries)
        markets.push_back({.expiry_id = expiry.expiry_id,
                           .market = input.market,
                           .convention = input.convention});

    std::vector<fx_vol_contributor_spec> contributors;
    contributors.reserve(input.contributors.size());
    for (const auto& name : input.contributors) {
        const auto known = std::ranges::find(contributors, name,
                                             &fx_vol_contributor_spec::source_name);
        if (known != contributors.end())
            return std::unexpected(
                std::format("The contributor '{}' observes the surface twice.", name));
        contributors.push_back(
            {.source_name = name,
             .profile = input.contributor_profiles.empty()
                            ? ores::analytics::quant::service::fx_vol_noise_profile{}
                            : input.contributor_profiles[contributors.size()],
             .stream = rng_stream(stream_key{
                 .run_seed = run_seed,
                 .series = series_key(std::format("{}/{}", config.source_name, name)),
                 .purpose = ores::analytics::quant::math::draw_purpose::price})});
    }

    return fx_vol_surface_emitter::make(start,
                                        std::move(dynamics),
                                        1.0 / days_in_a_year,
                                        std::move(markets),
                                        input.deltas,
                                        std::move(contributors));
}

fx_vol_surface_feed::fx_vol_surface_feed(ores::nats::service::client& nats,
                                         std::string source_name,
                                         std::string nats_subject,
                                         const fx_vol_surface_feed_build_input& input,
                                         std::uint64_t run_seed,
                                         tick_scheduler& scheduler,
                                         double sensitivity,
                                         const ores::analytics::quant::domain::arrival_spec&
                                             arrival)
    : nats_(nats)
    , source_name_(std::move(source_name))
    , nats_subject_(std::move(nats_subject))
    , emitter_(surface_or_throw(input, run_seed, source_name_))
    , arrival_(arrival)
    , scheduler_(scheduler)
    , sensitivity_(sensitivity) {
    ticks_per_second_ = static_cast<double>(input.config.ticks_per_hour) / 3600.0;

    const auto& base = input.config.base_currency_code;
    const auto& quote = input.config.quote_currency_code;
    // The points repeat for every contributor, so one contributor's block names them all.
    for (std::size_t i = 0; i < emitter_.points_per_contributor(); ++i) {
        const auto& point = emitter_.points()[i];
        series_uris_.push_back(
            std::format("oresmd://fx/{}?type=quote&instrument=fx_option&quote=rate_lnvol&"
                        "ccy={}&expiry={}&strike_label={}",
                        base, quote, point.expiry_id, point.label));
    }
}

void fx_vol_surface_feed::start() {
    if (handle_)
        return;
    handle_ = scheduler_.add(
        {.series = series_key(source_name_),
         .arrival = arrival_,
         .sensitivity = sensitivity_,
         .name = source_name_,
         .group = "fx",
         .driver_count = static_cast<std::size_t>(emitter_.surface().expiries.size() + 2)},
        [this](const tick_context& context) { return tick(context); });
}

std::string fx_vol_surface_feed::tick(const tick_context& context) {
    // The normals come from the market, one for each forward variance and one for each
    // driver; with no market they are zeros and the surface steps nowhere.
    const auto points = emitter_.step(context.drivers);
    const auto& codec = ores::nats::default_wire_codec();
    const auto now = context.observed_at;
    std::size_t sent = 0;
    const auto per_contributor = emitter_.points_per_contributor();
    for (std::size_t c = 0; c < emitter_.contributor_count(); ++c) {
        const auto& who = emitter_.contributor_name(c);
        for (std::size_t i = 0; i < per_contributor; ++i) {
            const auto& point = points[c * per_contributor + i];
            if (!point.value)
                continue;
            ores::marketdata::messaging::market_tick shown;
            shown.oresmd_uri = series_uris_[i];
            shown.value = std::format("{}", *point.value);
            shown.observation_time = now;
            shown.source = source_name_ + "." + who;
            nats_.publish(nats_subject_, codec.encode(shown));
            ++sent;
        }
    }
    published_.fetch_add(sent, std::memory_order_relaxed);
    return std::format("{} point(s)", sent);
}

void fx_vol_surface_feed::stop() {
    if (handle_) {
        scheduler_.remove(*handle_);
        handle_.reset();
    }
}

}
