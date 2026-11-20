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
#include "ores.analytics.quant/math/fx_pair_market.hpp"
#include "ores.analytics.quant/math/rng_stream.hpp"
#include "ores.logging/logger.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.api/messaging/adapter_operation_protocol.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.refdata.core/mapping/fx_option_convention_mapping.hpp"
#include <algorithm>
#include <chrono>
#include <format>
#include <map>
#include <memory>
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
using ores::analytics::quant::service::fx_vol_tenor_years;
using ores::analytics::quant::service::fx_vol_surface_dynamics;
using ores::analytics::quant::service::ssvi_expiry_fit;
using ores::analytics::quant::service::ssvi_parameters;
using ores::analytics::quant::service::tick_context;
using ores::analytics::quant::service::run_market;

namespace {

constexpr int days_in_a_year = 365;

/// The emitter a row describes, or the exception a feed cannot start without.
fx_vol_surface_emitter surface_or_throw(const fx_vol_surface_feed_build_input& input,
                                        std::uint64_t run_seed,
                                        const std::string& source_name) {
    auto built = make_fx_vol_surface_emitter(input, run_seed);
    if (!built)
        throw std::invalid_argument("Feed " + source_name + " did not start: " + built.error());
    return std::move(*built);
}

}

std::expected<fx_vol_surface_emitter, std::string>
make_fx_vol_surface_emitter(const fx_vol_surface_feed_build_input& input, std::uint64_t run_seed) {
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
    std::vector<fx_quote_convention> conventions;
    conventions.reserve(ordered.size());
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
        const auto convention =
            ores::refdata::mapping::parse_tenor_text(expiry->tenor)
                .and_then([&](ores::analytics::quant::domain::date_period period) {
                    return ores::refdata::mapping::to_quote_convention(input.convention, period);
                });
        if (!convention)
            return std::unexpected(
                std::format("The convention at {}: {}", expiry->tenor, convention.error()));
        conventions.push_back(*convention);
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

    std::vector<fx_vol_contributor_spec> contributors;
    contributors.reserve(input.contributors.size());
    for (const auto& name : input.contributors) {
        const auto known =
            std::ranges::find(contributors, name, &fx_vol_contributor_spec::source_name);
        if (known != contributors.end())
            return std::unexpected(
                std::format("The contributor '{}' observes the surface twice.", name));
        contributors.push_back(
            {.source_name = name,
             .profile = input.contributor_profiles.empty() ?
                            ores::analytics::quant::service::fx_vol_noise_profile{} :
                            input.contributor_profiles[contributors.size()],
             .stream = rng_stream(
                 stream_key{.run_seed = run_seed,
                            .series = series_key(std::format("{}/{}", config.source_name, name)),
                            .purpose = ores::analytics::quant::math::draw_purpose::price})});
    }

    return fx_vol_surface_emitter::make(start,
                                        std::move(dynamics),
                                        1.0 / days_in_a_year,
                                        std::move(conventions),
                                        input.deltas,
                                        std::move(contributors));
}

std::string fx_pair_member_code(std::string_view base, std::string_view quote) {
    return std::string(base) + "/" + std::string(quote);
}

std::expected<fx_vol_market_inputs, std::string>
fx_vol_board_inputs(const ores::analytics::quant::domain::market_board& values,
                    ores::analytics::quant::domain::market_board::member_id base_curve,
                    ores::analytics::quant::domain::market_board::member_id quote_curve,
                    ores::analytics::quant::domain::market_board::member_id spot) {
    // The base currency first, then the quote, then the pair's spot: the order a reader of the
    // run would look in, so the member a stopped curve left behind is the one named.
    const auto base = values.view(base_curve);
    if (!base.published)
        return std::unexpected(std::format("curve:{}", base.code));
    const auto quote = values.view(quote_curve);
    if (!quote.published)
        return std::unexpected(std::format("curve:{}", quote.code));
    const auto level = values.view(spot);
    if (!level.published)
        return std::unexpected(std::format("spot:{}", level.code));
    return fx_vol_market_inputs{.spot = fx_market{.spot = level.spot},
                                .base = base.points,
                                .quote = quote.points,
                                .spot_date = base.spot_date};
}

std::expected<void, std::string>
fx_vol_expiry_markets(const fx_vol_market_inputs& inputs,
                      std::span<const ores::analytics::quant::domain::date_period> periods,
                      const ores::analytics::quant::service::fx_expiry_date_rule& rule,
                      std::vector<ores::analytics::quant::math::fx_market>& out) {
    // A rule with no calendar dates nothing: the arithmetic refuses rather than treat an unread
    // calendar as one closed only at weekends. A surface built that way is named here, because
    // the exception's own message says more about indexes than about the pair.
    if (rule.calendar_count() == 0)
        return std::unexpected(std::string("the pair's expiry date rule names no calendar"));
    out.clear();
    out.reserve(periods.size());
    for (const auto& period : periods) {
        // The date the ladder publishes for this tenor, because both call the same rule.
        const auto value_date = rule.expiry(inputs.spot_date, period);
        auto resolved = ores::analytics::quant::math::pair_market_at(
            inputs.spot.spot, inputs.spot_date, inputs.base, inputs.quote, value_date, {});
        if (!resolved)
            return std::unexpected(resolved.error());
        out.push_back(*resolved);
    }
    return {};
}

fx_vol_surface_feed::fx_vol_surface_feed(
    ores::nats::service::client& nats,
    std::string source_name,
    std::string nats_subject,
    const fx_vol_surface_feed_build_input& input,
    std::uint64_t run_seed,
    run_market& market,
    double sensitivity,
    const ores::analytics::quant::domain::arrival_spec& arrival)
    : expiry_rule_(input.expiry_rule)
    , nats_(nats)
    , source_name_(std::move(source_name))
    , nats_subject_(std::move(nats_subject))
    , emitter_(surface_or_throw(input, run_seed, source_name_))
    , arrival_(arrival)
    , market_(market)
    , sensitivity_(sensitivity) {
    ticks_per_second_ = static_cast<double>(input.config.ticks_per_hour) / 3600.0;

    const auto& base = input.config.base_currency_code;
    const auto& quote = input.config.quote_currency_code;
    // The three values this surface reads, resolved once, off the loop. The pair's spot is named
    // by the one function its feed publishes it under, so the two agree by construction.
    {
        auto& values = market_.values();
        base_curve_id_ =
            values.member_of(ores::analytics::quant::domain::market_member_kind::curve, base);
        quote_curve_id_ =
            values.member_of(ores::analytics::quant::domain::market_member_kind::curve, quote);
        spot_id_ = values.member_of(ores::analytics::quant::domain::market_member_kind::spot,
                                    fx_pair_member_code(base, quote));
    }
    // The expiry each market is solved at, in the order the emitter quotes them: the emitter
    // sorted the expiries, so the periods are taken from its order rather than sorted again,
    // which two equal-year tenors could order differently.
    {
        std::map<std::string, ores::analytics::quant::domain::date_period> periods;
        for (const auto& expiry : input.expiries) {
            const auto period = ores::refdata::mapping::parse_tenor_text(expiry.tenor);
            if (!period)
                throw std::invalid_argument("fx_vol_surface_feed: the expiry '" + expiry.tenor +
                                            "' does not read as a tenor");
            periods[expiry.tenor] = *period;
        }
        for (const auto& expiry : emitter_.surface().expiries) {
            const auto found = periods.find(expiry.expiry_id);
            if (found == periods.end())
                throw std::invalid_argument("fx_vol_surface_feed: the emitter quotes an expiry '" +
                                            expiry.expiry_id + "' the row does not carry");
            expiry_periods_.push_back(found->second);
        }
        markets_.reserve(expiry_periods_.size());
    }
    // The points repeat for every contributor, so one contributor's block names them all.
    for (std::size_t i = 0; i < emitter_.points_per_contributor(); ++i) {
        const auto& point = emitter_.points()[i];
        series_uris_.push_back(
            std::format("oresmd://fx/{}?type=quote&instrument=fx_option&quote=rate_lnvol&"
                        "ccy={}&expiry={}&strike_label={}",
                        base,
                        quote,
                        point.expiry_id,
                        point.label));
    }
}

void fx_vol_surface_feed::start() {
    if (handle_)
        return;
    handle_ = market_.loop().add(
        {.series = series_key(source_name_),
         .arrival = arrival_,
         .sensitivity = sensitivity_,
         .name = source_name_,
         .group = "fx",
         .driver_count = static_cast<std::size_t>(emitter_.surface().expiries.size() + 2)},
        [this](const tick_context& context) { return tick(context); });
}

std::string fx_vol_surface_feed::tick(const tick_context& context) {
    // The normals come from the market, one for each forward variance and one for each driver.
    // A surface the market does not drive publishes nothing: its block was never in the market
    // the run built, and stepping it on nothing would move it on noise nobody chose.
    const auto wanted = static_cast<std::size_t>(emitter_.surface().expiries.size() + 2);
    if (context.drivers.size() != wanted)
        return std::format("no market: {} of {} driver(s)", context.drivers.size(), wanted);
    // The market of each expiry, from the three values this surface reads: the pair's spot
    // and its two currencies' curves. A curve moves when its feed ticks, so a strike moves
    // with it and no restart is needed.
    const auto inputs =
        fx_vol_board_inputs(market_.values(), *base_curve_id_, *quote_curve_id_, *spot_id_);
    if (!inputs)
        return "no market: " + inputs.error();
    if (const auto built = fx_vol_expiry_markets(*inputs, expiry_periods_, expiry_rule_, markets_);
        !built)
        return "no market: " + built.error();
    const auto points = emitter_.step(context.drivers, markets_);
    const auto& codec = ores::nats::default_wire_codec();
    const auto now = context.observed_at;
    std::size_t sent = 0;
    const auto per_contributor = emitter_.points_per_contributor();
    std::shared_ptr<const sink_map> sinks;
    {
        const std::lock_guard lock(sinks_mu_);
        sinks = sinks_;
    }
    for (std::size_t c = 0; c < emitter_.contributor_count(); ++c) {
        const auto& who = emitter_.contributor_name(c);
        // A contributor no adapter serves, such as the clean one no tenant subscribes to, still
        // records the surface it observed as market ticks, while the vendors quote it.
        const bool served = std::ranges::any_of(
            *sinks, [&who](const auto& entry) { return entry.second.contributor == who; });
        for (std::size_t i = 0; i < per_contributor; ++i) {
            const auto& point = points[c * per_contributor + i];
            if (!point.value)
                continue;
            if (!served) {
                ores::marketdata::messaging::market_tick shown;
                shown.oresmd_uri = series_uris_[i];
                shown.value = std::format("{}", *point.value);
                shown.observation_time = now;
                shown.source = source_name_ + "." + who;
                nats_.publish(nats_subject_, codec.encode(shown));
                ++sent;
                continue;
            }
            // Each adapter that serves this contributor gets the point under the vendor's id, as
            // an adapter of a live vendor sends it; the service finds the parties from the id.
            const auto key = std::pair{emitter_.points()[i].expiry_id, emitter_.points()[i].label};
            for (const auto& [adapter, sink] : *sinks) {
                if (sink.contributor != who)
                    continue;
                const auto target = sink.targets.find(key);
                if (target == sink.targets.end())
                    continue;
                ores::marketdata::messaging::adapter_tick tick;
                tick.id_scheme = target->second.id.id_scheme;
                tick.external_id = target->second.id.external_id;
                tick.field = target->second.id.field;
                tick.value = std::format("{}", *point.value);
                tick.vendor_time = now;
                tick.sequence = published_.load(std::memory_order_relaxed) + sent;
                nats_.publish(target->second.subject, codec.encode(tick));
                ++sent;
            }
        }
    }
    published_.fetch_add(sent, std::memory_order_relaxed);
    return std::format("{} point(s)", sent);
}

void fx_vol_surface_feed::attach(const std::string& adapter_id,
                                 const std::string& contributor,
                                 point_targets targets) {
    const std::lock_guard lock(sinks_mu_);
    auto next = std::make_shared<sink_map>(*sinks_);
    next->insert_or_assign(adapter_id,
                           sink{.contributor = contributor, .targets = std::move(targets)});
    sinks_ = std::move(next);
}

std::size_t fx_vol_surface_feed::detach(const std::string& adapter_id) {
    const std::lock_guard lock(sinks_mu_);
    auto next = std::make_shared<sink_map>(*sinks_);
    next->erase(adapter_id);
    sinks_ = std::move(next);
    return sinks_->size();
}

void fx_vol_surface_feed::stop() {
    if (handle_) {
        market_.loop().remove(*handle_);
        handle_.reset();
    }
}

}
