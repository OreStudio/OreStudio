/* -*- mode: c++; tab-width: 4; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
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
#include "ores.marketdata.service/app/curve_analytics_reader.hpp"
#include "../curve_pillar_reader.hpp"
#include "../curve_republish_resolver.hpp"
#include "ores.analytics.quant/service/day_count_calculator.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/repository/curve_publish_repository.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.service/app/curve_republish_service.hpp"
#include "ores.marketdata.service/app/staleness_policy.hpp"
#include "ores.refdata.api/domain/ir_curve_bootstrap_config.hpp"
#include "ores.refdata.api/domain/ir_curve_bootstrap_pillar.hpp"
#include "ores.refdata.core/mapping/quant_codes.hpp"
#include "ores.refdata.core/repository/ir_curve_bootstrap_config_repository.hpp"
#include "ores.refdata.core/repository/ir_curve_bootstrap_pillar_repository.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cmath>
#include <map>
#include <string>
#include <vector>

namespace ores::marketdata::service::app {

namespace {

namespace mm = ores::marketdata::messaging;
using namespace ores::logging;

auto& curve_analytics_lg() {
    static auto instance = make_logger("ores.marketdata.service.curve_analytics_reader");
    return instance;
}

}

mm::get_curve_analytics_response
get_curve_analytics(const ores::database::context& ctx,
                    const mm::get_curve_analytics_request& request) {
    mm::get_curve_analytics_response resp;
    resp.instrument_id = request.instrument_id;

    const auto now = std::chrono::system_clock::now();
    const auto as_of =
        request.as_of == std::chrono::system_clock::time_point{} ? now : request.as_of;
    resp.as_of = as_of;

    try {
        const auto config_id = boost::uuids::string_generator{}(request.instrument_id);
        const auto configs =
            ores::refdata::repository::ir_curve_bootstrap_config_repository{}.read_latest(
                ctx, boost::uuids::to_string(config_id));
        if (configs.empty())
            throw std::invalid_argument("No curve configuration named " + request.instrument_id +
                                        ".");
        const auto& config = configs.front();

        // The curve is one party's, and the request may only narrow the read to
        // a party the session already holds.
        const auto party = boost::uuids::to_string(config.party_id);
        const auto session_party = ctx.party_id();
        if (session_party && *session_party != config.party_id)
            throw std::invalid_argument("Curve " + config.name + " belongs to another party.");
        if (!request.party_id.empty() && request.party_id != party)
            throw std::invalid_argument("Curve " + config.name + " belongs to another party.");

        resp.name = config.name;

        auto pillars =
            ores::refdata::repository::ir_curve_bootstrap_pillar_repository{}.read_latest(ctx);
        std::erase_if(pillars, [&](const auto& p) { return p.bootstrap_config_id != config.id; });
        if (pillars.empty())
            throw std::invalid_argument("Curve " + config.name + " has no pillars.");
        std::ranges::sort(
            pillars, {}, &ores::refdata::domain::ir_curve_bootstrap_pillar::sequence_index);

        const auto horizon = std::chrono::floor<std::chrono::days>(as_of);
        const auto refctx = build_refdata_context(
            ctx, std::chrono::year_month_day{horizon}, config.tenor_convention_code);
        const auto raw = read_pillar_rates(ctx, pillars, refctx, as_of);
        const auto bootstrap_pillars =
            resolve_bootstrap_pillars(pillars, refctx, raw.rates_by_point_id);
        const auto points = curve_republish_service::compute(ctx, config.id, as_of);

        const auto day_count =
            ores::refdata::mapping::parse_day_count_convention_code(config.day_count_convention);
        const auto start = std::chrono::year_month_day{horizon};

        resp.conventions.index_name = config.name;
        resp.conventions.day_count = config.day_count_convention;
        resp.conventions.compounding = "continuous";
        resp.conventions.interpolation = config.interpolation_method;
        resp.conventions.pillar_count = static_cast<int>(pillars.size());

        repository::market_series_repository series_repo;
        std::optional<double> previous_zero;
        std::optional<double> previous_tau;
        // The newest instant any pillar's series was observed at: that is when the
        // market behind this curve was last written, and what its verdict measures.
        std::chrono::system_clock::time_point newest_instant{};
        for (std::size_t i = 0; i < pillars.size() && i < points.size(); ++i) {
            const auto& pillar = pillars[i];
            const auto& boot = bootstrap_pillars[i];
            const auto tau = ores::analytics::quant::service::day_count_calculator::year_fraction(
                start, boot.end_date, day_count);
            const auto df = points[i].discount_factor;
            if (df <= 0.0)
                continue;
            const auto zero = -std::log(df) / tau;

            mm::curve_market_pillar market;
            market.tenor = pillar.end_tenor_code;
            market.maturity_date = boot.end_date;
            market.year_fraction = tau;
            const auto rate = raw.rates_by_point_id.find(pillar.end_tenor_code);
            market.rate = rate == raw.rates_by_point_id.end() ? 0.0 : rate->second;
            market.series_id = boost::uuids::to_string(pillar.market_series_id);
            const auto series = series_repo.read_latest(ctx, market.series_id);
            if (!series.empty())
                market.provider_code = series.front().provider_code;
            for (const auto& observation :
                 repository::market_observation_repository{}.read_latest_for_series(
                     ctx, pillar.market_series_id))
                if (observation.observation_datetime > newest_instant)
                    newest_instant = observation.observation_datetime;
            market.as_of = newest_instant;
            resp.market_pillars.push_back(std::move(market));

            mm::curve_bootstrapped_point point;
            point.tenor = pillar.end_tenor_code;
            point.maturity_date = boot.end_date;
            point.year_fraction = tau;
            point.discount_factor = df;
            point.zero_rate = zero;
            point.implied_forward =
                previous_zero && previous_tau && tau != *previous_tau ?
                    (*previous_zero * *previous_tau - zero * tau) / (*previous_tau - tau) :
                    zero;
            point.shift_bps = (zero - resp.market_pillars.back().rate) * 10000.0;
            resp.bootstrapped.push_back(std::move(point));

            previous_zero = zero;
            previous_tau = tau;
        }

        // The prior close, when one was asked for: the party's published set that
        // was current at the instant, and the zero rate each pillar held then. A set
        // a person could not have seen at that instant is not a prior close, so a
        // publish stamped later does not count however old its as-of is.
        if (request.compare_at != std::chrono::system_clock::time_point{}) {
            const auto at = request.compare_at;
            auto sets = repository::curve_publish_repository{}.read_applied_for_series(
                ctx, config.output_series_id);
            std::erase_if(sets, [&](const auto& set) {
                if (!set.published_at || *set.published_at > at || set.as_of > at)
                    return true;
                return set.withdrawn_at && *set.withdrawn_at <= at;
            });
            if (!sets.empty()) {
                std::ranges::sort(sets, {}, &ores::marketdata::domain::curve_publish::as_of);
                const auto& prior = sets.back();
                resp.compare_at = prior.as_of;
                std::map<std::string, double> factors;
                for (const auto& observation :
                     repository::market_observation_repository{}.read_as_of(
                         ctx, config.output_series_id, prior.as_of)) {
                    const auto named = datum::oresmd_uri_codec::read(observation.oresmd_uri);
                    const auto value = ores::platform::numeric::parse_double(observation.value);
                    if (!named || !value)
                        continue;
                    const auto* term = named->get<datum::field::term>();
                    if (term != nullptr)
                        factors[term->text()] = *value;
                }
                for (auto& point : resp.bootstrapped) {
                    const auto found = factors.find(point.tenor);
                    if (found == factors.end() || !(found->second > 0.0) ||
                        !(point.year_fraction > 0.0))
                        continue;
                    const auto prior_zero = -std::log(found->second) / point.year_fraction;
                    point.prior_zero_rate = prior_zero;
                    point.change_bps = (point.zero_rate - prior_zero) * 10000.0;
                }
            }
        }

        // The verdict is the server's, asked of the one policy, of when the market
        // behind the curve was last written rather than of the instant it was read.
        resp.stale = staleness_policy::is_stale(newest_instant, std::chrono::system_clock::now());
        resp.success = !resp.bootstrapped.empty();
        if (!resp.success)
            resp.message = "Curve " + config.name + " produced no analyzable pillar.";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(curve_analytics_lg(), warn)
            << "Curve analytics for " << request.instrument_id << " failed: " << e.what();
        resp.success = false;
        resp.message = e.what();
    }
    return resp;
}

}
