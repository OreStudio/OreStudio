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
#include "curve_pillar_reader.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/oresmd/pillar_quote.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.platform/numeric/floating_point.hpp"
#include "ores.refdata.core/repository/calendar_event_repository.hpp"
#include "ores.refdata.core/repository/tenor_convention_repository.hpp"
#include "ores.refdata.core/repository/tenor_convention_resolution_repository.hpp"
#include "ores.refdata.core/repository/tenor_repository.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <stdexcept>
#include <string>

namespace ores::marketdata::service {

namespace {

auto& lg() {
    using namespace ores::logging;
    static auto instance = make_logger("ores.marketdata.service.curve_pillar_reader");
    return instance;
}

}

namespace {

std::vector<std::chrono::year_month_day> fomc_meeting_dates(const ores::database::context& ctx) {
    ores::refdata::repository::calendar_event_repository event_repo;
    std::vector<std::chrono::year_month_day> out;
    for (const auto& e :
         event_repo.read_latest_by_diary_entry_type(ctx, "central_bank_meeting", 0, 10000))
        out.push_back(e.event_date);
    std::sort(out.begin(), out.end());
    return out;
}

// The point the analytics layer reads: the discount curve's tenor, the one
// coordinate of the datum URI the row stores.
std::string discount_point_of(const domain::market_observation& obs) {
    const auto d = ores::marketdata::datum::oresmd_uri_codec::read(obs.oresmd_uri);
    if (!d || d->type() != ores::marketdata::datum::instrument_type::discount)
        throw std::invalid_argument("curve_republish_service: observation '" + obs.oresmd_uri +
                                    "' is not a discount-curve point");
    return ores::marketdata::datum::text_of(d->at(ores::marketdata::datum::field::term));
}

}

curve_republish_refdata_context build_refdata_context(const ores::database::context& ctx,
                                                      std::chrono::year_month_day horizon,
                                                      const std::string& tenor_convention_code) {
    namespace refdata_repo = ores::refdata::repository;

    refdata_repo::tenor_repository tenor_repo;
    refdata_repo::tenor_convention_repository convention_repo;
    refdata_repo::tenor_convention_resolution_repository resolution_repo(ctx);

    const auto conventions = convention_repo.read_latest(ctx, tenor_convention_code);
    if (conventions.empty())
        throw std::invalid_argument(std::string("curve_republish_service: tenor convention '") +
                                    tenor_convention_code + "' not found");

    curve_republish_refdata_context refctx;
    refctx.convention = conventions.front();
    for (const auto& t : tenor_repo.read_latest(ctx))
        refctx.tenors_by_code.emplace(t.code, t);
    for (const auto& r : resolution_repo.read_latest_by_convention(refctx.convention.code))
        refctx.resolutions_by_tenor.emplace(r.tenor_code, r);
    refctx.horizon = horizon;

    // A SCHEDULE_STEP convention walks event-lookup schedules (FOMC_MEETING);
    // the standard ANCHOR_OFFSET conventions have no schedule rows and never
    // consult the date set.
    if (refctx.convention.resolution_algorithm == "SCHEDULE_STEP")
        refctx.schedule_dates = fomc_meeting_dates(ctx);

    return refctx;
}

std::vector<ores::analytics::quant::service::bootstrapped_point>
read_discount_curve(const ores::database::context& ctx,
                    const boost::uuids::uuid& output_series_id,
                    std::chrono::system_clock::time_point as_of,
                    const curve_republish_refdata_context& refctx) {
    repository::market_observation_repository obs_repo;
    std::vector<ores::analytics::quant::service::bootstrapped_point> points;
    for (const auto& obs : obs_repo.read_as_of(ctx, output_series_id, as_of)) {
        ores::analytics::quant::service::bootstrapped_point p;
        p.point_id = discount_point_of(obs);
        p.date = resolve_tenor_date(refctx, p.point_id);
        p.discount_factor = observation_value(obs);
        points.push_back(p);
    }
    std::sort(points.begin(), points.end(), [](const auto& a, const auto& b) {
        return std::chrono::sys_days(a.date) < std::chrono::sys_days(b.date);
    });
    return points;
}


double observation_value(const domain::market_observation& obs) {
    const auto value = ores::platform::numeric::parse_double(obs.value);
    if (!value)
        throw std::runtime_error("observation " + boost::uuids::to_string(obs.id) +
                                 " holds a value that is not a number: '" + obs.value + "'");
    return *value;
}

pillar_read
read_pillar_rates(const ores::database::context& ctx,
                  const std::vector<ores::refdata::domain::ir_curve_bootstrap_pillar>& pillars,
                  const curve_republish_refdata_context& refctx,
                  std::chrono::system_clock::time_point as_of) {
    namespace datum = ores::marketdata::datum;

    repository::market_series_repository series_repo;
    repository::market_observation_repository obs_repo;

    pillar_read out;
    for (const auto& p : pillars) {
        const auto series =
            series_repo.read_latest(ctx, boost::uuids::to_string(p.market_series_id));
        if (series.empty()) {
            BOOST_LOG_SEV(lg(), ores::logging::warn)
                << "No market series " << boost::uuids::to_string(p.market_series_id)
                << " holds the pillar " << p.end_tenor_code << " of curve config "
                << boost::uuids::to_string(p.bootstrap_config_id);
            continue;
        }

        // The series says which ladder the pillar reads; its end tenor, resolved
        // through the config's convention, says which rung of it.
        const auto term = core::pillar_term_text(p.start_tenor_code,
                                                 p.end_tenor_code,
                                                 resolve_tenor_date(refctx, p.start_tenor_code),
                                                 resolve_tenor_date(refctx, p.end_tenor_code));
        bool found = false;
        for (const auto& obs : obs_repo.read_as_of(ctx, series.front().id, as_of)) {
            const auto named = datum::oresmd_uri_codec::read(obs.oresmd_uri);
            if (!named || named->is_series() || !named->holds(datum::field::term))
                continue;
            if (datum::text_of(named->at(datum::field::term)) != term)
                continue;
            out.rates_by_point_id.insert_or_assign(p.end_tenor_code, observation_value(obs));
            found = true;
        }
        // One series answers every rung of a ladder, so a list that repeated it
        // would say less than one that names it once: this is the set of series the
        // curve read, and it is what the lineage records.
        const auto series_id = boost::uuids::to_string(series.front().id);
        if (found && std::ranges::find(out.series_ids, series_id) == out.series_ids.end())
            out.series_ids.push_back(series_id);
    }

    return out;
}

}
