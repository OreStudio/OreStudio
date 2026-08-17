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
#include "ores.marketdata.service/app/curve_republish_service.hpp"
#include "../curve_republish_resolver.hpp"
#include "ores.analytics.quant/service/curve_bootstrap_engine.hpp"
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.api/domain/observation_lineage.hpp"
#include "ores.marketdata.core/repository/market_observations_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/repository/observation_lineage_repository.hpp"
#include "ores.marketdata.core/service/observation_lineage_service.hpp"
#include "ores.refdata.api/domain/ir_curve_bootstrap_config.hpp"
#include "ores.refdata.api/domain/ir_curve_bootstrap_pillar.hpp"
#include "ores.refdata.core/repository/calendar_event_repository.hpp"
#include "ores.refdata.core/repository/ir_curve_bootstrap_config_repository.hpp"
#include "ores.refdata.core/repository/ir_curve_bootstrap_pillar_repository.hpp"
#include "ores.refdata.core/repository/tenor_convention_repository.hpp"
#include "ores.refdata.core/repository/tenor_convention_resolution_repository.hpp"
#include "ores.refdata.core/repository/tenor_repository.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <format>
#include <stdexcept>
#include <unordered_map>

namespace ores::marketdata::service::app {

using namespace ores::logging;

namespace {

// The FOMC meeting dates the RATES_SPOT_FOMC schedule walk steps along, read
// from the event store. Sorted ascending explicitly: the walk's
// nth_on_or_after silently yields a wrong "n-th meeting" on unsorted input,
// and the repository's diary-entry-type read orders by id, not by event_date.
std::vector<std::chrono::year_month_day> fomc_meeting_dates(ores::database::context ctx) {
    ores::refdata::repository::calendar_event_repository event_repo;
    std::vector<std::chrono::year_month_day> out;
    for (const auto& e :
         event_repo.read_latest_by_diary_entry_type(ctx, "central_bank_meeting", 0, 10000))
        out.push_back(e.event_date);
    std::sort(out.begin(), out.end());
    return out;
}

// The tenor convention a curve's source series resolves under, selected by the
// series' oresmd URI: the FOMC raw grid (e.g.
// "oresmd://ir/usd?index=sofr&tenor=fomc&type=fixing") resolves under
// RATES_SPOT_FOMC; everything else under RATES_SPOT_FORWARD.
const char* tenor_convention_code_for(const std::string& uri) {
    return uri.find("&tenor=fomc") != std::string::npos ? "RATES_SPOT_FOMC" : "RATES_SPOT_FORWARD";
}

curve_republish_refdata_context build_refdata_context(ores::database::context ctx,
                                                      std::chrono::year_month_day horizon,
                                                      const boost::uuids::uuid& source_series_id) {
    namespace refdata_repo = ores::refdata::repository;

    repository::market_series_repository series_repo;
    const auto series = series_repo.read_latest(ctx, boost::uuids::to_string(source_series_id));
    if (series.empty())
        throw std::invalid_argument("curve_republish_service: source market_series not found: " +
                                    boost::uuids::to_string(source_series_id));
    const auto* tenor_convention_code = tenor_convention_code_for(series.front().oresmd_uri);

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

std::unordered_map<std::string, double>
read_raw_rates(ores::database::context ctx,
               const boost::uuids::uuid& source_series_id,
               std::chrono::system_clock::time_point as_of) {
    repository::market_observations_repository obs_repo;
    std::unordered_map<std::string, double> out;
    for (const auto& obs : obs_repo.read_as_of(ctx, source_series_id, as_of))
        out.emplace(obs.point_id, std::stod(obs.value));
    return out;
}

std::vector<ores::analytics::quant::service::bootstrapped_point>
read_discount_curve(ores::database::context ctx,
                    const boost::uuids::uuid& output_series_id,
                    std::chrono::system_clock::time_point as_of,
                    const curve_republish_refdata_context& refctx) {
    repository::market_observations_repository obs_repo;
    std::vector<ores::analytics::quant::service::bootstrapped_point> points;
    for (const auto& obs : obs_repo.read_as_of(ctx, output_series_id, as_of)) {
        ores::analytics::quant::service::bootstrapped_point p;
        p.point_id = obs.point_id;
        p.date = resolve_tenor_date(refctx, obs.point_id);
        p.discount_factor = std::stod(obs.value);
        points.push_back(p);
    }
    std::sort(points.begin(), points.end(), [](const auto& a, const auto& b) {
        return std::chrono::sys_days(a.date) < std::chrono::sys_days(b.date);
    });
    return points;
}

ores::refdata::domain::ir_curve_bootstrap_config
read_config(ores::database::context ctx, const boost::uuids::uuid& bootstrap_config_id) {
    ores::refdata::repository::ir_curve_bootstrap_config_repository config_repo;
    auto configs = config_repo.read_latest(ctx, boost::uuids::to_string(bootstrap_config_id));
    if (configs.empty())
        throw std::invalid_argument("curve_republish_service: bootstrap config not found: " +
                                    boost::uuids::to_string(bootstrap_config_id));
    return configs.front();
}

std::vector<ores::refdata::domain::ir_curve_bootstrap_pillar>
read_pillars(ores::database::context ctx, const boost::uuids::uuid& bootstrap_config_id) {
    ores::refdata::repository::ir_curve_bootstrap_pillar_repository pillar_repo;
    std::vector<ores::refdata::domain::ir_curve_bootstrap_pillar> pillars;
    for (auto& p : pillar_repo.read_latest(ctx))
        if (p.bootstrap_config_id == bootstrap_config_id)
            pillars.push_back(std::move(p));
    if (pillars.empty())
        throw std::invalid_argument("curve_republish_service: bootstrap config has no pillars: " +
                                    boost::uuids::to_string(bootstrap_config_id));
    return pillars;
}

// Ensures the config's pre-minted output market_series is stamped IR_CURVE_BOOTSTRAP -- the
// config-creation task's own doc comment describes this as happening at config-creation time, but
// no service code does it yet (see this task's own * Plan for why re-opening that task is out of
// scope here). A missing row is a config-integrity error, not auto-fabricated: this service has
// no oresmd URI to invent one from.
void ensure_output_series_stamped(ores::database::context ctx,
                                  const ores::refdata::domain::ir_curve_bootstrap_config& config) {
    repository::market_series_repository series_repo;
    auto series = series_repo.read_latest(ctx, boost::uuids::to_string(config.output_series_id));
    if (series.empty())
        throw std::invalid_argument(
            "curve_republish_service: output market_series not found for bootstrap config: " +
            boost::uuids::to_string(config.output_series_id));

    auto s = series.front();
    if (s.derivation_kind != "OBSERVED")
        return;

    s.derivation_kind = "IR_CURVE_BOOTSTRAP";
    s.derivation_config_id = config.id;
    s.derivation_config_version = config.version;
    s.modified_by = ctx.service_account();
    s.performed_by = ctx.service_account();
    s.change_reason_code = "system.derived_series";
    s.change_commentary = "Claimed by IR curve bootstrap config " +
                          boost::uuids::to_string(config.id) + " on first republish";
    series_repo.write(ctx, s);
}

} // namespace

std::vector<ores::analytics::quant::service::bootstrapped_point>
curve_republish_service::compute(context ctx,
                                 const boost::uuids::uuid& bootstrap_config_id,
                                 std::chrono::system_clock::time_point as_of) {
    namespace quant = ores::analytics::quant::service;

    BOOST_LOG_SEV(lg(), info) << "Computing bootstrap config " << bootstrap_config_id << " as of "
                              << as_of;

    const auto config = read_config(ctx, bootstrap_config_id);
    const auto pillars = read_pillars(ctx, bootstrap_config_id);
    const auto horizon = std::chrono::floor<std::chrono::days>(as_of);
    const auto refctx =
        build_refdata_context(ctx, std::chrono::year_month_day{horizon}, config.source_series_id);
    const auto raw_rates = read_raw_rates(ctx, config.source_series_id, as_of);
    const auto bootstrap_pillars = resolve_bootstrap_pillars(pillars, refctx, raw_rates);

    const auto day_count_convention =
        quant::parse_day_count_convention_code(config.day_count_convention);
    const auto interpolation_method =
        quant::parse_interpolation_method_code(config.interpolation_method);

    std::vector<quant::bootstrapped_point> discount_curve;
    const bool is_projection = config.curve_family_role == "PROJECTION";
    if (is_projection) {
        const auto discount_config = read_config(ctx, config.discount_curve_config_id);
        discount_curve = read_discount_curve(ctx, discount_config.output_series_id, as_of, refctx);
    }

    return quant::curve_bootstrap_engine::bootstrap(std::chrono::year_month_day{horizon},
                                                    bootstrap_pillars,
                                                    day_count_convention,
                                                    interpolation_method,
                                                    is_projection ? &discount_curve : nullptr);
}

void curve_republish_service::republish(context ctx,
                                        const boost::uuids::uuid& bootstrap_config_id,
                                        std::chrono::system_clock::time_point as_of) {
    BOOST_LOG_SEV(lg(), info) << "Republishing bootstrap config " << bootstrap_config_id
                              << " as of " << as_of;

    const auto bootstrapped = compute(ctx, bootstrap_config_id, as_of);
    const auto config = read_config(ctx, bootstrap_config_id);
    const bool is_projection = config.curve_family_role == "PROJECTION";

    ensure_output_series_stamped(ctx, config);

    const std::string source_series_ids =
        is_projection ? std::format("[\"{}\",\"{}\"]",
                                    boost::uuids::to_string(config.source_series_id),
                                    boost::uuids::to_string(config.discount_curve_config_id)) :
                        std::format("[\"{}\"]", boost::uuids::to_string(config.source_series_id));

    boost::uuids::random_generator uuid_gen;
    std::vector<domain::market_observation> observations;
    std::vector<domain::observation_lineage> lineages;
    observations.reserve(bootstrapped.size());
    lineages.reserve(bootstrapped.size());

    repository::observation_lineage_repository lineage_repo;
    for (const auto& point : bootstrapped) {
        domain::market_observation obs;
        obs.id = uuid_gen();
        obs.tenant_id = ctx.tenant_id();
        obs.party_id = config.party_id;
        obs.series_id = config.output_series_id;
        obs.observation_datetime = as_of;
        obs.point_id = point.point_id;
        obs.value = std::format("{:.17g}", point.discount_factor);
        obs.source = "ir_curve_bootstrap:" + boost::uuids::to_string(config.id);
        observations.push_back(std::move(obs));

        // A rerun over the same (series, as_of, point_id) natural key must reuse the prior
        // lineage row's own id -- the insert trigger's version-bump/close-prior-row logic only
        // fires when it finds an existing row with that same id, per this table's own doc
        // comment ("A rerun of a derivation over the same tenor/point natural key closes the
        // prior generation's lineage row and inserts a new one"). Minting a fresh id every
        // republish bypassed that and hit the natural-key unique index instead.
        const auto existing = lineage_repo.read_latest_by_observation(
            ctx, config.output_series_id, as_of, point.point_id);

        domain::observation_lineage lin;
        lin.tenant_id = ctx.tenant_id();
        lin.id = existing ? existing->id : uuid_gen();
        lin.party_id = config.party_id;
        lin.series_id = config.output_series_id;
        lin.observation_datetime = as_of;
        lin.point_id = point.point_id;
        lin.derivation_config_id = config.id;
        lin.derivation_config_version = config.version;
        lin.source_as_of = as_of;
        lin.source_series_ids = source_series_ids;
        lin.modified_by = ctx.service_account();
        lin.performed_by = ctx.service_account();
        lin.change_reason_code = "system.curve_bootstrap";
        lin.change_commentary = "IR curve bootstrap republish";
        lineages.push_back(std::move(lin));
    }

    repository::market_observations_repository obs_repo;
    obs_repo.write(ctx, observations);

    ores::marketdata::service::observation_lineage_service lineage_service(ctx);
    lineage_service.save_observation_lineages(lineages);

    BOOST_LOG_SEV(lg(), info) << "Republished " << observations.size()
                              << " points for bootstrap config " << bootstrap_config_id;
}

}
