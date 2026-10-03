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
#include "../curve_pillar_reader.hpp"
#include "../curve_republish_resolver.hpp"
#include "ores.analytics.quant/service/curve_bootstrap_engine.hpp"
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.api/domain/observation_lineage.hpp"
#include "ores.marketdata.core/datum/ore_key_codec.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/oresmd/pillar_quote_key.hpp"
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
#include <optional>
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

curve_republish_refdata_context build_refdata_context(ores::database::context ctx,
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

// The point the analytics layer reads: the discount curve's tenor, the one
// coordinate of the datum URI the row stores.
std::string discount_point_of(const domain::market_observation& obs) {
    const auto d = datum::oresmd_uri_codec::read(obs.oresmd_uri);
    if (!d || d->type() != datum::instrument_type::discount)
        throw std::invalid_argument("curve_republish_service: observation '" + obs.oresmd_uri +
                                    "' is not a discount-curve point");
    return datum::text_of(d->at(datum::field::term));
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
        p.point_id = discount_point_of(obs);
        p.date = resolve_tenor_date(refctx, p.point_id);
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

// Reads the config's pre-minted output market_series, which the keys below are projected
// from. A missing row is a config-integrity error, not auto-fabricated: this service has
// none of series_type/metric/qualifier to invent one from.
domain::market_series
read_output_series(ores::database::context ctx,
                   const ores::refdata::domain::ir_curve_bootstrap_config& config) {
    repository::market_series_repository series_repo;
    auto series = series_repo.read_latest(ctx, boost::uuids::to_string(config.output_series_id));
    if (series.empty())
        throw std::invalid_argument(
            "curve_republish_service: output market_series not found for bootstrap config: " +
            boost::uuids::to_string(config.output_series_id));
    return series.front();
}

// Stamps the config's output market_series IR_CURVE_BOOTSTRAP -- the config-creation
// task's own doc comment describes this as happening at config-creation time, but no
// service code does it yet (see this task's own * Plan for why re-opening that task is
// out of scope here).
void stamp_output_series(ores::database::context ctx,
                         const ores::refdata::domain::ir_curve_bootstrap_config& config,
                         domain::market_series s) {
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
    repository::market_series_repository series_repo;
    series_repo.write(ctx, s);
}

// The datum an observation is written under: the output series at the pillar's
// own point. A series and a point that make no datum, or a datum no ORE key
// names, is a config-integrity error.
datum::market_datum datum_for(const datum::market_datum& series, const std::string& point_id) {
    const auto fail = [&](const std::string& why) {
        return std::invalid_argument("curve_republish_service: output series and point '" +
                                     point_id + "' name no ORE quote: " + why);
    };
    auto d = datum::datum_at_point(series, point_id);
    if (!d)
        throw fail(d.error());
    if (const auto key = datum::ore_key_codec::write(*d); !key)
        throw fail(key.error());
    return std::move(*d);
}

// One republish's whole input and output: the config, the curve it bootstraps, and
// the series its quotes were read from. republish() needs the last of those for the
// lineage and compute() needs only the curve, so both call this rather than reading
// the pillars twice.
struct curve_computation final {
    ores::refdata::domain::ir_curve_bootstrap_config config;
    std::vector<ores::analytics::quant::service::bootstrapped_point> points;
    std::vector<std::string> source_series_ids;
};

curve_computation compute_curve(ores::database::context ctx,
                                const boost::uuids::uuid& bootstrap_config_id,
                                std::chrono::system_clock::time_point as_of) {
    namespace quant = ores::analytics::quant::service;

    const auto config = read_config(ctx, bootstrap_config_id);
    const auto pillars = read_pillars(ctx, bootstrap_config_id);
    const auto horizon = std::chrono::floor<std::chrono::days>(as_of);
    const auto refctx = build_refdata_context(
        ctx, std::chrono::year_month_day{horizon}, config.tenor_convention_code);
    const auto raw = read_pillar_rates(ctx, config, pillars, refctx, as_of);
    const auto bootstrap_pillars =
        resolve_bootstrap_pillars(pillars, refctx, raw.rates_by_point_id);

    const auto day_count_convention =
        quant::parse_day_count_convention_code(config.day_count_convention);
    const auto interpolation_method =
        quant::parse_interpolation_method_code(config.interpolation_method);

    std::vector<quant::bootstrapped_point> discount_curve;
    std::optional<ores::refdata::domain::ir_curve_bootstrap_config> discount_config;
    const bool is_projection = config.curve_family_role == "PROJECTION";
    if (is_projection) {
        discount_config = read_config(ctx, config.discount_curve_config_id);
        discount_curve = read_discount_curve(ctx, discount_config->output_series_id, as_of, refctx);
    }

    curve_computation out;
    out.config = config;
    out.source_series_ids = raw.series_ids;
    if (discount_config)
        out.source_series_ids.push_back(boost::uuids::to_string(discount_config->output_series_id));
    // A pillar read from the grid names the same series as its neighbours, and a
    // provenance list that repeats one id says less than one that names it once.
    std::vector<std::string> distinct;
    for (const auto& id : out.source_series_ids)
        if (std::find(distinct.begin(), distinct.end(), id) == distinct.end())
            distinct.push_back(id);
    out.source_series_ids = std::move(distinct);

    out.points =
        quant::curve_bootstrap_engine::bootstrap(std::chrono::year_month_day{horizon},
                                                 bootstrap_pillars,
                                                 day_count_convention,
                                                 interpolation_method,
                                                 is_projection ? &discount_curve : nullptr);
    return out;
}

std::string source_series_ids_json(const std::vector<std::string>& ids) {
    std::string out = "[";
    for (std::size_t i = 0; i < ids.size(); ++i) {
        if (i > 0)
            out += ",";
        out += "\"" + ids[i] + "\"";
    }
    return out + "]";
}

} // namespace

std::vector<ores::analytics::quant::service::bootstrapped_point>
curve_republish_service::compute(context ctx,
                                 const boost::uuids::uuid& bootstrap_config_id,
                                 std::chrono::system_clock::time_point as_of) {
    BOOST_LOG_SEV(lg(), info) << "Computing bootstrap config " << bootstrap_config_id << " as of "
                              << as_of;

    return compute_curve(ctx, bootstrap_config_id, as_of).points;
}

void curve_republish_service::republish(context ctx,
                                        const boost::uuids::uuid& bootstrap_config_id,
                                        std::chrono::system_clock::time_point as_of) {
    BOOST_LOG_SEV(lg(), info) << "Republishing bootstrap config " << bootstrap_config_id
                              << " as of " << as_of;

    const auto computed = compute_curve(ctx, bootstrap_config_id, as_of);
    const auto& config = computed.config;
    const auto& bootstrapped = computed.points;

    const auto output_series = read_output_series(ctx, config);
    // Every key before the series is stamped or a row is written: a point the output
    // series cannot name fails here, so a failure leaves neither a half-claimed series
    // nor an observation without a key. The identity is parsed once, not per pillar.
    const auto output_series_id = datum::oresmd_uri_codec::read(output_series.oresmd_uri);
    if (!output_series_id || !output_series_id->is_series())
        throw std::invalid_argument("curve_republish_service: output series '" +
                                    output_series.oresmd_uri + "' is not a series URI");
    std::vector<datum::market_datum> datums;
    datums.reserve(bootstrapped.size());
    for (const auto& point : bootstrapped)
        datums.push_back(datum_for(*output_series_id, point.point_id));

    stamp_output_series(ctx, config, output_series);

    const std::string source_series_ids = source_series_ids_json(computed.source_series_ids);

    boost::uuids::random_generator uuid_gen;
    std::vector<domain::market_observation> observations;
    std::vector<domain::observation_lineage> lineages;
    observations.reserve(bootstrapped.size());
    lineages.reserve(bootstrapped.size());

    repository::observation_lineage_repository lineage_repo;
    for (std::size_t i = 0; i < bootstrapped.size(); ++i) {
        const auto& point = bootstrapped[i];
        const auto& point_datum = datums[i];
        const auto datum_uri = datum::oresmd_uri_codec::write(point_datum).value();
        domain::market_observation obs;
        obs.id = uuid_gen();
        obs.tenant_id = ctx.tenant_id();
        obs.party_id = config.party_id;
        obs.series_id = config.output_series_id;
        obs.observation_datetime = as_of;
        obs.oresmd_uri = datum_uri;
        obs.value = std::format("{:.17g}", point.discount_factor);
        obs.source = "ir_curve_bootstrap:" + boost::uuids::to_string(config.id);
        obs.key = datum::ore_key_codec::write(point_datum).value();
        observations.push_back(std::move(obs));

        // A rerun over the same (series, as_of, point_id) natural key must reuse the prior
        // lineage row's own id -- the insert trigger's version-bump/close-prior-row logic only
        // fires when it finds an existing row with that same id, per this table's own doc
        // comment ("A rerun of a derivation over the same tenor/point natural key closes the
        // prior generation's lineage row and inserts a new one"). Minting a fresh id every
        // republish bypassed that and hit the natural-key unique index instead.
        const auto existing =
            lineage_repo.read_latest_by_observation(ctx, config.output_series_id, as_of, datum_uri);

        domain::observation_lineage lin;
        lin.tenant_id = ctx.tenant_id();
        lin.id = existing ? existing->id : uuid_gen();
        lin.party_id = config.party_id;
        lin.series_id = config.output_series_id;
        lin.observation_datetime = as_of;
        lin.oresmd_uri = datum_uri;
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
