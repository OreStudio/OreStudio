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
#include "ores.marketdata.service/app/fx_vol_surface_calibration_service.hpp"
#include "ores.analytics.quant/service/ssvi_calibration_engine.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.api/domain/catalogue_ids.hpp"
#include "ores.marketdata.api/domain/fx_vol_surface_calibration.hpp"
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.api/domain/observation_lineage.hpp"
#include "ores.marketdata.core/repository/fx_vol_surface_calibration_repository.hpp"
#include "ores.marketdata.core/repository/fx_vol_surface_publish_repository.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/repository/observation_lineage_repository.hpp"
#include "ores.marketdata.core/service/observation_lineage_service.hpp"
#include "ores.marketdata.core/service/series_shape_writer.hpp"
#include "ores.marketdata.service/app/fx_forward_publish_service.hpp"
#include "ores.marketdata.service/app/fx_vol_surface_calibration_plan.hpp"
#include "ores.platform/numeric/floating_point.hpp"
#include "ores.refdata.core/repository/fx_option_convention_repository.hpp"
#include "ores.refdata.core/repository/fx_vol_surface_blueprint_repository.hpp"
#include "ores.refdata.core/repository/fx_vol_surface_pillar_repository.hpp"
#include "ores.refdata.core/repository/fx_vol_surface_pillar_source_repository.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/uuid/name_generator_sha1.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <format>
#include <set>
#include <stdexcept>
#include <vector>

namespace ores::marketdata::service::app {

namespace {

namespace mm = ores::marketdata::messaging;
namespace qs = ores::analytics::quant::service;
namespace refdata_msg = ores::refdata::messaging;
namespace refdata_repo = ores::refdata::repository;
using time_point = std::chrono::system_clock::time_point;
using context = ores::database::context;

/// How many rows of a catalogue one read asks for. A recipe names one blueprint,
/// so a page covers it and the next page is the rare case.
constexpr std::uint32_t recipe_page_size = 500;

auto& calibration_log() {
    static auto instance = ores::logging::make_logger(
        "ores.marketdata.service.app.fx_vol_surface_calibration_service");
    return instance;
}

surface_calibration_recipe
read_recipe(context ctx, const std::string& blueprint_id, const boost::uuids::uuid& party) {
    const auto blueprints =
        refdata_repo::fx_vol_surface_blueprint_repository{}.read_latest(ctx, blueprint_id);
    if (blueprints.empty())
        throw std::invalid_argument("No FX volatility surface blueprint " + blueprint_id +
                                    " is visible to your party.");
    surface_calibration_recipe recipe;
    recipe.blueprint = blueprints.front();
    if (recipe.blueprint.party_id != party)
        throw std::invalid_argument("The blueprint " + recipe.blueprint.name +
                                    " belongs to another party, and a party calibrates its own.");
    // One blueprint's pillars, asked of the store by name. Reading the party's
    // whole catalogue here and narrowing it in memory made a calibration cost
    // what every blueprint the party holds costs, so each surface it added made
    // every later calibration slower.
    refdata_msg::fx_vol_surface_pillars_filter pillars_of_the_blueprint;
    pillars_of_the_blueprint.blueprint_id = recipe.blueprint.id;
    std::set<boost::uuids::uuid> pillar_ids;
    for (std::uint32_t offset = 0;; offset += recipe_page_size) {
        auto page = refdata_repo::fx_vol_surface_pillar_repository{}.read_latest(
            ctx, offset, recipe_page_size, {}, pillars_of_the_blueprint);
        for (auto& p : page) {
            pillar_ids.insert(p.id);
            recipe.pillars.push_back(std::move(p));
        }
        if (page.size() < recipe_page_size)
            break;
    }
    if (!pillar_ids.empty()) {
        refdata_msg::fx_vol_surface_pillar_sources_filter sources_of_those_pillars;
        sources_of_those_pillars.pillar_id_one_of =
            std::vector<boost::uuids::uuid>(pillar_ids.begin(), pillar_ids.end());
        for (std::uint32_t offset = 0;; offset += recipe_page_size) {
            auto page = refdata_repo::fx_vol_surface_pillar_source_repository{}.read_latest(
                ctx, offset, recipe_page_size, {}, sources_of_those_pillars);
            for (auto& s : page)
                recipe.sources.push_back(std::move(s));
            if (page.size() < recipe_page_size)
                break;
        }
    }
    const auto conventions = refdata_repo::fx_option_convention_repository{}.read_latest(
        ctx, recipe.blueprint.fx_option_convention_id);
    if (!conventions.empty())
        recipe.convention = conventions.front();
    return recipe;
}

surface_forward_set
read_forwards(context ctx, const surface_calibration_recipe& recipe, time_point as_of) {
    const auto pair =
        recipe.blueprint.base_currency_code + "/" + recipe.blueprint.quote_currency_code;
    const auto set = fx_forward_publish_service::read(ctx, {.pair_code = pair, .as_of = as_of});
    if (!set.success)
        throw std::invalid_argument("The forwards of " + pair + ": " + set.message +
                                    " A surface reads its forwards from a published set, which "
                                    "needs the published funding curves of both currencies.");
    surface_forward_set out{.publish_id = set.publish_id,
                            .spot = set.spot_rate,
                            .spot_date = set.spot_date,
                            .points = {}};
    for (const auto& row : set.rows)
        out.points.push_back({.tenor = row.tenor,
                              .value_date = row.value_date,
                              .base_discount = row.base_discount,
                              .quote_discount = row.quote_discount});
    return out;
}

surface_series_quotes
read_quotes(context ctx, const boost::uuids::uuid& series_id, time_point as_of) {
    surface_series_quotes out;
    for (const auto& obs :
         repository::market_observation_repository{}.read_as_of(ctx, series_id, as_of)) {
        const auto named = datum::oresmd_uri_codec::read(obs.oresmd_uri);
        if (!named)
            continue;
        const auto* expiry = named->get<datum::field::expiry>();
        const auto* label = named->get<datum::field::strike_label>();
        const auto value = ores::platform::numeric::parse_double(obs.value);
        if (!expiry || !label || !value || !std::isfinite(*value))
            continue;
        out[{expiry->text(), label->text()}] = *value;
    }
    return out;
}

/// The point of the output series a grid point is written as.
datum::market_datum grid_datum(const datum::market_datum& series,
                               const qs::ssvi_grid_point& point) {
    const auto expiry = datum::term::parse(point.expiry_id);
    const auto strike = datum::strike::parse(point.strike_id);
    if (!expiry || !strike)
        throw std::invalid_argument("The grid point " + point.expiry_id + " " + point.strike_id +
                                    " is not a term and a strike.");
    auto d = datum::datum_at(series,
                             {{datum::field::expiry, datum::value(*expiry)},
                              {datum::field::strike, datum::value(*strike)}});
    if (!d)
        throw std::invalid_argument("The output series names no point " + point.expiry_id + " " +
                                    point.strike_id + ": " + d.error());
    return std::move(*d);
}

std::string json_strings(const std::vector<std::string>& items) {
    std::string out = "[";
    for (std::size_t i = 0; i < items.size(); ++i)
        out += (i > 0 ? ",\"" : "\"") + items[i] + "\"";
    return out + "]";
}

std::string json_numbers(const std::vector<double>& items) {
    std::string out = "[";
    for (std::size_t i = 0; i < items.size(); ++i)
        out += (i > 0 ? "," : "") + std::format("{:.17g}", items[i]);
    return out + "]";
}

std::string to_string(qs::ssvi_arbitrage_kind kind) {
    return kind == qs::ssvi_arbitrage_kind::calendar ? "calendar" : "butterfly";
}

}

boost::uuids::uuid calibration_id_of(const boost::uuids::uuid& blueprint_id,
                                     std::chrono::system_clock::time_point as_of) {
    static const auto calibration_namespace =
        boost::uuids::string_generator{}("5a3c8e42-8a29-48ee-9953-c76a29a3c6ca");
    const auto micros =
        std::chrono::duration_cast<std::chrono::microseconds>(as_of.time_since_epoch()).count();
    return boost::uuids::name_generator_sha1(calibration_namespace)(
        boost::uuids::to_string(blueprint_id) + "@" + std::to_string(micros));
}

mm::calibrate_fx_vol_surface_response
fx_vol_surface_calibration_service::calibrate(context ctx,
                                              const mm::calibrate_fx_vol_surface_request& req) {
    mm::calibrate_fx_vol_surface_response out;
    try {
        const auto party = ores::service::messaging::session_party(ctx, "A surface");
        const auto as_of = std::chrono::floor<std::chrono::microseconds>(req.as_of);
        if (as_of == time_point{})
            throw std::invalid_argument(
                "Name the as-of instant the calibration reads the market at.");

        const auto recipe = read_recipe(ctx, req.blueprint_id, party);
        // A published surface never changes: its grid is what the publish authorised.
        for (const auto& published :
             repository::fx_vol_surface_publish_repository{}.read_published_for_blueprint(
                 ctx, recipe.blueprint.id))
            if (published.as_of == as_of)
                throw std::invalid_argument(
                    "The surface of " + recipe.blueprint.name +
                    " as of this instant is "
                    "published, and a published surface never changes. Calibrate at a "
                    "new as-of.");
        const auto checked = check_recipe(recipe);
        if (!checked)
            throw std::invalid_argument("The recipe of " + recipe.blueprint.name +
                                        " is refused: " + checked.error() + ".");

        const auto forwards = read_forwards(ctx, recipe, as_of);
        std::map<boost::uuids::uuid, surface_series_quotes> quotes;
        for (const auto& source : recipe.sources)
            if (!quotes.contains(source.market_series_id))
                quotes.emplace(source.market_series_id,
                               read_quotes(ctx, source.market_series_id, as_of));
        const auto as_of_date =
            std::chrono::year_month_day{std::chrono::floor<std::chrono::days>(as_of)};
        const auto prepared =
            prepare_calibration(recipe, *checked, forwards, quotes, as_of_date, pillar_tolerance);
        if (!prepared)
            throw std::invalid_argument("The quotes of " + recipe.blueprint.name +
                                        " are refused: " + prepared.error() + ".");
        const auto fit = qs::calibrate_ssvi(prepared->request);
        if (!fit)
            throw std::invalid_argument("The calibration of " + recipe.blueprint.name +
                                        " failed: " + fit.error() + ".");

        const auto& blueprint = recipe.blueprint;
        auto series_rows = repository::market_series_repository{}.read_latest(
            ctx, boost::uuids::to_string(blueprint.output_series_id));
        if (series_rows.empty())
            throw std::invalid_argument("The output series of " + blueprint.name +
                                        " is not stored.");
        auto output_series = series_rows.front();
        const auto series_datum = datum::oresmd_uri_codec::read(output_series.oresmd_uri);
        if (!series_datum || !series_datum->is_series())
            throw std::invalid_argument("The output series of " + blueprint.name + ", " +
                                        output_series.oresmd_uri + ", is not a series URI.");
        const bool claimed = output_series.derivation_kind == "FX_VOL_SURFACE_CALIBRATION" &&
                             output_series.derivation_config_id == blueprint.id;
        if (output_series.derivation_kind != "OBSERVED" && !claimed)
            throw std::invalid_argument("The output series of " + blueprint.name +
                                        " is derived by " + output_series.derivation_kind +
                                        ", and only its own blueprint writes a surface into it.");
        std::vector<datum::market_datum> datums;
        for (const auto& point : fit->grid)
            datums.push_back(grid_datum(*series_datum, point));

        // Every grid point comes from the whole fit, so each names every source series. A series
        // carries the weight of the first pillar that reads it; the weight of each pillar's source
        // is in the blueprint version the lineage names.
        std::vector<std::string> source_ids;
        std::vector<double> weights;
        for (const auto& [point, used] : prepared->sources_by_point)
            for (const auto& source : used) {
                const auto id = boost::uuids::to_string(source.series_id);
                if (std::ranges::find(source_ids, id) == source_ids.end()) {
                    source_ids.push_back(id);
                    weights.push_back(source.weight);
                }
            }
        const auto source_series_ids = json_strings(source_ids);
        const auto source_weights = json_numbers(weights);
        const auto writer = ores::service::messaging::delegated_actor(ctx);

        // The candidate is one act: a grid without its lineage or its record would be a surface
        // nobody can trace.
        ores::database::repository::unit_of_work uow(ctx);
        const auto& tx = uow.ctx();
        if (!claimed) {
            output_series.derivation_kind = "FX_VOL_SURFACE_CALIBRATION";
            output_series.derivation_config_id = blueprint.id;
            output_series.derivation_config_version = blueprint.version;
            output_series.modified_by = writer;
            output_series.performed_by = ctx.service_account();
            output_series.change_reason_code = "system.derived_series";
            output_series.change_commentary =
                "Claimed by FX volatility surface blueprint " + blueprint.name;
            repository::market_series_repository{}.write(tx, output_series);
        }

        boost::uuids::random_generator uuid_gen;
        repository::observation_lineage_repository lineage_repo;
        std::vector<domain::market_observation> observations;
        std::vector<domain::observation_lineage> lineages;
        for (std::size_t i = 0; i < fit->grid.size(); ++i) {
            const auto uri = datum::oresmd_uri_codec::write(datums[i]).value();
            domain::market_observation obs;
            obs.id = uuid_gen();
            obs.tenant_id = ctx.tenant_id();
            obs.party_id = blueprint.party_id;
            obs.series_id = blueprint.output_series_id;
            obs.observation_datetime = as_of;
            obs.oresmd_uri = uri;
            obs.value = std::format("{:.17g}", fit->grid[i].volatility);
            obs.source = "fx_vol_surface_calibration:" + boost::uuids::to_string(blueprint.id);
            obs.feed_id = domain::catalogue_ids::ore_derived_feed_id();
            observations.push_back(std::move(obs));

            const auto existing =
                lineage_repo.read_latest_by_observation(tx, blueprint.output_series_id, as_of, uri);
            domain::observation_lineage lin;
            lin.tenant_id = ctx.tenant_id();
            lin.id = existing ? existing->id : uuid_gen();
            lin.party_id = blueprint.party_id;
            lin.series_id = blueprint.output_series_id;
            lin.observation_datetime = as_of;
            lin.oresmd_uri = uri;
            lin.derivation_config_id = blueprint.id;
            lin.derivation_config_version = blueprint.version;
            lin.source_as_of = as_of;
            lin.source_series_ids = source_series_ids;
            lin.source_weights = source_weights;
            lin.modified_by = writer;
            lin.performed_by = ctx.service_account();
            lin.change_reason_code = "system.surface_calibration";
            lin.change_commentary = "FX volatility surface calibration of " + blueprint.name;
            lineages.push_back(std::move(lin));
        }
        repository::market_observation_repository{}.insert(tx, observations);
        series_shape_writer::declare(tx,
                                     *series_datum,
                                     blueprint.output_series_id,
                                     blueprint.party_id,
                                     datums,
                                     series_shape_writer::term_check::against_reference_tenors);
        ores::marketdata::service::observation_lineage_service(tx).save_observation_lineages(
            lineages);

        for (const auto& p : fit->pillars) {
            const auto slash = p.point_id.find('/');
            out.pillars.push_back({.point = p.point_id,
                                   .expiry = p.expiry_id,
                                   .label = p.point_id.substr(slash + 1),
                                   .quoted = p.quoted,
                                   .fitted = p.fitted,
                                   .error = p.error,
                                   .beyond_tolerance = p.beyond_tolerance});
        }
        for (const auto& v : fit->violations)
            out.violations.push_back(
                {.kind = to_string(v.kind), .expiry = v.expiry_id, .description = v.description});
        for (const auto& m : prepared->missing) {
            mm::fx_vol_missing_pillar missing{.point = m.point, .series_ids = {}};
            for (const auto& id : m.series_ids)
                missing.series_ids.push_back(boost::uuids::to_string(id));
            out.missing.push_back(std::move(missing));
        }
        for (const auto& g : fit->grid)
            out.grid.push_back({.expiry = g.expiry_id,
                                .strike = g.strike_id,
                                .log_moneyness = g.log_moneyness,
                                .volatility = g.volatility});

        double max_abs_error = 0.0;
        int beyond = 0;
        for (const auto& p : fit->pillars) {
            max_abs_error = std::max(max_abs_error, std::abs(p.error));
            beyond += p.beyond_tolerance ? 1 : 0;
        }
        domain::fx_vol_surface_calibration record;
        record.tenant_id = ctx.tenant_id();
        record.id = calibration_id_of(blueprint.id, as_of);
        record.blueprint_id = blueprint.id;
        record.as_of = as_of;
        record.party_id = blueprint.party_id;
        record.blueprint_version = blueprint.version;
        record.output_series_id = blueprint.output_series_id;
        record.forward_publish_id = boost::uuids::string_generator{}(forwards.publish_id);
        record.smile_model = blueprint.smile_model;
        record.rho = fit->parameters.rho;
        record.eta = fit->parameters.eta;
        record.gamma = fit->parameters.gamma;
        record.converged = fit->converged;
        record.max_abs_error = max_abs_error;
        record.pillars_beyond_tolerance = beyond;
        record.pillar_fits = ores::utility::codec::json::write(out.pillars);
        record.violations = ores::utility::codec::json::write(out.violations);
        record.missing_pillars = ores::utility::codec::json::write(out.missing);
        record.modified_by = writer;
        record.performed_by = ctx.service_account();
        record.change_reason_code = "system.surface_calibration";
        record.change_commentary = "FX volatility surface calibration of " + blueprint.name;
        repository::fx_vol_surface_calibration_repository{}.write(tx, record);
        uow.commit();

        out.success = true;
        out.message = std::format("Calibrated {} at {} points; the largest pillar error is {:.4f}.",
                                  blueprint.name,
                                  fit->grid.size(),
                                  max_abs_error);
        out.calibration_id = boost::uuids::to_string(record.id);
        out.blueprint_name = blueprint.name;
        out.blueprint_version = blueprint.version;
        out.as_of = as_of;
        out.forward_publish_id = forwards.publish_id;
        out.rho = record.rho;
        out.eta = record.eta;
        out.gamma = record.gamma;
        out.converged = record.converged;
        out.max_abs_error = max_abs_error;
        BOOST_LOG_SEV(calibration_log(), ores::logging::info) << out.message;
    } catch (const std::exception& e) {
        out = {};
        out.success = false;
        out.message = e.what();
        BOOST_LOG_SEV(calibration_log(), ores::logging::warn)
            << "Calibration refused: " << e.what();
    }
    return out;
}

}
