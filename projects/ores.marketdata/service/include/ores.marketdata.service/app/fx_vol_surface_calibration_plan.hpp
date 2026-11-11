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
#ifndef ORES_MARKETDATA_SERVICE_APP_FX_VOL_SURFACE_CALIBRATION_PLAN_HPP
#define ORES_MARKETDATA_SERVICE_APP_FX_VOL_SURFACE_CALIBRATION_PLAN_HPP

#include "ores.analytics.quant/domain/date_convention.hpp"
#include "ores.analytics.quant/service/ssvi_calibration_engine.hpp"
#include "ores.marketdata.service/export.hpp"
#include "ores.refdata.api/domain/fx_option_convention.hpp"
#include "ores.refdata.api/domain/fx_vol_surface_blueprint.hpp"
#include "ores.refdata.api/domain/fx_vol_surface_pillar.hpp"
#include "ores.refdata.api/domain/fx_vol_surface_pillar_source.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <expected>
#include <map>
#include <optional>
#include <string>
#include <utility>
#include <vector>

/**
 * @brief The pure part of an FX volatility surface calibration: from a blueprint, its pillars,
 * the convention, a published forward set and the quotes, to the engine's request.
 *
 * Nothing here reads the store. The service reads the rows and the quotes and hands them over,
 * so every rule the calibration applies is tested without a database.
 */
namespace ores::marketdata::service::app {

/// The recipe of a calibration: what a blueprint and its convention say, read once.
struct surface_calibration_recipe {
    refdata::domain::fx_vol_surface_blueprint blueprint;
    std::vector<refdata::domain::fx_vol_surface_pillar> pillars;
    std::vector<refdata::domain::fx_vol_surface_pillar_source> sources;
    std::optional<refdata::domain::fx_option_convention> convention;
};

/// One quoted expiry of the recipe, in increasing order.
struct recipe_expiry final {
    std::string code;
    analytics::quant::domain::date_period period;
};

/// One expiry of the output grid.
struct recipe_grid_expiry final {
    std::string code;
    analytics::quant::domain::date_period period;
};

/// A recipe that passed the four checks, with its expiries and its grid read.
struct checked_recipe final {
    std::vector<recipe_expiry> expiries;
    std::vector<recipe_grid_expiry> grid_expiries;
    std::vector<analytics::quant::service::ssvi_grid_strike> grid_strikes;
};

/**
 * @brief Checks a recipe before any quote is read.
 *
 * The four checks of the construction note, in order: the blueprint names a convention that
 * names a butterfly style; every strike label is one the smile model reads; every expiry has the
 * pillars its smile model needs; the expiries are tenors in strictly increasing order and the
 * output grid lies within them. SSVI is the only model fitted. Each refusal names what failed.
 */
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT std::expected<checked_recipe, std::string>
check_recipe(const surface_calibration_recipe& recipe);

/// One tenor of the published forward set: its value date and the discount factors from spot.
struct surface_forward_point final {
    std::string tenor;
    std::chrono::year_month_day value_date{};
    double base_discount = 0.0;
    double quote_discount = 0.0;
};

/// The published forward set a calibration reads its spot and forwards from.
struct surface_forward_set final {
    std::string publish_id;
    double spot = 0.0;
    std::chrono::year_month_day spot_date{};
    std::vector<surface_forward_point> points;
};

/// The quotes of one source series at the as-of, by the expiry and strike label of the datum.
using surface_series_quotes = std::map<std::pair<std::string, std::string>, double>;

/// A pillar no contributor quoted at the as-of.
struct missing_surface_pillar final {
    std::string point;
    std::vector<boost::uuids::uuid> series_ids;
};

/// A source a pillar's quote came from, with the weight it carried.
struct used_surface_source final {
    boost::uuids::uuid series_id;
    std::string contributor;
    double weight = 0.0;
};

/// The engine's request, and what the calibration needs beside it to write the candidate.
struct prepared_surface_calibration final {
    analytics::quant::service::ssvi_request request;
    std::vector<missing_surface_pillar> missing;
    /// The sources of each quoted pillar, by its point, such as 1Y/25RR.
    std::map<std::string, std::vector<used_surface_source>> sources_by_point;
};

/**
 * @brief Builds the engine's request from a checked recipe, the forward set and the quotes.
 *
 * Each expiry's market is the set's spot and its two discount factors at the expiry: the tenor's
 * own row, or a log-linear interpolation between the rows around the expiry's value date. Its
 * time is the expiry's length from the as-of in years of 365 days. A pillar's quote is each
 * quoting contributor's volatility at its weight; a pillar no contributor quoted is reported as
 * missing, and an expiry left short of the pillars SSVI needs is refused, naming the series it
 * could not read.
 */
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT std::expected<prepared_surface_calibration, std::string>
prepare_calibration(const surface_calibration_recipe& recipe,
                    const checked_recipe& checked,
                    const surface_forward_set& forwards,
                    const std::map<boost::uuids::uuid, surface_series_quotes>& quotes,
                    std::chrono::year_month_day as_of,
                    double pillar_tolerance);

/// The day a period after @p from falls on, unadjusted, the end of a short month clamped.
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT std::chrono::year_month_day
add_period(std::chrono::year_month_day from, analytics::quant::domain::date_period period);

}

#endif
