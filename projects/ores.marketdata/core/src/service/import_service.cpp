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
#include "ores.marketdata.core/service/import_service.hpp"
#include "ores.dq.api/domain/change_reason_constants.hpp"
#include "ores.marketdata.api/domain/market_fixing.hpp"
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.core/repository/market_fixings_repository.hpp"
#include "ores.marketdata.core/repository/market_observations_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.ore.core/market/fixing.hpp"
#include "ores.ore.core/market/market_data_parser.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <map>
#include <rfl/enums.hpp>
#include <sstream>
#include <stdexcept>
#include <tuple>

namespace ores::marketdata::service {

namespace {

struct series_classification {
    std::string asset_class;
    std::string series_subclass;
    bool is_scalar;
};

// Maps ORE series_type → classification.
series_classification classify_series_type(const std::string& series_type) {
    static const std::map<std::string, series_classification> k_table = {
        // FX
        {"FX", {"fx", "spot", true}},
        {"FXFWD", {"fx", "forward", false}},
        {"FX_OPTION", {"fx", "volatility", false}},
        // Rates curves
        {"DISCOUNT", {"rates", "yield", false}},
        {"ZERO", {"rates", "yield", false}},
        {"MM", {"rates", "yield", false}},
        {"MM_FUTURE", {"rates", "fra", false}},
        {"FRA", {"rates", "fra", false}},
        {"IMM_FRA", {"rates", "fra", false}},
        {"IR_SWAP", {"rates", "yield", false}},
        // Rates spreads
        {"BASIS_SWAP", {"rates", "basis", false}},
        {"BMA_SWAP", {"rates", "basis", false}},
        {"CC_BASIS_SWAP", {"rates", "xccy", false}},
        {"CC_FIX_FLOAT_SWAP", {"rates", "xccy", false}},
        // Rates vols
        {"SWAPTION", {"rates", "volatility", false}},
        {"CAPFLOOR", {"rates", "volatility", false}},
        // Credit
        {"HAZARD_RATE", {"credit", "spread", false}},
        {"CDS", {"credit", "spread", false}},
        {"CDS_INDEX", {"credit", "index_credit", false}},
        {"INDEX_CDS_OPTION", {"credit", "index_credit", false}},
        {"RECOVERY_RATE", {"credit", "recovery", true}},
        // Equity
        {"EQUITY", {"equity", "spot", true}},
        {"EQUITY_FWD", {"equity", "forward", false}},
        {"EQUITY_DIVIDEND", {"equity", "forward", false}},
        {"EQUITY_OPTION", {"equity", "volatility", false}},
        // Commodity
        {"COMMODITY", {"commodity", "spot", true}},
        {"COMMODITY_FWD", {"commodity", "forward", false}},
        {"COMMODITY_OPTION", {"commodity", "volatility", false}},
        // Inflation
        {"ZC_INFLATIONSWAP", {"inflation", "swap", false}},
        {"YY_INFLATIONSWAP", {"inflation", "swap", false}},
        {"ZC_INFLATIONCAPFLOOR", {"inflation", "capfloor", false}},
        {"YY_INFLATIONCAPFLOOR", {"inflation", "capfloor", false}},
        {"SEASONALITY", {"inflation", "seasonality", false}},
        // Bond
        {"BOND", {"bond", "price", false}},
        // Cross-asset
        {"CORRELATION", {"cross_asset", "correlation", true}},
        // Fixings (index series)
        {"FIXING", {"rates", "yield", true}},
    };

    const auto it = k_table.find(series_type);
    if (it != k_table.end())
        return it->second;
    throw std::invalid_argument("Unknown ORE series_type: " + series_type);
}

} // namespace

import_service::import_service(context ctx)
    : ctx_(std::move(ctx)) {}

messaging::import_market_data_response
import_service::import(const messaging::import_market_data_request& req) {
    messaging::import_market_data_response resp;
    boost::uuids::random_generator gen;
    repository::market_series_repository series_repo;
    repository::market_observations_repository obs_repo;
    repository::market_fixings_repository fixings_repo;

    // Cache: (series_type, metric, qualifier) → series.id
    using SeriesKey = std::tuple<std::string, std::string, std::string>;
    std::map<SeriesKey, boost::uuids::uuid> series_cache;

    auto find_or_create_series = [&](const std::string& series_type,
                                     const std::string& metric,
                                     const std::string& qualifier) -> boost::uuids::uuid {
        const auto key = std::make_tuple(series_type, metric, qualifier);
        const auto it = series_cache.find(key);
        if (it != series_cache.end())
            return it->second;

        // Look up existing series in DB.
        const auto existing = series_repo.read_latest_by_type(ctx_, series_type, metric, qualifier);
        if (!existing.empty()) {
            series_cache.emplace(key, existing.front().id);
            return existing.front().id;
        }

        // Create a new series.
        const auto cl = classify_series_type(series_type);
        domain::market_series s;
        s.id = gen();
        s.tenant_id = ctx_.tenant_id();
        s.party_id = ctx_.party_id().value_or(boost::uuids::uuid{});
        s.series_type = series_type;
        s.metric = metric;
        s.qualifier = qualifier;
        s.asset_class = rfl::string_to_enum<domain::asset_class>(cl.asset_class).value();
        s.series_subclass =
            rfl::string_to_enum<domain::series_subclass>(cl.series_subclass).value();
        s.is_scalar = cl.is_scalar;
        s.modified_by = ctx_.actor();
        s.performed_by = ctx_.service_account();
        s.change_reason_code =
            std::string(dq::domain::change_reason_constants::codes::external_data_import);
        s.change_commentary = "Imported from ORE market data file";
        series_repo.write(ctx_, s);

        series_cache.emplace(key, s.id);
        ++resp.series_count;
        return s.id;
    };

    // ── Import market observations ────────────────────────────────────────────
    if (!req.market_data_content.empty()) {
        std::istringstream in(req.market_data_content);
        const auto data = ores::ore::market::parse_market_data(in);

        std::vector<domain::market_observation> observations;
        observations.reserve(data.size());

        for (const auto& d : data) {
            const auto sid = find_or_create_series(d.series_type, d.metric, d.qualifier);

            domain::market_observation obs;
            obs.id = gen();
            obs.tenant_id = ctx_.tenant_id();
            obs.party_id = ctx_.party_id().value_or(boost::uuids::uuid{});
            obs.series_id = sid;
            obs.observation_datetime = std::chrono::sys_days{d.date};
            obs.point_id = d.point_id.value_or("");
            obs.source = req.source;
            obs.value = d.value;
            observations.push_back(std::move(obs));
        }

        obs_repo.write(ctx_, observations);
        resp.observation_count = static_cast<int>(observations.size());
    }

    // ── Import fixings ────────────────────────────────────────────────────────
    if (!req.fixings_content.empty()) {
        std::istringstream in(req.fixings_content);
        const auto data = ores::ore::market::parse_fixings(in);

        std::vector<domain::market_fixing> fixings;
        fixings.reserve(data.size());

        for (const auto& f : data) {
            // Fixing series: series_type=FIXING, metric=RATE, qualifier=index_name
            const auto sid = find_or_create_series("FIXING", "RATE", f.qualifier);

            domain::market_fixing fix;
            fix.id = gen();
            fix.tenant_id = ctx_.tenant_id();
            fix.party_id = ctx_.party_id().value_or(boost::uuids::uuid{});
            fix.series_id = sid;
            fix.fixing_date = f.date;
            fix.source = req.source;
            fix.value = f.value;
            fixings.push_back(std::move(fix));
        }

        fixings_repo.write(ctx_, fixings);
        resp.fixing_count = static_cast<int>(fixings.size());
    }

    resp.success = true;
    return resp;
}

}
