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

#include <map>
#include <sstream>
#include <stdexcept>
#include <tuple>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.marketdata.api/domain/asset_class.hpp"
#include "ores.marketdata.api/domain/series_subclass.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.api/domain/market_fixing.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/repository/market_observations_repository.hpp"
#include "ores.marketdata.core/repository/market_fixings_repository.hpp"
#include "ores.ore/market/market_data_parser.hpp"
#include "ores.ore/market/fixing.hpp"

namespace ores::marketdata::service {

namespace {

struct series_classification {
    domain::asset_class asset_class;
    domain::series_subclass subclass;
    bool is_scalar;
};

// Maps ORE series_type → classification.
// Entries absent from this map default to rates/yield/non-scalar.
series_classification classify_series_type(const std::string& series_type) {
    using ac = domain::asset_class;
    using sc = domain::series_subclass;

    static const std::map<std::string, series_classification> k_table = {
        // FX
        {"FX",               {ac::fx,          sc::spot,        true}},
        {"FXFWD",            {ac::fx,          sc::forward,     false}},
        {"FX_OPTION",        {ac::fx,          sc::volatility,  false}},
        // Rates curves
        {"DISCOUNT",         {ac::rates,       sc::yield,       false}},
        {"ZERO",             {ac::rates,       sc::yield,       false}},
        {"MM",               {ac::rates,       sc::yield,       false}},
        {"MM_FUTURE",        {ac::rates,       sc::fra,         false}},
        {"FRA",              {ac::rates,       sc::fra,         false}},
        {"IMM_FRA",          {ac::rates,       sc::fra,         false}},
        {"IR_SWAP",          {ac::rates,       sc::yield,       false}},
        // Rates spreads
        {"BASIS_SWAP",       {ac::rates,       sc::basis,       false}},
        {"BMA_SWAP",         {ac::rates,       sc::basis,       false}},
        {"CC_BASIS_SWAP",    {ac::rates,       sc::xccy,        false}},
        {"CC_FIX_FLOAT_SWAP",{ac::rates,       sc::xccy,        false}},
        // Rates vols
        {"SWAPTION",         {ac::rates,       sc::volatility,  false}},
        {"CAPFLOOR",         {ac::rates,       sc::volatility,  false}},
        // Credit
        {"HAZARD_RATE",      {ac::credit,      sc::spread,      false}},
        {"CDS",              {ac::credit,      sc::spread,      false}},
        {"CDS_INDEX",        {ac::credit,      sc::index_credit,false}},
        {"INDEX_CDS_OPTION", {ac::credit,      sc::index_credit,false}},
        {"RECOVERY_RATE",    {ac::credit,      sc::recovery,    true}},
        // Equity
        {"EQUITY",           {ac::equity,      sc::spot,        true}},
        {"EQUITY_FWD",       {ac::equity,      sc::forward,     false}},
        {"EQUITY_DIVIDEND",  {ac::equity,      sc::forward,     false}},
        {"EQUITY_OPTION",    {ac::equity,      sc::volatility,  false}},
        // Commodity
        {"COMMODITY",        {ac::commodity,   sc::spot,        true}},
        {"COMMODITY_FWD",    {ac::commodity,   sc::forward,     false}},
        {"COMMODITY_OPTION", {ac::commodity,   sc::volatility,  false}},
        // Inflation
        {"ZC_INFLATIONSWAP",     {ac::inflation, sc::swap,       false}},
        {"YY_INFLATIONSWAP",     {ac::inflation, sc::swap,       false}},
        {"ZC_INFLATIONCAPFLOOR", {ac::inflation, sc::capfloor,   false}},
        {"YY_INFLATIONCAPFLOOR", {ac::inflation, sc::capfloor,   false}},
        {"SEASONALITY",          {ac::inflation, sc::seasonality, false}},
        // Bond
        {"BOND",             {ac::bond,        sc::price,       false}},
        // Cross-asset
        {"CORRELATION",      {ac::cross_asset, sc::correlation, true}},
        // Fixings (index series)
        {"FIXING",           {ac::rates,       sc::yield,       true}},
    };

    const auto it = k_table.find(series_type);
    if (it != k_table.end())
        return it->second;
    throw std::invalid_argument("Unknown ORE series_type: " + series_type);
}

} // namespace

import_service::import_service(context ctx) : ctx_(std::move(ctx)) {}

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
        const auto existing = series_repo.read_latest_by_type(
            ctx_, series_type, metric, qualifier);
        if (!existing.empty()) {
            series_cache.emplace(key, existing.front().id);
            return existing.front().id;
        }

        // Create a new series.
        const auto cl = classify_series_type(series_type);
        domain::market_series s;
        s.id = gen();
        s.tenant_id = ctx_.tenant_id();
        s.series_type = series_type;
        s.metric = metric;
        s.qualifier = qualifier;
        s.asset_class = cl.asset_class;
        s.subclass = cl.subclass;
        s.is_scalar = cl.is_scalar;
        s.modified_by = "import";
        s.performed_by = "import";
        s.change_reason_code = "IMPORT";
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
            const auto sid = find_or_create_series(
                d.series_type, d.metric, d.qualifier);

            domain::market_observation obs;
            obs.id = gen();
            obs.tenant_id = ctx_.tenant_id();
            obs.series_id = sid;
            obs.observation_date = d.date;
            obs.point_id = d.point_id;
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
            fix.series_id = sid;
            fix.fixing_date = f.date;
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
