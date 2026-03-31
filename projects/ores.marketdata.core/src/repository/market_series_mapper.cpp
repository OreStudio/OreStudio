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
#include "ores.marketdata.core/repository/market_series_mapper.hpp"

#include <stdexcept>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.marketdata.api/domain/market_series_json_io.hpp" // IWYU pragma: keep.

namespace ores::marketdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

namespace {

std::string to_string(domain::asset_class ac) {
    switch (ac) {
        case domain::asset_class::fx:          return "fx";
        case domain::asset_class::rates:       return "rates";
        case domain::asset_class::credit:      return "credit";
        case domain::asset_class::equity:      return "equity";
        case domain::asset_class::commodity:   return "commodity";
        case domain::asset_class::inflation:   return "inflation";
        case domain::asset_class::bond:        return "bond";
        case domain::asset_class::cross_asset: return "cross_asset";
    }
    throw std::logic_error("Unknown asset_class value");
}

domain::asset_class asset_class_from_string(const std::string& s) {
    if (s == "fx")          return domain::asset_class::fx;
    if (s == "rates")       return domain::asset_class::rates;
    if (s == "credit")      return domain::asset_class::credit;
    if (s == "equity")      return domain::asset_class::equity;
    if (s == "commodity")   return domain::asset_class::commodity;
    if (s == "inflation")   return domain::asset_class::inflation;
    if (s == "bond")        return domain::asset_class::bond;
    if (s == "cross_asset") return domain::asset_class::cross_asset;
    throw std::invalid_argument("Unknown asset_class: " + s);
}

std::string to_string(domain::series_subclass sc) {
    switch (sc) {
        case domain::series_subclass::spot:         return "spot";
        case domain::series_subclass::forward:      return "forward";
        case domain::series_subclass::volatility:   return "volatility";
        case domain::series_subclass::yield:        return "yield";
        case domain::series_subclass::basis:        return "basis";
        case domain::series_subclass::fra:          return "fra";
        case domain::series_subclass::xccy:         return "xccy";
        case domain::series_subclass::spread:       return "spread";
        case domain::series_subclass::index_credit: return "index_credit";
        case domain::series_subclass::recovery:     return "recovery";
        case domain::series_subclass::swap:         return "swap";
        case domain::series_subclass::capfloor:     return "capfloor";
        case domain::series_subclass::seasonality:  return "seasonality";
        case domain::series_subclass::price:        return "price";
        case domain::series_subclass::correlation:  return "correlation";
    }
    throw std::logic_error("Unknown series_subclass value");
}

domain::series_subclass series_subclass_from_string(const std::string& s) {
    if (s == "spot")         return domain::series_subclass::spot;
    if (s == "forward")      return domain::series_subclass::forward;
    if (s == "volatility")   return domain::series_subclass::volatility;
    if (s == "yield")        return domain::series_subclass::yield;
    if (s == "basis")        return domain::series_subclass::basis;
    if (s == "fra")          return domain::series_subclass::fra;
    if (s == "xccy")         return domain::series_subclass::xccy;
    if (s == "spread")       return domain::series_subclass::spread;
    if (s == "index_credit") return domain::series_subclass::index_credit;
    if (s == "recovery")     return domain::series_subclass::recovery;
    if (s == "swap")         return domain::series_subclass::swap;
    if (s == "capfloor")     return domain::series_subclass::capfloor;
    if (s == "seasonality")  return domain::series_subclass::seasonality;
    if (s == "price")        return domain::series_subclass::price;
    if (s == "correlation")  return domain::series_subclass::correlation;
    throw std::invalid_argument("Unknown series_subclass: " + s);
}

} // namespace

domain::market_series
market_series_mapper::map(const market_series_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::market_series r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.series_type = v.series_type;
    r.metric = v.metric;
    r.qualifier = v.qualifier;
    r.asset_class = asset_class_from_string(v.asset_class);
    r.subclass = series_subclass_from_string(v.series_subclass);
    r.is_scalar = v.is_scalar;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    if (!v.valid_from)
        throw std::logic_error("Cannot map entity with null valid_from to domain object.");
    r.recorded_at = timestamp_to_timepoint(*v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

market_series_entity
market_series_mapper::map(const domain::market_series& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    market_series_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.series_type = v.series_type;
    r.metric = v.metric;
    r.qualifier = v.qualifier;
    r.asset_class = to_string(v.asset_class);
    r.series_subclass = to_string(v.subclass);
    r.is_scalar = v.is_scalar;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::market_series>
market_series_mapper::map(const std::vector<market_series_entity>& v) {
    return map_vector<market_series_entity, domain::market_series>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<market_series_entity>
market_series_mapper::map(const std::vector<domain::market_series>& v) {
    return map_vector<domain::market_series, market_series_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
