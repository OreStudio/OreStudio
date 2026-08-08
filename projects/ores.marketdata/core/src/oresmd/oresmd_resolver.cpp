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
#include "ores.marketdata.core/oresmd/oresmd_resolver.hpp"
#include "ores.marketdata.core/oresmd/oresmd_exception.hpp"
#include <boost/throw_exception.hpp>
#include <format>

namespace {

using namespace ores::marketdata::domain;
using ores::marketdata::core::oresmd_exception;

template <typename T>
T pick(const std::optional<T>& required, const std::optional<T>& fallback, std::string_view field) {
    if (required)
        return *required;
    if (fallback)
        return *fallback;
    BOOST_THROW_EXCEPTION(
        oresmd_exception(std::format("oresmd resolve: '{}' is unresolved.", field)));
}

template <typename T>
std::optional<T> pick_optional(const std::optional<T>& required, const std::optional<T>& fallback) {
    return required ? required : fallback;
}

/**
 * @brief Like pick(), but for the mandatory string fields (pair/ccy/ticker/...) that have
 * no sensible non-empty default -- an identifier struct's mandatory std::string members
 * are never themselves optional, so a caller passing an unfilled `defaults` identifier
 * has that field default-constructed to "", not "absent". Treating "" as absent here,
 * on both the requirement and the fallback, is what lets resolve() actually detect an
 * unresolved mandatory field instead of silently resolving it to an empty string.
 */
std::string pick_mandatory_string(const std::optional<std::string>& required,
                                  const std::string& fallback,
                                  std::string_view field) {
    if (required && !required->empty())
        return *required;
    if (!fallback.empty())
        return fallback;
    BOOST_THROW_EXCEPTION(
        oresmd_exception(std::format("oresmd resolve: '{}' is unresolved.", field)));
}

market_data_identifier resolve_fx(const fx_market_data_requirement& req,
                                  const market_data_identifier& defaults) {
    const auto* d = std::get_if<fx_market_data_identifier>(&defaults);
    fx_market_data_identifier id;
    id.pair = pick_mandatory_string(req.pair, d ? d->pair : std::string{}, "pair");
    id.type = pick(req.type, d ? std::optional(d->type) : std::nullopt, "type");
    return id;
}

market_data_identifier resolve_ir(const ir_market_data_requirement& req,
                                  const market_data_identifier& defaults) {
    const auto* d = std::get_if<ir_market_data_identifier>(&defaults);
    ir_market_data_identifier id;
    id.ccy = pick_mandatory_string(req.ccy, d ? d->ccy : std::string{}, "ccy");
    id.type = pick(req.type, d ? std::optional(d->type) : std::nullopt, "type");
    id.index = pick_optional(req.index, d ? d->index : std::nullopt);
    id.tenor = pick_optional(req.tenor, d ? d->tenor : std::nullopt);
    id.role = pick_optional(req.role, d ? d->role : std::nullopt);
    id.metric = pick_optional(req.metric, d ? d->metric : std::nullopt);
    id.quote_type = pick_optional(req.quote_type, d ? d->quote_type : std::nullopt);
    id.point = pick_optional(req.point, d ? d->point : std::nullopt);
    return id;
}

market_data_identifier resolve_equity(const equity_market_data_requirement& req,
                                      const market_data_identifier& defaults) {
    const auto* d = std::get_if<equity_market_data_identifier>(&defaults);
    equity_market_data_identifier id;
    id.ticker = pick_mandatory_string(req.ticker, d ? d->ticker : std::string{}, "ticker");
    id.ccy = pick_mandatory_string(req.ccy, d ? d->ccy : std::string{}, "ccy");
    id.type = pick(req.type, d ? std::optional(d->type) : std::nullopt, "type");
    id.quote_type = pick_optional(req.quote_type, d ? d->quote_type : std::nullopt);
    id.point = pick_optional(req.point, d ? d->point : std::nullopt);
    return id;
}

market_data_identifier resolve_credit(const credit_market_data_requirement& req,
                                      const market_data_identifier& defaults) {
    const auto* d = std::get_if<credit_market_data_identifier>(&defaults);
    credit_market_data_identifier id;
    id.reference_entity = pick_mandatory_string(
        req.reference_entity, d ? d->reference_entity : std::string{}, "reference_entity");
    id.ccy = pick_mandatory_string(req.ccy, d ? d->ccy : std::string{}, "ccy");
    id.type = pick(req.type, d ? std::optional(d->type) : std::nullopt, "type");
    id.quote_type = pick_optional(req.quote_type, d ? d->quote_type : std::nullopt);
    id.point = pick_optional(req.point, d ? d->point : std::nullopt);
    return id;
}

market_data_identifier resolve_commodity(const commodity_market_data_requirement& req,
                                         const market_data_identifier& defaults) {
    const auto* d = std::get_if<commodity_market_data_identifier>(&defaults);
    commodity_market_data_identifier id;
    id.commodity_code = pick_mandatory_string(
        req.commodity_code, d ? d->commodity_code : std::string{}, "commodity_code");
    id.ccy = pick_mandatory_string(req.ccy, d ? d->ccy : std::string{}, "ccy");
    id.type = pick(req.type, d ? std::optional(d->type) : std::nullopt, "type");
    id.quote_type = pick_optional(req.quote_type, d ? d->quote_type : std::nullopt);
    id.point = pick_optional(req.point, d ? d->point : std::nullopt);
    return id;
}

}

namespace ores::marketdata::core {

domain::market_data_identifier
oresmd_resolver::resolve(const domain::market_data_requirement& requirement,
                         const domain::market_data_identifier& defaults) {
    return std::visit(
        [&defaults](const auto& req) -> domain::market_data_identifier {
            using T = std::decay_t<decltype(req)>;
            if constexpr (std::is_same_v<T, fx_market_data_requirement>)
                return resolve_fx(req, defaults);
            else if constexpr (std::is_same_v<T, ir_market_data_requirement>)
                return resolve_ir(req, defaults);
            else if constexpr (std::is_same_v<T, equity_market_data_requirement>)
                return resolve_equity(req, defaults);
            else if constexpr (std::is_same_v<T, credit_market_data_requirement>)
                return resolve_credit(req, defaults);
            else
                return resolve_commodity(req, defaults);
        },
        requirement);
}

}
