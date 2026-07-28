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
#include "ores.marketdata.core/oresmd/oresmd_projections.hpp"
#include <algorithm>
#include <format>
#include <magic_enum/magic_enum.hpp>
#include <sstream>
#include <vector>

namespace {

using namespace ores::marketdata::domain;

std::string to_upper(std::string_view s) {
    std::string r(s);
    std::ranges::transform(r, r.begin(), [](unsigned char c) { return std::toupper(c); });
    return r;
}

std::vector<std::string> split_point(const std::string& point) {
    std::vector<std::string> parts;
    std::stringstream ss(point);
    std::string part;
    while (std::getline(ss, part, ','))
        parts.push_back(to_upper(part));
    return parts;
}

bool is_overnight(index_family f) {
    return f == index_family::sofr || f == index_family::estr || f == index_family::sonia ||
           f == index_family::tona;
}

std::string curve_id(std::string_view ccy, std::string_view tenor) {
    return std::format("{}{}", ccy, to_upper(tenor));
}

/*
 * Both index name and curve key are produced together, gated on `type=fixing` -- the
 * design doc's own worked-examples table shows a single `type=fixing` URI producing
 * BOTH an index name and a curve key (the underlying curve construct has both facets),
 * and reads "--" for both columns on every `type=quote` row. See
 * id:C3E053CA-0D4B-480B-9119-E11530160EC1, "Worked examples" > "Interest rates".
 */
std::optional<std::string> index_name_ir(const ir_market_data_identifier& id) {
    if (id.type != instrument_type::fixing || !id.index)
        return std::nullopt;
    const auto family = to_upper(std::string(magic_enum::enum_name(*id.index)));
    if (is_overnight(*id.index) || !id.tenor)
        return std::format("{}-{}", id.ccy, family);
    return std::format("{}-{}-{}", id.ccy, family, to_upper(*id.tenor));
}

std::optional<std::string> curve_key_ir(const ir_market_data_identifier& id) {
    if (id.type != instrument_type::fixing || !id.tenor)
        return std::nullopt;
    return std::format("Yield/{}/{}", id.ccy, curve_id(id.ccy, *id.tenor));
}

std::optional<std::string> quote_key_ir(const ir_market_data_identifier& id) {
    if (id.type == instrument_type::vol) {
        if (!id.point)
            return std::nullopt;
        const auto parts = split_point(*id.point);
        if (parts.size() != 3)
            return std::nullopt;
        return std::format("SWAPTION/RATE_LNVOL/{}/{}/{}/{}", id.ccy, parts[0], parts[1], parts[2]);
    }
    if (id.type != instrument_type::quote || !id.metric || !id.point || !id.tenor)
        return std::nullopt;
    const auto point = to_upper(*id.point);
    if (*id.metric == metric::par_rate)
        return std::format("IR_SWAP/RATE/{}/2D/{}/{}", id.ccy, to_upper(*id.tenor), point);
    return std::format("DISCOUNT/RATE/{}/{}/{}", id.ccy, curve_id(id.ccy, *id.tenor), point);
}

std::optional<std::string> quote_key_fx(const fx_market_data_identifier& id) {
    if (id.type != instrument_type::quote || id.pair.size() != 6)
        return std::nullopt;
    return std::format("FX/RATE/{}/{}", id.pair.substr(0, 3), id.pair.substr(3, 3));
}

std::optional<std::string> quote_key_equity(const equity_market_data_identifier& id) {
    if (id.type != instrument_type::quote)
        return std::nullopt;
    return std::format("EQUITY/PRICE/{}/{}", id.ticker, id.ccy);
}

std::optional<std::string> quote_key_credit(const credit_market_data_identifier& id) {
    if (id.type != instrument_type::quote || !id.point)
        return std::nullopt;
    const auto parts = split_point(*id.point);
    if (parts.size() != 2)
        return std::nullopt;
    return std::format(
        "CDS/CREDIT_SPREAD/{}/{}/{}/{}", id.reference_entity, parts[0], id.ccy, parts[1]);
}

std::optional<std::string> quote_key_commodity(const commodity_market_data_identifier& id) {
    if (id.type != instrument_type::quote)
        return std::nullopt;
    return std::format("COMMODITY/PRICE/{}/{}", id.commodity_code, id.ccy);
}

}

namespace ores::marketdata::core {

std::optional<std::string>
oresmd_projections::to_index_name(const domain::market_data_identifier& identifier) {
    if (const auto* ir = std::get_if<ir_market_data_identifier>(&identifier))
        return index_name_ir(*ir);
    return std::nullopt;
}

std::optional<std::string>
oresmd_projections::to_curve_key(const domain::market_data_identifier& identifier) {
    if (const auto* ir = std::get_if<ir_market_data_identifier>(&identifier))
        return curve_key_ir(*ir);
    return std::nullopt;
}

std::optional<std::string>
oresmd_projections::to_quote_key(const domain::market_data_identifier& identifier) {
    return std::visit(
        [](const auto& id) -> std::optional<std::string> {
            using T = std::decay_t<decltype(id)>;
            if constexpr (std::is_same_v<T, fx_market_data_identifier>)
                return quote_key_fx(id);
            else if constexpr (std::is_same_v<T, ir_market_data_identifier>)
                return quote_key_ir(id);
            else if constexpr (std::is_same_v<T, equity_market_data_identifier>)
                return quote_key_equity(id);
            else if constexpr (std::is_same_v<T, credit_market_data_identifier>)
                return quote_key_credit(id);
            else
                return quote_key_commodity(id);
        },
        identifier);
}

}
