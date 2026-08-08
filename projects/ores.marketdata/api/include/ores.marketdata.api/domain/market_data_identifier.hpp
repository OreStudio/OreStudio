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
#ifndef ORES_MARKETDATA_API_DOMAIN_MARKET_DATA_IDENTIFIER_HPP
#define ORES_MARKETDATA_API_DOMAIN_MARKET_DATA_IDENTIFIER_HPP

#include "ores.marketdata.api/domain/oresmd_enums.hpp"
#include <optional>
#include <string>
#include <variant>

namespace ores::marketdata::domain {

/**
 * @brief A fully-resolved oresmd identifier for an FX pair (asset_class=fx).
 *
 * `pair` is the 6-character currency pair carried in the URI's `entity` path segment
 * (e.g. "EURUSD"); there is no `ccy`/`index`/`tenor`/`role` member at all, mirroring the
 * query-parameter grammar's own per-asset-class conditionality at the type level -- see
 * id:C3E053CA-0D4B-480B-9119-E11530160EC1, "Data model".
 */
struct fx_market_data_identifier final {
    std::string pair;
    instrument_type type = instrument_type::quote;
    std::optional<domain::fx_quote_type> quote_type;
    std::optional<std::string> point;

    bool operator==(const fx_market_data_identifier&) const = default;
};

/**
 * @brief A fully-resolved oresmd identifier for an interest-rate instrument (asset_class=ir).
 *
 * Only `ccy` and `type` are unconditionally mandatory; every other field is optional
 * because which fields are meaningful depends on `type` (a fixing/curve needs
 * `index`/`tenor`/`role` but no `point`; a quote needs `point` and, for `type=quote`
 * specifically, `metric`) -- see "Tenor vs. point, and the par-rate/discount-factor
 * ambiguity" in the design doc.
 */
struct ir_market_data_identifier final {
    std::string ccy;
    instrument_type type = instrument_type::quote;
    std::optional<index_family> index;
    std::optional<std::string> tenor;
    std::optional<curve_role> role;
    std::optional<domain::metric> metric;
    std::optional<domain::ir_quote_type> quote_type;
    std::optional<std::string> point;

    bool operator==(const ir_market_data_identifier&) const = default;
};

/**
 * @brief A fully-resolved oresmd identifier for an equity/index instrument (asset_class=equity).
 *
 * `ccy` is required here (unlike IR, where `entity` already is the currency) since an
 * equity's `entity` is a ticker -- the quote currency is independent information.
 */
struct equity_market_data_identifier final {
    std::string ticker;
    std::string ccy;
    instrument_type type = instrument_type::quote;
    std::optional<domain::equity_quote_type> quote_type;
    std::optional<std::string> point;

    bool operator==(const equity_market_data_identifier&) const = default;
};

/**
 * @brief A fully-resolved oresmd identifier for a credit instrument (asset_class=credit).
 *
 * `point` carries both the seniority and tenor dimensions (e.g. "sr,5y"), since credit's
 * quote-key shape has one more dimension than IR's.
 */
struct credit_market_data_identifier final {
    std::string reference_entity;
    std::string ccy;
    instrument_type type = instrument_type::quote;
    std::optional<domain::credit_quote_type> quote_type;
    std::optional<std::string> point;

    bool operator==(const credit_market_data_identifier&) const = default;
};

/**
 * @brief A fully-resolved oresmd identifier for a commodity instrument (asset_class=commodity).
 */
struct commodity_market_data_identifier final {
    std::string commodity_code;
    std::string ccy;
    instrument_type type = instrument_type::quote;
    std::optional<domain::commodity_quote_type> quote_type;
    std::optional<std::string> point;

    bool operator==(const commodity_market_data_identifier&) const = default;
};

/**
 * @brief A fully-resolved oresmd identifier for an inflation instrument (asset_class=inflation).
 */
struct inflation_market_data_identifier final {
    std::string index_code;
    instrument_type type = instrument_type::quote;
    std::optional<domain::inflation_quote_type> quote_type;
    std::optional<std::string> point;

    bool operator==(const inflation_market_data_identifier&) const = default;
};

/**
 * @brief A fully-resolved oresmd identifier for a pairwise correlation (asset_class=correlation).
 */
struct correlation_market_data_identifier final {
    std::string factor_pair;
    instrument_type type = instrument_type::quote;
    std::optional<domain::correlation_quote_type> quote_type;

    bool operator==(const correlation_market_data_identifier&) const = default;
};

/**
 * @brief Tagged union of the seven per-asset-class identifier structs.
 *
 * Deliberately *not* a common base class with virtual dispatch: the URI's `asset_class`
 * authority component already tells a consumer which concrete struct applies, and
 * reflection-based serialisation (`rfl`/reflect-cpp) handles plain structs and
 * `std::variant` well but not polymorphic hierarchies -- see
 * id:C3E053CA-0D4B-480B-9119-E11530160EC1, "Data model".
 */
using market_data_identifier = std::variant<fx_market_data_identifier,
                                            ir_market_data_identifier,
                                            equity_market_data_identifier,
                                            credit_market_data_identifier,
                                            commodity_market_data_identifier,
                                            inflation_market_data_identifier,
                                            correlation_market_data_identifier>;

}

#endif
