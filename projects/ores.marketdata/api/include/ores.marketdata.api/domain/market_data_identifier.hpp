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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: oresmd_identifiers.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_MARKETDATA_API_DOMAIN_MARKET_DATA_IDENTIFIER_HPP
#define ORES_MARKETDATA_API_DOMAIN_MARKET_DATA_IDENTIFIER_HPP

#include "ores.marketdata.api/domain/oresmd_enums.hpp"
#include <optional>
#include <string>
#include <variant>

namespace ores::marketdata::domain {

/**
 * @brief Volatility surface point: expiry, strike, and model subtype — the three dimensions
 * every ORE volatility sub-family shares — plus the option convention the listed ones add.
 *
 * The equity and commodity option families write more than a coordinate: a call/put flag,
 * or a delta convention that puts a delta type in place of the strike and names where the
 * premium is read. Those are optional because the rate, inflation and credit surfaces do
 * not carry them. Only meaningful when `type=vol`.
 */
struct volatility_surface_point final {
    std::string expiry;
    std::string strike;
    volatility_model_subtype model_subtype = volatility_model_subtype::rate_lnvol;
    /**
     * @brief The option's call/put flag, or the delta convention's own side.
     */
    std::optional<std::string> call_put;
    /**
     * @brief What the key quotes when the strike slot holds a delta: DEL, ATM or ATMF,
     * the risk-reversal and butterfly conventions, and the swaption family's Smile,
     * whose shift then takes the value's place.
     */
    std::optional<std::string> delta_type;
    /**
     * @brief Where the premium is read -- Spot, Fwd, or unset.
     */
    std::optional<std::string> premium_type;

    bool operator==(const volatility_surface_point&) const = default;
};

/**
 * @brief A fully-resolved oresmd identifier for an FX pair (asset_class=fx).
 *
 * `pair` is the 6-character currency pair carried in the URI's `entity` path segment
 * (e.g. "EURUSD"); there is no `ccy`/`index`/`tenor`/`role` member at all, mirroring the
 * query-parameter grammar's own per-asset-class conditionality at the type level -- see
 * id:C3E053CA-0D4B-480B-9119-E11530160EC1, "Data model".
 *
 * A fixing also carries the source that published it, because one pair has more than
 * one fixing: the corpus writes both FX-ECB-EUR-USD and FX-TR20H-EUR-USD, which
 * are two different rates. `source` is that token lower-cased, and `source_spelling`
 * carries the token as ORE wrote it when the two differ, the way `index_spelling`
 * carries a family's other spelling. Neither belongs to a quote: ORE's FX quote keys
 * carry no source, and the corpus writes the token only in index names -- a fixing
 * file, or the qualifier of a correlation key.
 *
 * The pair is the pair as published, never a canonical order. A fixing's value is
 * only meaningful in the direction it was published, and the corpus writes
 * FX-TR20H-EUR-GBP and FX-TR20H-GBP-EUR as one rate both ways: reciprocal on all
 * 122 dates they share, the product of the two being 0.9997 on the median date and
 * 0.9964 on the worst. Folding them together would put two values under one date.
 */
struct fx_market_data_identifier final {
    std::string pair;
    instrument_type type = instrument_type::quote;
    std::optional<std::string> source;
    std::optional<std::string> source_spelling;
    std::optional<domain::fx_quote_type> quote_type;
    std::optional<std::string> point;
    std::optional<volatility_surface_point> vol;

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
    std::optional<std::string> index_spelling;
    std::optional<std::string> tenor;
    std::optional<std::string> second_tenor;
    std::optional<std::string> second_ccy;
    std::optional<std::string> contract_month;
    std::optional<std::string> contract_code;
    std::optional<std::string> shift;
    std::optional<std::string> strip;
    std::optional<std::string> curve_id;
    std::optional<std::string> settle;
    std::optional<std::string> day_count;
    std::optional<curve_role> role;
    std::optional<domain::metric> metric;
    std::optional<domain::ir_quote_type> quote_type;
    std::optional<std::string> point;
    std::optional<volatility_surface_point> vol;

    bool operator==(const ir_market_data_identifier&) const = default;
};

/**
 * @brief A fully-resolved oresmd identifier for an equity/index instrument (asset_class=equity).
 *
 * `ccy` is a quote's to give, and a fixing's is not: a quote is a price and has to say
 * what it is denominated in, while a fixing's key is an index name -- EQ-SP5 is the
 * level of an index, not a price in a currency. The field is therefore optional in the
 * type and the parser requires it for every `type` except `fixing`. Unlike IR, where
 * `entity` already is the currency, an equity's `entity` is a ticker, so this is
 * independent information rather than a repeat of the path segment.
 */
struct equity_market_data_identifier final {
    std::string ticker;
    std::optional<std::string> ccy;
    instrument_type type = instrument_type::quote;
    std::optional<domain::equity_quote_type> quote_type;
    std::optional<std::string> point;
    std::optional<volatility_surface_point> vol;

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
    std::optional<volatility_surface_point> vol;

    bool operator==(const credit_market_data_identifier&) const = default;
};

/**
 * @brief A fully-resolved oresmd identifier for a commodity instrument (asset_class=commodity).
 *
 * delivery is the contract period a *fixing* is specific to: the corpus writes
 * COMM-NYMEX:CL-2025-04, observed on 2024-12-02, so the period in the name is the
 * delivery period and not the observation date. It holds the contract month as the
 * index name writes it (2025-04). ORE's commodity parser also reads a contract
 * code that itself contains a dash and a full delivery date; neither is in the
 * corpus, so neither is admitted here rather than guessed at.
 *
 * A quote carries its own period in point instead, because a quote's observations
 * each name the period they are for and a fixing's do not. This class's validation
 * is the shared one that the three delegating classes use, and it gates no query key
 * by type, so a delivery on a quote is accepted and projects no index name, the way
 * a point on a fixing is. Closing that gate for equity, credit and commodity is the
 * codegen gap recorded in the fixing-identity task.
 */
struct commodity_market_data_identifier final {
    std::string commodity_code;
    std::optional<std::string> ccy;
    instrument_type type = instrument_type::quote;
    std::optional<std::string> delivery;
    std::optional<domain::commodity_quote_type> quote_type;
    std::optional<std::string> point;
    std::optional<volatility_surface_point> vol;

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
    std::optional<volatility_surface_point> vol;

    bool operator==(const inflation_market_data_identifier&) const = default;
};

/**
 * @brief A fully-resolved oresmd identifier for a pairwise correlation (asset_class=correlation).
 */
struct correlation_market_data_identifier final {
    std::string factor_pair;
    instrument_type type = instrument_type::quote;
    std::optional<std::string> second_factor;
    std::optional<domain::correlation_quote_type> quote_type;
    std::optional<std::string> point;

    bool operator==(const correlation_market_data_identifier&) const = default;
};

/**
 * @brief A fully-resolved oresmd identifier for a security (asset_class=security).
 */
struct security_market_data_identifier final {
    std::string security_id;
    instrument_type type = instrument_type::quote;
    std::optional<domain::security_quote_type> quote_type;

    bool operator==(const security_market_data_identifier&) const = default;
};

/**
 * @brief A fully-resolved oresmd identifier for a shape profile (asset_class=shape_profile).
 */
struct shape_profile_market_data_identifier final {
    std::string profile_id;
    instrument_type type = instrument_type::quote;
    std::optional<domain::shape_profile_quote_type> quote_type;
    std::optional<std::string> point;

    bool operator==(const shape_profile_market_data_identifier&) const = default;
};

/**
 * @brief A fully-resolved oresmd identifier for a rating provider (asset_class=rating).
 */
struct rating_market_data_identifier final {
    std::string provider_id;
    instrument_type type = instrument_type::quote;
    std::optional<domain::rating_quote_type> quote_type;
    std::optional<std::string> point;

    bool operator==(const rating_market_data_identifier&) const = default;
};

/**
 * @brief A fully-resolved oresmd identifier for an intraday power index (asset_class=power).
 *
 * commodity_code is the ORE commodity name the index is written against --
 * ICE:PDQ for the intraday power example, which is also the commodity code its
 * daily average price curve is quoted under. It may contain a colon and carries no
 * dash, because ORE reads the name up to the first dash as the commodity and the
 * rest as the delivery coordinate.
 *
 * delivery is that coordinate, as ORE writes it: a delivery date, then
 * optionally a half-open window in seconds from midnight, then optionally the DST
 * flag marking a daylight-saving hour. The corpus carries the window on 47 of 48
 * names and the date alone on one, and never the flag. The window is not an hour by
 * rule -- the example's own forward carries DeliveryStart 2700 and
 * DeliveryEnd 4500 for a window it calls "0.45am to 1.15am" -- so both numbers
 * are kept, not the window's index. The field is lower-cased on parse the way the
 * other keys are, and the projection writes the flag back as DST.
 */
struct power_market_data_identifier final {
    std::string commodity_code;
    instrument_type type = instrument_type::quote;
    std::optional<std::string> delivery;

    bool operator==(const power_market_data_identifier&) const = default;
};

/**
 * @brief Tagged union of the per-asset-class identifier structs.
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
                                            correlation_market_data_identifier,
                                            security_market_data_identifier,
                                            shape_profile_market_data_identifier,
                                            rating_market_data_identifier,
                                            power_market_data_identifier>;

}

#endif
