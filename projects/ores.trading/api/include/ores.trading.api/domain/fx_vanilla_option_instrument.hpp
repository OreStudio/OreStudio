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
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_API_DOMAIN_FX_VANILLA_OPTION_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_FX_VANILLA_OPTION_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/digest/canonical_form.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief FX Vanilla Option instrument (European, Bermudan and American).
 *
 * Represents FxOption trades. Strike is implicit in bought/sold
 * amounts; there is no separate strike field in ORE's FxOptionData.
 */
struct fx_vanilla_option_instrument final {
    instrument_identity identity;

    /**
     * @brief The currency reference stays soft. A currency is ores.refdata's row, published from
     * the data-quality pipeline per environment rather than by this component, and a trading
     * database may hold only the codes its own imports loaded. No tenant's currency table holds EUR
     * or USD in a freshly recreated database, so a trigger check would refuse the documents the
     * store is meant to accept.
     */
    std::string bought_currency;

    /**
     * @brief Amount bought in bought_currency. Must be positive.
     */
    ores::utility::decimal::decimal bought_amount;

    /**
     * @brief The currency reference stays soft. A currency is ores.refdata's row, published from
     * the data-quality pipeline per environment rather than by this component, and a trading
     * database may hold only the codes its own imports loaded. No tenant's currency table holds EUR
     * or USD in a freshly recreated database, so a trigger check would refuse the documents the
     * store is meant to accept.
     */
    std::string sold_currency;

    /**
     * @brief Amount sold in sold_currency. Must be positive.
     */
    ores::utility::decimal::decimal sold_amount;

    /**
     * @brief Option type: Call or Put.
     *
     * the values are the option types this store accepts (Call, Put), which ORE also states as a
     * closed set.
     */
    std::string option_type;

    /**
     * @brief Option expiry date (ISO 8601 date string).
     */
    std::chrono::year_month_day expiry_date;

    /**
     * @brief Exercise style, one of the ORE exerciseStyle set: European, Bermudan or American. The
     * lookup ores_trading_exercise_types_tbl holds the set, so the store refuses a style it does
     * not carry.
     */
    std::string exercise_style;

    /**
     * @brief Optional settlement method (e.g. Cash, Physical).
     */
    std::string settlement;

    /**
     * @brief Optional free-text description.
     */
    std::string description;

    ores::dq::domain::audit_record audit;
    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const fx_vanilla_option_instrument&,
                           const fx_vanilla_option_instrument&) = default;
};

/**
 * @brief The canonical text of the members of fx_vanilla_option_instrument the customer agreed to.
 *
 * Renders, in member order, the columns the model declares economic and each
 * group whose own model declares an economic field. The economic digest hashes
 * this text, so a member the customer did not agree to never moves it.
 */
[[nodiscard]] inline std::string canonical_form(const fx_vanilla_option_instrument& v) {
    std::string out = "{";
    ores::utility::digest::append_member(out, "bought_currency", v.bought_currency);
    ores::utility::digest::append_member(out, "bought_amount", v.bought_amount);
    ores::utility::digest::append_member(out, "sold_currency", v.sold_currency);
    ores::utility::digest::append_member(out, "sold_amount", v.sold_amount);
    ores::utility::digest::append_member(out, "option_type", v.option_type);
    ores::utility::digest::append_member(out, "expiry_date", v.expiry_date);
    ores::utility::digest::append_member(out, "exercise_style", v.exercise_style);
    ores::utility::digest::append_member(out, "settlement", v.settlement);
    out.push_back('}');
    return out;
}

/**
 * @brief Dispatch-key identifier for fx_vanilla_option_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const fx_vanilla_option_instrument&) {
    return "ores.trading.fx_vanilla_option_instrument";
}

}

#endif
