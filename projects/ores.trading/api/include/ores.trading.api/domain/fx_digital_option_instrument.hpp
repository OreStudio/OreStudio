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
#ifndef ORES_TRADING_API_DOMAIN_FX_DIGITAL_OPTION_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_FX_DIGITAL_OPTION_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>

namespace ores::trading::domain {

/**
 * @brief FX Digital Option instrument.
 *
 * Routes ORE product types: FxDigitalOption, FxDigitalBarrierOption,
 * FxTouchOption (OneTouch/NoTouch), FxDoubleTouchOption. Uses
 * ForeignCurrency/DomesticCurrency naming to match ORE XML semantics.
 * option_type is absent for touch products (no Call/Put distinction);
 * strike is absent for touch products; lower_barrier/upper_barrier
 * capture single or double barrier levels.
 */
struct fx_digital_option_instrument final {
    instrument_identity identity;

    /**
     * @brief The currency reference stays soft. A currency is ores.refdata's row, published from
     * the data-quality pipeline per environment rather than by this component, and a trading
     * database may hold only the codes its own imports loaded. No tenant's currency table holds EUR
     * or USD in a freshly recreated database, so a trigger check would refuse the documents the
     * store is meant to accept.
     */
    std::string foreign_currency;

    /**
     * @brief The currency reference stays soft. A currency is ores.refdata's row, published from
     * the data-quality pipeline per environment rather than by this component, and a trading
     * database may hold only the codes its own imports loaded. No tenant's currency table holds EUR
     * or USD in a freshly recreated database, so a trigger check would refuse the documents the
     * store is meant to accept.
     */
    std::string domestic_currency;

    /**
     * @brief The currency reference stays soft. A currency is ores.refdata's row, published from
     * the data-quality pipeline per environment rather than by this component, and a trading
     * database may hold only the codes its own imports loaded. No tenant's currency table holds EUR
     * or USD in a freshly recreated database, so a trigger check would refuse the documents the
     * store is meant to accept.
     */
    std::string payoff_currency;

    /**
     * @brief Fixed payoff amount. Must be positive.
     */
    ores::utility::decimal::decimal payoff_amount;

    /**
     * @brief Option type: Call or Put. Empty for touch options.
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
     * @brief Position direction: Long or Short.
     *
     * the values are the closed ORE longShort set (Long, Short), which the SQL schema already
     * states as a check.
     *
     * The four product codes share this table, so each check names the fields its own code engages
     * and requires them. A field another code uses is tolerated rather than refused, because one
     * generator writes a synthetic row for all four and would otherwise produce a row no code
     * accepts.
     */
    std::string long_short;

    /**
     * @brief Strike level. Absent for touch options.
     */
    std::optional<ores::utility::decimal::decimal> strike;

    /**
     * @brief Barrier type (e.g. DownAndIn, DownAndOut, KnockIn, KnockOut). Absent for plain digital
     * options.
     *
     * the values are the barrier styles ORE Studio supports. ORE states the leaf as free text, so
     * the lookup is the set this store accepts rather than a closed schema enumeration.
     */
    std::string barrier_type;

    /**
     * @brief Primary barrier level. Absent for plain digital options.
     */
    std::optional<ores::utility::decimal::decimal> lower_barrier;

    /**
     * @brief Second barrier level for FxDoubleTouchOption.
     */
    std::optional<ores::utility::decimal::decimal> upper_barrier;

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
    friend bool operator==(const fx_digital_option_instrument&,
                           const fx_digital_option_instrument&) = default;
};

/**
 * @brief The members of fx_digital_option_instrument the customer agreed to.
 *
 * Declared by the model's :economic: property on each column.
 */
inline constexpr std::string_view fx_digital_option_instrument_economic_fields[] = {
    "foreign_currency",
    "domestic_currency",
    "payoff_currency",
    "payoff_amount",
    "option_type",
    "expiry_date",
    "long_short",
    "strike",
    "barrier_type",
    "lower_barrier",
    "upper_barrier",
};

/**
 * @brief Names the members of fx_digital_option_instrument that are economic.
 *
 * The economic digest reads this at compile time and decides nothing by a
 * field's name. A type without this overload cannot be digested.
 */
[[nodiscard]] constexpr std::span<const std::string_view>
economic_fields_of(std::type_identity<fx_digital_option_instrument>) {
    return fx_digital_option_instrument_economic_fields;
}

/**
 * @brief Dispatch-key identifier for fx_digital_option_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const fx_digital_option_instrument&) {
    return "ores.trading.fx_digital_option_instrument";
}

}

#endif
