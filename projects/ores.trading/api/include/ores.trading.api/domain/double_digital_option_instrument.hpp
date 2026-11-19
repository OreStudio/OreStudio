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
#ifndef ORES_TRADING_API_DOMAIN_DOUBLE_DIGITAL_OPTION_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_DOUBLE_DIGITAL_OPTION_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/digest/canonical_form.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Double digital option.
 *
 * Represents DoubleDigitalOption trades: a binary option paying a fixed
 * amount when two underlyings are in the money together. ORE states it as an element of its own,
 * and its underlyings may be equities, FX rates or commodities, so it is held in the multi-asset
 * family, where each underlying row keeps its own type.
 * Underlying1 to Underlying4 are instrument underlying rows under the roles
 * underlying1 to underlying4.
 */
struct double_digital_option_instrument final {
    instrument_identity identity;

    /**
     * @brief The date both levels are observed, as Expiry states it.
     */
    std::chrono::year_month_day expiry;

    /**
     * @brief The date the payout is made, as Settlement states it.
     */
    std::chrono::year_month_day settlement;

    /**
     * @brief The amount paid, as BinaryPayout states it.
     */
    ores::utility::decimal::decimal binary_payout;

    /**
     * @brief The first underlying's level, as BinaryLevel1 states it.
     */
    ores::utility::decimal::decimal binary_level_1;

    /**
     * @brief The second underlying's level, as BinaryLevel2 states it.
     */
    ores::utility::decimal::decimal binary_level_2;

    /**
     * @brief The first underlying's upper level, for a collar, as BinaryLevelUpper1 states it.
     */
    std::optional<ores::utility::decimal::decimal> binary_level_upper_1;

    /**
     * @brief The second underlying's upper level, for a collar, as BinaryLevelUpper2 states it.
     */
    std::optional<ores::utility::decimal::decimal> binary_level_upper_2;

    /**
     * @brief Call, Put or Collar for the first underlying, as Type1 states it.
     */
    std::string type_1;

    /**
     * @brief Call, Put or Collar for the second underlying, as Type2 states it.
     */
    std::string type_2;

    /**
     * @brief Long or Short, as Position states it.
     */
    std::string long_short;

    /**
     * @brief The first underlying's name, as Name1 states it.
     */
    std::optional<std::string> name_1;

    /**
     * @brief The second underlying's name, as Name2 states it.
     */
    std::optional<std::string> name_2;

    /**
     * @brief The currency the product pays in, as PayCcy states it.
     */
    std::string pay_currency;

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
    friend bool operator==(const double_digital_option_instrument&,
                           const double_digital_option_instrument&) = default;
};

/**
 * @brief The canonical text of the members of double_digital_option_instrument the customer agreed
 * to.
 *
 * Renders, in member order, the columns the model declares economic and each
 * group whose own model declares an economic field. The economic digest hashes
 * this text, so a member the customer did not agree to never moves it.
 */
[[nodiscard]] inline std::string canonical_form(const double_digital_option_instrument& v) {
    std::string out = "{";
    ores::utility::digest::append_member(out, "expiry", v.expiry);
    ores::utility::digest::append_member(out, "settlement", v.settlement);
    ores::utility::digest::append_member(out, "binary_payout", v.binary_payout);
    ores::utility::digest::append_member(out, "binary_level_1", v.binary_level_1);
    ores::utility::digest::append_member(out, "binary_level_2", v.binary_level_2);
    ores::utility::digest::append_member(out, "binary_level_upper_1", v.binary_level_upper_1);
    ores::utility::digest::append_member(out, "binary_level_upper_2", v.binary_level_upper_2);
    ores::utility::digest::append_member(out, "type_1", v.type_1);
    ores::utility::digest::append_member(out, "type_2", v.type_2);
    ores::utility::digest::append_member(out, "long_short", v.long_short);
    ores::utility::digest::append_member(out, "name_1", v.name_1);
    ores::utility::digest::append_member(out, "name_2", v.name_2);
    ores::utility::digest::append_member(out, "pay_currency", v.pay_currency);
    out.push_back('}');
    return out;
}

/**
 * @brief Dispatch-key identifier for double_digital_option_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const double_digital_option_instrument&) {
    return "ores.trading.double_digital_option_instrument";
}

}

#endif
