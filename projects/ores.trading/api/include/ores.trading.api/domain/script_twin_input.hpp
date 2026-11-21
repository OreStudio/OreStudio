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
#ifndef ORES_TRADING_API_DOMAIN_SCRIPT_TWIN_INPUT_HPP
#define ORES_TRADING_API_DOMAIN_SCRIPT_TWIN_INPUT_HPP

#include "ores.utility/digest/canonical_form.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One input of a scripted twin: the native type, the script, the input, and where its value
 * comes from.
 *
 * One input of the generic ScriptedTrade that twins a native trade: the native
 * trade type, the library script that carries its scripted form, the script input
 * the row states, and where in the native trade's rows its value comes from. The
 * recipe's grammar and its place in booking are [[id:206312C0-6398-4B9D-86F8-D8AEC9A5CEAD][A twin
 * recipe maps a native product's terms onto its script]].
 *
 * The generator books a product ORE carries in both forms twice, once as its own
 * element and once as a generic ScriptedTrade on the same terms. The second form's
 * variables are read from the first form's rows through these recipes, so the two
 * state the same economics and can be compared. The rows are generated from the
 * Scripted twin recipes table of the trade type catalogue.
 */
struct script_twin_input final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The native trade type the recipe twins, as the catalogue names it.
     */
    std::string trade_type_code;

    /**
     * @brief Ordinal of the input within the recipe, in the library's order.
     */
    int sequence_number{};

    /**
     * @brief The library script that carries the type's scripted form.
     */
    std::string script_name;

    /**
     * @brief The input variable, as a scripted trade names it.
     */
    std::string input_name;

    /**
     * @brief The type ORE reads the input as: Number, Event, Index, Currency or Daycounter.
     */
    std::string data_type;

    /**
     * @brief Whether the input is a list rather than one value.
     */
    bool is_array = false;

    /**
     * @brief Where the value comes from: product.<column> for a column of the native product row,
     * <rows>[<role>].<column> for a column of the shared rows under a role, <rows>.<column> for a
     * column of the shared rows, or a quoted literal.
     */
    std::string source;

    /**
     * @brief How the value is turned into the code the script reads, as the Scripted twin
     * transforms table of the trade type catalogue states.
     */
    std::optional<std::string> transform;

    /**
     * @brief Username of the person who last modified this script twin input.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     *
     * The transaction-time window's start, which the store sets from its own
     * clock. It travels with the audit members because it is only ever read
     * with them: the history builder takes a version type that carries an
     * actor *and* this timestamp, so an entity without the actor has no use
     * for the timestamp either.
     */
    std::chrono::system_clock::time_point recorded_at;

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
    friend bool operator==(const script_twin_input&, const script_twin_input&) = default;
};

/**
 * @brief The canonical text of the members of script_twin_input the customer agreed to.
 *
 * Renders, in member order, the columns the model declares economic and each
 * group whose own model declares an economic field. The economic digest hashes
 * this text, so a member the customer did not agree to never moves it.
 */
[[nodiscard]] inline std::string canonical_form(const script_twin_input& v) {
    std::string out = "{";
    ores::utility::digest::append_member(out, "input_name", v.input_name);
    ores::utility::digest::append_member(out, "data_type", v.data_type);
    ores::utility::digest::append_member(out, "is_array", v.is_array);
    ores::utility::digest::append_member(out, "source", v.source);
    ores::utility::digest::append_member(out, "transform", v.transform);
    out.push_back('}');
    return out;
}

/**
 * @brief Dispatch-key identifier for script_twin_input, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const script_twin_input&) {
    return "ores.trading.script_twin_input";
}

}

#endif
