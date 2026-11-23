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
#ifndef ORES_TRADING_API_DOMAIN_SCRIPT_INPUT_CODE_HPP
#define ORES_TRADING_API_DOMAIN_SCRIPT_INPUT_CODE_HPP

#include "ores.utility/digest/canonical_form.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One code a coded script input accepts: the number the script reads and the name it stands
 * for.
 *
 * One code a coded input of a library script accepts. A script reads some choices
 * as numbers, BarrierType as 1 for DownIn up to 4 for UpOut and PutCall as 1 for a
 * call and -1 for a put, and states what each number means only in the comment of
 * the input. The codes are those statements as data, so a form offers the names
 * and books the numbers. They are derived from the library comments by the script
 * library generator, with a correction where a comment is wrong, as the
 * ore.script_library dataset's methodology states.
 */
struct script_input_code final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The library name of the script, as a scripted trade names it.
     */
    std::string script_name;

    /**
     * @brief Ordinal of the row within its script, in the library's order of inputs and the
     * comment's order of codes.
     */
    int sequence_number{};

    /**
     * @brief The coded input, as the script's inputs name it.
     */
    std::string input_name;

    /**
     * @brief The number the script reads for this choice.
     */
    int code = 0;

    /**
     * @brief The name the library comment gives the choice, for example DownIn or call.
     */
    std::string label;

    /**
     * @brief The version of the script row this row belongs to. The publish function writes it, as
     * it does for the script's inputs.
     */
    int script_version = 0;

    /**
     * @brief Username of the person who last modified this script input code.
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
    friend bool operator==(const script_input_code&, const script_input_code&) = default;
};

/**
 * @brief The canonical text of the members of script_input_code the customer agreed to.
 *
 * Renders, in member order, the columns the model declares economic and each
 * group whose own model declares an economic field. The economic digest hashes
 * this text, so a member the customer did not agree to never moves it.
 */
[[nodiscard]] inline std::string canonical_form(const script_input_code& v) {
    std::string out = "{";
    ores::utility::digest::append_member(out, "input_name", v.input_name);
    ores::utility::digest::append_member(out, "code", v.code);
    ores::utility::digest::append_member(out, "label", v.label);
    out.push_back('}');
    return out;
}

/**
 * @brief Dispatch-key identifier for script_input_code, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const script_input_code&) {
    return "ores.trading.script_input_code";
}

}

#endif
