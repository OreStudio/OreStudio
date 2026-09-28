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
#ifndef ORES_IAM_API_DOMAIN_SEED_PROFILE_PARAMETER_HPP
#define ORES_IAM_API_DOMAIN_SEED_PROFILE_PARAMETER_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::iam::domain {

/**
 * @brief One input a seed profile's form declares.
 *
 * The schema of the form that a seed profile presents: one row per
 * parameter, stating the label the form shows, the key a shell command types,
 * the type, the default and whether a value is required. The web builds the
 * tenant form from these rows. The shell takes the same values as
 * =--param key=value=.
 *
 * The declared shape is deliberately small. A profile decides how many
 * counterparties to create and which GLEIF root LEI to import; it does not
 * decide what a step kind does, because that is code.
 *
 * A parameter whose type is choice names the values it accepts, in the order
 * the form offers them. A choice is declared rather than left to free text so
 * that a value the run cannot use is not typeable at all.
 *
 * A profile with no parameters states that its form asks for nothing. The
 * demonstration profile is one: it carries every value it needs, so its
 * journey presents no input at all.
 *
 * The pair (seed_profile_id, name) is the identity.
 */
struct seed_profile_parameter final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate identifier for the parameter row.
     */
    boost::uuids::uuid id;

    /**
     * @brief Profile whose form declares this parameter.
     */
    boost::uuids::uuid seed_profile_id;

    /**
     * @brief Parameter name, as the form's field key and as the shell's --param key. For example
     * root_lei or counterparty_size.
     */
    std::string name;

    /**
     * @brief Label the form puts above the field, for example "Counterparty set". It is data rather
     * than a function of the name, because the name is a key a shell command types and the label is
     * a sentence a person reads.
     */
    std::string label;

    /**
     * @brief How to read the value: string, integer, boolean or choice. Stored beside the value
     * because one table carries every type, and the form picks its widget from it. A choice takes
     * one of the values in choices_json.
     */
    std::string data_type;

    /**
     * @brief Values a choice parameter accepts, in the order the form offers them, as a serialised
     * JSON array of strings. Null when the type offers no choice.
     */
    std::string choices_json;

    /**
     * @brief Value the form starts with, as text and read according to data_type. Null states that
     * the parameter has no default and the administrator must supply one.
     */
    std::string default_value;

    /**
     * @brief Whether the form refuses to continue without a value. A required parameter with a
     * default is prefilled and accepted as it stands.
     */
    bool is_required = false;

    /**
     * @brief What the parameter changes, as the form's help text.
     */
    std::string description;

    /**
     * @brief Order the form presents this field in. Lower numbers appear first.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this seed profile parameter.
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
    friend bool operator==(const seed_profile_parameter&, const seed_profile_parameter&) = default;
};

/**
 * @brief Dispatch-key identifier for seed_profile_parameter, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const seed_profile_parameter&) {
    return "ores.iam.seed_profile_parameter";
}

}

#endif
