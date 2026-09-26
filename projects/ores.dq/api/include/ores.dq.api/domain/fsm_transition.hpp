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
#ifndef ORES_DQ_API_DOMAIN_FSM_TRANSITION_HPP
#define ORES_DQ_API_DOMAIN_FSM_TRANSITION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::dq::domain {

/**
 * @brief A transition between two states of a finite state machine.
 *
 * A transition between two states of a finite state machine, unique within a
 * machine for a given pair of states.
 *
 * from_state_id is nullable, so a transition may start from anywhere rather
 * than from a named state. Like machine_id and to_state_id it is a plain uuid
 * column rather than a declared reference, because the table carries no
 * constraint and adding one would refuse rows the platform accepts today.
 */
struct fsm_transition final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this transition.
     */
    boost::uuids::uuid id;

    /**
     * @brief The state machine this transition belongs to; part of the natural key.
     */
    boost::uuids::uuid machine_id;

    /**
     * @brief The state the transition leaves, when it is named.
     */
    std::optional<boost::uuids::uuid> from_state_id;

    /**
     * @brief The state the transition enters; part of the natural key.
     */
    boost::uuids::uuid to_state_id;

    /**
     * @brief Name of the transition.
     */
    std::string name;

    /**
     * @brief Optional guard the transition is taken behind.
     */
    std::optional<std::string> guard_function;

    /**
     * @brief Username of the person who last modified this fsm transition.
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
    friend bool operator==(const fsm_transition&, const fsm_transition&) = default;
};

/**
 * @brief Dispatch-key identifier for fsm_transition, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const fsm_transition&) {
    return "ores.dq.fsm_transition";
}

}

#endif
