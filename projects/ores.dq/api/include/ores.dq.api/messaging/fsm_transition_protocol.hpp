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
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_DQ_API_MESSAGING_FSM_TRANSITION_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_FSM_TRANSITION_PROTOCOL_HPP

#include "ores.dq.api/domain/fsm_transition.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::messaging {

struct fsm_transition_key {
    std::string name;
};

struct fsm_transition_write {
    boost::uuids::uuid id;
    boost::uuids::uuid machine_id;
    std::optional<boost::uuids::uuid> from_state_id;
    boost::uuids::uuid to_state_id;
    std::string name;
    std::optional<std::string> guard_function;
};

struct fsm_transition_change {
    fsm_transition_write write;
    ores::utility::domain::precondition precondition;
};

struct fsm_transition_removal {
    fsm_transition_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct fsm_transition_lookup {
    fsm_transition_key key;
    std::optional<ores::dq::domain::fsm_transition> fsm_transition;
};

struct fsm_transitions_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct fsm_transition_event {
    boost::uuids::uuid event_id;
    fsm_transition_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct fsm_transition_version_key {
    fsm_transition_key fsm_transition;
    std::uint32_t version;
};

struct fsm_transition_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_fsm_transitions_request {
    using response_type = struct list_fsm_transitions_response;
    static constexpr std::string_view nats_subject = "dq.v1.fsm_transitions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<fsm_transitions_filter> filter;
};

struct list_fsm_transitions_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::fsm_transition> transitions;
    std::uint64_t total;
};

struct get_fsm_transition_request {
    using response_type = struct get_fsm_transition_response;
    static constexpr std::string_view nats_subject = "dq.v1.fsm_transitions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fsm_transition_key key;
};

struct get_fsm_transition_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::fsm_transition> fsm_transition;
};

struct get_many_fsm_transitions_request {
    using response_type = struct get_many_fsm_transitions_response;
    static constexpr std::string_view nats_subject = "dq.v1.fsm_transitions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fsm_transition_key> keys;
};

struct get_many_fsm_transitions_response {
    ores::utility::domain::result result;
    std::vector<fsm_transition_lookup> entries;
};

struct put_fsm_transition_request {
    using response_type = struct put_fsm_transition_response;
    static constexpr std::string_view nats_subject = "dq.v1.fsm_transitions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fsm_transition_change change;
    ores::utility::domain::change_intent intent;
};

struct put_fsm_transition_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::fsm_transition> fsm_transition;
};

struct put_many_fsm_transitions_request {
    using response_type = struct put_many_fsm_transitions_response;
    static constexpr std::string_view nats_subject = "dq.v1.fsm_transitions.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fsm_transition_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_fsm_transitions_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::fsm_transition> transitions;
};

struct delete_fsm_transition_request {
    using response_type = struct delete_fsm_transition_response;
    static constexpr std::string_view nats_subject = "dq.v1.fsm_transitions.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fsm_transition_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_fsm_transition_response {
    ores::utility::domain::result result;
};

struct delete_many_fsm_transitions_request {
    using response_type = struct delete_many_fsm_transitions_response;
    static constexpr std::string_view nats_subject = "dq.v1.fsm_transitions.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fsm_transition_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_fsm_transitions_response {
    ores::utility::domain::result result;
};

struct list_fsm_transition_versions_request {
    using response_type = struct list_fsm_transition_versions_response;
    static constexpr std::string_view nats_subject = "dq.v1.fsm_transitions_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fsm_transition_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<fsm_transition_versions_filter> filter;
};

struct list_fsm_transition_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::fsm_transition> versions;
    std::uint64_t total;
};

struct get_fsm_transition_version_request {
    using response_type = struct get_fsm_transition_version_response;
    static constexpr std::string_view nats_subject = "dq.v1.fsm_transitions_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fsm_transition_version_key key;
};

struct get_fsm_transition_version_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::fsm_transition> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace fsm_transition_event_subjects {
inline constexpr std::string_view created = "dq.v1.fsm_transitions_events.created";
inline constexpr std::string_view updated = "dq.v1.fsm_transitions_events.updated";
inline constexpr std::string_view deleted = "dq.v1.fsm_transitions_events.deleted";
}

}

#endif
