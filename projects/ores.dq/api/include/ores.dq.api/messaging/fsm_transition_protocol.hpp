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
#include "ores.dq.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
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
    std::optional<std::string> as_of;
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
/**
 * @brief The members of fsm_transition_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fsm_transition_key&) {
    return std::tuple{
        utility::codec::field{"name", &fsm_transition_key::name},
    };
}

/**
 * @brief The members of fsm_transition_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fsm_transition_write&) {
    return std::tuple{
        utility::codec::field{"id", &fsm_transition_write::id},
        utility::codec::field{"machine_id", &fsm_transition_write::machine_id},
        utility::codec::field{"from_state_id", &fsm_transition_write::from_state_id},
        utility::codec::field{"to_state_id", &fsm_transition_write::to_state_id},
        utility::codec::field{"name", &fsm_transition_write::name},
        utility::codec::field{"guard_function", &fsm_transition_write::guard_function},
    };
}

/**
 * @brief The members of fsm_transition_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fsm_transition_change&) {
    return std::tuple{
        utility::codec::field{"write", &fsm_transition_change::write},
        utility::codec::field{"precondition", &fsm_transition_change::precondition},
    };
}

/**
 * @brief The members of fsm_transition_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fsm_transition_removal&) {
    return std::tuple{
        utility::codec::field{"key", &fsm_transition_removal::key},
        utility::codec::field{"precondition", &fsm_transition_removal::precondition},
    };
}

/**
 * @brief The members of fsm_transition_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fsm_transition_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &fsm_transition_lookup::key},
        utility::codec::field{"fsm_transition", &fsm_transition_lookup::fsm_transition},
    };
}

/**
 * @brief The members of fsm_transitions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fsm_transitions_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &fsm_transitions_filter::id_one_of},
    };
}

/**
 * @brief The members of fsm_transition_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fsm_transition_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &fsm_transition_event::event_id},
        utility::codec::field{"key", &fsm_transition_event::key},
        utility::codec::field{"action", &fsm_transition_event::action},
        utility::codec::field{"version", &fsm_transition_event::version},
        utility::codec::field{"occurred_at", &fsm_transition_event::occurred_at},
        utility::codec::field{"correlation_id", &fsm_transition_event::correlation_id},
    };
}

/**
 * @brief The members of fsm_transition_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fsm_transition_version_key&) {
    return std::tuple{
        utility::codec::field{"fsm_transition", &fsm_transition_version_key::fsm_transition},
        utility::codec::field{"version", &fsm_transition_version_key::version},
    };
}

/**
 * @brief The members of fsm_transition_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fsm_transition_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &fsm_transition_versions_filter::version},
        utility::codec::field{"from_version", &fsm_transition_versions_filter::from_version},
        utility::codec::field{"to_version", &fsm_transition_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_fsm_transitions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_fsm_transitions_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_fsm_transitions_request::offset},
        utility::codec::field{"limit", &list_fsm_transitions_request::limit},
        utility::codec::field{"order", &list_fsm_transitions_request::order},
        utility::codec::field{"filter", &list_fsm_transitions_request::filter},
        utility::codec::field{"as_of", &list_fsm_transitions_request::as_of},
    };
}

/**
 * @brief The members of list_fsm_transitions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_fsm_transitions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_fsm_transitions_response::result},
        utility::codec::field{"transitions", &list_fsm_transitions_response::transitions},
        utility::codec::field{"total", &list_fsm_transitions_response::total},
    };
}

/**
 * @brief The members of get_fsm_transition_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_fsm_transition_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_fsm_transition_request::key},
    };
}

/**
 * @brief The members of get_fsm_transition_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_fsm_transition_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_fsm_transition_response::result},
        utility::codec::field{"fsm_transition", &get_fsm_transition_response::fsm_transition},
    };
}

/**
 * @brief The members of get_many_fsm_transitions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_fsm_transitions_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_fsm_transitions_request::keys},
    };
}

/**
 * @brief The members of get_many_fsm_transitions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_fsm_transitions_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_fsm_transitions_response::result},
        utility::codec::field{"entries", &get_many_fsm_transitions_response::entries},
    };
}

/**
 * @brief The members of put_fsm_transition_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_fsm_transition_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_fsm_transition_request::change},
        utility::codec::field{"intent", &put_fsm_transition_request::intent},
    };
}

/**
 * @brief The members of put_fsm_transition_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_fsm_transition_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_fsm_transition_response::result},
        utility::codec::field{"fsm_transition", &put_fsm_transition_response::fsm_transition},
    };
}

/**
 * @brief The members of put_many_fsm_transitions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_fsm_transitions_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_fsm_transitions_request::changes},
        utility::codec::field{"intent", &put_many_fsm_transitions_request::intent},
    };
}

/**
 * @brief The members of put_many_fsm_transitions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_fsm_transitions_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_fsm_transitions_response::result},
        utility::codec::field{"transitions", &put_many_fsm_transitions_response::transitions},
    };
}

/**
 * @brief The members of delete_fsm_transition_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_fsm_transition_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_fsm_transition_request::removal},
        utility::codec::field{"intent", &delete_fsm_transition_request::intent},
    };
}

/**
 * @brief The members of delete_fsm_transition_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_fsm_transition_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_fsm_transition_response::result},
    };
}

/**
 * @brief The members of delete_many_fsm_transitions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_fsm_transitions_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_fsm_transitions_request::removals},
        utility::codec::field{"intent", &delete_many_fsm_transitions_request::intent},
    };
}

/**
 * @brief The members of delete_many_fsm_transitions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_fsm_transitions_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_fsm_transitions_response::result},
    };
}

/**
 * @brief The members of list_fsm_transition_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_fsm_transition_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_fsm_transition_versions_request::key},
        utility::codec::field{"offset", &list_fsm_transition_versions_request::offset},
        utility::codec::field{"limit", &list_fsm_transition_versions_request::limit},
        utility::codec::field{"order", &list_fsm_transition_versions_request::order},
        utility::codec::field{"filter", &list_fsm_transition_versions_request::filter},
    };
}

/**
 * @brief The members of list_fsm_transition_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_fsm_transition_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_fsm_transition_versions_response::result},
        utility::codec::field{"versions", &list_fsm_transition_versions_response::versions},
        utility::codec::field{"total", &list_fsm_transition_versions_response::total},
    };
}

/**
 * @brief The members of get_fsm_transition_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_fsm_transition_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_fsm_transition_version_request::key},
    };
}

/**
 * @brief The members of get_fsm_transition_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_fsm_transition_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_fsm_transition_version_response::result},
        utility::codec::field{"version", &get_fsm_transition_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::fsm_transition_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::fsm_transition_key& v);
    static ores::dq::messaging::fsm_transition_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::fsm_transition_key& v);
    static ores::dq::messaging::fsm_transition_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::fsm_transition_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::fsm_transition_write& v);
    static ores::dq::messaging::fsm_transition_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::fsm_transition_write& v);
    static ores::dq::messaging::fsm_transition_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::fsm_transition_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::fsm_transition_change& v);
    static ores::dq::messaging::fsm_transition_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::fsm_transition_change& v);
    static ores::dq::messaging::fsm_transition_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::fsm_transition_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::fsm_transition_removal& v);
    static ores::dq::messaging::fsm_transition_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::fsm_transition_removal& v);
    static ores::dq::messaging::fsm_transition_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::fsm_transition_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::fsm_transition_lookup& v);
    static ores::dq::messaging::fsm_transition_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::fsm_transition_lookup& v);
    static ores::dq::messaging::fsm_transition_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::fsm_transitions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::fsm_transitions_filter& v);
    static ores::dq::messaging::fsm_transitions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::fsm_transitions_filter& v);
    static ores::dq::messaging::fsm_transitions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::fsm_transition_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::fsm_transition_event& v);
    static ores::dq::messaging::fsm_transition_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::fsm_transition_event& v);
    static ores::dq::messaging::fsm_transition_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::fsm_transition_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::fsm_transition_version_key& v);
    static ores::dq::messaging::fsm_transition_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::fsm_transition_version_key& v);
    static ores::dq::messaging::fsm_transition_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::fsm_transition_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::fsm_transition_versions_filter& v);
    static ores::dq::messaging::fsm_transition_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::fsm_transition_versions_filter& v);
    static ores::dq::messaging::fsm_transition_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_fsm_transitions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_fsm_transitions_request& v);
    static ores::dq::messaging::list_fsm_transitions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_fsm_transitions_request& v);
    static ores::dq::messaging::list_fsm_transitions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_fsm_transitions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_fsm_transitions_response& v);
    static ores::dq::messaging::list_fsm_transitions_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_fsm_transitions_response& v);
    static ores::dq::messaging::list_fsm_transitions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_fsm_transition_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_fsm_transition_request& v);
    static ores::dq::messaging::get_fsm_transition_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_fsm_transition_request& v);
    static ores::dq::messaging::get_fsm_transition_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_fsm_transition_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_fsm_transition_response& v);
    static ores::dq::messaging::get_fsm_transition_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_fsm_transition_response& v);
    static ores::dq::messaging::get_fsm_transition_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_fsm_transitions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_fsm_transitions_request& v);
    static ores::dq::messaging::get_many_fsm_transitions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_many_fsm_transitions_request& v);
    static ores::dq::messaging::get_many_fsm_transitions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_fsm_transitions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_fsm_transitions_response& v);
    static ores::dq::messaging::get_many_fsm_transitions_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_many_fsm_transitions_response& v);
    static ores::dq::messaging::get_many_fsm_transitions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_fsm_transition_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_fsm_transition_request& v);
    static ores::dq::messaging::put_fsm_transition_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_fsm_transition_request& v);
    static ores::dq::messaging::put_fsm_transition_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_fsm_transition_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_fsm_transition_response& v);
    static ores::dq::messaging::put_fsm_transition_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_fsm_transition_response& v);
    static ores::dq::messaging::put_fsm_transition_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_fsm_transitions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_fsm_transitions_request& v);
    static ores::dq::messaging::put_many_fsm_transitions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::put_many_fsm_transitions_request& v);
    static ores::dq::messaging::put_many_fsm_transitions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_fsm_transitions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_fsm_transitions_response& v);
    static ores::dq::messaging::put_many_fsm_transitions_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::put_many_fsm_transitions_response& v);
    static ores::dq::messaging::put_many_fsm_transitions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_fsm_transition_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_fsm_transition_request& v);
    static ores::dq::messaging::delete_fsm_transition_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_fsm_transition_request& v);
    static ores::dq::messaging::delete_fsm_transition_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_fsm_transition_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_fsm_transition_response& v);
    static ores::dq::messaging::delete_fsm_transition_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_fsm_transition_response& v);
    static ores::dq::messaging::delete_fsm_transition_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_fsm_transitions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::delete_many_fsm_transitions_request& v);
    static ores::dq::messaging::delete_many_fsm_transitions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_fsm_transitions_request& v);
    static ores::dq::messaging::delete_many_fsm_transitions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_fsm_transitions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::delete_many_fsm_transitions_response& v);
    static ores::dq::messaging::delete_many_fsm_transitions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_fsm_transitions_response& v);
    static ores::dq::messaging::delete_many_fsm_transitions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_fsm_transition_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_fsm_transition_versions_request& v);
    static ores::dq::messaging::list_fsm_transition_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_fsm_transition_versions_request& v);
    static ores::dq::messaging::list_fsm_transition_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_fsm_transition_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_fsm_transition_versions_response& v);
    static ores::dq::messaging::list_fsm_transition_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_fsm_transition_versions_response& v);
    static ores::dq::messaging::list_fsm_transition_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_fsm_transition_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_fsm_transition_version_request& v);
    static ores::dq::messaging::get_fsm_transition_version_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_fsm_transition_version_request& v);
    static ores::dq::messaging::get_fsm_transition_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_fsm_transition_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::get_fsm_transition_version_response& v);
    static ores::dq::messaging::get_fsm_transition_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_fsm_transition_version_response& v);
    static ores::dq::messaging::get_fsm_transition_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
