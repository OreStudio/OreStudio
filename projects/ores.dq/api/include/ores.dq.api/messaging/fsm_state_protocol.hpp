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
#ifndef ORES_DQ_API_MESSAGING_FSM_STATE_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_FSM_STATE_PROTOCOL_HPP

#include "ores.dq.api/domain/fsm_state.hpp"
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

struct fsm_state_key {
    std::string name;
};

struct fsm_state_write {
    boost::uuids::uuid id;
    boost::uuids::uuid machine_id;
    std::string name;
    bool is_initial;
    bool is_terminal;
};

struct fsm_state_change {
    fsm_state_write write;
    ores::utility::domain::precondition precondition;
};

struct fsm_state_removal {
    fsm_state_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct fsm_state_lookup {
    fsm_state_key key;
    std::optional<ores::dq::domain::fsm_state> fsm_state;
};

struct fsm_states_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct fsm_state_event {
    boost::uuids::uuid event_id;
    fsm_state_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct fsm_state_version_key {
    fsm_state_key fsm_state;
    std::uint32_t version;
};

struct fsm_state_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_fsm_states_request {
    using response_type = struct list_fsm_states_response;
    static constexpr std::string_view nats_subject = "dq.v1.fsm_states.list";
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
    std::optional<fsm_states_filter> filter;
    std::optional<std::string> as_of;
};

struct list_fsm_states_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::fsm_state> states;
    std::uint64_t total;
};

struct get_fsm_state_request {
    using response_type = struct get_fsm_state_response;
    static constexpr std::string_view nats_subject = "dq.v1.fsm_states.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fsm_state_key key;
};

struct get_fsm_state_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::fsm_state> fsm_state;
};

struct get_many_fsm_states_request {
    using response_type = struct get_many_fsm_states_response;
    static constexpr std::string_view nats_subject = "dq.v1.fsm_states.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fsm_state_key> keys;
};

struct get_many_fsm_states_response {
    ores::utility::domain::result result;
    std::vector<fsm_state_lookup> entries;
};

struct put_fsm_state_request {
    using response_type = struct put_fsm_state_response;
    static constexpr std::string_view nats_subject = "dq.v1.fsm_states.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fsm_state_change change;
    ores::utility::domain::change_intent intent;
};

struct put_fsm_state_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::fsm_state> fsm_state;
};

struct put_many_fsm_states_request {
    using response_type = struct put_many_fsm_states_response;
    static constexpr std::string_view nats_subject = "dq.v1.fsm_states.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fsm_state_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_fsm_states_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::fsm_state> states;
};

struct delete_fsm_state_request {
    using response_type = struct delete_fsm_state_response;
    static constexpr std::string_view nats_subject = "dq.v1.fsm_states.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fsm_state_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_fsm_state_response {
    ores::utility::domain::result result;
};

struct delete_many_fsm_states_request {
    using response_type = struct delete_many_fsm_states_response;
    static constexpr std::string_view nats_subject = "dq.v1.fsm_states.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fsm_state_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_fsm_states_response {
    ores::utility::domain::result result;
};

struct list_fsm_state_versions_request {
    using response_type = struct list_fsm_state_versions_response;
    static constexpr std::string_view nats_subject = "dq.v1.fsm_states_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fsm_state_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<fsm_state_versions_filter> filter;
};

struct list_fsm_state_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::fsm_state> versions;
    std::uint64_t total;
};

struct get_fsm_state_version_request {
    using response_type = struct get_fsm_state_version_response;
    static constexpr std::string_view nats_subject = "dq.v1.fsm_states_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fsm_state_version_key key;
};

struct get_fsm_state_version_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::fsm_state> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace fsm_state_event_subjects {
inline constexpr std::string_view created = "dq.v1.fsm_states_events.created";
inline constexpr std::string_view updated = "dq.v1.fsm_states_events.updated";
inline constexpr std::string_view deleted = "dq.v1.fsm_states_events.deleted";
}

/**
 * @brief The members of fsm_state_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fsm_state_key&) {
    return std::tuple{
        utility::codec::field{"name", &fsm_state_key::name},
    };
}

/**
 * @brief The members of fsm_state_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fsm_state_write&) {
    return std::tuple{
        utility::codec::field{"id", &fsm_state_write::id},
        utility::codec::field{"machine_id", &fsm_state_write::machine_id},
        utility::codec::field{"name", &fsm_state_write::name},
        utility::codec::field{"is_initial", &fsm_state_write::is_initial},
        utility::codec::field{"is_terminal", &fsm_state_write::is_terminal},
    };
}

/**
 * @brief The members of fsm_state_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fsm_state_change&) {
    return std::tuple{
        utility::codec::field{"write", &fsm_state_change::write},
        utility::codec::field{"precondition", &fsm_state_change::precondition},
    };
}

/**
 * @brief The members of fsm_state_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fsm_state_removal&) {
    return std::tuple{
        utility::codec::field{"key", &fsm_state_removal::key},
        utility::codec::field{"precondition", &fsm_state_removal::precondition},
    };
}

/**
 * @brief The members of fsm_state_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fsm_state_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &fsm_state_lookup::key},
        utility::codec::field{"fsm_state", &fsm_state_lookup::fsm_state},
    };
}

/**
 * @brief The members of fsm_states_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fsm_states_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &fsm_states_filter::id_one_of},
    };
}

/**
 * @brief The members of fsm_state_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fsm_state_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &fsm_state_event::event_id},
        utility::codec::field{"key", &fsm_state_event::key},
        utility::codec::field{"action", &fsm_state_event::action},
        utility::codec::field{"version", &fsm_state_event::version},
        utility::codec::field{"occurred_at", &fsm_state_event::occurred_at},
        utility::codec::field{"correlation_id", &fsm_state_event::correlation_id},
    };
}

/**
 * @brief The members of fsm_state_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fsm_state_version_key&) {
    return std::tuple{
        utility::codec::field{"fsm_state", &fsm_state_version_key::fsm_state},
        utility::codec::field{"version", &fsm_state_version_key::version},
    };
}

/**
 * @brief The members of fsm_state_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fsm_state_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &fsm_state_versions_filter::version},
        utility::codec::field{"from_version", &fsm_state_versions_filter::from_version},
        utility::codec::field{"to_version", &fsm_state_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_fsm_states_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_fsm_states_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_fsm_states_request::offset},
        utility::codec::field{"limit", &list_fsm_states_request::limit},
        utility::codec::field{"order", &list_fsm_states_request::order},
        utility::codec::field{"filter", &list_fsm_states_request::filter},
        utility::codec::field{"as_of", &list_fsm_states_request::as_of},
    };
}

/**
 * @brief The members of list_fsm_states_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_fsm_states_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_fsm_states_response::result},
        utility::codec::field{"states", &list_fsm_states_response::states},
        utility::codec::field{"total", &list_fsm_states_response::total},
    };
}

/**
 * @brief The members of get_fsm_state_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_fsm_state_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_fsm_state_request::key},
    };
}

/**
 * @brief The members of get_fsm_state_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_fsm_state_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_fsm_state_response::result},
        utility::codec::field{"fsm_state", &get_fsm_state_response::fsm_state},
    };
}

/**
 * @brief The members of get_many_fsm_states_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_fsm_states_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_fsm_states_request::keys},
    };
}

/**
 * @brief The members of get_many_fsm_states_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_fsm_states_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_fsm_states_response::result},
        utility::codec::field{"entries", &get_many_fsm_states_response::entries},
    };
}

/**
 * @brief The members of put_fsm_state_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_fsm_state_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_fsm_state_request::change},
        utility::codec::field{"intent", &put_fsm_state_request::intent},
    };
}

/**
 * @brief The members of put_fsm_state_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_fsm_state_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_fsm_state_response::result},
        utility::codec::field{"fsm_state", &put_fsm_state_response::fsm_state},
    };
}

/**
 * @brief The members of put_many_fsm_states_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_fsm_states_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_fsm_states_request::changes},
        utility::codec::field{"intent", &put_many_fsm_states_request::intent},
    };
}

/**
 * @brief The members of put_many_fsm_states_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_fsm_states_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_fsm_states_response::result},
        utility::codec::field{"states", &put_many_fsm_states_response::states},
    };
}

/**
 * @brief The members of delete_fsm_state_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_fsm_state_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_fsm_state_request::removal},
        utility::codec::field{"intent", &delete_fsm_state_request::intent},
    };
}

/**
 * @brief The members of delete_fsm_state_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_fsm_state_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_fsm_state_response::result},
    };
}

/**
 * @brief The members of delete_many_fsm_states_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_fsm_states_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_fsm_states_request::removals},
        utility::codec::field{"intent", &delete_many_fsm_states_request::intent},
    };
}

/**
 * @brief The members of delete_many_fsm_states_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_fsm_states_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_fsm_states_response::result},
    };
}

/**
 * @brief The members of list_fsm_state_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_fsm_state_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_fsm_state_versions_request::key},
        utility::codec::field{"offset", &list_fsm_state_versions_request::offset},
        utility::codec::field{"limit", &list_fsm_state_versions_request::limit},
        utility::codec::field{"order", &list_fsm_state_versions_request::order},
        utility::codec::field{"filter", &list_fsm_state_versions_request::filter},
    };
}

/**
 * @brief The members of list_fsm_state_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_fsm_state_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_fsm_state_versions_response::result},
        utility::codec::field{"versions", &list_fsm_state_versions_response::versions},
        utility::codec::field{"total", &list_fsm_state_versions_response::total},
    };
}

/**
 * @brief The members of get_fsm_state_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_fsm_state_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_fsm_state_version_request::key},
    };
}

/**
 * @brief The members of get_fsm_state_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_fsm_state_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_fsm_state_version_response::result},
        utility::codec::field{"version", &get_fsm_state_version_response::version},
    };
}

}

namespace ores::utility::codec {

template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::fsm_state_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::fsm_state_key& v);
    static ores::dq::messaging::fsm_state_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::fsm_state_key& v);
    static ores::dq::messaging::fsm_state_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::fsm_state_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::fsm_state_write& v);
    static ores::dq::messaging::fsm_state_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::fsm_state_write& v);
    static ores::dq::messaging::fsm_state_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::fsm_state_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::fsm_state_change& v);
    static ores::dq::messaging::fsm_state_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::fsm_state_change& v);
    static ores::dq::messaging::fsm_state_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::fsm_state_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::fsm_state_removal& v);
    static ores::dq::messaging::fsm_state_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::fsm_state_removal& v);
    static ores::dq::messaging::fsm_state_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::fsm_state_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::fsm_state_lookup& v);
    static ores::dq::messaging::fsm_state_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::fsm_state_lookup& v);
    static ores::dq::messaging::fsm_state_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::fsm_states_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::fsm_states_filter& v);
    static ores::dq::messaging::fsm_states_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::fsm_states_filter& v);
    static ores::dq::messaging::fsm_states_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::fsm_state_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::fsm_state_event& v);
    static ores::dq::messaging::fsm_state_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::fsm_state_event& v);
    static ores::dq::messaging::fsm_state_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::fsm_state_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::fsm_state_version_key& v);
    static ores::dq::messaging::fsm_state_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::fsm_state_version_key& v);
    static ores::dq::messaging::fsm_state_version_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::fsm_state_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::fsm_state_versions_filter& v);
    static ores::dq::messaging::fsm_state_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::fsm_state_versions_filter& v);
    static ores::dq::messaging::fsm_state_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_fsm_states_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_fsm_states_request& v);
    static ores::dq::messaging::list_fsm_states_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_fsm_states_request& v);
    static ores::dq::messaging::list_fsm_states_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_fsm_states_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_fsm_states_response& v);
    static ores::dq::messaging::list_fsm_states_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_fsm_states_response& v);
    static ores::dq::messaging::list_fsm_states_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_fsm_state_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_fsm_state_request& v);
    static ores::dq::messaging::get_fsm_state_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_fsm_state_request& v);
    static ores::dq::messaging::get_fsm_state_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_fsm_state_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_fsm_state_response& v);
    static ores::dq::messaging::get_fsm_state_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_fsm_state_response& v);
    static ores::dq::messaging::get_fsm_state_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_fsm_states_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_fsm_states_request& v);
    static ores::dq::messaging::get_many_fsm_states_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_many_fsm_states_request& v);
    static ores::dq::messaging::get_many_fsm_states_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_fsm_states_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_fsm_states_response& v);
    static ores::dq::messaging::get_many_fsm_states_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_many_fsm_states_response& v);
    static ores::dq::messaging::get_many_fsm_states_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_fsm_state_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_fsm_state_request& v);
    static ores::dq::messaging::put_fsm_state_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_fsm_state_request& v);
    static ores::dq::messaging::put_fsm_state_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_fsm_state_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_fsm_state_response& v);
    static ores::dq::messaging::put_fsm_state_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_fsm_state_response& v);
    static ores::dq::messaging::put_fsm_state_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_fsm_states_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_fsm_states_request& v);
    static ores::dq::messaging::put_many_fsm_states_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_many_fsm_states_request& v);
    static ores::dq::messaging::put_many_fsm_states_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_fsm_states_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_fsm_states_response& v);
    static ores::dq::messaging::put_many_fsm_states_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_many_fsm_states_response& v);
    static ores::dq::messaging::put_many_fsm_states_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_fsm_state_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_fsm_state_request& v);
    static ores::dq::messaging::delete_fsm_state_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_fsm_state_request& v);
    static ores::dq::messaging::delete_fsm_state_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_fsm_state_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_fsm_state_response& v);
    static ores::dq::messaging::delete_fsm_state_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_fsm_state_response& v);
    static ores::dq::messaging::delete_fsm_state_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_fsm_states_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_many_fsm_states_request& v);
    static ores::dq::messaging::delete_many_fsm_states_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_many_fsm_states_request& v);
    static ores::dq::messaging::delete_many_fsm_states_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_fsm_states_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_many_fsm_states_response& v);
    static ores::dq::messaging::delete_many_fsm_states_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_many_fsm_states_response& v);
    static ores::dq::messaging::delete_many_fsm_states_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_fsm_state_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_fsm_state_versions_request& v);
    static ores::dq::messaging::list_fsm_state_versions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_fsm_state_versions_request& v);
    static ores::dq::messaging::list_fsm_state_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_fsm_state_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_fsm_state_versions_response& v);
    static ores::dq::messaging::list_fsm_state_versions_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_fsm_state_versions_response& v);
    static ores::dq::messaging::list_fsm_state_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_fsm_state_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_fsm_state_version_request& v);
    static ores::dq::messaging::get_fsm_state_version_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_fsm_state_version_request& v);
    static ores::dq::messaging::get_fsm_state_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_fsm_state_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_fsm_state_version_response& v);
    static ores::dq::messaging::get_fsm_state_version_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_fsm_state_version_response& v);
    static ores::dq::messaging::get_fsm_state_version_response
    read_msgpack(std::span<const std::byte> data);
};
}

#endif
