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
#ifndef ORES_MARKETDATA_API_MESSAGING_SESSION_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_SESSION_PROTOCOL_HPP

#include "ores.marketdata.api/domain/session.hpp"
#include "ores.marketdata.api/export.hpp"
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

namespace ores::marketdata::messaging {

struct session_key {
    std::string code;
};

struct session_write {
    std::string code;
    std::string name;
    std::string supplier_code;
    std::string adapter_code;
    std::string environment;
    std::string endpoint;
    std::string credential_ref;
    std::string settings;
    bool enabled;
    std::string description;
};

struct session_change {
    session_write write;
    ores::utility::domain::precondition precondition;
};

struct session_removal {
    session_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct session_lookup {
    session_key key;
    std::optional<ores::marketdata::domain::session> session;
};

struct sessions_filter {
    std::optional<std::string> supplier_code;
    std::optional<std::string> adapter_code;
    std::optional<std::string> environment;
    std::optional<std::vector<std::string>> code_one_of;
    std::optional<std::vector<std::string>> supplier_code_one_of;
    std::optional<std::vector<std::string>> adapter_code_one_of;
    std::optional<std::vector<std::string>> environment_one_of;
    std::optional<std::string> search;
};

struct session_event {
    boost::uuids::uuid event_id;
    session_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct session_version_key {
    session_key session;
    std::uint32_t version;
};

struct session_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_sessions_request {
    using response_type = struct list_sessions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.sessions.list";
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
    std::optional<sessions_filter> filter;
    std::optional<std::string> as_of;
};

struct list_sessions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::session> sessions;
    std::uint64_t total;
};

struct get_session_request {
    using response_type = struct get_session_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.sessions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    session_key key;
};

struct get_session_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::session> session;
};

struct get_many_sessions_request {
    using response_type = struct get_many_sessions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.sessions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<session_key> keys;
};

struct get_many_sessions_response {
    ores::utility::domain::result result;
    std::vector<session_lookup> entries;
};

struct put_session_request {
    using response_type = struct put_session_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.sessions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    session_change change;
    ores::utility::domain::change_intent intent;
};

struct put_session_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::session> session;
};

struct put_many_sessions_request {
    using response_type = struct put_many_sessions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.sessions.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<session_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_sessions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::session> sessions;
};

struct delete_session_request {
    using response_type = struct delete_session_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.sessions.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    session_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_session_response {
    ores::utility::domain::result result;
};

struct delete_many_sessions_request {
    using response_type = struct delete_many_sessions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.sessions.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<session_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_sessions_response {
    ores::utility::domain::result result;
};

struct list_session_versions_request {
    using response_type = struct list_session_versions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.sessions_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    session_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<session_versions_filter> filter;
};

struct list_session_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::session> versions;
    std::uint64_t total;
};

struct get_session_version_request {
    using response_type = struct get_session_version_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.sessions_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    session_version_key key;
};

struct get_session_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::session> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace session_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.sessions_events.created";
inline constexpr std::string_view updated = "marketdata.v1.sessions_events.updated";
inline constexpr std::string_view deleted = "marketdata.v1.sessions_events.deleted";
}
/**
 * @brief The members of session_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const session_key&) {
    return std::tuple{
        utility::codec::field{"code", &session_key::code},
    };
}

/**
 * @brief The members of session_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const session_write&) {
    return std::tuple{
        utility::codec::field{"code", &session_write::code},
        utility::codec::field{"name", &session_write::name},
        utility::codec::field{"supplier_code", &session_write::supplier_code},
        utility::codec::field{"adapter_code", &session_write::adapter_code},
        utility::codec::field{"environment", &session_write::environment},
        utility::codec::field{"endpoint", &session_write::endpoint},
        utility::codec::field{"credential_ref", &session_write::credential_ref},
        utility::codec::field{"settings", &session_write::settings},
        utility::codec::field{"enabled", &session_write::enabled},
        utility::codec::field{"description", &session_write::description},
    };
}

/**
 * @brief The members of session_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const session_change&) {
    return std::tuple{
        utility::codec::field{"write", &session_change::write},
        utility::codec::field{"precondition", &session_change::precondition},
    };
}

/**
 * @brief The members of session_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const session_removal&) {
    return std::tuple{
        utility::codec::field{"key", &session_removal::key},
        utility::codec::field{"precondition", &session_removal::precondition},
    };
}

/**
 * @brief The members of session_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const session_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &session_lookup::key},
        utility::codec::field{"session", &session_lookup::session},
    };
}

/**
 * @brief The members of sessions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const sessions_filter&) {
    return std::tuple{
        utility::codec::field{"supplier_code", &sessions_filter::supplier_code},
        utility::codec::field{"adapter_code", &sessions_filter::adapter_code},
        utility::codec::field{"environment", &sessions_filter::environment},
        utility::codec::field{"code_one_of", &sessions_filter::code_one_of},
        utility::codec::field{"supplier_code_one_of", &sessions_filter::supplier_code_one_of},
        utility::codec::field{"adapter_code_one_of", &sessions_filter::adapter_code_one_of},
        utility::codec::field{"environment_one_of", &sessions_filter::environment_one_of},
        utility::codec::field{"search", &sessions_filter::search},
    };
}

/**
 * @brief The members of session_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const session_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &session_event::event_id},
        utility::codec::field{"key", &session_event::key},
        utility::codec::field{"action", &session_event::action},
        utility::codec::field{"version", &session_event::version},
        utility::codec::field{"occurred_at", &session_event::occurred_at},
        utility::codec::field{"correlation_id", &session_event::correlation_id},
    };
}

/**
 * @brief The members of session_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const session_version_key&) {
    return std::tuple{
        utility::codec::field{"session", &session_version_key::session},
        utility::codec::field{"version", &session_version_key::version},
    };
}

/**
 * @brief The members of session_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const session_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &session_versions_filter::version},
        utility::codec::field{"from_version", &session_versions_filter::from_version},
        utility::codec::field{"to_version", &session_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_sessions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_sessions_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_sessions_request::offset},
        utility::codec::field{"limit", &list_sessions_request::limit},
        utility::codec::field{"order", &list_sessions_request::order},
        utility::codec::field{"filter", &list_sessions_request::filter},
        utility::codec::field{"as_of", &list_sessions_request::as_of},
    };
}

/**
 * @brief The members of list_sessions_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_sessions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_sessions_response::result},
        utility::codec::field{"sessions", &list_sessions_response::sessions},
        utility::codec::field{"total", &list_sessions_response::total},
    };
}

/**
 * @brief The members of get_session_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_session_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_session_request::key},
    };
}

/**
 * @brief The members of get_session_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_session_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_session_response::result},
        utility::codec::field{"session", &get_session_response::session},
    };
}

/**
 * @brief The members of get_many_sessions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_sessions_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_sessions_request::keys},
    };
}

/**
 * @brief The members of get_many_sessions_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_sessions_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_sessions_response::result},
        utility::codec::field{"entries", &get_many_sessions_response::entries},
    };
}

/**
 * @brief The members of put_session_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_session_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_session_request::change},
        utility::codec::field{"intent", &put_session_request::intent},
    };
}

/**
 * @brief The members of put_session_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_session_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_session_response::result},
        utility::codec::field{"session", &put_session_response::session},
    };
}

/**
 * @brief The members of put_many_sessions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_sessions_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_sessions_request::changes},
        utility::codec::field{"intent", &put_many_sessions_request::intent},
    };
}

/**
 * @brief The members of put_many_sessions_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_sessions_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_sessions_response::result},
        utility::codec::field{"sessions", &put_many_sessions_response::sessions},
    };
}

/**
 * @brief The members of delete_session_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_session_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_session_request::removal},
        utility::codec::field{"intent", &delete_session_request::intent},
    };
}

/**
 * @brief The members of delete_session_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_session_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_session_response::result},
    };
}

/**
 * @brief The members of delete_many_sessions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_sessions_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_sessions_request::removals},
        utility::codec::field{"intent", &delete_many_sessions_request::intent},
    };
}

/**
 * @brief The members of delete_many_sessions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_sessions_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_sessions_response::result},
    };
}

/**
 * @brief The members of list_session_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_session_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_session_versions_request::key},
        utility::codec::field{"offset", &list_session_versions_request::offset},
        utility::codec::field{"limit", &list_session_versions_request::limit},
        utility::codec::field{"order", &list_session_versions_request::order},
        utility::codec::field{"filter", &list_session_versions_request::filter},
    };
}

/**
 * @brief The members of list_session_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_session_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_session_versions_response::result},
        utility::codec::field{"versions", &list_session_versions_response::versions},
        utility::codec::field{"total", &list_session_versions_response::total},
    };
}

/**
 * @brief The members of get_session_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_session_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_session_version_request::key},
    };
}

/**
 * @brief The members of get_session_version_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_session_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_session_version_response::result},
        utility::codec::field{"version", &get_session_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::session_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::session_key& v);
    static ores::marketdata::messaging::session_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::session_key& v);
    static ores::marketdata::messaging::session_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::session_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::session_write& v);
    static ores::marketdata::messaging::session_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::session_write& v);
    static ores::marketdata::messaging::session_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::session_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::session_change& v);
    static ores::marketdata::messaging::session_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::session_change& v);
    static ores::marketdata::messaging::session_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::session_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::session_removal& v);
    static ores::marketdata::messaging::session_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::session_removal& v);
    static ores::marketdata::messaging::session_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::session_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::session_lookup& v);
    static ores::marketdata::messaging::session_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::session_lookup& v);
    static ores::marketdata::messaging::session_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::sessions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::sessions_filter& v);
    static ores::marketdata::messaging::sessions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::sessions_filter& v);
    static ores::marketdata::messaging::sessions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::session_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::session_event& v);
    static ores::marketdata::messaging::session_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::session_event& v);
    static ores::marketdata::messaging::session_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::session_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::session_version_key& v);
    static ores::marketdata::messaging::session_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::session_version_key& v);
    static ores::marketdata::messaging::session_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::session_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::session_versions_filter& v);
    static ores::marketdata::messaging::session_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::session_versions_filter& v);
    static ores::marketdata::messaging::session_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::list_sessions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::list_sessions_request& v);
    static ores::marketdata::messaging::list_sessions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::list_sessions_request& v);
    static ores::marketdata::messaging::list_sessions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::list_sessions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::list_sessions_response& v);
    static ores::marketdata::messaging::list_sessions_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::list_sessions_response& v);
    static ores::marketdata::messaging::list_sessions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::get_session_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_session_request& v);
    static ores::marketdata::messaging::get_session_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::get_session_request& v);
    static ores::marketdata::messaging::get_session_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::get_session_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_session_response& v);
    static ores::marketdata::messaging::get_session_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::get_session_response& v);
    static ores::marketdata::messaging::get_session_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_sessions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_many_sessions_request& v);
    static ores::marketdata::messaging::get_many_sessions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_sessions_request& v);
    static ores::marketdata::messaging::get_many_sessions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_sessions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_many_sessions_response& v);
    static ores::marketdata::messaging::get_many_sessions_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_sessions_response& v);
    static ores::marketdata::messaging::get_many_sessions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::put_session_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::put_session_request& v);
    static ores::marketdata::messaging::put_session_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::put_session_request& v);
    static ores::marketdata::messaging::put_session_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::put_session_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::put_session_response& v);
    static ores::marketdata::messaging::put_session_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::put_session_response& v);
    static ores::marketdata::messaging::put_session_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_sessions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::put_many_sessions_request& v);
    static ores::marketdata::messaging::put_many_sessions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_sessions_request& v);
    static ores::marketdata::messaging::put_many_sessions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_sessions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::put_many_sessions_response& v);
    static ores::marketdata::messaging::put_many_sessions_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_sessions_response& v);
    static ores::marketdata::messaging::put_many_sessions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::delete_session_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::delete_session_request& v);
    static ores::marketdata::messaging::delete_session_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::delete_session_request& v);
    static ores::marketdata::messaging::delete_session_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_session_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::delete_session_response& v);
    static ores::marketdata::messaging::delete_session_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::delete_session_response& v);
    static ores::marketdata::messaging::delete_session_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_sessions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_sessions_request& v);
    static ores::marketdata::messaging::delete_many_sessions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_sessions_request& v);
    static ores::marketdata::messaging::delete_many_sessions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_sessions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_sessions_response& v);
    static ores::marketdata::messaging::delete_many_sessions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_sessions_response& v);
    static ores::marketdata::messaging::delete_many_sessions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_session_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_session_versions_request& v);
    static ores::marketdata::messaging::list_session_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_session_versions_request& v);
    static ores::marketdata::messaging::list_session_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_session_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_session_versions_response& v);
    static ores::marketdata::messaging::list_session_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_session_versions_response& v);
    static ores::marketdata::messaging::list_session_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_session_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_session_version_request& v);
    static ores::marketdata::messaging::get_session_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_session_version_request& v);
    static ores::marketdata::messaging::get_session_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_session_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_session_version_response& v);
    static ores::marketdata::messaging::get_session_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_session_version_response& v);
    static ores::marketdata::messaging::get_session_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
