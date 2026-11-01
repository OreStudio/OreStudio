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
#ifndef ORES_VARIABILITY_API_MESSAGING_SYSTEM_SETTING_PROTOCOL_HPP
#define ORES_VARIABILITY_API_MESSAGING_SYSTEM_SETTING_PROTOCOL_HPP

#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.variability.api/domain/system_setting.hpp"
#include "ores.variability.api/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::variability::messaging {

struct system_setting_key {
    std::string name;
};

struct system_setting_write {
    boost::uuids::uuid id;
    std::string name;
    boost::uuids::uuid party_id;
    std::string value;
    std::string data_type;
    std::string description;
};

struct system_setting_change {
    system_setting_write write;
    ores::utility::domain::precondition precondition;
};

struct system_setting_removal {
    system_setting_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct system_setting_lookup {
    system_setting_key key;
    std::optional<ores::variability::domain::system_setting> system_setting;
};

struct system_settings_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct system_setting_event {
    boost::uuids::uuid event_id;
    system_setting_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct system_setting_version_key {
    system_setting_key system_setting;
    std::uint32_t version;
};

struct system_setting_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_system_settings_request {
    using response_type = struct list_system_settings_response;
    static constexpr std::string_view nats_subject = "variability.v1.system_settings.list";
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
    std::optional<system_settings_filter> filter;
    std::optional<std::string> as_of;
};

struct list_system_settings_response {
    ores::utility::domain::result result;
    std::vector<ores::variability::domain::system_setting> settings;
    std::uint64_t total;
};

struct get_system_setting_request {
    using response_type = struct get_system_setting_response;
    static constexpr std::string_view nats_subject = "variability.v1.system_settings.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    system_setting_key key;
};

struct get_system_setting_response {
    ores::utility::domain::result result;
    std::optional<ores::variability::domain::system_setting> system_setting;
};

struct get_many_system_settings_request {
    using response_type = struct get_many_system_settings_response;
    static constexpr std::string_view nats_subject = "variability.v1.system_settings.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<system_setting_key> keys;
};

struct get_many_system_settings_response {
    ores::utility::domain::result result;
    std::vector<system_setting_lookup> entries;
};

struct put_system_setting_request {
    using response_type = struct put_system_setting_response;
    static constexpr std::string_view nats_subject = "variability.v1.system_settings.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    system_setting_change change;
    ores::utility::domain::change_intent intent;
};

struct put_system_setting_response {
    ores::utility::domain::result result;
    std::optional<ores::variability::domain::system_setting> system_setting;
};

struct put_many_system_settings_request {
    using response_type = struct put_many_system_settings_response;
    static constexpr std::string_view nats_subject = "variability.v1.system_settings.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<system_setting_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_system_settings_response {
    ores::utility::domain::result result;
    std::vector<ores::variability::domain::system_setting> settings;
};

struct delete_system_setting_request {
    using response_type = struct delete_system_setting_response;
    static constexpr std::string_view nats_subject = "variability.v1.system_settings.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    system_setting_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_system_setting_response {
    ores::utility::domain::result result;
};

struct delete_many_system_settings_request {
    using response_type = struct delete_many_system_settings_response;
    static constexpr std::string_view nats_subject = "variability.v1.system_settings.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<system_setting_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_system_settings_response {
    ores::utility::domain::result result;
};

struct list_system_setting_versions_request {
    using response_type = struct list_system_setting_versions_response;
    static constexpr std::string_view nats_subject = "variability.v1.system_settings_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    system_setting_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<system_setting_versions_filter> filter;
};

struct list_system_setting_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::variability::domain::system_setting> versions;
    std::uint64_t total;
};

struct get_system_setting_version_request {
    using response_type = struct get_system_setting_version_response;
    static constexpr std::string_view nats_subject = "variability.v1.system_settings_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    system_setting_version_key key;
};

struct get_system_setting_version_response {
    ores::utility::domain::result result;
    std::optional<ores::variability::domain::system_setting> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace system_setting_event_subjects {
inline constexpr std::string_view created = "variability.v1.system_settings_events.created";
inline constexpr std::string_view updated = "variability.v1.system_settings_events.updated";
inline constexpr std::string_view deleted = "variability.v1.system_settings_events.deleted";
}
/**
 * @brief The members of system_setting_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const system_setting_key&) {
    return std::tuple{
        utility::codec::field{"name", &system_setting_key::name},
    };
}

/**
 * @brief The members of system_setting_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const system_setting_write&) {
    return std::tuple{
        utility::codec::field{"id", &system_setting_write::id},
        utility::codec::field{"name", &system_setting_write::name},
        utility::codec::field{"party_id", &system_setting_write::party_id},
        utility::codec::field{"value", &system_setting_write::value},
        utility::codec::field{"data_type", &system_setting_write::data_type},
        utility::codec::field{"description", &system_setting_write::description},
    };
}

/**
 * @brief The members of system_setting_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const system_setting_change&) {
    return std::tuple{
        utility::codec::field{"write", &system_setting_change::write},
        utility::codec::field{"precondition", &system_setting_change::precondition},
    };
}

/**
 * @brief The members of system_setting_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const system_setting_removal&) {
    return std::tuple{
        utility::codec::field{"key", &system_setting_removal::key},
        utility::codec::field{"precondition", &system_setting_removal::precondition},
    };
}

/**
 * @brief The members of system_setting_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const system_setting_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &system_setting_lookup::key},
        utility::codec::field{"system_setting", &system_setting_lookup::system_setting},
    };
}

/**
 * @brief The members of system_settings_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const system_settings_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &system_settings_filter::id_one_of},
    };
}

/**
 * @brief The members of system_setting_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const system_setting_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &system_setting_event::event_id},
        utility::codec::field{"key", &system_setting_event::key},
        utility::codec::field{"action", &system_setting_event::action},
        utility::codec::field{"version", &system_setting_event::version},
        utility::codec::field{"occurred_at", &system_setting_event::occurred_at},
        utility::codec::field{"correlation_id", &system_setting_event::correlation_id},
    };
}

/**
 * @brief The members of system_setting_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const system_setting_version_key&) {
    return std::tuple{
        utility::codec::field{"system_setting", &system_setting_version_key::system_setting},
        utility::codec::field{"version", &system_setting_version_key::version},
    };
}

/**
 * @brief The members of system_setting_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const system_setting_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &system_setting_versions_filter::version},
        utility::codec::field{"from_version", &system_setting_versions_filter::from_version},
        utility::codec::field{"to_version", &system_setting_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_system_settings_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_system_settings_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_system_settings_request::offset},
        utility::codec::field{"limit", &list_system_settings_request::limit},
        utility::codec::field{"order", &list_system_settings_request::order},
        utility::codec::field{"filter", &list_system_settings_request::filter},
        utility::codec::field{"as_of", &list_system_settings_request::as_of},
    };
}

/**
 * @brief The members of list_system_settings_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_system_settings_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_system_settings_response::result},
        utility::codec::field{"settings", &list_system_settings_response::settings},
        utility::codec::field{"total", &list_system_settings_response::total},
    };
}

/**
 * @brief The members of get_system_setting_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_system_setting_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_system_setting_request::key},
    };
}

/**
 * @brief The members of get_system_setting_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_system_setting_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_system_setting_response::result},
        utility::codec::field{"system_setting", &get_system_setting_response::system_setting},
    };
}

/**
 * @brief The members of get_many_system_settings_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_system_settings_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_system_settings_request::keys},
    };
}

/**
 * @brief The members of get_many_system_settings_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_system_settings_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_system_settings_response::result},
        utility::codec::field{"entries", &get_many_system_settings_response::entries},
    };
}

/**
 * @brief The members of put_system_setting_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_system_setting_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_system_setting_request::change},
        utility::codec::field{"intent", &put_system_setting_request::intent},
    };
}

/**
 * @brief The members of put_system_setting_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_system_setting_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_system_setting_response::result},
        utility::codec::field{"system_setting", &put_system_setting_response::system_setting},
    };
}

/**
 * @brief The members of put_many_system_settings_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_system_settings_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_system_settings_request::changes},
        utility::codec::field{"intent", &put_many_system_settings_request::intent},
    };
}

/**
 * @brief The members of put_many_system_settings_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_system_settings_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_system_settings_response::result},
        utility::codec::field{"settings", &put_many_system_settings_response::settings},
    };
}

/**
 * @brief The members of delete_system_setting_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_system_setting_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_system_setting_request::removal},
        utility::codec::field{"intent", &delete_system_setting_request::intent},
    };
}

/**
 * @brief The members of delete_system_setting_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_system_setting_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_system_setting_response::result},
    };
}

/**
 * @brief The members of delete_many_system_settings_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_system_settings_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_system_settings_request::removals},
        utility::codec::field{"intent", &delete_many_system_settings_request::intent},
    };
}

/**
 * @brief The members of delete_many_system_settings_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_system_settings_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_system_settings_response::result},
    };
}

/**
 * @brief The members of list_system_setting_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_system_setting_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_system_setting_versions_request::key},
        utility::codec::field{"offset", &list_system_setting_versions_request::offset},
        utility::codec::field{"limit", &list_system_setting_versions_request::limit},
        utility::codec::field{"order", &list_system_setting_versions_request::order},
        utility::codec::field{"filter", &list_system_setting_versions_request::filter},
    };
}

/**
 * @brief The members of list_system_setting_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_system_setting_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_system_setting_versions_response::result},
        utility::codec::field{"versions", &list_system_setting_versions_response::versions},
        utility::codec::field{"total", &list_system_setting_versions_response::total},
    };
}

/**
 * @brief The members of get_system_setting_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_system_setting_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_system_setting_version_request::key},
    };
}

/**
 * @brief The members of get_system_setting_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_system_setting_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_system_setting_version_response::result},
        utility::codec::field{"version", &get_system_setting_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_VARIABILITY_API_EXPORT wire_traits<ores::variability::messaging::system_setting_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::variability::messaging::system_setting_key& v);
    static ores::variability::messaging::system_setting_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::variability::messaging::system_setting_key& v);
    static ores::variability::messaging::system_setting_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT wire_traits<ores::variability::messaging::system_setting_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::variability::messaging::system_setting_write& v);
    static ores::variability::messaging::system_setting_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::variability::messaging::system_setting_write& v);
    static ores::variability::messaging::system_setting_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT
    wire_traits<ores::variability::messaging::system_setting_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::variability::messaging::system_setting_change& v);
    static ores::variability::messaging::system_setting_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::variability::messaging::system_setting_change& v);
    static ores::variability::messaging::system_setting_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT
    wire_traits<ores::variability::messaging::system_setting_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::variability::messaging::system_setting_removal& v);
    static ores::variability::messaging::system_setting_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::variability::messaging::system_setting_removal& v);
    static ores::variability::messaging::system_setting_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT
    wire_traits<ores::variability::messaging::system_setting_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::variability::messaging::system_setting_lookup& v);
    static ores::variability::messaging::system_setting_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::variability::messaging::system_setting_lookup& v);
    static ores::variability::messaging::system_setting_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT
    wire_traits<ores::variability::messaging::system_settings_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::variability::messaging::system_settings_filter& v);
    static ores::variability::messaging::system_settings_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::variability::messaging::system_settings_filter& v);
    static ores::variability::messaging::system_settings_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT wire_traits<ores::variability::messaging::system_setting_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::variability::messaging::system_setting_event& v);
    static ores::variability::messaging::system_setting_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::variability::messaging::system_setting_event& v);
    static ores::variability::messaging::system_setting_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT
    wire_traits<ores::variability::messaging::system_setting_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::variability::messaging::system_setting_version_key& v);
    static ores::variability::messaging::system_setting_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::variability::messaging::system_setting_version_key& v);
    static ores::variability::messaging::system_setting_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT
    wire_traits<ores::variability::messaging::system_setting_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::variability::messaging::system_setting_versions_filter& v);
    static ores::variability::messaging::system_setting_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::variability::messaging::system_setting_versions_filter& v);
    static ores::variability::messaging::system_setting_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT
    wire_traits<ores::variability::messaging::list_system_settings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::variability::messaging::list_system_settings_request& v);
    static ores::variability::messaging::list_system_settings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::variability::messaging::list_system_settings_request& v);
    static ores::variability::messaging::list_system_settings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT
    wire_traits<ores::variability::messaging::list_system_settings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::variability::messaging::list_system_settings_response& v);
    static ores::variability::messaging::list_system_settings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::variability::messaging::list_system_settings_response& v);
    static ores::variability::messaging::list_system_settings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT
    wire_traits<ores::variability::messaging::get_system_setting_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::variability::messaging::get_system_setting_request& v);
    static ores::variability::messaging::get_system_setting_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::variability::messaging::get_system_setting_request& v);
    static ores::variability::messaging::get_system_setting_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT
    wire_traits<ores::variability::messaging::get_system_setting_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::variability::messaging::get_system_setting_response& v);
    static ores::variability::messaging::get_system_setting_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::variability::messaging::get_system_setting_response& v);
    static ores::variability::messaging::get_system_setting_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT
    wire_traits<ores::variability::messaging::get_many_system_settings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::variability::messaging::get_many_system_settings_request& v);
    static ores::variability::messaging::get_many_system_settings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::variability::messaging::get_many_system_settings_request& v);
    static ores::variability::messaging::get_many_system_settings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT
    wire_traits<ores::variability::messaging::get_many_system_settings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::variability::messaging::get_many_system_settings_response& v);
    static ores::variability::messaging::get_many_system_settings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::variability::messaging::get_many_system_settings_response& v);
    static ores::variability::messaging::get_many_system_settings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT
    wire_traits<ores::variability::messaging::put_system_setting_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::variability::messaging::put_system_setting_request& v);
    static ores::variability::messaging::put_system_setting_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::variability::messaging::put_system_setting_request& v);
    static ores::variability::messaging::put_system_setting_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT
    wire_traits<ores::variability::messaging::put_system_setting_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::variability::messaging::put_system_setting_response& v);
    static ores::variability::messaging::put_system_setting_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::variability::messaging::put_system_setting_response& v);
    static ores::variability::messaging::put_system_setting_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT
    wire_traits<ores::variability::messaging::put_many_system_settings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::variability::messaging::put_many_system_settings_request& v);
    static ores::variability::messaging::put_many_system_settings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::variability::messaging::put_many_system_settings_request& v);
    static ores::variability::messaging::put_many_system_settings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT
    wire_traits<ores::variability::messaging::put_many_system_settings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::variability::messaging::put_many_system_settings_response& v);
    static ores::variability::messaging::put_many_system_settings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::variability::messaging::put_many_system_settings_response& v);
    static ores::variability::messaging::put_many_system_settings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT
    wire_traits<ores::variability::messaging::delete_system_setting_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::variability::messaging::delete_system_setting_request& v);
    static ores::variability::messaging::delete_system_setting_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::variability::messaging::delete_system_setting_request& v);
    static ores::variability::messaging::delete_system_setting_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT
    wire_traits<ores::variability::messaging::delete_system_setting_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::variability::messaging::delete_system_setting_response& v);
    static ores::variability::messaging::delete_system_setting_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::variability::messaging::delete_system_setting_response& v);
    static ores::variability::messaging::delete_system_setting_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT
    wire_traits<ores::variability::messaging::delete_many_system_settings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::variability::messaging::delete_many_system_settings_request& v);
    static ores::variability::messaging::delete_many_system_settings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::variability::messaging::delete_many_system_settings_request& v);
    static ores::variability::messaging::delete_many_system_settings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT
    wire_traits<ores::variability::messaging::delete_many_system_settings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::variability::messaging::delete_many_system_settings_response& v);
    static ores::variability::messaging::delete_many_system_settings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::variability::messaging::delete_many_system_settings_response& v);
    static ores::variability::messaging::delete_many_system_settings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT
    wire_traits<ores::variability::messaging::list_system_setting_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::variability::messaging::list_system_setting_versions_request& v);
    static ores::variability::messaging::list_system_setting_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::variability::messaging::list_system_setting_versions_request& v);
    static ores::variability::messaging::list_system_setting_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT
    wire_traits<ores::variability::messaging::list_system_setting_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::variability::messaging::list_system_setting_versions_response& v);
    static ores::variability::messaging::list_system_setting_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::variability::messaging::list_system_setting_versions_response& v);
    static ores::variability::messaging::list_system_setting_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT
    wire_traits<ores::variability::messaging::get_system_setting_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::variability::messaging::get_system_setting_version_request& v);
    static ores::variability::messaging::get_system_setting_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::variability::messaging::get_system_setting_version_request& v);
    static ores::variability::messaging::get_system_setting_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VARIABILITY_API_EXPORT
    wire_traits<ores::variability::messaging::get_system_setting_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::variability::messaging::get_system_setting_version_response& v);
    static ores::variability::messaging::get_system_setting_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::variability::messaging::get_system_setting_version_response& v);
    static ores::variability::messaging::get_system_setting_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
