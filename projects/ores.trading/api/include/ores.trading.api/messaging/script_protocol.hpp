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
#ifndef ORES_TRADING_API_MESSAGING_SCRIPT_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_SCRIPT_PROTOCOL_HPP

#include "ores.trading.api/domain/script_codec.hpp"
#include "ores.trading.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::trading::messaging {

struct script_key {
    std::string name;

    friend bool operator==(const script_key&, const script_key&) = default;
};

struct script_write {
    std::string name;
    std::optional<std::string> product_tag;
    std::optional<std::string> description;

    friend bool operator==(const script_write&, const script_write&) = default;
};

struct script_change {
    script_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const script_change&, const script_change&) = default;
};

struct script_removal {
    script_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const script_removal&, const script_removal&) = default;
};

struct script_lookup {
    script_key key;
    std::optional<ores::trading::domain::script> script;

    friend bool operator==(const script_lookup&, const script_lookup&) = default;
};

struct scripts_filter {
    std::optional<std::vector<std::string>> name_one_of;

    friend bool operator==(const scripts_filter&, const scripts_filter&) = default;
};

struct script_event {
    boost::uuids::uuid event_id;
    script_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const script_event&, const script_event&) = default;
};

struct script_version_key {
    script_key script;
    std::uint32_t version{};

    friend bool operator==(const script_version_key&, const script_version_key&) = default;
};

struct script_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const script_versions_filter&, const script_versions_filter&) = default;
};

struct list_scripts_request {
    using response_type = struct list_scripts_response;
    static constexpr std::string_view nats_subject = "trading.v1.scripts.list";
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
    std::optional<scripts_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_scripts_request&, const list_scripts_request&) = default;
};

struct list_scripts_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::script> scripts;
    std::uint64_t total{};

    friend bool operator==(const list_scripts_response&, const list_scripts_response&) = default;
};

struct get_script_request {
    using response_type = struct get_script_response;
    static constexpr std::string_view nats_subject = "trading.v1.scripts.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    script_key key;

    friend bool operator==(const get_script_request&, const get_script_request&) = default;
};

struct get_script_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::script> script;

    friend bool operator==(const get_script_response&, const get_script_response&) = default;
};

struct get_many_scripts_request {
    using response_type = struct get_many_scripts_response;
    static constexpr std::string_view nats_subject = "trading.v1.scripts.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<script_key> keys;

    friend bool operator==(const get_many_scripts_request&,
                           const get_many_scripts_request&) = default;
};

struct get_many_scripts_response {
    ores::utility::domain::result result;
    std::vector<script_lookup> entries;

    friend bool operator==(const get_many_scripts_response&,
                           const get_many_scripts_response&) = default;
};

struct put_script_request {
    using response_type = struct put_script_response;
    static constexpr std::string_view nats_subject = "trading.v1.scripts.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    script_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_script_request&, const put_script_request&) = default;
};

struct put_script_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::script> script;

    friend bool operator==(const put_script_response&, const put_script_response&) = default;
};

struct put_many_scripts_request {
    using response_type = struct put_many_scripts_response;
    static constexpr std::string_view nats_subject = "trading.v1.scripts.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<script_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_scripts_request&,
                           const put_many_scripts_request&) = default;
};

struct put_many_scripts_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::script> scripts;

    friend bool operator==(const put_many_scripts_response&,
                           const put_many_scripts_response&) = default;
};

struct delete_script_request {
    using response_type = struct delete_script_response;
    static constexpr std::string_view nats_subject = "trading.v1.scripts.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    script_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_script_request&, const delete_script_request&) = default;
};

struct delete_script_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_script_response&, const delete_script_response&) = default;
};

struct delete_many_scripts_request {
    using response_type = struct delete_many_scripts_response;
    static constexpr std::string_view nats_subject = "trading.v1.scripts.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<script_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_scripts_request&,
                           const delete_many_scripts_request&) = default;
};

struct delete_many_scripts_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_scripts_response&,
                           const delete_many_scripts_response&) = default;
};

struct list_script_versions_request {
    using response_type = struct list_script_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.scripts_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    script_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<script_versions_filter> filter;

    friend bool operator==(const list_script_versions_request&,
                           const list_script_versions_request&) = default;
};

struct list_script_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::script> versions;
    std::uint64_t total{};

    friend bool operator==(const list_script_versions_response&,
                           const list_script_versions_response&) = default;
};

struct get_script_version_request {
    using response_type = struct get_script_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.scripts_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    script_version_key key;

    friend bool operator==(const get_script_version_request&,
                           const get_script_version_request&) = default;
};

struct get_script_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::script> version;

    friend bool operator==(const get_script_version_response&,
                           const get_script_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace script_event_subjects {
inline constexpr std::string_view created = "trading.v1.events.scripts.created";
inline constexpr std::string_view updated = "trading.v1.events.scripts.updated";
inline constexpr std::string_view deleted = "trading.v1.events.scripts.deleted";
}
/**
 * @brief The members of script_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<script_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"name", &script_key::name},
    };
}

/**
 * @brief The members of script_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<script_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"name", &script_write::name},
        utility::codec::field{"product_tag", &script_write::product_tag},
        utility::codec::field{"description", &script_write::description},
    };
}

/**
 * @brief The members of script_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<script_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &script_change::write},
        utility::codec::field{"precondition", &script_change::precondition},
    };
}

/**
 * @brief The members of script_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<script_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &script_removal::key},
        utility::codec::field{"precondition", &script_removal::precondition},
    };
}

/**
 * @brief The members of script_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<script_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &script_lookup::key},
        utility::codec::field{"script", &script_lookup::script},
    };
}

/**
 * @brief The members of scripts_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<scripts_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"name_one_of", &scripts_filter::name_one_of},
    };
}

/**
 * @brief The members of script_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<script_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &script_event::event_id},
        utility::codec::field{"key", &script_event::key},
        utility::codec::field{"action", &script_event::action},
        utility::codec::field{"version", &script_event::version},
        utility::codec::field{"occurred_at", &script_event::occurred_at},
        utility::codec::field{"correlation_id", &script_event::correlation_id},
    };
}

/**
 * @brief The members of script_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<script_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"script", &script_version_key::script},
        utility::codec::field{"version", &script_version_key::version},
    };
}

/**
 * @brief The members of script_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<script_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &script_versions_filter::version},
        utility::codec::field{"from_version", &script_versions_filter::from_version},
        utility::codec::field{"to_version", &script_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_scripts_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_scripts_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_scripts_request::offset},
        utility::codec::field{"limit", &list_scripts_request::limit},
        utility::codec::field{"order", &list_scripts_request::order},
        utility::codec::field{"filter", &list_scripts_request::filter},
        utility::codec::field{"as_of", &list_scripts_request::as_of},
    };
}

/**
 * @brief The members of list_scripts_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_scripts_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_scripts_response::result},
        utility::codec::field{"scripts", &list_scripts_response::scripts},
        utility::codec::field{"total", &list_scripts_response::total},
    };
}

/**
 * @brief The members of get_script_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_script_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_script_request::key},
    };
}

/**
 * @brief The members of get_script_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_script_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_script_response::result},
        utility::codec::field{"script", &get_script_response::script},
    };
}

/**
 * @brief The members of get_many_scripts_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_scripts_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_scripts_request::keys},
    };
}

/**
 * @brief The members of get_many_scripts_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_scripts_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_scripts_response::result},
        utility::codec::field{"entries", &get_many_scripts_response::entries},
    };
}

/**
 * @brief The members of put_script_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_script_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_script_request::change},
        utility::codec::field{"intent", &put_script_request::intent},
    };
}

/**
 * @brief The members of put_script_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_script_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_script_response::result},
        utility::codec::field{"script", &put_script_response::script},
    };
}

/**
 * @brief The members of put_many_scripts_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_scripts_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_scripts_request::changes},
        utility::codec::field{"intent", &put_many_scripts_request::intent},
    };
}

/**
 * @brief The members of put_many_scripts_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_scripts_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_scripts_response::result},
        utility::codec::field{"scripts", &put_many_scripts_response::scripts},
    };
}

/**
 * @brief The members of delete_script_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_script_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_script_request::removal},
        utility::codec::field{"intent", &delete_script_request::intent},
    };
}

/**
 * @brief The members of delete_script_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_script_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_script_response::result},
    };
}

/**
 * @brief The members of delete_many_scripts_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_scripts_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_scripts_request::removals},
        utility::codec::field{"intent", &delete_many_scripts_request::intent},
    };
}

/**
 * @brief The members of delete_many_scripts_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_scripts_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_scripts_response::result},
    };
}

/**
 * @brief The members of list_script_versions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_script_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_script_versions_request::key},
        utility::codec::field{"offset", &list_script_versions_request::offset},
        utility::codec::field{"limit", &list_script_versions_request::limit},
        utility::codec::field{"order", &list_script_versions_request::order},
        utility::codec::field{"filter", &list_script_versions_request::filter},
    };
}

/**
 * @brief The members of list_script_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_script_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_script_versions_response::result},
        utility::codec::field{"versions", &list_script_versions_response::versions},
        utility::codec::field{"total", &list_script_versions_response::total},
    };
}

/**
 * @brief The members of get_script_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_script_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_script_version_request::key},
    };
}

/**
 * @brief The members of get_script_version_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_script_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_script_version_response::result},
        utility::codec::field{"version", &get_script_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::script_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::script_key& v);
    static ores::trading::messaging::script_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::script_key& v);
    static ores::trading::messaging::script_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::script_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::script_write& v);
    static ores::trading::messaging::script_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::script_write& v);
    static ores::trading::messaging::script_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::script_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::script_change& v);
    static ores::trading::messaging::script_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::script_change& v);
    static ores::trading::messaging::script_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::script_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::script_removal& v);
    static ores::trading::messaging::script_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::script_removal& v);
    static ores::trading::messaging::script_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::script_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::script_lookup& v);
    static ores::trading::messaging::script_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::script_lookup& v);
    static ores::trading::messaging::script_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::scripts_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::scripts_filter& v);
    static ores::trading::messaging::scripts_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::scripts_filter& v);
    static ores::trading::messaging::scripts_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::script_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::script_event& v);
    static ores::trading::messaging::script_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::script_event& v);
    static ores::trading::messaging::script_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::script_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::script_version_key& v);
    static ores::trading::messaging::script_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::script_version_key& v);
    static ores::trading::messaging::script_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::script_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::script_versions_filter& v);
    static ores::trading::messaging::script_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::script_versions_filter& v);
    static ores::trading::messaging::script_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_scripts_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_scripts_request& v);
    static ores::trading::messaging::list_scripts_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::list_scripts_request& v);
    static ores::trading::messaging::list_scripts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_scripts_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_scripts_response& v);
    static ores::trading::messaging::list_scripts_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::list_scripts_response& v);
    static ores::trading::messaging::list_scripts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_script_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_script_request& v);
    static ores::trading::messaging::get_script_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_script_request& v);
    static ores::trading::messaging::get_script_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_script_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_script_response& v);
    static ores::trading::messaging::get_script_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_script_response& v);
    static ores::trading::messaging::get_script_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_many_scripts_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_many_scripts_request& v);
    static ores::trading::messaging::get_many_scripts_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_many_scripts_request& v);
    static ores::trading::messaging::get_many_scripts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_many_scripts_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_many_scripts_response& v);
    static ores::trading::messaging::get_many_scripts_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_many_scripts_response& v);
    static ores::trading::messaging::get_many_scripts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_script_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_script_request& v);
    static ores::trading::messaging::put_script_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_script_request& v);
    static ores::trading::messaging::put_script_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_script_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_script_response& v);
    static ores::trading::messaging::put_script_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_script_response& v);
    static ores::trading::messaging::put_script_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_many_scripts_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_many_scripts_request& v);
    static ores::trading::messaging::put_many_scripts_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_many_scripts_request& v);
    static ores::trading::messaging::put_many_scripts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_many_scripts_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_many_scripts_response& v);
    static ores::trading::messaging::put_many_scripts_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_many_scripts_response& v);
    static ores::trading::messaging::put_many_scripts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_script_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_script_request& v);
    static ores::trading::messaging::delete_script_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::delete_script_request& v);
    static ores::trading::messaging::delete_script_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_script_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_script_response& v);
    static ores::trading::messaging::delete_script_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::delete_script_response& v);
    static ores::trading::messaging::delete_script_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_many_scripts_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_many_scripts_request& v);
    static ores::trading::messaging::delete_many_scripts_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_scripts_request& v);
    static ores::trading::messaging::delete_many_scripts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_many_scripts_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_many_scripts_response& v);
    static ores::trading::messaging::delete_many_scripts_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_scripts_response& v);
    static ores::trading::messaging::delete_many_scripts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_script_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_script_versions_request& v);
    static ores::trading::messaging::list_script_versions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_script_versions_request& v);
    static ores::trading::messaging::list_script_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_script_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_script_versions_response& v);
    static ores::trading::messaging::list_script_versions_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_script_versions_response& v);
    static ores::trading::messaging::list_script_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_script_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_script_version_request& v);
    static ores::trading::messaging::get_script_version_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_script_version_request& v);
    static ores::trading::messaging::get_script_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_script_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_script_version_response& v);
    static ores::trading::messaging::get_script_version_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_script_version_response& v);
    static ores::trading::messaging::get_script_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
