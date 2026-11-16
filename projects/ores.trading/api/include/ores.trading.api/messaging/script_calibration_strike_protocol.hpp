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
#ifndef ORES_TRADING_API_MESSAGING_SCRIPT_CALIBRATION_STRIKE_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_SCRIPT_CALIBRATION_STRIKE_PROTOCOL_HPP

#include "ores.trading.api/domain/script_calibration_strike_codec.hpp"
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

struct script_calibration_strike_key {
    std::string script_name;
    std::string purpose;
    int calibration_number{};
    int sequence_number{};

    friend bool operator==(const script_calibration_strike_key&,
                           const script_calibration_strike_key&) = default;
};

struct script_calibration_strike_write {
    std::string script_name;
    std::string purpose;
    int calibration_number{};
    int sequence_number{};
    std::string strike_variable;

    friend bool operator==(const script_calibration_strike_write&,
                           const script_calibration_strike_write&) = default;
};

struct script_calibration_strike_change {
    script_calibration_strike_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const script_calibration_strike_change&,
                           const script_calibration_strike_change&) = default;
};

struct script_calibration_strike_removal {
    script_calibration_strike_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const script_calibration_strike_removal&,
                           const script_calibration_strike_removal&) = default;
};

struct script_calibration_strike_lookup {
    script_calibration_strike_key key;
    std::optional<ores::trading::domain::script_calibration_strike> script_calibration_strike;

    friend bool operator==(const script_calibration_strike_lookup&,
                           const script_calibration_strike_lookup&) = default;
};

struct script_calibration_strike_event {
    boost::uuids::uuid event_id;
    script_calibration_strike_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const script_calibration_strike_event&,
                           const script_calibration_strike_event&) = default;
};

struct script_calibration_strike_version_key {
    script_calibration_strike_key script_calibration_strike;
    std::uint32_t version{};

    friend bool operator==(const script_calibration_strike_version_key&,
                           const script_calibration_strike_version_key&) = default;
};

struct script_calibration_strike_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const script_calibration_strike_versions_filter&,
                           const script_calibration_strike_versions_filter&) = default;
};

struct list_script_calibration_strikes_request {
    using response_type = struct list_script_calibration_strikes_response;
    static constexpr std::string_view nats_subject = "trading.v1.script_calibration_strikes.list";
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
    std::optional<std::string> as_of;

    friend bool operator==(const list_script_calibration_strikes_request&,
                           const list_script_calibration_strikes_request&) = default;
};

struct list_script_calibration_strikes_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::script_calibration_strike> strikes;
    std::uint64_t total{};

    friend bool operator==(const list_script_calibration_strikes_response&,
                           const list_script_calibration_strikes_response&) = default;
};

struct get_script_calibration_strike_request {
    using response_type = struct get_script_calibration_strike_response;
    static constexpr std::string_view nats_subject = "trading.v1.script_calibration_strikes.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    script_calibration_strike_key key;

    friend bool operator==(const get_script_calibration_strike_request&,
                           const get_script_calibration_strike_request&) = default;
};

struct get_script_calibration_strike_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::script_calibration_strike> script_calibration_strike;

    friend bool operator==(const get_script_calibration_strike_response&,
                           const get_script_calibration_strike_response&) = default;
};

struct get_many_script_calibration_strikes_request {
    using response_type = struct get_many_script_calibration_strikes_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.script_calibration_strikes.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<script_calibration_strike_key> keys;

    friend bool operator==(const get_many_script_calibration_strikes_request&,
                           const get_many_script_calibration_strikes_request&) = default;
};

struct get_many_script_calibration_strikes_response {
    ores::utility::domain::result result;
    std::vector<script_calibration_strike_lookup> entries;

    friend bool operator==(const get_many_script_calibration_strikes_response&,
                           const get_many_script_calibration_strikes_response&) = default;
};

struct put_script_calibration_strike_request {
    using response_type = struct put_script_calibration_strike_response;
    static constexpr std::string_view nats_subject = "trading.v1.script_calibration_strikes.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    script_calibration_strike_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_script_calibration_strike_request&,
                           const put_script_calibration_strike_request&) = default;
};

struct put_script_calibration_strike_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::script_calibration_strike> script_calibration_strike;

    friend bool operator==(const put_script_calibration_strike_response&,
                           const put_script_calibration_strike_response&) = default;
};

struct put_many_script_calibration_strikes_request {
    using response_type = struct put_many_script_calibration_strikes_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.script_calibration_strikes.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<script_calibration_strike_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_script_calibration_strikes_request&,
                           const put_many_script_calibration_strikes_request&) = default;
};

struct put_many_script_calibration_strikes_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::script_calibration_strike> strikes;

    friend bool operator==(const put_many_script_calibration_strikes_response&,
                           const put_many_script_calibration_strikes_response&) = default;
};

struct delete_script_calibration_strike_request {
    using response_type = struct delete_script_calibration_strike_response;
    static constexpr std::string_view nats_subject = "trading.v1.script_calibration_strikes.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    script_calibration_strike_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_script_calibration_strike_request&,
                           const delete_script_calibration_strike_request&) = default;
};

struct delete_script_calibration_strike_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_script_calibration_strike_response&,
                           const delete_script_calibration_strike_response&) = default;
};

struct delete_many_script_calibration_strikes_request {
    using response_type = struct delete_many_script_calibration_strikes_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.script_calibration_strikes.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<script_calibration_strike_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_script_calibration_strikes_request&,
                           const delete_many_script_calibration_strikes_request&) = default;
};

struct delete_many_script_calibration_strikes_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_script_calibration_strikes_response&,
                           const delete_many_script_calibration_strikes_response&) = default;
};

struct list_script_calibration_strike_versions_request {
    using response_type = struct list_script_calibration_strike_versions_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.script_calibration_strikes_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    script_calibration_strike_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<script_calibration_strike_versions_filter> filter;

    friend bool operator==(const list_script_calibration_strike_versions_request&,
                           const list_script_calibration_strike_versions_request&) = default;
};

struct list_script_calibration_strike_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::script_calibration_strike> versions;
    std::uint64_t total{};

    friend bool operator==(const list_script_calibration_strike_versions_response&,
                           const list_script_calibration_strike_versions_response&) = default;
};

struct get_script_calibration_strike_version_request {
    using response_type = struct get_script_calibration_strike_version_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.script_calibration_strikes_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    script_calibration_strike_version_key key;

    friend bool operator==(const get_script_calibration_strike_version_request&,
                           const get_script_calibration_strike_version_request&) = default;
};

struct get_script_calibration_strike_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::script_calibration_strike> version;

    friend bool operator==(const get_script_calibration_strike_version_response&,
                           const get_script_calibration_strike_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace script_calibration_strike_event_subjects {
inline constexpr std::string_view created = "trading.v1.events.script_calibration_strikes.created";
inline constexpr std::string_view updated = "trading.v1.events.script_calibration_strikes.updated";
inline constexpr std::string_view deleted = "trading.v1.events.script_calibration_strikes.deleted";
}
/**
 * @brief The members of script_calibration_strike_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<script_calibration_strike_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"script_name", &script_calibration_strike_key::script_name},
        utility::codec::field{"purpose", &script_calibration_strike_key::purpose},
        utility::codec::field{"calibration_number",
                              &script_calibration_strike_key::calibration_number},
        utility::codec::field{"sequence_number", &script_calibration_strike_key::sequence_number},
    };
}

/**
 * @brief The members of script_calibration_strike_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<script_calibration_strike_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"script_name", &script_calibration_strike_write::script_name},
        utility::codec::field{"purpose", &script_calibration_strike_write::purpose},
        utility::codec::field{"calibration_number",
                              &script_calibration_strike_write::calibration_number},
        utility::codec::field{"sequence_number", &script_calibration_strike_write::sequence_number},
        utility::codec::field{"strike_variable", &script_calibration_strike_write::strike_variable},
    };
}

/**
 * @brief The members of script_calibration_strike_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<script_calibration_strike_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &script_calibration_strike_change::write},
        utility::codec::field{"precondition", &script_calibration_strike_change::precondition},
    };
}

/**
 * @brief The members of script_calibration_strike_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<script_calibration_strike_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &script_calibration_strike_removal::key},
        utility::codec::field{"precondition", &script_calibration_strike_removal::precondition},
    };
}

/**
 * @brief The members of script_calibration_strike_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<script_calibration_strike_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &script_calibration_strike_lookup::key},
        utility::codec::field{"script_calibration_strike",
                              &script_calibration_strike_lookup::script_calibration_strike},
    };
}

/**
 * @brief The members of script_calibration_strike_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<script_calibration_strike_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &script_calibration_strike_event::event_id},
        utility::codec::field{"key", &script_calibration_strike_event::key},
        utility::codec::field{"action", &script_calibration_strike_event::action},
        utility::codec::field{"version", &script_calibration_strike_event::version},
        utility::codec::field{"occurred_at", &script_calibration_strike_event::occurred_at},
        utility::codec::field{"correlation_id", &script_calibration_strike_event::correlation_id},
    };
}

/**
 * @brief The members of script_calibration_strike_version_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<script_calibration_strike_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"script_calibration_strike",
                              &script_calibration_strike_version_key::script_calibration_strike},
        utility::codec::field{"version", &script_calibration_strike_version_key::version},
    };
}

/**
 * @brief The members of script_calibration_strike_versions_filter, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<script_calibration_strike_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &script_calibration_strike_versions_filter::version},
        utility::codec::field{"from_version",
                              &script_calibration_strike_versions_filter::from_version},
        utility::codec::field{"to_version", &script_calibration_strike_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_script_calibration_strikes_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_script_calibration_strikes_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_script_calibration_strikes_request::offset},
        utility::codec::field{"limit", &list_script_calibration_strikes_request::limit},
        utility::codec::field{"order", &list_script_calibration_strikes_request::order},
        utility::codec::field{"as_of", &list_script_calibration_strikes_request::as_of},
    };
}

/**
 * @brief The members of list_script_calibration_strikes_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_script_calibration_strikes_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_script_calibration_strikes_response::result},
        utility::codec::field{"strikes", &list_script_calibration_strikes_response::strikes},
        utility::codec::field{"total", &list_script_calibration_strikes_response::total},
    };
}

/**
 * @brief The members of get_script_calibration_strike_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_script_calibration_strike_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_script_calibration_strike_request::key},
    };
}

/**
 * @brief The members of get_script_calibration_strike_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_script_calibration_strike_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_script_calibration_strike_response::result},
        utility::codec::field{"script_calibration_strike",
                              &get_script_calibration_strike_response::script_calibration_strike},
    };
}

/**
 * @brief The members of get_many_script_calibration_strikes_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_script_calibration_strikes_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_script_calibration_strikes_request::keys},
    };
}

/**
 * @brief The members of get_many_script_calibration_strikes_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_script_calibration_strikes_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_script_calibration_strikes_response::result},
        utility::codec::field{"entries", &get_many_script_calibration_strikes_response::entries},
    };
}

/**
 * @brief The members of put_script_calibration_strike_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_script_calibration_strike_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_script_calibration_strike_request::change},
        utility::codec::field{"intent", &put_script_calibration_strike_request::intent},
    };
}

/**
 * @brief The members of put_script_calibration_strike_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_script_calibration_strike_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_script_calibration_strike_response::result},
        utility::codec::field{"script_calibration_strike",
                              &put_script_calibration_strike_response::script_calibration_strike},
    };
}

/**
 * @brief The members of put_many_script_calibration_strikes_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_script_calibration_strikes_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_script_calibration_strikes_request::changes},
        utility::codec::field{"intent", &put_many_script_calibration_strikes_request::intent},
    };
}

/**
 * @brief The members of put_many_script_calibration_strikes_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_script_calibration_strikes_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_script_calibration_strikes_response::result},
        utility::codec::field{"strikes", &put_many_script_calibration_strikes_response::strikes},
    };
}

/**
 * @brief The members of delete_script_calibration_strike_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_script_calibration_strike_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_script_calibration_strike_request::removal},
        utility::codec::field{"intent", &delete_script_calibration_strike_request::intent},
    };
}

/**
 * @brief The members of delete_script_calibration_strike_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_script_calibration_strike_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_script_calibration_strike_response::result},
    };
}

/**
 * @brief The members of delete_many_script_calibration_strikes_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_script_calibration_strikes_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals",
                              &delete_many_script_calibration_strikes_request::removals},
        utility::codec::field{"intent", &delete_many_script_calibration_strikes_request::intent},
    };
}

/**
 * @brief The members of delete_many_script_calibration_strikes_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_script_calibration_strikes_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_script_calibration_strikes_response::result},
    };
}

/**
 * @brief The members of list_script_calibration_strike_versions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_script_calibration_strike_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_script_calibration_strike_versions_request::key},
        utility::codec::field{"offset", &list_script_calibration_strike_versions_request::offset},
        utility::codec::field{"limit", &list_script_calibration_strike_versions_request::limit},
        utility::codec::field{"order", &list_script_calibration_strike_versions_request::order},
        utility::codec::field{"filter", &list_script_calibration_strike_versions_request::filter},
    };
}

/**
 * @brief The members of list_script_calibration_strike_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_script_calibration_strike_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_script_calibration_strike_versions_response::result},
        utility::codec::field{"versions",
                              &list_script_calibration_strike_versions_response::versions},
        utility::codec::field{"total", &list_script_calibration_strike_versions_response::total},
    };
}

/**
 * @brief The members of get_script_calibration_strike_version_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_script_calibration_strike_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_script_calibration_strike_version_request::key},
    };
}

/**
 * @brief The members of get_script_calibration_strike_version_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_script_calibration_strike_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_script_calibration_strike_version_response::result},
        utility::codec::field{"version", &get_script_calibration_strike_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::script_calibration_strike_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::script_calibration_strike_key& v);
    static ores::trading::messaging::script_calibration_strike_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::script_calibration_strike_key& v);
    static ores::trading::messaging::script_calibration_strike_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::script_calibration_strike_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::script_calibration_strike_write& v);
    static ores::trading::messaging::script_calibration_strike_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::script_calibration_strike_write& v);
    static ores::trading::messaging::script_calibration_strike_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::script_calibration_strike_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::script_calibration_strike_change& v);
    static ores::trading::messaging::script_calibration_strike_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::script_calibration_strike_change& v);
    static ores::trading::messaging::script_calibration_strike_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::script_calibration_strike_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::script_calibration_strike_removal& v);
    static ores::trading::messaging::script_calibration_strike_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::script_calibration_strike_removal& v);
    static ores::trading::messaging::script_calibration_strike_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::script_calibration_strike_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::script_calibration_strike_lookup& v);
    static ores::trading::messaging::script_calibration_strike_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::script_calibration_strike_lookup& v);
    static ores::trading::messaging::script_calibration_strike_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::script_calibration_strike_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::script_calibration_strike_event& v);
    static ores::trading::messaging::script_calibration_strike_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::script_calibration_strike_event& v);
    static ores::trading::messaging::script_calibration_strike_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::script_calibration_strike_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::script_calibration_strike_version_key& v);
    static ores::trading::messaging::script_calibration_strike_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::script_calibration_strike_version_key& v);
    static ores::trading::messaging::script_calibration_strike_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::script_calibration_strike_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::script_calibration_strike_versions_filter& v);
    static ores::trading::messaging::script_calibration_strike_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::script_calibration_strike_versions_filter& v);
    static ores::trading::messaging::script_calibration_strike_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_script_calibration_strikes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_script_calibration_strikes_request& v);
    static ores::trading::messaging::list_script_calibration_strikes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_script_calibration_strikes_request& v);
    static ores::trading::messaging::list_script_calibration_strikes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_script_calibration_strikes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_script_calibration_strikes_response& v);
    static ores::trading::messaging::list_script_calibration_strikes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_script_calibration_strikes_response& v);
    static ores::trading::messaging::list_script_calibration_strikes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_script_calibration_strike_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_script_calibration_strike_request& v);
    static ores::trading::messaging::get_script_calibration_strike_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_script_calibration_strike_request& v);
    static ores::trading::messaging::get_script_calibration_strike_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_script_calibration_strike_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_script_calibration_strike_response& v);
    static ores::trading::messaging::get_script_calibration_strike_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_script_calibration_strike_response& v);
    static ores::trading::messaging::get_script_calibration_strike_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_script_calibration_strikes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_script_calibration_strikes_request& v);
    static ores::trading::messaging::get_many_script_calibration_strikes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_script_calibration_strikes_request& v);
    static ores::trading::messaging::get_many_script_calibration_strikes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_script_calibration_strikes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_script_calibration_strikes_response& v);
    static ores::trading::messaging::get_many_script_calibration_strikes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_script_calibration_strikes_response& v);
    static ores::trading::messaging::get_many_script_calibration_strikes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_script_calibration_strike_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_script_calibration_strike_request& v);
    static ores::trading::messaging::put_script_calibration_strike_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_script_calibration_strike_request& v);
    static ores::trading::messaging::put_script_calibration_strike_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_script_calibration_strike_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_script_calibration_strike_response& v);
    static ores::trading::messaging::put_script_calibration_strike_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_script_calibration_strike_response& v);
    static ores::trading::messaging::put_script_calibration_strike_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_script_calibration_strikes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_script_calibration_strikes_request& v);
    static ores::trading::messaging::put_many_script_calibration_strikes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_script_calibration_strikes_request& v);
    static ores::trading::messaging::put_many_script_calibration_strikes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_script_calibration_strikes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_script_calibration_strikes_response& v);
    static ores::trading::messaging::put_many_script_calibration_strikes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_script_calibration_strikes_response& v);
    static ores::trading::messaging::put_many_script_calibration_strikes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_script_calibration_strike_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_script_calibration_strike_request& v);
    static ores::trading::messaging::delete_script_calibration_strike_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_script_calibration_strike_request& v);
    static ores::trading::messaging::delete_script_calibration_strike_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_script_calibration_strike_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_script_calibration_strike_response& v);
    static ores::trading::messaging::delete_script_calibration_strike_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_script_calibration_strike_response& v);
    static ores::trading::messaging::delete_script_calibration_strike_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_script_calibration_strikes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_script_calibration_strikes_request& v);
    static ores::trading::messaging::delete_many_script_calibration_strikes_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::delete_many_script_calibration_strikes_request& v);
    static ores::trading::messaging::delete_many_script_calibration_strikes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_script_calibration_strikes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_script_calibration_strikes_response& v);
    static ores::trading::messaging::delete_many_script_calibration_strikes_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::delete_many_script_calibration_strikes_response& v);
    static ores::trading::messaging::delete_many_script_calibration_strikes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_script_calibration_strike_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_script_calibration_strike_versions_request& v);
    static ores::trading::messaging::list_script_calibration_strike_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_script_calibration_strike_versions_request& v);
    static ores::trading::messaging::list_script_calibration_strike_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_script_calibration_strike_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_script_calibration_strike_versions_response& v);
    static ores::trading::messaging::list_script_calibration_strike_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_script_calibration_strike_versions_response& v);
    static ores::trading::messaging::list_script_calibration_strike_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_script_calibration_strike_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_script_calibration_strike_version_request& v);
    static ores::trading::messaging::get_script_calibration_strike_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_script_calibration_strike_version_request& v);
    static ores::trading::messaging::get_script_calibration_strike_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_script_calibration_strike_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_script_calibration_strike_version_response& v);
    static ores::trading::messaging::get_script_calibration_strike_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::get_script_calibration_strike_version_response& v);
    static ores::trading::messaging::get_script_calibration_strike_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
