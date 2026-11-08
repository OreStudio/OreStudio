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
#ifndef ORES_REPORTING_API_MESSAGING_ENGINE_SETTINGS_FIXINGS_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_ENGINE_SETTINGS_FIXINGS_PROTOCOL_HPP

#include "ores.reporting.api/domain/engine_settings_fixings_codec.hpp"
#include "ores.reporting.api/export.hpp"
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

namespace ores::reporting::messaging {

struct engine_settings_fixings_key {
    boost::uuids::uuid id;

    friend bool operator==(const engine_settings_fixings_key&,
                           const engine_settings_fixings_key&) = default;
};

struct engine_settings_fixings_write {
    boost::uuids::uuid id;
    boost::uuids::uuid engine_settings_id;
    std::optional<bool> imply_todays_fixings;
    std::optional<bool> include_todays_cash_flows;
    std::optional<bool> include_reference_date_events;
    std::optional<bool> enrich_index_fixings;
    std::optional<int> ignore_fixing_lag;
    std::optional<int> ignore_fixing_lead;

    friend bool operator==(const engine_settings_fixings_write&,
                           const engine_settings_fixings_write&) = default;
};

struct engine_settings_fixings_change {
    engine_settings_fixings_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const engine_settings_fixings_change&,
                           const engine_settings_fixings_change&) = default;
};

struct engine_settings_fixings_removal {
    engine_settings_fixings_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const engine_settings_fixings_removal&,
                           const engine_settings_fixings_removal&) = default;
};

struct engine_settings_fixings_lookup {
    engine_settings_fixings_key key;
    std::optional<ores::reporting::domain::engine_settings_fixings> engine_settings_fixings;

    friend bool operator==(const engine_settings_fixings_lookup&,
                           const engine_settings_fixings_lookup&) = default;
};

struct engine_settings_fixings_filter {
    std::optional<boost::uuids::uuid> engine_settings_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> engine_settings_id_one_of;

    friend bool operator==(const engine_settings_fixings_filter&,
                           const engine_settings_fixings_filter&) = default;
};

struct engine_settings_fixings_event {
    boost::uuids::uuid event_id;
    engine_settings_fixings_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const engine_settings_fixings_event&,
                           const engine_settings_fixings_event&) = default;
};

struct engine_settings_fixings_version_key {
    engine_settings_fixings_key engine_settings_fixings;
    std::uint32_t version;

    friend bool operator==(const engine_settings_fixings_version_key&,
                           const engine_settings_fixings_version_key&) = default;
};

struct engine_settings_fixings_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const engine_settings_fixings_versions_filter&,
                           const engine_settings_fixings_versions_filter&) = default;
};

struct list_engine_settings_fixings_request {
    using response_type = struct list_engine_settings_fixings_response;
    static constexpr std::string_view nats_subject = "reporting.v1.engine_settings_fixings.list";
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
    std::optional<engine_settings_fixings_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_engine_settings_fixings_request&,
                           const list_engine_settings_fixings_request&) = default;
};

struct list_engine_settings_fixings_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::engine_settings_fixings> engine_settings_fixings;
    std::uint64_t total;

    friend bool operator==(const list_engine_settings_fixings_response&,
                           const list_engine_settings_fixings_response&) = default;
};

struct get_engine_settings_fixings_request {
    using response_type = struct get_engine_settings_fixings_response;
    static constexpr std::string_view nats_subject = "reporting.v1.engine_settings_fixings.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    engine_settings_fixings_key key;

    friend bool operator==(const get_engine_settings_fixings_request&,
                           const get_engine_settings_fixings_request&) = default;
};

struct get_engine_settings_fixings_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::engine_settings_fixings> engine_settings_fixings;

    friend bool operator==(const get_engine_settings_fixings_response&,
                           const get_engine_settings_fixings_response&) = default;
};

struct get_many_engine_settings_fixings_request {
    using response_type = struct get_many_engine_settings_fixings_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.engine_settings_fixings.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<engine_settings_fixings_key> keys;

    friend bool operator==(const get_many_engine_settings_fixings_request&,
                           const get_many_engine_settings_fixings_request&) = default;
};

struct get_many_engine_settings_fixings_response {
    ores::utility::domain::result result;
    std::vector<engine_settings_fixings_lookup> entries;

    friend bool operator==(const get_many_engine_settings_fixings_response&,
                           const get_many_engine_settings_fixings_response&) = default;
};

struct put_engine_settings_fixings_request {
    using response_type = struct put_engine_settings_fixings_response;
    static constexpr std::string_view nats_subject = "reporting.v1.engine_settings_fixings.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    engine_settings_fixings_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_engine_settings_fixings_request&,
                           const put_engine_settings_fixings_request&) = default;
};

struct put_engine_settings_fixings_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::engine_settings_fixings> engine_settings_fixings;

    friend bool operator==(const put_engine_settings_fixings_response&,
                           const put_engine_settings_fixings_response&) = default;
};

struct put_many_engine_settings_fixings_request {
    using response_type = struct put_many_engine_settings_fixings_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.engine_settings_fixings.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<engine_settings_fixings_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_engine_settings_fixings_request&,
                           const put_many_engine_settings_fixings_request&) = default;
};

struct put_many_engine_settings_fixings_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::engine_settings_fixings> engine_settings_fixings;

    friend bool operator==(const put_many_engine_settings_fixings_response&,
                           const put_many_engine_settings_fixings_response&) = default;
};

struct delete_engine_settings_fixings_request {
    using response_type = struct delete_engine_settings_fixings_response;
    static constexpr std::string_view nats_subject = "reporting.v1.engine_settings_fixings.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    engine_settings_fixings_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_engine_settings_fixings_request&,
                           const delete_engine_settings_fixings_request&) = default;
};

struct delete_engine_settings_fixings_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_engine_settings_fixings_response&,
                           const delete_engine_settings_fixings_response&) = default;
};

struct delete_many_engine_settings_fixings_request {
    using response_type = struct delete_many_engine_settings_fixings_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.engine_settings_fixings.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<engine_settings_fixings_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_engine_settings_fixings_request&,
                           const delete_many_engine_settings_fixings_request&) = default;
};

struct delete_many_engine_settings_fixings_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_engine_settings_fixings_response&,
                           const delete_many_engine_settings_fixings_response&) = default;
};

struct list_by_engine_settings_id_engine_settings_fixings_request {
    using response_type = struct list_by_engine_settings_id_engine_settings_fixings_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.engine_settings_fixings.list_by_engine_settings_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid engine_settings_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<engine_settings_fixings_filter> filter;

    friend bool
    operator==(const list_by_engine_settings_id_engine_settings_fixings_request&,
               const list_by_engine_settings_id_engine_settings_fixings_request&) = default;
};

struct list_by_engine_settings_id_engine_settings_fixings_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::engine_settings_fixings> engine_settings_fixings;
    std::uint64_t total;

    friend bool
    operator==(const list_by_engine_settings_id_engine_settings_fixings_response&,
               const list_by_engine_settings_id_engine_settings_fixings_response&) = default;
};

struct list_engine_settings_fixings_versions_request {
    using response_type = struct list_engine_settings_fixings_versions_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.engine_settings_fixings_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    engine_settings_fixings_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<engine_settings_fixings_versions_filter> filter;

    friend bool operator==(const list_engine_settings_fixings_versions_request&,
                           const list_engine_settings_fixings_versions_request&) = default;
};

struct list_engine_settings_fixings_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::engine_settings_fixings> versions;
    std::uint64_t total;

    friend bool operator==(const list_engine_settings_fixings_versions_response&,
                           const list_engine_settings_fixings_versions_response&) = default;
};

struct get_engine_settings_fixings_version_request {
    using response_type = struct get_engine_settings_fixings_version_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.engine_settings_fixings_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    engine_settings_fixings_version_key key;

    friend bool operator==(const get_engine_settings_fixings_version_request&,
                           const get_engine_settings_fixings_version_request&) = default;
};

struct get_engine_settings_fixings_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::engine_settings_fixings> version;

    friend bool operator==(const get_engine_settings_fixings_version_response&,
                           const get_engine_settings_fixings_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace engine_settings_fixings_event_subjects {
inline constexpr std::string_view created = "reporting.v1.events.engine_settings_fixings.created";
inline constexpr std::string_view updated = "reporting.v1.events.engine_settings_fixings.updated";
inline constexpr std::string_view deleted = "reporting.v1.events.engine_settings_fixings.deleted";
}
/**
 * @brief The members of engine_settings_fixings_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<engine_settings_fixings_key>) {
    return std::tuple{
        utility::codec::field{"id", &engine_settings_fixings_key::id},
    };
}

/**
 * @brief The members of engine_settings_fixings_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<engine_settings_fixings_write>) {
    return std::tuple{
        utility::codec::field{"id", &engine_settings_fixings_write::id},
        utility::codec::field{"engine_settings_id",
                              &engine_settings_fixings_write::engine_settings_id},
        utility::codec::field{"imply_todays_fixings",
                              &engine_settings_fixings_write::imply_todays_fixings},
        utility::codec::field{"include_todays_cash_flows",
                              &engine_settings_fixings_write::include_todays_cash_flows},
        utility::codec::field{"include_reference_date_events",
                              &engine_settings_fixings_write::include_reference_date_events},
        utility::codec::field{"enrich_index_fixings",
                              &engine_settings_fixings_write::enrich_index_fixings},
        utility::codec::field{"ignore_fixing_lag",
                              &engine_settings_fixings_write::ignore_fixing_lag},
        utility::codec::field{"ignore_fixing_lead",
                              &engine_settings_fixings_write::ignore_fixing_lead},
    };
}

/**
 * @brief The members of engine_settings_fixings_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<engine_settings_fixings_change>) {
    return std::tuple{
        utility::codec::field{"write", &engine_settings_fixings_change::write},
        utility::codec::field{"precondition", &engine_settings_fixings_change::precondition},
    };
}

/**
 * @brief The members of engine_settings_fixings_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<engine_settings_fixings_removal>) {
    return std::tuple{
        utility::codec::field{"key", &engine_settings_fixings_removal::key},
        utility::codec::field{"precondition", &engine_settings_fixings_removal::precondition},
    };
}

/**
 * @brief The members of engine_settings_fixings_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<engine_settings_fixings_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &engine_settings_fixings_lookup::key},
        utility::codec::field{"engine_settings_fixings",
                              &engine_settings_fixings_lookup::engine_settings_fixings},
    };
}

/**
 * @brief The members of engine_settings_fixings_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<engine_settings_fixings_filter>) {
    return std::tuple{
        utility::codec::field{"engine_settings_id",
                              &engine_settings_fixings_filter::engine_settings_id},
        utility::codec::field{"id_one_of", &engine_settings_fixings_filter::id_one_of},
        utility::codec::field{"engine_settings_id_one_of",
                              &engine_settings_fixings_filter::engine_settings_id_one_of},
    };
}

/**
 * @brief The members of engine_settings_fixings_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<engine_settings_fixings_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &engine_settings_fixings_event::event_id},
        utility::codec::field{"key", &engine_settings_fixings_event::key},
        utility::codec::field{"action", &engine_settings_fixings_event::action},
        utility::codec::field{"version", &engine_settings_fixings_event::version},
        utility::codec::field{"occurred_at", &engine_settings_fixings_event::occurred_at},
        utility::codec::field{"correlation_id", &engine_settings_fixings_event::correlation_id},
    };
}

/**
 * @brief The members of engine_settings_fixings_version_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<engine_settings_fixings_version_key>) {
    return std::tuple{
        utility::codec::field{"engine_settings_fixings",
                              &engine_settings_fixings_version_key::engine_settings_fixings},
        utility::codec::field{"version", &engine_settings_fixings_version_key::version},
    };
}

/**
 * @brief The members of engine_settings_fixings_versions_filter, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<engine_settings_fixings_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &engine_settings_fixings_versions_filter::version},
        utility::codec::field{"from_version",
                              &engine_settings_fixings_versions_filter::from_version},
        utility::codec::field{"to_version", &engine_settings_fixings_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_engine_settings_fixings_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_engine_settings_fixings_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_engine_settings_fixings_request::offset},
        utility::codec::field{"limit", &list_engine_settings_fixings_request::limit},
        utility::codec::field{"order", &list_engine_settings_fixings_request::order},
        utility::codec::field{"filter", &list_engine_settings_fixings_request::filter},
        utility::codec::field{"as_of", &list_engine_settings_fixings_request::as_of},
    };
}

/**
 * @brief The members of list_engine_settings_fixings_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_engine_settings_fixings_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_engine_settings_fixings_response::result},
        utility::codec::field{"engine_settings_fixings",
                              &list_engine_settings_fixings_response::engine_settings_fixings},
        utility::codec::field{"total", &list_engine_settings_fixings_response::total},
    };
}

/**
 * @brief The members of get_engine_settings_fixings_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_engine_settings_fixings_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_engine_settings_fixings_request::key},
    };
}

/**
 * @brief The members of get_engine_settings_fixings_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_engine_settings_fixings_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_engine_settings_fixings_response::result},
        utility::codec::field{"engine_settings_fixings",
                              &get_engine_settings_fixings_response::engine_settings_fixings},
    };
}

/**
 * @brief The members of get_many_engine_settings_fixings_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_engine_settings_fixings_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_engine_settings_fixings_request::keys},
    };
}

/**
 * @brief The members of get_many_engine_settings_fixings_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_engine_settings_fixings_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_engine_settings_fixings_response::result},
        utility::codec::field{"entries", &get_many_engine_settings_fixings_response::entries},
    };
}

/**
 * @brief The members of put_engine_settings_fixings_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_engine_settings_fixings_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_engine_settings_fixings_request::change},
        utility::codec::field{"intent", &put_engine_settings_fixings_request::intent},
    };
}

/**
 * @brief The members of put_engine_settings_fixings_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_engine_settings_fixings_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_engine_settings_fixings_response::result},
        utility::codec::field{"engine_settings_fixings",
                              &put_engine_settings_fixings_response::engine_settings_fixings},
    };
}

/**
 * @brief The members of put_many_engine_settings_fixings_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_engine_settings_fixings_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_engine_settings_fixings_request::changes},
        utility::codec::field{"intent", &put_many_engine_settings_fixings_request::intent},
    };
}

/**
 * @brief The members of put_many_engine_settings_fixings_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_engine_settings_fixings_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_engine_settings_fixings_response::result},
        utility::codec::field{"engine_settings_fixings",
                              &put_many_engine_settings_fixings_response::engine_settings_fixings},
    };
}

/**
 * @brief The members of delete_engine_settings_fixings_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_engine_settings_fixings_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_engine_settings_fixings_request::removal},
        utility::codec::field{"intent", &delete_engine_settings_fixings_request::intent},
    };
}

/**
 * @brief The members of delete_engine_settings_fixings_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_engine_settings_fixings_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_engine_settings_fixings_response::result},
    };
}

/**
 * @brief The members of delete_many_engine_settings_fixings_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_engine_settings_fixings_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_engine_settings_fixings_request::removals},
        utility::codec::field{"intent", &delete_many_engine_settings_fixings_request::intent},
    };
}

/**
 * @brief The members of delete_many_engine_settings_fixings_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_engine_settings_fixings_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_engine_settings_fixings_response::result},
    };
}

/**
 * @brief The members of list_by_engine_settings_id_engine_settings_fixings_request, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_engine_settings_id_engine_settings_fixings_request>) {
    return std::tuple{
        utility::codec::field{
            "engine_settings_id",
            &list_by_engine_settings_id_engine_settings_fixings_request::engine_settings_id},
        utility::codec::field{"scope",
                              &list_by_engine_settings_id_engine_settings_fixings_request::scope},
        utility::codec::field{"offset",
                              &list_by_engine_settings_id_engine_settings_fixings_request::offset},
        utility::codec::field{"limit",
                              &list_by_engine_settings_id_engine_settings_fixings_request::limit},
        utility::codec::field{"order",
                              &list_by_engine_settings_id_engine_settings_fixings_request::order},
        utility::codec::field{"filter",
                              &list_by_engine_settings_id_engine_settings_fixings_request::filter},
    };
}

/**
 * @brief The members of list_by_engine_settings_id_engine_settings_fixings_response, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_engine_settings_id_engine_settings_fixings_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_by_engine_settings_id_engine_settings_fixings_response::result},
        utility::codec::field{
            "engine_settings_fixings",
            &list_by_engine_settings_id_engine_settings_fixings_response::engine_settings_fixings},
        utility::codec::field{"total",
                              &list_by_engine_settings_id_engine_settings_fixings_response::total},
    };
}

/**
 * @brief The members of list_engine_settings_fixings_versions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_engine_settings_fixings_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_engine_settings_fixings_versions_request::key},
        utility::codec::field{"offset", &list_engine_settings_fixings_versions_request::offset},
        utility::codec::field{"limit", &list_engine_settings_fixings_versions_request::limit},
        utility::codec::field{"order", &list_engine_settings_fixings_versions_request::order},
        utility::codec::field{"filter", &list_engine_settings_fixings_versions_request::filter},
    };
}

/**
 * @brief The members of list_engine_settings_fixings_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_engine_settings_fixings_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_engine_settings_fixings_versions_response::result},
        utility::codec::field{"versions",
                              &list_engine_settings_fixings_versions_response::versions},
        utility::codec::field{"total", &list_engine_settings_fixings_versions_response::total},
    };
}

/**
 * @brief The members of get_engine_settings_fixings_version_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_engine_settings_fixings_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_engine_settings_fixings_version_request::key},
    };
}

/**
 * @brief The members of get_engine_settings_fixings_version_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_engine_settings_fixings_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_engine_settings_fixings_version_response::result},
        utility::codec::field{"version", &get_engine_settings_fixings_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::engine_settings_fixings_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::engine_settings_fixings_key& v);
    static ores::reporting::messaging::engine_settings_fixings_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::engine_settings_fixings_key& v);
    static ores::reporting::messaging::engine_settings_fixings_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::engine_settings_fixings_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::engine_settings_fixings_write& v);
    static ores::reporting::messaging::engine_settings_fixings_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::engine_settings_fixings_write& v);
    static ores::reporting::messaging::engine_settings_fixings_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::engine_settings_fixings_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::engine_settings_fixings_change& v);
    static ores::reporting::messaging::engine_settings_fixings_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::engine_settings_fixings_change& v);
    static ores::reporting::messaging::engine_settings_fixings_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::engine_settings_fixings_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::engine_settings_fixings_removal& v);
    static ores::reporting::messaging::engine_settings_fixings_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::engine_settings_fixings_removal& v);
    static ores::reporting::messaging::engine_settings_fixings_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::engine_settings_fixings_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::engine_settings_fixings_lookup& v);
    static ores::reporting::messaging::engine_settings_fixings_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::engine_settings_fixings_lookup& v);
    static ores::reporting::messaging::engine_settings_fixings_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::engine_settings_fixings_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::engine_settings_fixings_filter& v);
    static ores::reporting::messaging::engine_settings_fixings_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::engine_settings_fixings_filter& v);
    static ores::reporting::messaging::engine_settings_fixings_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::engine_settings_fixings_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::engine_settings_fixings_event& v);
    static ores::reporting::messaging::engine_settings_fixings_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::engine_settings_fixings_event& v);
    static ores::reporting::messaging::engine_settings_fixings_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::engine_settings_fixings_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::engine_settings_fixings_version_key& v);
    static ores::reporting::messaging::engine_settings_fixings_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::engine_settings_fixings_version_key& v);
    static ores::reporting::messaging::engine_settings_fixings_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::engine_settings_fixings_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::engine_settings_fixings_versions_filter& v);
    static ores::reporting::messaging::engine_settings_fixings_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::engine_settings_fixings_versions_filter& v);
    static ores::reporting::messaging::engine_settings_fixings_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_engine_settings_fixings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_engine_settings_fixings_request& v);
    static ores::reporting::messaging::list_engine_settings_fixings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_engine_settings_fixings_request& v);
    static ores::reporting::messaging::list_engine_settings_fixings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_engine_settings_fixings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_engine_settings_fixings_response& v);
    static ores::reporting::messaging::list_engine_settings_fixings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_engine_settings_fixings_response& v);
    static ores::reporting::messaging::list_engine_settings_fixings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_engine_settings_fixings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_engine_settings_fixings_request& v);
    static ores::reporting::messaging::get_engine_settings_fixings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_engine_settings_fixings_request& v);
    static ores::reporting::messaging::get_engine_settings_fixings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_engine_settings_fixings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_engine_settings_fixings_response& v);
    static ores::reporting::messaging::get_engine_settings_fixings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_engine_settings_fixings_response& v);
    static ores::reporting::messaging::get_engine_settings_fixings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_engine_settings_fixings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_engine_settings_fixings_request& v);
    static ores::reporting::messaging::get_many_engine_settings_fixings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_engine_settings_fixings_request& v);
    static ores::reporting::messaging::get_many_engine_settings_fixings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_engine_settings_fixings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_engine_settings_fixings_response& v);
    static ores::reporting::messaging::get_many_engine_settings_fixings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_engine_settings_fixings_response& v);
    static ores::reporting::messaging::get_many_engine_settings_fixings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_engine_settings_fixings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_engine_settings_fixings_request& v);
    static ores::reporting::messaging::put_engine_settings_fixings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_engine_settings_fixings_request& v);
    static ores::reporting::messaging::put_engine_settings_fixings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_engine_settings_fixings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_engine_settings_fixings_response& v);
    static ores::reporting::messaging::put_engine_settings_fixings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_engine_settings_fixings_response& v);
    static ores::reporting::messaging::put_engine_settings_fixings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_engine_settings_fixings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_many_engine_settings_fixings_request& v);
    static ores::reporting::messaging::put_many_engine_settings_fixings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_engine_settings_fixings_request& v);
    static ores::reporting::messaging::put_many_engine_settings_fixings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_engine_settings_fixings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_many_engine_settings_fixings_response& v);
    static ores::reporting::messaging::put_many_engine_settings_fixings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_engine_settings_fixings_response& v);
    static ores::reporting::messaging::put_many_engine_settings_fixings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_engine_settings_fixings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_engine_settings_fixings_request& v);
    static ores::reporting::messaging::delete_engine_settings_fixings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_engine_settings_fixings_request& v);
    static ores::reporting::messaging::delete_engine_settings_fixings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_engine_settings_fixings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_engine_settings_fixings_response& v);
    static ores::reporting::messaging::delete_engine_settings_fixings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_engine_settings_fixings_response& v);
    static ores::reporting::messaging::delete_engine_settings_fixings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_engine_settings_fixings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_engine_settings_fixings_request& v);
    static ores::reporting::messaging::delete_many_engine_settings_fixings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_many_engine_settings_fixings_request& v);
    static ores::reporting::messaging::delete_many_engine_settings_fixings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_engine_settings_fixings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_engine_settings_fixings_response& v);
    static ores::reporting::messaging::delete_many_engine_settings_fixings_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::delete_many_engine_settings_fixings_response& v);
    static ores::reporting::messaging::delete_many_engine_settings_fixings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<
    ores::reporting::messaging::list_by_engine_settings_id_engine_settings_fixings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::
                   list_by_engine_settings_id_engine_settings_fixings_request& v);
    static ores::reporting::messaging::list_by_engine_settings_id_engine_settings_fixings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::
                      list_by_engine_settings_id_engine_settings_fixings_request& v);
    static ores::reporting::messaging::list_by_engine_settings_id_engine_settings_fixings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<
    ores::reporting::messaging::list_by_engine_settings_id_engine_settings_fixings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::
                   list_by_engine_settings_id_engine_settings_fixings_response& v);
    static ores::reporting::messaging::list_by_engine_settings_id_engine_settings_fixings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::
                      list_by_engine_settings_id_engine_settings_fixings_response& v);
    static ores::reporting::messaging::list_by_engine_settings_id_engine_settings_fixings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_engine_settings_fixings_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_engine_settings_fixings_versions_request& v);
    static ores::reporting::messaging::list_engine_settings_fixings_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_engine_settings_fixings_versions_request& v);
    static ores::reporting::messaging::list_engine_settings_fixings_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_engine_settings_fixings_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_engine_settings_fixings_versions_response& v);
    static ores::reporting::messaging::list_engine_settings_fixings_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_engine_settings_fixings_versions_response& v);
    static ores::reporting::messaging::list_engine_settings_fixings_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_engine_settings_fixings_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_engine_settings_fixings_version_request& v);
    static ores::reporting::messaging::get_engine_settings_fixings_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_engine_settings_fixings_version_request& v);
    static ores::reporting::messaging::get_engine_settings_fixings_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_engine_settings_fixings_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_engine_settings_fixings_version_response& v);
    static ores::reporting::messaging::get_engine_settings_fixings_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::get_engine_settings_fixings_version_response& v);
    static ores::reporting::messaging::get_engine_settings_fixings_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
