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
#ifndef ORES_REFDATA_API_MESSAGING_INFLATION_SEASONALITY_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_INFLATION_SEASONALITY_PROTOCOL_HPP

#include "ores.refdata.api/domain/inflation_seasonality.hpp"
#include "ores.refdata.api/export.hpp"
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

namespace ores::refdata::messaging {

struct inflation_seasonality_key {
    boost::uuids::uuid id;

    friend bool operator==(const inflation_seasonality_key&,
                           const inflation_seasonality_key&) = default;
};

struct inflation_seasonality_write {
    boost::uuids::uuid id;
    boost::uuids::uuid curve_definition_id;
    std::string base_date;
    std::string frequency;

    friend bool operator==(const inflation_seasonality_write&,
                           const inflation_seasonality_write&) = default;
};

struct inflation_seasonality_change {
    inflation_seasonality_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const inflation_seasonality_change&,
                           const inflation_seasonality_change&) = default;
};

struct inflation_seasonality_removal {
    inflation_seasonality_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const inflation_seasonality_removal&,
                           const inflation_seasonality_removal&) = default;
};

struct inflation_seasonality_lookup {
    inflation_seasonality_key key;
    std::optional<ores::refdata::domain::inflation_seasonality> inflation_seasonality;

    friend bool operator==(const inflation_seasonality_lookup&,
                           const inflation_seasonality_lookup&) = default;
};

struct inflation_seasonalities_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;

    friend bool operator==(const inflation_seasonalities_filter&,
                           const inflation_seasonalities_filter&) = default;
};

struct inflation_seasonality_event {
    boost::uuids::uuid event_id;
    inflation_seasonality_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const inflation_seasonality_event&,
                           const inflation_seasonality_event&) = default;
};

struct inflation_seasonality_version_key {
    inflation_seasonality_key inflation_seasonality;
    std::uint32_t version;

    friend bool operator==(const inflation_seasonality_version_key&,
                           const inflation_seasonality_version_key&) = default;
};

struct inflation_seasonality_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const inflation_seasonality_versions_filter&,
                           const inflation_seasonality_versions_filter&) = default;
};

struct list_inflation_seasonalities_request {
    using response_type = struct list_inflation_seasonalities_response;
    static constexpr std::string_view nats_subject = "refdata.v1.inflation_seasonalities.list";
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
    std::optional<inflation_seasonalities_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_inflation_seasonalities_request&,
                           const list_inflation_seasonalities_request&) = default;
};

struct list_inflation_seasonalities_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::inflation_seasonality> inflation_seasonalities;
    std::uint64_t total;

    friend bool operator==(const list_inflation_seasonalities_response&,
                           const list_inflation_seasonalities_response&) = default;
};

struct get_inflation_seasonality_request {
    using response_type = struct get_inflation_seasonality_response;
    static constexpr std::string_view nats_subject = "refdata.v1.inflation_seasonalities.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    inflation_seasonality_key key;

    friend bool operator==(const get_inflation_seasonality_request&,
                           const get_inflation_seasonality_request&) = default;
};

struct get_inflation_seasonality_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::inflation_seasonality> inflation_seasonality;

    friend bool operator==(const get_inflation_seasonality_response&,
                           const get_inflation_seasonality_response&) = default;
};

struct get_many_inflation_seasonalities_request {
    using response_type = struct get_many_inflation_seasonalities_response;
    static constexpr std::string_view nats_subject = "refdata.v1.inflation_seasonalities.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<inflation_seasonality_key> keys;

    friend bool operator==(const get_many_inflation_seasonalities_request&,
                           const get_many_inflation_seasonalities_request&) = default;
};

struct get_many_inflation_seasonalities_response {
    ores::utility::domain::result result;
    std::vector<inflation_seasonality_lookup> entries;

    friend bool operator==(const get_many_inflation_seasonalities_response&,
                           const get_many_inflation_seasonalities_response&) = default;
};

struct put_inflation_seasonality_request {
    using response_type = struct put_inflation_seasonality_response;
    static constexpr std::string_view nats_subject = "refdata.v1.inflation_seasonalities.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    inflation_seasonality_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_inflation_seasonality_request&,
                           const put_inflation_seasonality_request&) = default;
};

struct put_inflation_seasonality_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::inflation_seasonality> inflation_seasonality;

    friend bool operator==(const put_inflation_seasonality_response&,
                           const put_inflation_seasonality_response&) = default;
};

struct put_many_inflation_seasonalities_request {
    using response_type = struct put_many_inflation_seasonalities_response;
    static constexpr std::string_view nats_subject = "refdata.v1.inflation_seasonalities.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<inflation_seasonality_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_inflation_seasonalities_request&,
                           const put_many_inflation_seasonalities_request&) = default;
};

struct put_many_inflation_seasonalities_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::inflation_seasonality> inflation_seasonalities;

    friend bool operator==(const put_many_inflation_seasonalities_response&,
                           const put_many_inflation_seasonalities_response&) = default;
};

struct delete_inflation_seasonality_request {
    using response_type = struct delete_inflation_seasonality_response;
    static constexpr std::string_view nats_subject = "refdata.v1.inflation_seasonalities.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    inflation_seasonality_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_inflation_seasonality_request&,
                           const delete_inflation_seasonality_request&) = default;
};

struct delete_inflation_seasonality_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_inflation_seasonality_response&,
                           const delete_inflation_seasonality_response&) = default;
};

struct delete_many_inflation_seasonalities_request {
    using response_type = struct delete_many_inflation_seasonalities_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.inflation_seasonalities.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<inflation_seasonality_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_inflation_seasonalities_request&,
                           const delete_many_inflation_seasonalities_request&) = default;
};

struct delete_many_inflation_seasonalities_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_inflation_seasonalities_response&,
                           const delete_many_inflation_seasonalities_response&) = default;
};

struct list_inflation_seasonality_versions_request {
    using response_type = struct list_inflation_seasonality_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.inflation_seasonalities_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    inflation_seasonality_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<inflation_seasonality_versions_filter> filter;

    friend bool operator==(const list_inflation_seasonality_versions_request&,
                           const list_inflation_seasonality_versions_request&) = default;
};

struct list_inflation_seasonality_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::inflation_seasonality> versions;
    std::uint64_t total;

    friend bool operator==(const list_inflation_seasonality_versions_response&,
                           const list_inflation_seasonality_versions_response&) = default;
};

struct get_inflation_seasonality_version_request {
    using response_type = struct get_inflation_seasonality_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.inflation_seasonalities_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    inflation_seasonality_version_key key;

    friend bool operator==(const get_inflation_seasonality_version_request&,
                           const get_inflation_seasonality_version_request&) = default;
};

struct get_inflation_seasonality_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::inflation_seasonality> version;

    friend bool operator==(const get_inflation_seasonality_version_response&,
                           const get_inflation_seasonality_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace inflation_seasonality_event_subjects {
inline constexpr std::string_view created = "refdata.v1.events.inflation_seasonalities.created";
inline constexpr std::string_view updated = "refdata.v1.events.inflation_seasonalities.updated";
inline constexpr std::string_view deleted = "refdata.v1.events.inflation_seasonalities.deleted";
}
/**
 * @brief The members of inflation_seasonality_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<inflation_seasonality_key>) {
    return std::tuple{
        utility::codec::field{"id", &inflation_seasonality_key::id},
    };
}

/**
 * @brief The members of inflation_seasonality_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<inflation_seasonality_write>) {
    return std::tuple{
        utility::codec::field{"id", &inflation_seasonality_write::id},
        utility::codec::field{"curve_definition_id",
                              &inflation_seasonality_write::curve_definition_id},
        utility::codec::field{"base_date", &inflation_seasonality_write::base_date},
        utility::codec::field{"frequency", &inflation_seasonality_write::frequency},
    };
}

/**
 * @brief The members of inflation_seasonality_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<inflation_seasonality_change>) {
    return std::tuple{
        utility::codec::field{"write", &inflation_seasonality_change::write},
        utility::codec::field{"precondition", &inflation_seasonality_change::precondition},
    };
}

/**
 * @brief The members of inflation_seasonality_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<inflation_seasonality_removal>) {
    return std::tuple{
        utility::codec::field{"key", &inflation_seasonality_removal::key},
        utility::codec::field{"precondition", &inflation_seasonality_removal::precondition},
    };
}

/**
 * @brief The members of inflation_seasonality_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<inflation_seasonality_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &inflation_seasonality_lookup::key},
        utility::codec::field{"inflation_seasonality",
                              &inflation_seasonality_lookup::inflation_seasonality},
    };
}

/**
 * @brief The members of inflation_seasonalities_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<inflation_seasonalities_filter>) {
    return std::tuple{
        utility::codec::field{"id_one_of", &inflation_seasonalities_filter::id_one_of},
    };
}

/**
 * @brief The members of inflation_seasonality_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<inflation_seasonality_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &inflation_seasonality_event::event_id},
        utility::codec::field{"key", &inflation_seasonality_event::key},
        utility::codec::field{"action", &inflation_seasonality_event::action},
        utility::codec::field{"version", &inflation_seasonality_event::version},
        utility::codec::field{"occurred_at", &inflation_seasonality_event::occurred_at},
        utility::codec::field{"correlation_id", &inflation_seasonality_event::correlation_id},
    };
}

/**
 * @brief The members of inflation_seasonality_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<inflation_seasonality_version_key>) {
    return std::tuple{
        utility::codec::field{"inflation_seasonality",
                              &inflation_seasonality_version_key::inflation_seasonality},
        utility::codec::field{"version", &inflation_seasonality_version_key::version},
    };
}

/**
 * @brief The members of inflation_seasonality_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<inflation_seasonality_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &inflation_seasonality_versions_filter::version},
        utility::codec::field{"from_version", &inflation_seasonality_versions_filter::from_version},
        utility::codec::field{"to_version", &inflation_seasonality_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_inflation_seasonalities_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_inflation_seasonalities_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_inflation_seasonalities_request::offset},
        utility::codec::field{"limit", &list_inflation_seasonalities_request::limit},
        utility::codec::field{"order", &list_inflation_seasonalities_request::order},
        utility::codec::field{"filter", &list_inflation_seasonalities_request::filter},
        utility::codec::field{"as_of", &list_inflation_seasonalities_request::as_of},
    };
}

/**
 * @brief The members of list_inflation_seasonalities_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_inflation_seasonalities_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_inflation_seasonalities_response::result},
        utility::codec::field{"inflation_seasonalities",
                              &list_inflation_seasonalities_response::inflation_seasonalities},
        utility::codec::field{"total", &list_inflation_seasonalities_response::total},
    };
}

/**
 * @brief The members of get_inflation_seasonality_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_inflation_seasonality_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_inflation_seasonality_request::key},
    };
}

/**
 * @brief The members of get_inflation_seasonality_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_inflation_seasonality_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_inflation_seasonality_response::result},
        utility::codec::field{"inflation_seasonality",
                              &get_inflation_seasonality_response::inflation_seasonality},
    };
}

/**
 * @brief The members of get_many_inflation_seasonalities_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_inflation_seasonalities_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_inflation_seasonalities_request::keys},
    };
}

/**
 * @brief The members of get_many_inflation_seasonalities_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_inflation_seasonalities_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_inflation_seasonalities_response::result},
        utility::codec::field{"entries", &get_many_inflation_seasonalities_response::entries},
    };
}

/**
 * @brief The members of put_inflation_seasonality_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_inflation_seasonality_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_inflation_seasonality_request::change},
        utility::codec::field{"intent", &put_inflation_seasonality_request::intent},
    };
}

/**
 * @brief The members of put_inflation_seasonality_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_inflation_seasonality_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_inflation_seasonality_response::result},
        utility::codec::field{"inflation_seasonality",
                              &put_inflation_seasonality_response::inflation_seasonality},
    };
}

/**
 * @brief The members of put_many_inflation_seasonalities_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_inflation_seasonalities_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_inflation_seasonalities_request::changes},
        utility::codec::field{"intent", &put_many_inflation_seasonalities_request::intent},
    };
}

/**
 * @brief The members of put_many_inflation_seasonalities_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_inflation_seasonalities_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_inflation_seasonalities_response::result},
        utility::codec::field{"inflation_seasonalities",
                              &put_many_inflation_seasonalities_response::inflation_seasonalities},
    };
}

/**
 * @brief The members of delete_inflation_seasonality_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_inflation_seasonality_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_inflation_seasonality_request::removal},
        utility::codec::field{"intent", &delete_inflation_seasonality_request::intent},
    };
}

/**
 * @brief The members of delete_inflation_seasonality_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_inflation_seasonality_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_inflation_seasonality_response::result},
    };
}

/**
 * @brief The members of delete_many_inflation_seasonalities_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_inflation_seasonalities_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_inflation_seasonalities_request::removals},
        utility::codec::field{"intent", &delete_many_inflation_seasonalities_request::intent},
    };
}

/**
 * @brief The members of delete_many_inflation_seasonalities_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_inflation_seasonalities_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_inflation_seasonalities_response::result},
    };
}

/**
 * @brief The members of list_inflation_seasonality_versions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_inflation_seasonality_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_inflation_seasonality_versions_request::key},
        utility::codec::field{"offset", &list_inflation_seasonality_versions_request::offset},
        utility::codec::field{"limit", &list_inflation_seasonality_versions_request::limit},
        utility::codec::field{"order", &list_inflation_seasonality_versions_request::order},
        utility::codec::field{"filter", &list_inflation_seasonality_versions_request::filter},
    };
}

/**
 * @brief The members of list_inflation_seasonality_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_inflation_seasonality_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_inflation_seasonality_versions_response::result},
        utility::codec::field{"versions", &list_inflation_seasonality_versions_response::versions},
        utility::codec::field{"total", &list_inflation_seasonality_versions_response::total},
    };
}

/**
 * @brief The members of get_inflation_seasonality_version_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_inflation_seasonality_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_inflation_seasonality_version_request::key},
    };
}

/**
 * @brief The members of get_inflation_seasonality_version_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_inflation_seasonality_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_inflation_seasonality_version_response::result},
        utility::codec::field{"version", &get_inflation_seasonality_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::inflation_seasonality_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::inflation_seasonality_key& v);
    static ores::refdata::messaging::inflation_seasonality_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::inflation_seasonality_key& v);
    static ores::refdata::messaging::inflation_seasonality_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::inflation_seasonality_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::inflation_seasonality_write& v);
    static ores::refdata::messaging::inflation_seasonality_write read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::inflation_seasonality_write& v);
    static ores::refdata::messaging::inflation_seasonality_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::inflation_seasonality_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::inflation_seasonality_change& v);
    static ores::refdata::messaging::inflation_seasonality_change read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::inflation_seasonality_change& v);
    static ores::refdata::messaging::inflation_seasonality_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::inflation_seasonality_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::inflation_seasonality_removal& v);
    static ores::refdata::messaging::inflation_seasonality_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::inflation_seasonality_removal& v);
    static ores::refdata::messaging::inflation_seasonality_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::inflation_seasonality_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::inflation_seasonality_lookup& v);
    static ores::refdata::messaging::inflation_seasonality_lookup read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::inflation_seasonality_lookup& v);
    static ores::refdata::messaging::inflation_seasonality_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::inflation_seasonalities_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::inflation_seasonalities_filter& v);
    static ores::refdata::messaging::inflation_seasonalities_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::inflation_seasonalities_filter& v);
    static ores::refdata::messaging::inflation_seasonalities_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::inflation_seasonality_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::inflation_seasonality_event& v);
    static ores::refdata::messaging::inflation_seasonality_event read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::inflation_seasonality_event& v);
    static ores::refdata::messaging::inflation_seasonality_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::inflation_seasonality_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::inflation_seasonality_version_key& v);
    static ores::refdata::messaging::inflation_seasonality_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::inflation_seasonality_version_key& v);
    static ores::refdata::messaging::inflation_seasonality_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::inflation_seasonality_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::inflation_seasonality_versions_filter& v);
    static ores::refdata::messaging::inflation_seasonality_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::inflation_seasonality_versions_filter& v);
    static ores::refdata::messaging::inflation_seasonality_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_inflation_seasonalities_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_inflation_seasonalities_request& v);
    static ores::refdata::messaging::list_inflation_seasonalities_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_inflation_seasonalities_request& v);
    static ores::refdata::messaging::list_inflation_seasonalities_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_inflation_seasonalities_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_inflation_seasonalities_response& v);
    static ores::refdata::messaging::list_inflation_seasonalities_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_inflation_seasonalities_response& v);
    static ores::refdata::messaging::list_inflation_seasonalities_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_inflation_seasonality_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_inflation_seasonality_request& v);
    static ores::refdata::messaging::get_inflation_seasonality_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_inflation_seasonality_request& v);
    static ores::refdata::messaging::get_inflation_seasonality_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_inflation_seasonality_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_inflation_seasonality_response& v);
    static ores::refdata::messaging::get_inflation_seasonality_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_inflation_seasonality_response& v);
    static ores::refdata::messaging::get_inflation_seasonality_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_inflation_seasonalities_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_inflation_seasonalities_request& v);
    static ores::refdata::messaging::get_many_inflation_seasonalities_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_inflation_seasonalities_request& v);
    static ores::refdata::messaging::get_many_inflation_seasonalities_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_inflation_seasonalities_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_inflation_seasonalities_response& v);
    static ores::refdata::messaging::get_many_inflation_seasonalities_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_inflation_seasonalities_response& v);
    static ores::refdata::messaging::get_many_inflation_seasonalities_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_inflation_seasonality_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_inflation_seasonality_request& v);
    static ores::refdata::messaging::put_inflation_seasonality_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_inflation_seasonality_request& v);
    static ores::refdata::messaging::put_inflation_seasonality_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_inflation_seasonality_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_inflation_seasonality_response& v);
    static ores::refdata::messaging::put_inflation_seasonality_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_inflation_seasonality_response& v);
    static ores::refdata::messaging::put_inflation_seasonality_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_inflation_seasonalities_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_inflation_seasonalities_request& v);
    static ores::refdata::messaging::put_many_inflation_seasonalities_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_inflation_seasonalities_request& v);
    static ores::refdata::messaging::put_many_inflation_seasonalities_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_inflation_seasonalities_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_inflation_seasonalities_response& v);
    static ores::refdata::messaging::put_many_inflation_seasonalities_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_inflation_seasonalities_response& v);
    static ores::refdata::messaging::put_many_inflation_seasonalities_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_inflation_seasonality_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_inflation_seasonality_request& v);
    static ores::refdata::messaging::delete_inflation_seasonality_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_inflation_seasonality_request& v);
    static ores::refdata::messaging::delete_inflation_seasonality_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_inflation_seasonality_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_inflation_seasonality_response& v);
    static ores::refdata::messaging::delete_inflation_seasonality_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_inflation_seasonality_response& v);
    static ores::refdata::messaging::delete_inflation_seasonality_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_inflation_seasonalities_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_inflation_seasonalities_request& v);
    static ores::refdata::messaging::delete_many_inflation_seasonalities_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_inflation_seasonalities_request& v);
    static ores::refdata::messaging::delete_many_inflation_seasonalities_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_inflation_seasonalities_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_inflation_seasonalities_response& v);
    static ores::refdata::messaging::delete_many_inflation_seasonalities_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_inflation_seasonalities_response& v);
    static ores::refdata::messaging::delete_many_inflation_seasonalities_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_inflation_seasonality_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_inflation_seasonality_versions_request& v);
    static ores::refdata::messaging::list_inflation_seasonality_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_inflation_seasonality_versions_request& v);
    static ores::refdata::messaging::list_inflation_seasonality_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_inflation_seasonality_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_inflation_seasonality_versions_response& v);
    static ores::refdata::messaging::list_inflation_seasonality_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_inflation_seasonality_versions_response& v);
    static ores::refdata::messaging::list_inflation_seasonality_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_inflation_seasonality_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_inflation_seasonality_version_request& v);
    static ores::refdata::messaging::get_inflation_seasonality_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_inflation_seasonality_version_request& v);
    static ores::refdata::messaging::get_inflation_seasonality_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_inflation_seasonality_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_inflation_seasonality_version_response& v);
    static ores::refdata::messaging::get_inflation_seasonality_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_inflation_seasonality_version_response& v);
    static ores::refdata::messaging::get_inflation_seasonality_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
