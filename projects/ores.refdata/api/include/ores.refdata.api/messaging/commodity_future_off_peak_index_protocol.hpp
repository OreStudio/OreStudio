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
#ifndef ORES_REFDATA_API_MESSAGING_COMMODITY_FUTURE_OFF_PEAK_INDEX_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_COMMODITY_FUTURE_OFF_PEAK_INDEX_PROTOCOL_HPP

#include "ores.refdata.api/domain/commodity_future_off_peak_index.hpp"
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

struct commodity_future_off_peak_index_key {
    boost::uuids::uuid id;
};

struct commodity_future_off_peak_index_write {
    boost::uuids::uuid id;
    boost::uuids::uuid convention_id;
    std::string off_peak_index;
    std::string peak_index;
    double off_peak_hours;
    std::string peak_calendar;
};

struct commodity_future_off_peak_index_change {
    commodity_future_off_peak_index_write write;
    ores::utility::domain::precondition precondition;
};

struct commodity_future_off_peak_index_removal {
    commodity_future_off_peak_index_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct commodity_future_off_peak_index_lookup {
    commodity_future_off_peak_index_key key;
    std::optional<ores::refdata::domain::commodity_future_off_peak_index>
        commodity_future_off_peak_index;
};

struct commodity_future_off_peak_indexes_filter {
    std::optional<boost::uuids::uuid> convention_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> convention_id_one_of;
};

struct commodity_future_off_peak_index_event {
    boost::uuids::uuid event_id;
    commodity_future_off_peak_index_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct commodity_future_off_peak_index_version_key {
    commodity_future_off_peak_index_key commodity_future_off_peak_index;
    std::uint32_t version;
};

struct commodity_future_off_peak_index_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_commodity_future_off_peak_indexes_request {
    using response_type = struct list_commodity_future_off_peak_indexes_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_off_peak_indexes.list";
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
    std::optional<commodity_future_off_peak_indexes_filter> filter;
    std::optional<std::string> as_of;
};

struct list_commodity_future_off_peak_indexes_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::commodity_future_off_peak_index>
        commodity_future_off_peak_indexes;
    std::uint64_t total;
};

struct get_commodity_future_off_peak_index_request {
    using response_type = struct get_commodity_future_off_peak_index_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_off_peak_indexes.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_future_off_peak_index_key key;
};

struct get_commodity_future_off_peak_index_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::commodity_future_off_peak_index>
        commodity_future_off_peak_index;
};

struct get_many_commodity_future_off_peak_indexes_request {
    using response_type = struct get_many_commodity_future_off_peak_indexes_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_off_peak_indexes.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<commodity_future_off_peak_index_key> keys;
};

struct get_many_commodity_future_off_peak_indexes_response {
    ores::utility::domain::result result;
    std::vector<commodity_future_off_peak_index_lookup> entries;
};

struct put_commodity_future_off_peak_index_request {
    using response_type = struct put_commodity_future_off_peak_index_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_off_peak_indexes.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_future_off_peak_index_change change;
    ores::utility::domain::change_intent intent;
};

struct put_commodity_future_off_peak_index_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::commodity_future_off_peak_index>
        commodity_future_off_peak_index;
};

struct put_many_commodity_future_off_peak_indexes_request {
    using response_type = struct put_many_commodity_future_off_peak_indexes_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_off_peak_indexes.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<commodity_future_off_peak_index_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_commodity_future_off_peak_indexes_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::commodity_future_off_peak_index>
        commodity_future_off_peak_indexes;
};

struct delete_commodity_future_off_peak_index_request {
    using response_type = struct delete_commodity_future_off_peak_index_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_off_peak_indexes.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_future_off_peak_index_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_commodity_future_off_peak_index_response {
    ores::utility::domain::result result;
};

struct delete_many_commodity_future_off_peak_indexes_request {
    using response_type = struct delete_many_commodity_future_off_peak_indexes_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_off_peak_indexes.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<commodity_future_off_peak_index_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_commodity_future_off_peak_indexes_response {
    ores::utility::domain::result result;
};

struct list_by_convention_id_commodity_future_off_peak_indexes_request {
    using response_type = struct list_by_convention_id_commodity_future_off_peak_indexes_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_off_peak_indexes.list_by_convention_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid convention_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<commodity_future_off_peak_indexes_filter> filter;
};

struct list_by_convention_id_commodity_future_off_peak_indexes_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::commodity_future_off_peak_index>
        commodity_future_off_peak_indexes;
    std::uint64_t total;
};

struct list_commodity_future_off_peak_index_versions_request {
    using response_type = struct list_commodity_future_off_peak_index_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_off_peak_indexes_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_future_off_peak_index_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<commodity_future_off_peak_index_versions_filter> filter;
};

struct list_commodity_future_off_peak_index_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::commodity_future_off_peak_index> versions;
    std::uint64_t total;
};

struct get_commodity_future_off_peak_index_version_request {
    using response_type = struct get_commodity_future_off_peak_index_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_off_peak_indexes_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_future_off_peak_index_version_key key;
};

struct get_commodity_future_off_peak_index_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::commodity_future_off_peak_index> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace commodity_future_off_peak_index_event_subjects {
inline constexpr std::string_view created =
    "refdata.v1.commodity_future_off_peak_indexes_events.created";
inline constexpr std::string_view updated =
    "refdata.v1.commodity_future_off_peak_indexes_events.updated";
inline constexpr std::string_view deleted =
    "refdata.v1.commodity_future_off_peak_indexes_events.deleted";
}
/**
 * @brief The members of commodity_future_off_peak_index_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const commodity_future_off_peak_index_key&) {
    return std::tuple{
        utility::codec::field{"id", &commodity_future_off_peak_index_key::id},
    };
}

/**
 * @brief The members of commodity_future_off_peak_index_write, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const commodity_future_off_peak_index_write&) {
    return std::tuple{
        utility::codec::field{"id", &commodity_future_off_peak_index_write::id},
        utility::codec::field{"convention_id",
                              &commodity_future_off_peak_index_write::convention_id},
        utility::codec::field{"off_peak_index",
                              &commodity_future_off_peak_index_write::off_peak_index},
        utility::codec::field{"peak_index", &commodity_future_off_peak_index_write::peak_index},
        utility::codec::field{"off_peak_hours",
                              &commodity_future_off_peak_index_write::off_peak_hours},
        utility::codec::field{"peak_calendar",
                              &commodity_future_off_peak_index_write::peak_calendar},
    };
}

/**
 * @brief The members of commodity_future_off_peak_index_change, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const commodity_future_off_peak_index_change&) {
    return std::tuple{
        utility::codec::field{"write", &commodity_future_off_peak_index_change::write},
        utility::codec::field{"precondition",
                              &commodity_future_off_peak_index_change::precondition},
    };
}

/**
 * @brief The members of commodity_future_off_peak_index_removal, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const commodity_future_off_peak_index_removal&) {
    return std::tuple{
        utility::codec::field{"key", &commodity_future_off_peak_index_removal::key},
        utility::codec::field{"precondition",
                              &commodity_future_off_peak_index_removal::precondition},
    };
}

/**
 * @brief The members of commodity_future_off_peak_index_lookup, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const commodity_future_off_peak_index_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &commodity_future_off_peak_index_lookup::key},
        utility::codec::field{
            "commodity_future_off_peak_index",
            &commodity_future_off_peak_index_lookup::commodity_future_off_peak_index},
    };
}

/**
 * @brief The members of commodity_future_off_peak_indexes_filter, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const commodity_future_off_peak_indexes_filter&) {
    return std::tuple{
        utility::codec::field{"convention_id",
                              &commodity_future_off_peak_indexes_filter::convention_id},
        utility::codec::field{"id_one_of", &commodity_future_off_peak_indexes_filter::id_one_of},
        utility::codec::field{"convention_id_one_of",
                              &commodity_future_off_peak_indexes_filter::convention_id_one_of},
    };
}

/**
 * @brief The members of commodity_future_off_peak_index_event, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const commodity_future_off_peak_index_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &commodity_future_off_peak_index_event::event_id},
        utility::codec::field{"key", &commodity_future_off_peak_index_event::key},
        utility::codec::field{"action", &commodity_future_off_peak_index_event::action},
        utility::codec::field{"version", &commodity_future_off_peak_index_event::version},
        utility::codec::field{"occurred_at", &commodity_future_off_peak_index_event::occurred_at},
        utility::codec::field{"correlation_id",
                              &commodity_future_off_peak_index_event::correlation_id},
    };
}

/**
 * @brief The members of commodity_future_off_peak_index_version_key, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const commodity_future_off_peak_index_version_key&) {
    return std::tuple{
        utility::codec::field{
            "commodity_future_off_peak_index",
            &commodity_future_off_peak_index_version_key::commodity_future_off_peak_index},
        utility::codec::field{"version", &commodity_future_off_peak_index_version_key::version},
    };
}

/**
 * @brief The members of commodity_future_off_peak_index_versions_filter, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const commodity_future_off_peak_index_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &commodity_future_off_peak_index_versions_filter::version},
        utility::codec::field{"from_version",
                              &commodity_future_off_peak_index_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &commodity_future_off_peak_index_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_commodity_future_off_peak_indexes_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_commodity_future_off_peak_indexes_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_commodity_future_off_peak_indexes_request::offset},
        utility::codec::field{"limit", &list_commodity_future_off_peak_indexes_request::limit},
        utility::codec::field{"order", &list_commodity_future_off_peak_indexes_request::order},
        utility::codec::field{"filter", &list_commodity_future_off_peak_indexes_request::filter},
        utility::codec::field{"as_of", &list_commodity_future_off_peak_indexes_request::as_of},
    };
}

/**
 * @brief The members of list_commodity_future_off_peak_indexes_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_commodity_future_off_peak_indexes_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_commodity_future_off_peak_indexes_response::result},
        utility::codec::field{
            "commodity_future_off_peak_indexes",
            &list_commodity_future_off_peak_indexes_response::commodity_future_off_peak_indexes},
        utility::codec::field{"total", &list_commodity_future_off_peak_indexes_response::total},
    };
}

/**
 * @brief The members of get_commodity_future_off_peak_index_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_commodity_future_off_peak_index_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_commodity_future_off_peak_index_request::key},
    };
}

/**
 * @brief The members of get_commodity_future_off_peak_index_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_commodity_future_off_peak_index_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_commodity_future_off_peak_index_response::result},
        utility::codec::field{
            "commodity_future_off_peak_index",
            &get_commodity_future_off_peak_index_response::commodity_future_off_peak_index},
    };
}

/**
 * @brief The members of get_many_commodity_future_off_peak_indexes_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const get_many_commodity_future_off_peak_indexes_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_commodity_future_off_peak_indexes_request::keys},
    };
}

/**
 * @brief The members of get_many_commodity_future_off_peak_indexes_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const get_many_commodity_future_off_peak_indexes_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &get_many_commodity_future_off_peak_indexes_response::result},
        utility::codec::field{"entries",
                              &get_many_commodity_future_off_peak_indexes_response::entries},
    };
}

/**
 * @brief The members of put_commodity_future_off_peak_index_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_commodity_future_off_peak_index_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_commodity_future_off_peak_index_request::change},
        utility::codec::field{"intent", &put_commodity_future_off_peak_index_request::intent},
    };
}

/**
 * @brief The members of put_commodity_future_off_peak_index_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_commodity_future_off_peak_index_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_commodity_future_off_peak_index_response::result},
        utility::codec::field{
            "commodity_future_off_peak_index",
            &put_commodity_future_off_peak_index_response::commodity_future_off_peak_index},
    };
}

/**
 * @brief The members of put_many_commodity_future_off_peak_indexes_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const put_many_commodity_future_off_peak_indexes_request&) {
    return std::tuple{
        utility::codec::field{"changes",
                              &put_many_commodity_future_off_peak_indexes_request::changes},
        utility::codec::field{"intent",
                              &put_many_commodity_future_off_peak_indexes_request::intent},
    };
}

/**
 * @brief The members of put_many_commodity_future_off_peak_indexes_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const put_many_commodity_future_off_peak_indexes_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &put_many_commodity_future_off_peak_indexes_response::result},
        utility::codec::field{"commodity_future_off_peak_indexes",
                              &put_many_commodity_future_off_peak_indexes_response::
                                  commodity_future_off_peak_indexes},
    };
}

/**
 * @brief The members of delete_commodity_future_off_peak_index_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_commodity_future_off_peak_index_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_commodity_future_off_peak_index_request::removal},
        utility::codec::field{"intent", &delete_commodity_future_off_peak_index_request::intent},
    };
}

/**
 * @brief The members of delete_commodity_future_off_peak_index_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_commodity_future_off_peak_index_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_commodity_future_off_peak_index_response::result},
    };
}

/**
 * @brief The members of delete_many_commodity_future_off_peak_indexes_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const delete_many_commodity_future_off_peak_indexes_request&) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_commodity_future_off_peak_indexes_request::removals},
        utility::codec::field{"intent",
                              &delete_many_commodity_future_off_peak_indexes_request::intent},
    };
}

/**
 * @brief The members of delete_many_commodity_future_off_peak_indexes_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const delete_many_commodity_future_off_peak_indexes_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &delete_many_commodity_future_off_peak_indexes_response::result},
    };
}

/**
 * @brief The members of list_by_convention_id_commodity_future_off_peak_indexes_request, named as
 * the wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_convention_id_commodity_future_off_peak_indexes_request&) {
    return std::tuple{
        utility::codec::field{
            "convention_id",
            &list_by_convention_id_commodity_future_off_peak_indexes_request::convention_id},
        utility::codec::field{
            "scope", &list_by_convention_id_commodity_future_off_peak_indexes_request::scope},
        utility::codec::field{
            "offset", &list_by_convention_id_commodity_future_off_peak_indexes_request::offset},
        utility::codec::field{
            "limit", &list_by_convention_id_commodity_future_off_peak_indexes_request::limit},
        utility::codec::field{
            "order", &list_by_convention_id_commodity_future_off_peak_indexes_request::order},
        utility::codec::field{
            "filter", &list_by_convention_id_commodity_future_off_peak_indexes_request::filter},
    };
}

/**
 * @brief The members of list_by_convention_id_commodity_future_off_peak_indexes_response, named as
 * the wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_convention_id_commodity_future_off_peak_indexes_response&) {
    return std::tuple{
        utility::codec::field{
            "result", &list_by_convention_id_commodity_future_off_peak_indexes_response::result},
        utility::codec::field{"commodity_future_off_peak_indexes",
                              &list_by_convention_id_commodity_future_off_peak_indexes_response::
                                  commodity_future_off_peak_indexes},
        utility::codec::field{
            "total", &list_by_convention_id_commodity_future_off_peak_indexes_response::total},
    };
}

/**
 * @brief The members of list_commodity_future_off_peak_index_versions_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_commodity_future_off_peak_index_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_commodity_future_off_peak_index_versions_request::key},
        utility::codec::field{"offset",
                              &list_commodity_future_off_peak_index_versions_request::offset},
        utility::codec::field{"limit",
                              &list_commodity_future_off_peak_index_versions_request::limit},
        utility::codec::field{"order",
                              &list_commodity_future_off_peak_index_versions_request::order},
        utility::codec::field{"filter",
                              &list_commodity_future_off_peak_index_versions_request::filter},
    };
}

/**
 * @brief The members of list_commodity_future_off_peak_index_versions_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_commodity_future_off_peak_index_versions_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_commodity_future_off_peak_index_versions_response::result},
        utility::codec::field{"versions",
                              &list_commodity_future_off_peak_index_versions_response::versions},
        utility::codec::field{"total",
                              &list_commodity_future_off_peak_index_versions_response::total},
    };
}

/**
 * @brief The members of get_commodity_future_off_peak_index_version_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const get_commodity_future_off_peak_index_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_commodity_future_off_peak_index_version_request::key},
    };
}

/**
 * @brief The members of get_commodity_future_off_peak_index_version_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const get_commodity_future_off_peak_index_version_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &get_commodity_future_off_peak_index_version_response::result},
        utility::codec::field{"version",
                              &get_commodity_future_off_peak_index_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_off_peak_index_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_off_peak_index_key& v);
    static ores::refdata::messaging::commodity_future_off_peak_index_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_off_peak_index_key& v);
    static ores::refdata::messaging::commodity_future_off_peak_index_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_off_peak_index_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_off_peak_index_write& v);
    static ores::refdata::messaging::commodity_future_off_peak_index_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_off_peak_index_write& v);
    static ores::refdata::messaging::commodity_future_off_peak_index_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_off_peak_index_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_off_peak_index_change& v);
    static ores::refdata::messaging::commodity_future_off_peak_index_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_off_peak_index_change& v);
    static ores::refdata::messaging::commodity_future_off_peak_index_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_off_peak_index_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_off_peak_index_removal& v);
    static ores::refdata::messaging::commodity_future_off_peak_index_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_off_peak_index_removal& v);
    static ores::refdata::messaging::commodity_future_off_peak_index_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_off_peak_index_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_off_peak_index_lookup& v);
    static ores::refdata::messaging::commodity_future_off_peak_index_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_off_peak_index_lookup& v);
    static ores::refdata::messaging::commodity_future_off_peak_index_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_off_peak_indexes_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_off_peak_indexes_filter& v);
    static ores::refdata::messaging::commodity_future_off_peak_indexes_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_off_peak_indexes_filter& v);
    static ores::refdata::messaging::commodity_future_off_peak_indexes_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_off_peak_index_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_off_peak_index_event& v);
    static ores::refdata::messaging::commodity_future_off_peak_index_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_off_peak_index_event& v);
    static ores::refdata::messaging::commodity_future_off_peak_index_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_off_peak_index_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_off_peak_index_version_key& v);
    static ores::refdata::messaging::commodity_future_off_peak_index_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_off_peak_index_version_key& v);
    static ores::refdata::messaging::commodity_future_off_peak_index_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_off_peak_index_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_off_peak_index_versions_filter& v);
    static ores::refdata::messaging::commodity_future_off_peak_index_versions_filter
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::commodity_future_off_peak_index_versions_filter& v);
    static ores::refdata::messaging::commodity_future_off_peak_index_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_commodity_future_off_peak_indexes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_commodity_future_off_peak_indexes_request& v);
    static ores::refdata::messaging::list_commodity_future_off_peak_indexes_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_commodity_future_off_peak_indexes_request& v);
    static ores::refdata::messaging::list_commodity_future_off_peak_indexes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_commodity_future_off_peak_indexes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_commodity_future_off_peak_indexes_response& v);
    static ores::refdata::messaging::list_commodity_future_off_peak_indexes_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_commodity_future_off_peak_indexes_response& v);
    static ores::refdata::messaging::list_commodity_future_off_peak_indexes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_commodity_future_off_peak_index_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_commodity_future_off_peak_index_request& v);
    static ores::refdata::messaging::get_commodity_future_off_peak_index_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_commodity_future_off_peak_index_request& v);
    static ores::refdata::messaging::get_commodity_future_off_peak_index_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_commodity_future_off_peak_index_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_commodity_future_off_peak_index_response& v);
    static ores::refdata::messaging::get_commodity_future_off_peak_index_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_commodity_future_off_peak_index_response& v);
    static ores::refdata::messaging::get_commodity_future_off_peak_index_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_commodity_future_off_peak_indexes_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_many_commodity_future_off_peak_indexes_request& v);
    static ores::refdata::messaging::get_many_commodity_future_off_peak_indexes_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_commodity_future_off_peak_indexes_request& v);
    static ores::refdata::messaging::get_many_commodity_future_off_peak_indexes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_commodity_future_off_peak_indexes_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_many_commodity_future_off_peak_indexes_response& v);
    static ores::refdata::messaging::get_many_commodity_future_off_peak_indexes_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_commodity_future_off_peak_indexes_response& v);
    static ores::refdata::messaging::get_many_commodity_future_off_peak_indexes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_commodity_future_off_peak_index_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_commodity_future_off_peak_index_request& v);
    static ores::refdata::messaging::put_commodity_future_off_peak_index_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_commodity_future_off_peak_index_request& v);
    static ores::refdata::messaging::put_commodity_future_off_peak_index_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_commodity_future_off_peak_index_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_commodity_future_off_peak_index_response& v);
    static ores::refdata::messaging::put_commodity_future_off_peak_index_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_commodity_future_off_peak_index_response& v);
    static ores::refdata::messaging::put_commodity_future_off_peak_index_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_commodity_future_off_peak_indexes_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::put_many_commodity_future_off_peak_indexes_request& v);
    static ores::refdata::messaging::put_many_commodity_future_off_peak_indexes_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_commodity_future_off_peak_indexes_request& v);
    static ores::refdata::messaging::put_many_commodity_future_off_peak_indexes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_commodity_future_off_peak_indexes_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::put_many_commodity_future_off_peak_indexes_response& v);
    static ores::refdata::messaging::put_many_commodity_future_off_peak_indexes_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_commodity_future_off_peak_indexes_response& v);
    static ores::refdata::messaging::put_many_commodity_future_off_peak_indexes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_commodity_future_off_peak_index_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_commodity_future_off_peak_index_request& v);
    static ores::refdata::messaging::delete_commodity_future_off_peak_index_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_commodity_future_off_peak_index_request& v);
    static ores::refdata::messaging::delete_commodity_future_off_peak_index_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_commodity_future_off_peak_index_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_commodity_future_off_peak_index_response& v);
    static ores::refdata::messaging::delete_commodity_future_off_peak_index_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_commodity_future_off_peak_index_response& v);
    static ores::refdata::messaging::delete_commodity_future_off_peak_index_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_commodity_future_off_peak_indexes_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_commodity_future_off_peak_indexes_request& v);
    static ores::refdata::messaging::delete_many_commodity_future_off_peak_indexes_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_commodity_future_off_peak_indexes_request& v);
    static ores::refdata::messaging::delete_many_commodity_future_off_peak_indexes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_commodity_future_off_peak_indexes_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_commodity_future_off_peak_indexes_response& v);
    static ores::refdata::messaging::delete_many_commodity_future_off_peak_indexes_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_commodity_future_off_peak_indexes_response& v);
    static ores::refdata::messaging::delete_many_commodity_future_off_peak_indexes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<
    ores::refdata::messaging::list_by_convention_id_commodity_future_off_peak_indexes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::
                   list_by_convention_id_commodity_future_off_peak_indexes_request& v);
    static ores::refdata::messaging::list_by_convention_id_commodity_future_off_peak_indexes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::
                      list_by_convention_id_commodity_future_off_peak_indexes_request& v);
    static ores::refdata::messaging::list_by_convention_id_commodity_future_off_peak_indexes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<
    ores::refdata::messaging::list_by_convention_id_commodity_future_off_peak_indexes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::
                   list_by_convention_id_commodity_future_off_peak_indexes_response& v);
    static ores::refdata::messaging::
        list_by_convention_id_commodity_future_off_peak_indexes_response
        read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::
                      list_by_convention_id_commodity_future_off_peak_indexes_response& v);
    static ores::refdata::messaging::
        list_by_convention_id_commodity_future_off_peak_indexes_response
        read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_commodity_future_off_peak_index_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_commodity_future_off_peak_index_versions_request& v);
    static ores::refdata::messaging::list_commodity_future_off_peak_index_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_commodity_future_off_peak_index_versions_request& v);
    static ores::refdata::messaging::list_commodity_future_off_peak_index_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_commodity_future_off_peak_index_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_commodity_future_off_peak_index_versions_response& v);
    static ores::refdata::messaging::list_commodity_future_off_peak_index_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_commodity_future_off_peak_index_versions_response& v);
    static ores::refdata::messaging::list_commodity_future_off_peak_index_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_commodity_future_off_peak_index_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_commodity_future_off_peak_index_version_request& v);
    static ores::refdata::messaging::get_commodity_future_off_peak_index_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_commodity_future_off_peak_index_version_request& v);
    static ores::refdata::messaging::get_commodity_future_off_peak_index_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_commodity_future_off_peak_index_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_commodity_future_off_peak_index_version_response& v);
    static ores::refdata::messaging::get_commodity_future_off_peak_index_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_commodity_future_off_peak_index_version_response& v);
    static ores::refdata::messaging::get_commodity_future_off_peak_index_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
