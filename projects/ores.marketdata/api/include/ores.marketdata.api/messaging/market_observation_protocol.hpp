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
#ifndef ORES_MARKETDATA_API_MESSAGING_MARKET_OBSERVATION_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_MARKET_OBSERVATION_PROTOCOL_HPP

#include "ores.marketdata.api/domain/market_observation.hpp"
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

struct market_observation_key {
    boost::uuids::uuid id;

    friend bool operator==(const market_observation_key&, const market_observation_key&) = default;
};

struct market_observation_write {
    boost::uuids::uuid id;
    boost::uuids::uuid party_id;
    boost::uuids::uuid series_id;
    std::chrono::system_clock::time_point observation_datetime;
    std::string oresmd_uri;
    std::string value;
    std::string source;
    boost::uuids::uuid feed_id;

    friend bool operator==(const market_observation_write&,
                           const market_observation_write&) = default;
};

struct market_observation_change {
    market_observation_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const market_observation_change&,
                           const market_observation_change&) = default;
};

struct market_observation_removal {
    market_observation_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const market_observation_removal&,
                           const market_observation_removal&) = default;
};

struct market_observation_lookup {
    market_observation_key key;
    std::optional<ores::marketdata::domain::market_observation> market_observation;

    friend bool operator==(const market_observation_lookup&,
                           const market_observation_lookup&) = default;
};

struct market_observations_filter {
    std::optional<boost::uuids::uuid> series_id;
    std::optional<std::chrono::system_clock::time_point> observation_datetime;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> series_id_one_of;
    std::optional<std::vector<std::chrono::system_clock::time_point>> observation_datetime_one_of;
    std::optional<std::string> observation_datetime_from;
    std::optional<std::string> observation_datetime_to;

    friend bool operator==(const market_observations_filter&,
                           const market_observations_filter&) = default;
};

struct market_observation_event {
    boost::uuids::uuid event_id;
    market_observation_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const market_observation_event&,
                           const market_observation_event&) = default;
};

struct list_market_observations_request {
    using response_type = struct list_market_observations_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_observations.list";
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
    std::optional<market_observations_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_market_observations_request&,
                           const list_market_observations_request&) = default;
};

struct list_market_observations_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::market_observation> market_observations;
    std::uint64_t total;

    friend bool operator==(const list_market_observations_response&,
                           const list_market_observations_response&) = default;
};

struct get_market_observation_request {
    using response_type = struct get_market_observation_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_observations.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_observation_key key;

    friend bool operator==(const get_market_observation_request&,
                           const get_market_observation_request&) = default;
};

struct get_market_observation_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::market_observation> market_observation;

    friend bool operator==(const get_market_observation_response&,
                           const get_market_observation_response&) = default;
};

struct get_many_market_observations_request {
    using response_type = struct get_many_market_observations_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_observations.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_observation_key> keys;

    friend bool operator==(const get_many_market_observations_request&,
                           const get_many_market_observations_request&) = default;
};

struct get_many_market_observations_response {
    ores::utility::domain::result result;
    std::vector<market_observation_lookup> entries;

    friend bool operator==(const get_many_market_observations_response&,
                           const get_many_market_observations_response&) = default;
};

struct put_market_observation_request {
    using response_type = struct put_market_observation_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_observations.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_observation_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_market_observation_request&,
                           const put_market_observation_request&) = default;
};

struct put_market_observation_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::market_observation> market_observation;

    friend bool operator==(const put_market_observation_response&,
                           const put_market_observation_response&) = default;
};

struct put_many_market_observations_request {
    using response_type = struct put_many_market_observations_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_observations.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_observation_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_market_observations_request&,
                           const put_many_market_observations_request&) = default;
};

struct put_many_market_observations_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::market_observation> market_observations;

    friend bool operator==(const put_many_market_observations_response&,
                           const put_many_market_observations_response&) = default;
};

struct delete_market_observation_request {
    using response_type = struct delete_market_observation_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_observations.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_observation_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_market_observation_request&,
                           const delete_market_observation_request&) = default;
};

struct delete_market_observation_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_market_observation_response&,
                           const delete_market_observation_response&) = default;
};

struct delete_many_market_observations_request {
    using response_type = struct delete_many_market_observations_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_observations.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_observation_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_market_observations_request&,
                           const delete_many_market_observations_request&) = default;
};

struct delete_many_market_observations_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_market_observations_response&,
                           const delete_many_market_observations_response&) = default;
};

struct list_by_series_id_market_observations_request {
    using response_type = struct list_by_series_id_market_observations_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_observations.list_by_series_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid series_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<market_observations_filter> filter;

    friend bool operator==(const list_by_series_id_market_observations_request&,
                           const list_by_series_id_market_observations_request&) = default;
};

struct list_by_series_id_market_observations_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::market_observation> market_observations;
    std::uint64_t total;

    friend bool operator==(const list_by_series_id_market_observations_response&,
                           const list_by_series_id_market_observations_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace market_observation_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.market_observations_events.created";
inline constexpr std::string_view updated = "marketdata.v1.market_observations_events.updated";
inline constexpr std::string_view deleted = "marketdata.v1.market_observations_events.deleted";
}
/**
 * @brief The members of market_observation_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const market_observation_key&) {
    return std::tuple{
        utility::codec::field{"id", &market_observation_key::id},
    };
}

/**
 * @brief The members of market_observation_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const market_observation_write&) {
    return std::tuple{
        utility::codec::field{"id", &market_observation_write::id},
        utility::codec::field{"party_id", &market_observation_write::party_id},
        utility::codec::field{"series_id", &market_observation_write::series_id},
        utility::codec::field{"observation_datetime",
                              &market_observation_write::observation_datetime},
        utility::codec::field{"oresmd_uri", &market_observation_write::oresmd_uri},
        utility::codec::field{"value", &market_observation_write::value},
        utility::codec::field{"source", &market_observation_write::source},
        utility::codec::field{"feed_id", &market_observation_write::feed_id},
    };
}

/**
 * @brief The members of market_observation_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const market_observation_change&) {
    return std::tuple{
        utility::codec::field{"write", &market_observation_change::write},
        utility::codec::field{"precondition", &market_observation_change::precondition},
    };
}

/**
 * @brief The members of market_observation_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const market_observation_removal&) {
    return std::tuple{
        utility::codec::field{"key", &market_observation_removal::key},
        utility::codec::field{"precondition", &market_observation_removal::precondition},
    };
}

/**
 * @brief The members of market_observation_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const market_observation_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &market_observation_lookup::key},
        utility::codec::field{"market_observation", &market_observation_lookup::market_observation},
    };
}

/**
 * @brief The members of market_observations_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const market_observations_filter&) {
    return std::tuple{
        utility::codec::field{"series_id", &market_observations_filter::series_id},
        utility::codec::field{"observation_datetime",
                              &market_observations_filter::observation_datetime},
        utility::codec::field{"id_one_of", &market_observations_filter::id_one_of},
        utility::codec::field{"series_id_one_of", &market_observations_filter::series_id_one_of},
        utility::codec::field{"observation_datetime_one_of",
                              &market_observations_filter::observation_datetime_one_of},
        utility::codec::field{"observation_datetime_from",
                              &market_observations_filter::observation_datetime_from},
        utility::codec::field{"observation_datetime_to",
                              &market_observations_filter::observation_datetime_to},
    };
}

/**
 * @brief The members of market_observation_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const market_observation_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &market_observation_event::event_id},
        utility::codec::field{"key", &market_observation_event::key},
        utility::codec::field{"action", &market_observation_event::action},
        utility::codec::field{"version", &market_observation_event::version},
        utility::codec::field{"occurred_at", &market_observation_event::occurred_at},
        utility::codec::field{"correlation_id", &market_observation_event::correlation_id},
    };
}

/**
 * @brief The members of list_market_observations_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_market_observations_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_market_observations_request::offset},
        utility::codec::field{"limit", &list_market_observations_request::limit},
        utility::codec::field{"order", &list_market_observations_request::order},
        utility::codec::field{"filter", &list_market_observations_request::filter},
        utility::codec::field{"as_of", &list_market_observations_request::as_of},
    };
}

/**
 * @brief The members of list_market_observations_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_market_observations_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_market_observations_response::result},
        utility::codec::field{"market_observations",
                              &list_market_observations_response::market_observations},
        utility::codec::field{"total", &list_market_observations_response::total},
    };
}

/**
 * @brief The members of get_market_observation_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_market_observation_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_market_observation_request::key},
    };
}

/**
 * @brief The members of get_market_observation_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_market_observation_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_market_observation_response::result},
        utility::codec::field{"market_observation",
                              &get_market_observation_response::market_observation},
    };
}

/**
 * @brief The members of get_many_market_observations_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_market_observations_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_market_observations_request::keys},
    };
}

/**
 * @brief The members of get_many_market_observations_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_market_observations_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_market_observations_response::result},
        utility::codec::field{"entries", &get_many_market_observations_response::entries},
    };
}

/**
 * @brief The members of put_market_observation_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_market_observation_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_market_observation_request::change},
        utility::codec::field{"intent", &put_market_observation_request::intent},
    };
}

/**
 * @brief The members of put_market_observation_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_market_observation_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_market_observation_response::result},
        utility::codec::field{"market_observation",
                              &put_market_observation_response::market_observation},
    };
}

/**
 * @brief The members of put_many_market_observations_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_market_observations_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_market_observations_request::changes},
        utility::codec::field{"intent", &put_many_market_observations_request::intent},
    };
}

/**
 * @brief The members of put_many_market_observations_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_market_observations_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_market_observations_response::result},
        utility::codec::field{"market_observations",
                              &put_many_market_observations_response::market_observations},
    };
}

/**
 * @brief The members of delete_market_observation_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_market_observation_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_market_observation_request::removal},
        utility::codec::field{"intent", &delete_market_observation_request::intent},
    };
}

/**
 * @brief The members of delete_market_observation_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_market_observation_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_market_observation_response::result},
    };
}

/**
 * @brief The members of delete_many_market_observations_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_market_observations_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_market_observations_request::removals},
        utility::codec::field{"intent", &delete_many_market_observations_request::intent},
    };
}

/**
 * @brief The members of delete_many_market_observations_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_market_observations_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_market_observations_response::result},
    };
}

/**
 * @brief The members of list_by_series_id_market_observations_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_series_id_market_observations_request&) {
    return std::tuple{
        utility::codec::field{"series_id",
                              &list_by_series_id_market_observations_request::series_id},
        utility::codec::field{"scope", &list_by_series_id_market_observations_request::scope},
        utility::codec::field{"offset", &list_by_series_id_market_observations_request::offset},
        utility::codec::field{"limit", &list_by_series_id_market_observations_request::limit},
        utility::codec::field{"order", &list_by_series_id_market_observations_request::order},
        utility::codec::field{"filter", &list_by_series_id_market_observations_request::filter},
    };
}

/**
 * @brief The members of list_by_series_id_market_observations_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_series_id_market_observations_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_by_series_id_market_observations_response::result},
        utility::codec::field{"market_observations",
                              &list_by_series_id_market_observations_response::market_observations},
        utility::codec::field{"total", &list_by_series_id_market_observations_response::total},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::market_observation_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_observation_key& v);
    static ores::marketdata::messaging::market_observation_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::market_observation_key& v);
    static ores::marketdata::messaging::market_observation_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_observation_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_observation_write& v);
    static ores::marketdata::messaging::market_observation_write read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_observation_write& v);
    static ores::marketdata::messaging::market_observation_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_observation_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_observation_change& v);
    static ores::marketdata::messaging::market_observation_change read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_observation_change& v);
    static ores::marketdata::messaging::market_observation_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_observation_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_observation_removal& v);
    static ores::marketdata::messaging::market_observation_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_observation_removal& v);
    static ores::marketdata::messaging::market_observation_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_observation_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_observation_lookup& v);
    static ores::marketdata::messaging::market_observation_lookup read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_observation_lookup& v);
    static ores::marketdata::messaging::market_observation_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_observations_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_observations_filter& v);
    static ores::marketdata::messaging::market_observations_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_observations_filter& v);
    static ores::marketdata::messaging::market_observations_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_observation_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_observation_event& v);
    static ores::marketdata::messaging::market_observation_event read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_observation_event& v);
    static ores::marketdata::messaging::market_observation_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_market_observations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_market_observations_request& v);
    static ores::marketdata::messaging::list_market_observations_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_market_observations_request& v);
    static ores::marketdata::messaging::list_market_observations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_market_observations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_market_observations_response& v);
    static ores::marketdata::messaging::list_market_observations_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_market_observations_response& v);
    static ores::marketdata::messaging::list_market_observations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_market_observation_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_market_observation_request& v);
    static ores::marketdata::messaging::get_market_observation_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_market_observation_request& v);
    static ores::marketdata::messaging::get_market_observation_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_market_observation_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_market_observation_response& v);
    static ores::marketdata::messaging::get_market_observation_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_market_observation_response& v);
    static ores::marketdata::messaging::get_market_observation_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_market_observations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_market_observations_request& v);
    static ores::marketdata::messaging::get_many_market_observations_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_market_observations_request& v);
    static ores::marketdata::messaging::get_many_market_observations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_market_observations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_market_observations_response& v);
    static ores::marketdata::messaging::get_many_market_observations_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_market_observations_response& v);
    static ores::marketdata::messaging::get_many_market_observations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_market_observation_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_market_observation_request& v);
    static ores::marketdata::messaging::put_market_observation_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_market_observation_request& v);
    static ores::marketdata::messaging::put_market_observation_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_market_observation_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_market_observation_response& v);
    static ores::marketdata::messaging::put_market_observation_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_market_observation_response& v);
    static ores::marketdata::messaging::put_market_observation_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_market_observations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_market_observations_request& v);
    static ores::marketdata::messaging::put_many_market_observations_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_market_observations_request& v);
    static ores::marketdata::messaging::put_many_market_observations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_market_observations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_market_observations_response& v);
    static ores::marketdata::messaging::put_many_market_observations_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_market_observations_response& v);
    static ores::marketdata::messaging::put_many_market_observations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_market_observation_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_market_observation_request& v);
    static ores::marketdata::messaging::delete_market_observation_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_market_observation_request& v);
    static ores::marketdata::messaging::delete_market_observation_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_market_observation_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_market_observation_response& v);
    static ores::marketdata::messaging::delete_market_observation_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_market_observation_response& v);
    static ores::marketdata::messaging::delete_market_observation_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_market_observations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_market_observations_request& v);
    static ores::marketdata::messaging::delete_many_market_observations_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_market_observations_request& v);
    static ores::marketdata::messaging::delete_many_market_observations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_market_observations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_market_observations_response& v);
    static ores::marketdata::messaging::delete_many_market_observations_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_market_observations_response& v);
    static ores::marketdata::messaging::delete_many_market_observations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_by_series_id_market_observations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_by_series_id_market_observations_request& v);
    static ores::marketdata::messaging::list_by_series_id_market_observations_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::list_by_series_id_market_observations_request& v);
    static ores::marketdata::messaging::list_by_series_id_market_observations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_by_series_id_market_observations_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::marketdata::messaging::list_by_series_id_market_observations_response& v);
    static ores::marketdata::messaging::list_by_series_id_market_observations_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::list_by_series_id_market_observations_response& v);
    static ores::marketdata::messaging::list_by_series_id_market_observations_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
