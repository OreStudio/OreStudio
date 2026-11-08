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
#ifndef ORES_MARKETDATA_API_MESSAGING_DEMAND_INTERVAL_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_DEMAND_INTERVAL_PROTOCOL_HPP

#include "ores.marketdata.api/domain/demand_interval_codec.hpp"
#include "ores.marketdata.api/export.hpp"
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

namespace ores::marketdata::messaging {

struct demand_interval_key {
    std::string oresmd_uri;

    friend bool operator==(const demand_interval_key&, const demand_interval_key&) = default;
};

struct demand_interval_write {
    boost::uuids::uuid id;
    boost::uuids::uuid party_id;
    std::string provider_code;
    std::string oresmd_uri;
    std::chrono::system_clock::time_point interval_from;
    boost::uuids::uuid feed_id;
    std::optional<std::chrono::system_clock::time_point> interval_to;

    friend bool operator==(const demand_interval_write&, const demand_interval_write&) = default;
};

struct demand_interval_change {
    demand_interval_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const demand_interval_change&, const demand_interval_change&) = default;
};

struct demand_interval_removal {
    demand_interval_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const demand_interval_removal&,
                           const demand_interval_removal&) = default;
};

struct demand_interval_lookup {
    demand_interval_key key;
    std::optional<ores::marketdata::domain::demand_interval> demand_interval;

    friend bool operator==(const demand_interval_lookup&, const demand_interval_lookup&) = default;
};

struct demand_intervals_filter {
    std::optional<boost::uuids::uuid> party_id;
    std::optional<std::string> provider_code;
    std::optional<boost::uuids::uuid> feed_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> party_id_one_of;
    std::optional<std::vector<std::string>> provider_code_one_of;
    std::optional<std::vector<boost::uuids::uuid>> feed_id_one_of;

    friend bool operator==(const demand_intervals_filter&,
                           const demand_intervals_filter&) = default;
};

struct demand_interval_event {
    boost::uuids::uuid event_id;
    demand_interval_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const demand_interval_event&, const demand_interval_event&) = default;
};

struct demand_interval_version_key {
    demand_interval_key demand_interval;
    std::uint32_t version{};

    friend bool operator==(const demand_interval_version_key&,
                           const demand_interval_version_key&) = default;
};

struct demand_interval_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const demand_interval_versions_filter&,
                           const demand_interval_versions_filter&) = default;
};

struct list_demand_intervals_request {
    using response_type = struct list_demand_intervals_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.demand_intervals.list";
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
    std::optional<demand_intervals_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_demand_intervals_request&,
                           const list_demand_intervals_request&) = default;
};

struct list_demand_intervals_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::demand_interval> demand_intervals;
    std::uint64_t total{};

    friend bool operator==(const list_demand_intervals_response&,
                           const list_demand_intervals_response&) = default;
};

struct get_demand_interval_request {
    using response_type = struct get_demand_interval_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.demand_intervals.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    demand_interval_key key;

    friend bool operator==(const get_demand_interval_request&,
                           const get_demand_interval_request&) = default;
};

struct get_demand_interval_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::demand_interval> demand_interval;

    friend bool operator==(const get_demand_interval_response&,
                           const get_demand_interval_response&) = default;
};

struct get_many_demand_intervals_request {
    using response_type = struct get_many_demand_intervals_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.demand_intervals.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<demand_interval_key> keys;

    friend bool operator==(const get_many_demand_intervals_request&,
                           const get_many_demand_intervals_request&) = default;
};

struct get_many_demand_intervals_response {
    ores::utility::domain::result result;
    std::vector<demand_interval_lookup> entries;

    friend bool operator==(const get_many_demand_intervals_response&,
                           const get_many_demand_intervals_response&) = default;
};

struct put_demand_interval_request {
    using response_type = struct put_demand_interval_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.demand_intervals.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    demand_interval_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_demand_interval_request&,
                           const put_demand_interval_request&) = default;
};

struct put_demand_interval_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::demand_interval> demand_interval;

    friend bool operator==(const put_demand_interval_response&,
                           const put_demand_interval_response&) = default;
};

struct put_many_demand_intervals_request {
    using response_type = struct put_many_demand_intervals_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.demand_intervals.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<demand_interval_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_demand_intervals_request&,
                           const put_many_demand_intervals_request&) = default;
};

struct put_many_demand_intervals_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::demand_interval> demand_intervals;

    friend bool operator==(const put_many_demand_intervals_response&,
                           const put_many_demand_intervals_response&) = default;
};

struct delete_demand_interval_request {
    using response_type = struct delete_demand_interval_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.demand_intervals.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    demand_interval_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_demand_interval_request&,
                           const delete_demand_interval_request&) = default;
};

struct delete_demand_interval_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_demand_interval_response&,
                           const delete_demand_interval_response&) = default;
};

struct delete_many_demand_intervals_request {
    using response_type = struct delete_many_demand_intervals_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.demand_intervals.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<demand_interval_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_demand_intervals_request&,
                           const delete_many_demand_intervals_request&) = default;
};

struct delete_many_demand_intervals_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_demand_intervals_response&,
                           const delete_many_demand_intervals_response&) = default;
};

struct list_demand_interval_versions_request {
    using response_type = struct list_demand_interval_versions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.demand_intervals_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    demand_interval_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<demand_interval_versions_filter> filter;

    friend bool operator==(const list_demand_interval_versions_request&,
                           const list_demand_interval_versions_request&) = default;
};

struct list_demand_interval_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::demand_interval> versions;
    std::uint64_t total{};

    friend bool operator==(const list_demand_interval_versions_response&,
                           const list_demand_interval_versions_response&) = default;
};

struct get_demand_interval_version_request {
    using response_type = struct get_demand_interval_version_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.demand_intervals_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    demand_interval_version_key key;

    friend bool operator==(const get_demand_interval_version_request&,
                           const get_demand_interval_version_request&) = default;
};

struct get_demand_interval_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::demand_interval> version;

    friend bool operator==(const get_demand_interval_version_response&,
                           const get_demand_interval_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace demand_interval_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.events.demand_intervals.created";
inline constexpr std::string_view updated = "marketdata.v1.events.demand_intervals.updated";
inline constexpr std::string_view deleted = "marketdata.v1.events.demand_intervals.deleted";
}
/**
 * @brief The members of demand_interval_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<demand_interval_key>) {
    return std::tuple{
        utility::codec::field{"oresmd_uri", &demand_interval_key::oresmd_uri},
    };
}

/**
 * @brief The members of demand_interval_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<demand_interval_write>) {
    return std::tuple{
        utility::codec::field{"id", &demand_interval_write::id},
        utility::codec::field{"party_id", &demand_interval_write::party_id},
        utility::codec::field{"provider_code", &demand_interval_write::provider_code},
        utility::codec::field{"oresmd_uri", &demand_interval_write::oresmd_uri},
        utility::codec::field{"interval_from", &demand_interval_write::interval_from},
        utility::codec::field{"feed_id", &demand_interval_write::feed_id},
        utility::codec::field{"interval_to", &demand_interval_write::interval_to},
    };
}

/**
 * @brief The members of demand_interval_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<demand_interval_change>) {
    return std::tuple{
        utility::codec::field{"write", &demand_interval_change::write},
        utility::codec::field{"precondition", &demand_interval_change::precondition},
    };
}

/**
 * @brief The members of demand_interval_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<demand_interval_removal>) {
    return std::tuple{
        utility::codec::field{"key", &demand_interval_removal::key},
        utility::codec::field{"precondition", &demand_interval_removal::precondition},
    };
}

/**
 * @brief The members of demand_interval_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<demand_interval_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &demand_interval_lookup::key},
        utility::codec::field{"demand_interval", &demand_interval_lookup::demand_interval},
    };
}

/**
 * @brief The members of demand_intervals_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<demand_intervals_filter>) {
    return std::tuple{
        utility::codec::field{"party_id", &demand_intervals_filter::party_id},
        utility::codec::field{"provider_code", &demand_intervals_filter::provider_code},
        utility::codec::field{"feed_id", &demand_intervals_filter::feed_id},
        utility::codec::field{"id_one_of", &demand_intervals_filter::id_one_of},
        utility::codec::field{"party_id_one_of", &demand_intervals_filter::party_id_one_of},
        utility::codec::field{"provider_code_one_of",
                              &demand_intervals_filter::provider_code_one_of},
        utility::codec::field{"feed_id_one_of", &demand_intervals_filter::feed_id_one_of},
    };
}

/**
 * @brief The members of demand_interval_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<demand_interval_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &demand_interval_event::event_id},
        utility::codec::field{"key", &demand_interval_event::key},
        utility::codec::field{"action", &demand_interval_event::action},
        utility::codec::field{"version", &demand_interval_event::version},
        utility::codec::field{"occurred_at", &demand_interval_event::occurred_at},
        utility::codec::field{"correlation_id", &demand_interval_event::correlation_id},
    };
}

/**
 * @brief The members of demand_interval_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<demand_interval_version_key>) {
    return std::tuple{
        utility::codec::field{"demand_interval", &demand_interval_version_key::demand_interval},
        utility::codec::field{"version", &demand_interval_version_key::version},
    };
}

/**
 * @brief The members of demand_interval_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<demand_interval_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &demand_interval_versions_filter::version},
        utility::codec::field{"from_version", &demand_interval_versions_filter::from_version},
        utility::codec::field{"to_version", &demand_interval_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_demand_intervals_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_demand_intervals_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_demand_intervals_request::offset},
        utility::codec::field{"limit", &list_demand_intervals_request::limit},
        utility::codec::field{"order", &list_demand_intervals_request::order},
        utility::codec::field{"filter", &list_demand_intervals_request::filter},
        utility::codec::field{"as_of", &list_demand_intervals_request::as_of},
    };
}

/**
 * @brief The members of list_demand_intervals_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_demand_intervals_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_demand_intervals_response::result},
        utility::codec::field{"demand_intervals",
                              &list_demand_intervals_response::demand_intervals},
        utility::codec::field{"total", &list_demand_intervals_response::total},
    };
}

/**
 * @brief The members of get_demand_interval_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_demand_interval_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_demand_interval_request::key},
    };
}

/**
 * @brief The members of get_demand_interval_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_demand_interval_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_demand_interval_response::result},
        utility::codec::field{"demand_interval", &get_demand_interval_response::demand_interval},
    };
}

/**
 * @brief The members of get_many_demand_intervals_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_demand_intervals_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_demand_intervals_request::keys},
    };
}

/**
 * @brief The members of get_many_demand_intervals_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_demand_intervals_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_demand_intervals_response::result},
        utility::codec::field{"entries", &get_many_demand_intervals_response::entries},
    };
}

/**
 * @brief The members of put_demand_interval_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_demand_interval_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_demand_interval_request::change},
        utility::codec::field{"intent", &put_demand_interval_request::intent},
    };
}

/**
 * @brief The members of put_demand_interval_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_demand_interval_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_demand_interval_response::result},
        utility::codec::field{"demand_interval", &put_demand_interval_response::demand_interval},
    };
}

/**
 * @brief The members of put_many_demand_intervals_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_demand_intervals_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_demand_intervals_request::changes},
        utility::codec::field{"intent", &put_many_demand_intervals_request::intent},
    };
}

/**
 * @brief The members of put_many_demand_intervals_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_demand_intervals_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_demand_intervals_response::result},
        utility::codec::field{"demand_intervals",
                              &put_many_demand_intervals_response::demand_intervals},
    };
}

/**
 * @brief The members of delete_demand_interval_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_demand_interval_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_demand_interval_request::removal},
        utility::codec::field{"intent", &delete_demand_interval_request::intent},
    };
}

/**
 * @brief The members of delete_demand_interval_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_demand_interval_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_demand_interval_response::result},
    };
}

/**
 * @brief The members of delete_many_demand_intervals_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_demand_intervals_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_demand_intervals_request::removals},
        utility::codec::field{"intent", &delete_many_demand_intervals_request::intent},
    };
}

/**
 * @brief The members of delete_many_demand_intervals_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_demand_intervals_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_demand_intervals_response::result},
    };
}

/**
 * @brief The members of list_demand_interval_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_demand_interval_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_demand_interval_versions_request::key},
        utility::codec::field{"offset", &list_demand_interval_versions_request::offset},
        utility::codec::field{"limit", &list_demand_interval_versions_request::limit},
        utility::codec::field{"order", &list_demand_interval_versions_request::order},
        utility::codec::field{"filter", &list_demand_interval_versions_request::filter},
    };
}

/**
 * @brief The members of list_demand_interval_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_demand_interval_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_demand_interval_versions_response::result},
        utility::codec::field{"versions", &list_demand_interval_versions_response::versions},
        utility::codec::field{"total", &list_demand_interval_versions_response::total},
    };
}

/**
 * @brief The members of get_demand_interval_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_demand_interval_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_demand_interval_version_request::key},
    };
}

/**
 * @brief The members of get_demand_interval_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_demand_interval_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_demand_interval_version_response::result},
        utility::codec::field{"version", &get_demand_interval_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::demand_interval_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::demand_interval_key& v);
    static ores::marketdata::messaging::demand_interval_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::demand_interval_key& v);
    static ores::marketdata::messaging::demand_interval_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::demand_interval_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::demand_interval_write& v);
    static ores::marketdata::messaging::demand_interval_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::demand_interval_write& v);
    static ores::marketdata::messaging::demand_interval_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::demand_interval_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::demand_interval_change& v);
    static ores::marketdata::messaging::demand_interval_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::demand_interval_change& v);
    static ores::marketdata::messaging::demand_interval_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::demand_interval_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::demand_interval_removal& v);
    static ores::marketdata::messaging::demand_interval_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::demand_interval_removal& v);
    static ores::marketdata::messaging::demand_interval_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::demand_interval_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::demand_interval_lookup& v);
    static ores::marketdata::messaging::demand_interval_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::demand_interval_lookup& v);
    static ores::marketdata::messaging::demand_interval_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::demand_intervals_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::demand_intervals_filter& v);
    static ores::marketdata::messaging::demand_intervals_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::demand_intervals_filter& v);
    static ores::marketdata::messaging::demand_intervals_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::demand_interval_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::demand_interval_event& v);
    static ores::marketdata::messaging::demand_interval_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::demand_interval_event& v);
    static ores::marketdata::messaging::demand_interval_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::demand_interval_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::demand_interval_version_key& v);
    static ores::marketdata::messaging::demand_interval_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::demand_interval_version_key& v);
    static ores::marketdata::messaging::demand_interval_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::demand_interval_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::demand_interval_versions_filter& v);
    static ores::marketdata::messaging::demand_interval_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::demand_interval_versions_filter& v);
    static ores::marketdata::messaging::demand_interval_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_demand_intervals_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_demand_intervals_request& v);
    static ores::marketdata::messaging::list_demand_intervals_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_demand_intervals_request& v);
    static ores::marketdata::messaging::list_demand_intervals_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_demand_intervals_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_demand_intervals_response& v);
    static ores::marketdata::messaging::list_demand_intervals_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_demand_intervals_response& v);
    static ores::marketdata::messaging::list_demand_intervals_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_demand_interval_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_demand_interval_request& v);
    static ores::marketdata::messaging::get_demand_interval_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_demand_interval_request& v);
    static ores::marketdata::messaging::get_demand_interval_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_demand_interval_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_demand_interval_response& v);
    static ores::marketdata::messaging::get_demand_interval_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_demand_interval_response& v);
    static ores::marketdata::messaging::get_demand_interval_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_demand_intervals_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_demand_intervals_request& v);
    static ores::marketdata::messaging::get_many_demand_intervals_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_demand_intervals_request& v);
    static ores::marketdata::messaging::get_many_demand_intervals_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_demand_intervals_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_demand_intervals_response& v);
    static ores::marketdata::messaging::get_many_demand_intervals_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_demand_intervals_response& v);
    static ores::marketdata::messaging::get_many_demand_intervals_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_demand_interval_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_demand_interval_request& v);
    static ores::marketdata::messaging::put_demand_interval_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_demand_interval_request& v);
    static ores::marketdata::messaging::put_demand_interval_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_demand_interval_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_demand_interval_response& v);
    static ores::marketdata::messaging::put_demand_interval_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_demand_interval_response& v);
    static ores::marketdata::messaging::put_demand_interval_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_demand_intervals_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_demand_intervals_request& v);
    static ores::marketdata::messaging::put_many_demand_intervals_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_demand_intervals_request& v);
    static ores::marketdata::messaging::put_many_demand_intervals_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_demand_intervals_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_demand_intervals_response& v);
    static ores::marketdata::messaging::put_many_demand_intervals_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_demand_intervals_response& v);
    static ores::marketdata::messaging::put_many_demand_intervals_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_demand_interval_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_demand_interval_request& v);
    static ores::marketdata::messaging::delete_demand_interval_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_demand_interval_request& v);
    static ores::marketdata::messaging::delete_demand_interval_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_demand_interval_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_demand_interval_response& v);
    static ores::marketdata::messaging::delete_demand_interval_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_demand_interval_response& v);
    static ores::marketdata::messaging::delete_demand_interval_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_demand_intervals_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_demand_intervals_request& v);
    static ores::marketdata::messaging::delete_many_demand_intervals_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_demand_intervals_request& v);
    static ores::marketdata::messaging::delete_many_demand_intervals_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_demand_intervals_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_demand_intervals_response& v);
    static ores::marketdata::messaging::delete_many_demand_intervals_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_demand_intervals_response& v);
    static ores::marketdata::messaging::delete_many_demand_intervals_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_demand_interval_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_demand_interval_versions_request& v);
    static ores::marketdata::messaging::list_demand_interval_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_demand_interval_versions_request& v);
    static ores::marketdata::messaging::list_demand_interval_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_demand_interval_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_demand_interval_versions_response& v);
    static ores::marketdata::messaging::list_demand_interval_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_demand_interval_versions_response& v);
    static ores::marketdata::messaging::list_demand_interval_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_demand_interval_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_demand_interval_version_request& v);
    static ores::marketdata::messaging::get_demand_interval_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_demand_interval_version_request& v);
    static ores::marketdata::messaging::get_demand_interval_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_demand_interval_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_demand_interval_version_response& v);
    static ores::marketdata::messaging::get_demand_interval_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_demand_interval_version_response& v);
    static ores::marketdata::messaging::get_demand_interval_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
