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
#ifndef ORES_MARKETDATA_API_MESSAGING_SUBSCRIPTION_INTERVAL_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_SUBSCRIPTION_INTERVAL_PROTOCOL_HPP

#include "ores.marketdata.api/domain/subscription_interval.hpp"
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

struct subscription_interval_key {
    std::string provider_code;
};

struct subscription_interval_write {
    boost::uuids::uuid id;
    boost::uuids::uuid party_id;
    std::string provider_code;
    std::chrono::system_clock::time_point interval_from;
    boost::uuids::uuid feed_id;
    bool display;
    bool non_display;
    bool derived;
    std::optional<std::chrono::system_clock::time_point> interval_to;
};

struct subscription_interval_change {
    subscription_interval_write write;
    ores::utility::domain::precondition precondition;
};

struct subscription_interval_removal {
    subscription_interval_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct subscription_interval_lookup {
    subscription_interval_key key;
    std::optional<ores::marketdata::domain::subscription_interval> subscription_interval;
};

struct subscription_intervals_filter {
    std::optional<boost::uuids::uuid> party_id;
    std::optional<std::string> provider_code;
    std::optional<boost::uuids::uuid> feed_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> party_id_one_of;
    std::optional<std::vector<std::string>> provider_code_one_of;
    std::optional<std::vector<boost::uuids::uuid>> feed_id_one_of;
};

struct subscription_interval_event {
    boost::uuids::uuid event_id;
    subscription_interval_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct subscription_interval_version_key {
    subscription_interval_key subscription_interval;
    std::uint32_t version;
};

struct subscription_interval_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_subscription_intervals_request {
    using response_type = struct list_subscription_intervals_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.subscription_intervals.list";
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
    std::optional<subscription_intervals_filter> filter;
    std::optional<std::string> as_of;
};

struct list_subscription_intervals_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::subscription_interval> subscription_intervals;
    std::uint64_t total;
};

struct get_subscription_interval_request {
    using response_type = struct get_subscription_interval_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.subscription_intervals.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    subscription_interval_key key;
};

struct get_subscription_interval_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::subscription_interval> subscription_interval;
};

struct get_many_subscription_intervals_request {
    using response_type = struct get_many_subscription_intervals_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.subscription_intervals.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<subscription_interval_key> keys;
};

struct get_many_subscription_intervals_response {
    ores::utility::domain::result result;
    std::vector<subscription_interval_lookup> entries;
};

struct put_subscription_interval_request {
    using response_type = struct put_subscription_interval_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.subscription_intervals.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    subscription_interval_change change;
    ores::utility::domain::change_intent intent;
};

struct put_subscription_interval_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::subscription_interval> subscription_interval;
};

struct put_many_subscription_intervals_request {
    using response_type = struct put_many_subscription_intervals_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.subscription_intervals.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<subscription_interval_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_subscription_intervals_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::subscription_interval> subscription_intervals;
};

struct delete_subscription_interval_request {
    using response_type = struct delete_subscription_interval_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.subscription_intervals.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    subscription_interval_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_subscription_interval_response {
    ores::utility::domain::result result;
};

struct delete_many_subscription_intervals_request {
    using response_type = struct delete_many_subscription_intervals_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.subscription_intervals.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<subscription_interval_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_subscription_intervals_response {
    ores::utility::domain::result result;
};

struct list_subscription_interval_versions_request {
    using response_type = struct list_subscription_interval_versions_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.subscription_intervals_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    subscription_interval_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<subscription_interval_versions_filter> filter;
};

struct list_subscription_interval_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::subscription_interval> versions;
    std::uint64_t total;
};

struct get_subscription_interval_version_request {
    using response_type = struct get_subscription_interval_version_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.subscription_intervals_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    subscription_interval_version_key key;
};

struct get_subscription_interval_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::subscription_interval> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace subscription_interval_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.subscription_intervals_events.created";
inline constexpr std::string_view updated = "marketdata.v1.subscription_intervals_events.updated";
inline constexpr std::string_view deleted = "marketdata.v1.subscription_intervals_events.deleted";
}
/**
 * @brief The members of subscription_interval_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const subscription_interval_key&) {
    return std::tuple{
        utility::codec::field{"provider_code", &subscription_interval_key::provider_code},
    };
}

/**
 * @brief The members of subscription_interval_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const subscription_interval_write&) {
    return std::tuple{
        utility::codec::field{"id", &subscription_interval_write::id},
        utility::codec::field{"party_id", &subscription_interval_write::party_id},
        utility::codec::field{"provider_code", &subscription_interval_write::provider_code},
        utility::codec::field{"interval_from", &subscription_interval_write::interval_from},
        utility::codec::field{"feed_id", &subscription_interval_write::feed_id},
        utility::codec::field{"display", &subscription_interval_write::display},
        utility::codec::field{"non_display", &subscription_interval_write::non_display},
        utility::codec::field{"derived", &subscription_interval_write::derived},
        utility::codec::field{"interval_to", &subscription_interval_write::interval_to},
    };
}

/**
 * @brief The members of subscription_interval_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const subscription_interval_change&) {
    return std::tuple{
        utility::codec::field{"write", &subscription_interval_change::write},
        utility::codec::field{"precondition", &subscription_interval_change::precondition},
    };
}

/**
 * @brief The members of subscription_interval_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const subscription_interval_removal&) {
    return std::tuple{
        utility::codec::field{"key", &subscription_interval_removal::key},
        utility::codec::field{"precondition", &subscription_interval_removal::precondition},
    };
}

/**
 * @brief The members of subscription_interval_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const subscription_interval_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &subscription_interval_lookup::key},
        utility::codec::field{"subscription_interval",
                              &subscription_interval_lookup::subscription_interval},
    };
}

/**
 * @brief The members of subscription_intervals_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const subscription_intervals_filter&) {
    return std::tuple{
        utility::codec::field{"party_id", &subscription_intervals_filter::party_id},
        utility::codec::field{"provider_code", &subscription_intervals_filter::provider_code},
        utility::codec::field{"feed_id", &subscription_intervals_filter::feed_id},
        utility::codec::field{"id_one_of", &subscription_intervals_filter::id_one_of},
        utility::codec::field{"party_id_one_of", &subscription_intervals_filter::party_id_one_of},
        utility::codec::field{"provider_code_one_of",
                              &subscription_intervals_filter::provider_code_one_of},
        utility::codec::field{"feed_id_one_of", &subscription_intervals_filter::feed_id_one_of},
    };
}

/**
 * @brief The members of subscription_interval_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const subscription_interval_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &subscription_interval_event::event_id},
        utility::codec::field{"key", &subscription_interval_event::key},
        utility::codec::field{"action", &subscription_interval_event::action},
        utility::codec::field{"version", &subscription_interval_event::version},
        utility::codec::field{"occurred_at", &subscription_interval_event::occurred_at},
        utility::codec::field{"correlation_id", &subscription_interval_event::correlation_id},
    };
}

/**
 * @brief The members of subscription_interval_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const subscription_interval_version_key&) {
    return std::tuple{
        utility::codec::field{"subscription_interval",
                              &subscription_interval_version_key::subscription_interval},
        utility::codec::field{"version", &subscription_interval_version_key::version},
    };
}

/**
 * @brief The members of subscription_interval_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const subscription_interval_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &subscription_interval_versions_filter::version},
        utility::codec::field{"from_version", &subscription_interval_versions_filter::from_version},
        utility::codec::field{"to_version", &subscription_interval_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_subscription_intervals_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_subscription_intervals_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_subscription_intervals_request::offset},
        utility::codec::field{"limit", &list_subscription_intervals_request::limit},
        utility::codec::field{"order", &list_subscription_intervals_request::order},
        utility::codec::field{"filter", &list_subscription_intervals_request::filter},
        utility::codec::field{"as_of", &list_subscription_intervals_request::as_of},
    };
}

/**
 * @brief The members of list_subscription_intervals_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_subscription_intervals_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_subscription_intervals_response::result},
        utility::codec::field{"subscription_intervals",
                              &list_subscription_intervals_response::subscription_intervals},
        utility::codec::field{"total", &list_subscription_intervals_response::total},
    };
}

/**
 * @brief The members of get_subscription_interval_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_subscription_interval_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_subscription_interval_request::key},
    };
}

/**
 * @brief The members of get_subscription_interval_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_subscription_interval_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_subscription_interval_response::result},
        utility::codec::field{"subscription_interval",
                              &get_subscription_interval_response::subscription_interval},
    };
}

/**
 * @brief The members of get_many_subscription_intervals_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_subscription_intervals_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_subscription_intervals_request::keys},
    };
}

/**
 * @brief The members of get_many_subscription_intervals_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_subscription_intervals_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_subscription_intervals_response::result},
        utility::codec::field{"entries", &get_many_subscription_intervals_response::entries},
    };
}

/**
 * @brief The members of put_subscription_interval_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_subscription_interval_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_subscription_interval_request::change},
        utility::codec::field{"intent", &put_subscription_interval_request::intent},
    };
}

/**
 * @brief The members of put_subscription_interval_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_subscription_interval_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_subscription_interval_response::result},
        utility::codec::field{"subscription_interval",
                              &put_subscription_interval_response::subscription_interval},
    };
}

/**
 * @brief The members of put_many_subscription_intervals_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_subscription_intervals_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_subscription_intervals_request::changes},
        utility::codec::field{"intent", &put_many_subscription_intervals_request::intent},
    };
}

/**
 * @brief The members of put_many_subscription_intervals_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_subscription_intervals_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_subscription_intervals_response::result},
        utility::codec::field{"subscription_intervals",
                              &put_many_subscription_intervals_response::subscription_intervals},
    };
}

/**
 * @brief The members of delete_subscription_interval_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_subscription_interval_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_subscription_interval_request::removal},
        utility::codec::field{"intent", &delete_subscription_interval_request::intent},
    };
}

/**
 * @brief The members of delete_subscription_interval_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_subscription_interval_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_subscription_interval_response::result},
    };
}

/**
 * @brief The members of delete_many_subscription_intervals_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_subscription_intervals_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_subscription_intervals_request::removals},
        utility::codec::field{"intent", &delete_many_subscription_intervals_request::intent},
    };
}

/**
 * @brief The members of delete_many_subscription_intervals_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_subscription_intervals_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_subscription_intervals_response::result},
    };
}

/**
 * @brief The members of list_subscription_interval_versions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_subscription_interval_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_subscription_interval_versions_request::key},
        utility::codec::field{"offset", &list_subscription_interval_versions_request::offset},
        utility::codec::field{"limit", &list_subscription_interval_versions_request::limit},
        utility::codec::field{"order", &list_subscription_interval_versions_request::order},
        utility::codec::field{"filter", &list_subscription_interval_versions_request::filter},
    };
}

/**
 * @brief The members of list_subscription_interval_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_subscription_interval_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_subscription_interval_versions_response::result},
        utility::codec::field{"versions", &list_subscription_interval_versions_response::versions},
        utility::codec::field{"total", &list_subscription_interval_versions_response::total},
    };
}

/**
 * @brief The members of get_subscription_interval_version_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_subscription_interval_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_subscription_interval_version_request::key},
    };
}

/**
 * @brief The members of get_subscription_interval_version_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_subscription_interval_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_subscription_interval_version_response::result},
        utility::codec::field{"version", &get_subscription_interval_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::subscription_interval_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::subscription_interval_key& v);
    static ores::marketdata::messaging::subscription_interval_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::subscription_interval_key& v);
    static ores::marketdata::messaging::subscription_interval_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::subscription_interval_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::subscription_interval_write& v);
    static ores::marketdata::messaging::subscription_interval_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::subscription_interval_write& v);
    static ores::marketdata::messaging::subscription_interval_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::subscription_interval_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::subscription_interval_change& v);
    static ores::marketdata::messaging::subscription_interval_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::subscription_interval_change& v);
    static ores::marketdata::messaging::subscription_interval_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::subscription_interval_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::subscription_interval_removal& v);
    static ores::marketdata::messaging::subscription_interval_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::subscription_interval_removal& v);
    static ores::marketdata::messaging::subscription_interval_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::subscription_interval_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::subscription_interval_lookup& v);
    static ores::marketdata::messaging::subscription_interval_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::subscription_interval_lookup& v);
    static ores::marketdata::messaging::subscription_interval_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::subscription_intervals_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::subscription_intervals_filter& v);
    static ores::marketdata::messaging::subscription_intervals_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::subscription_intervals_filter& v);
    static ores::marketdata::messaging::subscription_intervals_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::subscription_interval_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::subscription_interval_event& v);
    static ores::marketdata::messaging::subscription_interval_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::subscription_interval_event& v);
    static ores::marketdata::messaging::subscription_interval_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::subscription_interval_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::subscription_interval_version_key& v);
    static ores::marketdata::messaging::subscription_interval_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::subscription_interval_version_key& v);
    static ores::marketdata::messaging::subscription_interval_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::subscription_interval_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::subscription_interval_versions_filter& v);
    static ores::marketdata::messaging::subscription_interval_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::subscription_interval_versions_filter& v);
    static ores::marketdata::messaging::subscription_interval_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_subscription_intervals_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_subscription_intervals_request& v);
    static ores::marketdata::messaging::list_subscription_intervals_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_subscription_intervals_request& v);
    static ores::marketdata::messaging::list_subscription_intervals_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_subscription_intervals_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_subscription_intervals_response& v);
    static ores::marketdata::messaging::list_subscription_intervals_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_subscription_intervals_response& v);
    static ores::marketdata::messaging::list_subscription_intervals_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_subscription_interval_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_subscription_interval_request& v);
    static ores::marketdata::messaging::get_subscription_interval_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_subscription_interval_request& v);
    static ores::marketdata::messaging::get_subscription_interval_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_subscription_interval_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_subscription_interval_response& v);
    static ores::marketdata::messaging::get_subscription_interval_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_subscription_interval_response& v);
    static ores::marketdata::messaging::get_subscription_interval_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_subscription_intervals_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_subscription_intervals_request& v);
    static ores::marketdata::messaging::get_many_subscription_intervals_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_subscription_intervals_request& v);
    static ores::marketdata::messaging::get_many_subscription_intervals_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_subscription_intervals_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_subscription_intervals_response& v);
    static ores::marketdata::messaging::get_many_subscription_intervals_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_subscription_intervals_response& v);
    static ores::marketdata::messaging::get_many_subscription_intervals_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_subscription_interval_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_subscription_interval_request& v);
    static ores::marketdata::messaging::put_subscription_interval_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_subscription_interval_request& v);
    static ores::marketdata::messaging::put_subscription_interval_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_subscription_interval_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_subscription_interval_response& v);
    static ores::marketdata::messaging::put_subscription_interval_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_subscription_interval_response& v);
    static ores::marketdata::messaging::put_subscription_interval_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_subscription_intervals_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_subscription_intervals_request& v);
    static ores::marketdata::messaging::put_many_subscription_intervals_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_subscription_intervals_request& v);
    static ores::marketdata::messaging::put_many_subscription_intervals_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_subscription_intervals_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_subscription_intervals_response& v);
    static ores::marketdata::messaging::put_many_subscription_intervals_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_subscription_intervals_response& v);
    static ores::marketdata::messaging::put_many_subscription_intervals_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_subscription_interval_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_subscription_interval_request& v);
    static ores::marketdata::messaging::delete_subscription_interval_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_subscription_interval_request& v);
    static ores::marketdata::messaging::delete_subscription_interval_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_subscription_interval_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_subscription_interval_response& v);
    static ores::marketdata::messaging::delete_subscription_interval_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_subscription_interval_response& v);
    static ores::marketdata::messaging::delete_subscription_interval_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_subscription_intervals_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_subscription_intervals_request& v);
    static ores::marketdata::messaging::delete_many_subscription_intervals_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_subscription_intervals_request& v);
    static ores::marketdata::messaging::delete_many_subscription_intervals_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_subscription_intervals_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_subscription_intervals_response& v);
    static ores::marketdata::messaging::delete_many_subscription_intervals_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::delete_many_subscription_intervals_response& v);
    static ores::marketdata::messaging::delete_many_subscription_intervals_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_subscription_interval_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_subscription_interval_versions_request& v);
    static ores::marketdata::messaging::list_subscription_interval_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::list_subscription_interval_versions_request& v);
    static ores::marketdata::messaging::list_subscription_interval_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_subscription_interval_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_subscription_interval_versions_response& v);
    static ores::marketdata::messaging::list_subscription_interval_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::list_subscription_interval_versions_response& v);
    static ores::marketdata::messaging::list_subscription_interval_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_subscription_interval_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_subscription_interval_version_request& v);
    static ores::marketdata::messaging::get_subscription_interval_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_subscription_interval_version_request& v);
    static ores::marketdata::messaging::get_subscription_interval_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_subscription_interval_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_subscription_interval_version_response& v);
    static ores::marketdata::messaging::get_subscription_interval_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_subscription_interval_version_response& v);
    static ores::marketdata::messaging::get_subscription_interval_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
