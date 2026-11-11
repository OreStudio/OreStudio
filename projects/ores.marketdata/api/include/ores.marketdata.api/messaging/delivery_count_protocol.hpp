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
#ifndef ORES_MARKETDATA_API_MESSAGING_DELIVERY_COUNT_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_DELIVERY_COUNT_PROTOCOL_HPP

#include "ores.marketdata.api/domain/delivery_count_codec.hpp"
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

struct delivery_count_key {
    std::string oresmd_uri;

    friend bool operator==(const delivery_count_key&, const delivery_count_key&) = default;
};

struct delivery_count_write {
    boost::uuids::uuid id;
    boost::uuids::uuid party_id;
    std::string provider_code;
    std::string oresmd_uri;
    std::chrono::system_clock::time_point period_start;
    std::chrono::system_clock::time_point flushed_at;
    boost::uuids::uuid feed_id;
    std::int64_t deliveries{};

    friend bool operator==(const delivery_count_write&, const delivery_count_write&) = default;
};

struct delivery_count_change {
    delivery_count_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const delivery_count_change&, const delivery_count_change&) = default;
};

struct delivery_count_removal {
    delivery_count_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const delivery_count_removal&, const delivery_count_removal&) = default;
};

struct delivery_count_lookup {
    delivery_count_key key;
    std::optional<ores::marketdata::domain::delivery_count> delivery_count;

    friend bool operator==(const delivery_count_lookup&, const delivery_count_lookup&) = default;
};

struct delivery_counts_filter {
    std::optional<boost::uuids::uuid> party_id;
    std::optional<std::string> provider_code;
    std::optional<boost::uuids::uuid> feed_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> party_id_one_of;
    std::optional<std::vector<std::string>> provider_code_one_of;
    std::optional<std::vector<boost::uuids::uuid>> feed_id_one_of;

    friend bool operator==(const delivery_counts_filter&, const delivery_counts_filter&) = default;
};

struct delivery_count_event {
    boost::uuids::uuid event_id;
    delivery_count_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const delivery_count_event&, const delivery_count_event&) = default;
};

struct delivery_count_version_key {
    delivery_count_key delivery_count;
    std::uint32_t version{};

    friend bool operator==(const delivery_count_version_key&,
                           const delivery_count_version_key&) = default;
};

struct delivery_count_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const delivery_count_versions_filter&,
                           const delivery_count_versions_filter&) = default;
};

struct list_delivery_counts_request {
    using response_type = struct list_delivery_counts_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.delivery_counts.list";
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
    std::optional<delivery_counts_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_delivery_counts_request&,
                           const list_delivery_counts_request&) = default;
};

struct list_delivery_counts_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::delivery_count> delivery_counts;
    std::uint64_t total{};

    friend bool operator==(const list_delivery_counts_response&,
                           const list_delivery_counts_response&) = default;
};

struct get_delivery_count_request {
    using response_type = struct get_delivery_count_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.delivery_counts.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    delivery_count_key key;

    friend bool operator==(const get_delivery_count_request&,
                           const get_delivery_count_request&) = default;
};

struct get_delivery_count_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::delivery_count> delivery_count;

    friend bool operator==(const get_delivery_count_response&,
                           const get_delivery_count_response&) = default;
};

struct get_many_delivery_counts_request {
    using response_type = struct get_many_delivery_counts_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.delivery_counts.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<delivery_count_key> keys;

    friend bool operator==(const get_many_delivery_counts_request&,
                           const get_many_delivery_counts_request&) = default;
};

struct get_many_delivery_counts_response {
    ores::utility::domain::result result;
    std::vector<delivery_count_lookup> entries;

    friend bool operator==(const get_many_delivery_counts_response&,
                           const get_many_delivery_counts_response&) = default;
};

struct put_delivery_count_request {
    using response_type = struct put_delivery_count_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.delivery_counts.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    delivery_count_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_delivery_count_request&,
                           const put_delivery_count_request&) = default;
};

struct put_delivery_count_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::delivery_count> delivery_count;

    friend bool operator==(const put_delivery_count_response&,
                           const put_delivery_count_response&) = default;
};

struct put_many_delivery_counts_request {
    using response_type = struct put_many_delivery_counts_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.delivery_counts.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<delivery_count_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_delivery_counts_request&,
                           const put_many_delivery_counts_request&) = default;
};

struct put_many_delivery_counts_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::delivery_count> delivery_counts;

    friend bool operator==(const put_many_delivery_counts_response&,
                           const put_many_delivery_counts_response&) = default;
};

struct delete_delivery_count_request {
    using response_type = struct delete_delivery_count_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.delivery_counts.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    delivery_count_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_delivery_count_request&,
                           const delete_delivery_count_request&) = default;
};

struct delete_delivery_count_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_delivery_count_response&,
                           const delete_delivery_count_response&) = default;
};

struct delete_many_delivery_counts_request {
    using response_type = struct delete_many_delivery_counts_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.delivery_counts.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<delivery_count_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_delivery_counts_request&,
                           const delete_many_delivery_counts_request&) = default;
};

struct delete_many_delivery_counts_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_delivery_counts_response&,
                           const delete_many_delivery_counts_response&) = default;
};

struct list_delivery_count_versions_request {
    using response_type = struct list_delivery_count_versions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.delivery_counts_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    delivery_count_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<delivery_count_versions_filter> filter;

    friend bool operator==(const list_delivery_count_versions_request&,
                           const list_delivery_count_versions_request&) = default;
};

struct list_delivery_count_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::delivery_count> versions;
    std::uint64_t total{};

    friend bool operator==(const list_delivery_count_versions_response&,
                           const list_delivery_count_versions_response&) = default;
};

struct get_delivery_count_version_request {
    using response_type = struct get_delivery_count_version_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.delivery_counts_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    delivery_count_version_key key;

    friend bool operator==(const get_delivery_count_version_request&,
                           const get_delivery_count_version_request&) = default;
};

struct get_delivery_count_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::delivery_count> version;

    friend bool operator==(const get_delivery_count_version_response&,
                           const get_delivery_count_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace delivery_count_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.events.delivery_counts.created";
inline constexpr std::string_view updated = "marketdata.v1.events.delivery_counts.updated";
inline constexpr std::string_view deleted = "marketdata.v1.events.delivery_counts.deleted";
}
/**
 * @brief The members of delivery_count_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delivery_count_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"oresmd_uri", &delivery_count_key::oresmd_uri},
    };
}

/**
 * @brief The members of delivery_count_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delivery_count_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &delivery_count_write::id},
        utility::codec::field{"party_id", &delivery_count_write::party_id},
        utility::codec::field{"provider_code", &delivery_count_write::provider_code},
        utility::codec::field{"oresmd_uri", &delivery_count_write::oresmd_uri},
        utility::codec::field{"period_start", &delivery_count_write::period_start},
        utility::codec::field{"flushed_at", &delivery_count_write::flushed_at},
        utility::codec::field{"feed_id", &delivery_count_write::feed_id},
        utility::codec::field{"deliveries", &delivery_count_write::deliveries},
    };
}

/**
 * @brief The members of delivery_count_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delivery_count_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &delivery_count_change::write},
        utility::codec::field{"precondition", &delivery_count_change::precondition},
    };
}

/**
 * @brief The members of delivery_count_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delivery_count_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &delivery_count_removal::key},
        utility::codec::field{"precondition", &delivery_count_removal::precondition},
    };
}

/**
 * @brief The members of delivery_count_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delivery_count_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &delivery_count_lookup::key},
        utility::codec::field{"delivery_count", &delivery_count_lookup::delivery_count},
    };
}

/**
 * @brief The members of delivery_counts_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delivery_counts_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"party_id", &delivery_counts_filter::party_id},
        utility::codec::field{"provider_code", &delivery_counts_filter::provider_code},
        utility::codec::field{"feed_id", &delivery_counts_filter::feed_id},
        utility::codec::field{"id_one_of", &delivery_counts_filter::id_one_of},
        utility::codec::field{"party_id_one_of", &delivery_counts_filter::party_id_one_of},
        utility::codec::field{"provider_code_one_of",
                              &delivery_counts_filter::provider_code_one_of},
        utility::codec::field{"feed_id_one_of", &delivery_counts_filter::feed_id_one_of},
    };
}

/**
 * @brief The members of delivery_count_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delivery_count_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &delivery_count_event::event_id},
        utility::codec::field{"key", &delivery_count_event::key},
        utility::codec::field{"action", &delivery_count_event::action},
        utility::codec::field{"version", &delivery_count_event::version},
        utility::codec::field{"occurred_at", &delivery_count_event::occurred_at},
        utility::codec::field{"correlation_id", &delivery_count_event::correlation_id},
    };
}

/**
 * @brief The members of delivery_count_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delivery_count_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"delivery_count", &delivery_count_version_key::delivery_count},
        utility::codec::field{"version", &delivery_count_version_key::version},
    };
}

/**
 * @brief The members of delivery_count_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delivery_count_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &delivery_count_versions_filter::version},
        utility::codec::field{"from_version", &delivery_count_versions_filter::from_version},
        utility::codec::field{"to_version", &delivery_count_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_delivery_counts_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_delivery_counts_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_delivery_counts_request::offset},
        utility::codec::field{"limit", &list_delivery_counts_request::limit},
        utility::codec::field{"order", &list_delivery_counts_request::order},
        utility::codec::field{"filter", &list_delivery_counts_request::filter},
        utility::codec::field{"as_of", &list_delivery_counts_request::as_of},
    };
}

/**
 * @brief The members of list_delivery_counts_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_delivery_counts_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_delivery_counts_response::result},
        utility::codec::field{"delivery_counts", &list_delivery_counts_response::delivery_counts},
        utility::codec::field{"total", &list_delivery_counts_response::total},
    };
}

/**
 * @brief The members of get_delivery_count_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_delivery_count_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_delivery_count_request::key},
    };
}

/**
 * @brief The members of get_delivery_count_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_delivery_count_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_delivery_count_response::result},
        utility::codec::field{"delivery_count", &get_delivery_count_response::delivery_count},
    };
}

/**
 * @brief The members of get_many_delivery_counts_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_delivery_counts_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_delivery_counts_request::keys},
    };
}

/**
 * @brief The members of get_many_delivery_counts_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_delivery_counts_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_delivery_counts_response::result},
        utility::codec::field{"entries", &get_many_delivery_counts_response::entries},
    };
}

/**
 * @brief The members of put_delivery_count_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_delivery_count_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_delivery_count_request::change},
        utility::codec::field{"intent", &put_delivery_count_request::intent},
    };
}

/**
 * @brief The members of put_delivery_count_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_delivery_count_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_delivery_count_response::result},
        utility::codec::field{"delivery_count", &put_delivery_count_response::delivery_count},
    };
}

/**
 * @brief The members of put_many_delivery_counts_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_delivery_counts_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_delivery_counts_request::changes},
        utility::codec::field{"intent", &put_many_delivery_counts_request::intent},
    };
}

/**
 * @brief The members of put_many_delivery_counts_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_delivery_counts_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_delivery_counts_response::result},
        utility::codec::field{"delivery_counts",
                              &put_many_delivery_counts_response::delivery_counts},
    };
}

/**
 * @brief The members of delete_delivery_count_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_delivery_count_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_delivery_count_request::removal},
        utility::codec::field{"intent", &delete_delivery_count_request::intent},
    };
}

/**
 * @brief The members of delete_delivery_count_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_delivery_count_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_delivery_count_response::result},
    };
}

/**
 * @brief The members of delete_many_delivery_counts_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_delivery_counts_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_delivery_counts_request::removals},
        utility::codec::field{"intent", &delete_many_delivery_counts_request::intent},
    };
}

/**
 * @brief The members of delete_many_delivery_counts_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_delivery_counts_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_delivery_counts_response::result},
    };
}

/**
 * @brief The members of list_delivery_count_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_delivery_count_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_delivery_count_versions_request::key},
        utility::codec::field{"offset", &list_delivery_count_versions_request::offset},
        utility::codec::field{"limit", &list_delivery_count_versions_request::limit},
        utility::codec::field{"order", &list_delivery_count_versions_request::order},
        utility::codec::field{"filter", &list_delivery_count_versions_request::filter},
    };
}

/**
 * @brief The members of list_delivery_count_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_delivery_count_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_delivery_count_versions_response::result},
        utility::codec::field{"versions", &list_delivery_count_versions_response::versions},
        utility::codec::field{"total", &list_delivery_count_versions_response::total},
    };
}

/**
 * @brief The members of get_delivery_count_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_delivery_count_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_delivery_count_version_request::key},
    };
}

/**
 * @brief The members of get_delivery_count_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_delivery_count_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_delivery_count_version_response::result},
        utility::codec::field{"version", &get_delivery_count_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::delivery_count_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::delivery_count_key& v);
    static ores::marketdata::messaging::delivery_count_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::delivery_count_key& v);
    static ores::marketdata::messaging::delivery_count_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::delivery_count_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::delivery_count_write& v);
    static ores::marketdata::messaging::delivery_count_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::delivery_count_write& v);
    static ores::marketdata::messaging::delivery_count_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::delivery_count_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::delivery_count_change& v);
    static ores::marketdata::messaging::delivery_count_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::delivery_count_change& v);
    static ores::marketdata::messaging::delivery_count_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::delivery_count_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::delivery_count_removal& v);
    static ores::marketdata::messaging::delivery_count_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::delivery_count_removal& v);
    static ores::marketdata::messaging::delivery_count_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::delivery_count_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::delivery_count_lookup& v);
    static ores::marketdata::messaging::delivery_count_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::delivery_count_lookup& v);
    static ores::marketdata::messaging::delivery_count_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::delivery_counts_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::delivery_counts_filter& v);
    static ores::marketdata::messaging::delivery_counts_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::delivery_counts_filter& v);
    static ores::marketdata::messaging::delivery_counts_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::delivery_count_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::delivery_count_event& v);
    static ores::marketdata::messaging::delivery_count_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::delivery_count_event& v);
    static ores::marketdata::messaging::delivery_count_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delivery_count_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::delivery_count_version_key& v);
    static ores::marketdata::messaging::delivery_count_version_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delivery_count_version_key& v);
    static ores::marketdata::messaging::delivery_count_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delivery_count_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delivery_count_versions_filter& v);
    static ores::marketdata::messaging::delivery_count_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delivery_count_versions_filter& v);
    static ores::marketdata::messaging::delivery_count_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_delivery_counts_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_delivery_counts_request& v);
    static ores::marketdata::messaging::list_delivery_counts_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_delivery_counts_request& v);
    static ores::marketdata::messaging::list_delivery_counts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_delivery_counts_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_delivery_counts_response& v);
    static ores::marketdata::messaging::list_delivery_counts_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_delivery_counts_response& v);
    static ores::marketdata::messaging::list_delivery_counts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_delivery_count_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_delivery_count_request& v);
    static ores::marketdata::messaging::get_delivery_count_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_delivery_count_request& v);
    static ores::marketdata::messaging::get_delivery_count_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_delivery_count_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_delivery_count_response& v);
    static ores::marketdata::messaging::get_delivery_count_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_delivery_count_response& v);
    static ores::marketdata::messaging::get_delivery_count_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_delivery_counts_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_delivery_counts_request& v);
    static ores::marketdata::messaging::get_many_delivery_counts_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_delivery_counts_request& v);
    static ores::marketdata::messaging::get_many_delivery_counts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_delivery_counts_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_delivery_counts_response& v);
    static ores::marketdata::messaging::get_many_delivery_counts_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_delivery_counts_response& v);
    static ores::marketdata::messaging::get_many_delivery_counts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_delivery_count_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::put_delivery_count_request& v);
    static ores::marketdata::messaging::put_delivery_count_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_delivery_count_request& v);
    static ores::marketdata::messaging::put_delivery_count_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_delivery_count_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_delivery_count_response& v);
    static ores::marketdata::messaging::put_delivery_count_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_delivery_count_response& v);
    static ores::marketdata::messaging::put_delivery_count_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_delivery_counts_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_delivery_counts_request& v);
    static ores::marketdata::messaging::put_many_delivery_counts_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_delivery_counts_request& v);
    static ores::marketdata::messaging::put_many_delivery_counts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_delivery_counts_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_delivery_counts_response& v);
    static ores::marketdata::messaging::put_many_delivery_counts_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_delivery_counts_response& v);
    static ores::marketdata::messaging::put_many_delivery_counts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_delivery_count_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_delivery_count_request& v);
    static ores::marketdata::messaging::delete_delivery_count_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_delivery_count_request& v);
    static ores::marketdata::messaging::delete_delivery_count_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_delivery_count_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_delivery_count_response& v);
    static ores::marketdata::messaging::delete_delivery_count_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_delivery_count_response& v);
    static ores::marketdata::messaging::delete_delivery_count_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_delivery_counts_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_delivery_counts_request& v);
    static ores::marketdata::messaging::delete_many_delivery_counts_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_delivery_counts_request& v);
    static ores::marketdata::messaging::delete_many_delivery_counts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_delivery_counts_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_delivery_counts_response& v);
    static ores::marketdata::messaging::delete_many_delivery_counts_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_delivery_counts_response& v);
    static ores::marketdata::messaging::delete_many_delivery_counts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_delivery_count_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_delivery_count_versions_request& v);
    static ores::marketdata::messaging::list_delivery_count_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_delivery_count_versions_request& v);
    static ores::marketdata::messaging::list_delivery_count_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_delivery_count_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_delivery_count_versions_response& v);
    static ores::marketdata::messaging::list_delivery_count_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_delivery_count_versions_response& v);
    static ores::marketdata::messaging::list_delivery_count_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_delivery_count_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_delivery_count_version_request& v);
    static ores::marketdata::messaging::get_delivery_count_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_delivery_count_version_request& v);
    static ores::marketdata::messaging::get_delivery_count_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_delivery_count_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_delivery_count_version_response& v);
    static ores::marketdata::messaging::get_delivery_count_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_delivery_count_version_response& v);
    static ores::marketdata::messaging::get_delivery_count_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
