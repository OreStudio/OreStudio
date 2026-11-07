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
#ifndef ORES_MARKETDATA_API_MESSAGING_RATE_CARD_COMPONENT_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_RATE_CARD_COMPONENT_PROTOCOL_HPP

#include "ores.marketdata.api/domain/rate_card_component_codec.hpp"
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

struct rate_card_component_key {
    std::string driver;

    friend bool operator==(const rate_card_component_key&,
                           const rate_card_component_key&) = default;
};

struct rate_card_component_write {
    boost::uuids::uuid id;
    boost::uuids::uuid feed_id;
    std::string driver;
    std::string licence_right;
    std::chrono::system_clock::time_point effective_from;
    std::string currency;
    std::int64_t charge_micros;
    std::optional<std::chrono::system_clock::time_point> effective_to;

    friend bool operator==(const rate_card_component_write&,
                           const rate_card_component_write&) = default;
};

struct rate_card_component_change {
    rate_card_component_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const rate_card_component_change&,
                           const rate_card_component_change&) = default;
};

struct rate_card_component_removal {
    rate_card_component_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const rate_card_component_removal&,
                           const rate_card_component_removal&) = default;
};

struct rate_card_component_lookup {
    rate_card_component_key key;
    std::optional<ores::marketdata::domain::rate_card_component> rate_card_component;

    friend bool operator==(const rate_card_component_lookup&,
                           const rate_card_component_lookup&) = default;
};

struct rate_card_components_filter {
    std::optional<boost::uuids::uuid> feed_id;
    std::optional<std::string> driver;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> feed_id_one_of;
    std::optional<std::vector<std::string>> driver_one_of;

    friend bool operator==(const rate_card_components_filter&,
                           const rate_card_components_filter&) = default;
};

struct rate_card_component_event {
    boost::uuids::uuid event_id;
    rate_card_component_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const rate_card_component_event&,
                           const rate_card_component_event&) = default;
};

struct rate_card_component_version_key {
    rate_card_component_key rate_card_component;
    std::uint32_t version;

    friend bool operator==(const rate_card_component_version_key&,
                           const rate_card_component_version_key&) = default;
};

struct rate_card_component_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const rate_card_component_versions_filter&,
                           const rate_card_component_versions_filter&) = default;
};

struct list_rate_card_components_request {
    using response_type = struct list_rate_card_components_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.rate_card_components.list";
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
    std::optional<rate_card_components_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_rate_card_components_request&,
                           const list_rate_card_components_request&) = default;
};

struct list_rate_card_components_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::rate_card_component> rate_card_components;
    std::uint64_t total;

    friend bool operator==(const list_rate_card_components_response&,
                           const list_rate_card_components_response&) = default;
};

struct get_rate_card_component_request {
    using response_type = struct get_rate_card_component_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.rate_card_components.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    rate_card_component_key key;

    friend bool operator==(const get_rate_card_component_request&,
                           const get_rate_card_component_request&) = default;
};

struct get_rate_card_component_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::rate_card_component> rate_card_component;

    friend bool operator==(const get_rate_card_component_response&,
                           const get_rate_card_component_response&) = default;
};

struct get_many_rate_card_components_request {
    using response_type = struct get_many_rate_card_components_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.rate_card_components.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<rate_card_component_key> keys;

    friend bool operator==(const get_many_rate_card_components_request&,
                           const get_many_rate_card_components_request&) = default;
};

struct get_many_rate_card_components_response {
    ores::utility::domain::result result;
    std::vector<rate_card_component_lookup> entries;

    friend bool operator==(const get_many_rate_card_components_response&,
                           const get_many_rate_card_components_response&) = default;
};

struct put_rate_card_component_request {
    using response_type = struct put_rate_card_component_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.rate_card_components.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    rate_card_component_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_rate_card_component_request&,
                           const put_rate_card_component_request&) = default;
};

struct put_rate_card_component_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::rate_card_component> rate_card_component;

    friend bool operator==(const put_rate_card_component_response&,
                           const put_rate_card_component_response&) = default;
};

struct put_many_rate_card_components_request {
    using response_type = struct put_many_rate_card_components_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.rate_card_components.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<rate_card_component_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_rate_card_components_request&,
                           const put_many_rate_card_components_request&) = default;
};

struct put_many_rate_card_components_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::rate_card_component> rate_card_components;

    friend bool operator==(const put_many_rate_card_components_response&,
                           const put_many_rate_card_components_response&) = default;
};

struct delete_rate_card_component_request {
    using response_type = struct delete_rate_card_component_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.rate_card_components.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    rate_card_component_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_rate_card_component_request&,
                           const delete_rate_card_component_request&) = default;
};

struct delete_rate_card_component_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_rate_card_component_response&,
                           const delete_rate_card_component_response&) = default;
};

struct delete_many_rate_card_components_request {
    using response_type = struct delete_many_rate_card_components_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.rate_card_components.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<rate_card_component_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_rate_card_components_request&,
                           const delete_many_rate_card_components_request&) = default;
};

struct delete_many_rate_card_components_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_rate_card_components_response&,
                           const delete_many_rate_card_components_response&) = default;
};

struct list_rate_card_component_versions_request {
    using response_type = struct list_rate_card_component_versions_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.rate_card_components_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    rate_card_component_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<rate_card_component_versions_filter> filter;

    friend bool operator==(const list_rate_card_component_versions_request&,
                           const list_rate_card_component_versions_request&) = default;
};

struct list_rate_card_component_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::rate_card_component> versions;
    std::uint64_t total;

    friend bool operator==(const list_rate_card_component_versions_response&,
                           const list_rate_card_component_versions_response&) = default;
};

struct get_rate_card_component_version_request {
    using response_type = struct get_rate_card_component_version_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.rate_card_components_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    rate_card_component_version_key key;

    friend bool operator==(const get_rate_card_component_version_request&,
                           const get_rate_card_component_version_request&) = default;
};

struct get_rate_card_component_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::rate_card_component> version;

    friend bool operator==(const get_rate_card_component_version_response&,
                           const get_rate_card_component_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace rate_card_component_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.events.rate_card_components.created";
inline constexpr std::string_view updated = "marketdata.v1.events.rate_card_components.updated";
inline constexpr std::string_view deleted = "marketdata.v1.events.rate_card_components.deleted";
}
/**
 * @brief The members of rate_card_component_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<rate_card_component_key>) {
    return std::tuple{
        utility::codec::field{"driver", &rate_card_component_key::driver},
    };
}

/**
 * @brief The members of rate_card_component_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<rate_card_component_write>) {
    return std::tuple{
        utility::codec::field{"id", &rate_card_component_write::id},
        utility::codec::field{"feed_id", &rate_card_component_write::feed_id},
        utility::codec::field{"driver", &rate_card_component_write::driver},
        utility::codec::field{"licence_right", &rate_card_component_write::licence_right},
        utility::codec::field{"effective_from", &rate_card_component_write::effective_from},
        utility::codec::field{"currency", &rate_card_component_write::currency},
        utility::codec::field{"charge_micros", &rate_card_component_write::charge_micros},
        utility::codec::field{"effective_to", &rate_card_component_write::effective_to},
    };
}

/**
 * @brief The members of rate_card_component_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<rate_card_component_change>) {
    return std::tuple{
        utility::codec::field{"write", &rate_card_component_change::write},
        utility::codec::field{"precondition", &rate_card_component_change::precondition},
    };
}

/**
 * @brief The members of rate_card_component_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<rate_card_component_removal>) {
    return std::tuple{
        utility::codec::field{"key", &rate_card_component_removal::key},
        utility::codec::field{"precondition", &rate_card_component_removal::precondition},
    };
}

/**
 * @brief The members of rate_card_component_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<rate_card_component_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &rate_card_component_lookup::key},
        utility::codec::field{"rate_card_component",
                              &rate_card_component_lookup::rate_card_component},
    };
}

/**
 * @brief The members of rate_card_components_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<rate_card_components_filter>) {
    return std::tuple{
        utility::codec::field{"feed_id", &rate_card_components_filter::feed_id},
        utility::codec::field{"driver", &rate_card_components_filter::driver},
        utility::codec::field{"id_one_of", &rate_card_components_filter::id_one_of},
        utility::codec::field{"feed_id_one_of", &rate_card_components_filter::feed_id_one_of},
        utility::codec::field{"driver_one_of", &rate_card_components_filter::driver_one_of},
    };
}

/**
 * @brief The members of rate_card_component_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<rate_card_component_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &rate_card_component_event::event_id},
        utility::codec::field{"key", &rate_card_component_event::key},
        utility::codec::field{"action", &rate_card_component_event::action},
        utility::codec::field{"version", &rate_card_component_event::version},
        utility::codec::field{"occurred_at", &rate_card_component_event::occurred_at},
        utility::codec::field{"correlation_id", &rate_card_component_event::correlation_id},
    };
}

/**
 * @brief The members of rate_card_component_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<rate_card_component_version_key>) {
    return std::tuple{
        utility::codec::field{"rate_card_component",
                              &rate_card_component_version_key::rate_card_component},
        utility::codec::field{"version", &rate_card_component_version_key::version},
    };
}

/**
 * @brief The members of rate_card_component_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<rate_card_component_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &rate_card_component_versions_filter::version},
        utility::codec::field{"from_version", &rate_card_component_versions_filter::from_version},
        utility::codec::field{"to_version", &rate_card_component_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_rate_card_components_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_rate_card_components_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_rate_card_components_request::offset},
        utility::codec::field{"limit", &list_rate_card_components_request::limit},
        utility::codec::field{"order", &list_rate_card_components_request::order},
        utility::codec::field{"filter", &list_rate_card_components_request::filter},
        utility::codec::field{"as_of", &list_rate_card_components_request::as_of},
    };
}

/**
 * @brief The members of list_rate_card_components_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_rate_card_components_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_rate_card_components_response::result},
        utility::codec::field{"rate_card_components",
                              &list_rate_card_components_response::rate_card_components},
        utility::codec::field{"total", &list_rate_card_components_response::total},
    };
}

/**
 * @brief The members of get_rate_card_component_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_rate_card_component_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_rate_card_component_request::key},
    };
}

/**
 * @brief The members of get_rate_card_component_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_rate_card_component_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_rate_card_component_response::result},
        utility::codec::field{"rate_card_component",
                              &get_rate_card_component_response::rate_card_component},
    };
}

/**
 * @brief The members of get_many_rate_card_components_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_rate_card_components_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_rate_card_components_request::keys},
    };
}

/**
 * @brief The members of get_many_rate_card_components_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_rate_card_components_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_rate_card_components_response::result},
        utility::codec::field{"entries", &get_many_rate_card_components_response::entries},
    };
}

/**
 * @brief The members of put_rate_card_component_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_rate_card_component_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_rate_card_component_request::change},
        utility::codec::field{"intent", &put_rate_card_component_request::intent},
    };
}

/**
 * @brief The members of put_rate_card_component_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_rate_card_component_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_rate_card_component_response::result},
        utility::codec::field{"rate_card_component",
                              &put_rate_card_component_response::rate_card_component},
    };
}

/**
 * @brief The members of put_many_rate_card_components_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_rate_card_components_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_rate_card_components_request::changes},
        utility::codec::field{"intent", &put_many_rate_card_components_request::intent},
    };
}

/**
 * @brief The members of put_many_rate_card_components_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_rate_card_components_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_rate_card_components_response::result},
        utility::codec::field{"rate_card_components",
                              &put_many_rate_card_components_response::rate_card_components},
    };
}

/**
 * @brief The members of delete_rate_card_component_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_rate_card_component_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_rate_card_component_request::removal},
        utility::codec::field{"intent", &delete_rate_card_component_request::intent},
    };
}

/**
 * @brief The members of delete_rate_card_component_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_rate_card_component_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_rate_card_component_response::result},
    };
}

/**
 * @brief The members of delete_many_rate_card_components_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_rate_card_components_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_rate_card_components_request::removals},
        utility::codec::field{"intent", &delete_many_rate_card_components_request::intent},
    };
}

/**
 * @brief The members of delete_many_rate_card_components_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_rate_card_components_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_rate_card_components_response::result},
    };
}

/**
 * @brief The members of list_rate_card_component_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_rate_card_component_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_rate_card_component_versions_request::key},
        utility::codec::field{"offset", &list_rate_card_component_versions_request::offset},
        utility::codec::field{"limit", &list_rate_card_component_versions_request::limit},
        utility::codec::field{"order", &list_rate_card_component_versions_request::order},
        utility::codec::field{"filter", &list_rate_card_component_versions_request::filter},
    };
}

/**
 * @brief The members of list_rate_card_component_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_rate_card_component_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_rate_card_component_versions_response::result},
        utility::codec::field{"versions", &list_rate_card_component_versions_response::versions},
        utility::codec::field{"total", &list_rate_card_component_versions_response::total},
    };
}

/**
 * @brief The members of get_rate_card_component_version_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_rate_card_component_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_rate_card_component_version_request::key},
    };
}

/**
 * @brief The members of get_rate_card_component_version_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_rate_card_component_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_rate_card_component_version_response::result},
        utility::codec::field{"version", &get_rate_card_component_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::rate_card_component_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::rate_card_component_key& v);
    static ores::marketdata::messaging::rate_card_component_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::rate_card_component_key& v);
    static ores::marketdata::messaging::rate_card_component_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::rate_card_component_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::rate_card_component_write& v);
    static ores::marketdata::messaging::rate_card_component_write read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::rate_card_component_write& v);
    static ores::marketdata::messaging::rate_card_component_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::rate_card_component_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::rate_card_component_change& v);
    static ores::marketdata::messaging::rate_card_component_change read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::rate_card_component_change& v);
    static ores::marketdata::messaging::rate_card_component_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::rate_card_component_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::rate_card_component_removal& v);
    static ores::marketdata::messaging::rate_card_component_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::rate_card_component_removal& v);
    static ores::marketdata::messaging::rate_card_component_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::rate_card_component_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::rate_card_component_lookup& v);
    static ores::marketdata::messaging::rate_card_component_lookup read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::rate_card_component_lookup& v);
    static ores::marketdata::messaging::rate_card_component_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::rate_card_components_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::rate_card_components_filter& v);
    static ores::marketdata::messaging::rate_card_components_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::rate_card_components_filter& v);
    static ores::marketdata::messaging::rate_card_components_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::rate_card_component_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::rate_card_component_event& v);
    static ores::marketdata::messaging::rate_card_component_event read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::rate_card_component_event& v);
    static ores::marketdata::messaging::rate_card_component_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::rate_card_component_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::rate_card_component_version_key& v);
    static ores::marketdata::messaging::rate_card_component_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::rate_card_component_version_key& v);
    static ores::marketdata::messaging::rate_card_component_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::rate_card_component_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::rate_card_component_versions_filter& v);
    static ores::marketdata::messaging::rate_card_component_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::rate_card_component_versions_filter& v);
    static ores::marketdata::messaging::rate_card_component_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_rate_card_components_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_rate_card_components_request& v);
    static ores::marketdata::messaging::list_rate_card_components_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_rate_card_components_request& v);
    static ores::marketdata::messaging::list_rate_card_components_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_rate_card_components_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_rate_card_components_response& v);
    static ores::marketdata::messaging::list_rate_card_components_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_rate_card_components_response& v);
    static ores::marketdata::messaging::list_rate_card_components_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_rate_card_component_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_rate_card_component_request& v);
    static ores::marketdata::messaging::get_rate_card_component_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_rate_card_component_request& v);
    static ores::marketdata::messaging::get_rate_card_component_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_rate_card_component_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_rate_card_component_response& v);
    static ores::marketdata::messaging::get_rate_card_component_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_rate_card_component_response& v);
    static ores::marketdata::messaging::get_rate_card_component_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_rate_card_components_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_rate_card_components_request& v);
    static ores::marketdata::messaging::get_many_rate_card_components_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_rate_card_components_request& v);
    static ores::marketdata::messaging::get_many_rate_card_components_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_rate_card_components_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_rate_card_components_response& v);
    static ores::marketdata::messaging::get_many_rate_card_components_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_rate_card_components_response& v);
    static ores::marketdata::messaging::get_many_rate_card_components_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_rate_card_component_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_rate_card_component_request& v);
    static ores::marketdata::messaging::put_rate_card_component_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_rate_card_component_request& v);
    static ores::marketdata::messaging::put_rate_card_component_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_rate_card_component_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_rate_card_component_response& v);
    static ores::marketdata::messaging::put_rate_card_component_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_rate_card_component_response& v);
    static ores::marketdata::messaging::put_rate_card_component_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_rate_card_components_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_rate_card_components_request& v);
    static ores::marketdata::messaging::put_many_rate_card_components_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_rate_card_components_request& v);
    static ores::marketdata::messaging::put_many_rate_card_components_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_rate_card_components_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_rate_card_components_response& v);
    static ores::marketdata::messaging::put_many_rate_card_components_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_rate_card_components_response& v);
    static ores::marketdata::messaging::put_many_rate_card_components_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_rate_card_component_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_rate_card_component_request& v);
    static ores::marketdata::messaging::delete_rate_card_component_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_rate_card_component_request& v);
    static ores::marketdata::messaging::delete_rate_card_component_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_rate_card_component_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_rate_card_component_response& v);
    static ores::marketdata::messaging::delete_rate_card_component_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_rate_card_component_response& v);
    static ores::marketdata::messaging::delete_rate_card_component_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_rate_card_components_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_rate_card_components_request& v);
    static ores::marketdata::messaging::delete_many_rate_card_components_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_rate_card_components_request& v);
    static ores::marketdata::messaging::delete_many_rate_card_components_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_rate_card_components_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_rate_card_components_response& v);
    static ores::marketdata::messaging::delete_many_rate_card_components_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_rate_card_components_response& v);
    static ores::marketdata::messaging::delete_many_rate_card_components_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_rate_card_component_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_rate_card_component_versions_request& v);
    static ores::marketdata::messaging::list_rate_card_component_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_rate_card_component_versions_request& v);
    static ores::marketdata::messaging::list_rate_card_component_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_rate_card_component_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_rate_card_component_versions_response& v);
    static ores::marketdata::messaging::list_rate_card_component_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_rate_card_component_versions_response& v);
    static ores::marketdata::messaging::list_rate_card_component_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_rate_card_component_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_rate_card_component_version_request& v);
    static ores::marketdata::messaging::get_rate_card_component_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_rate_card_component_version_request& v);
    static ores::marketdata::messaging::get_rate_card_component_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_rate_card_component_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_rate_card_component_version_response& v);
    static ores::marketdata::messaging::get_rate_card_component_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_rate_card_component_version_response& v);
    static ores::marketdata::messaging::get_rate_card_component_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
