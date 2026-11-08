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
#ifndef ORES_MARKETDATA_API_MESSAGING_COMPOSITE_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_COMPOSITE_PROTOCOL_HPP

#include "ores.marketdata.api/domain/composite_codec.hpp"
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

struct composite_key {
    std::string name;

    friend bool operator==(const composite_key&, const composite_key&) = default;
};

struct composite_write {
    boost::uuids::uuid id;
    std::string name;
    std::string provider_code;
    std::string oresmd_uri;
    std::string rule_code;
    std::string description;

    friend bool operator==(const composite_write&, const composite_write&) = default;
};

struct composite_change {
    composite_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const composite_change&, const composite_change&) = default;
};

struct composite_removal {
    composite_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const composite_removal&, const composite_removal&) = default;
};

struct composite_lookup {
    composite_key key;
    std::optional<ores::marketdata::domain::composite> composite;

    friend bool operator==(const composite_lookup&, const composite_lookup&) = default;
};

struct composites_filter {
    std::optional<std::string> provider_code;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<std::string>> provider_code_one_of;
    std::optional<std::string> search;

    friend bool operator==(const composites_filter&, const composites_filter&) = default;
};

struct composite_event {
    boost::uuids::uuid event_id;
    composite_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const composite_event&, const composite_event&) = default;
};

struct composite_version_key {
    composite_key composite;
    std::uint32_t version{};

    friend bool operator==(const composite_version_key&, const composite_version_key&) = default;
};

struct composite_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const composite_versions_filter&,
                           const composite_versions_filter&) = default;
};

struct list_composites_request {
    using response_type = struct list_composites_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composites.list";
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
    std::optional<composites_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_composites_request&,
                           const list_composites_request&) = default;
};

struct list_composites_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::composite> composites;
    std::uint64_t total{};

    friend bool operator==(const list_composites_response&,
                           const list_composites_response&) = default;
};

struct get_composite_request {
    using response_type = struct get_composite_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composites.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_key key;

    friend bool operator==(const get_composite_request&, const get_composite_request&) = default;
};

struct get_composite_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::composite> composite;

    friend bool operator==(const get_composite_response&, const get_composite_response&) = default;
};

struct get_many_composites_request {
    using response_type = struct get_many_composites_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composites.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<composite_key> keys;

    friend bool operator==(const get_many_composites_request&,
                           const get_many_composites_request&) = default;
};

struct get_many_composites_response {
    ores::utility::domain::result result;
    std::vector<composite_lookup> entries;

    friend bool operator==(const get_many_composites_response&,
                           const get_many_composites_response&) = default;
};

struct put_composite_request {
    using response_type = struct put_composite_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composites.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_composite_request&, const put_composite_request&) = default;
};

struct put_composite_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::composite> composite;

    friend bool operator==(const put_composite_response&, const put_composite_response&) = default;
};

struct put_many_composites_request {
    using response_type = struct put_many_composites_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composites.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<composite_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_composites_request&,
                           const put_many_composites_request&) = default;
};

struct put_many_composites_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::composite> composites;

    friend bool operator==(const put_many_composites_response&,
                           const put_many_composites_response&) = default;
};

struct delete_composite_request {
    using response_type = struct delete_composite_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composites.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_composite_request&,
                           const delete_composite_request&) = default;
};

struct delete_composite_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_composite_response&,
                           const delete_composite_response&) = default;
};

struct delete_many_composites_request {
    using response_type = struct delete_many_composites_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composites.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<composite_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_composites_request&,
                           const delete_many_composites_request&) = default;
};

struct delete_many_composites_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_composites_response&,
                           const delete_many_composites_response&) = default;
};

struct list_composite_versions_request {
    using response_type = struct list_composite_versions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composites_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<composite_versions_filter> filter;

    friend bool operator==(const list_composite_versions_request&,
                           const list_composite_versions_request&) = default;
};

struct list_composite_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::composite> versions;
    std::uint64_t total{};

    friend bool operator==(const list_composite_versions_response&,
                           const list_composite_versions_response&) = default;
};

struct get_composite_version_request {
    using response_type = struct get_composite_version_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composites_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_version_key key;

    friend bool operator==(const get_composite_version_request&,
                           const get_composite_version_request&) = default;
};

struct get_composite_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::composite> version;

    friend bool operator==(const get_composite_version_response&,
                           const get_composite_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace composite_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.events.composites.created";
inline constexpr std::string_view updated = "marketdata.v1.events.composites.updated";
inline constexpr std::string_view deleted = "marketdata.v1.events.composites.deleted";
}
/**
 * @brief The members of composite_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<composite_key>) {
    return std::tuple{
        utility::codec::field{"name", &composite_key::name},
    };
}

/**
 * @brief The members of composite_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<composite_write>) {
    return std::tuple{
        utility::codec::field{"id", &composite_write::id},
        utility::codec::field{"name", &composite_write::name},
        utility::codec::field{"provider_code", &composite_write::provider_code},
        utility::codec::field{"oresmd_uri", &composite_write::oresmd_uri},
        utility::codec::field{"rule_code", &composite_write::rule_code},
        utility::codec::field{"description", &composite_write::description},
    };
}

/**
 * @brief The members of composite_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<composite_change>) {
    return std::tuple{
        utility::codec::field{"write", &composite_change::write},
        utility::codec::field{"precondition", &composite_change::precondition},
    };
}

/**
 * @brief The members of composite_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<composite_removal>) {
    return std::tuple{
        utility::codec::field{"key", &composite_removal::key},
        utility::codec::field{"precondition", &composite_removal::precondition},
    };
}

/**
 * @brief The members of composite_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<composite_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &composite_lookup::key},
        utility::codec::field{"composite", &composite_lookup::composite},
    };
}

/**
 * @brief The members of composites_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<composites_filter>) {
    return std::tuple{
        utility::codec::field{"provider_code", &composites_filter::provider_code},
        utility::codec::field{"id_one_of", &composites_filter::id_one_of},
        utility::codec::field{"provider_code_one_of", &composites_filter::provider_code_one_of},
        utility::codec::field{"search", &composites_filter::search},
    };
}

/**
 * @brief The members of composite_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<composite_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &composite_event::event_id},
        utility::codec::field{"key", &composite_event::key},
        utility::codec::field{"action", &composite_event::action},
        utility::codec::field{"version", &composite_event::version},
        utility::codec::field{"occurred_at", &composite_event::occurred_at},
        utility::codec::field{"correlation_id", &composite_event::correlation_id},
    };
}

/**
 * @brief The members of composite_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<composite_version_key>) {
    return std::tuple{
        utility::codec::field{"composite", &composite_version_key::composite},
        utility::codec::field{"version", &composite_version_key::version},
    };
}

/**
 * @brief The members of composite_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<composite_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &composite_versions_filter::version},
        utility::codec::field{"from_version", &composite_versions_filter::from_version},
        utility::codec::field{"to_version", &composite_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_composites_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_composites_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_composites_request::offset},
        utility::codec::field{"limit", &list_composites_request::limit},
        utility::codec::field{"order", &list_composites_request::order},
        utility::codec::field{"filter", &list_composites_request::filter},
        utility::codec::field{"as_of", &list_composites_request::as_of},
    };
}

/**
 * @brief The members of list_composites_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_composites_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_composites_response::result},
        utility::codec::field{"composites", &list_composites_response::composites},
        utility::codec::field{"total", &list_composites_response::total},
    };
}

/**
 * @brief The members of get_composite_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_composite_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_composite_request::key},
    };
}

/**
 * @brief The members of get_composite_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_composite_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_composite_response::result},
        utility::codec::field{"composite", &get_composite_response::composite},
    };
}

/**
 * @brief The members of get_many_composites_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_composites_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_composites_request::keys},
    };
}

/**
 * @brief The members of get_many_composites_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_composites_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_composites_response::result},
        utility::codec::field{"entries", &get_many_composites_response::entries},
    };
}

/**
 * @brief The members of put_composite_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_composite_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_composite_request::change},
        utility::codec::field{"intent", &put_composite_request::intent},
    };
}

/**
 * @brief The members of put_composite_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_composite_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_composite_response::result},
        utility::codec::field{"composite", &put_composite_response::composite},
    };
}

/**
 * @brief The members of put_many_composites_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_composites_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_composites_request::changes},
        utility::codec::field{"intent", &put_many_composites_request::intent},
    };
}

/**
 * @brief The members of put_many_composites_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_composites_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_composites_response::result},
        utility::codec::field{"composites", &put_many_composites_response::composites},
    };
}

/**
 * @brief The members of delete_composite_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_composite_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_composite_request::removal},
        utility::codec::field{"intent", &delete_composite_request::intent},
    };
}

/**
 * @brief The members of delete_composite_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_composite_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_composite_response::result},
    };
}

/**
 * @brief The members of delete_many_composites_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_composites_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_composites_request::removals},
        utility::codec::field{"intent", &delete_many_composites_request::intent},
    };
}

/**
 * @brief The members of delete_many_composites_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_composites_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_composites_response::result},
    };
}

/**
 * @brief The members of list_composite_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_composite_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_composite_versions_request::key},
        utility::codec::field{"offset", &list_composite_versions_request::offset},
        utility::codec::field{"limit", &list_composite_versions_request::limit},
        utility::codec::field{"order", &list_composite_versions_request::order},
        utility::codec::field{"filter", &list_composite_versions_request::filter},
    };
}

/**
 * @brief The members of list_composite_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_composite_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_composite_versions_response::result},
        utility::codec::field{"versions", &list_composite_versions_response::versions},
        utility::codec::field{"total", &list_composite_versions_response::total},
    };
}

/**
 * @brief The members of get_composite_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_composite_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_composite_version_request::key},
    };
}

/**
 * @brief The members of get_composite_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_composite_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_composite_version_response::result},
        utility::codec::field{"version", &get_composite_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::composite_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::composite_key& v);
    static ores::marketdata::messaging::composite_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::composite_key& v);
    static ores::marketdata::messaging::composite_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::composite_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::composite_write& v);
    static ores::marketdata::messaging::composite_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::composite_write& v);
    static ores::marketdata::messaging::composite_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::composite_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::composite_change& v);
    static ores::marketdata::messaging::composite_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::composite_change& v);
    static ores::marketdata::messaging::composite_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::composite_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::composite_removal& v);
    static ores::marketdata::messaging::composite_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::composite_removal& v);
    static ores::marketdata::messaging::composite_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::composite_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::composite_lookup& v);
    static ores::marketdata::messaging::composite_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::composite_lookup& v);
    static ores::marketdata::messaging::composite_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::composites_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::composites_filter& v);
    static ores::marketdata::messaging::composites_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::composites_filter& v);
    static ores::marketdata::messaging::composites_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::composite_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::composite_event& v);
    static ores::marketdata::messaging::composite_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::composite_event& v);
    static ores::marketdata::messaging::composite_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::composite_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::composite_version_key& v);
    static ores::marketdata::messaging::composite_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::composite_version_key& v);
    static ores::marketdata::messaging::composite_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::composite_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::composite_versions_filter& v);
    static ores::marketdata::messaging::composite_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::composite_versions_filter& v);
    static ores::marketdata::messaging::composite_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_composites_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::list_composites_request& v);
    static ores::marketdata::messaging::list_composites_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::list_composites_request& v);
    static ores::marketdata::messaging::list_composites_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_composites_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::list_composites_response& v);
    static ores::marketdata::messaging::list_composites_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_composites_response& v);
    static ores::marketdata::messaging::list_composites_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::get_composite_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_composite_request& v);
    static ores::marketdata::messaging::get_composite_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::get_composite_request& v);
    static ores::marketdata::messaging::get_composite_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::get_composite_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_composite_response& v);
    static ores::marketdata::messaging::get_composite_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::get_composite_response& v);
    static ores::marketdata::messaging::get_composite_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_composites_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_composites_request& v);
    static ores::marketdata::messaging::get_many_composites_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_composites_request& v);
    static ores::marketdata::messaging::get_many_composites_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_composites_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_composites_response& v);
    static ores::marketdata::messaging::get_many_composites_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_composites_response& v);
    static ores::marketdata::messaging::get_many_composites_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::put_composite_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::put_composite_request& v);
    static ores::marketdata::messaging::put_composite_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::put_composite_request& v);
    static ores::marketdata::messaging::put_composite_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::put_composite_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::put_composite_response& v);
    static ores::marketdata::messaging::put_composite_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::put_composite_response& v);
    static ores::marketdata::messaging::put_composite_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_composites_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_composites_request& v);
    static ores::marketdata::messaging::put_many_composites_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_composites_request& v);
    static ores::marketdata::messaging::put_many_composites_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_composites_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_composites_response& v);
    static ores::marketdata::messaging::put_many_composites_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_composites_response& v);
    static ores::marketdata::messaging::put_many_composites_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_composite_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::delete_composite_request& v);
    static ores::marketdata::messaging::delete_composite_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_composite_request& v);
    static ores::marketdata::messaging::delete_composite_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_composite_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::delete_composite_response& v);
    static ores::marketdata::messaging::delete_composite_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_composite_response& v);
    static ores::marketdata::messaging::delete_composite_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_composites_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_composites_request& v);
    static ores::marketdata::messaging::delete_many_composites_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_composites_request& v);
    static ores::marketdata::messaging::delete_many_composites_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_composites_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_composites_response& v);
    static ores::marketdata::messaging::delete_many_composites_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_composites_response& v);
    static ores::marketdata::messaging::delete_many_composites_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_composite_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_composite_versions_request& v);
    static ores::marketdata::messaging::list_composite_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_composite_versions_request& v);
    static ores::marketdata::messaging::list_composite_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_composite_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_composite_versions_response& v);
    static ores::marketdata::messaging::list_composite_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_composite_versions_response& v);
    static ores::marketdata::messaging::list_composite_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_composite_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_composite_version_request& v);
    static ores::marketdata::messaging::get_composite_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_composite_version_request& v);
    static ores::marketdata::messaging::get_composite_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_composite_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_composite_version_response& v);
    static ores::marketdata::messaging::get_composite_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_composite_version_response& v);
    static ores::marketdata::messaging::get_composite_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
