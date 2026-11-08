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
#ifndef ORES_IAM_API_MESSAGING_ROLE_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_ROLE_PROTOCOL_HPP

#include "ores.iam.api/domain/role_codec.hpp"
#include "ores.iam.api/export.hpp"
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

namespace ores::iam::messaging {

struct role_key {
    std::string name;

    friend bool operator==(const role_key&, const role_key&) = default;
};

struct role_write {
    boost::uuids::uuid id;
    std::string name;
    std::string description;
    bool is_registration_default{};
    bool is_requestable{};

    friend bool operator==(const role_write&, const role_write&) = default;
};

struct role_change {
    role_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const role_change&, const role_change&) = default;
};

struct role_removal {
    role_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const role_removal&, const role_removal&) = default;
};

struct role_lookup {
    role_key key;
    std::optional<ores::iam::domain::role> role;

    friend bool operator==(const role_lookup&, const role_lookup&) = default;
};

struct roles_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;

    friend bool operator==(const roles_filter&, const roles_filter&) = default;
};

struct role_event {
    boost::uuids::uuid event_id;
    role_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const role_event&, const role_event&) = default;
};

struct role_version_key {
    role_key role;
    std::uint32_t version{};

    friend bool operator==(const role_version_key&, const role_version_key&) = default;
};

struct role_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const role_versions_filter&, const role_versions_filter&) = default;
};

struct list_roles_request {
    using response_type = struct list_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.list";
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
    std::optional<roles_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_roles_request&, const list_roles_request&) = default;
};

struct list_roles_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role> roles;
    std::uint64_t total{};

    friend bool operator==(const list_roles_response&, const list_roles_response&) = default;
};

struct get_role_request {
    using response_type = struct get_role_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_key key;

    friend bool operator==(const get_role_request&, const get_role_request&) = default;
};

struct get_role_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::role> role;

    friend bool operator==(const get_role_response&, const get_role_response&) = default;
};

struct get_many_roles_request {
    using response_type = struct get_many_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<role_key> keys;

    friend bool operator==(const get_many_roles_request&, const get_many_roles_request&) = default;
};

struct get_many_roles_response {
    ores::utility::domain::result result;
    std::vector<role_lookup> entries;

    friend bool operator==(const get_many_roles_response&,
                           const get_many_roles_response&) = default;
};

struct put_role_request {
    using response_type = struct put_role_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_role_request&, const put_role_request&) = default;
};

struct put_role_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::role> role;

    friend bool operator==(const put_role_response&, const put_role_response&) = default;
};

struct put_many_roles_request {
    using response_type = struct put_many_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<role_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_roles_request&, const put_many_roles_request&) = default;
};

struct put_many_roles_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role> roles;

    friend bool operator==(const put_many_roles_response&,
                           const put_many_roles_response&) = default;
};

struct delete_role_request {
    using response_type = struct delete_role_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_role_request&, const delete_role_request&) = default;
};

struct delete_role_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_role_response&, const delete_role_response&) = default;
};

struct delete_many_roles_request {
    using response_type = struct delete_many_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<role_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_roles_request&,
                           const delete_many_roles_request&) = default;
};

struct delete_many_roles_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_roles_response&,
                           const delete_many_roles_response&) = default;
};

struct list_role_versions_request {
    using response_type = struct list_role_versions_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<role_versions_filter> filter;

    friend bool operator==(const list_role_versions_request&,
                           const list_role_versions_request&) = default;
};

struct list_role_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role> versions;
    std::uint64_t total{};

    friend bool operator==(const list_role_versions_response&,
                           const list_role_versions_response&) = default;
};

struct get_role_version_request {
    using response_type = struct get_role_version_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_version_key key;

    friend bool operator==(const get_role_version_request&,
                           const get_role_version_request&) = default;
};

struct get_role_version_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::role> version;

    friend bool operator==(const get_role_version_response&,
                           const get_role_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace role_event_subjects {
inline constexpr std::string_view created = "iam.v1.events.roles.created";
inline constexpr std::string_view updated = "iam.v1.events.roles.updated";
inline constexpr std::string_view deleted = "iam.v1.events.roles.deleted";
}
/**
 * @brief The members of role_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_key>) {
    return std::tuple{
        utility::codec::field{"name", &role_key::name},
    };
}

/**
 * @brief The members of role_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_write>) {
    return std::tuple{
        utility::codec::field{"id", &role_write::id},
        utility::codec::field{"name", &role_write::name},
        utility::codec::field{"description", &role_write::description},
        utility::codec::field{"is_registration_default", &role_write::is_registration_default},
        utility::codec::field{"is_requestable", &role_write::is_requestable},
    };
}

/**
 * @brief The members of role_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_change>) {
    return std::tuple{
        utility::codec::field{"write", &role_change::write},
        utility::codec::field{"precondition", &role_change::precondition},
    };
}

/**
 * @brief The members of role_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_removal>) {
    return std::tuple{
        utility::codec::field{"key", &role_removal::key},
        utility::codec::field{"precondition", &role_removal::precondition},
    };
}

/**
 * @brief The members of role_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &role_lookup::key},
        utility::codec::field{"role", &role_lookup::role},
    };
}

/**
 * @brief The members of roles_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<roles_filter>) {
    return std::tuple{
        utility::codec::field{"id_one_of", &roles_filter::id_one_of},
    };
}

/**
 * @brief The members of role_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &role_event::event_id},
        utility::codec::field{"key", &role_event::key},
        utility::codec::field{"action", &role_event::action},
        utility::codec::field{"version", &role_event::version},
        utility::codec::field{"occurred_at", &role_event::occurred_at},
        utility::codec::field{"correlation_id", &role_event::correlation_id},
    };
}

/**
 * @brief The members of role_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_version_key>) {
    return std::tuple{
        utility::codec::field{"role", &role_version_key::role},
        utility::codec::field{"version", &role_version_key::version},
    };
}

/**
 * @brief The members of role_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &role_versions_filter::version},
        utility::codec::field{"from_version", &role_versions_filter::from_version},
        utility::codec::field{"to_version", &role_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_roles_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_roles_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_roles_request::offset},
        utility::codec::field{"limit", &list_roles_request::limit},
        utility::codec::field{"order", &list_roles_request::order},
        utility::codec::field{"filter", &list_roles_request::filter},
        utility::codec::field{"as_of", &list_roles_request::as_of},
    };
}

/**
 * @brief The members of list_roles_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_roles_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_roles_response::result},
        utility::codec::field{"roles", &list_roles_response::roles},
        utility::codec::field{"total", &list_roles_response::total},
    };
}

/**
 * @brief The members of get_role_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_role_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_role_request::key},
    };
}

/**
 * @brief The members of get_role_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_role_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_role_response::result},
        utility::codec::field{"role", &get_role_response::role},
    };
}

/**
 * @brief The members of get_many_roles_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_roles_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_roles_request::keys},
    };
}

/**
 * @brief The members of get_many_roles_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_roles_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_roles_response::result},
        utility::codec::field{"entries", &get_many_roles_response::entries},
    };
}

/**
 * @brief The members of put_role_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_role_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_role_request::change},
        utility::codec::field{"intent", &put_role_request::intent},
    };
}

/**
 * @brief The members of put_role_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_role_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_role_response::result},
        utility::codec::field{"role", &put_role_response::role},
    };
}

/**
 * @brief The members of put_many_roles_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_roles_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_roles_request::changes},
        utility::codec::field{"intent", &put_many_roles_request::intent},
    };
}

/**
 * @brief The members of put_many_roles_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_roles_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_roles_response::result},
        utility::codec::field{"roles", &put_many_roles_response::roles},
    };
}

/**
 * @brief The members of delete_role_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_role_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_role_request::removal},
        utility::codec::field{"intent", &delete_role_request::intent},
    };
}

/**
 * @brief The members of delete_role_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_role_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_role_response::result},
    };
}

/**
 * @brief The members of delete_many_roles_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_roles_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_roles_request::removals},
        utility::codec::field{"intent", &delete_many_roles_request::intent},
    };
}

/**
 * @brief The members of delete_many_roles_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_roles_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_roles_response::result},
    };
}

/**
 * @brief The members of list_role_versions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_role_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_role_versions_request::key},
        utility::codec::field{"offset", &list_role_versions_request::offset},
        utility::codec::field{"limit", &list_role_versions_request::limit},
        utility::codec::field{"order", &list_role_versions_request::order},
        utility::codec::field{"filter", &list_role_versions_request::filter},
    };
}

/**
 * @brief The members of list_role_versions_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_role_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_role_versions_response::result},
        utility::codec::field{"versions", &list_role_versions_response::versions},
        utility::codec::field{"total", &list_role_versions_response::total},
    };
}

/**
 * @brief The members of get_role_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_role_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_role_version_request::key},
    };
}

/**
 * @brief The members of get_role_version_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_role_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_role_version_response::result},
        utility::codec::field{"version", &get_role_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_key& v);
    static ores::iam::messaging::role_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_key& v);
    static ores::iam::messaging::role_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_write& v);
    static ores::iam::messaging::role_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_write& v);
    static ores::iam::messaging::role_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_change& v);
    static ores::iam::messaging::role_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_change& v);
    static ores::iam::messaging::role_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_removal& v);
    static ores::iam::messaging::role_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_removal& v);
    static ores::iam::messaging::role_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_lookup& v);
    static ores::iam::messaging::role_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_lookup& v);
    static ores::iam::messaging::role_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::roles_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::roles_filter& v);
    static ores::iam::messaging::roles_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::roles_filter& v);
    static ores::iam::messaging::roles_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_event& v);
    static ores::iam::messaging::role_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_event& v);
    static ores::iam::messaging::role_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_version_key& v);
    static ores::iam::messaging::role_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_version_key& v);
    static ores::iam::messaging::role_version_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_versions_filter& v);
    static ores::iam::messaging::role_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_versions_filter& v);
    static ores::iam::messaging::role_versions_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_roles_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_roles_request& v);
    static ores::iam::messaging::list_roles_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_roles_request& v);
    static ores::iam::messaging::list_roles_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_roles_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_roles_response& v);
    static ores::iam::messaging::list_roles_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_roles_response& v);
    static ores::iam::messaging::list_roles_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_role_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_role_request& v);
    static ores::iam::messaging::get_role_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_role_request& v);
    static ores::iam::messaging::get_role_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_role_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_role_response& v);
    static ores::iam::messaging::get_role_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_role_response& v);
    static ores::iam::messaging::get_role_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_roles_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_many_roles_request& v);
    static ores::iam::messaging::get_many_roles_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_many_roles_request& v);
    static ores::iam::messaging::get_many_roles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_roles_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_many_roles_response& v);
    static ores::iam::messaging::get_many_roles_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_many_roles_response& v);
    static ores::iam::messaging::get_many_roles_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_role_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_role_request& v);
    static ores::iam::messaging::put_role_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_role_request& v);
    static ores::iam::messaging::put_role_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_role_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_role_response& v);
    static ores::iam::messaging::put_role_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_role_response& v);
    static ores::iam::messaging::put_role_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_many_roles_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_many_roles_request& v);
    static ores::iam::messaging::put_many_roles_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_many_roles_request& v);
    static ores::iam::messaging::put_many_roles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_many_roles_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_many_roles_response& v);
    static ores::iam::messaging::put_many_roles_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_many_roles_response& v);
    static ores::iam::messaging::put_many_roles_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_role_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_role_request& v);
    static ores::iam::messaging::delete_role_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::delete_role_request& v);
    static ores::iam::messaging::delete_role_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_role_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_role_response& v);
    static ores::iam::messaging::delete_role_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::delete_role_response& v);
    static ores::iam::messaging::delete_role_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_many_roles_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_many_roles_request& v);
    static ores::iam::messaging::delete_many_roles_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::delete_many_roles_request& v);
    static ores::iam::messaging::delete_many_roles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_many_roles_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_many_roles_response& v);
    static ores::iam::messaging::delete_many_roles_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::delete_many_roles_response& v);
    static ores::iam::messaging::delete_many_roles_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_role_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_role_versions_request& v);
    static ores::iam::messaging::list_role_versions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_role_versions_request& v);
    static ores::iam::messaging::list_role_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_role_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_role_versions_response& v);
    static ores::iam::messaging::list_role_versions_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_role_versions_response& v);
    static ores::iam::messaging::list_role_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_role_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_role_version_request& v);
    static ores::iam::messaging::get_role_version_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_role_version_request& v);
    static ores::iam::messaging::get_role_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_role_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_role_version_response& v);
    static ores::iam::messaging::get_role_version_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_role_version_response& v);
    static ores::iam::messaging::get_role_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
