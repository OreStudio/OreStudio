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
#ifndef ORES_IAM_API_MESSAGING_ROLE_PERMISSION_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_ROLE_PERMISSION_PROTOCOL_HPP

#include "ores.iam.api/domain/role_permission.hpp"
#include "ores.iam.api/export.hpp"
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

namespace ores::iam::messaging {

struct role_permission_key {
    boost::uuids::uuid role_id;
    boost::uuids::uuid permission_id;

    friend bool operator==(const role_permission_key&, const role_permission_key&) = default;
};

struct role_permission_write {
    boost::uuids::uuid role_id;
    boost::uuids::uuid permission_id;

    friend bool operator==(const role_permission_write&, const role_permission_write&) = default;
};

struct role_permission_change {
    role_permission_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const role_permission_change&, const role_permission_change&) = default;
};

struct role_permission_removal {
    role_permission_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const role_permission_removal&,
                           const role_permission_removal&) = default;
};

struct role_permission_lookup {
    role_permission_key key;
    std::optional<ores::iam::domain::role_permission> role_permission;

    friend bool operator==(const role_permission_lookup&, const role_permission_lookup&) = default;
};

struct role_permissions_filter {
    std::optional<boost::uuids::uuid> role_id;
    std::optional<boost::uuids::uuid> permission_id;
    std::optional<std::vector<boost::uuids::uuid>> role_id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> permission_id_one_of;

    friend bool operator==(const role_permissions_filter&,
                           const role_permissions_filter&) = default;
};

struct role_permission_event {
    boost::uuids::uuid event_id;
    role_permission_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const role_permission_event&, const role_permission_event&) = default;
};

struct list_role_permissions_request {
    using response_type = struct list_role_permissions_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_permissions.list";
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
    std::optional<role_permissions_filter> filter;

    friend bool operator==(const list_role_permissions_request&,
                           const list_role_permissions_request&) = default;
};

struct list_role_permissions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role_permission> role_permissions;
    std::uint64_t total;

    friend bool operator==(const list_role_permissions_response&,
                           const list_role_permissions_response&) = default;
};

struct get_role_permission_request {
    using response_type = struct get_role_permission_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_permissions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_permission_key key;

    friend bool operator==(const get_role_permission_request&,
                           const get_role_permission_request&) = default;
};

struct get_role_permission_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::role_permission> role_permission;

    friend bool operator==(const get_role_permission_response&,
                           const get_role_permission_response&) = default;
};

struct get_many_role_permissions_request {
    using response_type = struct get_many_role_permissions_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_permissions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<role_permission_key> keys;

    friend bool operator==(const get_many_role_permissions_request&,
                           const get_many_role_permissions_request&) = default;
};

struct get_many_role_permissions_response {
    ores::utility::domain::result result;
    std::vector<role_permission_lookup> entries;

    friend bool operator==(const get_many_role_permissions_response&,
                           const get_many_role_permissions_response&) = default;
};

struct put_role_permission_request {
    using response_type = struct put_role_permission_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_permissions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_permission_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_role_permission_request&,
                           const put_role_permission_request&) = default;
};

struct put_role_permission_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::role_permission> role_permission;

    friend bool operator==(const put_role_permission_response&,
                           const put_role_permission_response&) = default;
};

struct put_many_role_permissions_request {
    using response_type = struct put_many_role_permissions_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_permissions.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<role_permission_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_role_permissions_request&,
                           const put_many_role_permissions_request&) = default;
};

struct put_many_role_permissions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role_permission> role_permissions;

    friend bool operator==(const put_many_role_permissions_response&,
                           const put_many_role_permissions_response&) = default;
};

struct delete_role_permission_request {
    using response_type = struct delete_role_permission_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_permissions.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_permission_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_role_permission_request&,
                           const delete_role_permission_request&) = default;
};

struct delete_role_permission_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_role_permission_response&,
                           const delete_role_permission_response&) = default;
};

struct delete_many_role_permissions_request {
    using response_type = struct delete_many_role_permissions_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_permissions.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<role_permission_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_role_permissions_request&,
                           const delete_many_role_permissions_request&) = default;
};

struct delete_many_role_permissions_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_role_permissions_response&,
                           const delete_many_role_permissions_response&) = default;
};

struct list_by_role_id_role_permissions_request {
    using response_type = struct list_by_role_id_role_permissions_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_permissions.list_by_role_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid role_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<role_permissions_filter> filter;

    friend bool operator==(const list_by_role_id_role_permissions_request&,
                           const list_by_role_id_role_permissions_request&) = default;
};

struct list_by_role_id_role_permissions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role_permission> role_permissions;
    std::uint64_t total;

    friend bool operator==(const list_by_role_id_role_permissions_response&,
                           const list_by_role_id_role_permissions_response&) = default;
};

struct list_by_permission_id_role_permissions_request {
    using response_type = struct list_by_permission_id_role_permissions_response;
    static constexpr std::string_view nats_subject =
        "iam.v1.role_permissions.list_by_permission_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid permission_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<role_permissions_filter> filter;

    friend bool operator==(const list_by_permission_id_role_permissions_request&,
                           const list_by_permission_id_role_permissions_request&) = default;
};

struct list_by_permission_id_role_permissions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role_permission> role_permissions;
    std::uint64_t total;

    friend bool operator==(const list_by_permission_id_role_permissions_response&,
                           const list_by_permission_id_role_permissions_response&) = default;
};

/**
 * @brief The members of role_permission_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_permission_key>) {
    return std::tuple{
        utility::codec::field{"role_id", &role_permission_key::role_id},
        utility::codec::field{"permission_id", &role_permission_key::permission_id},
    };
}

/**
 * @brief The members of role_permission_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_permission_write>) {
    return std::tuple{
        utility::codec::field{"role_id", &role_permission_write::role_id},
        utility::codec::field{"permission_id", &role_permission_write::permission_id},
    };
}

/**
 * @brief The members of role_permission_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_permission_change>) {
    return std::tuple{
        utility::codec::field{"write", &role_permission_change::write},
        utility::codec::field{"precondition", &role_permission_change::precondition},
    };
}

/**
 * @brief The members of role_permission_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_permission_removal>) {
    return std::tuple{
        utility::codec::field{"key", &role_permission_removal::key},
        utility::codec::field{"precondition", &role_permission_removal::precondition},
    };
}

/**
 * @brief The members of role_permission_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_permission_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &role_permission_lookup::key},
        utility::codec::field{"role_permission", &role_permission_lookup::role_permission},
    };
}

/**
 * @brief The members of role_permissions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_permissions_filter>) {
    return std::tuple{
        utility::codec::field{"role_id", &role_permissions_filter::role_id},
        utility::codec::field{"permission_id", &role_permissions_filter::permission_id},
        utility::codec::field{"role_id_one_of", &role_permissions_filter::role_id_one_of},
        utility::codec::field{"permission_id_one_of",
                              &role_permissions_filter::permission_id_one_of},
    };
}

/**
 * @brief The members of role_permission_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_permission_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &role_permission_event::event_id},
        utility::codec::field{"key", &role_permission_event::key},
        utility::codec::field{"action", &role_permission_event::action},
        utility::codec::field{"version", &role_permission_event::version},
        utility::codec::field{"occurred_at", &role_permission_event::occurred_at},
        utility::codec::field{"correlation_id", &role_permission_event::correlation_id},
    };
}

/**
 * @brief The members of list_role_permissions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_role_permissions_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_role_permissions_request::offset},
        utility::codec::field{"limit", &list_role_permissions_request::limit},
        utility::codec::field{"order", &list_role_permissions_request::order},
        utility::codec::field{"filter", &list_role_permissions_request::filter},
    };
}

/**
 * @brief The members of list_role_permissions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_role_permissions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_role_permissions_response::result},
        utility::codec::field{"role_permissions",
                              &list_role_permissions_response::role_permissions},
        utility::codec::field{"total", &list_role_permissions_response::total},
    };
}

/**
 * @brief The members of get_role_permission_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_role_permission_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_role_permission_request::key},
    };
}

/**
 * @brief The members of get_role_permission_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_role_permission_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_role_permission_response::result},
        utility::codec::field{"role_permission", &get_role_permission_response::role_permission},
    };
}

/**
 * @brief The members of get_many_role_permissions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_role_permissions_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_role_permissions_request::keys},
    };
}

/**
 * @brief The members of get_many_role_permissions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_role_permissions_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_role_permissions_response::result},
        utility::codec::field{"entries", &get_many_role_permissions_response::entries},
    };
}

/**
 * @brief The members of put_role_permission_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_role_permission_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_role_permission_request::change},
        utility::codec::field{"intent", &put_role_permission_request::intent},
    };
}

/**
 * @brief The members of put_role_permission_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_role_permission_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_role_permission_response::result},
        utility::codec::field{"role_permission", &put_role_permission_response::role_permission},
    };
}

/**
 * @brief The members of put_many_role_permissions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_role_permissions_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_role_permissions_request::changes},
        utility::codec::field{"intent", &put_many_role_permissions_request::intent},
    };
}

/**
 * @brief The members of put_many_role_permissions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_role_permissions_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_role_permissions_response::result},
        utility::codec::field{"role_permissions",
                              &put_many_role_permissions_response::role_permissions},
    };
}

/**
 * @brief The members of delete_role_permission_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_role_permission_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_role_permission_request::removal},
        utility::codec::field{"intent", &delete_role_permission_request::intent},
    };
}

/**
 * @brief The members of delete_role_permission_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_role_permission_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_role_permission_response::result},
    };
}

/**
 * @brief The members of delete_many_role_permissions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_role_permissions_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_role_permissions_request::removals},
        utility::codec::field{"intent", &delete_many_role_permissions_request::intent},
    };
}

/**
 * @brief The members of delete_many_role_permissions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_role_permissions_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_role_permissions_response::result},
    };
}

/**
 * @brief The members of list_by_role_id_role_permissions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_role_id_role_permissions_request>) {
    return std::tuple{
        utility::codec::field{"role_id", &list_by_role_id_role_permissions_request::role_id},
        utility::codec::field{"scope", &list_by_role_id_role_permissions_request::scope},
        utility::codec::field{"offset", &list_by_role_id_role_permissions_request::offset},
        utility::codec::field{"limit", &list_by_role_id_role_permissions_request::limit},
        utility::codec::field{"order", &list_by_role_id_role_permissions_request::order},
        utility::codec::field{"filter", &list_by_role_id_role_permissions_request::filter},
    };
}

/**
 * @brief The members of list_by_role_id_role_permissions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_role_id_role_permissions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_by_role_id_role_permissions_response::result},
        utility::codec::field{"role_permissions",
                              &list_by_role_id_role_permissions_response::role_permissions},
        utility::codec::field{"total", &list_by_role_id_role_permissions_response::total},
    };
}

/**
 * @brief The members of list_by_permission_id_role_permissions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_permission_id_role_permissions_request>) {
    return std::tuple{
        utility::codec::field{"permission_id",
                              &list_by_permission_id_role_permissions_request::permission_id},
        utility::codec::field{"scope", &list_by_permission_id_role_permissions_request::scope},
        utility::codec::field{"offset", &list_by_permission_id_role_permissions_request::offset},
        utility::codec::field{"limit", &list_by_permission_id_role_permissions_request::limit},
        utility::codec::field{"order", &list_by_permission_id_role_permissions_request::order},
        utility::codec::field{"filter", &list_by_permission_id_role_permissions_request::filter},
    };
}

/**
 * @brief The members of list_by_permission_id_role_permissions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_permission_id_role_permissions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_by_permission_id_role_permissions_response::result},
        utility::codec::field{"role_permissions",
                              &list_by_permission_id_role_permissions_response::role_permissions},
        utility::codec::field{"total", &list_by_permission_id_role_permissions_response::total},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_permission_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_permission_key& v);
    static ores::iam::messaging::role_permission_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_permission_key& v);
    static ores::iam::messaging::role_permission_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_permission_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_permission_write& v);
    static ores::iam::messaging::role_permission_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_permission_write& v);
    static ores::iam::messaging::role_permission_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_permission_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_permission_change& v);
    static ores::iam::messaging::role_permission_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_permission_change& v);
    static ores::iam::messaging::role_permission_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_permission_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_permission_removal& v);
    static ores::iam::messaging::role_permission_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_permission_removal& v);
    static ores::iam::messaging::role_permission_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_permission_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_permission_lookup& v);
    static ores::iam::messaging::role_permission_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_permission_lookup& v);
    static ores::iam::messaging::role_permission_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_permissions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_permissions_filter& v);
    static ores::iam::messaging::role_permissions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_permissions_filter& v);
    static ores::iam::messaging::role_permissions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_permission_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_permission_event& v);
    static ores::iam::messaging::role_permission_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_permission_event& v);
    static ores::iam::messaging::role_permission_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_role_permissions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_role_permissions_request& v);
    static ores::iam::messaging::list_role_permissions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_role_permissions_request& v);
    static ores::iam::messaging::list_role_permissions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_role_permissions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_role_permissions_response& v);
    static ores::iam::messaging::list_role_permissions_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_role_permissions_response& v);
    static ores::iam::messaging::list_role_permissions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_role_permission_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_role_permission_request& v);
    static ores::iam::messaging::get_role_permission_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_role_permission_request& v);
    static ores::iam::messaging::get_role_permission_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_role_permission_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_role_permission_response& v);
    static ores::iam::messaging::get_role_permission_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_role_permission_response& v);
    static ores::iam::messaging::get_role_permission_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_role_permissions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_many_role_permissions_request& v);
    static ores::iam::messaging::get_many_role_permissions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_many_role_permissions_request& v);
    static ores::iam::messaging::get_many_role_permissions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_role_permissions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_many_role_permissions_response& v);
    static ores::iam::messaging::get_many_role_permissions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_many_role_permissions_response& v);
    static ores::iam::messaging::get_many_role_permissions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_role_permission_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_role_permission_request& v);
    static ores::iam::messaging::put_role_permission_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_role_permission_request& v);
    static ores::iam::messaging::put_role_permission_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_role_permission_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_role_permission_response& v);
    static ores::iam::messaging::put_role_permission_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_role_permission_response& v);
    static ores::iam::messaging::put_role_permission_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_many_role_permissions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_many_role_permissions_request& v);
    static ores::iam::messaging::put_many_role_permissions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::put_many_role_permissions_request& v);
    static ores::iam::messaging::put_many_role_permissions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_many_role_permissions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::put_many_role_permissions_response& v);
    static ores::iam::messaging::put_many_role_permissions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::put_many_role_permissions_response& v);
    static ores::iam::messaging::put_many_role_permissions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_role_permission_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_role_permission_request& v);
    static ores::iam::messaging::delete_role_permission_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::delete_role_permission_request& v);
    static ores::iam::messaging::delete_role_permission_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_role_permission_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_role_permission_response& v);
    static ores::iam::messaging::delete_role_permission_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_role_permission_response& v);
    static ores::iam::messaging::delete_role_permission_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_many_role_permissions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::delete_many_role_permissions_request& v);
    static ores::iam::messaging::delete_many_role_permissions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_many_role_permissions_request& v);
    static ores::iam::messaging::delete_many_role_permissions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::delete_many_role_permissions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::delete_many_role_permissions_response& v);
    static ores::iam::messaging::delete_many_role_permissions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_many_role_permissions_response& v);
    static ores::iam::messaging::delete_many_role_permissions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_role_id_role_permissions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_by_role_id_role_permissions_request& v);
    static ores::iam::messaging::list_by_role_id_role_permissions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_by_role_id_role_permissions_request& v);
    static ores::iam::messaging::list_by_role_id_role_permissions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_role_id_role_permissions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_by_role_id_role_permissions_response& v);
    static ores::iam::messaging::list_by_role_id_role_permissions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_by_role_id_role_permissions_response& v);
    static ores::iam::messaging::list_by_role_id_role_permissions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_permission_id_role_permissions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_by_permission_id_role_permissions_request& v);
    static ores::iam::messaging::list_by_permission_id_role_permissions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_by_permission_id_role_permissions_request& v);
    static ores::iam::messaging::list_by_permission_id_role_permissions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_permission_id_role_permissions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_by_permission_id_role_permissions_response& v);
    static ores::iam::messaging::list_by_permission_id_role_permissions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_by_permission_id_role_permissions_response& v);
    static ores::iam::messaging::list_by_permission_id_role_permissions_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
