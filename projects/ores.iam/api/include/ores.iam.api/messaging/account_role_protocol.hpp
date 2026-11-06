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
#ifndef ORES_IAM_API_MESSAGING_ACCOUNT_ROLE_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_ACCOUNT_ROLE_PROTOCOL_HPP

#include "ores.iam.api/domain/account_role.hpp"
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

struct account_role_key {
    boost::uuids::uuid account_id;
    boost::uuids::uuid role_id;

    friend bool operator==(const account_role_key&, const account_role_key&) = default;
};

struct account_role_write {
    boost::uuids::uuid account_id;
    boost::uuids::uuid role_id;

    friend bool operator==(const account_role_write&, const account_role_write&) = default;
};

struct account_role_change {
    account_role_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const account_role_change&, const account_role_change&) = default;
};

struct account_role_removal {
    account_role_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const account_role_removal&, const account_role_removal&) = default;
};

struct account_role_lookup {
    account_role_key key;
    std::optional<ores::iam::domain::account_role> account_role;

    friend bool operator==(const account_role_lookup&, const account_role_lookup&) = default;
};

struct account_roles_filter {
    std::optional<boost::uuids::uuid> account_id;
    std::optional<boost::uuids::uuid> role_id;
    std::optional<std::vector<boost::uuids::uuid>> account_id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> role_id_one_of;

    friend bool operator==(const account_roles_filter&, const account_roles_filter&) = default;
};

struct account_role_event {
    boost::uuids::uuid event_id;
    account_role_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const account_role_event&, const account_role_event&) = default;
};

struct list_account_roles_request {
    using response_type = struct list_account_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_roles.list";
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
    std::optional<account_roles_filter> filter;

    friend bool operator==(const list_account_roles_request&,
                           const list_account_roles_request&) = default;
};

struct list_account_roles_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_role> account_roles;
    std::uint64_t total;

    friend bool operator==(const list_account_roles_response&,
                           const list_account_roles_response&) = default;
};

struct get_account_role_request {
    using response_type = struct get_account_role_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_roles.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_role_key key;

    friend bool operator==(const get_account_role_request&,
                           const get_account_role_request&) = default;
};

struct get_account_role_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::account_role> account_role;

    friend bool operator==(const get_account_role_response&,
                           const get_account_role_response&) = default;
};

struct get_many_account_roles_request {
    using response_type = struct get_many_account_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_roles.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<account_role_key> keys;

    friend bool operator==(const get_many_account_roles_request&,
                           const get_many_account_roles_request&) = default;
};

struct get_many_account_roles_response {
    ores::utility::domain::result result;
    std::vector<account_role_lookup> entries;

    friend bool operator==(const get_many_account_roles_response&,
                           const get_many_account_roles_response&) = default;
};

struct put_account_role_request {
    using response_type = struct put_account_role_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_roles.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_role_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_account_role_request&,
                           const put_account_role_request&) = default;
};

struct put_account_role_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::account_role> account_role;

    friend bool operator==(const put_account_role_response&,
                           const put_account_role_response&) = default;
};

struct put_many_account_roles_request {
    using response_type = struct put_many_account_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_roles.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<account_role_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_account_roles_request&,
                           const put_many_account_roles_request&) = default;
};

struct put_many_account_roles_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_role> account_roles;

    friend bool operator==(const put_many_account_roles_response&,
                           const put_many_account_roles_response&) = default;
};

struct delete_account_role_request {
    using response_type = struct delete_account_role_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_roles.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_role_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_account_role_request&,
                           const delete_account_role_request&) = default;
};

struct delete_account_role_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_account_role_response&,
                           const delete_account_role_response&) = default;
};

struct delete_many_account_roles_request {
    using response_type = struct delete_many_account_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_roles.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<account_role_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_account_roles_request&,
                           const delete_many_account_roles_request&) = default;
};

struct delete_many_account_roles_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_account_roles_response&,
                           const delete_many_account_roles_response&) = default;
};

struct list_by_account_id_account_roles_request {
    using response_type = struct list_by_account_id_account_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_roles.list_by_account_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid account_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<account_roles_filter> filter;

    friend bool operator==(const list_by_account_id_account_roles_request&,
                           const list_by_account_id_account_roles_request&) = default;
};

struct list_by_account_id_account_roles_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_role> account_roles;
    std::uint64_t total;

    friend bool operator==(const list_by_account_id_account_roles_response&,
                           const list_by_account_id_account_roles_response&) = default;
};

struct list_by_role_id_account_roles_request {
    using response_type = struct list_by_role_id_account_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_roles.list_by_role_id";
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
    std::optional<account_roles_filter> filter;

    friend bool operator==(const list_by_role_id_account_roles_request&,
                           const list_by_role_id_account_roles_request&) = default;
};

struct list_by_role_id_account_roles_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_role> account_roles;
    std::uint64_t total;

    friend bool operator==(const list_by_role_id_account_roles_response&,
                           const list_by_role_id_account_roles_response&) = default;
};

/**
 * @brief The members of account_role_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<account_role_key>) {
    return std::tuple{
        utility::codec::field{"account_id", &account_role_key::account_id},
        utility::codec::field{"role_id", &account_role_key::role_id},
    };
}

/**
 * @brief The members of account_role_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<account_role_write>) {
    return std::tuple{
        utility::codec::field{"account_id", &account_role_write::account_id},
        utility::codec::field{"role_id", &account_role_write::role_id},
    };
}

/**
 * @brief The members of account_role_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<account_role_change>) {
    return std::tuple{
        utility::codec::field{"write", &account_role_change::write},
        utility::codec::field{"precondition", &account_role_change::precondition},
    };
}

/**
 * @brief The members of account_role_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<account_role_removal>) {
    return std::tuple{
        utility::codec::field{"key", &account_role_removal::key},
        utility::codec::field{"precondition", &account_role_removal::precondition},
    };
}

/**
 * @brief The members of account_role_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<account_role_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &account_role_lookup::key},
        utility::codec::field{"account_role", &account_role_lookup::account_role},
    };
}

/**
 * @brief The members of account_roles_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<account_roles_filter>) {
    return std::tuple{
        utility::codec::field{"account_id", &account_roles_filter::account_id},
        utility::codec::field{"role_id", &account_roles_filter::role_id},
        utility::codec::field{"account_id_one_of", &account_roles_filter::account_id_one_of},
        utility::codec::field{"role_id_one_of", &account_roles_filter::role_id_one_of},
    };
}

/**
 * @brief The members of account_role_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<account_role_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &account_role_event::event_id},
        utility::codec::field{"key", &account_role_event::key},
        utility::codec::field{"action", &account_role_event::action},
        utility::codec::field{"version", &account_role_event::version},
        utility::codec::field{"occurred_at", &account_role_event::occurred_at},
        utility::codec::field{"correlation_id", &account_role_event::correlation_id},
    };
}

/**
 * @brief The members of list_account_roles_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_account_roles_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_account_roles_request::offset},
        utility::codec::field{"limit", &list_account_roles_request::limit},
        utility::codec::field{"order", &list_account_roles_request::order},
        utility::codec::field{"filter", &list_account_roles_request::filter},
    };
}

/**
 * @brief The members of list_account_roles_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_account_roles_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_account_roles_response::result},
        utility::codec::field{"account_roles", &list_account_roles_response::account_roles},
        utility::codec::field{"total", &list_account_roles_response::total},
    };
}

/**
 * @brief The members of get_account_role_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_account_role_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_account_role_request::key},
    };
}

/**
 * @brief The members of get_account_role_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_account_role_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_account_role_response::result},
        utility::codec::field{"account_role", &get_account_role_response::account_role},
    };
}

/**
 * @brief The members of get_many_account_roles_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_account_roles_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_account_roles_request::keys},
    };
}

/**
 * @brief The members of get_many_account_roles_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_account_roles_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_account_roles_response::result},
        utility::codec::field{"entries", &get_many_account_roles_response::entries},
    };
}

/**
 * @brief The members of put_account_role_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_account_role_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_account_role_request::change},
        utility::codec::field{"intent", &put_account_role_request::intent},
    };
}

/**
 * @brief The members of put_account_role_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_account_role_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_account_role_response::result},
        utility::codec::field{"account_role", &put_account_role_response::account_role},
    };
}

/**
 * @brief The members of put_many_account_roles_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_account_roles_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_account_roles_request::changes},
        utility::codec::field{"intent", &put_many_account_roles_request::intent},
    };
}

/**
 * @brief The members of put_many_account_roles_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_account_roles_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_account_roles_response::result},
        utility::codec::field{"account_roles", &put_many_account_roles_response::account_roles},
    };
}

/**
 * @brief The members of delete_account_role_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_account_role_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_account_role_request::removal},
        utility::codec::field{"intent", &delete_account_role_request::intent},
    };
}

/**
 * @brief The members of delete_account_role_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_account_role_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_account_role_response::result},
    };
}

/**
 * @brief The members of delete_many_account_roles_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_account_roles_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_account_roles_request::removals},
        utility::codec::field{"intent", &delete_many_account_roles_request::intent},
    };
}

/**
 * @brief The members of delete_many_account_roles_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_account_roles_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_account_roles_response::result},
    };
}

/**
 * @brief The members of list_by_account_id_account_roles_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_account_id_account_roles_request>) {
    return std::tuple{
        utility::codec::field{"account_id", &list_by_account_id_account_roles_request::account_id},
        utility::codec::field{"scope", &list_by_account_id_account_roles_request::scope},
        utility::codec::field{"offset", &list_by_account_id_account_roles_request::offset},
        utility::codec::field{"limit", &list_by_account_id_account_roles_request::limit},
        utility::codec::field{"order", &list_by_account_id_account_roles_request::order},
        utility::codec::field{"filter", &list_by_account_id_account_roles_request::filter},
    };
}

/**
 * @brief The members of list_by_account_id_account_roles_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_account_id_account_roles_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_by_account_id_account_roles_response::result},
        utility::codec::field{"account_roles",
                              &list_by_account_id_account_roles_response::account_roles},
        utility::codec::field{"total", &list_by_account_id_account_roles_response::total},
    };
}

/**
 * @brief The members of list_by_role_id_account_roles_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_role_id_account_roles_request>) {
    return std::tuple{
        utility::codec::field{"role_id", &list_by_role_id_account_roles_request::role_id},
        utility::codec::field{"scope", &list_by_role_id_account_roles_request::scope},
        utility::codec::field{"offset", &list_by_role_id_account_roles_request::offset},
        utility::codec::field{"limit", &list_by_role_id_account_roles_request::limit},
        utility::codec::field{"order", &list_by_role_id_account_roles_request::order},
        utility::codec::field{"filter", &list_by_role_id_account_roles_request::filter},
    };
}

/**
 * @brief The members of list_by_role_id_account_roles_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_role_id_account_roles_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_by_role_id_account_roles_response::result},
        utility::codec::field{"account_roles",
                              &list_by_role_id_account_roles_response::account_roles},
        utility::codec::field{"total", &list_by_role_id_account_roles_response::total},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_role_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::account_role_key& v);
    static ores::iam::messaging::account_role_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::account_role_key& v);
    static ores::iam::messaging::account_role_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_role_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::account_role_write& v);
    static ores::iam::messaging::account_role_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::account_role_write& v);
    static ores::iam::messaging::account_role_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_role_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::account_role_change& v);
    static ores::iam::messaging::account_role_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::account_role_change& v);
    static ores::iam::messaging::account_role_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_role_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::account_role_removal& v);
    static ores::iam::messaging::account_role_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::account_role_removal& v);
    static ores::iam::messaging::account_role_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_role_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::account_role_lookup& v);
    static ores::iam::messaging::account_role_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::account_role_lookup& v);
    static ores::iam::messaging::account_role_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_roles_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::account_roles_filter& v);
    static ores::iam::messaging::account_roles_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::account_roles_filter& v);
    static ores::iam::messaging::account_roles_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_role_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::account_role_event& v);
    static ores::iam::messaging::account_role_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::account_role_event& v);
    static ores::iam::messaging::account_role_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_account_roles_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_account_roles_request& v);
    static ores::iam::messaging::list_account_roles_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_account_roles_request& v);
    static ores::iam::messaging::list_account_roles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_account_roles_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_account_roles_response& v);
    static ores::iam::messaging::list_account_roles_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_account_roles_response& v);
    static ores::iam::messaging::list_account_roles_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_account_role_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_account_role_request& v);
    static ores::iam::messaging::get_account_role_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_account_role_request& v);
    static ores::iam::messaging::get_account_role_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_account_role_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_account_role_response& v);
    static ores::iam::messaging::get_account_role_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_account_role_response& v);
    static ores::iam::messaging::get_account_role_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_account_roles_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_many_account_roles_request& v);
    static ores::iam::messaging::get_many_account_roles_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_many_account_roles_request& v);
    static ores::iam::messaging::get_many_account_roles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_account_roles_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_many_account_roles_response& v);
    static ores::iam::messaging::get_many_account_roles_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_many_account_roles_response& v);
    static ores::iam::messaging::get_many_account_roles_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_account_role_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_account_role_request& v);
    static ores::iam::messaging::put_account_role_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_account_role_request& v);
    static ores::iam::messaging::put_account_role_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_account_role_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_account_role_response& v);
    static ores::iam::messaging::put_account_role_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_account_role_response& v);
    static ores::iam::messaging::put_account_role_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_many_account_roles_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_many_account_roles_request& v);
    static ores::iam::messaging::put_many_account_roles_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_many_account_roles_request& v);
    static ores::iam::messaging::put_many_account_roles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_many_account_roles_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_many_account_roles_response& v);
    static ores::iam::messaging::put_many_account_roles_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::put_many_account_roles_response& v);
    static ores::iam::messaging::put_many_account_roles_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_account_role_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_account_role_request& v);
    static ores::iam::messaging::delete_account_role_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::delete_account_role_request& v);
    static ores::iam::messaging::delete_account_role_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_account_role_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_account_role_response& v);
    static ores::iam::messaging::delete_account_role_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::delete_account_role_response& v);
    static ores::iam::messaging::delete_account_role_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_many_account_roles_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_many_account_roles_request& v);
    static ores::iam::messaging::delete_many_account_roles_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_many_account_roles_request& v);
    static ores::iam::messaging::delete_many_account_roles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_many_account_roles_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::delete_many_account_roles_response& v);
    static ores::iam::messaging::delete_many_account_roles_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_many_account_roles_response& v);
    static ores::iam::messaging::delete_many_account_roles_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_account_id_account_roles_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_by_account_id_account_roles_request& v);
    static ores::iam::messaging::list_by_account_id_account_roles_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_by_account_id_account_roles_request& v);
    static ores::iam::messaging::list_by_account_id_account_roles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_account_id_account_roles_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_by_account_id_account_roles_response& v);
    static ores::iam::messaging::list_by_account_id_account_roles_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_by_account_id_account_roles_response& v);
    static ores::iam::messaging::list_by_account_id_account_roles_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_role_id_account_roles_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_by_role_id_account_roles_request& v);
    static ores::iam::messaging::list_by_role_id_account_roles_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_by_role_id_account_roles_request& v);
    static ores::iam::messaging::list_by_role_id_account_roles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_role_id_account_roles_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_by_role_id_account_roles_response& v);
    static ores::iam::messaging::list_by_role_id_account_roles_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_by_role_id_account_roles_response& v);
    static ores::iam::messaging::list_by_role_id_account_roles_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
