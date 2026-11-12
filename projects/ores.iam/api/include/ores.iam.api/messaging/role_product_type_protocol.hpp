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
#ifndef ORES_IAM_API_MESSAGING_ROLE_PRODUCT_TYPE_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_ROLE_PRODUCT_TYPE_PROTOCOL_HPP

#include "ores.iam.api/domain/role_product_type.hpp"
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

struct role_product_type_key {
    boost::uuids::uuid role_id;
    std::string product_type_code;

    friend bool operator==(const role_product_type_key&, const role_product_type_key&) = default;
};

struct role_product_type_write {
    boost::uuids::uuid role_id;
    std::string product_type_code;

    friend bool operator==(const role_product_type_write&,
                           const role_product_type_write&) = default;
};

struct role_product_type_change {
    role_product_type_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const role_product_type_change&,
                           const role_product_type_change&) = default;
};

struct role_product_type_removal {
    role_product_type_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const role_product_type_removal&,
                           const role_product_type_removal&) = default;
};

struct role_product_type_lookup {
    role_product_type_key key;
    std::optional<ores::iam::domain::role_product_type> role_product_type;

    friend bool operator==(const role_product_type_lookup&,
                           const role_product_type_lookup&) = default;
};

struct role_product_types_filter {
    std::optional<boost::uuids::uuid> role_id;
    std::optional<std::string> product_type_code;
    std::optional<std::vector<boost::uuids::uuid>> role_id_one_of;
    std::optional<std::vector<std::string>> product_type_code_one_of;

    friend bool operator==(const role_product_types_filter&,
                           const role_product_types_filter&) = default;
};

struct role_product_type_event {
    boost::uuids::uuid event_id;
    role_product_type_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const role_product_type_event&,
                           const role_product_type_event&) = default;
};

struct list_role_product_types_request {
    using response_type = struct list_role_product_types_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_product_types.list";
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
    std::optional<role_product_types_filter> filter;

    friend bool operator==(const list_role_product_types_request&,
                           const list_role_product_types_request&) = default;
};

struct list_role_product_types_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role_product_type> role_product_types;
    std::uint64_t total{};

    friend bool operator==(const list_role_product_types_response&,
                           const list_role_product_types_response&) = default;
};

struct get_role_product_type_request {
    using response_type = struct get_role_product_type_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_product_types.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_product_type_key key;

    friend bool operator==(const get_role_product_type_request&,
                           const get_role_product_type_request&) = default;
};

struct get_role_product_type_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::role_product_type> role_product_type;

    friend bool operator==(const get_role_product_type_response&,
                           const get_role_product_type_response&) = default;
};

struct get_many_role_product_types_request {
    using response_type = struct get_many_role_product_types_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_product_types.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<role_product_type_key> keys;

    friend bool operator==(const get_many_role_product_types_request&,
                           const get_many_role_product_types_request&) = default;
};

struct get_many_role_product_types_response {
    ores::utility::domain::result result;
    std::vector<role_product_type_lookup> entries;

    friend bool operator==(const get_many_role_product_types_response&,
                           const get_many_role_product_types_response&) = default;
};

struct put_role_product_type_request {
    using response_type = struct put_role_product_type_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_product_types.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_product_type_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_role_product_type_request&,
                           const put_role_product_type_request&) = default;
};

struct put_role_product_type_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::role_product_type> role_product_type;

    friend bool operator==(const put_role_product_type_response&,
                           const put_role_product_type_response&) = default;
};

struct put_many_role_product_types_request {
    using response_type = struct put_many_role_product_types_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_product_types.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<role_product_type_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_role_product_types_request&,
                           const put_many_role_product_types_request&) = default;
};

struct put_many_role_product_types_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role_product_type> role_product_types;

    friend bool operator==(const put_many_role_product_types_response&,
                           const put_many_role_product_types_response&) = default;
};

struct delete_role_product_type_request {
    using response_type = struct delete_role_product_type_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_product_types.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_product_type_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_role_product_type_request&,
                           const delete_role_product_type_request&) = default;
};

struct delete_role_product_type_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_role_product_type_response&,
                           const delete_role_product_type_response&) = default;
};

struct delete_many_role_product_types_request {
    using response_type = struct delete_many_role_product_types_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_product_types.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<role_product_type_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_role_product_types_request&,
                           const delete_many_role_product_types_request&) = default;
};

struct delete_many_role_product_types_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_role_product_types_response&,
                           const delete_many_role_product_types_response&) = default;
};

struct list_by_role_id_role_product_types_request {
    using response_type = struct list_by_role_id_role_product_types_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_product_types.list_by_role_id";
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
    std::optional<role_product_types_filter> filter;

    friend bool operator==(const list_by_role_id_role_product_types_request&,
                           const list_by_role_id_role_product_types_request&) = default;
};

struct list_by_role_id_role_product_types_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role_product_type> role_product_types;
    std::uint64_t total{};

    friend bool operator==(const list_by_role_id_role_product_types_response&,
                           const list_by_role_id_role_product_types_response&) = default;
};

struct list_by_product_type_code_role_product_types_request {
    using response_type = struct list_by_product_type_code_role_product_types_response;
    static constexpr std::string_view nats_subject =
        "iam.v1.role_product_types.list_by_product_type_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string product_type_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<role_product_types_filter> filter;

    friend bool operator==(const list_by_product_type_code_role_product_types_request&,
                           const list_by_product_type_code_role_product_types_request&) = default;
};

struct list_by_product_type_code_role_product_types_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role_product_type> role_product_types;
    std::uint64_t total{};

    friend bool operator==(const list_by_product_type_code_role_product_types_response&,
                           const list_by_product_type_code_role_product_types_response&) = default;
};

/**
 * @brief The members of role_product_type_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_product_type_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"role_id", &role_product_type_key::role_id},
        utility::codec::field{"product_type_code", &role_product_type_key::product_type_code},
    };
}

/**
 * @brief The members of role_product_type_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_product_type_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"role_id", &role_product_type_write::role_id},
        utility::codec::field{"product_type_code", &role_product_type_write::product_type_code},
    };
}

/**
 * @brief The members of role_product_type_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_product_type_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &role_product_type_change::write},
        utility::codec::field{"precondition", &role_product_type_change::precondition},
    };
}

/**
 * @brief The members of role_product_type_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_product_type_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &role_product_type_removal::key},
        utility::codec::field{"precondition", &role_product_type_removal::precondition},
    };
}

/**
 * @brief The members of role_product_type_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_product_type_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &role_product_type_lookup::key},
        utility::codec::field{"role_product_type", &role_product_type_lookup::role_product_type},
    };
}

/**
 * @brief The members of role_product_types_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_product_types_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"role_id", &role_product_types_filter::role_id},
        utility::codec::field{"product_type_code", &role_product_types_filter::product_type_code},
        utility::codec::field{"role_id_one_of", &role_product_types_filter::role_id_one_of},
        utility::codec::field{"product_type_code_one_of",
                              &role_product_types_filter::product_type_code_one_of},
    };
}

/**
 * @brief The members of role_product_type_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_product_type_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &role_product_type_event::event_id},
        utility::codec::field{"key", &role_product_type_event::key},
        utility::codec::field{"action", &role_product_type_event::action},
        utility::codec::field{"version", &role_product_type_event::version},
        utility::codec::field{"occurred_at", &role_product_type_event::occurred_at},
        utility::codec::field{"correlation_id", &role_product_type_event::correlation_id},
    };
}

/**
 * @brief The members of list_role_product_types_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_role_product_types_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_role_product_types_request::offset},
        utility::codec::field{"limit", &list_role_product_types_request::limit},
        utility::codec::field{"order", &list_role_product_types_request::order},
        utility::codec::field{"filter", &list_role_product_types_request::filter},
    };
}

/**
 * @brief The members of list_role_product_types_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_role_product_types_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_role_product_types_response::result},
        utility::codec::field{"role_product_types",
                              &list_role_product_types_response::role_product_types},
        utility::codec::field{"total", &list_role_product_types_response::total},
    };
}

/**
 * @brief The members of get_role_product_type_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_role_product_type_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_role_product_type_request::key},
    };
}

/**
 * @brief The members of get_role_product_type_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_role_product_type_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_role_product_type_response::result},
        utility::codec::field{"role_product_type",
                              &get_role_product_type_response::role_product_type},
    };
}

/**
 * @brief The members of get_many_role_product_types_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_role_product_types_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_role_product_types_request::keys},
    };
}

/**
 * @brief The members of get_many_role_product_types_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_role_product_types_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_role_product_types_response::result},
        utility::codec::field{"entries", &get_many_role_product_types_response::entries},
    };
}

/**
 * @brief The members of put_role_product_type_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_role_product_type_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_role_product_type_request::change},
        utility::codec::field{"intent", &put_role_product_type_request::intent},
    };
}

/**
 * @brief The members of put_role_product_type_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_role_product_type_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_role_product_type_response::result},
        utility::codec::field{"role_product_type",
                              &put_role_product_type_response::role_product_type},
    };
}

/**
 * @brief The members of put_many_role_product_types_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_role_product_types_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_role_product_types_request::changes},
        utility::codec::field{"intent", &put_many_role_product_types_request::intent},
    };
}

/**
 * @brief The members of put_many_role_product_types_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_role_product_types_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_role_product_types_response::result},
        utility::codec::field{"role_product_types",
                              &put_many_role_product_types_response::role_product_types},
    };
}

/**
 * @brief The members of delete_role_product_type_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_role_product_type_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_role_product_type_request::removal},
        utility::codec::field{"intent", &delete_role_product_type_request::intent},
    };
}

/**
 * @brief The members of delete_role_product_type_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_role_product_type_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_role_product_type_response::result},
    };
}

/**
 * @brief The members of delete_many_role_product_types_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_role_product_types_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_role_product_types_request::removals},
        utility::codec::field{"intent", &delete_many_role_product_types_request::intent},
    };
}

/**
 * @brief The members of delete_many_role_product_types_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_role_product_types_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_role_product_types_response::result},
    };
}

/**
 * @brief The members of list_by_role_id_role_product_types_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_role_id_role_product_types_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"role_id", &list_by_role_id_role_product_types_request::role_id},
        utility::codec::field{"scope", &list_by_role_id_role_product_types_request::scope},
        utility::codec::field{"offset", &list_by_role_id_role_product_types_request::offset},
        utility::codec::field{"limit", &list_by_role_id_role_product_types_request::limit},
        utility::codec::field{"order", &list_by_role_id_role_product_types_request::order},
        utility::codec::field{"filter", &list_by_role_id_role_product_types_request::filter},
    };
}

/**
 * @brief The members of list_by_role_id_role_product_types_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_role_id_role_product_types_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_by_role_id_role_product_types_response::result},
        utility::codec::field{"role_product_types",
                              &list_by_role_id_role_product_types_response::role_product_types},
        utility::codec::field{"total", &list_by_role_id_role_product_types_response::total},
    };
}

/**
 * @brief The members of list_by_product_type_code_role_product_types_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_product_type_code_role_product_types_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{
            "product_type_code",
            &list_by_product_type_code_role_product_types_request::product_type_code},
        utility::codec::field{"scope",
                              &list_by_product_type_code_role_product_types_request::scope},
        utility::codec::field{"offset",
                              &list_by_product_type_code_role_product_types_request::offset},
        utility::codec::field{"limit",
                              &list_by_product_type_code_role_product_types_request::limit},
        utility::codec::field{"order",
                              &list_by_product_type_code_role_product_types_request::order},
        utility::codec::field{"filter",
                              &list_by_product_type_code_role_product_types_request::filter},
    };
}

/**
 * @brief The members of list_by_product_type_code_role_product_types_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_product_type_code_role_product_types_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result",
                              &list_by_product_type_code_role_product_types_response::result},
        utility::codec::field{
            "role_product_types",
            &list_by_product_type_code_role_product_types_response::role_product_types},
        utility::codec::field{"total",
                              &list_by_product_type_code_role_product_types_response::total},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_product_type_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_product_type_key& v);
    static ores::iam::messaging::role_product_type_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_product_type_key& v);
    static ores::iam::messaging::role_product_type_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_product_type_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_product_type_write& v);
    static ores::iam::messaging::role_product_type_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_product_type_write& v);
    static ores::iam::messaging::role_product_type_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_product_type_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_product_type_change& v);
    static ores::iam::messaging::role_product_type_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_product_type_change& v);
    static ores::iam::messaging::role_product_type_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_product_type_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_product_type_removal& v);
    static ores::iam::messaging::role_product_type_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_product_type_removal& v);
    static ores::iam::messaging::role_product_type_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_product_type_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_product_type_lookup& v);
    static ores::iam::messaging::role_product_type_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_product_type_lookup& v);
    static ores::iam::messaging::role_product_type_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_product_types_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_product_types_filter& v);
    static ores::iam::messaging::role_product_types_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_product_types_filter& v);
    static ores::iam::messaging::role_product_types_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_product_type_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_product_type_event& v);
    static ores::iam::messaging::role_product_type_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_product_type_event& v);
    static ores::iam::messaging::role_product_type_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_role_product_types_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_role_product_types_request& v);
    static ores::iam::messaging::list_role_product_types_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_role_product_types_request& v);
    static ores::iam::messaging::list_role_product_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_role_product_types_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_role_product_types_response& v);
    static ores::iam::messaging::list_role_product_types_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_role_product_types_response& v);
    static ores::iam::messaging::list_role_product_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_role_product_type_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_role_product_type_request& v);
    static ores::iam::messaging::get_role_product_type_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_role_product_type_request& v);
    static ores::iam::messaging::get_role_product_type_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_role_product_type_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_role_product_type_response& v);
    static ores::iam::messaging::get_role_product_type_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_role_product_type_response& v);
    static ores::iam::messaging::get_role_product_type_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_role_product_types_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_many_role_product_types_request& v);
    static ores::iam::messaging::get_many_role_product_types_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_many_role_product_types_request& v);
    static ores::iam::messaging::get_many_role_product_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_role_product_types_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_many_role_product_types_response& v);
    static ores::iam::messaging::get_many_role_product_types_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_many_role_product_types_response& v);
    static ores::iam::messaging::get_many_role_product_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_role_product_type_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_role_product_type_request& v);
    static ores::iam::messaging::put_role_product_type_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_role_product_type_request& v);
    static ores::iam::messaging::put_role_product_type_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_role_product_type_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_role_product_type_response& v);
    static ores::iam::messaging::put_role_product_type_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_role_product_type_response& v);
    static ores::iam::messaging::put_role_product_type_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_many_role_product_types_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::put_many_role_product_types_request& v);
    static ores::iam::messaging::put_many_role_product_types_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::put_many_role_product_types_request& v);
    static ores::iam::messaging::put_many_role_product_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_many_role_product_types_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::put_many_role_product_types_response& v);
    static ores::iam::messaging::put_many_role_product_types_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::put_many_role_product_types_response& v);
    static ores::iam::messaging::put_many_role_product_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_role_product_type_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_role_product_type_request& v);
    static ores::iam::messaging::delete_role_product_type_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_role_product_type_request& v);
    static ores::iam::messaging::delete_role_product_type_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_role_product_type_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_role_product_type_response& v);
    static ores::iam::messaging::delete_role_product_type_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_role_product_type_response& v);
    static ores::iam::messaging::delete_role_product_type_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::delete_many_role_product_types_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::delete_many_role_product_types_request& v);
    static ores::iam::messaging::delete_many_role_product_types_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_many_role_product_types_request& v);
    static ores::iam::messaging::delete_many_role_product_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::delete_many_role_product_types_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::delete_many_role_product_types_response& v);
    static ores::iam::messaging::delete_many_role_product_types_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_many_role_product_types_response& v);
    static ores::iam::messaging::delete_many_role_product_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_role_id_role_product_types_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_by_role_id_role_product_types_request& v);
    static ores::iam::messaging::list_by_role_id_role_product_types_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_by_role_id_role_product_types_request& v);
    static ores::iam::messaging::list_by_role_id_role_product_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_role_id_role_product_types_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_by_role_id_role_product_types_response& v);
    static ores::iam::messaging::list_by_role_id_role_product_types_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_by_role_id_role_product_types_response& v);
    static ores::iam::messaging::list_by_role_id_role_product_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_product_type_code_role_product_types_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_by_product_type_code_role_product_types_request& v);
    static ores::iam::messaging::list_by_product_type_code_role_product_types_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::iam::messaging::list_by_product_type_code_role_product_types_request& v);
    static ores::iam::messaging::list_by_product_type_code_role_product_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_product_type_code_role_product_types_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::iam::messaging::list_by_product_type_code_role_product_types_response& v);
    static ores::iam::messaging::list_by_product_type_code_role_product_types_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::iam::messaging::list_by_product_type_code_role_product_types_response& v);
    static ores::iam::messaging::list_by_product_type_code_role_product_types_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
