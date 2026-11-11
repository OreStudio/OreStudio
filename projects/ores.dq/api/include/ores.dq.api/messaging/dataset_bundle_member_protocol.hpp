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
#ifndef ORES_DQ_API_MESSAGING_DATASET_BUNDLE_MEMBER_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_DATASET_BUNDLE_MEMBER_PROTOCOL_HPP

#include "ores.dq.api/domain/dataset_bundle_member.hpp"
#include "ores.dq.api/export.hpp"
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

namespace ores::dq::messaging {

struct dataset_bundle_member_key {
    std::string bundle_code;
    std::string dataset_code;

    friend bool operator==(const dataset_bundle_member_key&,
                           const dataset_bundle_member_key&) = default;
};

struct dataset_bundle_member_write {
    std::string bundle_code;
    std::string dataset_code;
    int display_order{};
    bool optional{};

    friend bool operator==(const dataset_bundle_member_write&,
                           const dataset_bundle_member_write&) = default;
};

struct dataset_bundle_member_change {
    dataset_bundle_member_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const dataset_bundle_member_change&,
                           const dataset_bundle_member_change&) = default;
};

struct dataset_bundle_member_removal {
    dataset_bundle_member_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const dataset_bundle_member_removal&,
                           const dataset_bundle_member_removal&) = default;
};

struct dataset_bundle_member_lookup {
    dataset_bundle_member_key key;
    std::optional<ores::dq::domain::dataset_bundle_member> dataset_bundle_member;

    friend bool operator==(const dataset_bundle_member_lookup&,
                           const dataset_bundle_member_lookup&) = default;
};

struct dataset_bundle_members_filter {
    std::optional<std::string> bundle_code;
    std::optional<std::vector<std::string>> bundle_code_one_of;

    friend bool operator==(const dataset_bundle_members_filter&,
                           const dataset_bundle_members_filter&) = default;
};

struct dataset_bundle_member_event {
    boost::uuids::uuid event_id;
    dataset_bundle_member_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const dataset_bundle_member_event&,
                           const dataset_bundle_member_event&) = default;
};

struct list_dataset_bundle_members_request {
    using response_type = struct list_dataset_bundle_members_response;
    static constexpr std::string_view nats_subject = "dq.v1.dataset_bundle_members.list";
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
    std::optional<dataset_bundle_members_filter> filter;

    friend bool operator==(const list_dataset_bundle_members_request&,
                           const list_dataset_bundle_members_request&) = default;
};

struct list_dataset_bundle_members_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::dataset_bundle_member> dataset_bundle_members;
    std::uint64_t total{};

    friend bool operator==(const list_dataset_bundle_members_response&,
                           const list_dataset_bundle_members_response&) = default;
};

struct get_dataset_bundle_member_request {
    using response_type = struct get_dataset_bundle_member_response;
    static constexpr std::string_view nats_subject = "dq.v1.dataset_bundle_members.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    dataset_bundle_member_key key;

    friend bool operator==(const get_dataset_bundle_member_request&,
                           const get_dataset_bundle_member_request&) = default;
};

struct get_dataset_bundle_member_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::dataset_bundle_member> dataset_bundle_member;

    friend bool operator==(const get_dataset_bundle_member_response&,
                           const get_dataset_bundle_member_response&) = default;
};

struct get_many_dataset_bundle_members_request {
    using response_type = struct get_many_dataset_bundle_members_response;
    static constexpr std::string_view nats_subject = "dq.v1.dataset_bundle_members.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<dataset_bundle_member_key> keys;

    friend bool operator==(const get_many_dataset_bundle_members_request&,
                           const get_many_dataset_bundle_members_request&) = default;
};

struct get_many_dataset_bundle_members_response {
    ores::utility::domain::result result;
    std::vector<dataset_bundle_member_lookup> entries;

    friend bool operator==(const get_many_dataset_bundle_members_response&,
                           const get_many_dataset_bundle_members_response&) = default;
};

struct put_dataset_bundle_member_request {
    using response_type = struct put_dataset_bundle_member_response;
    static constexpr std::string_view nats_subject = "dq.v1.dataset_bundle_members.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    dataset_bundle_member_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_dataset_bundle_member_request&,
                           const put_dataset_bundle_member_request&) = default;
};

struct put_dataset_bundle_member_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::dataset_bundle_member> dataset_bundle_member;

    friend bool operator==(const put_dataset_bundle_member_response&,
                           const put_dataset_bundle_member_response&) = default;
};

struct put_many_dataset_bundle_members_request {
    using response_type = struct put_many_dataset_bundle_members_response;
    static constexpr std::string_view nats_subject = "dq.v1.dataset_bundle_members.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<dataset_bundle_member_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_dataset_bundle_members_request&,
                           const put_many_dataset_bundle_members_request&) = default;
};

struct put_many_dataset_bundle_members_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::dataset_bundle_member> dataset_bundle_members;

    friend bool operator==(const put_many_dataset_bundle_members_response&,
                           const put_many_dataset_bundle_members_response&) = default;
};

struct delete_dataset_bundle_member_request {
    using response_type = struct delete_dataset_bundle_member_response;
    static constexpr std::string_view nats_subject = "dq.v1.dataset_bundle_members.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    dataset_bundle_member_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_dataset_bundle_member_request&,
                           const delete_dataset_bundle_member_request&) = default;
};

struct delete_dataset_bundle_member_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_dataset_bundle_member_response&,
                           const delete_dataset_bundle_member_response&) = default;
};

struct delete_many_dataset_bundle_members_request {
    using response_type = struct delete_many_dataset_bundle_members_response;
    static constexpr std::string_view nats_subject = "dq.v1.dataset_bundle_members.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<dataset_bundle_member_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_dataset_bundle_members_request&,
                           const delete_many_dataset_bundle_members_request&) = default;
};

struct delete_many_dataset_bundle_members_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_dataset_bundle_members_response&,
                           const delete_many_dataset_bundle_members_response&) = default;
};

struct list_by_bundle_code_dataset_bundle_members_request {
    using response_type = struct list_by_bundle_code_dataset_bundle_members_response;
    static constexpr std::string_view nats_subject =
        "dq.v1.dataset_bundle_members.list_by_bundle_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string bundle_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<dataset_bundle_members_filter> filter;

    friend bool operator==(const list_by_bundle_code_dataset_bundle_members_request&,
                           const list_by_bundle_code_dataset_bundle_members_request&) = default;
};

struct list_by_bundle_code_dataset_bundle_members_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::dataset_bundle_member> dataset_bundle_members;
    std::uint64_t total{};

    friend bool operator==(const list_by_bundle_code_dataset_bundle_members_response&,
                           const list_by_bundle_code_dataset_bundle_members_response&) = default;
};

/**
 * @brief The members of dataset_bundle_member_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<dataset_bundle_member_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"bundle_code", &dataset_bundle_member_key::bundle_code},
        utility::codec::field{"dataset_code", &dataset_bundle_member_key::dataset_code},
    };
}

/**
 * @brief The members of dataset_bundle_member_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<dataset_bundle_member_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"bundle_code", &dataset_bundle_member_write::bundle_code},
        utility::codec::field{"dataset_code", &dataset_bundle_member_write::dataset_code},
        utility::codec::field{"display_order", &dataset_bundle_member_write::display_order},
        utility::codec::field{"optional", &dataset_bundle_member_write::optional},
    };
}

/**
 * @brief The members of dataset_bundle_member_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<dataset_bundle_member_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &dataset_bundle_member_change::write},
        utility::codec::field{"precondition", &dataset_bundle_member_change::precondition},
    };
}

/**
 * @brief The members of dataset_bundle_member_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<dataset_bundle_member_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &dataset_bundle_member_removal::key},
        utility::codec::field{"precondition", &dataset_bundle_member_removal::precondition},
    };
}

/**
 * @brief The members of dataset_bundle_member_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<dataset_bundle_member_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &dataset_bundle_member_lookup::key},
        utility::codec::field{"dataset_bundle_member",
                              &dataset_bundle_member_lookup::dataset_bundle_member},
    };
}

/**
 * @brief The members of dataset_bundle_members_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<dataset_bundle_members_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"bundle_code", &dataset_bundle_members_filter::bundle_code},
        utility::codec::field{"bundle_code_one_of",
                              &dataset_bundle_members_filter::bundle_code_one_of},
    };
}

/**
 * @brief The members of dataset_bundle_member_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<dataset_bundle_member_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &dataset_bundle_member_event::event_id},
        utility::codec::field{"key", &dataset_bundle_member_event::key},
        utility::codec::field{"action", &dataset_bundle_member_event::action},
        utility::codec::field{"version", &dataset_bundle_member_event::version},
        utility::codec::field{"occurred_at", &dataset_bundle_member_event::occurred_at},
        utility::codec::field{"correlation_id", &dataset_bundle_member_event::correlation_id},
    };
}

/**
 * @brief The members of list_dataset_bundle_members_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_dataset_bundle_members_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_dataset_bundle_members_request::offset},
        utility::codec::field{"limit", &list_dataset_bundle_members_request::limit},
        utility::codec::field{"order", &list_dataset_bundle_members_request::order},
        utility::codec::field{"filter", &list_dataset_bundle_members_request::filter},
    };
}

/**
 * @brief The members of list_dataset_bundle_members_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_dataset_bundle_members_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_dataset_bundle_members_response::result},
        utility::codec::field{"dataset_bundle_members",
                              &list_dataset_bundle_members_response::dataset_bundle_members},
        utility::codec::field{"total", &list_dataset_bundle_members_response::total},
    };
}

/**
 * @brief The members of get_dataset_bundle_member_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_dataset_bundle_member_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_dataset_bundle_member_request::key},
    };
}

/**
 * @brief The members of get_dataset_bundle_member_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_dataset_bundle_member_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_dataset_bundle_member_response::result},
        utility::codec::field{"dataset_bundle_member",
                              &get_dataset_bundle_member_response::dataset_bundle_member},
    };
}

/**
 * @brief The members of get_many_dataset_bundle_members_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_dataset_bundle_members_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_dataset_bundle_members_request::keys},
    };
}

/**
 * @brief The members of get_many_dataset_bundle_members_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_dataset_bundle_members_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_dataset_bundle_members_response::result},
        utility::codec::field{"entries", &get_many_dataset_bundle_members_response::entries},
    };
}

/**
 * @brief The members of put_dataset_bundle_member_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_dataset_bundle_member_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_dataset_bundle_member_request::change},
        utility::codec::field{"intent", &put_dataset_bundle_member_request::intent},
    };
}

/**
 * @brief The members of put_dataset_bundle_member_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_dataset_bundle_member_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_dataset_bundle_member_response::result},
        utility::codec::field{"dataset_bundle_member",
                              &put_dataset_bundle_member_response::dataset_bundle_member},
    };
}

/**
 * @brief The members of put_many_dataset_bundle_members_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_dataset_bundle_members_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_dataset_bundle_members_request::changes},
        utility::codec::field{"intent", &put_many_dataset_bundle_members_request::intent},
    };
}

/**
 * @brief The members of put_many_dataset_bundle_members_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_dataset_bundle_members_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_dataset_bundle_members_response::result},
        utility::codec::field{"dataset_bundle_members",
                              &put_many_dataset_bundle_members_response::dataset_bundle_members},
    };
}

/**
 * @brief The members of delete_dataset_bundle_member_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_dataset_bundle_member_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_dataset_bundle_member_request::removal},
        utility::codec::field{"intent", &delete_dataset_bundle_member_request::intent},
    };
}

/**
 * @brief The members of delete_dataset_bundle_member_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_dataset_bundle_member_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_dataset_bundle_member_response::result},
    };
}

/**
 * @brief The members of delete_many_dataset_bundle_members_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_dataset_bundle_members_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_dataset_bundle_members_request::removals},
        utility::codec::field{"intent", &delete_many_dataset_bundle_members_request::intent},
    };
}

/**
 * @brief The members of delete_many_dataset_bundle_members_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_dataset_bundle_members_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_dataset_bundle_members_response::result},
    };
}

/**
 * @brief The members of list_by_bundle_code_dataset_bundle_members_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_bundle_code_dataset_bundle_members_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"bundle_code",
                              &list_by_bundle_code_dataset_bundle_members_request::bundle_code},
        utility::codec::field{"scope", &list_by_bundle_code_dataset_bundle_members_request::scope},
        utility::codec::field{"offset",
                              &list_by_bundle_code_dataset_bundle_members_request::offset},
        utility::codec::field{"limit", &list_by_bundle_code_dataset_bundle_members_request::limit},
        utility::codec::field{"order", &list_by_bundle_code_dataset_bundle_members_request::order},
        utility::codec::field{"filter",
                              &list_by_bundle_code_dataset_bundle_members_request::filter},
    };
}

/**
 * @brief The members of list_by_bundle_code_dataset_bundle_members_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_bundle_code_dataset_bundle_members_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result",
                              &list_by_bundle_code_dataset_bundle_members_response::result},
        utility::codec::field{
            "dataset_bundle_members",
            &list_by_bundle_code_dataset_bundle_members_response::dataset_bundle_members},
        utility::codec::field{"total", &list_by_bundle_code_dataset_bundle_members_response::total},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_bundle_member_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_bundle_member_key& v);
    static ores::dq::messaging::dataset_bundle_member_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_bundle_member_key& v);
    static ores::dq::messaging::dataset_bundle_member_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_bundle_member_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_bundle_member_write& v);
    static ores::dq::messaging::dataset_bundle_member_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_bundle_member_write& v);
    static ores::dq::messaging::dataset_bundle_member_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_bundle_member_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_bundle_member_change& v);
    static ores::dq::messaging::dataset_bundle_member_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_bundle_member_change& v);
    static ores::dq::messaging::dataset_bundle_member_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_bundle_member_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_bundle_member_removal& v);
    static ores::dq::messaging::dataset_bundle_member_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_bundle_member_removal& v);
    static ores::dq::messaging::dataset_bundle_member_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_bundle_member_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_bundle_member_lookup& v);
    static ores::dq::messaging::dataset_bundle_member_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_bundle_member_lookup& v);
    static ores::dq::messaging::dataset_bundle_member_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_bundle_members_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_bundle_members_filter& v);
    static ores::dq::messaging::dataset_bundle_members_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_bundle_members_filter& v);
    static ores::dq::messaging::dataset_bundle_members_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_bundle_member_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_bundle_member_event& v);
    static ores::dq::messaging::dataset_bundle_member_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_bundle_member_event& v);
    static ores::dq::messaging::dataset_bundle_member_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_dataset_bundle_members_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_dataset_bundle_members_request& v);
    static ores::dq::messaging::list_dataset_bundle_members_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_dataset_bundle_members_request& v);
    static ores::dq::messaging::list_dataset_bundle_members_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_dataset_bundle_members_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_dataset_bundle_members_response& v);
    static ores::dq::messaging::list_dataset_bundle_members_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_dataset_bundle_members_response& v);
    static ores::dq::messaging::list_dataset_bundle_members_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_dataset_bundle_member_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_dataset_bundle_member_request& v);
    static ores::dq::messaging::get_dataset_bundle_member_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_dataset_bundle_member_request& v);
    static ores::dq::messaging::get_dataset_bundle_member_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_dataset_bundle_member_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_dataset_bundle_member_response& v);
    static ores::dq::messaging::get_dataset_bundle_member_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_dataset_bundle_member_response& v);
    static ores::dq::messaging::get_dataset_bundle_member_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT
    wire_traits<ores::dq::messaging::get_many_dataset_bundle_members_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::get_many_dataset_bundle_members_request& v);
    static ores::dq::messaging::get_many_dataset_bundle_members_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_many_dataset_bundle_members_request& v);
    static ores::dq::messaging::get_many_dataset_bundle_members_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT
    wire_traits<ores::dq::messaging::get_many_dataset_bundle_members_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::get_many_dataset_bundle_members_response& v);
    static ores::dq::messaging::get_many_dataset_bundle_members_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_many_dataset_bundle_members_response& v);
    static ores::dq::messaging::get_many_dataset_bundle_members_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_dataset_bundle_member_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_dataset_bundle_member_request& v);
    static ores::dq::messaging::put_dataset_bundle_member_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::put_dataset_bundle_member_request& v);
    static ores::dq::messaging::put_dataset_bundle_member_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_dataset_bundle_member_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_dataset_bundle_member_response& v);
    static ores::dq::messaging::put_dataset_bundle_member_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::put_dataset_bundle_member_response& v);
    static ores::dq::messaging::put_dataset_bundle_member_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT
    wire_traits<ores::dq::messaging::put_many_dataset_bundle_members_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::put_many_dataset_bundle_members_request& v);
    static ores::dq::messaging::put_many_dataset_bundle_members_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::put_many_dataset_bundle_members_request& v);
    static ores::dq::messaging::put_many_dataset_bundle_members_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT
    wire_traits<ores::dq::messaging::put_many_dataset_bundle_members_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::put_many_dataset_bundle_members_response& v);
    static ores::dq::messaging::put_many_dataset_bundle_members_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::put_many_dataset_bundle_members_response& v);
    static ores::dq::messaging::put_many_dataset_bundle_members_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_dataset_bundle_member_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::delete_dataset_bundle_member_request& v);
    static ores::dq::messaging::delete_dataset_bundle_member_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_dataset_bundle_member_request& v);
    static ores::dq::messaging::delete_dataset_bundle_member_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_dataset_bundle_member_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::delete_dataset_bundle_member_response& v);
    static ores::dq::messaging::delete_dataset_bundle_member_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_dataset_bundle_member_response& v);
    static ores::dq::messaging::delete_dataset_bundle_member_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT
    wire_traits<ores::dq::messaging::delete_many_dataset_bundle_members_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::delete_many_dataset_bundle_members_request& v);
    static ores::dq::messaging::delete_many_dataset_bundle_members_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_dataset_bundle_members_request& v);
    static ores::dq::messaging::delete_many_dataset_bundle_members_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT
    wire_traits<ores::dq::messaging::delete_many_dataset_bundle_members_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::delete_many_dataset_bundle_members_response& v);
    static ores::dq::messaging::delete_many_dataset_bundle_members_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_dataset_bundle_members_response& v);
    static ores::dq::messaging::delete_many_dataset_bundle_members_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT
    wire_traits<ores::dq::messaging::list_by_bundle_code_dataset_bundle_members_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_by_bundle_code_dataset_bundle_members_request& v);
    static ores::dq::messaging::list_by_bundle_code_dataset_bundle_members_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_by_bundle_code_dataset_bundle_members_request& v);
    static ores::dq::messaging::list_by_bundle_code_dataset_bundle_members_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT
    wire_traits<ores::dq::messaging::list_by_bundle_code_dataset_bundle_members_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_by_bundle_code_dataset_bundle_members_response& v);
    static ores::dq::messaging::list_by_bundle_code_dataset_bundle_members_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::dq::messaging::list_by_bundle_code_dataset_bundle_members_response& v);
    static ores::dq::messaging::list_by_bundle_code_dataset_bundle_members_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
