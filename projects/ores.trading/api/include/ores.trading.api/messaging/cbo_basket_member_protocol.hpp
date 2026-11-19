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
#ifndef ORES_TRADING_API_MESSAGING_CBO_BASKET_MEMBER_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_CBO_BASKET_MEMBER_PROTOCOL_HPP

#include "ores.trading.api/domain/cbo_basket_member_codec.hpp"
#include "ores.trading.api/export.hpp"
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

namespace ores::trading::messaging {

struct cbo_basket_member_key {
    boost::uuids::uuid trade_id;
    int sequence_number{};

    friend bool operator==(const cbo_basket_member_key&, const cbo_basket_member_key&) = default;
};

struct cbo_basket_member_write {
    boost::uuids::uuid trade_id;
    int sequence_number{};
    boost::uuids::uuid trade_activity_id;
    std::string sub_trade_id;
    std::string security_id;
    ores::utility::decimal::decimal bond_notional;
    std::optional<std::string> counterparty;

    friend bool operator==(const cbo_basket_member_write&,
                           const cbo_basket_member_write&) = default;
};

struct cbo_basket_member_change {
    cbo_basket_member_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const cbo_basket_member_change&,
                           const cbo_basket_member_change&) = default;
};

struct cbo_basket_member_removal {
    cbo_basket_member_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const cbo_basket_member_removal&,
                           const cbo_basket_member_removal&) = default;
};

struct cbo_basket_member_lookup {
    cbo_basket_member_key key;
    std::optional<ores::trading::domain::cbo_basket_member> cbo_basket_member;

    friend bool operator==(const cbo_basket_member_lookup&,
                           const cbo_basket_member_lookup&) = default;
};

struct cbo_basket_members_filter {
    std::optional<boost::uuids::uuid> trade_id;
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;

    friend bool operator==(const cbo_basket_members_filter&,
                           const cbo_basket_members_filter&) = default;
};

struct cbo_basket_member_event {
    boost::uuids::uuid event_id;
    cbo_basket_member_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const cbo_basket_member_event&,
                           const cbo_basket_member_event&) = default;
};

struct cbo_basket_member_version_key {
    cbo_basket_member_key cbo_basket_member;
    std::uint32_t version{};

    friend bool operator==(const cbo_basket_member_version_key&,
                           const cbo_basket_member_version_key&) = default;
};

struct cbo_basket_member_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const cbo_basket_member_versions_filter&,
                           const cbo_basket_member_versions_filter&) = default;
};

struct list_cbo_basket_members_request {
    using response_type = struct list_cbo_basket_members_response;
    static constexpr std::string_view nats_subject = "trading.v1.cbo_basket_members.list";
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
    std::optional<cbo_basket_members_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_cbo_basket_members_request&,
                           const list_cbo_basket_members_request&) = default;
};

struct list_cbo_basket_members_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::cbo_basket_member> cbo_basket_members;
    std::uint64_t total{};

    friend bool operator==(const list_cbo_basket_members_response&,
                           const list_cbo_basket_members_response&) = default;
};

struct get_cbo_basket_member_request {
    using response_type = struct get_cbo_basket_member_response;
    static constexpr std::string_view nats_subject = "trading.v1.cbo_basket_members.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cbo_basket_member_key key;

    friend bool operator==(const get_cbo_basket_member_request&,
                           const get_cbo_basket_member_request&) = default;
};

struct get_cbo_basket_member_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::cbo_basket_member> cbo_basket_member;

    friend bool operator==(const get_cbo_basket_member_response&,
                           const get_cbo_basket_member_response&) = default;
};

struct get_many_cbo_basket_members_request {
    using response_type = struct get_many_cbo_basket_members_response;
    static constexpr std::string_view nats_subject = "trading.v1.cbo_basket_members.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cbo_basket_member_key> keys;

    friend bool operator==(const get_many_cbo_basket_members_request&,
                           const get_many_cbo_basket_members_request&) = default;
};

struct get_many_cbo_basket_members_response {
    ores::utility::domain::result result;
    std::vector<cbo_basket_member_lookup> entries;

    friend bool operator==(const get_many_cbo_basket_members_response&,
                           const get_many_cbo_basket_members_response&) = default;
};

struct put_cbo_basket_member_request {
    using response_type = struct put_cbo_basket_member_response;
    static constexpr std::string_view nats_subject = "trading.v1.cbo_basket_members.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cbo_basket_member_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_cbo_basket_member_request&,
                           const put_cbo_basket_member_request&) = default;
};

struct put_cbo_basket_member_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::cbo_basket_member> cbo_basket_member;

    friend bool operator==(const put_cbo_basket_member_response&,
                           const put_cbo_basket_member_response&) = default;
};

struct put_many_cbo_basket_members_request {
    using response_type = struct put_many_cbo_basket_members_response;
    static constexpr std::string_view nats_subject = "trading.v1.cbo_basket_members.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cbo_basket_member_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_cbo_basket_members_request&,
                           const put_many_cbo_basket_members_request&) = default;
};

struct put_many_cbo_basket_members_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::cbo_basket_member> cbo_basket_members;

    friend bool operator==(const put_many_cbo_basket_members_response&,
                           const put_many_cbo_basket_members_response&) = default;
};

struct delete_cbo_basket_member_request {
    using response_type = struct delete_cbo_basket_member_response;
    static constexpr std::string_view nats_subject = "trading.v1.cbo_basket_members.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cbo_basket_member_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_cbo_basket_member_request&,
                           const delete_cbo_basket_member_request&) = default;
};

struct delete_cbo_basket_member_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_cbo_basket_member_response&,
                           const delete_cbo_basket_member_response&) = default;
};

struct delete_many_cbo_basket_members_request {
    using response_type = struct delete_many_cbo_basket_members_response;
    static constexpr std::string_view nats_subject = "trading.v1.cbo_basket_members.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cbo_basket_member_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_cbo_basket_members_request&,
                           const delete_many_cbo_basket_members_request&) = default;
};

struct delete_many_cbo_basket_members_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_cbo_basket_members_response&,
                           const delete_many_cbo_basket_members_response&) = default;
};

struct list_by_trade_id_cbo_basket_members_request {
    using response_type = struct list_by_trade_id_cbo_basket_members_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.cbo_basket_members.list_by_trade_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid trade_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<cbo_basket_members_filter> filter;

    friend bool operator==(const list_by_trade_id_cbo_basket_members_request&,
                           const list_by_trade_id_cbo_basket_members_request&) = default;
};

struct list_by_trade_id_cbo_basket_members_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::cbo_basket_member> cbo_basket_members;
    std::uint64_t total{};

    friend bool operator==(const list_by_trade_id_cbo_basket_members_response&,
                           const list_by_trade_id_cbo_basket_members_response&) = default;
};

struct list_cbo_basket_member_versions_request {
    using response_type = struct list_cbo_basket_member_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.cbo_basket_members_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cbo_basket_member_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<cbo_basket_member_versions_filter> filter;

    friend bool operator==(const list_cbo_basket_member_versions_request&,
                           const list_cbo_basket_member_versions_request&) = default;
};

struct list_cbo_basket_member_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::cbo_basket_member> versions;
    std::uint64_t total{};

    friend bool operator==(const list_cbo_basket_member_versions_response&,
                           const list_cbo_basket_member_versions_response&) = default;
};

struct get_cbo_basket_member_version_request {
    using response_type = struct get_cbo_basket_member_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.cbo_basket_members_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cbo_basket_member_version_key key;

    friend bool operator==(const get_cbo_basket_member_version_request&,
                           const get_cbo_basket_member_version_request&) = default;
};

struct get_cbo_basket_member_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::cbo_basket_member> version;

    friend bool operator==(const get_cbo_basket_member_version_response&,
                           const get_cbo_basket_member_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace cbo_basket_member_event_subjects {
inline constexpr std::string_view created = "trading.v1.events.cbo_basket_members.created";
inline constexpr std::string_view updated = "trading.v1.events.cbo_basket_members.updated";
inline constexpr std::string_view deleted = "trading.v1.events.cbo_basket_members.deleted";
}
/**
 * @brief The members of cbo_basket_member_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cbo_basket_member_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"trade_id", &cbo_basket_member_key::trade_id},
        utility::codec::field{"sequence_number", &cbo_basket_member_key::sequence_number},
    };
}

/**
 * @brief The members of cbo_basket_member_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cbo_basket_member_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"trade_id", &cbo_basket_member_write::trade_id},
        utility::codec::field{"sequence_number", &cbo_basket_member_write::sequence_number},
        utility::codec::field{"trade_activity_id", &cbo_basket_member_write::trade_activity_id},
        utility::codec::field{"sub_trade_id", &cbo_basket_member_write::sub_trade_id},
        utility::codec::field{"security_id", &cbo_basket_member_write::security_id},
        utility::codec::field{"bond_notional", &cbo_basket_member_write::bond_notional},
        utility::codec::field{"counterparty", &cbo_basket_member_write::counterparty},
    };
}

/**
 * @brief The members of cbo_basket_member_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cbo_basket_member_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &cbo_basket_member_change::write},
        utility::codec::field{"precondition", &cbo_basket_member_change::precondition},
    };
}

/**
 * @brief The members of cbo_basket_member_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cbo_basket_member_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &cbo_basket_member_removal::key},
        utility::codec::field{"precondition", &cbo_basket_member_removal::precondition},
    };
}

/**
 * @brief The members of cbo_basket_member_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cbo_basket_member_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &cbo_basket_member_lookup::key},
        utility::codec::field{"cbo_basket_member", &cbo_basket_member_lookup::cbo_basket_member},
    };
}

/**
 * @brief The members of cbo_basket_members_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cbo_basket_members_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"trade_id", &cbo_basket_members_filter::trade_id},
        utility::codec::field{"trade_id_one_of", &cbo_basket_members_filter::trade_id_one_of},
    };
}

/**
 * @brief The members of cbo_basket_member_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cbo_basket_member_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &cbo_basket_member_event::event_id},
        utility::codec::field{"key", &cbo_basket_member_event::key},
        utility::codec::field{"action", &cbo_basket_member_event::action},
        utility::codec::field{"version", &cbo_basket_member_event::version},
        utility::codec::field{"occurred_at", &cbo_basket_member_event::occurred_at},
        utility::codec::field{"correlation_id", &cbo_basket_member_event::correlation_id},
    };
}

/**
 * @brief The members of cbo_basket_member_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cbo_basket_member_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"cbo_basket_member",
                              &cbo_basket_member_version_key::cbo_basket_member},
        utility::codec::field{"version", &cbo_basket_member_version_key::version},
    };
}

/**
 * @brief The members of cbo_basket_member_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cbo_basket_member_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &cbo_basket_member_versions_filter::version},
        utility::codec::field{"from_version", &cbo_basket_member_versions_filter::from_version},
        utility::codec::field{"to_version", &cbo_basket_member_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_cbo_basket_members_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_cbo_basket_members_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_cbo_basket_members_request::offset},
        utility::codec::field{"limit", &list_cbo_basket_members_request::limit},
        utility::codec::field{"order", &list_cbo_basket_members_request::order},
        utility::codec::field{"filter", &list_cbo_basket_members_request::filter},
        utility::codec::field{"as_of", &list_cbo_basket_members_request::as_of},
    };
}

/**
 * @brief The members of list_cbo_basket_members_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_cbo_basket_members_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_cbo_basket_members_response::result},
        utility::codec::field{"cbo_basket_members",
                              &list_cbo_basket_members_response::cbo_basket_members},
        utility::codec::field{"total", &list_cbo_basket_members_response::total},
    };
}

/**
 * @brief The members of get_cbo_basket_member_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_cbo_basket_member_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_cbo_basket_member_request::key},
    };
}

/**
 * @brief The members of get_cbo_basket_member_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_cbo_basket_member_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_cbo_basket_member_response::result},
        utility::codec::field{"cbo_basket_member",
                              &get_cbo_basket_member_response::cbo_basket_member},
    };
}

/**
 * @brief The members of get_many_cbo_basket_members_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_cbo_basket_members_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_cbo_basket_members_request::keys},
    };
}

/**
 * @brief The members of get_many_cbo_basket_members_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_cbo_basket_members_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_cbo_basket_members_response::result},
        utility::codec::field{"entries", &get_many_cbo_basket_members_response::entries},
    };
}

/**
 * @brief The members of put_cbo_basket_member_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_cbo_basket_member_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_cbo_basket_member_request::change},
        utility::codec::field{"intent", &put_cbo_basket_member_request::intent},
    };
}

/**
 * @brief The members of put_cbo_basket_member_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_cbo_basket_member_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_cbo_basket_member_response::result},
        utility::codec::field{"cbo_basket_member",
                              &put_cbo_basket_member_response::cbo_basket_member},
    };
}

/**
 * @brief The members of put_many_cbo_basket_members_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_cbo_basket_members_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_cbo_basket_members_request::changes},
        utility::codec::field{"intent", &put_many_cbo_basket_members_request::intent},
    };
}

/**
 * @brief The members of put_many_cbo_basket_members_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_cbo_basket_members_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_cbo_basket_members_response::result},
        utility::codec::field{"cbo_basket_members",
                              &put_many_cbo_basket_members_response::cbo_basket_members},
    };
}

/**
 * @brief The members of delete_cbo_basket_member_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_cbo_basket_member_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_cbo_basket_member_request::removal},
        utility::codec::field{"intent", &delete_cbo_basket_member_request::intent},
    };
}

/**
 * @brief The members of delete_cbo_basket_member_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_cbo_basket_member_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_cbo_basket_member_response::result},
    };
}

/**
 * @brief The members of delete_many_cbo_basket_members_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_cbo_basket_members_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_cbo_basket_members_request::removals},
        utility::codec::field{"intent", &delete_many_cbo_basket_members_request::intent},
    };
}

/**
 * @brief The members of delete_many_cbo_basket_members_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_cbo_basket_members_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_cbo_basket_members_response::result},
    };
}

/**
 * @brief The members of list_by_trade_id_cbo_basket_members_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_trade_id_cbo_basket_members_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"trade_id", &list_by_trade_id_cbo_basket_members_request::trade_id},
        utility::codec::field{"scope", &list_by_trade_id_cbo_basket_members_request::scope},
        utility::codec::field{"offset", &list_by_trade_id_cbo_basket_members_request::offset},
        utility::codec::field{"limit", &list_by_trade_id_cbo_basket_members_request::limit},
        utility::codec::field{"order", &list_by_trade_id_cbo_basket_members_request::order},
        utility::codec::field{"filter", &list_by_trade_id_cbo_basket_members_request::filter},
    };
}

/**
 * @brief The members of list_by_trade_id_cbo_basket_members_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_trade_id_cbo_basket_members_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_by_trade_id_cbo_basket_members_response::result},
        utility::codec::field{"cbo_basket_members",
                              &list_by_trade_id_cbo_basket_members_response::cbo_basket_members},
        utility::codec::field{"total", &list_by_trade_id_cbo_basket_members_response::total},
    };
}

/**
 * @brief The members of list_cbo_basket_member_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_cbo_basket_member_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_cbo_basket_member_versions_request::key},
        utility::codec::field{"offset", &list_cbo_basket_member_versions_request::offset},
        utility::codec::field{"limit", &list_cbo_basket_member_versions_request::limit},
        utility::codec::field{"order", &list_cbo_basket_member_versions_request::order},
        utility::codec::field{"filter", &list_cbo_basket_member_versions_request::filter},
    };
}

/**
 * @brief The members of list_cbo_basket_member_versions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_cbo_basket_member_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_cbo_basket_member_versions_response::result},
        utility::codec::field{"versions", &list_cbo_basket_member_versions_response::versions},
        utility::codec::field{"total", &list_cbo_basket_member_versions_response::total},
    };
}

/**
 * @brief The members of get_cbo_basket_member_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_cbo_basket_member_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_cbo_basket_member_version_request::key},
    };
}

/**
 * @brief The members of get_cbo_basket_member_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_cbo_basket_member_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_cbo_basket_member_version_response::result},
        utility::codec::field{"version", &get_cbo_basket_member_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::cbo_basket_member_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::cbo_basket_member_key& v);
    static ores::trading::messaging::cbo_basket_member_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::cbo_basket_member_key& v);
    static ores::trading::messaging::cbo_basket_member_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::cbo_basket_member_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::cbo_basket_member_write& v);
    static ores::trading::messaging::cbo_basket_member_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::cbo_basket_member_write& v);
    static ores::trading::messaging::cbo_basket_member_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::cbo_basket_member_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::cbo_basket_member_change& v);
    static ores::trading::messaging::cbo_basket_member_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::cbo_basket_member_change& v);
    static ores::trading::messaging::cbo_basket_member_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::cbo_basket_member_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::cbo_basket_member_removal& v);
    static ores::trading::messaging::cbo_basket_member_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::cbo_basket_member_removal& v);
    static ores::trading::messaging::cbo_basket_member_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::cbo_basket_member_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::cbo_basket_member_lookup& v);
    static ores::trading::messaging::cbo_basket_member_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::cbo_basket_member_lookup& v);
    static ores::trading::messaging::cbo_basket_member_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::cbo_basket_members_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::cbo_basket_members_filter& v);
    static ores::trading::messaging::cbo_basket_members_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::cbo_basket_members_filter& v);
    static ores::trading::messaging::cbo_basket_members_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::cbo_basket_member_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::cbo_basket_member_event& v);
    static ores::trading::messaging::cbo_basket_member_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::cbo_basket_member_event& v);
    static ores::trading::messaging::cbo_basket_member_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::cbo_basket_member_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::cbo_basket_member_version_key& v);
    static ores::trading::messaging::cbo_basket_member_version_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::cbo_basket_member_version_key& v);
    static ores::trading::messaging::cbo_basket_member_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::cbo_basket_member_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::cbo_basket_member_versions_filter& v);
    static ores::trading::messaging::cbo_basket_member_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::cbo_basket_member_versions_filter& v);
    static ores::trading::messaging::cbo_basket_member_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_cbo_basket_members_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_cbo_basket_members_request& v);
    static ores::trading::messaging::list_cbo_basket_members_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_cbo_basket_members_request& v);
    static ores::trading::messaging::list_cbo_basket_members_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_cbo_basket_members_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_cbo_basket_members_response& v);
    static ores::trading::messaging::list_cbo_basket_members_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_cbo_basket_members_response& v);
    static ores::trading::messaging::list_cbo_basket_members_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_cbo_basket_member_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_cbo_basket_member_request& v);
    static ores::trading::messaging::get_cbo_basket_member_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_cbo_basket_member_request& v);
    static ores::trading::messaging::get_cbo_basket_member_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_cbo_basket_member_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_cbo_basket_member_response& v);
    static ores::trading::messaging::get_cbo_basket_member_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_cbo_basket_member_response& v);
    static ores::trading::messaging::get_cbo_basket_member_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_cbo_basket_members_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_cbo_basket_members_request& v);
    static ores::trading::messaging::get_many_cbo_basket_members_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_cbo_basket_members_request& v);
    static ores::trading::messaging::get_many_cbo_basket_members_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_cbo_basket_members_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_cbo_basket_members_response& v);
    static ores::trading::messaging::get_many_cbo_basket_members_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_cbo_basket_members_response& v);
    static ores::trading::messaging::get_many_cbo_basket_members_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_cbo_basket_member_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_cbo_basket_member_request& v);
    static ores::trading::messaging::put_cbo_basket_member_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_cbo_basket_member_request& v);
    static ores::trading::messaging::put_cbo_basket_member_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_cbo_basket_member_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_cbo_basket_member_response& v);
    static ores::trading::messaging::put_cbo_basket_member_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_cbo_basket_member_response& v);
    static ores::trading::messaging::put_cbo_basket_member_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_cbo_basket_members_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_cbo_basket_members_request& v);
    static ores::trading::messaging::put_many_cbo_basket_members_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_cbo_basket_members_request& v);
    static ores::trading::messaging::put_many_cbo_basket_members_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_cbo_basket_members_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_cbo_basket_members_response& v);
    static ores::trading::messaging::put_many_cbo_basket_members_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_cbo_basket_members_response& v);
    static ores::trading::messaging::put_many_cbo_basket_members_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_cbo_basket_member_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_cbo_basket_member_request& v);
    static ores::trading::messaging::delete_cbo_basket_member_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_cbo_basket_member_request& v);
    static ores::trading::messaging::delete_cbo_basket_member_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_cbo_basket_member_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_cbo_basket_member_response& v);
    static ores::trading::messaging::delete_cbo_basket_member_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_cbo_basket_member_response& v);
    static ores::trading::messaging::delete_cbo_basket_member_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_cbo_basket_members_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_cbo_basket_members_request& v);
    static ores::trading::messaging::delete_many_cbo_basket_members_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_cbo_basket_members_request& v);
    static ores::trading::messaging::delete_many_cbo_basket_members_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_cbo_basket_members_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_cbo_basket_members_response& v);
    static ores::trading::messaging::delete_many_cbo_basket_members_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_cbo_basket_members_response& v);
    static ores::trading::messaging::delete_many_cbo_basket_members_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_cbo_basket_members_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_by_trade_id_cbo_basket_members_request& v);
    static ores::trading::messaging::list_by_trade_id_cbo_basket_members_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_by_trade_id_cbo_basket_members_request& v);
    static ores::trading::messaging::list_by_trade_id_cbo_basket_members_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_cbo_basket_members_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_by_trade_id_cbo_basket_members_response& v);
    static ores::trading::messaging::list_by_trade_id_cbo_basket_members_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_by_trade_id_cbo_basket_members_response& v);
    static ores::trading::messaging::list_by_trade_id_cbo_basket_members_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_cbo_basket_member_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_cbo_basket_member_versions_request& v);
    static ores::trading::messaging::list_cbo_basket_member_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_cbo_basket_member_versions_request& v);
    static ores::trading::messaging::list_cbo_basket_member_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_cbo_basket_member_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_cbo_basket_member_versions_response& v);
    static ores::trading::messaging::list_cbo_basket_member_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_cbo_basket_member_versions_response& v);
    static ores::trading::messaging::list_cbo_basket_member_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_cbo_basket_member_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_cbo_basket_member_version_request& v);
    static ores::trading::messaging::get_cbo_basket_member_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_cbo_basket_member_version_request& v);
    static ores::trading::messaging::get_cbo_basket_member_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_cbo_basket_member_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_cbo_basket_member_version_response& v);
    static ores::trading::messaging::get_cbo_basket_member_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_cbo_basket_member_version_response& v);
    static ores::trading::messaging::get_cbo_basket_member_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
