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
#ifndef ORES_IAM_API_MESSAGING_ACCOUNT_PARTY_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_ACCOUNT_PARTY_PROTOCOL_HPP

#include "ores.iam.api/domain/account_party.hpp"
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

struct account_party_key {
    boost::uuids::uuid account_id;
    boost::uuids::uuid party_id;
};

struct account_party_write {
    boost::uuids::uuid account_id;
    boost::uuids::uuid party_id;
};

struct account_party_change {
    account_party_write write;
    ores::utility::domain::precondition precondition;
};

struct account_party_removal {
    account_party_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct account_party_lookup {
    account_party_key key;
    std::optional<ores::iam::domain::account_party> account_party;
};

struct account_parties_filter {
    std::optional<boost::uuids::uuid> account_id;
    std::optional<std::vector<boost::uuids::uuid>> account_id_one_of;
};

struct account_party_event {
    boost::uuids::uuid event_id;
    account_party_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_account_parties_request {
    using response_type = struct list_account_parties_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_parties.list";
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
    std::optional<account_parties_filter> filter;
};

struct list_account_parties_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_party> account_parties;
    std::uint64_t total;
};

struct get_account_party_request {
    using response_type = struct get_account_party_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_parties.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_party_key key;
};

struct get_account_party_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::account_party> account_party;
};

struct get_many_account_parties_request {
    using response_type = struct get_many_account_parties_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_parties.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<account_party_key> keys;
};

struct get_many_account_parties_response {
    ores::utility::domain::result result;
    std::vector<account_party_lookup> entries;
};

struct put_account_party_request {
    using response_type = struct put_account_party_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_parties.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_party_change change;
    ores::utility::domain::change_intent intent;
};

struct put_account_party_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::account_party> account_party;
};

struct put_many_account_parties_request {
    using response_type = struct put_many_account_parties_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_parties.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<account_party_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_account_parties_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_party> account_parties;
};

struct delete_account_party_request {
    using response_type = struct delete_account_party_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_parties.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_party_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_account_party_response {
    ores::utility::domain::result result;
};

struct delete_many_account_parties_request {
    using response_type = struct delete_many_account_parties_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_parties.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<account_party_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_account_parties_response {
    ores::utility::domain::result result;
};

struct list_by_account_id_account_parties_request {
    using response_type = struct list_by_account_id_account_parties_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_parties.list_by_account_id";
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
    std::optional<account_parties_filter> filter;
};

struct list_by_account_id_account_parties_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_party> account_parties;
    std::uint64_t total;
};

/**
 * @brief The members of account_party_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const account_party_key&) {
    return std::tuple{
        utility::codec::field{"account_id", &account_party_key::account_id},
        utility::codec::field{"party_id", &account_party_key::party_id},
    };
}

/**
 * @brief The members of account_party_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const account_party_write&) {
    return std::tuple{
        utility::codec::field{"account_id", &account_party_write::account_id},
        utility::codec::field{"party_id", &account_party_write::party_id},
    };
}

/**
 * @brief The members of account_party_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const account_party_change&) {
    return std::tuple{
        utility::codec::field{"write", &account_party_change::write},
        utility::codec::field{"precondition", &account_party_change::precondition},
    };
}

/**
 * @brief The members of account_party_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const account_party_removal&) {
    return std::tuple{
        utility::codec::field{"key", &account_party_removal::key},
        utility::codec::field{"precondition", &account_party_removal::precondition},
    };
}

/**
 * @brief The members of account_party_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const account_party_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &account_party_lookup::key},
        utility::codec::field{"account_party", &account_party_lookup::account_party},
    };
}

/**
 * @brief The members of account_parties_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const account_parties_filter&) {
    return std::tuple{
        utility::codec::field{"account_id", &account_parties_filter::account_id},
        utility::codec::field{"account_id_one_of", &account_parties_filter::account_id_one_of},
    };
}

/**
 * @brief The members of account_party_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const account_party_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &account_party_event::event_id},
        utility::codec::field{"key", &account_party_event::key},
        utility::codec::field{"action", &account_party_event::action},
        utility::codec::field{"version", &account_party_event::version},
        utility::codec::field{"occurred_at", &account_party_event::occurred_at},
        utility::codec::field{"correlation_id", &account_party_event::correlation_id},
    };
}

/**
 * @brief The members of list_account_parties_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_account_parties_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_account_parties_request::offset},
        utility::codec::field{"limit", &list_account_parties_request::limit},
        utility::codec::field{"order", &list_account_parties_request::order},
        utility::codec::field{"filter", &list_account_parties_request::filter},
    };
}

/**
 * @brief The members of list_account_parties_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_account_parties_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_account_parties_response::result},
        utility::codec::field{"account_parties", &list_account_parties_response::account_parties},
        utility::codec::field{"total", &list_account_parties_response::total},
    };
}

/**
 * @brief The members of get_account_party_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_account_party_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_account_party_request::key},
    };
}

/**
 * @brief The members of get_account_party_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_account_party_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_account_party_response::result},
        utility::codec::field{"account_party", &get_account_party_response::account_party},
    };
}

/**
 * @brief The members of get_many_account_parties_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_account_parties_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_account_parties_request::keys},
    };
}

/**
 * @brief The members of get_many_account_parties_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_account_parties_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_account_parties_response::result},
        utility::codec::field{"entries", &get_many_account_parties_response::entries},
    };
}

/**
 * @brief The members of put_account_party_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_account_party_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_account_party_request::change},
        utility::codec::field{"intent", &put_account_party_request::intent},
    };
}

/**
 * @brief The members of put_account_party_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_account_party_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_account_party_response::result},
        utility::codec::field{"account_party", &put_account_party_response::account_party},
    };
}

/**
 * @brief The members of put_many_account_parties_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_account_parties_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_account_parties_request::changes},
        utility::codec::field{"intent", &put_many_account_parties_request::intent},
    };
}

/**
 * @brief The members of put_many_account_parties_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_account_parties_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_account_parties_response::result},
        utility::codec::field{"account_parties",
                              &put_many_account_parties_response::account_parties},
    };
}

/**
 * @brief The members of delete_account_party_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_account_party_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_account_party_request::removal},
        utility::codec::field{"intent", &delete_account_party_request::intent},
    };
}

/**
 * @brief The members of delete_account_party_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_account_party_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_account_party_response::result},
    };
}

/**
 * @brief The members of delete_many_account_parties_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_account_parties_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_account_parties_request::removals},
        utility::codec::field{"intent", &delete_many_account_parties_request::intent},
    };
}

/**
 * @brief The members of delete_many_account_parties_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_account_parties_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_account_parties_response::result},
    };
}

/**
 * @brief The members of list_by_account_id_account_parties_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_account_id_account_parties_request&) {
    return std::tuple{
        utility::codec::field{"account_id",
                              &list_by_account_id_account_parties_request::account_id},
        utility::codec::field{"scope", &list_by_account_id_account_parties_request::scope},
        utility::codec::field{"offset", &list_by_account_id_account_parties_request::offset},
        utility::codec::field{"limit", &list_by_account_id_account_parties_request::limit},
        utility::codec::field{"order", &list_by_account_id_account_parties_request::order},
        utility::codec::field{"filter", &list_by_account_id_account_parties_request::filter},
    };
}

/**
 * @brief The members of list_by_account_id_account_parties_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_account_id_account_parties_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_by_account_id_account_parties_response::result},
        utility::codec::field{"account_parties",
                              &list_by_account_id_account_parties_response::account_parties},
        utility::codec::field{"total", &list_by_account_id_account_parties_response::total},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_party_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::account_party_key& v);
    static ores::iam::messaging::account_party_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::account_party_key& v);
    static ores::iam::messaging::account_party_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_party_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::account_party_write& v);
    static ores::iam::messaging::account_party_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::account_party_write& v);
    static ores::iam::messaging::account_party_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_party_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::account_party_change& v);
    static ores::iam::messaging::account_party_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::account_party_change& v);
    static ores::iam::messaging::account_party_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_party_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::account_party_removal& v);
    static ores::iam::messaging::account_party_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::account_party_removal& v);
    static ores::iam::messaging::account_party_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_party_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::account_party_lookup& v);
    static ores::iam::messaging::account_party_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::account_party_lookup& v);
    static ores::iam::messaging::account_party_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_parties_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::account_parties_filter& v);
    static ores::iam::messaging::account_parties_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::account_parties_filter& v);
    static ores::iam::messaging::account_parties_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_party_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::account_party_event& v);
    static ores::iam::messaging::account_party_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::account_party_event& v);
    static ores::iam::messaging::account_party_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_account_parties_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_account_parties_request& v);
    static ores::iam::messaging::list_account_parties_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_account_parties_request& v);
    static ores::iam::messaging::list_account_parties_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_account_parties_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_account_parties_response& v);
    static ores::iam::messaging::list_account_parties_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_account_parties_response& v);
    static ores::iam::messaging::list_account_parties_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_account_party_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_account_party_request& v);
    static ores::iam::messaging::get_account_party_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_account_party_request& v);
    static ores::iam::messaging::get_account_party_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_account_party_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_account_party_response& v);
    static ores::iam::messaging::get_account_party_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_account_party_response& v);
    static ores::iam::messaging::get_account_party_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_account_parties_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_many_account_parties_request& v);
    static ores::iam::messaging::get_many_account_parties_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_many_account_parties_request& v);
    static ores::iam::messaging::get_many_account_parties_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_account_parties_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_many_account_parties_response& v);
    static ores::iam::messaging::get_many_account_parties_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_many_account_parties_response& v);
    static ores::iam::messaging::get_many_account_parties_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_account_party_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_account_party_request& v);
    static ores::iam::messaging::put_account_party_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_account_party_request& v);
    static ores::iam::messaging::put_account_party_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_account_party_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_account_party_response& v);
    static ores::iam::messaging::put_account_party_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_account_party_response& v);
    static ores::iam::messaging::put_account_party_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_many_account_parties_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_many_account_parties_request& v);
    static ores::iam::messaging::put_many_account_parties_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::put_many_account_parties_request& v);
    static ores::iam::messaging::put_many_account_parties_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_many_account_parties_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_many_account_parties_response& v);
    static ores::iam::messaging::put_many_account_parties_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::put_many_account_parties_response& v);
    static ores::iam::messaging::put_many_account_parties_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_account_party_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_account_party_request& v);
    static ores::iam::messaging::delete_account_party_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::delete_account_party_request& v);
    static ores::iam::messaging::delete_account_party_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_account_party_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_account_party_response& v);
    static ores::iam::messaging::delete_account_party_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::delete_account_party_response& v);
    static ores::iam::messaging::delete_account_party_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_many_account_parties_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::delete_many_account_parties_request& v);
    static ores::iam::messaging::delete_many_account_parties_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_many_account_parties_request& v);
    static ores::iam::messaging::delete_many_account_parties_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_many_account_parties_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::delete_many_account_parties_response& v);
    static ores::iam::messaging::delete_many_account_parties_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_many_account_parties_response& v);
    static ores::iam::messaging::delete_many_account_parties_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_account_id_account_parties_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_by_account_id_account_parties_request& v);
    static ores::iam::messaging::list_by_account_id_account_parties_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_by_account_id_account_parties_request& v);
    static ores::iam::messaging::list_by_account_id_account_parties_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_account_id_account_parties_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_by_account_id_account_parties_response& v);
    static ores::iam::messaging::list_by_account_id_account_parties_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_by_account_id_account_parties_response& v);
    static ores::iam::messaging::list_by_account_id_account_parties_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
