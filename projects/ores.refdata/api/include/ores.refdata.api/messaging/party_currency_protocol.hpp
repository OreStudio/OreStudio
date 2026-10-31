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
#ifndef ORES_REFDATA_API_MESSAGING_PARTY_CURRENCY_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_PARTY_CURRENCY_PROTOCOL_HPP

#include "ores.refdata.api/domain/party_currency.hpp"
#include "ores.refdata.api/export.hpp"
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

namespace ores::refdata::messaging {

struct party_currency_key {
    boost::uuids::uuid party_id;
    std::string currency_iso_code;
};

struct party_currency_write {
    boost::uuids::uuid party_id;
    std::string currency_iso_code;
};

struct party_currency_change {
    party_currency_write write;
    ores::utility::domain::precondition precondition;
};

struct party_currency_removal {
    party_currency_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct party_currency_lookup {
    party_currency_key key;
    std::optional<ores::refdata::domain::party_currency> party_currency;
};

struct party_currencies_filter {
    std::optional<boost::uuids::uuid> party_id;
    std::optional<std::vector<boost::uuids::uuid>> party_id_one_of;
};

struct party_currency_event {
    boost::uuids::uuid event_id;
    party_currency_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_party_currencies_request {
    using response_type = struct list_party_currencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_currencies.list";
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
    std::optional<party_currencies_filter> filter;
};

struct list_party_currencies_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::party_currency> party_currencies;
    std::uint64_t total;
};

struct get_party_currency_request {
    using response_type = struct get_party_currency_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_currencies.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    party_currency_key key;
};

struct get_party_currency_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::party_currency> party_currency;
};

struct get_many_party_currencies_request {
    using response_type = struct get_many_party_currencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_currencies.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<party_currency_key> keys;
};

struct get_many_party_currencies_response {
    ores::utility::domain::result result;
    std::vector<party_currency_lookup> entries;
};

struct put_party_currency_request {
    using response_type = struct put_party_currency_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_currencies.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    party_currency_change change;
    ores::utility::domain::change_intent intent;
};

struct put_party_currency_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::party_currency> party_currency;
};

struct put_many_party_currencies_request {
    using response_type = struct put_many_party_currencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_currencies.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<party_currency_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_party_currencies_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::party_currency> party_currencies;
};

struct delete_party_currency_request {
    using response_type = struct delete_party_currency_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_currencies.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    party_currency_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_party_currency_response {
    ores::utility::domain::result result;
};

struct delete_many_party_currencies_request {
    using response_type = struct delete_many_party_currencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_currencies.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<party_currency_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_party_currencies_response {
    ores::utility::domain::result result;
};

struct list_by_party_id_party_currencies_request {
    using response_type = struct list_by_party_id_party_currencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_currencies.list_by_party_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid party_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<party_currencies_filter> filter;
};

struct list_by_party_id_party_currencies_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::party_currency> party_currencies;
    std::uint64_t total;
};

/**
 * @brief The members of party_currency_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_currency_key&) {
    return std::tuple{
        utility::codec::field{"party_id", &party_currency_key::party_id},
        utility::codec::field{"currency_iso_code", &party_currency_key::currency_iso_code},
    };
}

/**
 * @brief The members of party_currency_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_currency_write&) {
    return std::tuple{
        utility::codec::field{"party_id", &party_currency_write::party_id},
        utility::codec::field{"currency_iso_code", &party_currency_write::currency_iso_code},
    };
}

/**
 * @brief The members of party_currency_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_currency_change&) {
    return std::tuple{
        utility::codec::field{"write", &party_currency_change::write},
        utility::codec::field{"precondition", &party_currency_change::precondition},
    };
}

/**
 * @brief The members of party_currency_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_currency_removal&) {
    return std::tuple{
        utility::codec::field{"key", &party_currency_removal::key},
        utility::codec::field{"precondition", &party_currency_removal::precondition},
    };
}

/**
 * @brief The members of party_currency_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_currency_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &party_currency_lookup::key},
        utility::codec::field{"party_currency", &party_currency_lookup::party_currency},
    };
}

/**
 * @brief The members of party_currencies_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_currencies_filter&) {
    return std::tuple{
        utility::codec::field{"party_id", &party_currencies_filter::party_id},
        utility::codec::field{"party_id_one_of", &party_currencies_filter::party_id_one_of},
    };
}

/**
 * @brief The members of party_currency_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_currency_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &party_currency_event::event_id},
        utility::codec::field{"key", &party_currency_event::key},
        utility::codec::field{"action", &party_currency_event::action},
        utility::codec::field{"version", &party_currency_event::version},
        utility::codec::field{"occurred_at", &party_currency_event::occurred_at},
        utility::codec::field{"correlation_id", &party_currency_event::correlation_id},
    };
}

/**
 * @brief The members of list_party_currencies_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_party_currencies_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_party_currencies_request::offset},
        utility::codec::field{"limit", &list_party_currencies_request::limit},
        utility::codec::field{"order", &list_party_currencies_request::order},
        utility::codec::field{"filter", &list_party_currencies_request::filter},
    };
}

/**
 * @brief The members of list_party_currencies_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_party_currencies_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_party_currencies_response::result},
        utility::codec::field{"party_currencies",
                              &list_party_currencies_response::party_currencies},
        utility::codec::field{"total", &list_party_currencies_response::total},
    };
}

/**
 * @brief The members of get_party_currency_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_party_currency_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_party_currency_request::key},
    };
}

/**
 * @brief The members of get_party_currency_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_party_currency_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_party_currency_response::result},
        utility::codec::field{"party_currency", &get_party_currency_response::party_currency},
    };
}

/**
 * @brief The members of get_many_party_currencies_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_party_currencies_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_party_currencies_request::keys},
    };
}

/**
 * @brief The members of get_many_party_currencies_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_party_currencies_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_party_currencies_response::result},
        utility::codec::field{"entries", &get_many_party_currencies_response::entries},
    };
}

/**
 * @brief The members of put_party_currency_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_party_currency_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_party_currency_request::change},
        utility::codec::field{"intent", &put_party_currency_request::intent},
    };
}

/**
 * @brief The members of put_party_currency_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_party_currency_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_party_currency_response::result},
        utility::codec::field{"party_currency", &put_party_currency_response::party_currency},
    };
}

/**
 * @brief The members of put_many_party_currencies_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_party_currencies_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_party_currencies_request::changes},
        utility::codec::field{"intent", &put_many_party_currencies_request::intent},
    };
}

/**
 * @brief The members of put_many_party_currencies_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_party_currencies_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_party_currencies_response::result},
        utility::codec::field{"party_currencies",
                              &put_many_party_currencies_response::party_currencies},
    };
}

/**
 * @brief The members of delete_party_currency_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_party_currency_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_party_currency_request::removal},
        utility::codec::field{"intent", &delete_party_currency_request::intent},
    };
}

/**
 * @brief The members of delete_party_currency_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_party_currency_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_party_currency_response::result},
    };
}

/**
 * @brief The members of delete_many_party_currencies_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_party_currencies_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_party_currencies_request::removals},
        utility::codec::field{"intent", &delete_many_party_currencies_request::intent},
    };
}

/**
 * @brief The members of delete_many_party_currencies_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_party_currencies_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_party_currencies_response::result},
    };
}

/**
 * @brief The members of list_by_party_id_party_currencies_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_party_id_party_currencies_request&) {
    return std::tuple{
        utility::codec::field{"party_id", &list_by_party_id_party_currencies_request::party_id},
        utility::codec::field{"scope", &list_by_party_id_party_currencies_request::scope},
        utility::codec::field{"offset", &list_by_party_id_party_currencies_request::offset},
        utility::codec::field{"limit", &list_by_party_id_party_currencies_request::limit},
        utility::codec::field{"order", &list_by_party_id_party_currencies_request::order},
        utility::codec::field{"filter", &list_by_party_id_party_currencies_request::filter},
    };
}

/**
 * @brief The members of list_by_party_id_party_currencies_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_party_id_party_currencies_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_by_party_id_party_currencies_response::result},
        utility::codec::field{"party_currencies",
                              &list_by_party_id_party_currencies_response::party_currencies},
        utility::codec::field{"total", &list_by_party_id_party_currencies_response::total},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::party_currency_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::party_currency_key& v);
    static ores::refdata::messaging::party_currency_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::party_currency_key& v);
    static ores::refdata::messaging::party_currency_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::party_currency_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::party_currency_write& v);
    static ores::refdata::messaging::party_currency_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::party_currency_write& v);
    static ores::refdata::messaging::party_currency_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::party_currency_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::party_currency_change& v);
    static ores::refdata::messaging::party_currency_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::party_currency_change& v);
    static ores::refdata::messaging::party_currency_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::party_currency_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::party_currency_removal& v);
    static ores::refdata::messaging::party_currency_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::party_currency_removal& v);
    static ores::refdata::messaging::party_currency_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::party_currency_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::party_currency_lookup& v);
    static ores::refdata::messaging::party_currency_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::party_currency_lookup& v);
    static ores::refdata::messaging::party_currency_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::party_currencies_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::party_currencies_filter& v);
    static ores::refdata::messaging::party_currencies_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::party_currencies_filter& v);
    static ores::refdata::messaging::party_currencies_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::party_currency_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::party_currency_event& v);
    static ores::refdata::messaging::party_currency_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::party_currency_event& v);
    static ores::refdata::messaging::party_currency_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_party_currencies_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_party_currencies_request& v);
    static ores::refdata::messaging::list_party_currencies_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_party_currencies_request& v);
    static ores::refdata::messaging::list_party_currencies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_party_currencies_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_party_currencies_response& v);
    static ores::refdata::messaging::list_party_currencies_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_party_currencies_response& v);
    static ores::refdata::messaging::list_party_currencies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_party_currency_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_party_currency_request& v);
    static ores::refdata::messaging::get_party_currency_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_party_currency_request& v);
    static ores::refdata::messaging::get_party_currency_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_party_currency_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_party_currency_response& v);
    static ores::refdata::messaging::get_party_currency_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_party_currency_response& v);
    static ores::refdata::messaging::get_party_currency_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_party_currencies_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_party_currencies_request& v);
    static ores::refdata::messaging::get_many_party_currencies_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_party_currencies_request& v);
    static ores::refdata::messaging::get_many_party_currencies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_party_currencies_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_party_currencies_response& v);
    static ores::refdata::messaging::get_many_party_currencies_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_party_currencies_response& v);
    static ores::refdata::messaging::get_many_party_currencies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_party_currency_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_party_currency_request& v);
    static ores::refdata::messaging::put_party_currency_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_party_currency_request& v);
    static ores::refdata::messaging::put_party_currency_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_party_currency_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_party_currency_response& v);
    static ores::refdata::messaging::put_party_currency_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_party_currency_response& v);
    static ores::refdata::messaging::put_party_currency_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_party_currencies_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_party_currencies_request& v);
    static ores::refdata::messaging::put_many_party_currencies_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_party_currencies_request& v);
    static ores::refdata::messaging::put_many_party_currencies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_party_currencies_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_party_currencies_response& v);
    static ores::refdata::messaging::put_many_party_currencies_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_party_currencies_response& v);
    static ores::refdata::messaging::put_many_party_currencies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_party_currency_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_party_currency_request& v);
    static ores::refdata::messaging::delete_party_currency_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_party_currency_request& v);
    static ores::refdata::messaging::delete_party_currency_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_party_currency_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_party_currency_response& v);
    static ores::refdata::messaging::delete_party_currency_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_party_currency_response& v);
    static ores::refdata::messaging::delete_party_currency_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_party_currencies_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_party_currencies_request& v);
    static ores::refdata::messaging::delete_many_party_currencies_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_party_currencies_request& v);
    static ores::refdata::messaging::delete_many_party_currencies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_party_currencies_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_party_currencies_response& v);
    static ores::refdata::messaging::delete_many_party_currencies_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_party_currencies_response& v);
    static ores::refdata::messaging::delete_many_party_currencies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_by_party_id_party_currencies_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_by_party_id_party_currencies_request& v);
    static ores::refdata::messaging::list_by_party_id_party_currencies_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_by_party_id_party_currencies_request& v);
    static ores::refdata::messaging::list_by_party_id_party_currencies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_by_party_id_party_currencies_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_by_party_id_party_currencies_response& v);
    static ores::refdata::messaging::list_by_party_id_party_currencies_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_by_party_id_party_currencies_response& v);
    static ores::refdata::messaging::list_by_party_id_party_currencies_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
