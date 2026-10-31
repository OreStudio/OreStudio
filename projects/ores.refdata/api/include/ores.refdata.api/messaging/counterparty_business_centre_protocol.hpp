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
#ifndef ORES_REFDATA_API_MESSAGING_COUNTERPARTY_BUSINESS_CENTRE_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_COUNTERPARTY_BUSINESS_CENTRE_PROTOCOL_HPP

#include "ores.refdata.api/domain/counterparty_business_centre.hpp"
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

struct counterparty_business_centre_key {
    boost::uuids::uuid counterparty_id;
    std::string business_centre_code;
};

struct counterparty_business_centre_write {
    boost::uuids::uuid counterparty_id;
    std::string business_centre_code;
};

struct counterparty_business_centre_change {
    counterparty_business_centre_write write;
    ores::utility::domain::precondition precondition;
};

struct counterparty_business_centre_removal {
    counterparty_business_centre_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct counterparty_business_centre_lookup {
    counterparty_business_centre_key key;
    std::optional<ores::refdata::domain::counterparty_business_centre> counterparty_business_centre;
};

struct counterparty_business_centres_filter {
    std::optional<boost::uuids::uuid> counterparty_id;
    std::optional<std::vector<boost::uuids::uuid>> counterparty_id_one_of;
};

struct counterparty_business_centre_event {
    boost::uuids::uuid event_id;
    counterparty_business_centre_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_counterparty_business_centres_request {
    using response_type = struct list_counterparty_business_centres_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_business_centres.list";
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
    std::optional<counterparty_business_centres_filter> filter;
};

struct list_counterparty_business_centres_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::counterparty_business_centre> counterparty_business_centres;
    std::uint64_t total;
};

struct get_counterparty_business_centre_request {
    using response_type = struct get_counterparty_business_centre_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparty_business_centres.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_business_centre_key key;
};

struct get_counterparty_business_centre_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::counterparty_business_centre> counterparty_business_centre;
};

struct get_many_counterparty_business_centres_request {
    using response_type = struct get_many_counterparty_business_centres_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_business_centres.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<counterparty_business_centre_key> keys;
};

struct get_many_counterparty_business_centres_response {
    ores::utility::domain::result result;
    std::vector<counterparty_business_centre_lookup> entries;
};

struct put_counterparty_business_centre_request {
    using response_type = struct put_counterparty_business_centre_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparty_business_centres.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_business_centre_change change;
    ores::utility::domain::change_intent intent;
};

struct put_counterparty_business_centre_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::counterparty_business_centre> counterparty_business_centre;
};

struct put_many_counterparty_business_centres_request {
    using response_type = struct put_many_counterparty_business_centres_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_business_centres.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<counterparty_business_centre_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_counterparty_business_centres_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::counterparty_business_centre> counterparty_business_centres;
};

struct delete_counterparty_business_centre_request {
    using response_type = struct delete_counterparty_business_centre_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_business_centres.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_business_centre_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_counterparty_business_centre_response {
    ores::utility::domain::result result;
};

struct delete_many_counterparty_business_centres_request {
    using response_type = struct delete_many_counterparty_business_centres_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_business_centres.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<counterparty_business_centre_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_counterparty_business_centres_response {
    ores::utility::domain::result result;
};

struct list_by_counterparty_id_counterparty_business_centres_request {
    using response_type = struct list_by_counterparty_id_counterparty_business_centres_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_business_centres.list_by_counterparty_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid counterparty_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<counterparty_business_centres_filter> filter;
};

struct list_by_counterparty_id_counterparty_business_centres_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::counterparty_business_centre> counterparty_business_centres;
    std::uint64_t total;
};

/**
 * @brief The members of counterparty_business_centre_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_business_centre_key&) {
    return std::tuple{
        utility::codec::field{"counterparty_id",
                              &counterparty_business_centre_key::counterparty_id},
        utility::codec::field{"business_centre_code",
                              &counterparty_business_centre_key::business_centre_code},
    };
}

/**
 * @brief The members of counterparty_business_centre_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_business_centre_write&) {
    return std::tuple{
        utility::codec::field{"counterparty_id",
                              &counterparty_business_centre_write::counterparty_id},
        utility::codec::field{"business_centre_code",
                              &counterparty_business_centre_write::business_centre_code},
    };
}

/**
 * @brief The members of counterparty_business_centre_change, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_business_centre_change&) {
    return std::tuple{
        utility::codec::field{"write", &counterparty_business_centre_change::write},
        utility::codec::field{"precondition", &counterparty_business_centre_change::precondition},
    };
}

/**
 * @brief The members of counterparty_business_centre_removal, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_business_centre_removal&) {
    return std::tuple{
        utility::codec::field{"key", &counterparty_business_centre_removal::key},
        utility::codec::field{"precondition", &counterparty_business_centre_removal::precondition},
    };
}

/**
 * @brief The members of counterparty_business_centre_lookup, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_business_centre_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &counterparty_business_centre_lookup::key},
        utility::codec::field{"counterparty_business_centre",
                              &counterparty_business_centre_lookup::counterparty_business_centre},
    };
}

/**
 * @brief The members of counterparty_business_centres_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_business_centres_filter&) {
    return std::tuple{
        utility::codec::field{"counterparty_id",
                              &counterparty_business_centres_filter::counterparty_id},
        utility::codec::field{"counterparty_id_one_of",
                              &counterparty_business_centres_filter::counterparty_id_one_of},
    };
}

/**
 * @brief The members of counterparty_business_centre_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_business_centre_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &counterparty_business_centre_event::event_id},
        utility::codec::field{"key", &counterparty_business_centre_event::key},
        utility::codec::field{"action", &counterparty_business_centre_event::action},
        utility::codec::field{"version", &counterparty_business_centre_event::version},
        utility::codec::field{"occurred_at", &counterparty_business_centre_event::occurred_at},
        utility::codec::field{"correlation_id",
                              &counterparty_business_centre_event::correlation_id},
    };
}

/**
 * @brief The members of list_counterparty_business_centres_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_counterparty_business_centres_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_counterparty_business_centres_request::offset},
        utility::codec::field{"limit", &list_counterparty_business_centres_request::limit},
        utility::codec::field{"order", &list_counterparty_business_centres_request::order},
        utility::codec::field{"filter", &list_counterparty_business_centres_request::filter},
    };
}

/**
 * @brief The members of list_counterparty_business_centres_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_counterparty_business_centres_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_counterparty_business_centres_response::result},
        utility::codec::field{
            "counterparty_business_centres",
            &list_counterparty_business_centres_response::counterparty_business_centres},
        utility::codec::field{"total", &list_counterparty_business_centres_response::total},
    };
}

/**
 * @brief The members of get_counterparty_business_centre_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_counterparty_business_centre_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_counterparty_business_centre_request::key},
    };
}

/**
 * @brief The members of get_counterparty_business_centre_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_counterparty_business_centre_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_counterparty_business_centre_response::result},
        utility::codec::field{
            "counterparty_business_centre",
            &get_counterparty_business_centre_response::counterparty_business_centre},
    };
}

/**
 * @brief The members of get_many_counterparty_business_centres_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_counterparty_business_centres_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_counterparty_business_centres_request::keys},
    };
}

/**
 * @brief The members of get_many_counterparty_business_centres_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_counterparty_business_centres_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_counterparty_business_centres_response::result},
        utility::codec::field{"entries", &get_many_counterparty_business_centres_response::entries},
    };
}

/**
 * @brief The members of put_counterparty_business_centre_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_counterparty_business_centre_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_counterparty_business_centre_request::change},
        utility::codec::field{"intent", &put_counterparty_business_centre_request::intent},
    };
}

/**
 * @brief The members of put_counterparty_business_centre_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_counterparty_business_centre_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_counterparty_business_centre_response::result},
        utility::codec::field{
            "counterparty_business_centre",
            &put_counterparty_business_centre_response::counterparty_business_centre},
    };
}

/**
 * @brief The members of put_many_counterparty_business_centres_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_counterparty_business_centres_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_counterparty_business_centres_request::changes},
        utility::codec::field{"intent", &put_many_counterparty_business_centres_request::intent},
    };
}

/**
 * @brief The members of put_many_counterparty_business_centres_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_counterparty_business_centres_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_counterparty_business_centres_response::result},
        utility::codec::field{
            "counterparty_business_centres",
            &put_many_counterparty_business_centres_response::counterparty_business_centres},
    };
}

/**
 * @brief The members of delete_counterparty_business_centre_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_counterparty_business_centre_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_counterparty_business_centre_request::removal},
        utility::codec::field{"intent", &delete_counterparty_business_centre_request::intent},
    };
}

/**
 * @brief The members of delete_counterparty_business_centre_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_counterparty_business_centre_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_counterparty_business_centre_response::result},
    };
}

/**
 * @brief The members of delete_many_counterparty_business_centres_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_counterparty_business_centres_request&) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_counterparty_business_centres_request::removals},
        utility::codec::field{"intent", &delete_many_counterparty_business_centres_request::intent},
    };
}

/**
 * @brief The members of delete_many_counterparty_business_centres_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const delete_many_counterparty_business_centres_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &delete_many_counterparty_business_centres_response::result},
    };
}

/**
 * @brief The members of list_by_counterparty_id_counterparty_business_centres_request, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_counterparty_id_counterparty_business_centres_request&) {
    return std::tuple{
        utility::codec::field{
            "counterparty_id",
            &list_by_counterparty_id_counterparty_business_centres_request::counterparty_id},
        utility::codec::field{
            "scope", &list_by_counterparty_id_counterparty_business_centres_request::scope},
        utility::codec::field{
            "offset", &list_by_counterparty_id_counterparty_business_centres_request::offset},
        utility::codec::field{
            "limit", &list_by_counterparty_id_counterparty_business_centres_request::limit},
        utility::codec::field{
            "order", &list_by_counterparty_id_counterparty_business_centres_request::order},
        utility::codec::field{
            "filter", &list_by_counterparty_id_counterparty_business_centres_request::filter},
    };
}

/**
 * @brief The members of list_by_counterparty_id_counterparty_business_centres_response, named as
 * the wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_counterparty_id_counterparty_business_centres_response&) {
    return std::tuple{
        utility::codec::field{
            "result", &list_by_counterparty_id_counterparty_business_centres_response::result},
        utility::codec::field{"counterparty_business_centres",
                              &list_by_counterparty_id_counterparty_business_centres_response::
                                  counterparty_business_centres},
        utility::codec::field{
            "total", &list_by_counterparty_id_counterparty_business_centres_response::total},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_business_centre_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_business_centre_key& v);
    static ores::refdata::messaging::counterparty_business_centre_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_business_centre_key& v);
    static ores::refdata::messaging::counterparty_business_centre_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_business_centre_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_business_centre_write& v);
    static ores::refdata::messaging::counterparty_business_centre_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_business_centre_write& v);
    static ores::refdata::messaging::counterparty_business_centre_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_business_centre_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_business_centre_change& v);
    static ores::refdata::messaging::counterparty_business_centre_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_business_centre_change& v);
    static ores::refdata::messaging::counterparty_business_centre_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_business_centre_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_business_centre_removal& v);
    static ores::refdata::messaging::counterparty_business_centre_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_business_centre_removal& v);
    static ores::refdata::messaging::counterparty_business_centre_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_business_centre_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_business_centre_lookup& v);
    static ores::refdata::messaging::counterparty_business_centre_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_business_centre_lookup& v);
    static ores::refdata::messaging::counterparty_business_centre_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_business_centres_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_business_centres_filter& v);
    static ores::refdata::messaging::counterparty_business_centres_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_business_centres_filter& v);
    static ores::refdata::messaging::counterparty_business_centres_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_business_centre_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_business_centre_event& v);
    static ores::refdata::messaging::counterparty_business_centre_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_business_centre_event& v);
    static ores::refdata::messaging::counterparty_business_centre_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_counterparty_business_centres_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_counterparty_business_centres_request& v);
    static ores::refdata::messaging::list_counterparty_business_centres_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_counterparty_business_centres_request& v);
    static ores::refdata::messaging::list_counterparty_business_centres_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_counterparty_business_centres_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_counterparty_business_centres_response& v);
    static ores::refdata::messaging::list_counterparty_business_centres_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_counterparty_business_centres_response& v);
    static ores::refdata::messaging::list_counterparty_business_centres_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_counterparty_business_centre_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_counterparty_business_centre_request& v);
    static ores::refdata::messaging::get_counterparty_business_centre_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_counterparty_business_centre_request& v);
    static ores::refdata::messaging::get_counterparty_business_centre_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_counterparty_business_centre_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_counterparty_business_centre_response& v);
    static ores::refdata::messaging::get_counterparty_business_centre_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_counterparty_business_centre_response& v);
    static ores::refdata::messaging::get_counterparty_business_centre_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_counterparty_business_centres_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_counterparty_business_centres_request& v);
    static ores::refdata::messaging::get_many_counterparty_business_centres_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_counterparty_business_centres_request& v);
    static ores::refdata::messaging::get_many_counterparty_business_centres_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_counterparty_business_centres_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_counterparty_business_centres_response& v);
    static ores::refdata::messaging::get_many_counterparty_business_centres_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_counterparty_business_centres_response& v);
    static ores::refdata::messaging::get_many_counterparty_business_centres_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_counterparty_business_centre_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_counterparty_business_centre_request& v);
    static ores::refdata::messaging::put_counterparty_business_centre_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_counterparty_business_centre_request& v);
    static ores::refdata::messaging::put_counterparty_business_centre_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_counterparty_business_centre_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_counterparty_business_centre_response& v);
    static ores::refdata::messaging::put_counterparty_business_centre_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_counterparty_business_centre_response& v);
    static ores::refdata::messaging::put_counterparty_business_centre_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_counterparty_business_centres_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_counterparty_business_centres_request& v);
    static ores::refdata::messaging::put_many_counterparty_business_centres_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_counterparty_business_centres_request& v);
    static ores::refdata::messaging::put_many_counterparty_business_centres_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_counterparty_business_centres_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_counterparty_business_centres_response& v);
    static ores::refdata::messaging::put_many_counterparty_business_centres_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_counterparty_business_centres_response& v);
    static ores::refdata::messaging::put_many_counterparty_business_centres_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_counterparty_business_centre_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_counterparty_business_centre_request& v);
    static ores::refdata::messaging::delete_counterparty_business_centre_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_counterparty_business_centre_request& v);
    static ores::refdata::messaging::delete_counterparty_business_centre_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_counterparty_business_centre_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_counterparty_business_centre_response& v);
    static ores::refdata::messaging::delete_counterparty_business_centre_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_counterparty_business_centre_response& v);
    static ores::refdata::messaging::delete_counterparty_business_centre_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_counterparty_business_centres_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_counterparty_business_centres_request& v);
    static ores::refdata::messaging::delete_many_counterparty_business_centres_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_counterparty_business_centres_request& v);
    static ores::refdata::messaging::delete_many_counterparty_business_centres_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_counterparty_business_centres_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_counterparty_business_centres_response& v);
    static ores::refdata::messaging::delete_many_counterparty_business_centres_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_counterparty_business_centres_response& v);
    static ores::refdata::messaging::delete_many_counterparty_business_centres_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<
    ores::refdata::messaging::list_by_counterparty_id_counterparty_business_centres_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::
                   list_by_counterparty_id_counterparty_business_centres_request& v);
    static ores::refdata::messaging::list_by_counterparty_id_counterparty_business_centres_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::
                      list_by_counterparty_id_counterparty_business_centres_request& v);
    static ores::refdata::messaging::list_by_counterparty_id_counterparty_business_centres_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<
    ores::refdata::messaging::list_by_counterparty_id_counterparty_business_centres_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::
                   list_by_counterparty_id_counterparty_business_centres_response& v);
    static ores::refdata::messaging::list_by_counterparty_id_counterparty_business_centres_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::
                      list_by_counterparty_id_counterparty_business_centres_response& v);
    static ores::refdata::messaging::list_by_counterparty_id_counterparty_business_centres_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
