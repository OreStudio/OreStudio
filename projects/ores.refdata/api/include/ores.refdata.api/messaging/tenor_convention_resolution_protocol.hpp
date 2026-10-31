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
#ifndef ORES_REFDATA_API_MESSAGING_TENOR_CONVENTION_RESOLUTION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_TENOR_CONVENTION_RESOLUTION_PROTOCOL_HPP

#include "ores.refdata.api/domain/tenor_convention_resolution.hpp"
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

struct tenor_convention_resolution_key {
    std::string convention_code;
    std::string tenor_code;
};

struct tenor_convention_resolution_write {
    std::string convention_code;
    std::string tenor_code;
    std::optional<std::string> anchor_override;
    std::optional<std::string> offset_unit;
    std::optional<int> offset_multiplier;
    std::optional<std::string> schedule_code;
    std::optional<int> schedule_step_count;
};

struct tenor_convention_resolution_change {
    tenor_convention_resolution_write write;
    ores::utility::domain::precondition precondition;
};

struct tenor_convention_resolution_removal {
    tenor_convention_resolution_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct tenor_convention_resolution_lookup {
    tenor_convention_resolution_key key;
    std::optional<ores::refdata::domain::tenor_convention_resolution> tenor_convention_resolution;
};

struct tenor_convention_resolutions_filter {
    std::optional<std::string> convention_code;
    std::optional<std::vector<std::string>> convention_code_one_of;
};

struct tenor_convention_resolution_event {
    boost::uuids::uuid event_id;
    tenor_convention_resolution_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_tenor_convention_resolutions_request {
    using response_type = struct list_tenor_convention_resolutions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_convention_resolutions.list";
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
    std::optional<tenor_convention_resolutions_filter> filter;
};

struct list_tenor_convention_resolutions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::tenor_convention_resolution> tenor_convention_resolutions;
    std::uint64_t total;
};

struct get_tenor_convention_resolution_request {
    using response_type = struct get_tenor_convention_resolution_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_convention_resolutions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_convention_resolution_key key;
};

struct get_tenor_convention_resolution_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::tenor_convention_resolution> tenor_convention_resolution;
};

struct get_many_tenor_convention_resolutions_request {
    using response_type = struct get_many_tenor_convention_resolutions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.tenor_convention_resolutions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tenor_convention_resolution_key> keys;
};

struct get_many_tenor_convention_resolutions_response {
    ores::utility::domain::result result;
    std::vector<tenor_convention_resolution_lookup> entries;
};

struct put_tenor_convention_resolution_request {
    using response_type = struct put_tenor_convention_resolution_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_convention_resolutions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_convention_resolution_change change;
    ores::utility::domain::change_intent intent;
};

struct put_tenor_convention_resolution_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::tenor_convention_resolution> tenor_convention_resolution;
};

struct put_many_tenor_convention_resolutions_request {
    using response_type = struct put_many_tenor_convention_resolutions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.tenor_convention_resolutions.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tenor_convention_resolution_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_tenor_convention_resolutions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::tenor_convention_resolution> tenor_convention_resolutions;
};

struct delete_tenor_convention_resolution_request {
    using response_type = struct delete_tenor_convention_resolution_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.tenor_convention_resolutions.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_convention_resolution_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_tenor_convention_resolution_response {
    ores::utility::domain::result result;
};

struct delete_many_tenor_convention_resolutions_request {
    using response_type = struct delete_many_tenor_convention_resolutions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.tenor_convention_resolutions.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tenor_convention_resolution_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_tenor_convention_resolutions_response {
    ores::utility::domain::result result;
};

struct list_by_convention_code_tenor_convention_resolutions_request {
    using response_type = struct list_by_convention_code_tenor_convention_resolutions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.tenor_convention_resolutions.list_by_convention_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string convention_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<tenor_convention_resolutions_filter> filter;
};

struct list_by_convention_code_tenor_convention_resolutions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::tenor_convention_resolution> tenor_convention_resolutions;
    std::uint64_t total;
};

/**
 * @brief The members of tenor_convention_resolution_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_convention_resolution_key&) {
    return std::tuple{
        utility::codec::field{"convention_code", &tenor_convention_resolution_key::convention_code},
        utility::codec::field{"tenor_code", &tenor_convention_resolution_key::tenor_code},
    };
}

/**
 * @brief The members of tenor_convention_resolution_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_convention_resolution_write&) {
    return std::tuple{
        utility::codec::field{"convention_code",
                              &tenor_convention_resolution_write::convention_code},
        utility::codec::field{"tenor_code", &tenor_convention_resolution_write::tenor_code},
        utility::codec::field{"anchor_override",
                              &tenor_convention_resolution_write::anchor_override},
        utility::codec::field{"offset_unit", &tenor_convention_resolution_write::offset_unit},
        utility::codec::field{"offset_multiplier",
                              &tenor_convention_resolution_write::offset_multiplier},
        utility::codec::field{"schedule_code", &tenor_convention_resolution_write::schedule_code},
        utility::codec::field{"schedule_step_count",
                              &tenor_convention_resolution_write::schedule_step_count},
    };
}

/**
 * @brief The members of tenor_convention_resolution_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_convention_resolution_change&) {
    return std::tuple{
        utility::codec::field{"write", &tenor_convention_resolution_change::write},
        utility::codec::field{"precondition", &tenor_convention_resolution_change::precondition},
    };
}

/**
 * @brief The members of tenor_convention_resolution_removal, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_convention_resolution_removal&) {
    return std::tuple{
        utility::codec::field{"key", &tenor_convention_resolution_removal::key},
        utility::codec::field{"precondition", &tenor_convention_resolution_removal::precondition},
    };
}

/**
 * @brief The members of tenor_convention_resolution_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_convention_resolution_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &tenor_convention_resolution_lookup::key},
        utility::codec::field{"tenor_convention_resolution",
                              &tenor_convention_resolution_lookup::tenor_convention_resolution},
    };
}

/**
 * @brief The members of tenor_convention_resolutions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_convention_resolutions_filter&) {
    return std::tuple{
        utility::codec::field{"convention_code",
                              &tenor_convention_resolutions_filter::convention_code},
        utility::codec::field{"convention_code_one_of",
                              &tenor_convention_resolutions_filter::convention_code_one_of},
    };
}

/**
 * @brief The members of tenor_convention_resolution_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_convention_resolution_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &tenor_convention_resolution_event::event_id},
        utility::codec::field{"key", &tenor_convention_resolution_event::key},
        utility::codec::field{"action", &tenor_convention_resolution_event::action},
        utility::codec::field{"version", &tenor_convention_resolution_event::version},
        utility::codec::field{"occurred_at", &tenor_convention_resolution_event::occurred_at},
        utility::codec::field{"correlation_id", &tenor_convention_resolution_event::correlation_id},
    };
}

/**
 * @brief The members of list_tenor_convention_resolutions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_tenor_convention_resolutions_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_tenor_convention_resolutions_request::offset},
        utility::codec::field{"limit", &list_tenor_convention_resolutions_request::limit},
        utility::codec::field{"order", &list_tenor_convention_resolutions_request::order},
        utility::codec::field{"filter", &list_tenor_convention_resolutions_request::filter},
    };
}

/**
 * @brief The members of list_tenor_convention_resolutions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_tenor_convention_resolutions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_tenor_convention_resolutions_response::result},
        utility::codec::field{
            "tenor_convention_resolutions",
            &list_tenor_convention_resolutions_response::tenor_convention_resolutions},
        utility::codec::field{"total", &list_tenor_convention_resolutions_response::total},
    };
}

/**
 * @brief The members of get_tenor_convention_resolution_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_tenor_convention_resolution_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_tenor_convention_resolution_request::key},
    };
}

/**
 * @brief The members of get_tenor_convention_resolution_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_tenor_convention_resolution_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_tenor_convention_resolution_response::result},
        utility::codec::field{
            "tenor_convention_resolution",
            &get_tenor_convention_resolution_response::tenor_convention_resolution},
    };
}

/**
 * @brief The members of get_many_tenor_convention_resolutions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_tenor_convention_resolutions_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_tenor_convention_resolutions_request::keys},
    };
}

/**
 * @brief The members of get_many_tenor_convention_resolutions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_tenor_convention_resolutions_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_tenor_convention_resolutions_response::result},
        utility::codec::field{"entries", &get_many_tenor_convention_resolutions_response::entries},
    };
}

/**
 * @brief The members of put_tenor_convention_resolution_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_tenor_convention_resolution_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_tenor_convention_resolution_request::change},
        utility::codec::field{"intent", &put_tenor_convention_resolution_request::intent},
    };
}

/**
 * @brief The members of put_tenor_convention_resolution_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_tenor_convention_resolution_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_tenor_convention_resolution_response::result},
        utility::codec::field{
            "tenor_convention_resolution",
            &put_tenor_convention_resolution_response::tenor_convention_resolution},
    };
}

/**
 * @brief The members of put_many_tenor_convention_resolutions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_tenor_convention_resolutions_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_tenor_convention_resolutions_request::changes},
        utility::codec::field{"intent", &put_many_tenor_convention_resolutions_request::intent},
    };
}

/**
 * @brief The members of put_many_tenor_convention_resolutions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_tenor_convention_resolutions_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_tenor_convention_resolutions_response::result},
        utility::codec::field{
            "tenor_convention_resolutions",
            &put_many_tenor_convention_resolutions_response::tenor_convention_resolutions},
    };
}

/**
 * @brief The members of delete_tenor_convention_resolution_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_tenor_convention_resolution_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_tenor_convention_resolution_request::removal},
        utility::codec::field{"intent", &delete_tenor_convention_resolution_request::intent},
    };
}

/**
 * @brief The members of delete_tenor_convention_resolution_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_tenor_convention_resolution_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_tenor_convention_resolution_response::result},
    };
}

/**
 * @brief The members of delete_many_tenor_convention_resolutions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_tenor_convention_resolutions_request&) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_tenor_convention_resolutions_request::removals},
        utility::codec::field{"intent", &delete_many_tenor_convention_resolutions_request::intent},
    };
}

/**
 * @brief The members of delete_many_tenor_convention_resolutions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_tenor_convention_resolutions_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_tenor_convention_resolutions_response::result},
    };
}

/**
 * @brief The members of list_by_convention_code_tenor_convention_resolutions_request, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_convention_code_tenor_convention_resolutions_request&) {
    return std::tuple{
        utility::codec::field{
            "convention_code",
            &list_by_convention_code_tenor_convention_resolutions_request::convention_code},
        utility::codec::field{"scope",
                              &list_by_convention_code_tenor_convention_resolutions_request::scope},
        utility::codec::field{
            "offset", &list_by_convention_code_tenor_convention_resolutions_request::offset},
        utility::codec::field{"limit",
                              &list_by_convention_code_tenor_convention_resolutions_request::limit},
        utility::codec::field{"order",
                              &list_by_convention_code_tenor_convention_resolutions_request::order},
        utility::codec::field{
            "filter", &list_by_convention_code_tenor_convention_resolutions_request::filter},
    };
}

/**
 * @brief The members of list_by_convention_code_tenor_convention_resolutions_response, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_convention_code_tenor_convention_resolutions_response&) {
    return std::tuple{
        utility::codec::field{
            "result", &list_by_convention_code_tenor_convention_resolutions_response::result},
        utility::codec::field{"tenor_convention_resolutions",
                              &list_by_convention_code_tenor_convention_resolutions_response::
                                  tenor_convention_resolutions},
        utility::codec::field{
            "total", &list_by_convention_code_tenor_convention_resolutions_response::total},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::tenor_convention_resolution_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::tenor_convention_resolution_key& v);
    static ores::refdata::messaging::tenor_convention_resolution_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::tenor_convention_resolution_key& v);
    static ores::refdata::messaging::tenor_convention_resolution_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::tenor_convention_resolution_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::tenor_convention_resolution_write& v);
    static ores::refdata::messaging::tenor_convention_resolution_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::tenor_convention_resolution_write& v);
    static ores::refdata::messaging::tenor_convention_resolution_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::tenor_convention_resolution_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::tenor_convention_resolution_change& v);
    static ores::refdata::messaging::tenor_convention_resolution_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::tenor_convention_resolution_change& v);
    static ores::refdata::messaging::tenor_convention_resolution_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::tenor_convention_resolution_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::tenor_convention_resolution_removal& v);
    static ores::refdata::messaging::tenor_convention_resolution_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::tenor_convention_resolution_removal& v);
    static ores::refdata::messaging::tenor_convention_resolution_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::tenor_convention_resolution_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::tenor_convention_resolution_lookup& v);
    static ores::refdata::messaging::tenor_convention_resolution_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::tenor_convention_resolution_lookup& v);
    static ores::refdata::messaging::tenor_convention_resolution_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::tenor_convention_resolutions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::tenor_convention_resolutions_filter& v);
    static ores::refdata::messaging::tenor_convention_resolutions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::tenor_convention_resolutions_filter& v);
    static ores::refdata::messaging::tenor_convention_resolutions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::tenor_convention_resolution_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::tenor_convention_resolution_event& v);
    static ores::refdata::messaging::tenor_convention_resolution_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::tenor_convention_resolution_event& v);
    static ores::refdata::messaging::tenor_convention_resolution_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_tenor_convention_resolutions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_tenor_convention_resolutions_request& v);
    static ores::refdata::messaging::list_tenor_convention_resolutions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_tenor_convention_resolutions_request& v);
    static ores::refdata::messaging::list_tenor_convention_resolutions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_tenor_convention_resolutions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_tenor_convention_resolutions_response& v);
    static ores::refdata::messaging::list_tenor_convention_resolutions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_tenor_convention_resolutions_response& v);
    static ores::refdata::messaging::list_tenor_convention_resolutions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_tenor_convention_resolution_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_tenor_convention_resolution_request& v);
    static ores::refdata::messaging::get_tenor_convention_resolution_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_tenor_convention_resolution_request& v);
    static ores::refdata::messaging::get_tenor_convention_resolution_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_tenor_convention_resolution_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_tenor_convention_resolution_response& v);
    static ores::refdata::messaging::get_tenor_convention_resolution_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_tenor_convention_resolution_response& v);
    static ores::refdata::messaging::get_tenor_convention_resolution_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_tenor_convention_resolutions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_tenor_convention_resolutions_request& v);
    static ores::refdata::messaging::get_many_tenor_convention_resolutions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_tenor_convention_resolutions_request& v);
    static ores::refdata::messaging::get_many_tenor_convention_resolutions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_tenor_convention_resolutions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_tenor_convention_resolutions_response& v);
    static ores::refdata::messaging::get_many_tenor_convention_resolutions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_tenor_convention_resolutions_response& v);
    static ores::refdata::messaging::get_many_tenor_convention_resolutions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_tenor_convention_resolution_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_tenor_convention_resolution_request& v);
    static ores::refdata::messaging::put_tenor_convention_resolution_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_tenor_convention_resolution_request& v);
    static ores::refdata::messaging::put_tenor_convention_resolution_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_tenor_convention_resolution_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_tenor_convention_resolution_response& v);
    static ores::refdata::messaging::put_tenor_convention_resolution_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_tenor_convention_resolution_response& v);
    static ores::refdata::messaging::put_tenor_convention_resolution_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_tenor_convention_resolutions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_tenor_convention_resolutions_request& v);
    static ores::refdata::messaging::put_many_tenor_convention_resolutions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_tenor_convention_resolutions_request& v);
    static ores::refdata::messaging::put_many_tenor_convention_resolutions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_tenor_convention_resolutions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_tenor_convention_resolutions_response& v);
    static ores::refdata::messaging::put_many_tenor_convention_resolutions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_tenor_convention_resolutions_response& v);
    static ores::refdata::messaging::put_many_tenor_convention_resolutions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_tenor_convention_resolution_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_tenor_convention_resolution_request& v);
    static ores::refdata::messaging::delete_tenor_convention_resolution_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_tenor_convention_resolution_request& v);
    static ores::refdata::messaging::delete_tenor_convention_resolution_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_tenor_convention_resolution_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_tenor_convention_resolution_response& v);
    static ores::refdata::messaging::delete_tenor_convention_resolution_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_tenor_convention_resolution_response& v);
    static ores::refdata::messaging::delete_tenor_convention_resolution_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_tenor_convention_resolutions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_tenor_convention_resolutions_request& v);
    static ores::refdata::messaging::delete_many_tenor_convention_resolutions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_tenor_convention_resolutions_request& v);
    static ores::refdata::messaging::delete_many_tenor_convention_resolutions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_tenor_convention_resolutions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_tenor_convention_resolutions_response& v);
    static ores::refdata::messaging::delete_many_tenor_convention_resolutions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_tenor_convention_resolutions_response& v);
    static ores::refdata::messaging::delete_many_tenor_convention_resolutions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<
    ores::refdata::messaging::list_by_convention_code_tenor_convention_resolutions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::
                   list_by_convention_code_tenor_convention_resolutions_request& v);
    static ores::refdata::messaging::list_by_convention_code_tenor_convention_resolutions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::
                      list_by_convention_code_tenor_convention_resolutions_request& v);
    static ores::refdata::messaging::list_by_convention_code_tenor_convention_resolutions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<
    ores::refdata::messaging::list_by_convention_code_tenor_convention_resolutions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::
                   list_by_convention_code_tenor_convention_resolutions_response& v);
    static ores::refdata::messaging::list_by_convention_code_tenor_convention_resolutions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::
                      list_by_convention_code_tenor_convention_resolutions_response& v);
    static ores::refdata::messaging::list_by_convention_code_tenor_convention_resolutions_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
