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
#ifndef ORES_REPORTING_API_MESSAGING_CUT_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_CUT_PROTOCOL_HPP

#include "ores.reporting.api/domain/cut_codec.hpp"
#include "ores.reporting.api/export.hpp"
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

namespace ores::reporting::messaging {

struct cut_key {
    boost::uuids::uuid id;

    friend bool operator==(const cut_key&, const cut_key&) = default;
};

struct cut_lookup {
    cut_key key;
    std::optional<ores::reporting::domain::cut> cut;

    friend bool operator==(const cut_lookup&, const cut_lookup&) = default;
};

struct cuts_filter {
    std::optional<std::string> kind_code;
    std::optional<boost::uuids::uuid> party_id;
    std::optional<std::string> business_centre_code;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<std::string>> kind_code_one_of;
    std::optional<std::vector<boost::uuids::uuid>> party_id_one_of;
    std::optional<std::vector<std::string>> business_centre_code_one_of;

    friend bool operator==(const cuts_filter&, const cuts_filter&) = default;
};

struct cut_event {
    boost::uuids::uuid event_id;
    cut_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const cut_event&, const cut_event&) = default;
};

struct cut_version_key {
    cut_key cut;
    std::uint32_t version;

    friend bool operator==(const cut_version_key&, const cut_version_key&) = default;
};

struct cut_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const cut_versions_filter&, const cut_versions_filter&) = default;
};

struct list_cuts_request {
    using response_type = struct list_cuts_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cuts.list";
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
    std::optional<cuts_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_cuts_request&, const list_cuts_request&) = default;
};

struct list_cuts_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::cut> cuts;
    std::uint64_t total;

    friend bool operator==(const list_cuts_response&, const list_cuts_response&) = default;
};

struct get_cut_request {
    using response_type = struct get_cut_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cuts.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cut_key key;

    friend bool operator==(const get_cut_request&, const get_cut_request&) = default;
};

struct get_cut_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::cut> cut;

    friend bool operator==(const get_cut_response&, const get_cut_response&) = default;
};

struct get_many_cuts_request {
    using response_type = struct get_many_cuts_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cuts.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cut_key> keys;

    friend bool operator==(const get_many_cuts_request&, const get_many_cuts_request&) = default;
};

struct get_many_cuts_response {
    ores::utility::domain::result result;
    std::vector<cut_lookup> entries;

    friend bool operator==(const get_many_cuts_response&, const get_many_cuts_response&) = default;
};

struct list_by_kind_code_cuts_request {
    using response_type = struct list_by_kind_code_cuts_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cuts.list_by_kind_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string kind_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<cuts_filter> filter;

    friend bool operator==(const list_by_kind_code_cuts_request&,
                           const list_by_kind_code_cuts_request&) = default;
};

struct list_by_kind_code_cuts_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::cut> cuts;
    std::uint64_t total;

    friend bool operator==(const list_by_kind_code_cuts_response&,
                           const list_by_kind_code_cuts_response&) = default;
};

struct list_cut_versions_request {
    using response_type = struct list_cut_versions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cuts_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cut_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<cut_versions_filter> filter;

    friend bool operator==(const list_cut_versions_request&,
                           const list_cut_versions_request&) = default;
};

struct list_cut_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::cut> versions;
    std::uint64_t total;

    friend bool operator==(const list_cut_versions_response&,
                           const list_cut_versions_response&) = default;
};

struct get_cut_version_request {
    using response_type = struct get_cut_version_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cuts_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cut_version_key key;

    friend bool operator==(const get_cut_version_request&,
                           const get_cut_version_request&) = default;
};

struct get_cut_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::cut> version;

    friend bool operator==(const get_cut_version_response&,
                           const get_cut_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace cut_event_subjects {
inline constexpr std::string_view created = "reporting.v1.events.cuts.created";
inline constexpr std::string_view updated = "reporting.v1.events.cuts.updated";
inline constexpr std::string_view deleted = "reporting.v1.events.cuts.deleted";
}
/**
 * @brief The members of cut_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cut_key>) {
    return std::tuple{
        utility::codec::field{"id", &cut_key::id},
    };
}

/**
 * @brief The members of cut_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cut_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &cut_lookup::key},
        utility::codec::field{"cut", &cut_lookup::cut},
    };
}

/**
 * @brief The members of cuts_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cuts_filter>) {
    return std::tuple{
        utility::codec::field{"kind_code", &cuts_filter::kind_code},
        utility::codec::field{"party_id", &cuts_filter::party_id},
        utility::codec::field{"business_centre_code", &cuts_filter::business_centre_code},
        utility::codec::field{"id_one_of", &cuts_filter::id_one_of},
        utility::codec::field{"kind_code_one_of", &cuts_filter::kind_code_one_of},
        utility::codec::field{"party_id_one_of", &cuts_filter::party_id_one_of},
        utility::codec::field{"business_centre_code_one_of",
                              &cuts_filter::business_centre_code_one_of},
    };
}

/**
 * @brief The members of cut_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cut_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &cut_event::event_id},
        utility::codec::field{"key", &cut_event::key},
        utility::codec::field{"action", &cut_event::action},
        utility::codec::field{"version", &cut_event::version},
        utility::codec::field{"occurred_at", &cut_event::occurred_at},
        utility::codec::field{"correlation_id", &cut_event::correlation_id},
    };
}

/**
 * @brief The members of cut_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cut_version_key>) {
    return std::tuple{
        utility::codec::field{"cut", &cut_version_key::cut},
        utility::codec::field{"version", &cut_version_key::version},
    };
}

/**
 * @brief The members of cut_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cut_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &cut_versions_filter::version},
        utility::codec::field{"from_version", &cut_versions_filter::from_version},
        utility::codec::field{"to_version", &cut_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_cuts_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_cuts_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_cuts_request::offset},
        utility::codec::field{"limit", &list_cuts_request::limit},
        utility::codec::field{"order", &list_cuts_request::order},
        utility::codec::field{"filter", &list_cuts_request::filter},
        utility::codec::field{"as_of", &list_cuts_request::as_of},
    };
}

/**
 * @brief The members of list_cuts_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_cuts_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_cuts_response::result},
        utility::codec::field{"cuts", &list_cuts_response::cuts},
        utility::codec::field{"total", &list_cuts_response::total},
    };
}

/**
 * @brief The members of get_cut_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_cut_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_cut_request::key},
    };
}

/**
 * @brief The members of get_cut_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_cut_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_cut_response::result},
        utility::codec::field{"cut", &get_cut_response::cut},
    };
}

/**
 * @brief The members of get_many_cuts_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_cuts_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_cuts_request::keys},
    };
}

/**
 * @brief The members of get_many_cuts_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_cuts_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_cuts_response::result},
        utility::codec::field{"entries", &get_many_cuts_response::entries},
    };
}

/**
 * @brief The members of list_by_kind_code_cuts_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_by_kind_code_cuts_request>) {
    return std::tuple{
        utility::codec::field{"kind_code", &list_by_kind_code_cuts_request::kind_code},
        utility::codec::field{"scope", &list_by_kind_code_cuts_request::scope},
        utility::codec::field{"offset", &list_by_kind_code_cuts_request::offset},
        utility::codec::field{"limit", &list_by_kind_code_cuts_request::limit},
        utility::codec::field{"order", &list_by_kind_code_cuts_request::order},
        utility::codec::field{"filter", &list_by_kind_code_cuts_request::filter},
    };
}

/**
 * @brief The members of list_by_kind_code_cuts_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_by_kind_code_cuts_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_by_kind_code_cuts_response::result},
        utility::codec::field{"cuts", &list_by_kind_code_cuts_response::cuts},
        utility::codec::field{"total", &list_by_kind_code_cuts_response::total},
    };
}

/**
 * @brief The members of list_cut_versions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_cut_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_cut_versions_request::key},
        utility::codec::field{"offset", &list_cut_versions_request::offset},
        utility::codec::field{"limit", &list_cut_versions_request::limit},
        utility::codec::field{"order", &list_cut_versions_request::order},
        utility::codec::field{"filter", &list_cut_versions_request::filter},
    };
}

/**
 * @brief The members of list_cut_versions_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_cut_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_cut_versions_response::result},
        utility::codec::field{"versions", &list_cut_versions_response::versions},
        utility::codec::field{"total", &list_cut_versions_response::total},
    };
}

/**
 * @brief The members of get_cut_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_cut_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_cut_version_request::key},
    };
}

/**
 * @brief The members of get_cut_version_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_cut_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_cut_version_response::result},
        utility::codec::field{"version", &get_cut_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::cut_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::cut_key& v);
    static ores::reporting::messaging::cut_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::cut_key& v);
    static ores::reporting::messaging::cut_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::cut_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::cut_lookup& v);
    static ores::reporting::messaging::cut_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::cut_lookup& v);
    static ores::reporting::messaging::cut_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::cuts_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::cuts_filter& v);
    static ores::reporting::messaging::cuts_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::cuts_filter& v);
    static ores::reporting::messaging::cuts_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::cut_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::cut_event& v);
    static ores::reporting::messaging::cut_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::cut_event& v);
    static ores::reporting::messaging::cut_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::cut_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::cut_version_key& v);
    static ores::reporting::messaging::cut_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::cut_version_key& v);
    static ores::reporting::messaging::cut_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::cut_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::cut_versions_filter& v);
    static ores::reporting::messaging::cut_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::cut_versions_filter& v);
    static ores::reporting::messaging::cut_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::list_cuts_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::list_cuts_request& v);
    static ores::reporting::messaging::list_cuts_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::list_cuts_request& v);
    static ores::reporting::messaging::list_cuts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::list_cuts_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::list_cuts_response& v);
    static ores::reporting::messaging::list_cuts_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::list_cuts_response& v);
    static ores::reporting::messaging::list_cuts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::get_cut_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_cut_request& v);
    static ores::reporting::messaging::get_cut_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::get_cut_request& v);
    static ores::reporting::messaging::get_cut_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::get_cut_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_cut_response& v);
    static ores::reporting::messaging::get_cut_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::get_cut_response& v);
    static ores::reporting::messaging::get_cut_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::get_many_cuts_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_many_cuts_request& v);
    static ores::reporting::messaging::get_many_cuts_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::get_many_cuts_request& v);
    static ores::reporting::messaging::get_many_cuts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::get_many_cuts_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_many_cuts_response& v);
    static ores::reporting::messaging::get_many_cuts_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::get_many_cuts_response& v);
    static ores::reporting::messaging::get_many_cuts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_by_kind_code_cuts_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_by_kind_code_cuts_request& v);
    static ores::reporting::messaging::list_by_kind_code_cuts_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_by_kind_code_cuts_request& v);
    static ores::reporting::messaging::list_by_kind_code_cuts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_by_kind_code_cuts_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_by_kind_code_cuts_response& v);
    static ores::reporting::messaging::list_by_kind_code_cuts_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_by_kind_code_cuts_response& v);
    static ores::reporting::messaging::list_by_kind_code_cuts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_cut_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::list_cut_versions_request& v);
    static ores::reporting::messaging::list_cut_versions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_cut_versions_request& v);
    static ores::reporting::messaging::list_cut_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_cut_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::list_cut_versions_response& v);
    static ores::reporting::messaging::list_cut_versions_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_cut_versions_response& v);
    static ores::reporting::messaging::list_cut_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::get_cut_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_cut_version_request& v);
    static ores::reporting::messaging::get_cut_version_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::get_cut_version_request& v);
    static ores::reporting::messaging::get_cut_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::get_cut_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_cut_version_response& v);
    static ores::reporting::messaging::get_cut_version_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::get_cut_version_response& v);
    static ores::reporting::messaging::get_cut_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
