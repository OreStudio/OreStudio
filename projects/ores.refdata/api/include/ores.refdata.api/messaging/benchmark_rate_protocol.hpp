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
#ifndef ORES_REFDATA_API_MESSAGING_BENCHMARK_RATE_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_BENCHMARK_RATE_PROTOCOL_HPP

#include "ores.refdata.api/domain/benchmark_rate.hpp"
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

struct benchmark_rate_key {
    std::string code;
    std::string coding_scheme_code;
};

struct benchmark_rate_write {
    std::string code;
    std::string coding_scheme_code;
    std::string source;
    std::string description;
};

struct benchmark_rate_change {
    benchmark_rate_write write;
    ores::utility::domain::precondition precondition;
};

struct benchmark_rate_removal {
    benchmark_rate_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct benchmark_rate_lookup {
    benchmark_rate_key key;
    std::optional<ores::refdata::domain::benchmark_rate> benchmark_rate;
};

struct benchmark_rate_event {
    boost::uuids::uuid event_id;
    benchmark_rate_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct benchmark_rate_version_key {
    benchmark_rate_key benchmark_rate;
    std::uint32_t version;
};

struct benchmark_rate_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_benchmark_rates_request {
    using response_type = struct list_benchmark_rates_response;
    static constexpr std::string_view nats_subject = "refdata.v1.benchmark_rates.list";
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
    std::optional<std::string> as_of;
};

struct list_benchmark_rates_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::benchmark_rate> rates;
    std::uint64_t total;
};

struct get_benchmark_rate_request {
    using response_type = struct get_benchmark_rate_response;
    static constexpr std::string_view nats_subject = "refdata.v1.benchmark_rates.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    benchmark_rate_key key;
};

struct get_benchmark_rate_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::benchmark_rate> benchmark_rate;
};

struct get_many_benchmark_rates_request {
    using response_type = struct get_many_benchmark_rates_response;
    static constexpr std::string_view nats_subject = "refdata.v1.benchmark_rates.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<benchmark_rate_key> keys;
};

struct get_many_benchmark_rates_response {
    ores::utility::domain::result result;
    std::vector<benchmark_rate_lookup> entries;
};

struct put_benchmark_rate_request {
    using response_type = struct put_benchmark_rate_response;
    static constexpr std::string_view nats_subject = "refdata.v1.benchmark_rates.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    benchmark_rate_change change;
    ores::utility::domain::change_intent intent;
};

struct put_benchmark_rate_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::benchmark_rate> benchmark_rate;
};

struct put_many_benchmark_rates_request {
    using response_type = struct put_many_benchmark_rates_response;
    static constexpr std::string_view nats_subject = "refdata.v1.benchmark_rates.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<benchmark_rate_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_benchmark_rates_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::benchmark_rate> rates;
};

struct delete_benchmark_rate_request {
    using response_type = struct delete_benchmark_rate_response;
    static constexpr std::string_view nats_subject = "refdata.v1.benchmark_rates.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    benchmark_rate_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_benchmark_rate_response {
    ores::utility::domain::result result;
};

struct delete_many_benchmark_rates_request {
    using response_type = struct delete_many_benchmark_rates_response;
    static constexpr std::string_view nats_subject = "refdata.v1.benchmark_rates.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<benchmark_rate_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_benchmark_rates_response {
    ores::utility::domain::result result;
};

struct list_benchmark_rate_versions_request {
    using response_type = struct list_benchmark_rate_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.benchmark_rates_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    benchmark_rate_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<benchmark_rate_versions_filter> filter;
};

struct list_benchmark_rate_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::benchmark_rate> versions;
    std::uint64_t total;
};

struct get_benchmark_rate_version_request {
    using response_type = struct get_benchmark_rate_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.benchmark_rates_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    benchmark_rate_version_key key;
};

struct get_benchmark_rate_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::benchmark_rate> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace benchmark_rate_event_subjects {
inline constexpr std::string_view created = "refdata.v1.benchmark_rates_events.created";
inline constexpr std::string_view updated = "refdata.v1.benchmark_rates_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.benchmark_rates_events.deleted";
}
/**
 * @brief The members of benchmark_rate_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const benchmark_rate_key&) {
    return std::tuple{
        utility::codec::field{"code", &benchmark_rate_key::code},
        utility::codec::field{"coding_scheme_code", &benchmark_rate_key::coding_scheme_code},
    };
}

/**
 * @brief The members of benchmark_rate_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const benchmark_rate_write&) {
    return std::tuple{
        utility::codec::field{"code", &benchmark_rate_write::code},
        utility::codec::field{"coding_scheme_code", &benchmark_rate_write::coding_scheme_code},
        utility::codec::field{"source", &benchmark_rate_write::source},
        utility::codec::field{"description", &benchmark_rate_write::description},
    };
}

/**
 * @brief The members of benchmark_rate_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const benchmark_rate_change&) {
    return std::tuple{
        utility::codec::field{"write", &benchmark_rate_change::write},
        utility::codec::field{"precondition", &benchmark_rate_change::precondition},
    };
}

/**
 * @brief The members of benchmark_rate_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const benchmark_rate_removal&) {
    return std::tuple{
        utility::codec::field{"key", &benchmark_rate_removal::key},
        utility::codec::field{"precondition", &benchmark_rate_removal::precondition},
    };
}

/**
 * @brief The members of benchmark_rate_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const benchmark_rate_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &benchmark_rate_lookup::key},
        utility::codec::field{"benchmark_rate", &benchmark_rate_lookup::benchmark_rate},
    };
}

/**
 * @brief The members of benchmark_rate_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const benchmark_rate_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &benchmark_rate_event::event_id},
        utility::codec::field{"key", &benchmark_rate_event::key},
        utility::codec::field{"action", &benchmark_rate_event::action},
        utility::codec::field{"version", &benchmark_rate_event::version},
        utility::codec::field{"occurred_at", &benchmark_rate_event::occurred_at},
        utility::codec::field{"correlation_id", &benchmark_rate_event::correlation_id},
    };
}

/**
 * @brief The members of benchmark_rate_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const benchmark_rate_version_key&) {
    return std::tuple{
        utility::codec::field{"benchmark_rate", &benchmark_rate_version_key::benchmark_rate},
        utility::codec::field{"version", &benchmark_rate_version_key::version},
    };
}

/**
 * @brief The members of benchmark_rate_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const benchmark_rate_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &benchmark_rate_versions_filter::version},
        utility::codec::field{"from_version", &benchmark_rate_versions_filter::from_version},
        utility::codec::field{"to_version", &benchmark_rate_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_benchmark_rates_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_benchmark_rates_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_benchmark_rates_request::offset},
        utility::codec::field{"limit", &list_benchmark_rates_request::limit},
        utility::codec::field{"order", &list_benchmark_rates_request::order},
        utility::codec::field{"as_of", &list_benchmark_rates_request::as_of},
    };
}

/**
 * @brief The members of list_benchmark_rates_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_benchmark_rates_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_benchmark_rates_response::result},
        utility::codec::field{"rates", &list_benchmark_rates_response::rates},
        utility::codec::field{"total", &list_benchmark_rates_response::total},
    };
}

/**
 * @brief The members of get_benchmark_rate_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_benchmark_rate_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_benchmark_rate_request::key},
    };
}

/**
 * @brief The members of get_benchmark_rate_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_benchmark_rate_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_benchmark_rate_response::result},
        utility::codec::field{"benchmark_rate", &get_benchmark_rate_response::benchmark_rate},
    };
}

/**
 * @brief The members of get_many_benchmark_rates_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_benchmark_rates_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_benchmark_rates_request::keys},
    };
}

/**
 * @brief The members of get_many_benchmark_rates_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_benchmark_rates_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_benchmark_rates_response::result},
        utility::codec::field{"entries", &get_many_benchmark_rates_response::entries},
    };
}

/**
 * @brief The members of put_benchmark_rate_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_benchmark_rate_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_benchmark_rate_request::change},
        utility::codec::field{"intent", &put_benchmark_rate_request::intent},
    };
}

/**
 * @brief The members of put_benchmark_rate_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_benchmark_rate_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_benchmark_rate_response::result},
        utility::codec::field{"benchmark_rate", &put_benchmark_rate_response::benchmark_rate},
    };
}

/**
 * @brief The members of put_many_benchmark_rates_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_benchmark_rates_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_benchmark_rates_request::changes},
        utility::codec::field{"intent", &put_many_benchmark_rates_request::intent},
    };
}

/**
 * @brief The members of put_many_benchmark_rates_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_benchmark_rates_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_benchmark_rates_response::result},
        utility::codec::field{"rates", &put_many_benchmark_rates_response::rates},
    };
}

/**
 * @brief The members of delete_benchmark_rate_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_benchmark_rate_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_benchmark_rate_request::removal},
        utility::codec::field{"intent", &delete_benchmark_rate_request::intent},
    };
}

/**
 * @brief The members of delete_benchmark_rate_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_benchmark_rate_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_benchmark_rate_response::result},
    };
}

/**
 * @brief The members of delete_many_benchmark_rates_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_benchmark_rates_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_benchmark_rates_request::removals},
        utility::codec::field{"intent", &delete_many_benchmark_rates_request::intent},
    };
}

/**
 * @brief The members of delete_many_benchmark_rates_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_benchmark_rates_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_benchmark_rates_response::result},
    };
}

/**
 * @brief The members of list_benchmark_rate_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_benchmark_rate_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_benchmark_rate_versions_request::key},
        utility::codec::field{"offset", &list_benchmark_rate_versions_request::offset},
        utility::codec::field{"limit", &list_benchmark_rate_versions_request::limit},
        utility::codec::field{"order", &list_benchmark_rate_versions_request::order},
        utility::codec::field{"filter", &list_benchmark_rate_versions_request::filter},
    };
}

/**
 * @brief The members of list_benchmark_rate_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_benchmark_rate_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_benchmark_rate_versions_response::result},
        utility::codec::field{"versions", &list_benchmark_rate_versions_response::versions},
        utility::codec::field{"total", &list_benchmark_rate_versions_response::total},
    };
}

/**
 * @brief The members of get_benchmark_rate_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_benchmark_rate_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_benchmark_rate_version_request::key},
    };
}

/**
 * @brief The members of get_benchmark_rate_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_benchmark_rate_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_benchmark_rate_version_response::result},
        utility::codec::field{"version", &get_benchmark_rate_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::benchmark_rate_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::benchmark_rate_key& v);
    static ores::refdata::messaging::benchmark_rate_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::benchmark_rate_key& v);
    static ores::refdata::messaging::benchmark_rate_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::benchmark_rate_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::benchmark_rate_write& v);
    static ores::refdata::messaging::benchmark_rate_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::benchmark_rate_write& v);
    static ores::refdata::messaging::benchmark_rate_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::benchmark_rate_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::benchmark_rate_change& v);
    static ores::refdata::messaging::benchmark_rate_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::benchmark_rate_change& v);
    static ores::refdata::messaging::benchmark_rate_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::benchmark_rate_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::benchmark_rate_removal& v);
    static ores::refdata::messaging::benchmark_rate_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::benchmark_rate_removal& v);
    static ores::refdata::messaging::benchmark_rate_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::benchmark_rate_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::benchmark_rate_lookup& v);
    static ores::refdata::messaging::benchmark_rate_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::benchmark_rate_lookup& v);
    static ores::refdata::messaging::benchmark_rate_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::benchmark_rate_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::benchmark_rate_event& v);
    static ores::refdata::messaging::benchmark_rate_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::benchmark_rate_event& v);
    static ores::refdata::messaging::benchmark_rate_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::benchmark_rate_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::benchmark_rate_version_key& v);
    static ores::refdata::messaging::benchmark_rate_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::benchmark_rate_version_key& v);
    static ores::refdata::messaging::benchmark_rate_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::benchmark_rate_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::benchmark_rate_versions_filter& v);
    static ores::refdata::messaging::benchmark_rate_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::benchmark_rate_versions_filter& v);
    static ores::refdata::messaging::benchmark_rate_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_benchmark_rates_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_benchmark_rates_request& v);
    static ores::refdata::messaging::list_benchmark_rates_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_benchmark_rates_request& v);
    static ores::refdata::messaging::list_benchmark_rates_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_benchmark_rates_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_benchmark_rates_response& v);
    static ores::refdata::messaging::list_benchmark_rates_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_benchmark_rates_response& v);
    static ores::refdata::messaging::list_benchmark_rates_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_benchmark_rate_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_benchmark_rate_request& v);
    static ores::refdata::messaging::get_benchmark_rate_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_benchmark_rate_request& v);
    static ores::refdata::messaging::get_benchmark_rate_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_benchmark_rate_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_benchmark_rate_response& v);
    static ores::refdata::messaging::get_benchmark_rate_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_benchmark_rate_response& v);
    static ores::refdata::messaging::get_benchmark_rate_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_benchmark_rates_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_benchmark_rates_request& v);
    static ores::refdata::messaging::get_many_benchmark_rates_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_benchmark_rates_request& v);
    static ores::refdata::messaging::get_many_benchmark_rates_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_benchmark_rates_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_benchmark_rates_response& v);
    static ores::refdata::messaging::get_many_benchmark_rates_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_benchmark_rates_response& v);
    static ores::refdata::messaging::get_many_benchmark_rates_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_benchmark_rate_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_benchmark_rate_request& v);
    static ores::refdata::messaging::put_benchmark_rate_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_benchmark_rate_request& v);
    static ores::refdata::messaging::put_benchmark_rate_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_benchmark_rate_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_benchmark_rate_response& v);
    static ores::refdata::messaging::put_benchmark_rate_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_benchmark_rate_response& v);
    static ores::refdata::messaging::put_benchmark_rate_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_benchmark_rates_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_benchmark_rates_request& v);
    static ores::refdata::messaging::put_many_benchmark_rates_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_benchmark_rates_request& v);
    static ores::refdata::messaging::put_many_benchmark_rates_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_benchmark_rates_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_benchmark_rates_response& v);
    static ores::refdata::messaging::put_many_benchmark_rates_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_benchmark_rates_response& v);
    static ores::refdata::messaging::put_many_benchmark_rates_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_benchmark_rate_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_benchmark_rate_request& v);
    static ores::refdata::messaging::delete_benchmark_rate_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_benchmark_rate_request& v);
    static ores::refdata::messaging::delete_benchmark_rate_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_benchmark_rate_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_benchmark_rate_response& v);
    static ores::refdata::messaging::delete_benchmark_rate_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_benchmark_rate_response& v);
    static ores::refdata::messaging::delete_benchmark_rate_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_benchmark_rates_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_benchmark_rates_request& v);
    static ores::refdata::messaging::delete_many_benchmark_rates_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_benchmark_rates_request& v);
    static ores::refdata::messaging::delete_many_benchmark_rates_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_benchmark_rates_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_benchmark_rates_response& v);
    static ores::refdata::messaging::delete_many_benchmark_rates_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_benchmark_rates_response& v);
    static ores::refdata::messaging::delete_many_benchmark_rates_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_benchmark_rate_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_benchmark_rate_versions_request& v);
    static ores::refdata::messaging::list_benchmark_rate_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_benchmark_rate_versions_request& v);
    static ores::refdata::messaging::list_benchmark_rate_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_benchmark_rate_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_benchmark_rate_versions_response& v);
    static ores::refdata::messaging::list_benchmark_rate_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_benchmark_rate_versions_response& v);
    static ores::refdata::messaging::list_benchmark_rate_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_benchmark_rate_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_benchmark_rate_version_request& v);
    static ores::refdata::messaging::get_benchmark_rate_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_benchmark_rate_version_request& v);
    static ores::refdata::messaging::get_benchmark_rate_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_benchmark_rate_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_benchmark_rate_version_response& v);
    static ores::refdata::messaging::get_benchmark_rate_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_benchmark_rate_version_response& v);
    static ores::refdata::messaging::get_benchmark_rate_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
