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
#ifndef ORES_DQ_API_MESSAGING_BADGE_MAPPING_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_BADGE_MAPPING_PROTOCOL_HPP

#include "ores.dq.api/domain/badge_mapping.hpp"
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

struct badge_mapping_key {
    std::string code_domain_code;
    std::string entity_code;
};

struct badge_mapping_write {
    std::string code_domain_code;
    std::string entity_code;
    std::string badge_code;
};

struct badge_mapping_change {
    badge_mapping_write write;
    ores::utility::domain::precondition precondition;
};

struct badge_mapping_removal {
    badge_mapping_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct badge_mapping_lookup {
    badge_mapping_key key;
    std::optional<ores::dq::domain::badge_mapping> badge_mapping;
};

struct badge_mappings_filter {
    std::optional<std::string> code_domain_code;
    std::optional<std::vector<std::string>> code_domain_code_one_of;
};

struct badge_mapping_event {
    boost::uuids::uuid event_id;
    badge_mapping_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_badge_mappings_request {
    using response_type = struct list_badge_mappings_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_mappings.list";
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
    std::optional<badge_mappings_filter> filter;
};

struct list_badge_mappings_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::badge_mapping> badge_mappings;
    std::uint64_t total;
};

struct get_badge_mapping_request {
    using response_type = struct get_badge_mapping_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_mappings.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    badge_mapping_key key;
};

struct get_badge_mapping_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::badge_mapping> badge_mapping;
};

struct get_many_badge_mappings_request {
    using response_type = struct get_many_badge_mappings_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_mappings.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<badge_mapping_key> keys;
};

struct get_many_badge_mappings_response {
    ores::utility::domain::result result;
    std::vector<badge_mapping_lookup> entries;
};

struct put_badge_mapping_request {
    using response_type = struct put_badge_mapping_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_mappings.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    badge_mapping_change change;
    ores::utility::domain::change_intent intent;
};

struct put_badge_mapping_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::badge_mapping> badge_mapping;
};

struct put_many_badge_mappings_request {
    using response_type = struct put_many_badge_mappings_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_mappings.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<badge_mapping_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_badge_mappings_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::badge_mapping> badge_mappings;
};

struct delete_badge_mapping_request {
    using response_type = struct delete_badge_mapping_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_mappings.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    badge_mapping_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_badge_mapping_response {
    ores::utility::domain::result result;
};

struct delete_many_badge_mappings_request {
    using response_type = struct delete_many_badge_mappings_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_mappings.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<badge_mapping_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_badge_mappings_response {
    ores::utility::domain::result result;
};

struct list_by_code_domain_code_badge_mappings_request {
    using response_type = struct list_by_code_domain_code_badge_mappings_response;
    static constexpr std::string_view nats_subject =
        "dq.v1.badge_mappings.list_by_code_domain_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string code_domain_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<badge_mappings_filter> filter;
};

struct list_by_code_domain_code_badge_mappings_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::badge_mapping> badge_mappings;
    std::uint64_t total;
};

/**
 * @brief The members of badge_mapping_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const badge_mapping_key&) {
    return std::tuple{
        utility::codec::field{"code_domain_code", &badge_mapping_key::code_domain_code},
        utility::codec::field{"entity_code", &badge_mapping_key::entity_code},
    };
}

/**
 * @brief The members of badge_mapping_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const badge_mapping_write&) {
    return std::tuple{
        utility::codec::field{"code_domain_code", &badge_mapping_write::code_domain_code},
        utility::codec::field{"entity_code", &badge_mapping_write::entity_code},
        utility::codec::field{"badge_code", &badge_mapping_write::badge_code},
    };
}

/**
 * @brief The members of badge_mapping_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const badge_mapping_change&) {
    return std::tuple{
        utility::codec::field{"write", &badge_mapping_change::write},
        utility::codec::field{"precondition", &badge_mapping_change::precondition},
    };
}

/**
 * @brief The members of badge_mapping_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const badge_mapping_removal&) {
    return std::tuple{
        utility::codec::field{"key", &badge_mapping_removal::key},
        utility::codec::field{"precondition", &badge_mapping_removal::precondition},
    };
}

/**
 * @brief The members of badge_mapping_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const badge_mapping_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &badge_mapping_lookup::key},
        utility::codec::field{"badge_mapping", &badge_mapping_lookup::badge_mapping},
    };
}

/**
 * @brief The members of badge_mappings_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const badge_mappings_filter&) {
    return std::tuple{
        utility::codec::field{"code_domain_code", &badge_mappings_filter::code_domain_code},
        utility::codec::field{"code_domain_code_one_of",
                              &badge_mappings_filter::code_domain_code_one_of},
    };
}

/**
 * @brief The members of badge_mapping_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const badge_mapping_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &badge_mapping_event::event_id},
        utility::codec::field{"key", &badge_mapping_event::key},
        utility::codec::field{"action", &badge_mapping_event::action},
        utility::codec::field{"version", &badge_mapping_event::version},
        utility::codec::field{"occurred_at", &badge_mapping_event::occurred_at},
        utility::codec::field{"correlation_id", &badge_mapping_event::correlation_id},
    };
}

/**
 * @brief The members of list_badge_mappings_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_badge_mappings_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_badge_mappings_request::offset},
        utility::codec::field{"limit", &list_badge_mappings_request::limit},
        utility::codec::field{"order", &list_badge_mappings_request::order},
        utility::codec::field{"filter", &list_badge_mappings_request::filter},
    };
}

/**
 * @brief The members of list_badge_mappings_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_badge_mappings_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_badge_mappings_response::result},
        utility::codec::field{"badge_mappings", &list_badge_mappings_response::badge_mappings},
        utility::codec::field{"total", &list_badge_mappings_response::total},
    };
}

/**
 * @brief The members of get_badge_mapping_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_badge_mapping_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_badge_mapping_request::key},
    };
}

/**
 * @brief The members of get_badge_mapping_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_badge_mapping_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_badge_mapping_response::result},
        utility::codec::field{"badge_mapping", &get_badge_mapping_response::badge_mapping},
    };
}

/**
 * @brief The members of get_many_badge_mappings_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_badge_mappings_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_badge_mappings_request::keys},
    };
}

/**
 * @brief The members of get_many_badge_mappings_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_badge_mappings_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_badge_mappings_response::result},
        utility::codec::field{"entries", &get_many_badge_mappings_response::entries},
    };
}

/**
 * @brief The members of put_badge_mapping_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_badge_mapping_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_badge_mapping_request::change},
        utility::codec::field{"intent", &put_badge_mapping_request::intent},
    };
}

/**
 * @brief The members of put_badge_mapping_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_badge_mapping_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_badge_mapping_response::result},
        utility::codec::field{"badge_mapping", &put_badge_mapping_response::badge_mapping},
    };
}

/**
 * @brief The members of put_many_badge_mappings_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_badge_mappings_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_badge_mappings_request::changes},
        utility::codec::field{"intent", &put_many_badge_mappings_request::intent},
    };
}

/**
 * @brief The members of put_many_badge_mappings_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_badge_mappings_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_badge_mappings_response::result},
        utility::codec::field{"badge_mappings", &put_many_badge_mappings_response::badge_mappings},
    };
}

/**
 * @brief The members of delete_badge_mapping_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_badge_mapping_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_badge_mapping_request::removal},
        utility::codec::field{"intent", &delete_badge_mapping_request::intent},
    };
}

/**
 * @brief The members of delete_badge_mapping_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_badge_mapping_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_badge_mapping_response::result},
    };
}

/**
 * @brief The members of delete_many_badge_mappings_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_badge_mappings_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_badge_mappings_request::removals},
        utility::codec::field{"intent", &delete_many_badge_mappings_request::intent},
    };
}

/**
 * @brief The members of delete_many_badge_mappings_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_badge_mappings_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_badge_mappings_response::result},
    };
}

/**
 * @brief The members of list_by_code_domain_code_badge_mappings_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_code_domain_code_badge_mappings_request&) {
    return std::tuple{
        utility::codec::field{"code_domain_code",
                              &list_by_code_domain_code_badge_mappings_request::code_domain_code},
        utility::codec::field{"scope", &list_by_code_domain_code_badge_mappings_request::scope},
        utility::codec::field{"offset", &list_by_code_domain_code_badge_mappings_request::offset},
        utility::codec::field{"limit", &list_by_code_domain_code_badge_mappings_request::limit},
        utility::codec::field{"order", &list_by_code_domain_code_badge_mappings_request::order},
        utility::codec::field{"filter", &list_by_code_domain_code_badge_mappings_request::filter},
    };
}

/**
 * @brief The members of list_by_code_domain_code_badge_mappings_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_code_domain_code_badge_mappings_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_by_code_domain_code_badge_mappings_response::result},
        utility::codec::field{"badge_mappings",
                              &list_by_code_domain_code_badge_mappings_response::badge_mappings},
        utility::codec::field{"total", &list_by_code_domain_code_badge_mappings_response::total},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::badge_mapping_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::badge_mapping_key& v);
    static ores::dq::messaging::badge_mapping_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::badge_mapping_key& v);
    static ores::dq::messaging::badge_mapping_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::badge_mapping_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::badge_mapping_write& v);
    static ores::dq::messaging::badge_mapping_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::badge_mapping_write& v);
    static ores::dq::messaging::badge_mapping_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::badge_mapping_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::badge_mapping_change& v);
    static ores::dq::messaging::badge_mapping_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::badge_mapping_change& v);
    static ores::dq::messaging::badge_mapping_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::badge_mapping_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::badge_mapping_removal& v);
    static ores::dq::messaging::badge_mapping_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::badge_mapping_removal& v);
    static ores::dq::messaging::badge_mapping_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::badge_mapping_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::badge_mapping_lookup& v);
    static ores::dq::messaging::badge_mapping_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::badge_mapping_lookup& v);
    static ores::dq::messaging::badge_mapping_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::badge_mappings_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::badge_mappings_filter& v);
    static ores::dq::messaging::badge_mappings_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::badge_mappings_filter& v);
    static ores::dq::messaging::badge_mappings_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::badge_mapping_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::badge_mapping_event& v);
    static ores::dq::messaging::badge_mapping_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::badge_mapping_event& v);
    static ores::dq::messaging::badge_mapping_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_badge_mappings_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_badge_mappings_request& v);
    static ores::dq::messaging::list_badge_mappings_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_badge_mappings_request& v);
    static ores::dq::messaging::list_badge_mappings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_badge_mappings_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_badge_mappings_response& v);
    static ores::dq::messaging::list_badge_mappings_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_badge_mappings_response& v);
    static ores::dq::messaging::list_badge_mappings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_badge_mapping_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_badge_mapping_request& v);
    static ores::dq::messaging::get_badge_mapping_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_badge_mapping_request& v);
    static ores::dq::messaging::get_badge_mapping_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_badge_mapping_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_badge_mapping_response& v);
    static ores::dq::messaging::get_badge_mapping_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_badge_mapping_response& v);
    static ores::dq::messaging::get_badge_mapping_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_badge_mappings_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_badge_mappings_request& v);
    static ores::dq::messaging::get_many_badge_mappings_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_many_badge_mappings_request& v);
    static ores::dq::messaging::get_many_badge_mappings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_badge_mappings_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_badge_mappings_response& v);
    static ores::dq::messaging::get_many_badge_mappings_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_many_badge_mappings_response& v);
    static ores::dq::messaging::get_many_badge_mappings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_badge_mapping_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_badge_mapping_request& v);
    static ores::dq::messaging::put_badge_mapping_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_badge_mapping_request& v);
    static ores::dq::messaging::put_badge_mapping_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_badge_mapping_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_badge_mapping_response& v);
    static ores::dq::messaging::put_badge_mapping_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_badge_mapping_response& v);
    static ores::dq::messaging::put_badge_mapping_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_badge_mappings_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_badge_mappings_request& v);
    static ores::dq::messaging::put_many_badge_mappings_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_many_badge_mappings_request& v);
    static ores::dq::messaging::put_many_badge_mappings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_badge_mappings_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_badge_mappings_response& v);
    static ores::dq::messaging::put_many_badge_mappings_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::put_many_badge_mappings_response& v);
    static ores::dq::messaging::put_many_badge_mappings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_badge_mapping_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_badge_mapping_request& v);
    static ores::dq::messaging::delete_badge_mapping_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_badge_mapping_request& v);
    static ores::dq::messaging::delete_badge_mapping_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_badge_mapping_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_badge_mapping_response& v);
    static ores::dq::messaging::delete_badge_mapping_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_badge_mapping_response& v);
    static ores::dq::messaging::delete_badge_mapping_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_badge_mappings_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_many_badge_mappings_request& v);
    static ores::dq::messaging::delete_many_badge_mappings_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_badge_mappings_request& v);
    static ores::dq::messaging::delete_many_badge_mappings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_badge_mappings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::delete_many_badge_mappings_response& v);
    static ores::dq::messaging::delete_many_badge_mappings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_badge_mappings_response& v);
    static ores::dq::messaging::delete_many_badge_mappings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT
    wire_traits<ores::dq::messaging::list_by_code_domain_code_badge_mappings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_by_code_domain_code_badge_mappings_request& v);
    static ores::dq::messaging::list_by_code_domain_code_badge_mappings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_by_code_domain_code_badge_mappings_request& v);
    static ores::dq::messaging::list_by_code_domain_code_badge_mappings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT
    wire_traits<ores::dq::messaging::list_by_code_domain_code_badge_mappings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_by_code_domain_code_badge_mappings_response& v);
    static ores::dq::messaging::list_by_code_domain_code_badge_mappings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_by_code_domain_code_badge_mappings_response& v);
    static ores::dq::messaging::list_by_code_domain_code_badge_mappings_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
