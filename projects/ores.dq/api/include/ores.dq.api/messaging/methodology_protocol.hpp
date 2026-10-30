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
#ifndef ORES_DQ_API_MESSAGING_METHODOLOGY_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_METHODOLOGY_PROTOCOL_HPP

#include "ores.dq.api/domain/methodology.hpp"
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

struct methodology_key {
    std::string name;
};

struct methodology_write {
    boost::uuids::uuid id;
    std::string name;
    std::string description;
    std::string logic_reference;
    std::string implementation_details;
};

struct methodology_change {
    methodology_write write;
    ores::utility::domain::precondition precondition;
};

struct methodology_removal {
    methodology_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct methodology_lookup {
    methodology_key key;
    std::optional<ores::dq::domain::methodology> methodology;
};

struct methodologies_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct methodology_event {
    boost::uuids::uuid event_id;
    methodology_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct methodology_version_key {
    methodology_key methodology;
    std::uint32_t version;
};

struct methodology_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_methodologies_request {
    using response_type = struct list_methodologies_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies.list";
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
    std::optional<methodologies_filter> filter;
    std::optional<std::string> as_of;
};

struct list_methodologies_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::methodology> methodologies;
    std::uint64_t total;
};

struct get_methodology_request {
    using response_type = struct get_methodology_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    methodology_key key;
};

struct get_methodology_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::methodology> methodology;
};

struct get_many_methodologies_request {
    using response_type = struct get_many_methodologies_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<methodology_key> keys;
};

struct get_many_methodologies_response {
    ores::utility::domain::result result;
    std::vector<methodology_lookup> entries;
};

struct put_methodology_request {
    using response_type = struct put_methodology_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    methodology_change change;
    ores::utility::domain::change_intent intent;
};

struct put_methodology_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::methodology> methodology;
};

struct put_many_methodologies_request {
    using response_type = struct put_many_methodologies_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<methodology_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_methodologies_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::methodology> methodologies;
};

struct delete_methodology_request {
    using response_type = struct delete_methodology_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    methodology_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_methodology_response {
    ores::utility::domain::result result;
};

struct delete_many_methodologies_request {
    using response_type = struct delete_many_methodologies_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<methodology_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_methodologies_response {
    ores::utility::domain::result result;
};

struct list_methodology_versions_request {
    using response_type = struct list_methodology_versions_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    methodology_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<methodology_versions_filter> filter;
};

struct list_methodology_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::methodology> versions;
    std::uint64_t total;
};

struct get_methodology_version_request {
    using response_type = struct get_methodology_version_response;
    static constexpr std::string_view nats_subject = "dq.v1.methodologies_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    methodology_version_key key;
};

struct get_methodology_version_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::methodology> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace methodology_event_subjects {
inline constexpr std::string_view created = "dq.v1.methodologies_events.created";
inline constexpr std::string_view updated = "dq.v1.methodologies_events.updated";
inline constexpr std::string_view deleted = "dq.v1.methodologies_events.deleted";
}
/**
 * @brief The members of methodology_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const methodology_key&) {
    return std::tuple{
        utility::codec::field{"name", &methodology_key::name},
    };
}

/**
 * @brief The members of methodology_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const methodology_write&) {
    return std::tuple{
        utility::codec::field{"id", &methodology_write::id},
        utility::codec::field{"name", &methodology_write::name},
        utility::codec::field{"description", &methodology_write::description},
        utility::codec::field{"logic_reference", &methodology_write::logic_reference},
        utility::codec::field{"implementation_details", &methodology_write::implementation_details},
    };
}

/**
 * @brief The members of methodology_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const methodology_change&) {
    return std::tuple{
        utility::codec::field{"write", &methodology_change::write},
        utility::codec::field{"precondition", &methodology_change::precondition},
    };
}

/**
 * @brief The members of methodology_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const methodology_removal&) {
    return std::tuple{
        utility::codec::field{"key", &methodology_removal::key},
        utility::codec::field{"precondition", &methodology_removal::precondition},
    };
}

/**
 * @brief The members of methodology_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const methodology_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &methodology_lookup::key},
        utility::codec::field{"methodology", &methodology_lookup::methodology},
    };
}

/**
 * @brief The members of methodologies_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const methodologies_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &methodologies_filter::id_one_of},
    };
}

/**
 * @brief The members of methodology_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const methodology_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &methodology_event::event_id},
        utility::codec::field{"key", &methodology_event::key},
        utility::codec::field{"action", &methodology_event::action},
        utility::codec::field{"version", &methodology_event::version},
        utility::codec::field{"occurred_at", &methodology_event::occurred_at},
        utility::codec::field{"correlation_id", &methodology_event::correlation_id},
    };
}

/**
 * @brief The members of methodology_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const methodology_version_key&) {
    return std::tuple{
        utility::codec::field{"methodology", &methodology_version_key::methodology},
        utility::codec::field{"version", &methodology_version_key::version},
    };
}

/**
 * @brief The members of methodology_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const methodology_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &methodology_versions_filter::version},
        utility::codec::field{"from_version", &methodology_versions_filter::from_version},
        utility::codec::field{"to_version", &methodology_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_methodologies_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_methodologies_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_methodologies_request::offset},
        utility::codec::field{"limit", &list_methodologies_request::limit},
        utility::codec::field{"order", &list_methodologies_request::order},
        utility::codec::field{"filter", &list_methodologies_request::filter},
        utility::codec::field{"as_of", &list_methodologies_request::as_of},
    };
}

/**
 * @brief The members of list_methodologies_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_methodologies_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_methodologies_response::result},
        utility::codec::field{"methodologies", &list_methodologies_response::methodologies},
        utility::codec::field{"total", &list_methodologies_response::total},
    };
}

/**
 * @brief The members of get_methodology_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_methodology_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_methodology_request::key},
    };
}

/**
 * @brief The members of get_methodology_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_methodology_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_methodology_response::result},
        utility::codec::field{"methodology", &get_methodology_response::methodology},
    };
}

/**
 * @brief The members of get_many_methodologies_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_methodologies_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_methodologies_request::keys},
    };
}

/**
 * @brief The members of get_many_methodologies_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_methodologies_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_methodologies_response::result},
        utility::codec::field{"entries", &get_many_methodologies_response::entries},
    };
}

/**
 * @brief The members of put_methodology_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_methodology_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_methodology_request::change},
        utility::codec::field{"intent", &put_methodology_request::intent},
    };
}

/**
 * @brief The members of put_methodology_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_methodology_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_methodology_response::result},
        utility::codec::field{"methodology", &put_methodology_response::methodology},
    };
}

/**
 * @brief The members of put_many_methodologies_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_methodologies_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_methodologies_request::changes},
        utility::codec::field{"intent", &put_many_methodologies_request::intent},
    };
}

/**
 * @brief The members of put_many_methodologies_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_methodologies_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_methodologies_response::result},
        utility::codec::field{"methodologies", &put_many_methodologies_response::methodologies},
    };
}

/**
 * @brief The members of delete_methodology_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_methodology_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_methodology_request::removal},
        utility::codec::field{"intent", &delete_methodology_request::intent},
    };
}

/**
 * @brief The members of delete_methodology_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_methodology_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_methodology_response::result},
    };
}

/**
 * @brief The members of delete_many_methodologies_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_methodologies_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_methodologies_request::removals},
        utility::codec::field{"intent", &delete_many_methodologies_request::intent},
    };
}

/**
 * @brief The members of delete_many_methodologies_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_methodologies_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_methodologies_response::result},
    };
}

/**
 * @brief The members of list_methodology_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_methodology_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_methodology_versions_request::key},
        utility::codec::field{"offset", &list_methodology_versions_request::offset},
        utility::codec::field{"limit", &list_methodology_versions_request::limit},
        utility::codec::field{"order", &list_methodology_versions_request::order},
        utility::codec::field{"filter", &list_methodology_versions_request::filter},
    };
}

/**
 * @brief The members of list_methodology_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_methodology_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_methodology_versions_response::result},
        utility::codec::field{"versions", &list_methodology_versions_response::versions},
        utility::codec::field{"total", &list_methodology_versions_response::total},
    };
}

/**
 * @brief The members of get_methodology_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_methodology_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_methodology_version_request::key},
    };
}

/**
 * @brief The members of get_methodology_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_methodology_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_methodology_version_response::result},
        utility::codec::field{"version", &get_methodology_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::methodology_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::methodology_key& v);
    static ores::dq::messaging::methodology_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::methodology_key& v);
    static ores::dq::messaging::methodology_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::methodology_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::methodology_write& v);
    static ores::dq::messaging::methodology_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::methodology_write& v);
    static ores::dq::messaging::methodology_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::methodology_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::methodology_change& v);
    static ores::dq::messaging::methodology_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::methodology_change& v);
    static ores::dq::messaging::methodology_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::methodology_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::methodology_removal& v);
    static ores::dq::messaging::methodology_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::methodology_removal& v);
    static ores::dq::messaging::methodology_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::methodology_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::methodology_lookup& v);
    static ores::dq::messaging::methodology_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::methodology_lookup& v);
    static ores::dq::messaging::methodology_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::methodologies_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::methodologies_filter& v);
    static ores::dq::messaging::methodologies_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::methodologies_filter& v);
    static ores::dq::messaging::methodologies_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::methodology_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::methodology_event& v);
    static ores::dq::messaging::methodology_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::methodology_event& v);
    static ores::dq::messaging::methodology_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::methodology_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::methodology_version_key& v);
    static ores::dq::messaging::methodology_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::methodology_version_key& v);
    static ores::dq::messaging::methodology_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::methodology_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::methodology_versions_filter& v);
    static ores::dq::messaging::methodology_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::methodology_versions_filter& v);
    static ores::dq::messaging::methodology_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_methodologies_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_methodologies_request& v);
    static ores::dq::messaging::list_methodologies_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_methodologies_request& v);
    static ores::dq::messaging::list_methodologies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_methodologies_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_methodologies_response& v);
    static ores::dq::messaging::list_methodologies_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_methodologies_response& v);
    static ores::dq::messaging::list_methodologies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_methodology_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_methodology_request& v);
    static ores::dq::messaging::get_methodology_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_methodology_request& v);
    static ores::dq::messaging::get_methodology_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_methodology_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_methodology_response& v);
    static ores::dq::messaging::get_methodology_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_methodology_response& v);
    static ores::dq::messaging::get_methodology_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_methodologies_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_methodologies_request& v);
    static ores::dq::messaging::get_many_methodologies_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_many_methodologies_request& v);
    static ores::dq::messaging::get_many_methodologies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_methodologies_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_methodologies_response& v);
    static ores::dq::messaging::get_many_methodologies_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_many_methodologies_response& v);
    static ores::dq::messaging::get_many_methodologies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_methodology_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_methodology_request& v);
    static ores::dq::messaging::put_methodology_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_methodology_request& v);
    static ores::dq::messaging::put_methodology_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_methodology_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_methodology_response& v);
    static ores::dq::messaging::put_methodology_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_methodology_response& v);
    static ores::dq::messaging::put_methodology_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_methodologies_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_methodologies_request& v);
    static ores::dq::messaging::put_many_methodologies_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_many_methodologies_request& v);
    static ores::dq::messaging::put_many_methodologies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_methodologies_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_methodologies_response& v);
    static ores::dq::messaging::put_many_methodologies_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_many_methodologies_response& v);
    static ores::dq::messaging::put_many_methodologies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_methodology_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_methodology_request& v);
    static ores::dq::messaging::delete_methodology_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_methodology_request& v);
    static ores::dq::messaging::delete_methodology_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_methodology_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_methodology_response& v);
    static ores::dq::messaging::delete_methodology_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_methodology_response& v);
    static ores::dq::messaging::delete_methodology_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_methodologies_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_many_methodologies_request& v);
    static ores::dq::messaging::delete_many_methodologies_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_methodologies_request& v);
    static ores::dq::messaging::delete_many_methodologies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_methodologies_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_many_methodologies_response& v);
    static ores::dq::messaging::delete_many_methodologies_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_methodologies_response& v);
    static ores::dq::messaging::delete_many_methodologies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_methodology_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_methodology_versions_request& v);
    static ores::dq::messaging::list_methodology_versions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_methodology_versions_request& v);
    static ores::dq::messaging::list_methodology_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_methodology_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_methodology_versions_response& v);
    static ores::dq::messaging::list_methodology_versions_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_methodology_versions_response& v);
    static ores::dq::messaging::list_methodology_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_methodology_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_methodology_version_request& v);
    static ores::dq::messaging::get_methodology_version_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_methodology_version_request& v);
    static ores::dq::messaging::get_methodology_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_methodology_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_methodology_version_response& v);
    static ores::dq::messaging::get_methodology_version_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_methodology_version_response& v);
    static ores::dq::messaging::get_methodology_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
