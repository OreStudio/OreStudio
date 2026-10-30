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
#ifndef ORES_DQ_API_MESSAGING_DATA_DOMAIN_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_DATA_DOMAIN_PROTOCOL_HPP

#include "ores.dq.api/domain/data_domain.hpp"
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

struct data_domain_key {
    std::string name;
};

struct data_domain_write {
    std::string name;
    std::string description;
};

struct data_domain_change {
    data_domain_write write;
    ores::utility::domain::precondition precondition;
};

struct data_domain_removal {
    data_domain_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct data_domain_lookup {
    data_domain_key key;
    std::optional<ores::dq::domain::data_domain> data_domain;
};

struct data_domains_filter {
    std::optional<std::vector<std::string>> name_one_of;
};

struct data_domain_event {
    boost::uuids::uuid event_id;
    data_domain_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct data_domain_version_key {
    data_domain_key data_domain;
    std::uint32_t version;
};

struct data_domain_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_data_domains_request {
    using response_type = struct list_data_domains_response;
    static constexpr std::string_view nats_subject = "dq.v1.data_domains.list";
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
    std::optional<data_domains_filter> filter;
    std::optional<std::string> as_of;
};

struct list_data_domains_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::data_domain> domains;
    std::uint64_t total;
};

struct get_data_domain_request {
    using response_type = struct get_data_domain_response;
    static constexpr std::string_view nats_subject = "dq.v1.data_domains.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    data_domain_key key;
};

struct get_data_domain_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::data_domain> data_domain;
};

struct get_many_data_domains_request {
    using response_type = struct get_many_data_domains_response;
    static constexpr std::string_view nats_subject = "dq.v1.data_domains.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<data_domain_key> keys;
};

struct get_many_data_domains_response {
    ores::utility::domain::result result;
    std::vector<data_domain_lookup> entries;
};

struct put_data_domain_request {
    using response_type = struct put_data_domain_response;
    static constexpr std::string_view nats_subject = "dq.v1.data_domains.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    data_domain_change change;
    ores::utility::domain::change_intent intent;
};

struct put_data_domain_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::data_domain> data_domain;
};

struct put_many_data_domains_request {
    using response_type = struct put_many_data_domains_response;
    static constexpr std::string_view nats_subject = "dq.v1.data_domains.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<data_domain_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_data_domains_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::data_domain> domains;
};

struct delete_data_domain_request {
    using response_type = struct delete_data_domain_response;
    static constexpr std::string_view nats_subject = "dq.v1.data_domains.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    data_domain_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_data_domain_response {
    ores::utility::domain::result result;
};

struct delete_many_data_domains_request {
    using response_type = struct delete_many_data_domains_response;
    static constexpr std::string_view nats_subject = "dq.v1.data_domains.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<data_domain_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_data_domains_response {
    ores::utility::domain::result result;
};

struct list_data_domain_versions_request {
    using response_type = struct list_data_domain_versions_response;
    static constexpr std::string_view nats_subject = "dq.v1.data_domains_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    data_domain_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<data_domain_versions_filter> filter;
};

struct list_data_domain_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::data_domain> versions;
    std::uint64_t total;
};

struct get_data_domain_version_request {
    using response_type = struct get_data_domain_version_response;
    static constexpr std::string_view nats_subject = "dq.v1.data_domains_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    data_domain_version_key key;
};

struct get_data_domain_version_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::data_domain> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace data_domain_event_subjects {
inline constexpr std::string_view created = "dq.v1.data_domains_events.created";
inline constexpr std::string_view updated = "dq.v1.data_domains_events.updated";
inline constexpr std::string_view deleted = "dq.v1.data_domains_events.deleted";
}
/**
 * @brief The members of data_domain_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const data_domain_key&) {
    return std::tuple{
        utility::codec::field{"name", &data_domain_key::name},
    };
}

/**
 * @brief The members of data_domain_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const data_domain_write&) {
    return std::tuple{
        utility::codec::field{"name", &data_domain_write::name},
        utility::codec::field{"description", &data_domain_write::description},
    };
}

/**
 * @brief The members of data_domain_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const data_domain_change&) {
    return std::tuple{
        utility::codec::field{"write", &data_domain_change::write},
        utility::codec::field{"precondition", &data_domain_change::precondition},
    };
}

/**
 * @brief The members of data_domain_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const data_domain_removal&) {
    return std::tuple{
        utility::codec::field{"key", &data_domain_removal::key},
        utility::codec::field{"precondition", &data_domain_removal::precondition},
    };
}

/**
 * @brief The members of data_domain_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const data_domain_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &data_domain_lookup::key},
        utility::codec::field{"data_domain", &data_domain_lookup::data_domain},
    };
}

/**
 * @brief The members of data_domains_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const data_domains_filter&) {
    return std::tuple{
        utility::codec::field{"name_one_of", &data_domains_filter::name_one_of},
    };
}

/**
 * @brief The members of data_domain_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const data_domain_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &data_domain_event::event_id},
        utility::codec::field{"key", &data_domain_event::key},
        utility::codec::field{"action", &data_domain_event::action},
        utility::codec::field{"version", &data_domain_event::version},
        utility::codec::field{"occurred_at", &data_domain_event::occurred_at},
        utility::codec::field{"correlation_id", &data_domain_event::correlation_id},
    };
}

/**
 * @brief The members of data_domain_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const data_domain_version_key&) {
    return std::tuple{
        utility::codec::field{"data_domain", &data_domain_version_key::data_domain},
        utility::codec::field{"version", &data_domain_version_key::version},
    };
}

/**
 * @brief The members of data_domain_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const data_domain_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &data_domain_versions_filter::version},
        utility::codec::field{"from_version", &data_domain_versions_filter::from_version},
        utility::codec::field{"to_version", &data_domain_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_data_domains_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_data_domains_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_data_domains_request::offset},
        utility::codec::field{"limit", &list_data_domains_request::limit},
        utility::codec::field{"order", &list_data_domains_request::order},
        utility::codec::field{"filter", &list_data_domains_request::filter},
        utility::codec::field{"as_of", &list_data_domains_request::as_of},
    };
}

/**
 * @brief The members of list_data_domains_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_data_domains_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_data_domains_response::result},
        utility::codec::field{"domains", &list_data_domains_response::domains},
        utility::codec::field{"total", &list_data_domains_response::total},
    };
}

/**
 * @brief The members of get_data_domain_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_data_domain_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_data_domain_request::key},
    };
}

/**
 * @brief The members of get_data_domain_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_data_domain_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_data_domain_response::result},
        utility::codec::field{"data_domain", &get_data_domain_response::data_domain},
    };
}

/**
 * @brief The members of get_many_data_domains_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_data_domains_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_data_domains_request::keys},
    };
}

/**
 * @brief The members of get_many_data_domains_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_data_domains_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_data_domains_response::result},
        utility::codec::field{"entries", &get_many_data_domains_response::entries},
    };
}

/**
 * @brief The members of put_data_domain_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_data_domain_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_data_domain_request::change},
        utility::codec::field{"intent", &put_data_domain_request::intent},
    };
}

/**
 * @brief The members of put_data_domain_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_data_domain_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_data_domain_response::result},
        utility::codec::field{"data_domain", &put_data_domain_response::data_domain},
    };
}

/**
 * @brief The members of put_many_data_domains_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_data_domains_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_data_domains_request::changes},
        utility::codec::field{"intent", &put_many_data_domains_request::intent},
    };
}

/**
 * @brief The members of put_many_data_domains_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_data_domains_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_data_domains_response::result},
        utility::codec::field{"domains", &put_many_data_domains_response::domains},
    };
}

/**
 * @brief The members of delete_data_domain_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_data_domain_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_data_domain_request::removal},
        utility::codec::field{"intent", &delete_data_domain_request::intent},
    };
}

/**
 * @brief The members of delete_data_domain_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_data_domain_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_data_domain_response::result},
    };
}

/**
 * @brief The members of delete_many_data_domains_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_data_domains_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_data_domains_request::removals},
        utility::codec::field{"intent", &delete_many_data_domains_request::intent},
    };
}

/**
 * @brief The members of delete_many_data_domains_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_data_domains_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_data_domains_response::result},
    };
}

/**
 * @brief The members of list_data_domain_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_data_domain_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_data_domain_versions_request::key},
        utility::codec::field{"offset", &list_data_domain_versions_request::offset},
        utility::codec::field{"limit", &list_data_domain_versions_request::limit},
        utility::codec::field{"order", &list_data_domain_versions_request::order},
        utility::codec::field{"filter", &list_data_domain_versions_request::filter},
    };
}

/**
 * @brief The members of list_data_domain_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_data_domain_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_data_domain_versions_response::result},
        utility::codec::field{"versions", &list_data_domain_versions_response::versions},
        utility::codec::field{"total", &list_data_domain_versions_response::total},
    };
}

/**
 * @brief The members of get_data_domain_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_data_domain_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_data_domain_version_request::key},
    };
}

/**
 * @brief The members of get_data_domain_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_data_domain_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_data_domain_version_response::result},
        utility::codec::field{"version", &get_data_domain_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::data_domain_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::data_domain_key& v);
    static ores::dq::messaging::data_domain_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::data_domain_key& v);
    static ores::dq::messaging::data_domain_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::data_domain_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::data_domain_write& v);
    static ores::dq::messaging::data_domain_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::data_domain_write& v);
    static ores::dq::messaging::data_domain_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::data_domain_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::data_domain_change& v);
    static ores::dq::messaging::data_domain_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::data_domain_change& v);
    static ores::dq::messaging::data_domain_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::data_domain_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::data_domain_removal& v);
    static ores::dq::messaging::data_domain_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::data_domain_removal& v);
    static ores::dq::messaging::data_domain_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::data_domain_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::data_domain_lookup& v);
    static ores::dq::messaging::data_domain_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::data_domain_lookup& v);
    static ores::dq::messaging::data_domain_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::data_domains_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::data_domains_filter& v);
    static ores::dq::messaging::data_domains_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::data_domains_filter& v);
    static ores::dq::messaging::data_domains_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::data_domain_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::data_domain_event& v);
    static ores::dq::messaging::data_domain_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::data_domain_event& v);
    static ores::dq::messaging::data_domain_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::data_domain_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::data_domain_version_key& v);
    static ores::dq::messaging::data_domain_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::data_domain_version_key& v);
    static ores::dq::messaging::data_domain_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::data_domain_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::data_domain_versions_filter& v);
    static ores::dq::messaging::data_domain_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::data_domain_versions_filter& v);
    static ores::dq::messaging::data_domain_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_data_domains_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_data_domains_request& v);
    static ores::dq::messaging::list_data_domains_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_data_domains_request& v);
    static ores::dq::messaging::list_data_domains_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_data_domains_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_data_domains_response& v);
    static ores::dq::messaging::list_data_domains_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_data_domains_response& v);
    static ores::dq::messaging::list_data_domains_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_data_domain_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_data_domain_request& v);
    static ores::dq::messaging::get_data_domain_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_data_domain_request& v);
    static ores::dq::messaging::get_data_domain_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_data_domain_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_data_domain_response& v);
    static ores::dq::messaging::get_data_domain_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_data_domain_response& v);
    static ores::dq::messaging::get_data_domain_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_data_domains_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_data_domains_request& v);
    static ores::dq::messaging::get_many_data_domains_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_many_data_domains_request& v);
    static ores::dq::messaging::get_many_data_domains_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_data_domains_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_data_domains_response& v);
    static ores::dq::messaging::get_many_data_domains_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_many_data_domains_response& v);
    static ores::dq::messaging::get_many_data_domains_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_data_domain_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_data_domain_request& v);
    static ores::dq::messaging::put_data_domain_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_data_domain_request& v);
    static ores::dq::messaging::put_data_domain_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_data_domain_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_data_domain_response& v);
    static ores::dq::messaging::put_data_domain_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_data_domain_response& v);
    static ores::dq::messaging::put_data_domain_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_data_domains_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_data_domains_request& v);
    static ores::dq::messaging::put_many_data_domains_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_many_data_domains_request& v);
    static ores::dq::messaging::put_many_data_domains_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_data_domains_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_data_domains_response& v);
    static ores::dq::messaging::put_many_data_domains_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_many_data_domains_response& v);
    static ores::dq::messaging::put_many_data_domains_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_data_domain_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_data_domain_request& v);
    static ores::dq::messaging::delete_data_domain_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_data_domain_request& v);
    static ores::dq::messaging::delete_data_domain_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_data_domain_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_data_domain_response& v);
    static ores::dq::messaging::delete_data_domain_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_data_domain_response& v);
    static ores::dq::messaging::delete_data_domain_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_data_domains_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_many_data_domains_request& v);
    static ores::dq::messaging::delete_many_data_domains_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_data_domains_request& v);
    static ores::dq::messaging::delete_many_data_domains_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_data_domains_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_many_data_domains_response& v);
    static ores::dq::messaging::delete_many_data_domains_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_data_domains_response& v);
    static ores::dq::messaging::delete_many_data_domains_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_data_domain_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_data_domain_versions_request& v);
    static ores::dq::messaging::list_data_domain_versions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_data_domain_versions_request& v);
    static ores::dq::messaging::list_data_domain_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_data_domain_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_data_domain_versions_response& v);
    static ores::dq::messaging::list_data_domain_versions_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_data_domain_versions_response& v);
    static ores::dq::messaging::list_data_domain_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_data_domain_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_data_domain_version_request& v);
    static ores::dq::messaging::get_data_domain_version_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_data_domain_version_request& v);
    static ores::dq::messaging::get_data_domain_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_data_domain_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_data_domain_version_response& v);
    static ores::dq::messaging::get_data_domain_version_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_data_domain_version_response& v);
    static ores::dq::messaging::get_data_domain_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
