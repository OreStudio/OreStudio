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
#ifndef ORES_IAM_API_MESSAGING_TENANT_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_TENANT_PROTOCOL_HPP

#include "ores.iam.api/domain/tenant.hpp"
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

struct tenant_key {
    std::string code;
};

struct tenant_write {
    boost::uuids::uuid id;
    std::string code;
    std::string name;
    std::string type;
    std::string description;
    std::string hostname;
    std::string status;
    bool is_registration_default;
};

struct tenant_change {
    tenant_write write;
    ores::utility::domain::precondition precondition;
};

struct tenant_removal {
    tenant_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct tenant_lookup {
    tenant_key key;
    std::optional<ores::iam::domain::tenant> tenant;
};

struct tenants_filter {
    std::optional<std::string> type;
    std::optional<std::string> status;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<std::string>> type_one_of;
    std::optional<std::vector<std::string>> status_one_of;
    std::optional<std::string> search;
};

struct tenant_event {
    boost::uuids::uuid event_id;
    tenant_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct tenant_version_key {
    tenant_key tenant;
    std::uint32_t version;
};

struct tenant_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_tenants_request {
    using response_type = struct list_tenants_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants.list";
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
    std::optional<tenants_filter> filter;
    std::optional<std::string> as_of;
};

struct list_tenants_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::tenant> tenants;
    std::uint64_t total;
};

struct get_tenant_request {
    using response_type = struct get_tenant_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenant_key key;
};

struct get_tenant_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::tenant> tenant;
};

struct get_many_tenants_request {
    using response_type = struct get_many_tenants_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tenant_key> keys;
};

struct get_many_tenants_response {
    ores::utility::domain::result result;
    std::vector<tenant_lookup> entries;
};

struct put_tenant_request {
    using response_type = struct put_tenant_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenant_change change;
    ores::utility::domain::change_intent intent;
};

struct put_tenant_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::tenant> tenant;
};

struct put_many_tenants_request {
    using response_type = struct put_many_tenants_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tenant_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_tenants_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::tenant> tenants;
};

struct delete_tenant_request {
    using response_type = struct delete_tenant_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenant_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_tenant_response {
    ores::utility::domain::result result;
};

struct delete_many_tenants_request {
    using response_type = struct delete_many_tenants_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tenant_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_tenants_response {
    ores::utility::domain::result result;
};

struct list_tenant_versions_request {
    using response_type = struct list_tenant_versions_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenant_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<tenant_versions_filter> filter;
};

struct list_tenant_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::tenant> versions;
    std::uint64_t total;
};

struct get_tenant_version_request {
    using response_type = struct get_tenant_version_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenant_version_key key;
};

struct get_tenant_version_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::tenant> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace tenant_event_subjects {
inline constexpr std::string_view created = "iam.v1.tenants_events.created";
inline constexpr std::string_view updated = "iam.v1.tenants_events.updated";
inline constexpr std::string_view deleted = "iam.v1.tenants_events.deleted";
}
/**
 * @brief The members of tenant_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenant_key&) {
    return std::tuple{
        utility::codec::field{"code", &tenant_key::code},
    };
}

/**
 * @brief The members of tenant_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenant_write&) {
    return std::tuple{
        utility::codec::field{"id", &tenant_write::id},
        utility::codec::field{"code", &tenant_write::code},
        utility::codec::field{"name", &tenant_write::name},
        utility::codec::field{"type", &tenant_write::type},
        utility::codec::field{"description", &tenant_write::description},
        utility::codec::field{"hostname", &tenant_write::hostname},
        utility::codec::field{"status", &tenant_write::status},
        utility::codec::field{"is_registration_default", &tenant_write::is_registration_default},
    };
}

/**
 * @brief The members of tenant_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenant_change&) {
    return std::tuple{
        utility::codec::field{"write", &tenant_change::write},
        utility::codec::field{"precondition", &tenant_change::precondition},
    };
}

/**
 * @brief The members of tenant_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenant_removal&) {
    return std::tuple{
        utility::codec::field{"key", &tenant_removal::key},
        utility::codec::field{"precondition", &tenant_removal::precondition},
    };
}

/**
 * @brief The members of tenant_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenant_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &tenant_lookup::key},
        utility::codec::field{"tenant", &tenant_lookup::tenant},
    };
}

/**
 * @brief The members of tenants_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenants_filter&) {
    return std::tuple{
        utility::codec::field{"type", &tenants_filter::type},
        utility::codec::field{"status", &tenants_filter::status},
        utility::codec::field{"id_one_of", &tenants_filter::id_one_of},
        utility::codec::field{"type_one_of", &tenants_filter::type_one_of},
        utility::codec::field{"status_one_of", &tenants_filter::status_one_of},
        utility::codec::field{"search", &tenants_filter::search},
    };
}

/**
 * @brief The members of tenant_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenant_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &tenant_event::event_id},
        utility::codec::field{"key", &tenant_event::key},
        utility::codec::field{"action", &tenant_event::action},
        utility::codec::field{"version", &tenant_event::version},
        utility::codec::field{"occurred_at", &tenant_event::occurred_at},
        utility::codec::field{"correlation_id", &tenant_event::correlation_id},
    };
}

/**
 * @brief The members of tenant_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenant_version_key&) {
    return std::tuple{
        utility::codec::field{"tenant", &tenant_version_key::tenant},
        utility::codec::field{"version", &tenant_version_key::version},
    };
}

/**
 * @brief The members of tenant_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenant_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &tenant_versions_filter::version},
        utility::codec::field{"from_version", &tenant_versions_filter::from_version},
        utility::codec::field{"to_version", &tenant_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_tenants_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_tenants_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_tenants_request::offset},
        utility::codec::field{"limit", &list_tenants_request::limit},
        utility::codec::field{"order", &list_tenants_request::order},
        utility::codec::field{"filter", &list_tenants_request::filter},
        utility::codec::field{"as_of", &list_tenants_request::as_of},
    };
}

/**
 * @brief The members of list_tenants_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_tenants_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_tenants_response::result},
        utility::codec::field{"tenants", &list_tenants_response::tenants},
        utility::codec::field{"total", &list_tenants_response::total},
    };
}

/**
 * @brief The members of get_tenant_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_tenant_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_tenant_request::key},
    };
}

/**
 * @brief The members of get_tenant_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_tenant_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_tenant_response::result},
        utility::codec::field{"tenant", &get_tenant_response::tenant},
    };
}

/**
 * @brief The members of get_many_tenants_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_tenants_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_tenants_request::keys},
    };
}

/**
 * @brief The members of get_many_tenants_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_tenants_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_tenants_response::result},
        utility::codec::field{"entries", &get_many_tenants_response::entries},
    };
}

/**
 * @brief The members of put_tenant_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_tenant_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_tenant_request::change},
        utility::codec::field{"intent", &put_tenant_request::intent},
    };
}

/**
 * @brief The members of put_tenant_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_tenant_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_tenant_response::result},
        utility::codec::field{"tenant", &put_tenant_response::tenant},
    };
}

/**
 * @brief The members of put_many_tenants_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_tenants_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_tenants_request::changes},
        utility::codec::field{"intent", &put_many_tenants_request::intent},
    };
}

/**
 * @brief The members of put_many_tenants_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_tenants_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_tenants_response::result},
        utility::codec::field{"tenants", &put_many_tenants_response::tenants},
    };
}

/**
 * @brief The members of delete_tenant_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_tenant_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_tenant_request::removal},
        utility::codec::field{"intent", &delete_tenant_request::intent},
    };
}

/**
 * @brief The members of delete_tenant_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_tenant_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_tenant_response::result},
    };
}

/**
 * @brief The members of delete_many_tenants_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_tenants_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_tenants_request::removals},
        utility::codec::field{"intent", &delete_many_tenants_request::intent},
    };
}

/**
 * @brief The members of delete_many_tenants_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_tenants_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_tenants_response::result},
    };
}

/**
 * @brief The members of list_tenant_versions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_tenant_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_tenant_versions_request::key},
        utility::codec::field{"offset", &list_tenant_versions_request::offset},
        utility::codec::field{"limit", &list_tenant_versions_request::limit},
        utility::codec::field{"order", &list_tenant_versions_request::order},
        utility::codec::field{"filter", &list_tenant_versions_request::filter},
    };
}

/**
 * @brief The members of list_tenant_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_tenant_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_tenant_versions_response::result},
        utility::codec::field{"versions", &list_tenant_versions_response::versions},
        utility::codec::field{"total", &list_tenant_versions_response::total},
    };
}

/**
 * @brief The members of get_tenant_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_tenant_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_tenant_version_request::key},
    };
}

/**
 * @brief The members of get_tenant_version_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_tenant_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_tenant_version_response::result},
        utility::codec::field{"version", &get_tenant_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::tenant_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::tenant_key& v);
    static ores::iam::messaging::tenant_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::tenant_key& v);
    static ores::iam::messaging::tenant_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::tenant_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::tenant_write& v);
    static ores::iam::messaging::tenant_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::tenant_write& v);
    static ores::iam::messaging::tenant_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::tenant_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::tenant_change& v);
    static ores::iam::messaging::tenant_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::tenant_change& v);
    static ores::iam::messaging::tenant_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::tenant_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::tenant_removal& v);
    static ores::iam::messaging::tenant_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::tenant_removal& v);
    static ores::iam::messaging::tenant_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::tenant_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::tenant_lookup& v);
    static ores::iam::messaging::tenant_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::tenant_lookup& v);
    static ores::iam::messaging::tenant_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::tenants_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::tenants_filter& v);
    static ores::iam::messaging::tenants_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::tenants_filter& v);
    static ores::iam::messaging::tenants_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::tenant_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::tenant_event& v);
    static ores::iam::messaging::tenant_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::tenant_event& v);
    static ores::iam::messaging::tenant_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::tenant_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::tenant_version_key& v);
    static ores::iam::messaging::tenant_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::tenant_version_key& v);
    static ores::iam::messaging::tenant_version_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::tenant_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::tenant_versions_filter& v);
    static ores::iam::messaging::tenant_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::tenant_versions_filter& v);
    static ores::iam::messaging::tenant_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_tenants_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_tenants_request& v);
    static ores::iam::messaging::list_tenants_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_tenants_request& v);
    static ores::iam::messaging::list_tenants_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_tenants_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_tenants_response& v);
    static ores::iam::messaging::list_tenants_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_tenants_response& v);
    static ores::iam::messaging::list_tenants_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_tenant_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_tenant_request& v);
    static ores::iam::messaging::get_tenant_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_tenant_request& v);
    static ores::iam::messaging::get_tenant_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_tenant_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_tenant_response& v);
    static ores::iam::messaging::get_tenant_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_tenant_response& v);
    static ores::iam::messaging::get_tenant_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_tenants_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_many_tenants_request& v);
    static ores::iam::messaging::get_many_tenants_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_many_tenants_request& v);
    static ores::iam::messaging::get_many_tenants_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_tenants_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_many_tenants_response& v);
    static ores::iam::messaging::get_many_tenants_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_many_tenants_response& v);
    static ores::iam::messaging::get_many_tenants_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_tenant_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_tenant_request& v);
    static ores::iam::messaging::put_tenant_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_tenant_request& v);
    static ores::iam::messaging::put_tenant_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_tenant_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_tenant_response& v);
    static ores::iam::messaging::put_tenant_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_tenant_response& v);
    static ores::iam::messaging::put_tenant_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_many_tenants_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_many_tenants_request& v);
    static ores::iam::messaging::put_many_tenants_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_many_tenants_request& v);
    static ores::iam::messaging::put_many_tenants_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_many_tenants_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_many_tenants_response& v);
    static ores::iam::messaging::put_many_tenants_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_many_tenants_response& v);
    static ores::iam::messaging::put_many_tenants_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_tenant_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_tenant_request& v);
    static ores::iam::messaging::delete_tenant_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::delete_tenant_request& v);
    static ores::iam::messaging::delete_tenant_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_tenant_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_tenant_response& v);
    static ores::iam::messaging::delete_tenant_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::delete_tenant_response& v);
    static ores::iam::messaging::delete_tenant_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_many_tenants_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_many_tenants_request& v);
    static ores::iam::messaging::delete_many_tenants_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::delete_many_tenants_request& v);
    static ores::iam::messaging::delete_many_tenants_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_many_tenants_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_many_tenants_response& v);
    static ores::iam::messaging::delete_many_tenants_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::delete_many_tenants_response& v);
    static ores::iam::messaging::delete_many_tenants_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_tenant_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_tenant_versions_request& v);
    static ores::iam::messaging::list_tenant_versions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_tenant_versions_request& v);
    static ores::iam::messaging::list_tenant_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_tenant_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_tenant_versions_response& v);
    static ores::iam::messaging::list_tenant_versions_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_tenant_versions_response& v);
    static ores::iam::messaging::list_tenant_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_tenant_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_tenant_version_request& v);
    static ores::iam::messaging::get_tenant_version_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_tenant_version_request& v);
    static ores::iam::messaging::get_tenant_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_tenant_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_tenant_version_response& v);
    static ores::iam::messaging::get_tenant_version_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_tenant_version_response& v);
    static ores::iam::messaging::get_tenant_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
