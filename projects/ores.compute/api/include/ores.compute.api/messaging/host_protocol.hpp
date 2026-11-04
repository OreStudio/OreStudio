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
#ifndef ORES_COMPUTE_API_MESSAGING_HOST_PROTOCOL_HPP
#define ORES_COMPUTE_API_MESSAGING_HOST_PROTOCOL_HPP

#include "ores.compute.api/domain/host.hpp"
#include "ores.compute.api/export.hpp"
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

namespace ores::compute::messaging {

struct host_key {
    std::string external_id;

    friend bool operator==(const host_key&, const host_key&) = default;
};

struct host_write {
    boost::uuids::uuid id;
    std::string external_id;
    std::string location;
    int cpu_count;
    std::int64_t ram_mb;
    std::string gpu_type;
    std::string display_name;
    double credit_total;

    friend bool operator==(const host_write&, const host_write&) = default;
};

struct host_change {
    host_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const host_change&, const host_change&) = default;
};

struct host_removal {
    host_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const host_removal&, const host_removal&) = default;
};

struct host_lookup {
    host_key key;
    std::optional<ores::compute::domain::host> host;

    friend bool operator==(const host_lookup&, const host_lookup&) = default;
};

struct hosts_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;

    friend bool operator==(const hosts_filter&, const hosts_filter&) = default;
};

struct host_event {
    boost::uuids::uuid event_id;
    host_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const host_event&, const host_event&) = default;
};

struct host_version_key {
    host_key host;
    std::uint32_t version;

    friend bool operator==(const host_version_key&, const host_version_key&) = default;
};

struct host_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const host_versions_filter&, const host_versions_filter&) = default;
};

struct list_hosts_request {
    using response_type = struct list_hosts_response;
    static constexpr std::string_view nats_subject = "compute.v1.hosts.list";
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
    std::optional<hosts_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_hosts_request&, const list_hosts_request&) = default;
};

struct list_hosts_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::host> hosts;
    std::uint64_t total;

    friend bool operator==(const list_hosts_response&, const list_hosts_response&) = default;
};

struct get_host_request {
    using response_type = struct get_host_response;
    static constexpr std::string_view nats_subject = "compute.v1.hosts.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    host_key key;

    friend bool operator==(const get_host_request&, const get_host_request&) = default;
};

struct get_host_response {
    ores::utility::domain::result result;
    std::optional<ores::compute::domain::host> host;

    friend bool operator==(const get_host_response&, const get_host_response&) = default;
};

struct get_many_hosts_request {
    using response_type = struct get_many_hosts_response;
    static constexpr std::string_view nats_subject = "compute.v1.hosts.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<host_key> keys;

    friend bool operator==(const get_many_hosts_request&, const get_many_hosts_request&) = default;
};

struct get_many_hosts_response {
    ores::utility::domain::result result;
    std::vector<host_lookup> entries;

    friend bool operator==(const get_many_hosts_response&,
                           const get_many_hosts_response&) = default;
};

struct put_host_request {
    using response_type = struct put_host_response;
    static constexpr std::string_view nats_subject = "compute.v1.hosts.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    host_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_host_request&, const put_host_request&) = default;
};

struct put_host_response {
    ores::utility::domain::result result;
    std::optional<ores::compute::domain::host> host;

    friend bool operator==(const put_host_response&, const put_host_response&) = default;
};

struct put_many_hosts_request {
    using response_type = struct put_many_hosts_response;
    static constexpr std::string_view nats_subject = "compute.v1.hosts.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<host_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_hosts_request&, const put_many_hosts_request&) = default;
};

struct put_many_hosts_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::host> hosts;

    friend bool operator==(const put_many_hosts_response&,
                           const put_many_hosts_response&) = default;
};

struct delete_host_request {
    using response_type = struct delete_host_response;
    static constexpr std::string_view nats_subject = "compute.v1.hosts.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    host_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_host_request&, const delete_host_request&) = default;
};

struct delete_host_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_host_response&, const delete_host_response&) = default;
};

struct delete_many_hosts_request {
    using response_type = struct delete_many_hosts_response;
    static constexpr std::string_view nats_subject = "compute.v1.hosts.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<host_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_hosts_request&,
                           const delete_many_hosts_request&) = default;
};

struct delete_many_hosts_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_hosts_response&,
                           const delete_many_hosts_response&) = default;
};

struct list_host_versions_request {
    using response_type = struct list_host_versions_response;
    static constexpr std::string_view nats_subject = "compute.v1.hosts_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    host_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<host_versions_filter> filter;

    friend bool operator==(const list_host_versions_request&,
                           const list_host_versions_request&) = default;
};

struct list_host_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::host> versions;
    std::uint64_t total;

    friend bool operator==(const list_host_versions_response&,
                           const list_host_versions_response&) = default;
};

struct get_host_version_request {
    using response_type = struct get_host_version_response;
    static constexpr std::string_view nats_subject = "compute.v1.hosts_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    host_version_key key;

    friend bool operator==(const get_host_version_request&,
                           const get_host_version_request&) = default;
};

struct get_host_version_response {
    ores::utility::domain::result result;
    std::optional<ores::compute::domain::host> version;

    friend bool operator==(const get_host_version_response&,
                           const get_host_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace host_event_subjects {
inline constexpr std::string_view created = "compute.v1.events.hosts.created";
inline constexpr std::string_view updated = "compute.v1.events.hosts.updated";
inline constexpr std::string_view deleted = "compute.v1.events.hosts.deleted";
}
/**
 * @brief The members of host_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const host_key&) {
    return std::tuple{
        utility::codec::field{"external_id", &host_key::external_id},
    };
}

/**
 * @brief The members of host_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const host_write&) {
    return std::tuple{
        utility::codec::field{"id", &host_write::id},
        utility::codec::field{"external_id", &host_write::external_id},
        utility::codec::field{"location", &host_write::location},
        utility::codec::field{"cpu_count", &host_write::cpu_count},
        utility::codec::field{"ram_mb", &host_write::ram_mb},
        utility::codec::field{"gpu_type", &host_write::gpu_type},
        utility::codec::field{"display_name", &host_write::display_name},
        utility::codec::field{"credit_total", &host_write::credit_total},
    };
}

/**
 * @brief The members of host_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const host_change&) {
    return std::tuple{
        utility::codec::field{"write", &host_change::write},
        utility::codec::field{"precondition", &host_change::precondition},
    };
}

/**
 * @brief The members of host_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const host_removal&) {
    return std::tuple{
        utility::codec::field{"key", &host_removal::key},
        utility::codec::field{"precondition", &host_removal::precondition},
    };
}

/**
 * @brief The members of host_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const host_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &host_lookup::key},
        utility::codec::field{"host", &host_lookup::host},
    };
}

/**
 * @brief The members of hosts_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const hosts_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &hosts_filter::id_one_of},
    };
}

/**
 * @brief The members of host_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const host_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &host_event::event_id},
        utility::codec::field{"key", &host_event::key},
        utility::codec::field{"action", &host_event::action},
        utility::codec::field{"version", &host_event::version},
        utility::codec::field{"occurred_at", &host_event::occurred_at},
        utility::codec::field{"correlation_id", &host_event::correlation_id},
    };
}

/**
 * @brief The members of host_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const host_version_key&) {
    return std::tuple{
        utility::codec::field{"host", &host_version_key::host},
        utility::codec::field{"version", &host_version_key::version},
    };
}

/**
 * @brief The members of host_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const host_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &host_versions_filter::version},
        utility::codec::field{"from_version", &host_versions_filter::from_version},
        utility::codec::field{"to_version", &host_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_hosts_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_hosts_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_hosts_request::offset},
        utility::codec::field{"limit", &list_hosts_request::limit},
        utility::codec::field{"order", &list_hosts_request::order},
        utility::codec::field{"filter", &list_hosts_request::filter},
        utility::codec::field{"as_of", &list_hosts_request::as_of},
    };
}

/**
 * @brief The members of list_hosts_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_hosts_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_hosts_response::result},
        utility::codec::field{"hosts", &list_hosts_response::hosts},
        utility::codec::field{"total", &list_hosts_response::total},
    };
}

/**
 * @brief The members of get_host_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_host_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_host_request::key},
    };
}

/**
 * @brief The members of get_host_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_host_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_host_response::result},
        utility::codec::field{"host", &get_host_response::host},
    };
}

/**
 * @brief The members of get_many_hosts_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_hosts_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_hosts_request::keys},
    };
}

/**
 * @brief The members of get_many_hosts_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_hosts_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_hosts_response::result},
        utility::codec::field{"entries", &get_many_hosts_response::entries},
    };
}

/**
 * @brief The members of put_host_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_host_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_host_request::change},
        utility::codec::field{"intent", &put_host_request::intent},
    };
}

/**
 * @brief The members of put_host_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_host_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_host_response::result},
        utility::codec::field{"host", &put_host_response::host},
    };
}

/**
 * @brief The members of put_many_hosts_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_hosts_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_hosts_request::changes},
        utility::codec::field{"intent", &put_many_hosts_request::intent},
    };
}

/**
 * @brief The members of put_many_hosts_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_hosts_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_hosts_response::result},
        utility::codec::field{"hosts", &put_many_hosts_response::hosts},
    };
}

/**
 * @brief The members of delete_host_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_host_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_host_request::removal},
        utility::codec::field{"intent", &delete_host_request::intent},
    };
}

/**
 * @brief The members of delete_host_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_host_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_host_response::result},
    };
}

/**
 * @brief The members of delete_many_hosts_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_hosts_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_hosts_request::removals},
        utility::codec::field{"intent", &delete_many_hosts_request::intent},
    };
}

/**
 * @brief The members of delete_many_hosts_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_hosts_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_hosts_response::result},
    };
}

/**
 * @brief The members of list_host_versions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_host_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_host_versions_request::key},
        utility::codec::field{"offset", &list_host_versions_request::offset},
        utility::codec::field{"limit", &list_host_versions_request::limit},
        utility::codec::field{"order", &list_host_versions_request::order},
        utility::codec::field{"filter", &list_host_versions_request::filter},
    };
}

/**
 * @brief The members of list_host_versions_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_host_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_host_versions_response::result},
        utility::codec::field{"versions", &list_host_versions_response::versions},
        utility::codec::field{"total", &list_host_versions_response::total},
    };
}

/**
 * @brief The members of get_host_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_host_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_host_version_request::key},
    };
}

/**
 * @brief The members of get_host_version_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_host_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_host_version_response::result},
        utility::codec::field{"version", &get_host_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::host_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::host_key& v);
    static ores::compute::messaging::host_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::host_key& v);
    static ores::compute::messaging::host_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::host_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::host_write& v);
    static ores::compute::messaging::host_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::host_write& v);
    static ores::compute::messaging::host_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::host_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::host_change& v);
    static ores::compute::messaging::host_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::host_change& v);
    static ores::compute::messaging::host_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::host_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::host_removal& v);
    static ores::compute::messaging::host_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::host_removal& v);
    static ores::compute::messaging::host_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::host_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::host_lookup& v);
    static ores::compute::messaging::host_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::host_lookup& v);
    static ores::compute::messaging::host_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::hosts_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::hosts_filter& v);
    static ores::compute::messaging::hosts_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::hosts_filter& v);
    static ores::compute::messaging::hosts_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::host_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::host_event& v);
    static ores::compute::messaging::host_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::host_event& v);
    static ores::compute::messaging::host_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::host_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::host_version_key& v);
    static ores::compute::messaging::host_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::host_version_key& v);
    static ores::compute::messaging::host_version_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::host_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::host_versions_filter& v);
    static ores::compute::messaging::host_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::host_versions_filter& v);
    static ores::compute::messaging::host_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::list_hosts_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::list_hosts_request& v);
    static ores::compute::messaging::list_hosts_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::list_hosts_request& v);
    static ores::compute::messaging::list_hosts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::list_hosts_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::list_hosts_response& v);
    static ores::compute::messaging::list_hosts_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::list_hosts_response& v);
    static ores::compute::messaging::list_hosts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::get_host_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::get_host_request& v);
    static ores::compute::messaging::get_host_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::get_host_request& v);
    static ores::compute::messaging::get_host_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::get_host_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::get_host_response& v);
    static ores::compute::messaging::get_host_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::get_host_response& v);
    static ores::compute::messaging::get_host_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::get_many_hosts_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::get_many_hosts_request& v);
    static ores::compute::messaging::get_many_hosts_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::get_many_hosts_request& v);
    static ores::compute::messaging::get_many_hosts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::get_many_hosts_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::get_many_hosts_response& v);
    static ores::compute::messaging::get_many_hosts_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::get_many_hosts_response& v);
    static ores::compute::messaging::get_many_hosts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::put_host_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::put_host_request& v);
    static ores::compute::messaging::put_host_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::put_host_request& v);
    static ores::compute::messaging::put_host_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::put_host_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::put_host_response& v);
    static ores::compute::messaging::put_host_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::put_host_response& v);
    static ores::compute::messaging::put_host_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::put_many_hosts_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::put_many_hosts_request& v);
    static ores::compute::messaging::put_many_hosts_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::put_many_hosts_request& v);
    static ores::compute::messaging::put_many_hosts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::put_many_hosts_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::put_many_hosts_response& v);
    static ores::compute::messaging::put_many_hosts_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::put_many_hosts_response& v);
    static ores::compute::messaging::put_many_hosts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::delete_host_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::delete_host_request& v);
    static ores::compute::messaging::delete_host_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::delete_host_request& v);
    static ores::compute::messaging::delete_host_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::delete_host_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::delete_host_response& v);
    static ores::compute::messaging::delete_host_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::delete_host_response& v);
    static ores::compute::messaging::delete_host_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::delete_many_hosts_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::delete_many_hosts_request& v);
    static ores::compute::messaging::delete_many_hosts_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::delete_many_hosts_request& v);
    static ores::compute::messaging::delete_many_hosts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::delete_many_hosts_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::delete_many_hosts_response& v);
    static ores::compute::messaging::delete_many_hosts_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::delete_many_hosts_response& v);
    static ores::compute::messaging::delete_many_hosts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::list_host_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::list_host_versions_request& v);
    static ores::compute::messaging::list_host_versions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::list_host_versions_request& v);
    static ores::compute::messaging::list_host_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::list_host_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::list_host_versions_response& v);
    static ores::compute::messaging::list_host_versions_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::list_host_versions_response& v);
    static ores::compute::messaging::list_host_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::get_host_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::get_host_version_request& v);
    static ores::compute::messaging::get_host_version_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::get_host_version_request& v);
    static ores::compute::messaging::get_host_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::get_host_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::get_host_version_response& v);
    static ores::compute::messaging::get_host_version_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::get_host_version_response& v);
    static ores::compute::messaging::get_host_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
