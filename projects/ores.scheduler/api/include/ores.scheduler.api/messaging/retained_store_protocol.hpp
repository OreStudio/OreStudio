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
#ifndef ORES_SCHEDULER_API_MESSAGING_RETAINED_STORE_PROTOCOL_HPP
#define ORES_SCHEDULER_API_MESSAGING_RETAINED_STORE_PROTOCOL_HPP

#include "ores.scheduler.api/domain/retained_store_codec.hpp"
#include "ores.scheduler.api/export.hpp"
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

namespace ores::scheduler::messaging {

struct retained_store_key {
    std::string code;

    friend bool operator==(const retained_store_key&, const retained_store_key&) = default;
};

struct retained_store_write {
    std::string code;
    std::string display_name;
    std::string description;
    std::string owner_component;
    std::string scope;
    std::string target_kind;
    std::string target;
    std::optional<std::string> time_column;
    std::string measure_kind;
    std::string bound_kind;
    std::optional<std::string> default_action;
    std::optional<int> default_retention_days;
    int default_cadence_hours{};
    std::optional<std::string> default_reason;

    friend bool operator==(const retained_store_write&, const retained_store_write&) = default;
};

struct retained_store_change {
    retained_store_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const retained_store_change&, const retained_store_change&) = default;
};

struct retained_store_removal {
    retained_store_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const retained_store_removal&, const retained_store_removal&) = default;
};

struct retained_store_lookup {
    retained_store_key key;
    std::optional<ores::scheduler::domain::retained_store> retained_store;

    friend bool operator==(const retained_store_lookup&, const retained_store_lookup&) = default;
};

struct retained_stores_filter {
    std::optional<std::vector<std::string>> code_one_of;

    friend bool operator==(const retained_stores_filter&, const retained_stores_filter&) = default;
};

struct retained_store_event {
    boost::uuids::uuid event_id;
    retained_store_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const retained_store_event&, const retained_store_event&) = default;
};

struct retained_store_version_key {
    retained_store_key retained_store;
    std::uint32_t version{};

    friend bool operator==(const retained_store_version_key&,
                           const retained_store_version_key&) = default;
};

struct retained_store_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const retained_store_versions_filter&,
                           const retained_store_versions_filter&) = default;
};

struct list_retained_stores_request {
    using response_type = struct list_retained_stores_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.retained_stores.list";
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
    std::optional<retained_stores_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_retained_stores_request&,
                           const list_retained_stores_request&) = default;
};

struct list_retained_stores_response {
    ores::utility::domain::result result;
    std::vector<ores::scheduler::domain::retained_store> stores;
    std::uint64_t total{};

    friend bool operator==(const list_retained_stores_response&,
                           const list_retained_stores_response&) = default;
};

struct get_retained_store_request {
    using response_type = struct get_retained_store_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.retained_stores.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    retained_store_key key;

    friend bool operator==(const get_retained_store_request&,
                           const get_retained_store_request&) = default;
};

struct get_retained_store_response {
    ores::utility::domain::result result;
    std::optional<ores::scheduler::domain::retained_store> retained_store;

    friend bool operator==(const get_retained_store_response&,
                           const get_retained_store_response&) = default;
};

struct get_many_retained_stores_request {
    using response_type = struct get_many_retained_stores_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.retained_stores.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<retained_store_key> keys;

    friend bool operator==(const get_many_retained_stores_request&,
                           const get_many_retained_stores_request&) = default;
};

struct get_many_retained_stores_response {
    ores::utility::domain::result result;
    std::vector<retained_store_lookup> entries;

    friend bool operator==(const get_many_retained_stores_response&,
                           const get_many_retained_stores_response&) = default;
};

struct put_retained_store_request {
    using response_type = struct put_retained_store_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.retained_stores.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    retained_store_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_retained_store_request&,
                           const put_retained_store_request&) = default;
};

struct put_retained_store_response {
    ores::utility::domain::result result;
    std::optional<ores::scheduler::domain::retained_store> retained_store;

    friend bool operator==(const put_retained_store_response&,
                           const put_retained_store_response&) = default;
};

struct put_many_retained_stores_request {
    using response_type = struct put_many_retained_stores_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.retained_stores.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<retained_store_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_retained_stores_request&,
                           const put_many_retained_stores_request&) = default;
};

struct put_many_retained_stores_response {
    ores::utility::domain::result result;
    std::vector<ores::scheduler::domain::retained_store> stores;

    friend bool operator==(const put_many_retained_stores_response&,
                           const put_many_retained_stores_response&) = default;
};

struct delete_retained_store_request {
    using response_type = struct delete_retained_store_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.retained_stores.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    retained_store_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_retained_store_request&,
                           const delete_retained_store_request&) = default;
};

struct delete_retained_store_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_retained_store_response&,
                           const delete_retained_store_response&) = default;
};

struct delete_many_retained_stores_request {
    using response_type = struct delete_many_retained_stores_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.retained_stores.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<retained_store_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_retained_stores_request&,
                           const delete_many_retained_stores_request&) = default;
};

struct delete_many_retained_stores_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_retained_stores_response&,
                           const delete_many_retained_stores_response&) = default;
};

struct list_retained_store_versions_request {
    using response_type = struct list_retained_store_versions_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.retained_stores_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    retained_store_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<retained_store_versions_filter> filter;

    friend bool operator==(const list_retained_store_versions_request&,
                           const list_retained_store_versions_request&) = default;
};

struct list_retained_store_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::scheduler::domain::retained_store> versions;
    std::uint64_t total{};

    friend bool operator==(const list_retained_store_versions_response&,
                           const list_retained_store_versions_response&) = default;
};

struct get_retained_store_version_request {
    using response_type = struct get_retained_store_version_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.retained_stores_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    retained_store_version_key key;

    friend bool operator==(const get_retained_store_version_request&,
                           const get_retained_store_version_request&) = default;
};

struct get_retained_store_version_response {
    ores::utility::domain::result result;
    std::optional<ores::scheduler::domain::retained_store> version;

    friend bool operator==(const get_retained_store_version_response&,
                           const get_retained_store_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace retained_store_event_subjects {
inline constexpr std::string_view created = "scheduler.v1.events.retained_stores.created";
inline constexpr std::string_view updated = "scheduler.v1.events.retained_stores.updated";
inline constexpr std::string_view deleted = "scheduler.v1.events.retained_stores.deleted";
}
/**
 * @brief The members of retained_store_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<retained_store_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"code", &retained_store_key::code},
    };
}

/**
 * @brief The members of retained_store_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<retained_store_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"code", &retained_store_write::code},
        utility::codec::field{"display_name", &retained_store_write::display_name},
        utility::codec::field{"description", &retained_store_write::description},
        utility::codec::field{"owner_component", &retained_store_write::owner_component},
        utility::codec::field{"scope", &retained_store_write::scope},
        utility::codec::field{"target_kind", &retained_store_write::target_kind},
        utility::codec::field{"target", &retained_store_write::target},
        utility::codec::field{"time_column", &retained_store_write::time_column},
        utility::codec::field{"measure_kind", &retained_store_write::measure_kind},
        utility::codec::field{"bound_kind", &retained_store_write::bound_kind},
        utility::codec::field{"default_action", &retained_store_write::default_action},
        utility::codec::field{"default_retention_days",
                              &retained_store_write::default_retention_days},
        utility::codec::field{"default_cadence_hours",
                              &retained_store_write::default_cadence_hours},
        utility::codec::field{"default_reason", &retained_store_write::default_reason},
    };
}

/**
 * @brief The members of retained_store_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<retained_store_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &retained_store_change::write},
        utility::codec::field{"precondition", &retained_store_change::precondition},
    };
}

/**
 * @brief The members of retained_store_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<retained_store_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &retained_store_removal::key},
        utility::codec::field{"precondition", &retained_store_removal::precondition},
    };
}

/**
 * @brief The members of retained_store_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<retained_store_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &retained_store_lookup::key},
        utility::codec::field{"retained_store", &retained_store_lookup::retained_store},
    };
}

/**
 * @brief The members of retained_stores_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<retained_stores_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"code_one_of", &retained_stores_filter::code_one_of},
    };
}

/**
 * @brief The members of retained_store_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<retained_store_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &retained_store_event::event_id},
        utility::codec::field{"key", &retained_store_event::key},
        utility::codec::field{"action", &retained_store_event::action},
        utility::codec::field{"version", &retained_store_event::version},
        utility::codec::field{"occurred_at", &retained_store_event::occurred_at},
        utility::codec::field{"correlation_id", &retained_store_event::correlation_id},
    };
}

/**
 * @brief The members of retained_store_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<retained_store_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"retained_store", &retained_store_version_key::retained_store},
        utility::codec::field{"version", &retained_store_version_key::version},
    };
}

/**
 * @brief The members of retained_store_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<retained_store_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &retained_store_versions_filter::version},
        utility::codec::field{"from_version", &retained_store_versions_filter::from_version},
        utility::codec::field{"to_version", &retained_store_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_retained_stores_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_retained_stores_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_retained_stores_request::offset},
        utility::codec::field{"limit", &list_retained_stores_request::limit},
        utility::codec::field{"order", &list_retained_stores_request::order},
        utility::codec::field{"filter", &list_retained_stores_request::filter},
        utility::codec::field{"as_of", &list_retained_stores_request::as_of},
    };
}

/**
 * @brief The members of list_retained_stores_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_retained_stores_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_retained_stores_response::result},
        utility::codec::field{"stores", &list_retained_stores_response::stores},
        utility::codec::field{"total", &list_retained_stores_response::total},
    };
}

/**
 * @brief The members of get_retained_store_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_retained_store_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_retained_store_request::key},
    };
}

/**
 * @brief The members of get_retained_store_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_retained_store_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_retained_store_response::result},
        utility::codec::field{"retained_store", &get_retained_store_response::retained_store},
    };
}

/**
 * @brief The members of get_many_retained_stores_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_retained_stores_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_retained_stores_request::keys},
    };
}

/**
 * @brief The members of get_many_retained_stores_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_retained_stores_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_retained_stores_response::result},
        utility::codec::field{"entries", &get_many_retained_stores_response::entries},
    };
}

/**
 * @brief The members of put_retained_store_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_retained_store_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_retained_store_request::change},
        utility::codec::field{"intent", &put_retained_store_request::intent},
    };
}

/**
 * @brief The members of put_retained_store_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_retained_store_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_retained_store_response::result},
        utility::codec::field{"retained_store", &put_retained_store_response::retained_store},
    };
}

/**
 * @brief The members of put_many_retained_stores_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_retained_stores_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_retained_stores_request::changes},
        utility::codec::field{"intent", &put_many_retained_stores_request::intent},
    };
}

/**
 * @brief The members of put_many_retained_stores_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_retained_stores_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_retained_stores_response::result},
        utility::codec::field{"stores", &put_many_retained_stores_response::stores},
    };
}

/**
 * @brief The members of delete_retained_store_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_retained_store_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_retained_store_request::removal},
        utility::codec::field{"intent", &delete_retained_store_request::intent},
    };
}

/**
 * @brief The members of delete_retained_store_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_retained_store_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_retained_store_response::result},
    };
}

/**
 * @brief The members of delete_many_retained_stores_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_retained_stores_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_retained_stores_request::removals},
        utility::codec::field{"intent", &delete_many_retained_stores_request::intent},
    };
}

/**
 * @brief The members of delete_many_retained_stores_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_retained_stores_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_retained_stores_response::result},
    };
}

/**
 * @brief The members of list_retained_store_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_retained_store_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_retained_store_versions_request::key},
        utility::codec::field{"offset", &list_retained_store_versions_request::offset},
        utility::codec::field{"limit", &list_retained_store_versions_request::limit},
        utility::codec::field{"order", &list_retained_store_versions_request::order},
        utility::codec::field{"filter", &list_retained_store_versions_request::filter},
    };
}

/**
 * @brief The members of list_retained_store_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_retained_store_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_retained_store_versions_response::result},
        utility::codec::field{"versions", &list_retained_store_versions_response::versions},
        utility::codec::field{"total", &list_retained_store_versions_response::total},
    };
}

/**
 * @brief The members of get_retained_store_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_retained_store_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_retained_store_version_request::key},
    };
}

/**
 * @brief The members of get_retained_store_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_retained_store_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_retained_store_version_response::result},
        utility::codec::field{"version", &get_retained_store_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_SCHEDULER_API_EXPORT wire_traits<ores::scheduler::messaging::retained_store_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::retained_store_key& v);
    static ores::scheduler::messaging::retained_store_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::scheduler::messaging::retained_store_key& v);
    static ores::scheduler::messaging::retained_store_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT wire_traits<ores::scheduler::messaging::retained_store_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::retained_store_write& v);
    static ores::scheduler::messaging::retained_store_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::scheduler::messaging::retained_store_write& v);
    static ores::scheduler::messaging::retained_store_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT wire_traits<ores::scheduler::messaging::retained_store_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::retained_store_change& v);
    static ores::scheduler::messaging::retained_store_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::scheduler::messaging::retained_store_change& v);
    static ores::scheduler::messaging::retained_store_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT wire_traits<ores::scheduler::messaging::retained_store_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::retained_store_removal& v);
    static ores::scheduler::messaging::retained_store_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::scheduler::messaging::retained_store_removal& v);
    static ores::scheduler::messaging::retained_store_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT wire_traits<ores::scheduler::messaging::retained_store_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::retained_store_lookup& v);
    static ores::scheduler::messaging::retained_store_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::scheduler::messaging::retained_store_lookup& v);
    static ores::scheduler::messaging::retained_store_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT wire_traits<ores::scheduler::messaging::retained_stores_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::retained_stores_filter& v);
    static ores::scheduler::messaging::retained_stores_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::scheduler::messaging::retained_stores_filter& v);
    static ores::scheduler::messaging::retained_stores_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT wire_traits<ores::scheduler::messaging::retained_store_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::retained_store_event& v);
    static ores::scheduler::messaging::retained_store_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::scheduler::messaging::retained_store_event& v);
    static ores::scheduler::messaging::retained_store_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::retained_store_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::retained_store_version_key& v);
    static ores::scheduler::messaging::retained_store_version_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::retained_store_version_key& v);
    static ores::scheduler::messaging::retained_store_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::retained_store_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::retained_store_versions_filter& v);
    static ores::scheduler::messaging::retained_store_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::retained_store_versions_filter& v);
    static ores::scheduler::messaging::retained_store_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::list_retained_stores_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::list_retained_stores_request& v);
    static ores::scheduler::messaging::list_retained_stores_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::list_retained_stores_request& v);
    static ores::scheduler::messaging::list_retained_stores_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::list_retained_stores_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::list_retained_stores_response& v);
    static ores::scheduler::messaging::list_retained_stores_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::list_retained_stores_response& v);
    static ores::scheduler::messaging::list_retained_stores_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::get_retained_store_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::get_retained_store_request& v);
    static ores::scheduler::messaging::get_retained_store_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::get_retained_store_request& v);
    static ores::scheduler::messaging::get_retained_store_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::get_retained_store_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::get_retained_store_response& v);
    static ores::scheduler::messaging::get_retained_store_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::get_retained_store_response& v);
    static ores::scheduler::messaging::get_retained_store_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::get_many_retained_stores_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::get_many_retained_stores_request& v);
    static ores::scheduler::messaging::get_many_retained_stores_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::get_many_retained_stores_request& v);
    static ores::scheduler::messaging::get_many_retained_stores_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::get_many_retained_stores_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::get_many_retained_stores_response& v);
    static ores::scheduler::messaging::get_many_retained_stores_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::get_many_retained_stores_response& v);
    static ores::scheduler::messaging::get_many_retained_stores_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::put_retained_store_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::put_retained_store_request& v);
    static ores::scheduler::messaging::put_retained_store_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::put_retained_store_request& v);
    static ores::scheduler::messaging::put_retained_store_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::put_retained_store_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::put_retained_store_response& v);
    static ores::scheduler::messaging::put_retained_store_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::put_retained_store_response& v);
    static ores::scheduler::messaging::put_retained_store_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::put_many_retained_stores_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::put_many_retained_stores_request& v);
    static ores::scheduler::messaging::put_many_retained_stores_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::put_many_retained_stores_request& v);
    static ores::scheduler::messaging::put_many_retained_stores_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::put_many_retained_stores_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::put_many_retained_stores_response& v);
    static ores::scheduler::messaging::put_many_retained_stores_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::put_many_retained_stores_response& v);
    static ores::scheduler::messaging::put_many_retained_stores_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::delete_retained_store_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::delete_retained_store_request& v);
    static ores::scheduler::messaging::delete_retained_store_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::delete_retained_store_request& v);
    static ores::scheduler::messaging::delete_retained_store_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::delete_retained_store_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::delete_retained_store_response& v);
    static ores::scheduler::messaging::delete_retained_store_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::delete_retained_store_response& v);
    static ores::scheduler::messaging::delete_retained_store_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::delete_many_retained_stores_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::delete_many_retained_stores_request& v);
    static ores::scheduler::messaging::delete_many_retained_stores_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::delete_many_retained_stores_request& v);
    static ores::scheduler::messaging::delete_many_retained_stores_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::delete_many_retained_stores_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::delete_many_retained_stores_response& v);
    static ores::scheduler::messaging::delete_many_retained_stores_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::delete_many_retained_stores_response& v);
    static ores::scheduler::messaging::delete_many_retained_stores_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::list_retained_store_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::list_retained_store_versions_request& v);
    static ores::scheduler::messaging::list_retained_store_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::list_retained_store_versions_request& v);
    static ores::scheduler::messaging::list_retained_store_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::list_retained_store_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::list_retained_store_versions_response& v);
    static ores::scheduler::messaging::list_retained_store_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::list_retained_store_versions_response& v);
    static ores::scheduler::messaging::list_retained_store_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::get_retained_store_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::get_retained_store_version_request& v);
    static ores::scheduler::messaging::get_retained_store_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::get_retained_store_version_request& v);
    static ores::scheduler::messaging::get_retained_store_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::get_retained_store_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::get_retained_store_version_response& v);
    static ores::scheduler::messaging::get_retained_store_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::get_retained_store_version_response& v);
    static ores::scheduler::messaging::get_retained_store_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
