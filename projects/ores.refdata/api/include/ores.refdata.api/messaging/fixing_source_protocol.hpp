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
#ifndef ORES_REFDATA_API_MESSAGING_FIXING_SOURCE_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_FIXING_SOURCE_PROTOCOL_HPP

#include "ores.refdata.api/domain/fixing_source_codec.hpp"
#include "ores.refdata.api/export.hpp"
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

namespace ores::refdata::messaging {

struct fixing_source_key {
    std::string code;

    friend bool operator==(const fixing_source_key&, const fixing_source_key&) = default;
};

struct fixing_source_write {
    std::string code;
    std::string name;
    std::string description;
    std::string kind_code;
    std::string asset_class;
    std::string administrator;
    ores::platform::time::time_of_day publication_time;
    std::string publication_time_zone;
    std::string calendar_code;
    int rounding_decimals{};
    std::string status_code;
    int display_order{};

    friend bool operator==(const fixing_source_write&, const fixing_source_write&) = default;
};

struct fixing_source_change {
    fixing_source_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const fixing_source_change&, const fixing_source_change&) = default;
};

struct fixing_source_removal {
    fixing_source_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const fixing_source_removal&, const fixing_source_removal&) = default;
};

struct fixing_source_lookup {
    fixing_source_key key;
    std::optional<ores::refdata::domain::fixing_source> fixing_source;

    friend bool operator==(const fixing_source_lookup&, const fixing_source_lookup&) = default;
};

struct fixing_sources_filter {
    std::optional<std::vector<std::string>> code_one_of;

    friend bool operator==(const fixing_sources_filter&, const fixing_sources_filter&) = default;
};

struct fixing_source_event {
    boost::uuids::uuid event_id;
    fixing_source_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const fixing_source_event&, const fixing_source_event&) = default;
};

struct fixing_source_version_key {
    fixing_source_key fixing_source;
    std::uint32_t version{};

    friend bool operator==(const fixing_source_version_key&,
                           const fixing_source_version_key&) = default;
};

struct fixing_source_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const fixing_source_versions_filter&,
                           const fixing_source_versions_filter&) = default;
};

struct list_fixing_sources_request {
    using response_type = struct list_fixing_sources_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fixing_sources.list";
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
    std::optional<fixing_sources_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_fixing_sources_request&,
                           const list_fixing_sources_request&) = default;
};

struct list_fixing_sources_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::fixing_source> sources;
    std::uint64_t total{};

    friend bool operator==(const list_fixing_sources_response&,
                           const list_fixing_sources_response&) = default;
};

struct get_fixing_source_request {
    using response_type = struct get_fixing_source_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fixing_sources.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fixing_source_key key;

    friend bool operator==(const get_fixing_source_request&,
                           const get_fixing_source_request&) = default;
};

struct get_fixing_source_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::fixing_source> fixing_source;

    friend bool operator==(const get_fixing_source_response&,
                           const get_fixing_source_response&) = default;
};

struct get_many_fixing_sources_request {
    using response_type = struct get_many_fixing_sources_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fixing_sources.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fixing_source_key> keys;

    friend bool operator==(const get_many_fixing_sources_request&,
                           const get_many_fixing_sources_request&) = default;
};

struct get_many_fixing_sources_response {
    ores::utility::domain::result result;
    std::vector<fixing_source_lookup> entries;

    friend bool operator==(const get_many_fixing_sources_response&,
                           const get_many_fixing_sources_response&) = default;
};

struct put_fixing_source_request {
    using response_type = struct put_fixing_source_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fixing_sources.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fixing_source_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_fixing_source_request&,
                           const put_fixing_source_request&) = default;
};

struct put_fixing_source_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::fixing_source> fixing_source;

    friend bool operator==(const put_fixing_source_response&,
                           const put_fixing_source_response&) = default;
};

struct put_many_fixing_sources_request {
    using response_type = struct put_many_fixing_sources_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fixing_sources.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fixing_source_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_fixing_sources_request&,
                           const put_many_fixing_sources_request&) = default;
};

struct put_many_fixing_sources_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::fixing_source> sources;

    friend bool operator==(const put_many_fixing_sources_response&,
                           const put_many_fixing_sources_response&) = default;
};

struct delete_fixing_source_request {
    using response_type = struct delete_fixing_source_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fixing_sources.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fixing_source_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_fixing_source_request&,
                           const delete_fixing_source_request&) = default;
};

struct delete_fixing_source_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_fixing_source_response&,
                           const delete_fixing_source_response&) = default;
};

struct delete_many_fixing_sources_request {
    using response_type = struct delete_many_fixing_sources_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fixing_sources.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fixing_source_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_fixing_sources_request&,
                           const delete_many_fixing_sources_request&) = default;
};

struct delete_many_fixing_sources_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_fixing_sources_response&,
                           const delete_many_fixing_sources_response&) = default;
};

struct list_fixing_source_versions_request {
    using response_type = struct list_fixing_source_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fixing_sources_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fixing_source_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<fixing_source_versions_filter> filter;

    friend bool operator==(const list_fixing_source_versions_request&,
                           const list_fixing_source_versions_request&) = default;
};

struct list_fixing_source_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::fixing_source> versions;
    std::uint64_t total{};

    friend bool operator==(const list_fixing_source_versions_response&,
                           const list_fixing_source_versions_response&) = default;
};

struct get_fixing_source_version_request {
    using response_type = struct get_fixing_source_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fixing_sources_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fixing_source_version_key key;

    friend bool operator==(const get_fixing_source_version_request&,
                           const get_fixing_source_version_request&) = default;
};

struct get_fixing_source_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::fixing_source> version;

    friend bool operator==(const get_fixing_source_version_response&,
                           const get_fixing_source_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace fixing_source_event_subjects {
inline constexpr std::string_view created = "refdata.v1.events.fixing_sources.created";
inline constexpr std::string_view updated = "refdata.v1.events.fixing_sources.updated";
inline constexpr std::string_view deleted = "refdata.v1.events.fixing_sources.deleted";
}
/**
 * @brief The members of fixing_source_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fixing_source_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"code", &fixing_source_key::code},
    };
}

/**
 * @brief The members of fixing_source_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fixing_source_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"code", &fixing_source_write::code},
        utility::codec::field{"name", &fixing_source_write::name},
        utility::codec::field{"description", &fixing_source_write::description},
        utility::codec::field{"kind_code", &fixing_source_write::kind_code},
        utility::codec::field{"asset_class", &fixing_source_write::asset_class},
        utility::codec::field{"administrator", &fixing_source_write::administrator},
        utility::codec::field{"publication_time", &fixing_source_write::publication_time},
        utility::codec::field{"publication_time_zone", &fixing_source_write::publication_time_zone},
        utility::codec::field{"calendar_code", &fixing_source_write::calendar_code},
        utility::codec::field{"rounding_decimals", &fixing_source_write::rounding_decimals},
        utility::codec::field{"status_code", &fixing_source_write::status_code},
        utility::codec::field{"display_order", &fixing_source_write::display_order},
    };
}

/**
 * @brief The members of fixing_source_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fixing_source_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &fixing_source_change::write},
        utility::codec::field{"precondition", &fixing_source_change::precondition},
    };
}

/**
 * @brief The members of fixing_source_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fixing_source_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &fixing_source_removal::key},
        utility::codec::field{"precondition", &fixing_source_removal::precondition},
    };
}

/**
 * @brief The members of fixing_source_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fixing_source_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &fixing_source_lookup::key},
        utility::codec::field{"fixing_source", &fixing_source_lookup::fixing_source},
    };
}

/**
 * @brief The members of fixing_sources_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fixing_sources_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"code_one_of", &fixing_sources_filter::code_one_of},
    };
}

/**
 * @brief The members of fixing_source_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fixing_source_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &fixing_source_event::event_id},
        utility::codec::field{"key", &fixing_source_event::key},
        utility::codec::field{"action", &fixing_source_event::action},
        utility::codec::field{"version", &fixing_source_event::version},
        utility::codec::field{"occurred_at", &fixing_source_event::occurred_at},
        utility::codec::field{"correlation_id", &fixing_source_event::correlation_id},
    };
}

/**
 * @brief The members of fixing_source_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fixing_source_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"fixing_source", &fixing_source_version_key::fixing_source},
        utility::codec::field{"version", &fixing_source_version_key::version},
    };
}

/**
 * @brief The members of fixing_source_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fixing_source_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &fixing_source_versions_filter::version},
        utility::codec::field{"from_version", &fixing_source_versions_filter::from_version},
        utility::codec::field{"to_version", &fixing_source_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_fixing_sources_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_fixing_sources_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_fixing_sources_request::offset},
        utility::codec::field{"limit", &list_fixing_sources_request::limit},
        utility::codec::field{"order", &list_fixing_sources_request::order},
        utility::codec::field{"filter", &list_fixing_sources_request::filter},
        utility::codec::field{"as_of", &list_fixing_sources_request::as_of},
    };
}

/**
 * @brief The members of list_fixing_sources_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_fixing_sources_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_fixing_sources_response::result},
        utility::codec::field{"sources", &list_fixing_sources_response::sources},
        utility::codec::field{"total", &list_fixing_sources_response::total},
    };
}

/**
 * @brief The members of get_fixing_source_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_fixing_source_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_fixing_source_request::key},
    };
}

/**
 * @brief The members of get_fixing_source_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_fixing_source_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_fixing_source_response::result},
        utility::codec::field{"fixing_source", &get_fixing_source_response::fixing_source},
    };
}

/**
 * @brief The members of get_many_fixing_sources_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_fixing_sources_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_fixing_sources_request::keys},
    };
}

/**
 * @brief The members of get_many_fixing_sources_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_fixing_sources_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_fixing_sources_response::result},
        utility::codec::field{"entries", &get_many_fixing_sources_response::entries},
    };
}

/**
 * @brief The members of put_fixing_source_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_fixing_source_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_fixing_source_request::change},
        utility::codec::field{"intent", &put_fixing_source_request::intent},
    };
}

/**
 * @brief The members of put_fixing_source_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_fixing_source_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_fixing_source_response::result},
        utility::codec::field{"fixing_source", &put_fixing_source_response::fixing_source},
    };
}

/**
 * @brief The members of put_many_fixing_sources_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_fixing_sources_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_fixing_sources_request::changes},
        utility::codec::field{"intent", &put_many_fixing_sources_request::intent},
    };
}

/**
 * @brief The members of put_many_fixing_sources_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_fixing_sources_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_fixing_sources_response::result},
        utility::codec::field{"sources", &put_many_fixing_sources_response::sources},
    };
}

/**
 * @brief The members of delete_fixing_source_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_fixing_source_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_fixing_source_request::removal},
        utility::codec::field{"intent", &delete_fixing_source_request::intent},
    };
}

/**
 * @brief The members of delete_fixing_source_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_fixing_source_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_fixing_source_response::result},
    };
}

/**
 * @brief The members of delete_many_fixing_sources_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_fixing_sources_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_fixing_sources_request::removals},
        utility::codec::field{"intent", &delete_many_fixing_sources_request::intent},
    };
}

/**
 * @brief The members of delete_many_fixing_sources_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_fixing_sources_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_fixing_sources_response::result},
    };
}

/**
 * @brief The members of list_fixing_source_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_fixing_source_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_fixing_source_versions_request::key},
        utility::codec::field{"offset", &list_fixing_source_versions_request::offset},
        utility::codec::field{"limit", &list_fixing_source_versions_request::limit},
        utility::codec::field{"order", &list_fixing_source_versions_request::order},
        utility::codec::field{"filter", &list_fixing_source_versions_request::filter},
    };
}

/**
 * @brief The members of list_fixing_source_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_fixing_source_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_fixing_source_versions_response::result},
        utility::codec::field{"versions", &list_fixing_source_versions_response::versions},
        utility::codec::field{"total", &list_fixing_source_versions_response::total},
    };
}

/**
 * @brief The members of get_fixing_source_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_fixing_source_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_fixing_source_version_request::key},
    };
}

/**
 * @brief The members of get_fixing_source_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_fixing_source_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_fixing_source_version_response::result},
        utility::codec::field{"version", &get_fixing_source_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::fixing_source_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::fixing_source_key& v);
    static ores::refdata::messaging::fixing_source_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::fixing_source_key& v);
    static ores::refdata::messaging::fixing_source_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::fixing_source_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::fixing_source_write& v);
    static ores::refdata::messaging::fixing_source_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::fixing_source_write& v);
    static ores::refdata::messaging::fixing_source_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::fixing_source_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::fixing_source_change& v);
    static ores::refdata::messaging::fixing_source_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::fixing_source_change& v);
    static ores::refdata::messaging::fixing_source_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::fixing_source_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::fixing_source_removal& v);
    static ores::refdata::messaging::fixing_source_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::fixing_source_removal& v);
    static ores::refdata::messaging::fixing_source_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::fixing_source_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::fixing_source_lookup& v);
    static ores::refdata::messaging::fixing_source_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::fixing_source_lookup& v);
    static ores::refdata::messaging::fixing_source_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::fixing_sources_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::fixing_sources_filter& v);
    static ores::refdata::messaging::fixing_sources_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::fixing_sources_filter& v);
    static ores::refdata::messaging::fixing_sources_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::fixing_source_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::fixing_source_event& v);
    static ores::refdata::messaging::fixing_source_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::fixing_source_event& v);
    static ores::refdata::messaging::fixing_source_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::fixing_source_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::fixing_source_version_key& v);
    static ores::refdata::messaging::fixing_source_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::fixing_source_version_key& v);
    static ores::refdata::messaging::fixing_source_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::fixing_source_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::fixing_source_versions_filter& v);
    static ores::refdata::messaging::fixing_source_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::fixing_source_versions_filter& v);
    static ores::refdata::messaging::fixing_source_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_fixing_sources_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_fixing_sources_request& v);
    static ores::refdata::messaging::list_fixing_sources_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_fixing_sources_request& v);
    static ores::refdata::messaging::list_fixing_sources_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_fixing_sources_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_fixing_sources_response& v);
    static ores::refdata::messaging::list_fixing_sources_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_fixing_sources_response& v);
    static ores::refdata::messaging::list_fixing_sources_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_fixing_source_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_fixing_source_request& v);
    static ores::refdata::messaging::get_fixing_source_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_fixing_source_request& v);
    static ores::refdata::messaging::get_fixing_source_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_fixing_source_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_fixing_source_response& v);
    static ores::refdata::messaging::get_fixing_source_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_fixing_source_response& v);
    static ores::refdata::messaging::get_fixing_source_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_fixing_sources_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_fixing_sources_request& v);
    static ores::refdata::messaging::get_many_fixing_sources_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_fixing_sources_request& v);
    static ores::refdata::messaging::get_many_fixing_sources_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_fixing_sources_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_fixing_sources_response& v);
    static ores::refdata::messaging::get_many_fixing_sources_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_fixing_sources_response& v);
    static ores::refdata::messaging::get_many_fixing_sources_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_fixing_source_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_fixing_source_request& v);
    static ores::refdata::messaging::put_fixing_source_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_fixing_source_request& v);
    static ores::refdata::messaging::put_fixing_source_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_fixing_source_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_fixing_source_response& v);
    static ores::refdata::messaging::put_fixing_source_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_fixing_source_response& v);
    static ores::refdata::messaging::put_fixing_source_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_fixing_sources_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_fixing_sources_request& v);
    static ores::refdata::messaging::put_many_fixing_sources_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_fixing_sources_request& v);
    static ores::refdata::messaging::put_many_fixing_sources_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_fixing_sources_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_fixing_sources_response& v);
    static ores::refdata::messaging::put_many_fixing_sources_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_fixing_sources_response& v);
    static ores::refdata::messaging::put_many_fixing_sources_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_fixing_source_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_fixing_source_request& v);
    static ores::refdata::messaging::delete_fixing_source_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_fixing_source_request& v);
    static ores::refdata::messaging::delete_fixing_source_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_fixing_source_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_fixing_source_response& v);
    static ores::refdata::messaging::delete_fixing_source_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_fixing_source_response& v);
    static ores::refdata::messaging::delete_fixing_source_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_fixing_sources_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_fixing_sources_request& v);
    static ores::refdata::messaging::delete_many_fixing_sources_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_fixing_sources_request& v);
    static ores::refdata::messaging::delete_many_fixing_sources_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_fixing_sources_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_fixing_sources_response& v);
    static ores::refdata::messaging::delete_many_fixing_sources_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_fixing_sources_response& v);
    static ores::refdata::messaging::delete_many_fixing_sources_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_fixing_source_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_fixing_source_versions_request& v);
    static ores::refdata::messaging::list_fixing_source_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_fixing_source_versions_request& v);
    static ores::refdata::messaging::list_fixing_source_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_fixing_source_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_fixing_source_versions_response& v);
    static ores::refdata::messaging::list_fixing_source_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_fixing_source_versions_response& v);
    static ores::refdata::messaging::list_fixing_source_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_fixing_source_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_fixing_source_version_request& v);
    static ores::refdata::messaging::get_fixing_source_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_fixing_source_version_request& v);
    static ores::refdata::messaging::get_fixing_source_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_fixing_source_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_fixing_source_version_response& v);
    static ores::refdata::messaging::get_fixing_source_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_fixing_source_version_response& v);
    static ores::refdata::messaging::get_fixing_source_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
