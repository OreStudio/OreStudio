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
#ifndef ORES_DQ_API_MESSAGING_SUBJECT_AREA_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_SUBJECT_AREA_PROTOCOL_HPP

#include "ores.dq.api/domain/subject_area_codec.hpp"
#include "ores.dq.api/export.hpp"
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

namespace ores::dq::messaging {

struct subject_area_key {
    std::string name;
    std::string domain_name;

    friend bool operator==(const subject_area_key&, const subject_area_key&) = default;
};

struct subject_area_write {
    std::string name;
    std::string domain_name;
    std::string description;

    friend bool operator==(const subject_area_write&, const subject_area_write&) = default;
};

struct subject_area_change {
    subject_area_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const subject_area_change&, const subject_area_change&) = default;
};

struct subject_area_removal {
    subject_area_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const subject_area_removal&, const subject_area_removal&) = default;
};

struct subject_area_lookup {
    subject_area_key key;
    std::optional<ores::dq::domain::subject_area> subject_area;

    friend bool operator==(const subject_area_lookup&, const subject_area_lookup&) = default;
};

struct subject_area_event {
    boost::uuids::uuid event_id;
    subject_area_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const subject_area_event&, const subject_area_event&) = default;
};

struct subject_area_version_key {
    subject_area_key subject_area;
    std::uint32_t version{};

    friend bool operator==(const subject_area_version_key&,
                           const subject_area_version_key&) = default;
};

struct subject_area_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const subject_area_versions_filter&,
                           const subject_area_versions_filter&) = default;
};

struct list_subject_areas_request {
    using response_type = struct list_subject_areas_response;
    static constexpr std::string_view nats_subject = "dq.v1.subject_areas.list";
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

    friend bool operator==(const list_subject_areas_request&,
                           const list_subject_areas_request&) = default;
};

struct list_subject_areas_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::subject_area> areas;
    std::uint64_t total{};

    friend bool operator==(const list_subject_areas_response&,
                           const list_subject_areas_response&) = default;
};

struct get_subject_area_request {
    using response_type = struct get_subject_area_response;
    static constexpr std::string_view nats_subject = "dq.v1.subject_areas.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    subject_area_key key;

    friend bool operator==(const get_subject_area_request&,
                           const get_subject_area_request&) = default;
};

struct get_subject_area_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::subject_area> subject_area;

    friend bool operator==(const get_subject_area_response&,
                           const get_subject_area_response&) = default;
};

struct get_many_subject_areas_request {
    using response_type = struct get_many_subject_areas_response;
    static constexpr std::string_view nats_subject = "dq.v1.subject_areas.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<subject_area_key> keys;

    friend bool operator==(const get_many_subject_areas_request&,
                           const get_many_subject_areas_request&) = default;
};

struct get_many_subject_areas_response {
    ores::utility::domain::result result;
    std::vector<subject_area_lookup> entries;

    friend bool operator==(const get_many_subject_areas_response&,
                           const get_many_subject_areas_response&) = default;
};

struct put_subject_area_request {
    using response_type = struct put_subject_area_response;
    static constexpr std::string_view nats_subject = "dq.v1.subject_areas.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    subject_area_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_subject_area_request&,
                           const put_subject_area_request&) = default;
};

struct put_subject_area_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::subject_area> subject_area;

    friend bool operator==(const put_subject_area_response&,
                           const put_subject_area_response&) = default;
};

struct put_many_subject_areas_request {
    using response_type = struct put_many_subject_areas_response;
    static constexpr std::string_view nats_subject = "dq.v1.subject_areas.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<subject_area_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_subject_areas_request&,
                           const put_many_subject_areas_request&) = default;
};

struct put_many_subject_areas_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::subject_area> areas;

    friend bool operator==(const put_many_subject_areas_response&,
                           const put_many_subject_areas_response&) = default;
};

struct delete_subject_area_request {
    using response_type = struct delete_subject_area_response;
    static constexpr std::string_view nats_subject = "dq.v1.subject_areas.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    subject_area_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_subject_area_request&,
                           const delete_subject_area_request&) = default;
};

struct delete_subject_area_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_subject_area_response&,
                           const delete_subject_area_response&) = default;
};

struct delete_many_subject_areas_request {
    using response_type = struct delete_many_subject_areas_response;
    static constexpr std::string_view nats_subject = "dq.v1.subject_areas.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<subject_area_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_subject_areas_request&,
                           const delete_many_subject_areas_request&) = default;
};

struct delete_many_subject_areas_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_subject_areas_response&,
                           const delete_many_subject_areas_response&) = default;
};

struct list_subject_area_versions_request {
    using response_type = struct list_subject_area_versions_response;
    static constexpr std::string_view nats_subject = "dq.v1.subject_areas_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    subject_area_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<subject_area_versions_filter> filter;

    friend bool operator==(const list_subject_area_versions_request&,
                           const list_subject_area_versions_request&) = default;
};

struct list_subject_area_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::subject_area> versions;
    std::uint64_t total{};

    friend bool operator==(const list_subject_area_versions_response&,
                           const list_subject_area_versions_response&) = default;
};

struct get_subject_area_version_request {
    using response_type = struct get_subject_area_version_response;
    static constexpr std::string_view nats_subject = "dq.v1.subject_areas_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    subject_area_version_key key;

    friend bool operator==(const get_subject_area_version_request&,
                           const get_subject_area_version_request&) = default;
};

struct get_subject_area_version_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::subject_area> version;

    friend bool operator==(const get_subject_area_version_response&,
                           const get_subject_area_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace subject_area_event_subjects {
inline constexpr std::string_view created = "dq.v1.events.subject_areas.created";
inline constexpr std::string_view updated = "dq.v1.events.subject_areas.updated";
inline constexpr std::string_view deleted = "dq.v1.events.subject_areas.deleted";
}
/**
 * @brief The members of subject_area_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<subject_area_key>) {
    return std::tuple{
        utility::codec::field{"name", &subject_area_key::name},
        utility::codec::field{"domain_name", &subject_area_key::domain_name},
    };
}

/**
 * @brief The members of subject_area_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<subject_area_write>) {
    return std::tuple{
        utility::codec::field{"name", &subject_area_write::name},
        utility::codec::field{"domain_name", &subject_area_write::domain_name},
        utility::codec::field{"description", &subject_area_write::description},
    };
}

/**
 * @brief The members of subject_area_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<subject_area_change>) {
    return std::tuple{
        utility::codec::field{"write", &subject_area_change::write},
        utility::codec::field{"precondition", &subject_area_change::precondition},
    };
}

/**
 * @brief The members of subject_area_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<subject_area_removal>) {
    return std::tuple{
        utility::codec::field{"key", &subject_area_removal::key},
        utility::codec::field{"precondition", &subject_area_removal::precondition},
    };
}

/**
 * @brief The members of subject_area_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<subject_area_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &subject_area_lookup::key},
        utility::codec::field{"subject_area", &subject_area_lookup::subject_area},
    };
}

/**
 * @brief The members of subject_area_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<subject_area_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &subject_area_event::event_id},
        utility::codec::field{"key", &subject_area_event::key},
        utility::codec::field{"action", &subject_area_event::action},
        utility::codec::field{"version", &subject_area_event::version},
        utility::codec::field{"occurred_at", &subject_area_event::occurred_at},
        utility::codec::field{"correlation_id", &subject_area_event::correlation_id},
    };
}

/**
 * @brief The members of subject_area_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<subject_area_version_key>) {
    return std::tuple{
        utility::codec::field{"subject_area", &subject_area_version_key::subject_area},
        utility::codec::field{"version", &subject_area_version_key::version},
    };
}

/**
 * @brief The members of subject_area_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<subject_area_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &subject_area_versions_filter::version},
        utility::codec::field{"from_version", &subject_area_versions_filter::from_version},
        utility::codec::field{"to_version", &subject_area_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_subject_areas_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_subject_areas_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_subject_areas_request::offset},
        utility::codec::field{"limit", &list_subject_areas_request::limit},
        utility::codec::field{"order", &list_subject_areas_request::order},
        utility::codec::field{"as_of", &list_subject_areas_request::as_of},
    };
}

/**
 * @brief The members of list_subject_areas_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_subject_areas_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_subject_areas_response::result},
        utility::codec::field{"areas", &list_subject_areas_response::areas},
        utility::codec::field{"total", &list_subject_areas_response::total},
    };
}

/**
 * @brief The members of get_subject_area_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_subject_area_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_subject_area_request::key},
    };
}

/**
 * @brief The members of get_subject_area_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_subject_area_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_subject_area_response::result},
        utility::codec::field{"subject_area", &get_subject_area_response::subject_area},
    };
}

/**
 * @brief The members of get_many_subject_areas_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_subject_areas_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_subject_areas_request::keys},
    };
}

/**
 * @brief The members of get_many_subject_areas_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_subject_areas_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_subject_areas_response::result},
        utility::codec::field{"entries", &get_many_subject_areas_response::entries},
    };
}

/**
 * @brief The members of put_subject_area_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_subject_area_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_subject_area_request::change},
        utility::codec::field{"intent", &put_subject_area_request::intent},
    };
}

/**
 * @brief The members of put_subject_area_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_subject_area_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_subject_area_response::result},
        utility::codec::field{"subject_area", &put_subject_area_response::subject_area},
    };
}

/**
 * @brief The members of put_many_subject_areas_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_subject_areas_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_subject_areas_request::changes},
        utility::codec::field{"intent", &put_many_subject_areas_request::intent},
    };
}

/**
 * @brief The members of put_many_subject_areas_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_subject_areas_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_subject_areas_response::result},
        utility::codec::field{"areas", &put_many_subject_areas_response::areas},
    };
}

/**
 * @brief The members of delete_subject_area_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_subject_area_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_subject_area_request::removal},
        utility::codec::field{"intent", &delete_subject_area_request::intent},
    };
}

/**
 * @brief The members of delete_subject_area_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_subject_area_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_subject_area_response::result},
    };
}

/**
 * @brief The members of delete_many_subject_areas_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_subject_areas_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_subject_areas_request::removals},
        utility::codec::field{"intent", &delete_many_subject_areas_request::intent},
    };
}

/**
 * @brief The members of delete_many_subject_areas_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_subject_areas_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_subject_areas_response::result},
    };
}

/**
 * @brief The members of list_subject_area_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_subject_area_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_subject_area_versions_request::key},
        utility::codec::field{"offset", &list_subject_area_versions_request::offset},
        utility::codec::field{"limit", &list_subject_area_versions_request::limit},
        utility::codec::field{"order", &list_subject_area_versions_request::order},
        utility::codec::field{"filter", &list_subject_area_versions_request::filter},
    };
}

/**
 * @brief The members of list_subject_area_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_subject_area_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_subject_area_versions_response::result},
        utility::codec::field{"versions", &list_subject_area_versions_response::versions},
        utility::codec::field{"total", &list_subject_area_versions_response::total},
    };
}

/**
 * @brief The members of get_subject_area_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_subject_area_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_subject_area_version_request::key},
    };
}

/**
 * @brief The members of get_subject_area_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_subject_area_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_subject_area_version_response::result},
        utility::codec::field{"version", &get_subject_area_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::subject_area_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::subject_area_key& v);
    static ores::dq::messaging::subject_area_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::subject_area_key& v);
    static ores::dq::messaging::subject_area_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::subject_area_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::subject_area_write& v);
    static ores::dq::messaging::subject_area_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::subject_area_write& v);
    static ores::dq::messaging::subject_area_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::subject_area_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::subject_area_change& v);
    static ores::dq::messaging::subject_area_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::subject_area_change& v);
    static ores::dq::messaging::subject_area_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::subject_area_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::subject_area_removal& v);
    static ores::dq::messaging::subject_area_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::subject_area_removal& v);
    static ores::dq::messaging::subject_area_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::subject_area_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::subject_area_lookup& v);
    static ores::dq::messaging::subject_area_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::subject_area_lookup& v);
    static ores::dq::messaging::subject_area_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::subject_area_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::subject_area_event& v);
    static ores::dq::messaging::subject_area_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::subject_area_event& v);
    static ores::dq::messaging::subject_area_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::subject_area_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::subject_area_version_key& v);
    static ores::dq::messaging::subject_area_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::subject_area_version_key& v);
    static ores::dq::messaging::subject_area_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::subject_area_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::subject_area_versions_filter& v);
    static ores::dq::messaging::subject_area_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::subject_area_versions_filter& v);
    static ores::dq::messaging::subject_area_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_subject_areas_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_subject_areas_request& v);
    static ores::dq::messaging::list_subject_areas_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_subject_areas_request& v);
    static ores::dq::messaging::list_subject_areas_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_subject_areas_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_subject_areas_response& v);
    static ores::dq::messaging::list_subject_areas_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_subject_areas_response& v);
    static ores::dq::messaging::list_subject_areas_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_subject_area_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_subject_area_request& v);
    static ores::dq::messaging::get_subject_area_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_subject_area_request& v);
    static ores::dq::messaging::get_subject_area_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_subject_area_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_subject_area_response& v);
    static ores::dq::messaging::get_subject_area_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_subject_area_response& v);
    static ores::dq::messaging::get_subject_area_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_subject_areas_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_subject_areas_request& v);
    static ores::dq::messaging::get_many_subject_areas_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_many_subject_areas_request& v);
    static ores::dq::messaging::get_many_subject_areas_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_subject_areas_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_subject_areas_response& v);
    static ores::dq::messaging::get_many_subject_areas_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_many_subject_areas_response& v);
    static ores::dq::messaging::get_many_subject_areas_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_subject_area_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_subject_area_request& v);
    static ores::dq::messaging::put_subject_area_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_subject_area_request& v);
    static ores::dq::messaging::put_subject_area_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_subject_area_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_subject_area_response& v);
    static ores::dq::messaging::put_subject_area_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_subject_area_response& v);
    static ores::dq::messaging::put_subject_area_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_subject_areas_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_subject_areas_request& v);
    static ores::dq::messaging::put_many_subject_areas_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_many_subject_areas_request& v);
    static ores::dq::messaging::put_many_subject_areas_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_subject_areas_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_subject_areas_response& v);
    static ores::dq::messaging::put_many_subject_areas_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_many_subject_areas_response& v);
    static ores::dq::messaging::put_many_subject_areas_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_subject_area_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_subject_area_request& v);
    static ores::dq::messaging::delete_subject_area_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_subject_area_request& v);
    static ores::dq::messaging::delete_subject_area_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_subject_area_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_subject_area_response& v);
    static ores::dq::messaging::delete_subject_area_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_subject_area_response& v);
    static ores::dq::messaging::delete_subject_area_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_subject_areas_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_many_subject_areas_request& v);
    static ores::dq::messaging::delete_many_subject_areas_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_subject_areas_request& v);
    static ores::dq::messaging::delete_many_subject_areas_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_subject_areas_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_many_subject_areas_response& v);
    static ores::dq::messaging::delete_many_subject_areas_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_subject_areas_response& v);
    static ores::dq::messaging::delete_many_subject_areas_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_subject_area_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_subject_area_versions_request& v);
    static ores::dq::messaging::list_subject_area_versions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_subject_area_versions_request& v);
    static ores::dq::messaging::list_subject_area_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_subject_area_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_subject_area_versions_response& v);
    static ores::dq::messaging::list_subject_area_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_subject_area_versions_response& v);
    static ores::dq::messaging::list_subject_area_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_subject_area_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_subject_area_version_request& v);
    static ores::dq::messaging::get_subject_area_version_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_subject_area_version_request& v);
    static ores::dq::messaging::get_subject_area_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_subject_area_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_subject_area_version_response& v);
    static ores::dq::messaging::get_subject_area_version_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_subject_area_version_response& v);
    static ores::dq::messaging::get_subject_area_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
