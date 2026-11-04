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
#ifndef ORES_REFDATA_API_MESSAGING_FX_VOL_SURFACE_PILLAR_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_FX_VOL_SURFACE_PILLAR_PROTOCOL_HPP

#include "ores.refdata.api/domain/fx_vol_surface_pillar.hpp"
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

struct fx_vol_surface_pillar_key {
    boost::uuids::uuid id;

    friend bool operator==(const fx_vol_surface_pillar_key&,
                           const fx_vol_surface_pillar_key&) = default;
};

struct fx_vol_surface_pillar_write {
    boost::uuids::uuid id;
    boost::uuids::uuid party_id;
    boost::uuids::uuid blueprint_id;
    std::string expiry_code;
    std::string strike_label;

    friend bool operator==(const fx_vol_surface_pillar_write&,
                           const fx_vol_surface_pillar_write&) = default;
};

struct fx_vol_surface_pillar_change {
    fx_vol_surface_pillar_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const fx_vol_surface_pillar_change&,
                           const fx_vol_surface_pillar_change&) = default;
};

struct fx_vol_surface_pillar_removal {
    fx_vol_surface_pillar_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const fx_vol_surface_pillar_removal&,
                           const fx_vol_surface_pillar_removal&) = default;
};

struct fx_vol_surface_pillar_lookup {
    fx_vol_surface_pillar_key key;
    std::optional<ores::refdata::domain::fx_vol_surface_pillar> fx_vol_surface_pillar;

    friend bool operator==(const fx_vol_surface_pillar_lookup&,
                           const fx_vol_surface_pillar_lookup&) = default;
};

struct fx_vol_surface_pillars_filter {
    std::optional<boost::uuids::uuid> blueprint_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> blueprint_id_one_of;

    friend bool operator==(const fx_vol_surface_pillars_filter&,
                           const fx_vol_surface_pillars_filter&) = default;
};

struct fx_vol_surface_pillar_event {
    boost::uuids::uuid event_id;
    fx_vol_surface_pillar_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const fx_vol_surface_pillar_event&,
                           const fx_vol_surface_pillar_event&) = default;
};

struct fx_vol_surface_pillar_version_key {
    fx_vol_surface_pillar_key fx_vol_surface_pillar;
    std::uint32_t version;

    friend bool operator==(const fx_vol_surface_pillar_version_key&,
                           const fx_vol_surface_pillar_version_key&) = default;
};

struct fx_vol_surface_pillar_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const fx_vol_surface_pillar_versions_filter&,
                           const fx_vol_surface_pillar_versions_filter&) = default;
};

struct list_fx_vol_surface_pillars_request {
    using response_type = struct list_fx_vol_surface_pillars_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fx_vol_surface_pillars.list";
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
    std::optional<fx_vol_surface_pillars_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_fx_vol_surface_pillars_request&,
                           const list_fx_vol_surface_pillars_request&) = default;
};

struct list_fx_vol_surface_pillars_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::fx_vol_surface_pillar> pillars;
    std::uint64_t total;

    friend bool operator==(const list_fx_vol_surface_pillars_response&,
                           const list_fx_vol_surface_pillars_response&) = default;
};

struct get_fx_vol_surface_pillar_request {
    using response_type = struct get_fx_vol_surface_pillar_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fx_vol_surface_pillars.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_vol_surface_pillar_key key;

    friend bool operator==(const get_fx_vol_surface_pillar_request&,
                           const get_fx_vol_surface_pillar_request&) = default;
};

struct get_fx_vol_surface_pillar_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::fx_vol_surface_pillar> fx_vol_surface_pillar;

    friend bool operator==(const get_fx_vol_surface_pillar_response&,
                           const get_fx_vol_surface_pillar_response&) = default;
};

struct get_many_fx_vol_surface_pillars_request {
    using response_type = struct get_many_fx_vol_surface_pillars_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fx_vol_surface_pillars.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fx_vol_surface_pillar_key> keys;

    friend bool operator==(const get_many_fx_vol_surface_pillars_request&,
                           const get_many_fx_vol_surface_pillars_request&) = default;
};

struct get_many_fx_vol_surface_pillars_response {
    ores::utility::domain::result result;
    std::vector<fx_vol_surface_pillar_lookup> entries;

    friend bool operator==(const get_many_fx_vol_surface_pillars_response&,
                           const get_many_fx_vol_surface_pillars_response&) = default;
};

struct put_fx_vol_surface_pillar_request {
    using response_type = struct put_fx_vol_surface_pillar_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fx_vol_surface_pillars.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_vol_surface_pillar_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_fx_vol_surface_pillar_request&,
                           const put_fx_vol_surface_pillar_request&) = default;
};

struct put_fx_vol_surface_pillar_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::fx_vol_surface_pillar> fx_vol_surface_pillar;

    friend bool operator==(const put_fx_vol_surface_pillar_response&,
                           const put_fx_vol_surface_pillar_response&) = default;
};

struct put_many_fx_vol_surface_pillars_request {
    using response_type = struct put_many_fx_vol_surface_pillars_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fx_vol_surface_pillars.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fx_vol_surface_pillar_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_fx_vol_surface_pillars_request&,
                           const put_many_fx_vol_surface_pillars_request&) = default;
};

struct put_many_fx_vol_surface_pillars_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::fx_vol_surface_pillar> pillars;

    friend bool operator==(const put_many_fx_vol_surface_pillars_response&,
                           const put_many_fx_vol_surface_pillars_response&) = default;
};

struct delete_fx_vol_surface_pillar_request {
    using response_type = struct delete_fx_vol_surface_pillar_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fx_vol_surface_pillars.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_vol_surface_pillar_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_fx_vol_surface_pillar_request&,
                           const delete_fx_vol_surface_pillar_request&) = default;
};

struct delete_fx_vol_surface_pillar_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_fx_vol_surface_pillar_response&,
                           const delete_fx_vol_surface_pillar_response&) = default;
};

struct delete_many_fx_vol_surface_pillars_request {
    using response_type = struct delete_many_fx_vol_surface_pillars_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.fx_vol_surface_pillars.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fx_vol_surface_pillar_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_fx_vol_surface_pillars_request&,
                           const delete_many_fx_vol_surface_pillars_request&) = default;
};

struct delete_many_fx_vol_surface_pillars_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_fx_vol_surface_pillars_response&,
                           const delete_many_fx_vol_surface_pillars_response&) = default;
};

struct list_fx_vol_surface_pillar_versions_request {
    using response_type = struct list_fx_vol_surface_pillar_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.fx_vol_surface_pillars_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_vol_surface_pillar_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<fx_vol_surface_pillar_versions_filter> filter;

    friend bool operator==(const list_fx_vol_surface_pillar_versions_request&,
                           const list_fx_vol_surface_pillar_versions_request&) = default;
};

struct list_fx_vol_surface_pillar_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::fx_vol_surface_pillar> versions;
    std::uint64_t total;

    friend bool operator==(const list_fx_vol_surface_pillar_versions_response&,
                           const list_fx_vol_surface_pillar_versions_response&) = default;
};

struct get_fx_vol_surface_pillar_version_request {
    using response_type = struct get_fx_vol_surface_pillar_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.fx_vol_surface_pillars_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_vol_surface_pillar_version_key key;

    friend bool operator==(const get_fx_vol_surface_pillar_version_request&,
                           const get_fx_vol_surface_pillar_version_request&) = default;
};

struct get_fx_vol_surface_pillar_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::fx_vol_surface_pillar> version;

    friend bool operator==(const get_fx_vol_surface_pillar_version_response&,
                           const get_fx_vol_surface_pillar_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace fx_vol_surface_pillar_event_subjects {
inline constexpr std::string_view created = "refdata.v1.events.fx_vol_surface_pillars.created";
inline constexpr std::string_view updated = "refdata.v1.events.fx_vol_surface_pillars.updated";
inline constexpr std::string_view deleted = "refdata.v1.events.fx_vol_surface_pillars.deleted";
}
/**
 * @brief The members of fx_vol_surface_pillar_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fx_vol_surface_pillar_key&) {
    return std::tuple{
        utility::codec::field{"id", &fx_vol_surface_pillar_key::id},
    };
}

/**
 * @brief The members of fx_vol_surface_pillar_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fx_vol_surface_pillar_write&) {
    return std::tuple{
        utility::codec::field{"id", &fx_vol_surface_pillar_write::id},
        utility::codec::field{"party_id", &fx_vol_surface_pillar_write::party_id},
        utility::codec::field{"blueprint_id", &fx_vol_surface_pillar_write::blueprint_id},
        utility::codec::field{"expiry_code", &fx_vol_surface_pillar_write::expiry_code},
        utility::codec::field{"strike_label", &fx_vol_surface_pillar_write::strike_label},
    };
}

/**
 * @brief The members of fx_vol_surface_pillar_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fx_vol_surface_pillar_change&) {
    return std::tuple{
        utility::codec::field{"write", &fx_vol_surface_pillar_change::write},
        utility::codec::field{"precondition", &fx_vol_surface_pillar_change::precondition},
    };
}

/**
 * @brief The members of fx_vol_surface_pillar_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fx_vol_surface_pillar_removal&) {
    return std::tuple{
        utility::codec::field{"key", &fx_vol_surface_pillar_removal::key},
        utility::codec::field{"precondition", &fx_vol_surface_pillar_removal::precondition},
    };
}

/**
 * @brief The members of fx_vol_surface_pillar_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fx_vol_surface_pillar_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &fx_vol_surface_pillar_lookup::key},
        utility::codec::field{"fx_vol_surface_pillar",
                              &fx_vol_surface_pillar_lookup::fx_vol_surface_pillar},
    };
}

/**
 * @brief The members of fx_vol_surface_pillars_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fx_vol_surface_pillars_filter&) {
    return std::tuple{
        utility::codec::field{"blueprint_id", &fx_vol_surface_pillars_filter::blueprint_id},
        utility::codec::field{"id_one_of", &fx_vol_surface_pillars_filter::id_one_of},
        utility::codec::field{"blueprint_id_one_of",
                              &fx_vol_surface_pillars_filter::blueprint_id_one_of},
    };
}

/**
 * @brief The members of fx_vol_surface_pillar_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fx_vol_surface_pillar_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &fx_vol_surface_pillar_event::event_id},
        utility::codec::field{"key", &fx_vol_surface_pillar_event::key},
        utility::codec::field{"action", &fx_vol_surface_pillar_event::action},
        utility::codec::field{"version", &fx_vol_surface_pillar_event::version},
        utility::codec::field{"occurred_at", &fx_vol_surface_pillar_event::occurred_at},
        utility::codec::field{"correlation_id", &fx_vol_surface_pillar_event::correlation_id},
    };
}

/**
 * @brief The members of fx_vol_surface_pillar_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fx_vol_surface_pillar_version_key&) {
    return std::tuple{
        utility::codec::field{"fx_vol_surface_pillar",
                              &fx_vol_surface_pillar_version_key::fx_vol_surface_pillar},
        utility::codec::field{"version", &fx_vol_surface_pillar_version_key::version},
    };
}

/**
 * @brief The members of fx_vol_surface_pillar_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const fx_vol_surface_pillar_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &fx_vol_surface_pillar_versions_filter::version},
        utility::codec::field{"from_version", &fx_vol_surface_pillar_versions_filter::from_version},
        utility::codec::field{"to_version", &fx_vol_surface_pillar_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_fx_vol_surface_pillars_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_fx_vol_surface_pillars_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_fx_vol_surface_pillars_request::offset},
        utility::codec::field{"limit", &list_fx_vol_surface_pillars_request::limit},
        utility::codec::field{"order", &list_fx_vol_surface_pillars_request::order},
        utility::codec::field{"filter", &list_fx_vol_surface_pillars_request::filter},
        utility::codec::field{"as_of", &list_fx_vol_surface_pillars_request::as_of},
    };
}

/**
 * @brief The members of list_fx_vol_surface_pillars_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_fx_vol_surface_pillars_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_fx_vol_surface_pillars_response::result},
        utility::codec::field{"pillars", &list_fx_vol_surface_pillars_response::pillars},
        utility::codec::field{"total", &list_fx_vol_surface_pillars_response::total},
    };
}

/**
 * @brief The members of get_fx_vol_surface_pillar_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_fx_vol_surface_pillar_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_fx_vol_surface_pillar_request::key},
    };
}

/**
 * @brief The members of get_fx_vol_surface_pillar_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_fx_vol_surface_pillar_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_fx_vol_surface_pillar_response::result},
        utility::codec::field{"fx_vol_surface_pillar",
                              &get_fx_vol_surface_pillar_response::fx_vol_surface_pillar},
    };
}

/**
 * @brief The members of get_many_fx_vol_surface_pillars_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_fx_vol_surface_pillars_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_fx_vol_surface_pillars_request::keys},
    };
}

/**
 * @brief The members of get_many_fx_vol_surface_pillars_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_fx_vol_surface_pillars_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_fx_vol_surface_pillars_response::result},
        utility::codec::field{"entries", &get_many_fx_vol_surface_pillars_response::entries},
    };
}

/**
 * @brief The members of put_fx_vol_surface_pillar_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_fx_vol_surface_pillar_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_fx_vol_surface_pillar_request::change},
        utility::codec::field{"intent", &put_fx_vol_surface_pillar_request::intent},
    };
}

/**
 * @brief The members of put_fx_vol_surface_pillar_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_fx_vol_surface_pillar_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_fx_vol_surface_pillar_response::result},
        utility::codec::field{"fx_vol_surface_pillar",
                              &put_fx_vol_surface_pillar_response::fx_vol_surface_pillar},
    };
}

/**
 * @brief The members of put_many_fx_vol_surface_pillars_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_fx_vol_surface_pillars_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_fx_vol_surface_pillars_request::changes},
        utility::codec::field{"intent", &put_many_fx_vol_surface_pillars_request::intent},
    };
}

/**
 * @brief The members of put_many_fx_vol_surface_pillars_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_fx_vol_surface_pillars_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_fx_vol_surface_pillars_response::result},
        utility::codec::field{"pillars", &put_many_fx_vol_surface_pillars_response::pillars},
    };
}

/**
 * @brief The members of delete_fx_vol_surface_pillar_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_fx_vol_surface_pillar_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_fx_vol_surface_pillar_request::removal},
        utility::codec::field{"intent", &delete_fx_vol_surface_pillar_request::intent},
    };
}

/**
 * @brief The members of delete_fx_vol_surface_pillar_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_fx_vol_surface_pillar_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_fx_vol_surface_pillar_response::result},
    };
}

/**
 * @brief The members of delete_many_fx_vol_surface_pillars_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_fx_vol_surface_pillars_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_fx_vol_surface_pillars_request::removals},
        utility::codec::field{"intent", &delete_many_fx_vol_surface_pillars_request::intent},
    };
}

/**
 * @brief The members of delete_many_fx_vol_surface_pillars_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_fx_vol_surface_pillars_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_fx_vol_surface_pillars_response::result},
    };
}

/**
 * @brief The members of list_fx_vol_surface_pillar_versions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_fx_vol_surface_pillar_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_fx_vol_surface_pillar_versions_request::key},
        utility::codec::field{"offset", &list_fx_vol_surface_pillar_versions_request::offset},
        utility::codec::field{"limit", &list_fx_vol_surface_pillar_versions_request::limit},
        utility::codec::field{"order", &list_fx_vol_surface_pillar_versions_request::order},
        utility::codec::field{"filter", &list_fx_vol_surface_pillar_versions_request::filter},
    };
}

/**
 * @brief The members of list_fx_vol_surface_pillar_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_fx_vol_surface_pillar_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_fx_vol_surface_pillar_versions_response::result},
        utility::codec::field{"versions", &list_fx_vol_surface_pillar_versions_response::versions},
        utility::codec::field{"total", &list_fx_vol_surface_pillar_versions_response::total},
    };
}

/**
 * @brief The members of get_fx_vol_surface_pillar_version_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_fx_vol_surface_pillar_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_fx_vol_surface_pillar_version_request::key},
    };
}

/**
 * @brief The members of get_fx_vol_surface_pillar_version_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_fx_vol_surface_pillar_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_fx_vol_surface_pillar_version_response::result},
        utility::codec::field{"version", &get_fx_vol_surface_pillar_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::fx_vol_surface_pillar_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::fx_vol_surface_pillar_key& v);
    static ores::refdata::messaging::fx_vol_surface_pillar_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::fx_vol_surface_pillar_key& v);
    static ores::refdata::messaging::fx_vol_surface_pillar_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::fx_vol_surface_pillar_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::fx_vol_surface_pillar_write& v);
    static ores::refdata::messaging::fx_vol_surface_pillar_write read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::fx_vol_surface_pillar_write& v);
    static ores::refdata::messaging::fx_vol_surface_pillar_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::fx_vol_surface_pillar_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::fx_vol_surface_pillar_change& v);
    static ores::refdata::messaging::fx_vol_surface_pillar_change read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::fx_vol_surface_pillar_change& v);
    static ores::refdata::messaging::fx_vol_surface_pillar_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::fx_vol_surface_pillar_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::fx_vol_surface_pillar_removal& v);
    static ores::refdata::messaging::fx_vol_surface_pillar_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::fx_vol_surface_pillar_removal& v);
    static ores::refdata::messaging::fx_vol_surface_pillar_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::fx_vol_surface_pillar_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::fx_vol_surface_pillar_lookup& v);
    static ores::refdata::messaging::fx_vol_surface_pillar_lookup read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::fx_vol_surface_pillar_lookup& v);
    static ores::refdata::messaging::fx_vol_surface_pillar_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::fx_vol_surface_pillars_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::fx_vol_surface_pillars_filter& v);
    static ores::refdata::messaging::fx_vol_surface_pillars_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::fx_vol_surface_pillars_filter& v);
    static ores::refdata::messaging::fx_vol_surface_pillars_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::fx_vol_surface_pillar_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::fx_vol_surface_pillar_event& v);
    static ores::refdata::messaging::fx_vol_surface_pillar_event read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::fx_vol_surface_pillar_event& v);
    static ores::refdata::messaging::fx_vol_surface_pillar_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::fx_vol_surface_pillar_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::fx_vol_surface_pillar_version_key& v);
    static ores::refdata::messaging::fx_vol_surface_pillar_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::fx_vol_surface_pillar_version_key& v);
    static ores::refdata::messaging::fx_vol_surface_pillar_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::fx_vol_surface_pillar_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::fx_vol_surface_pillar_versions_filter& v);
    static ores::refdata::messaging::fx_vol_surface_pillar_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::fx_vol_surface_pillar_versions_filter& v);
    static ores::refdata::messaging::fx_vol_surface_pillar_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_fx_vol_surface_pillars_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_fx_vol_surface_pillars_request& v);
    static ores::refdata::messaging::list_fx_vol_surface_pillars_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_fx_vol_surface_pillars_request& v);
    static ores::refdata::messaging::list_fx_vol_surface_pillars_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_fx_vol_surface_pillars_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_fx_vol_surface_pillars_response& v);
    static ores::refdata::messaging::list_fx_vol_surface_pillars_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_fx_vol_surface_pillars_response& v);
    static ores::refdata::messaging::list_fx_vol_surface_pillars_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_fx_vol_surface_pillar_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_fx_vol_surface_pillar_request& v);
    static ores::refdata::messaging::get_fx_vol_surface_pillar_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_fx_vol_surface_pillar_request& v);
    static ores::refdata::messaging::get_fx_vol_surface_pillar_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_fx_vol_surface_pillar_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_fx_vol_surface_pillar_response& v);
    static ores::refdata::messaging::get_fx_vol_surface_pillar_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_fx_vol_surface_pillar_response& v);
    static ores::refdata::messaging::get_fx_vol_surface_pillar_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_fx_vol_surface_pillars_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_fx_vol_surface_pillars_request& v);
    static ores::refdata::messaging::get_many_fx_vol_surface_pillars_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_fx_vol_surface_pillars_request& v);
    static ores::refdata::messaging::get_many_fx_vol_surface_pillars_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_fx_vol_surface_pillars_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_fx_vol_surface_pillars_response& v);
    static ores::refdata::messaging::get_many_fx_vol_surface_pillars_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_fx_vol_surface_pillars_response& v);
    static ores::refdata::messaging::get_many_fx_vol_surface_pillars_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_fx_vol_surface_pillar_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_fx_vol_surface_pillar_request& v);
    static ores::refdata::messaging::put_fx_vol_surface_pillar_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_fx_vol_surface_pillar_request& v);
    static ores::refdata::messaging::put_fx_vol_surface_pillar_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_fx_vol_surface_pillar_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_fx_vol_surface_pillar_response& v);
    static ores::refdata::messaging::put_fx_vol_surface_pillar_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_fx_vol_surface_pillar_response& v);
    static ores::refdata::messaging::put_fx_vol_surface_pillar_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_fx_vol_surface_pillars_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_fx_vol_surface_pillars_request& v);
    static ores::refdata::messaging::put_many_fx_vol_surface_pillars_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_fx_vol_surface_pillars_request& v);
    static ores::refdata::messaging::put_many_fx_vol_surface_pillars_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_fx_vol_surface_pillars_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_fx_vol_surface_pillars_response& v);
    static ores::refdata::messaging::put_many_fx_vol_surface_pillars_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_fx_vol_surface_pillars_response& v);
    static ores::refdata::messaging::put_many_fx_vol_surface_pillars_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_fx_vol_surface_pillar_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_fx_vol_surface_pillar_request& v);
    static ores::refdata::messaging::delete_fx_vol_surface_pillar_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_fx_vol_surface_pillar_request& v);
    static ores::refdata::messaging::delete_fx_vol_surface_pillar_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_fx_vol_surface_pillar_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_fx_vol_surface_pillar_response& v);
    static ores::refdata::messaging::delete_fx_vol_surface_pillar_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_fx_vol_surface_pillar_response& v);
    static ores::refdata::messaging::delete_fx_vol_surface_pillar_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_fx_vol_surface_pillars_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_fx_vol_surface_pillars_request& v);
    static ores::refdata::messaging::delete_many_fx_vol_surface_pillars_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_fx_vol_surface_pillars_request& v);
    static ores::refdata::messaging::delete_many_fx_vol_surface_pillars_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_fx_vol_surface_pillars_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_fx_vol_surface_pillars_response& v);
    static ores::refdata::messaging::delete_many_fx_vol_surface_pillars_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_fx_vol_surface_pillars_response& v);
    static ores::refdata::messaging::delete_many_fx_vol_surface_pillars_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_fx_vol_surface_pillar_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_fx_vol_surface_pillar_versions_request& v);
    static ores::refdata::messaging::list_fx_vol_surface_pillar_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_fx_vol_surface_pillar_versions_request& v);
    static ores::refdata::messaging::list_fx_vol_surface_pillar_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_fx_vol_surface_pillar_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_fx_vol_surface_pillar_versions_response& v);
    static ores::refdata::messaging::list_fx_vol_surface_pillar_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_fx_vol_surface_pillar_versions_response& v);
    static ores::refdata::messaging::list_fx_vol_surface_pillar_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_fx_vol_surface_pillar_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_fx_vol_surface_pillar_version_request& v);
    static ores::refdata::messaging::get_fx_vol_surface_pillar_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_fx_vol_surface_pillar_version_request& v);
    static ores::refdata::messaging::get_fx_vol_surface_pillar_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_fx_vol_surface_pillar_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_fx_vol_surface_pillar_version_response& v);
    static ores::refdata::messaging::get_fx_vol_surface_pillar_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_fx_vol_surface_pillar_version_response& v);
    static ores::refdata::messaging::get_fx_vol_surface_pillar_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
