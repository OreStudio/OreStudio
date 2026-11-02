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
#ifndef ORES_MARKETDATA_API_MESSAGING_OBSERVATION_LINEAGE_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_OBSERVATION_LINEAGE_PROTOCOL_HPP

#include "ores.marketdata.api/domain/observation_lineage.hpp"
#include "ores.marketdata.api/export.hpp"
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

namespace ores::marketdata::messaging {

struct observation_lineage_key {
    boost::uuids::uuid id;
};

struct observation_lineage_write {
    boost::uuids::uuid id;
    boost::uuids::uuid party_id;
    boost::uuids::uuid series_id;
    std::chrono::system_clock::time_point observation_datetime;
    std::string oresmd_uri;
    std::string point_source_kind;
    std::optional<boost::uuids::uuid> derivation_config_id;
    int derivation_config_version;
    std::optional<std::chrono::system_clock::time_point> source_as_of;
    std::string source_series_ids;
};

struct observation_lineage_change {
    observation_lineage_write write;
    ores::utility::domain::precondition precondition;
};

struct observation_lineage_removal {
    observation_lineage_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct observation_lineage_lookup {
    observation_lineage_key key;
    std::optional<ores::marketdata::domain::observation_lineage> observation_lineage;
};

struct observation_lineages_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct observation_lineage_event {
    boost::uuids::uuid event_id;
    observation_lineage_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct observation_lineage_version_key {
    observation_lineage_key observation_lineage;
    std::uint32_t version;
};

struct observation_lineage_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_observation_lineages_request {
    using response_type = struct list_observation_lineages_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.observation_lineages.list";
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
    std::optional<observation_lineages_filter> filter;
    std::optional<std::string> as_of;
};

struct list_observation_lineages_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::observation_lineage> observation_lineages;
    std::uint64_t total;
};

struct get_observation_lineage_request {
    using response_type = struct get_observation_lineage_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.observation_lineages.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    observation_lineage_key key;
};

struct get_observation_lineage_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::observation_lineage> observation_lineage;
};

struct get_many_observation_lineages_request {
    using response_type = struct get_many_observation_lineages_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.observation_lineages.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<observation_lineage_key> keys;
};

struct get_many_observation_lineages_response {
    ores::utility::domain::result result;
    std::vector<observation_lineage_lookup> entries;
};

struct put_observation_lineage_request {
    using response_type = struct put_observation_lineage_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.observation_lineages.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    observation_lineage_change change;
    ores::utility::domain::change_intent intent;
};

struct put_observation_lineage_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::observation_lineage> observation_lineage;
};

struct put_many_observation_lineages_request {
    using response_type = struct put_many_observation_lineages_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.observation_lineages.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<observation_lineage_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_observation_lineages_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::observation_lineage> observation_lineages;
};

struct delete_observation_lineage_request {
    using response_type = struct delete_observation_lineage_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.observation_lineages.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    observation_lineage_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_observation_lineage_response {
    ores::utility::domain::result result;
};

struct delete_many_observation_lineages_request {
    using response_type = struct delete_many_observation_lineages_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.observation_lineages.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<observation_lineage_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_observation_lineages_response {
    ores::utility::domain::result result;
};

struct list_observation_lineage_versions_request {
    using response_type = struct list_observation_lineage_versions_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.observation_lineages_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    observation_lineage_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<observation_lineage_versions_filter> filter;
};

struct list_observation_lineage_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::observation_lineage> versions;
    std::uint64_t total;
};

struct get_observation_lineage_version_request {
    using response_type = struct get_observation_lineage_version_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.observation_lineages_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    observation_lineage_version_key key;
};

struct get_observation_lineage_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::observation_lineage> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace observation_lineage_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.observation_lineages_events.created";
inline constexpr std::string_view updated = "marketdata.v1.observation_lineages_events.updated";
inline constexpr std::string_view deleted = "marketdata.v1.observation_lineages_events.deleted";
}
/**
 * @brief The members of observation_lineage_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const observation_lineage_key&) {
    return std::tuple{
        utility::codec::field{"id", &observation_lineage_key::id},
    };
}

/**
 * @brief The members of observation_lineage_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const observation_lineage_write&) {
    return std::tuple{
        utility::codec::field{"id", &observation_lineage_write::id},
        utility::codec::field{"party_id", &observation_lineage_write::party_id},
        utility::codec::field{"series_id", &observation_lineage_write::series_id},
        utility::codec::field{"observation_datetime",
                              &observation_lineage_write::observation_datetime},
        utility::codec::field{"oresmd_uri", &observation_lineage_write::oresmd_uri},
        utility::codec::field{"point_source_kind", &observation_lineage_write::point_source_kind},
        utility::codec::field{"derivation_config_id",
                              &observation_lineage_write::derivation_config_id},
        utility::codec::field{"derivation_config_version",
                              &observation_lineage_write::derivation_config_version},
        utility::codec::field{"source_as_of", &observation_lineage_write::source_as_of},
        utility::codec::field{"source_series_ids", &observation_lineage_write::source_series_ids},
    };
}

/**
 * @brief The members of observation_lineage_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const observation_lineage_change&) {
    return std::tuple{
        utility::codec::field{"write", &observation_lineage_change::write},
        utility::codec::field{"precondition", &observation_lineage_change::precondition},
    };
}

/**
 * @brief The members of observation_lineage_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const observation_lineage_removal&) {
    return std::tuple{
        utility::codec::field{"key", &observation_lineage_removal::key},
        utility::codec::field{"precondition", &observation_lineage_removal::precondition},
    };
}

/**
 * @brief The members of observation_lineage_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const observation_lineage_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &observation_lineage_lookup::key},
        utility::codec::field{"observation_lineage",
                              &observation_lineage_lookup::observation_lineage},
    };
}

/**
 * @brief The members of observation_lineages_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const observation_lineages_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &observation_lineages_filter::id_one_of},
    };
}

/**
 * @brief The members of observation_lineage_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const observation_lineage_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &observation_lineage_event::event_id},
        utility::codec::field{"key", &observation_lineage_event::key},
        utility::codec::field{"action", &observation_lineage_event::action},
        utility::codec::field{"version", &observation_lineage_event::version},
        utility::codec::field{"occurred_at", &observation_lineage_event::occurred_at},
        utility::codec::field{"correlation_id", &observation_lineage_event::correlation_id},
    };
}

/**
 * @brief The members of observation_lineage_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const observation_lineage_version_key&) {
    return std::tuple{
        utility::codec::field{"observation_lineage",
                              &observation_lineage_version_key::observation_lineage},
        utility::codec::field{"version", &observation_lineage_version_key::version},
    };
}

/**
 * @brief The members of observation_lineage_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const observation_lineage_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &observation_lineage_versions_filter::version},
        utility::codec::field{"from_version", &observation_lineage_versions_filter::from_version},
        utility::codec::field{"to_version", &observation_lineage_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_observation_lineages_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_observation_lineages_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_observation_lineages_request::offset},
        utility::codec::field{"limit", &list_observation_lineages_request::limit},
        utility::codec::field{"order", &list_observation_lineages_request::order},
        utility::codec::field{"filter", &list_observation_lineages_request::filter},
        utility::codec::field{"as_of", &list_observation_lineages_request::as_of},
    };
}

/**
 * @brief The members of list_observation_lineages_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_observation_lineages_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_observation_lineages_response::result},
        utility::codec::field{"observation_lineages",
                              &list_observation_lineages_response::observation_lineages},
        utility::codec::field{"total", &list_observation_lineages_response::total},
    };
}

/**
 * @brief The members of get_observation_lineage_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_observation_lineage_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_observation_lineage_request::key},
    };
}

/**
 * @brief The members of get_observation_lineage_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_observation_lineage_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_observation_lineage_response::result},
        utility::codec::field{"observation_lineage",
                              &get_observation_lineage_response::observation_lineage},
    };
}

/**
 * @brief The members of get_many_observation_lineages_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_observation_lineages_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_observation_lineages_request::keys},
    };
}

/**
 * @brief The members of get_many_observation_lineages_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_observation_lineages_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_observation_lineages_response::result},
        utility::codec::field{"entries", &get_many_observation_lineages_response::entries},
    };
}

/**
 * @brief The members of put_observation_lineage_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_observation_lineage_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_observation_lineage_request::change},
        utility::codec::field{"intent", &put_observation_lineage_request::intent},
    };
}

/**
 * @brief The members of put_observation_lineage_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_observation_lineage_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_observation_lineage_response::result},
        utility::codec::field{"observation_lineage",
                              &put_observation_lineage_response::observation_lineage},
    };
}

/**
 * @brief The members of put_many_observation_lineages_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_observation_lineages_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_observation_lineages_request::changes},
        utility::codec::field{"intent", &put_many_observation_lineages_request::intent},
    };
}

/**
 * @brief The members of put_many_observation_lineages_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_observation_lineages_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_observation_lineages_response::result},
        utility::codec::field{"observation_lineages",
                              &put_many_observation_lineages_response::observation_lineages},
    };
}

/**
 * @brief The members of delete_observation_lineage_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_observation_lineage_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_observation_lineage_request::removal},
        utility::codec::field{"intent", &delete_observation_lineage_request::intent},
    };
}

/**
 * @brief The members of delete_observation_lineage_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_observation_lineage_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_observation_lineage_response::result},
    };
}

/**
 * @brief The members of delete_many_observation_lineages_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_observation_lineages_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_observation_lineages_request::removals},
        utility::codec::field{"intent", &delete_many_observation_lineages_request::intent},
    };
}

/**
 * @brief The members of delete_many_observation_lineages_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_observation_lineages_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_observation_lineages_response::result},
    };
}

/**
 * @brief The members of list_observation_lineage_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_observation_lineage_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_observation_lineage_versions_request::key},
        utility::codec::field{"offset", &list_observation_lineage_versions_request::offset},
        utility::codec::field{"limit", &list_observation_lineage_versions_request::limit},
        utility::codec::field{"order", &list_observation_lineage_versions_request::order},
        utility::codec::field{"filter", &list_observation_lineage_versions_request::filter},
    };
}

/**
 * @brief The members of list_observation_lineage_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_observation_lineage_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_observation_lineage_versions_response::result},
        utility::codec::field{"versions", &list_observation_lineage_versions_response::versions},
        utility::codec::field{"total", &list_observation_lineage_versions_response::total},
    };
}

/**
 * @brief The members of get_observation_lineage_version_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_observation_lineage_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_observation_lineage_version_request::key},
    };
}

/**
 * @brief The members of get_observation_lineage_version_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_observation_lineage_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_observation_lineage_version_response::result},
        utility::codec::field{"version", &get_observation_lineage_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::observation_lineage_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::observation_lineage_key& v);
    static ores::marketdata::messaging::observation_lineage_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::observation_lineage_key& v);
    static ores::marketdata::messaging::observation_lineage_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::observation_lineage_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::observation_lineage_write& v);
    static ores::marketdata::messaging::observation_lineage_write read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::observation_lineage_write& v);
    static ores::marketdata::messaging::observation_lineage_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::observation_lineage_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::observation_lineage_change& v);
    static ores::marketdata::messaging::observation_lineage_change read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::observation_lineage_change& v);
    static ores::marketdata::messaging::observation_lineage_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::observation_lineage_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::observation_lineage_removal& v);
    static ores::marketdata::messaging::observation_lineage_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::observation_lineage_removal& v);
    static ores::marketdata::messaging::observation_lineage_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::observation_lineage_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::observation_lineage_lookup& v);
    static ores::marketdata::messaging::observation_lineage_lookup read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::observation_lineage_lookup& v);
    static ores::marketdata::messaging::observation_lineage_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::observation_lineages_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::observation_lineages_filter& v);
    static ores::marketdata::messaging::observation_lineages_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::observation_lineages_filter& v);
    static ores::marketdata::messaging::observation_lineages_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::observation_lineage_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::observation_lineage_event& v);
    static ores::marketdata::messaging::observation_lineage_event read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::observation_lineage_event& v);
    static ores::marketdata::messaging::observation_lineage_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::observation_lineage_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::observation_lineage_version_key& v);
    static ores::marketdata::messaging::observation_lineage_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::observation_lineage_version_key& v);
    static ores::marketdata::messaging::observation_lineage_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::observation_lineage_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::observation_lineage_versions_filter& v);
    static ores::marketdata::messaging::observation_lineage_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::observation_lineage_versions_filter& v);
    static ores::marketdata::messaging::observation_lineage_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_observation_lineages_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_observation_lineages_request& v);
    static ores::marketdata::messaging::list_observation_lineages_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_observation_lineages_request& v);
    static ores::marketdata::messaging::list_observation_lineages_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_observation_lineages_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_observation_lineages_response& v);
    static ores::marketdata::messaging::list_observation_lineages_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_observation_lineages_response& v);
    static ores::marketdata::messaging::list_observation_lineages_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_observation_lineage_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_observation_lineage_request& v);
    static ores::marketdata::messaging::get_observation_lineage_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_observation_lineage_request& v);
    static ores::marketdata::messaging::get_observation_lineage_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_observation_lineage_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_observation_lineage_response& v);
    static ores::marketdata::messaging::get_observation_lineage_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_observation_lineage_response& v);
    static ores::marketdata::messaging::get_observation_lineage_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_observation_lineages_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_observation_lineages_request& v);
    static ores::marketdata::messaging::get_many_observation_lineages_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_observation_lineages_request& v);
    static ores::marketdata::messaging::get_many_observation_lineages_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_observation_lineages_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_observation_lineages_response& v);
    static ores::marketdata::messaging::get_many_observation_lineages_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_observation_lineages_response& v);
    static ores::marketdata::messaging::get_many_observation_lineages_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_observation_lineage_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_observation_lineage_request& v);
    static ores::marketdata::messaging::put_observation_lineage_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_observation_lineage_request& v);
    static ores::marketdata::messaging::put_observation_lineage_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_observation_lineage_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_observation_lineage_response& v);
    static ores::marketdata::messaging::put_observation_lineage_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_observation_lineage_response& v);
    static ores::marketdata::messaging::put_observation_lineage_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_observation_lineages_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_observation_lineages_request& v);
    static ores::marketdata::messaging::put_many_observation_lineages_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_observation_lineages_request& v);
    static ores::marketdata::messaging::put_many_observation_lineages_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_observation_lineages_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_observation_lineages_response& v);
    static ores::marketdata::messaging::put_many_observation_lineages_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_observation_lineages_response& v);
    static ores::marketdata::messaging::put_many_observation_lineages_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_observation_lineage_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_observation_lineage_request& v);
    static ores::marketdata::messaging::delete_observation_lineage_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_observation_lineage_request& v);
    static ores::marketdata::messaging::delete_observation_lineage_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_observation_lineage_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_observation_lineage_response& v);
    static ores::marketdata::messaging::delete_observation_lineage_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_observation_lineage_response& v);
    static ores::marketdata::messaging::delete_observation_lineage_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_observation_lineages_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_observation_lineages_request& v);
    static ores::marketdata::messaging::delete_many_observation_lineages_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_observation_lineages_request& v);
    static ores::marketdata::messaging::delete_many_observation_lineages_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_observation_lineages_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_observation_lineages_response& v);
    static ores::marketdata::messaging::delete_many_observation_lineages_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_observation_lineages_response& v);
    static ores::marketdata::messaging::delete_many_observation_lineages_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_observation_lineage_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_observation_lineage_versions_request& v);
    static ores::marketdata::messaging::list_observation_lineage_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_observation_lineage_versions_request& v);
    static ores::marketdata::messaging::list_observation_lineage_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_observation_lineage_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_observation_lineage_versions_response& v);
    static ores::marketdata::messaging::list_observation_lineage_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_observation_lineage_versions_response& v);
    static ores::marketdata::messaging::list_observation_lineage_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_observation_lineage_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_observation_lineage_version_request& v);
    static ores::marketdata::messaging::get_observation_lineage_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_observation_lineage_version_request& v);
    static ores::marketdata::messaging::get_observation_lineage_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_observation_lineage_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_observation_lineage_version_response& v);
    static ores::marketdata::messaging::get_observation_lineage_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_observation_lineage_version_response& v);
    static ores::marketdata::messaging::get_observation_lineage_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
