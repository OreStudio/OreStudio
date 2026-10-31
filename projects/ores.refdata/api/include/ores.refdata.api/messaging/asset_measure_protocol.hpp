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
#ifndef ORES_REFDATA_API_MESSAGING_ASSET_MEASURE_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_ASSET_MEASURE_PROTOCOL_HPP

#include "ores.refdata.api/domain/asset_measure.hpp"
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

struct asset_measure_key {
    std::string code;
    std::string coding_scheme_code;
};

struct asset_measure_write {
    std::string code;
    std::string coding_scheme_code;
    std::string source;
    std::string description;
};

struct asset_measure_change {
    asset_measure_write write;
    ores::utility::domain::precondition precondition;
};

struct asset_measure_removal {
    asset_measure_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct asset_measure_lookup {
    asset_measure_key key;
    std::optional<ores::refdata::domain::asset_measure> asset_measure;
};

struct asset_measure_event {
    boost::uuids::uuid event_id;
    asset_measure_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct asset_measure_version_key {
    asset_measure_key asset_measure;
    std::uint32_t version;
};

struct asset_measure_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_asset_measures_request {
    using response_type = struct list_asset_measures_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_measures.list";
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
};

struct list_asset_measures_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::asset_measure> measures;
    std::uint64_t total;
};

struct get_asset_measure_request {
    using response_type = struct get_asset_measure_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_measures.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    asset_measure_key key;
};

struct get_asset_measure_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::asset_measure> asset_measure;
};

struct get_many_asset_measures_request {
    using response_type = struct get_many_asset_measures_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_measures.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<asset_measure_key> keys;
};

struct get_many_asset_measures_response {
    ores::utility::domain::result result;
    std::vector<asset_measure_lookup> entries;
};

struct put_asset_measure_request {
    using response_type = struct put_asset_measure_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_measures.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    asset_measure_change change;
    ores::utility::domain::change_intent intent;
};

struct put_asset_measure_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::asset_measure> asset_measure;
};

struct put_many_asset_measures_request {
    using response_type = struct put_many_asset_measures_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_measures.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<asset_measure_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_asset_measures_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::asset_measure> measures;
};

struct delete_asset_measure_request {
    using response_type = struct delete_asset_measure_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_measures.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    asset_measure_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_asset_measure_response {
    ores::utility::domain::result result;
};

struct delete_many_asset_measures_request {
    using response_type = struct delete_many_asset_measures_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_measures.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<asset_measure_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_asset_measures_response {
    ores::utility::domain::result result;
};

struct list_asset_measure_versions_request {
    using response_type = struct list_asset_measure_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_measures_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    asset_measure_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<asset_measure_versions_filter> filter;
};

struct list_asset_measure_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::asset_measure> versions;
    std::uint64_t total;
};

struct get_asset_measure_version_request {
    using response_type = struct get_asset_measure_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_measures_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    asset_measure_version_key key;
};

struct get_asset_measure_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::asset_measure> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace asset_measure_event_subjects {
inline constexpr std::string_view created = "refdata.v1.asset_measures_events.created";
inline constexpr std::string_view updated = "refdata.v1.asset_measures_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.asset_measures_events.deleted";
}
/**
 * @brief The members of asset_measure_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const asset_measure_key&) {
    return std::tuple{
        utility::codec::field{"code", &asset_measure_key::code},
        utility::codec::field{"coding_scheme_code", &asset_measure_key::coding_scheme_code},
    };
}

/**
 * @brief The members of asset_measure_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const asset_measure_write&) {
    return std::tuple{
        utility::codec::field{"code", &asset_measure_write::code},
        utility::codec::field{"coding_scheme_code", &asset_measure_write::coding_scheme_code},
        utility::codec::field{"source", &asset_measure_write::source},
        utility::codec::field{"description", &asset_measure_write::description},
    };
}

/**
 * @brief The members of asset_measure_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const asset_measure_change&) {
    return std::tuple{
        utility::codec::field{"write", &asset_measure_change::write},
        utility::codec::field{"precondition", &asset_measure_change::precondition},
    };
}

/**
 * @brief The members of asset_measure_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const asset_measure_removal&) {
    return std::tuple{
        utility::codec::field{"key", &asset_measure_removal::key},
        utility::codec::field{"precondition", &asset_measure_removal::precondition},
    };
}

/**
 * @brief The members of asset_measure_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const asset_measure_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &asset_measure_lookup::key},
        utility::codec::field{"asset_measure", &asset_measure_lookup::asset_measure},
    };
}

/**
 * @brief The members of asset_measure_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const asset_measure_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &asset_measure_event::event_id},
        utility::codec::field{"key", &asset_measure_event::key},
        utility::codec::field{"action", &asset_measure_event::action},
        utility::codec::field{"version", &asset_measure_event::version},
        utility::codec::field{"occurred_at", &asset_measure_event::occurred_at},
        utility::codec::field{"correlation_id", &asset_measure_event::correlation_id},
    };
}

/**
 * @brief The members of asset_measure_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const asset_measure_version_key&) {
    return std::tuple{
        utility::codec::field{"asset_measure", &asset_measure_version_key::asset_measure},
        utility::codec::field{"version", &asset_measure_version_key::version},
    };
}

/**
 * @brief The members of asset_measure_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const asset_measure_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &asset_measure_versions_filter::version},
        utility::codec::field{"from_version", &asset_measure_versions_filter::from_version},
        utility::codec::field{"to_version", &asset_measure_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_asset_measures_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_asset_measures_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_asset_measures_request::offset},
        utility::codec::field{"limit", &list_asset_measures_request::limit},
        utility::codec::field{"order", &list_asset_measures_request::order},
        utility::codec::field{"as_of", &list_asset_measures_request::as_of},
    };
}

/**
 * @brief The members of list_asset_measures_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_asset_measures_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_asset_measures_response::result},
        utility::codec::field{"measures", &list_asset_measures_response::measures},
        utility::codec::field{"total", &list_asset_measures_response::total},
    };
}

/**
 * @brief The members of get_asset_measure_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_asset_measure_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_asset_measure_request::key},
    };
}

/**
 * @brief The members of get_asset_measure_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_asset_measure_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_asset_measure_response::result},
        utility::codec::field{"asset_measure", &get_asset_measure_response::asset_measure},
    };
}

/**
 * @brief The members of get_many_asset_measures_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_asset_measures_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_asset_measures_request::keys},
    };
}

/**
 * @brief The members of get_many_asset_measures_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_asset_measures_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_asset_measures_response::result},
        utility::codec::field{"entries", &get_many_asset_measures_response::entries},
    };
}

/**
 * @brief The members of put_asset_measure_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_asset_measure_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_asset_measure_request::change},
        utility::codec::field{"intent", &put_asset_measure_request::intent},
    };
}

/**
 * @brief The members of put_asset_measure_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_asset_measure_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_asset_measure_response::result},
        utility::codec::field{"asset_measure", &put_asset_measure_response::asset_measure},
    };
}

/**
 * @brief The members of put_many_asset_measures_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_asset_measures_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_asset_measures_request::changes},
        utility::codec::field{"intent", &put_many_asset_measures_request::intent},
    };
}

/**
 * @brief The members of put_many_asset_measures_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_asset_measures_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_asset_measures_response::result},
        utility::codec::field{"measures", &put_many_asset_measures_response::measures},
    };
}

/**
 * @brief The members of delete_asset_measure_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_asset_measure_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_asset_measure_request::removal},
        utility::codec::field{"intent", &delete_asset_measure_request::intent},
    };
}

/**
 * @brief The members of delete_asset_measure_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_asset_measure_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_asset_measure_response::result},
    };
}

/**
 * @brief The members of delete_many_asset_measures_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_asset_measures_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_asset_measures_request::removals},
        utility::codec::field{"intent", &delete_many_asset_measures_request::intent},
    };
}

/**
 * @brief The members of delete_many_asset_measures_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_asset_measures_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_asset_measures_response::result},
    };
}

/**
 * @brief The members of list_asset_measure_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_asset_measure_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_asset_measure_versions_request::key},
        utility::codec::field{"offset", &list_asset_measure_versions_request::offset},
        utility::codec::field{"limit", &list_asset_measure_versions_request::limit},
        utility::codec::field{"order", &list_asset_measure_versions_request::order},
        utility::codec::field{"filter", &list_asset_measure_versions_request::filter},
    };
}

/**
 * @brief The members of list_asset_measure_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_asset_measure_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_asset_measure_versions_response::result},
        utility::codec::field{"versions", &list_asset_measure_versions_response::versions},
        utility::codec::field{"total", &list_asset_measure_versions_response::total},
    };
}

/**
 * @brief The members of get_asset_measure_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_asset_measure_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_asset_measure_version_request::key},
    };
}

/**
 * @brief The members of get_asset_measure_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_asset_measure_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_asset_measure_version_response::result},
        utility::codec::field{"version", &get_asset_measure_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::asset_measure_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::asset_measure_key& v);
    static ores::refdata::messaging::asset_measure_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::asset_measure_key& v);
    static ores::refdata::messaging::asset_measure_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::asset_measure_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::asset_measure_write& v);
    static ores::refdata::messaging::asset_measure_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::asset_measure_write& v);
    static ores::refdata::messaging::asset_measure_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::asset_measure_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::asset_measure_change& v);
    static ores::refdata::messaging::asset_measure_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::asset_measure_change& v);
    static ores::refdata::messaging::asset_measure_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::asset_measure_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::asset_measure_removal& v);
    static ores::refdata::messaging::asset_measure_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::asset_measure_removal& v);
    static ores::refdata::messaging::asset_measure_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::asset_measure_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::asset_measure_lookup& v);
    static ores::refdata::messaging::asset_measure_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::asset_measure_lookup& v);
    static ores::refdata::messaging::asset_measure_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::asset_measure_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::asset_measure_event& v);
    static ores::refdata::messaging::asset_measure_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::asset_measure_event& v);
    static ores::refdata::messaging::asset_measure_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::asset_measure_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::asset_measure_version_key& v);
    static ores::refdata::messaging::asset_measure_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::asset_measure_version_key& v);
    static ores::refdata::messaging::asset_measure_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::asset_measure_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::asset_measure_versions_filter& v);
    static ores::refdata::messaging::asset_measure_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::asset_measure_versions_filter& v);
    static ores::refdata::messaging::asset_measure_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_asset_measures_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_asset_measures_request& v);
    static ores::refdata::messaging::list_asset_measures_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_asset_measures_request& v);
    static ores::refdata::messaging::list_asset_measures_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_asset_measures_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_asset_measures_response& v);
    static ores::refdata::messaging::list_asset_measures_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_asset_measures_response& v);
    static ores::refdata::messaging::list_asset_measures_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_asset_measure_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_asset_measure_request& v);
    static ores::refdata::messaging::get_asset_measure_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_asset_measure_request& v);
    static ores::refdata::messaging::get_asset_measure_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_asset_measure_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_asset_measure_response& v);
    static ores::refdata::messaging::get_asset_measure_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_asset_measure_response& v);
    static ores::refdata::messaging::get_asset_measure_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_asset_measures_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_asset_measures_request& v);
    static ores::refdata::messaging::get_many_asset_measures_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_asset_measures_request& v);
    static ores::refdata::messaging::get_many_asset_measures_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_asset_measures_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_asset_measures_response& v);
    static ores::refdata::messaging::get_many_asset_measures_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_asset_measures_response& v);
    static ores::refdata::messaging::get_many_asset_measures_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_asset_measure_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_asset_measure_request& v);
    static ores::refdata::messaging::put_asset_measure_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_asset_measure_request& v);
    static ores::refdata::messaging::put_asset_measure_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_asset_measure_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_asset_measure_response& v);
    static ores::refdata::messaging::put_asset_measure_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_asset_measure_response& v);
    static ores::refdata::messaging::put_asset_measure_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_asset_measures_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_asset_measures_request& v);
    static ores::refdata::messaging::put_many_asset_measures_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_asset_measures_request& v);
    static ores::refdata::messaging::put_many_asset_measures_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_asset_measures_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_asset_measures_response& v);
    static ores::refdata::messaging::put_many_asset_measures_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_asset_measures_response& v);
    static ores::refdata::messaging::put_many_asset_measures_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_asset_measure_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_asset_measure_request& v);
    static ores::refdata::messaging::delete_asset_measure_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_asset_measure_request& v);
    static ores::refdata::messaging::delete_asset_measure_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_asset_measure_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_asset_measure_response& v);
    static ores::refdata::messaging::delete_asset_measure_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_asset_measure_response& v);
    static ores::refdata::messaging::delete_asset_measure_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_asset_measures_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_asset_measures_request& v);
    static ores::refdata::messaging::delete_many_asset_measures_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_asset_measures_request& v);
    static ores::refdata::messaging::delete_many_asset_measures_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_asset_measures_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_asset_measures_response& v);
    static ores::refdata::messaging::delete_many_asset_measures_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_asset_measures_response& v);
    static ores::refdata::messaging::delete_many_asset_measures_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_asset_measure_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_asset_measure_versions_request& v);
    static ores::refdata::messaging::list_asset_measure_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_asset_measure_versions_request& v);
    static ores::refdata::messaging::list_asset_measure_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_asset_measure_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_asset_measure_versions_response& v);
    static ores::refdata::messaging::list_asset_measure_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_asset_measure_versions_response& v);
    static ores::refdata::messaging::list_asset_measure_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_asset_measure_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_asset_measure_version_request& v);
    static ores::refdata::messaging::get_asset_measure_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_asset_measure_version_request& v);
    static ores::refdata::messaging::get_asset_measure_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_asset_measure_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_asset_measure_version_response& v);
    static ores::refdata::messaging::get_asset_measure_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_asset_measure_version_response& v);
    static ores::refdata::messaging::get_asset_measure_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
