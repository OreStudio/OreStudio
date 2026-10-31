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
#ifndef ORES_REFDATA_API_MESSAGING_ASSET_CLASS_CODE_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_ASSET_CLASS_CODE_PROTOCOL_HPP

#include "ores.refdata.api/domain/asset_class_code.hpp"
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

struct asset_class_code_key {
    std::string code;
};

struct asset_class_code_write {
    std::string code;
    std::string name;
    std::string description;
    int display_order;
};

struct asset_class_code_change {
    asset_class_code_write write;
    ores::utility::domain::precondition precondition;
};

struct asset_class_code_removal {
    asset_class_code_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct asset_class_code_lookup {
    asset_class_code_key key;
    std::optional<ores::refdata::domain::asset_class_code> asset_class_code;
};

struct asset_class_codes_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct asset_class_code_event {
    boost::uuids::uuid event_id;
    asset_class_code_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct asset_class_code_version_key {
    asset_class_code_key asset_class_code;
    std::uint32_t version;
};

struct asset_class_code_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_asset_class_codes_request {
    using response_type = struct list_asset_class_codes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_class_codes.list";
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
    std::optional<asset_class_codes_filter> filter;
    std::optional<std::string> as_of;
};

struct list_asset_class_codes_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::asset_class_code> asset_classes;
    std::uint64_t total;
};

struct get_asset_class_code_request {
    using response_type = struct get_asset_class_code_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_class_codes.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    asset_class_code_key key;
};

struct get_asset_class_code_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::asset_class_code> asset_class_code;
};

struct get_many_asset_class_codes_request {
    using response_type = struct get_many_asset_class_codes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_class_codes.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<asset_class_code_key> keys;
};

struct get_many_asset_class_codes_response {
    ores::utility::domain::result result;
    std::vector<asset_class_code_lookup> entries;
};

struct put_asset_class_code_request {
    using response_type = struct put_asset_class_code_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_class_codes.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    asset_class_code_change change;
    ores::utility::domain::change_intent intent;
};

struct put_asset_class_code_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::asset_class_code> asset_class_code;
};

struct put_many_asset_class_codes_request {
    using response_type = struct put_many_asset_class_codes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_class_codes.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<asset_class_code_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_asset_class_codes_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::asset_class_code> asset_classes;
};

struct delete_asset_class_code_request {
    using response_type = struct delete_asset_class_code_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_class_codes.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    asset_class_code_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_asset_class_code_response {
    ores::utility::domain::result result;
};

struct delete_many_asset_class_codes_request {
    using response_type = struct delete_many_asset_class_codes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_class_codes.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<asset_class_code_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_asset_class_codes_response {
    ores::utility::domain::result result;
};

struct list_asset_class_code_versions_request {
    using response_type = struct list_asset_class_code_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_class_codes_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    asset_class_code_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<asset_class_code_versions_filter> filter;
};

struct list_asset_class_code_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::asset_class_code> versions;
    std::uint64_t total;
};

struct get_asset_class_code_version_request {
    using response_type = struct get_asset_class_code_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.asset_class_codes_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    asset_class_code_version_key key;
};

struct get_asset_class_code_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::asset_class_code> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace asset_class_code_event_subjects {
inline constexpr std::string_view created = "refdata.v1.asset_class_codes_events.created";
inline constexpr std::string_view updated = "refdata.v1.asset_class_codes_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.asset_class_codes_events.deleted";
}
/**
 * @brief The members of asset_class_code_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const asset_class_code_key&) {
    return std::tuple{
        utility::codec::field{"code", &asset_class_code_key::code},
    };
}

/**
 * @brief The members of asset_class_code_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const asset_class_code_write&) {
    return std::tuple{
        utility::codec::field{"code", &asset_class_code_write::code},
        utility::codec::field{"name", &asset_class_code_write::name},
        utility::codec::field{"description", &asset_class_code_write::description},
        utility::codec::field{"display_order", &asset_class_code_write::display_order},
    };
}

/**
 * @brief The members of asset_class_code_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const asset_class_code_change&) {
    return std::tuple{
        utility::codec::field{"write", &asset_class_code_change::write},
        utility::codec::field{"precondition", &asset_class_code_change::precondition},
    };
}

/**
 * @brief The members of asset_class_code_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const asset_class_code_removal&) {
    return std::tuple{
        utility::codec::field{"key", &asset_class_code_removal::key},
        utility::codec::field{"precondition", &asset_class_code_removal::precondition},
    };
}

/**
 * @brief The members of asset_class_code_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const asset_class_code_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &asset_class_code_lookup::key},
        utility::codec::field{"asset_class_code", &asset_class_code_lookup::asset_class_code},
    };
}

/**
 * @brief The members of asset_class_codes_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const asset_class_codes_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &asset_class_codes_filter::code_one_of},
    };
}

/**
 * @brief The members of asset_class_code_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const asset_class_code_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &asset_class_code_event::event_id},
        utility::codec::field{"key", &asset_class_code_event::key},
        utility::codec::field{"action", &asset_class_code_event::action},
        utility::codec::field{"version", &asset_class_code_event::version},
        utility::codec::field{"occurred_at", &asset_class_code_event::occurred_at},
        utility::codec::field{"correlation_id", &asset_class_code_event::correlation_id},
    };
}

/**
 * @brief The members of asset_class_code_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const asset_class_code_version_key&) {
    return std::tuple{
        utility::codec::field{"asset_class_code", &asset_class_code_version_key::asset_class_code},
        utility::codec::field{"version", &asset_class_code_version_key::version},
    };
}

/**
 * @brief The members of asset_class_code_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const asset_class_code_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &asset_class_code_versions_filter::version},
        utility::codec::field{"from_version", &asset_class_code_versions_filter::from_version},
        utility::codec::field{"to_version", &asset_class_code_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_asset_class_codes_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_asset_class_codes_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_asset_class_codes_request::offset},
        utility::codec::field{"limit", &list_asset_class_codes_request::limit},
        utility::codec::field{"order", &list_asset_class_codes_request::order},
        utility::codec::field{"filter", &list_asset_class_codes_request::filter},
        utility::codec::field{"as_of", &list_asset_class_codes_request::as_of},
    };
}

/**
 * @brief The members of list_asset_class_codes_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_asset_class_codes_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_asset_class_codes_response::result},
        utility::codec::field{"asset_classes", &list_asset_class_codes_response::asset_classes},
        utility::codec::field{"total", &list_asset_class_codes_response::total},
    };
}

/**
 * @brief The members of get_asset_class_code_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_asset_class_code_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_asset_class_code_request::key},
    };
}

/**
 * @brief The members of get_asset_class_code_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_asset_class_code_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_asset_class_code_response::result},
        utility::codec::field{"asset_class_code", &get_asset_class_code_response::asset_class_code},
    };
}

/**
 * @brief The members of get_many_asset_class_codes_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_asset_class_codes_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_asset_class_codes_request::keys},
    };
}

/**
 * @brief The members of get_many_asset_class_codes_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_asset_class_codes_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_asset_class_codes_response::result},
        utility::codec::field{"entries", &get_many_asset_class_codes_response::entries},
    };
}

/**
 * @brief The members of put_asset_class_code_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_asset_class_code_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_asset_class_code_request::change},
        utility::codec::field{"intent", &put_asset_class_code_request::intent},
    };
}

/**
 * @brief The members of put_asset_class_code_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_asset_class_code_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_asset_class_code_response::result},
        utility::codec::field{"asset_class_code", &put_asset_class_code_response::asset_class_code},
    };
}

/**
 * @brief The members of put_many_asset_class_codes_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_asset_class_codes_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_asset_class_codes_request::changes},
        utility::codec::field{"intent", &put_many_asset_class_codes_request::intent},
    };
}

/**
 * @brief The members of put_many_asset_class_codes_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_asset_class_codes_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_asset_class_codes_response::result},
        utility::codec::field{"asset_classes", &put_many_asset_class_codes_response::asset_classes},
    };
}

/**
 * @brief The members of delete_asset_class_code_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_asset_class_code_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_asset_class_code_request::removal},
        utility::codec::field{"intent", &delete_asset_class_code_request::intent},
    };
}

/**
 * @brief The members of delete_asset_class_code_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_asset_class_code_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_asset_class_code_response::result},
    };
}

/**
 * @brief The members of delete_many_asset_class_codes_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_asset_class_codes_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_asset_class_codes_request::removals},
        utility::codec::field{"intent", &delete_many_asset_class_codes_request::intent},
    };
}

/**
 * @brief The members of delete_many_asset_class_codes_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_asset_class_codes_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_asset_class_codes_response::result},
    };
}

/**
 * @brief The members of list_asset_class_code_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_asset_class_code_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_asset_class_code_versions_request::key},
        utility::codec::field{"offset", &list_asset_class_code_versions_request::offset},
        utility::codec::field{"limit", &list_asset_class_code_versions_request::limit},
        utility::codec::field{"order", &list_asset_class_code_versions_request::order},
        utility::codec::field{"filter", &list_asset_class_code_versions_request::filter},
    };
}

/**
 * @brief The members of list_asset_class_code_versions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_asset_class_code_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_asset_class_code_versions_response::result},
        utility::codec::field{"versions", &list_asset_class_code_versions_response::versions},
        utility::codec::field{"total", &list_asset_class_code_versions_response::total},
    };
}

/**
 * @brief The members of get_asset_class_code_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_asset_class_code_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_asset_class_code_version_request::key},
    };
}

/**
 * @brief The members of get_asset_class_code_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_asset_class_code_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_asset_class_code_version_response::result},
        utility::codec::field{"version", &get_asset_class_code_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::asset_class_code_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::asset_class_code_key& v);
    static ores::refdata::messaging::asset_class_code_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::asset_class_code_key& v);
    static ores::refdata::messaging::asset_class_code_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::asset_class_code_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::asset_class_code_write& v);
    static ores::refdata::messaging::asset_class_code_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::asset_class_code_write& v);
    static ores::refdata::messaging::asset_class_code_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::asset_class_code_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::asset_class_code_change& v);
    static ores::refdata::messaging::asset_class_code_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::asset_class_code_change& v);
    static ores::refdata::messaging::asset_class_code_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::asset_class_code_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::asset_class_code_removal& v);
    static ores::refdata::messaging::asset_class_code_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::asset_class_code_removal& v);
    static ores::refdata::messaging::asset_class_code_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::asset_class_code_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::asset_class_code_lookup& v);
    static ores::refdata::messaging::asset_class_code_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::asset_class_code_lookup& v);
    static ores::refdata::messaging::asset_class_code_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::asset_class_codes_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::asset_class_codes_filter& v);
    static ores::refdata::messaging::asset_class_codes_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::asset_class_codes_filter& v);
    static ores::refdata::messaging::asset_class_codes_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::asset_class_code_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::asset_class_code_event& v);
    static ores::refdata::messaging::asset_class_code_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::asset_class_code_event& v);
    static ores::refdata::messaging::asset_class_code_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::asset_class_code_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::asset_class_code_version_key& v);
    static ores::refdata::messaging::asset_class_code_version_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::asset_class_code_version_key& v);
    static ores::refdata::messaging::asset_class_code_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::asset_class_code_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::asset_class_code_versions_filter& v);
    static ores::refdata::messaging::asset_class_code_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::asset_class_code_versions_filter& v);
    static ores::refdata::messaging::asset_class_code_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_asset_class_codes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_asset_class_codes_request& v);
    static ores::refdata::messaging::list_asset_class_codes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_asset_class_codes_request& v);
    static ores::refdata::messaging::list_asset_class_codes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_asset_class_codes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_asset_class_codes_response& v);
    static ores::refdata::messaging::list_asset_class_codes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_asset_class_codes_response& v);
    static ores::refdata::messaging::list_asset_class_codes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_asset_class_code_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_asset_class_code_request& v);
    static ores::refdata::messaging::get_asset_class_code_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_asset_class_code_request& v);
    static ores::refdata::messaging::get_asset_class_code_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_asset_class_code_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_asset_class_code_response& v);
    static ores::refdata::messaging::get_asset_class_code_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_asset_class_code_response& v);
    static ores::refdata::messaging::get_asset_class_code_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_asset_class_codes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_asset_class_codes_request& v);
    static ores::refdata::messaging::get_many_asset_class_codes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_asset_class_codes_request& v);
    static ores::refdata::messaging::get_many_asset_class_codes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_asset_class_codes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_asset_class_codes_response& v);
    static ores::refdata::messaging::get_many_asset_class_codes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_asset_class_codes_response& v);
    static ores::refdata::messaging::get_many_asset_class_codes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_asset_class_code_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_asset_class_code_request& v);
    static ores::refdata::messaging::put_asset_class_code_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_asset_class_code_request& v);
    static ores::refdata::messaging::put_asset_class_code_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_asset_class_code_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_asset_class_code_response& v);
    static ores::refdata::messaging::put_asset_class_code_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_asset_class_code_response& v);
    static ores::refdata::messaging::put_asset_class_code_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_asset_class_codes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_asset_class_codes_request& v);
    static ores::refdata::messaging::put_many_asset_class_codes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_asset_class_codes_request& v);
    static ores::refdata::messaging::put_many_asset_class_codes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_asset_class_codes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_asset_class_codes_response& v);
    static ores::refdata::messaging::put_many_asset_class_codes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_asset_class_codes_response& v);
    static ores::refdata::messaging::put_many_asset_class_codes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_asset_class_code_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_asset_class_code_request& v);
    static ores::refdata::messaging::delete_asset_class_code_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_asset_class_code_request& v);
    static ores::refdata::messaging::delete_asset_class_code_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_asset_class_code_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_asset_class_code_response& v);
    static ores::refdata::messaging::delete_asset_class_code_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_asset_class_code_response& v);
    static ores::refdata::messaging::delete_asset_class_code_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_asset_class_codes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_asset_class_codes_request& v);
    static ores::refdata::messaging::delete_many_asset_class_codes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_asset_class_codes_request& v);
    static ores::refdata::messaging::delete_many_asset_class_codes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_asset_class_codes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_asset_class_codes_response& v);
    static ores::refdata::messaging::delete_many_asset_class_codes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_asset_class_codes_response& v);
    static ores::refdata::messaging::delete_many_asset_class_codes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_asset_class_code_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_asset_class_code_versions_request& v);
    static ores::refdata::messaging::list_asset_class_code_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_asset_class_code_versions_request& v);
    static ores::refdata::messaging::list_asset_class_code_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_asset_class_code_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_asset_class_code_versions_response& v);
    static ores::refdata::messaging::list_asset_class_code_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_asset_class_code_versions_response& v);
    static ores::refdata::messaging::list_asset_class_code_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_asset_class_code_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_asset_class_code_version_request& v);
    static ores::refdata::messaging::get_asset_class_code_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_asset_class_code_version_request& v);
    static ores::refdata::messaging::get_asset_class_code_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_asset_class_code_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_asset_class_code_version_response& v);
    static ores::refdata::messaging::get_asset_class_code_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_asset_class_code_version_response& v);
    static ores::refdata::messaging::get_asset_class_code_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
