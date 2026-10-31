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
#ifndef ORES_REFDATA_API_MESSAGING_BUNDLE_IMPORT_ITEM_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_BUNDLE_IMPORT_ITEM_PROTOCOL_HPP

#include "ores.refdata.api/domain/bundle_import_item.hpp"
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

struct bundle_import_item_key {
    boost::uuids::uuid id;
};

struct bundle_import_item_write {
    boost::uuids::uuid id;
    boost::uuids::uuid import_id;
    std::string family;
    int sequence;
    std::string payload;
    std::optional<std::string> checksum;
};

struct bundle_import_item_change {
    bundle_import_item_write write;
    ores::utility::domain::precondition precondition;
};

struct bundle_import_item_removal {
    bundle_import_item_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct bundle_import_item_lookup {
    bundle_import_item_key key;
    std::optional<ores::refdata::domain::bundle_import_item> bundle_import_item;
};

struct bundle_import_items_filter {
    std::optional<boost::uuids::uuid> import_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> import_id_one_of;
};

struct bundle_import_item_event {
    boost::uuids::uuid event_id;
    bundle_import_item_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct bundle_import_item_version_key {
    bundle_import_item_key bundle_import_item;
    std::uint32_t version;
};

struct bundle_import_item_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_bundle_import_items_request {
    using response_type = struct list_bundle_import_items_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bundle_import_items.list";
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
    std::optional<bundle_import_items_filter> filter;
    std::optional<std::string> as_of;
};

struct list_bundle_import_items_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::bundle_import_item> bundle_import_items;
    std::uint64_t total;
};

struct get_bundle_import_item_request {
    using response_type = struct get_bundle_import_item_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bundle_import_items.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bundle_import_item_key key;
};

struct get_bundle_import_item_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::bundle_import_item> bundle_import_item;
};

struct get_many_bundle_import_items_request {
    using response_type = struct get_many_bundle_import_items_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bundle_import_items.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bundle_import_item_key> keys;
};

struct get_many_bundle_import_items_response {
    ores::utility::domain::result result;
    std::vector<bundle_import_item_lookup> entries;
};

struct put_bundle_import_item_request {
    using response_type = struct put_bundle_import_item_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bundle_import_items.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bundle_import_item_change change;
    ores::utility::domain::change_intent intent;
};

struct put_bundle_import_item_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::bundle_import_item> bundle_import_item;
};

struct put_many_bundle_import_items_request {
    using response_type = struct put_many_bundle_import_items_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bundle_import_items.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bundle_import_item_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_bundle_import_items_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::bundle_import_item> bundle_import_items;
};

struct delete_bundle_import_item_request {
    using response_type = struct delete_bundle_import_item_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bundle_import_items.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bundle_import_item_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_bundle_import_item_response {
    ores::utility::domain::result result;
};

struct delete_many_bundle_import_items_request {
    using response_type = struct delete_many_bundle_import_items_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bundle_import_items.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bundle_import_item_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_bundle_import_items_response {
    ores::utility::domain::result result;
};

struct list_by_import_id_bundle_import_items_request {
    using response_type = struct list_by_import_id_bundle_import_items_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.bundle_import_items.list_by_import_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid import_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<bundle_import_items_filter> filter;
};

struct list_by_import_id_bundle_import_items_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::bundle_import_item> bundle_import_items;
    std::uint64_t total;
};

struct list_bundle_import_item_versions_request {
    using response_type = struct list_bundle_import_item_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bundle_import_items_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bundle_import_item_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<bundle_import_item_versions_filter> filter;
};

struct list_bundle_import_item_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::bundle_import_item> versions;
    std::uint64_t total;
};

struct get_bundle_import_item_version_request {
    using response_type = struct get_bundle_import_item_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bundle_import_items_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bundle_import_item_version_key key;
};

struct get_bundle_import_item_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::bundle_import_item> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace bundle_import_item_event_subjects {
inline constexpr std::string_view created = "refdata.v1.bundle_import_items_events.created";
inline constexpr std::string_view updated = "refdata.v1.bundle_import_items_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.bundle_import_items_events.deleted";
}
/**
 * @brief The members of bundle_import_item_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bundle_import_item_key&) {
    return std::tuple{
        utility::codec::field{"id", &bundle_import_item_key::id},
    };
}

/**
 * @brief The members of bundle_import_item_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bundle_import_item_write&) {
    return std::tuple{
        utility::codec::field{"id", &bundle_import_item_write::id},
        utility::codec::field{"import_id", &bundle_import_item_write::import_id},
        utility::codec::field{"family", &bundle_import_item_write::family},
        utility::codec::field{"sequence", &bundle_import_item_write::sequence},
        utility::codec::field{"payload", &bundle_import_item_write::payload},
        utility::codec::field{"checksum", &bundle_import_item_write::checksum},
    };
}

/**
 * @brief The members of bundle_import_item_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bundle_import_item_change&) {
    return std::tuple{
        utility::codec::field{"write", &bundle_import_item_change::write},
        utility::codec::field{"precondition", &bundle_import_item_change::precondition},
    };
}

/**
 * @brief The members of bundle_import_item_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bundle_import_item_removal&) {
    return std::tuple{
        utility::codec::field{"key", &bundle_import_item_removal::key},
        utility::codec::field{"precondition", &bundle_import_item_removal::precondition},
    };
}

/**
 * @brief The members of bundle_import_item_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bundle_import_item_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &bundle_import_item_lookup::key},
        utility::codec::field{"bundle_import_item", &bundle_import_item_lookup::bundle_import_item},
    };
}

/**
 * @brief The members of bundle_import_items_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bundle_import_items_filter&) {
    return std::tuple{
        utility::codec::field{"import_id", &bundle_import_items_filter::import_id},
        utility::codec::field{"id_one_of", &bundle_import_items_filter::id_one_of},
        utility::codec::field{"import_id_one_of", &bundle_import_items_filter::import_id_one_of},
    };
}

/**
 * @brief The members of bundle_import_item_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bundle_import_item_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &bundle_import_item_event::event_id},
        utility::codec::field{"key", &bundle_import_item_event::key},
        utility::codec::field{"action", &bundle_import_item_event::action},
        utility::codec::field{"version", &bundle_import_item_event::version},
        utility::codec::field{"occurred_at", &bundle_import_item_event::occurred_at},
        utility::codec::field{"correlation_id", &bundle_import_item_event::correlation_id},
    };
}

/**
 * @brief The members of bundle_import_item_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bundle_import_item_version_key&) {
    return std::tuple{
        utility::codec::field{"bundle_import_item",
                              &bundle_import_item_version_key::bundle_import_item},
        utility::codec::field{"version", &bundle_import_item_version_key::version},
    };
}

/**
 * @brief The members of bundle_import_item_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bundle_import_item_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &bundle_import_item_versions_filter::version},
        utility::codec::field{"from_version", &bundle_import_item_versions_filter::from_version},
        utility::codec::field{"to_version", &bundle_import_item_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_bundle_import_items_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bundle_import_items_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_bundle_import_items_request::offset},
        utility::codec::field{"limit", &list_bundle_import_items_request::limit},
        utility::codec::field{"order", &list_bundle_import_items_request::order},
        utility::codec::field{"filter", &list_bundle_import_items_request::filter},
        utility::codec::field{"as_of", &list_bundle_import_items_request::as_of},
    };
}

/**
 * @brief The members of list_bundle_import_items_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bundle_import_items_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_bundle_import_items_response::result},
        utility::codec::field{"bundle_import_items",
                              &list_bundle_import_items_response::bundle_import_items},
        utility::codec::field{"total", &list_bundle_import_items_response::total},
    };
}

/**
 * @brief The members of get_bundle_import_item_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bundle_import_item_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_bundle_import_item_request::key},
    };
}

/**
 * @brief The members of get_bundle_import_item_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bundle_import_item_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_bundle_import_item_response::result},
        utility::codec::field{"bundle_import_item",
                              &get_bundle_import_item_response::bundle_import_item},
    };
}

/**
 * @brief The members of get_many_bundle_import_items_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_bundle_import_items_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_bundle_import_items_request::keys},
    };
}

/**
 * @brief The members of get_many_bundle_import_items_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_bundle_import_items_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_bundle_import_items_response::result},
        utility::codec::field{"entries", &get_many_bundle_import_items_response::entries},
    };
}

/**
 * @brief The members of put_bundle_import_item_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_bundle_import_item_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_bundle_import_item_request::change},
        utility::codec::field{"intent", &put_bundle_import_item_request::intent},
    };
}

/**
 * @brief The members of put_bundle_import_item_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_bundle_import_item_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_bundle_import_item_response::result},
        utility::codec::field{"bundle_import_item",
                              &put_bundle_import_item_response::bundle_import_item},
    };
}

/**
 * @brief The members of put_many_bundle_import_items_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_bundle_import_items_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_bundle_import_items_request::changes},
        utility::codec::field{"intent", &put_many_bundle_import_items_request::intent},
    };
}

/**
 * @brief The members of put_many_bundle_import_items_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_bundle_import_items_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_bundle_import_items_response::result},
        utility::codec::field{"bundle_import_items",
                              &put_many_bundle_import_items_response::bundle_import_items},
    };
}

/**
 * @brief The members of delete_bundle_import_item_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_bundle_import_item_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_bundle_import_item_request::removal},
        utility::codec::field{"intent", &delete_bundle_import_item_request::intent},
    };
}

/**
 * @brief The members of delete_bundle_import_item_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_bundle_import_item_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_bundle_import_item_response::result},
    };
}

/**
 * @brief The members of delete_many_bundle_import_items_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_bundle_import_items_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_bundle_import_items_request::removals},
        utility::codec::field{"intent", &delete_many_bundle_import_items_request::intent},
    };
}

/**
 * @brief The members of delete_many_bundle_import_items_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_bundle_import_items_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_bundle_import_items_response::result},
    };
}

/**
 * @brief The members of list_by_import_id_bundle_import_items_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_import_id_bundle_import_items_request&) {
    return std::tuple{
        utility::codec::field{"import_id",
                              &list_by_import_id_bundle_import_items_request::import_id},
        utility::codec::field{"scope", &list_by_import_id_bundle_import_items_request::scope},
        utility::codec::field{"offset", &list_by_import_id_bundle_import_items_request::offset},
        utility::codec::field{"limit", &list_by_import_id_bundle_import_items_request::limit},
        utility::codec::field{"order", &list_by_import_id_bundle_import_items_request::order},
        utility::codec::field{"filter", &list_by_import_id_bundle_import_items_request::filter},
    };
}

/**
 * @brief The members of list_by_import_id_bundle_import_items_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_import_id_bundle_import_items_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_by_import_id_bundle_import_items_response::result},
        utility::codec::field{"bundle_import_items",
                              &list_by_import_id_bundle_import_items_response::bundle_import_items},
        utility::codec::field{"total", &list_by_import_id_bundle_import_items_response::total},
    };
}

/**
 * @brief The members of list_bundle_import_item_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bundle_import_item_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_bundle_import_item_versions_request::key},
        utility::codec::field{"offset", &list_bundle_import_item_versions_request::offset},
        utility::codec::field{"limit", &list_bundle_import_item_versions_request::limit},
        utility::codec::field{"order", &list_bundle_import_item_versions_request::order},
        utility::codec::field{"filter", &list_bundle_import_item_versions_request::filter},
    };
}

/**
 * @brief The members of list_bundle_import_item_versions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bundle_import_item_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_bundle_import_item_versions_response::result},
        utility::codec::field{"versions", &list_bundle_import_item_versions_response::versions},
        utility::codec::field{"total", &list_bundle_import_item_versions_response::total},
    };
}

/**
 * @brief The members of get_bundle_import_item_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bundle_import_item_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_bundle_import_item_version_request::key},
    };
}

/**
 * @brief The members of get_bundle_import_item_version_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bundle_import_item_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_bundle_import_item_version_response::result},
        utility::codec::field{"version", &get_bundle_import_item_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::bundle_import_item_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::bundle_import_item_key& v);
    static ores::refdata::messaging::bundle_import_item_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::bundle_import_item_key& v);
    static ores::refdata::messaging::bundle_import_item_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::bundle_import_item_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::bundle_import_item_write& v);
    static ores::refdata::messaging::bundle_import_item_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::bundle_import_item_write& v);
    static ores::refdata::messaging::bundle_import_item_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::bundle_import_item_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::bundle_import_item_change& v);
    static ores::refdata::messaging::bundle_import_item_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::bundle_import_item_change& v);
    static ores::refdata::messaging::bundle_import_item_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::bundle_import_item_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::bundle_import_item_removal& v);
    static ores::refdata::messaging::bundle_import_item_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::bundle_import_item_removal& v);
    static ores::refdata::messaging::bundle_import_item_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::bundle_import_item_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::bundle_import_item_lookup& v);
    static ores::refdata::messaging::bundle_import_item_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::bundle_import_item_lookup& v);
    static ores::refdata::messaging::bundle_import_item_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::bundle_import_items_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::bundle_import_items_filter& v);
    static ores::refdata::messaging::bundle_import_items_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::bundle_import_items_filter& v);
    static ores::refdata::messaging::bundle_import_items_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::bundle_import_item_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::bundle_import_item_event& v);
    static ores::refdata::messaging::bundle_import_item_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::bundle_import_item_event& v);
    static ores::refdata::messaging::bundle_import_item_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::bundle_import_item_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::bundle_import_item_version_key& v);
    static ores::refdata::messaging::bundle_import_item_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::bundle_import_item_version_key& v);
    static ores::refdata::messaging::bundle_import_item_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::bundle_import_item_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::bundle_import_item_versions_filter& v);
    static ores::refdata::messaging::bundle_import_item_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::bundle_import_item_versions_filter& v);
    static ores::refdata::messaging::bundle_import_item_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_bundle_import_items_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_bundle_import_items_request& v);
    static ores::refdata::messaging::list_bundle_import_items_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_bundle_import_items_request& v);
    static ores::refdata::messaging::list_bundle_import_items_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_bundle_import_items_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_bundle_import_items_response& v);
    static ores::refdata::messaging::list_bundle_import_items_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_bundle_import_items_response& v);
    static ores::refdata::messaging::list_bundle_import_items_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_bundle_import_item_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_bundle_import_item_request& v);
    static ores::refdata::messaging::get_bundle_import_item_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_bundle_import_item_request& v);
    static ores::refdata::messaging::get_bundle_import_item_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_bundle_import_item_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_bundle_import_item_response& v);
    static ores::refdata::messaging::get_bundle_import_item_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_bundle_import_item_response& v);
    static ores::refdata::messaging::get_bundle_import_item_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_bundle_import_items_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_bundle_import_items_request& v);
    static ores::refdata::messaging::get_many_bundle_import_items_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_bundle_import_items_request& v);
    static ores::refdata::messaging::get_many_bundle_import_items_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_bundle_import_items_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_bundle_import_items_response& v);
    static ores::refdata::messaging::get_many_bundle_import_items_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_bundle_import_items_response& v);
    static ores::refdata::messaging::get_many_bundle_import_items_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_bundle_import_item_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_bundle_import_item_request& v);
    static ores::refdata::messaging::put_bundle_import_item_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_bundle_import_item_request& v);
    static ores::refdata::messaging::put_bundle_import_item_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_bundle_import_item_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_bundle_import_item_response& v);
    static ores::refdata::messaging::put_bundle_import_item_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_bundle_import_item_response& v);
    static ores::refdata::messaging::put_bundle_import_item_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_bundle_import_items_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_bundle_import_items_request& v);
    static ores::refdata::messaging::put_many_bundle_import_items_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_bundle_import_items_request& v);
    static ores::refdata::messaging::put_many_bundle_import_items_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_bundle_import_items_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_bundle_import_items_response& v);
    static ores::refdata::messaging::put_many_bundle_import_items_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_bundle_import_items_response& v);
    static ores::refdata::messaging::put_many_bundle_import_items_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_bundle_import_item_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_bundle_import_item_request& v);
    static ores::refdata::messaging::delete_bundle_import_item_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_bundle_import_item_request& v);
    static ores::refdata::messaging::delete_bundle_import_item_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_bundle_import_item_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_bundle_import_item_response& v);
    static ores::refdata::messaging::delete_bundle_import_item_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_bundle_import_item_response& v);
    static ores::refdata::messaging::delete_bundle_import_item_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_bundle_import_items_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_bundle_import_items_request& v);
    static ores::refdata::messaging::delete_many_bundle_import_items_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_bundle_import_items_request& v);
    static ores::refdata::messaging::delete_many_bundle_import_items_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_bundle_import_items_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_bundle_import_items_response& v);
    static ores::refdata::messaging::delete_many_bundle_import_items_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_bundle_import_items_response& v);
    static ores::refdata::messaging::delete_many_bundle_import_items_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_by_import_id_bundle_import_items_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_by_import_id_bundle_import_items_request& v);
    static ores::refdata::messaging::list_by_import_id_bundle_import_items_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_by_import_id_bundle_import_items_request& v);
    static ores::refdata::messaging::list_by_import_id_bundle_import_items_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_by_import_id_bundle_import_items_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_by_import_id_bundle_import_items_response& v);
    static ores::refdata::messaging::list_by_import_id_bundle_import_items_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_by_import_id_bundle_import_items_response& v);
    static ores::refdata::messaging::list_by_import_id_bundle_import_items_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_bundle_import_item_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_bundle_import_item_versions_request& v);
    static ores::refdata::messaging::list_bundle_import_item_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_bundle_import_item_versions_request& v);
    static ores::refdata::messaging::list_bundle_import_item_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_bundle_import_item_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_bundle_import_item_versions_response& v);
    static ores::refdata::messaging::list_bundle_import_item_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_bundle_import_item_versions_response& v);
    static ores::refdata::messaging::list_bundle_import_item_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_bundle_import_item_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_bundle_import_item_version_request& v);
    static ores::refdata::messaging::get_bundle_import_item_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_bundle_import_item_version_request& v);
    static ores::refdata::messaging::get_bundle_import_item_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_bundle_import_item_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_bundle_import_item_version_response& v);
    static ores::refdata::messaging::get_bundle_import_item_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_bundle_import_item_version_response& v);
    static ores::refdata::messaging::get_bundle_import_item_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
