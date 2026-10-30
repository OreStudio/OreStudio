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
#ifndef ORES_DQ_API_MESSAGING_DATASET_BUNDLE_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_DATASET_BUNDLE_PROTOCOL_HPP

#include "ores.dq.api/domain/dataset_bundle.hpp"
#include "ores.dq.api/export.hpp"
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

namespace ores::dq::messaging {

struct dataset_bundle_key {
    std::string code;
};

struct dataset_bundle_write {
    boost::uuids::uuid id;
    std::string code;
    std::string name;
    std::string description;
};

struct dataset_bundle_change {
    dataset_bundle_write write;
    ores::utility::domain::precondition precondition;
};

struct dataset_bundle_removal {
    dataset_bundle_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct dataset_bundle_lookup {
    dataset_bundle_key key;
    std::optional<ores::dq::domain::dataset_bundle> dataset_bundle;
};

struct dataset_bundles_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct dataset_bundle_event {
    boost::uuids::uuid event_id;
    dataset_bundle_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct dataset_bundle_version_key {
    dataset_bundle_key dataset_bundle;
    std::uint32_t version;
};

struct dataset_bundle_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_dataset_bundles_request {
    using response_type = struct list_dataset_bundles_response;
    static constexpr std::string_view nats_subject = "dq.v1.dataset_bundles.list";
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
    std::optional<dataset_bundles_filter> filter;
    std::optional<std::string> as_of;
};

struct list_dataset_bundles_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::dataset_bundle> bundles;
    std::uint64_t total;
};

struct get_dataset_bundle_request {
    using response_type = struct get_dataset_bundle_response;
    static constexpr std::string_view nats_subject = "dq.v1.dataset_bundles.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    dataset_bundle_key key;
};

struct get_dataset_bundle_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::dataset_bundle> dataset_bundle;
};

struct get_many_dataset_bundles_request {
    using response_type = struct get_many_dataset_bundles_response;
    static constexpr std::string_view nats_subject = "dq.v1.dataset_bundles.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<dataset_bundle_key> keys;
};

struct get_many_dataset_bundles_response {
    ores::utility::domain::result result;
    std::vector<dataset_bundle_lookup> entries;
};

struct put_dataset_bundle_request {
    using response_type = struct put_dataset_bundle_response;
    static constexpr std::string_view nats_subject = "dq.v1.dataset_bundles.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    dataset_bundle_change change;
    ores::utility::domain::change_intent intent;
};

struct put_dataset_bundle_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::dataset_bundle> dataset_bundle;
};

struct put_many_dataset_bundles_request {
    using response_type = struct put_many_dataset_bundles_response;
    static constexpr std::string_view nats_subject = "dq.v1.dataset_bundles.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<dataset_bundle_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_dataset_bundles_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::dataset_bundle> bundles;
};

struct delete_dataset_bundle_request {
    using response_type = struct delete_dataset_bundle_response;
    static constexpr std::string_view nats_subject = "dq.v1.dataset_bundles.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    dataset_bundle_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_dataset_bundle_response {
    ores::utility::domain::result result;
};

struct delete_many_dataset_bundles_request {
    using response_type = struct delete_many_dataset_bundles_response;
    static constexpr std::string_view nats_subject = "dq.v1.dataset_bundles.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<dataset_bundle_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_dataset_bundles_response {
    ores::utility::domain::result result;
};

struct list_dataset_bundle_versions_request {
    using response_type = struct list_dataset_bundle_versions_response;
    static constexpr std::string_view nats_subject = "dq.v1.dataset_bundles_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    dataset_bundle_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<dataset_bundle_versions_filter> filter;
};

struct list_dataset_bundle_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::dataset_bundle> versions;
    std::uint64_t total;
};

struct get_dataset_bundle_version_request {
    using response_type = struct get_dataset_bundle_version_response;
    static constexpr std::string_view nats_subject = "dq.v1.dataset_bundles_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    dataset_bundle_version_key key;
};

struct get_dataset_bundle_version_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::dataset_bundle> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace dataset_bundle_event_subjects {
inline constexpr std::string_view created = "dq.v1.dataset_bundles_events.created";
inline constexpr std::string_view updated = "dq.v1.dataset_bundles_events.updated";
inline constexpr std::string_view deleted = "dq.v1.dataset_bundles_events.deleted";
}
/**
 * @brief The members of dataset_bundle_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const dataset_bundle_key&) {
    return std::tuple{
        utility::codec::field{"code", &dataset_bundle_key::code},
    };
}

/**
 * @brief The members of dataset_bundle_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const dataset_bundle_write&) {
    return std::tuple{
        utility::codec::field{"id", &dataset_bundle_write::id},
        utility::codec::field{"code", &dataset_bundle_write::code},
        utility::codec::field{"name", &dataset_bundle_write::name},
        utility::codec::field{"description", &dataset_bundle_write::description},
    };
}

/**
 * @brief The members of dataset_bundle_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const dataset_bundle_change&) {
    return std::tuple{
        utility::codec::field{"write", &dataset_bundle_change::write},
        utility::codec::field{"precondition", &dataset_bundle_change::precondition},
    };
}

/**
 * @brief The members of dataset_bundle_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const dataset_bundle_removal&) {
    return std::tuple{
        utility::codec::field{"key", &dataset_bundle_removal::key},
        utility::codec::field{"precondition", &dataset_bundle_removal::precondition},
    };
}

/**
 * @brief The members of dataset_bundle_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const dataset_bundle_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &dataset_bundle_lookup::key},
        utility::codec::field{"dataset_bundle", &dataset_bundle_lookup::dataset_bundle},
    };
}

/**
 * @brief The members of dataset_bundles_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const dataset_bundles_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &dataset_bundles_filter::id_one_of},
    };
}

/**
 * @brief The members of dataset_bundle_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const dataset_bundle_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &dataset_bundle_event::event_id},
        utility::codec::field{"key", &dataset_bundle_event::key},
        utility::codec::field{"action", &dataset_bundle_event::action},
        utility::codec::field{"version", &dataset_bundle_event::version},
        utility::codec::field{"occurred_at", &dataset_bundle_event::occurred_at},
        utility::codec::field{"correlation_id", &dataset_bundle_event::correlation_id},
    };
}

/**
 * @brief The members of dataset_bundle_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const dataset_bundle_version_key&) {
    return std::tuple{
        utility::codec::field{"dataset_bundle", &dataset_bundle_version_key::dataset_bundle},
        utility::codec::field{"version", &dataset_bundle_version_key::version},
    };
}

/**
 * @brief The members of dataset_bundle_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const dataset_bundle_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &dataset_bundle_versions_filter::version},
        utility::codec::field{"from_version", &dataset_bundle_versions_filter::from_version},
        utility::codec::field{"to_version", &dataset_bundle_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_dataset_bundles_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_dataset_bundles_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_dataset_bundles_request::offset},
        utility::codec::field{"limit", &list_dataset_bundles_request::limit},
        utility::codec::field{"order", &list_dataset_bundles_request::order},
        utility::codec::field{"filter", &list_dataset_bundles_request::filter},
        utility::codec::field{"as_of", &list_dataset_bundles_request::as_of},
    };
}

/**
 * @brief The members of list_dataset_bundles_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_dataset_bundles_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_dataset_bundles_response::result},
        utility::codec::field{"bundles", &list_dataset_bundles_response::bundles},
        utility::codec::field{"total", &list_dataset_bundles_response::total},
    };
}

/**
 * @brief The members of get_dataset_bundle_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_dataset_bundle_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_dataset_bundle_request::key},
    };
}

/**
 * @brief The members of get_dataset_bundle_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_dataset_bundle_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_dataset_bundle_response::result},
        utility::codec::field{"dataset_bundle", &get_dataset_bundle_response::dataset_bundle},
    };
}

/**
 * @brief The members of get_many_dataset_bundles_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_dataset_bundles_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_dataset_bundles_request::keys},
    };
}

/**
 * @brief The members of get_many_dataset_bundles_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_dataset_bundles_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_dataset_bundles_response::result},
        utility::codec::field{"entries", &get_many_dataset_bundles_response::entries},
    };
}

/**
 * @brief The members of put_dataset_bundle_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_dataset_bundle_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_dataset_bundle_request::change},
        utility::codec::field{"intent", &put_dataset_bundle_request::intent},
    };
}

/**
 * @brief The members of put_dataset_bundle_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_dataset_bundle_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_dataset_bundle_response::result},
        utility::codec::field{"dataset_bundle", &put_dataset_bundle_response::dataset_bundle},
    };
}

/**
 * @brief The members of put_many_dataset_bundles_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_dataset_bundles_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_dataset_bundles_request::changes},
        utility::codec::field{"intent", &put_many_dataset_bundles_request::intent},
    };
}

/**
 * @brief The members of put_many_dataset_bundles_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_dataset_bundles_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_dataset_bundles_response::result},
        utility::codec::field{"bundles", &put_many_dataset_bundles_response::bundles},
    };
}

/**
 * @brief The members of delete_dataset_bundle_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_dataset_bundle_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_dataset_bundle_request::removal},
        utility::codec::field{"intent", &delete_dataset_bundle_request::intent},
    };
}

/**
 * @brief The members of delete_dataset_bundle_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_dataset_bundle_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_dataset_bundle_response::result},
    };
}

/**
 * @brief The members of delete_many_dataset_bundles_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_dataset_bundles_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_dataset_bundles_request::removals},
        utility::codec::field{"intent", &delete_many_dataset_bundles_request::intent},
    };
}

/**
 * @brief The members of delete_many_dataset_bundles_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_dataset_bundles_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_dataset_bundles_response::result},
    };
}

/**
 * @brief The members of list_dataset_bundle_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_dataset_bundle_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_dataset_bundle_versions_request::key},
        utility::codec::field{"offset", &list_dataset_bundle_versions_request::offset},
        utility::codec::field{"limit", &list_dataset_bundle_versions_request::limit},
        utility::codec::field{"order", &list_dataset_bundle_versions_request::order},
        utility::codec::field{"filter", &list_dataset_bundle_versions_request::filter},
    };
}

/**
 * @brief The members of list_dataset_bundle_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_dataset_bundle_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_dataset_bundle_versions_response::result},
        utility::codec::field{"versions", &list_dataset_bundle_versions_response::versions},
        utility::codec::field{"total", &list_dataset_bundle_versions_response::total},
    };
}

/**
 * @brief The members of get_dataset_bundle_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_dataset_bundle_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_dataset_bundle_version_request::key},
    };
}

/**
 * @brief The members of get_dataset_bundle_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_dataset_bundle_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_dataset_bundle_version_response::result},
        utility::codec::field{"version", &get_dataset_bundle_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_bundle_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_bundle_key& v);
    static ores::dq::messaging::dataset_bundle_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_bundle_key& v);
    static ores::dq::messaging::dataset_bundle_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_bundle_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_bundle_write& v);
    static ores::dq::messaging::dataset_bundle_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_bundle_write& v);
    static ores::dq::messaging::dataset_bundle_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_bundle_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_bundle_change& v);
    static ores::dq::messaging::dataset_bundle_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_bundle_change& v);
    static ores::dq::messaging::dataset_bundle_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_bundle_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_bundle_removal& v);
    static ores::dq::messaging::dataset_bundle_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_bundle_removal& v);
    static ores::dq::messaging::dataset_bundle_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_bundle_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_bundle_lookup& v);
    static ores::dq::messaging::dataset_bundle_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_bundle_lookup& v);
    static ores::dq::messaging::dataset_bundle_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_bundles_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_bundles_filter& v);
    static ores::dq::messaging::dataset_bundles_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_bundles_filter& v);
    static ores::dq::messaging::dataset_bundles_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_bundle_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_bundle_event& v);
    static ores::dq::messaging::dataset_bundle_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_bundle_event& v);
    static ores::dq::messaging::dataset_bundle_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_bundle_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_bundle_version_key& v);
    static ores::dq::messaging::dataset_bundle_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_bundle_version_key& v);
    static ores::dq::messaging::dataset_bundle_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_bundle_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_bundle_versions_filter& v);
    static ores::dq::messaging::dataset_bundle_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_bundle_versions_filter& v);
    static ores::dq::messaging::dataset_bundle_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_dataset_bundles_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_dataset_bundles_request& v);
    static ores::dq::messaging::list_dataset_bundles_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_dataset_bundles_request& v);
    static ores::dq::messaging::list_dataset_bundles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_dataset_bundles_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_dataset_bundles_response& v);
    static ores::dq::messaging::list_dataset_bundles_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_dataset_bundles_response& v);
    static ores::dq::messaging::list_dataset_bundles_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_dataset_bundle_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_dataset_bundle_request& v);
    static ores::dq::messaging::get_dataset_bundle_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_dataset_bundle_request& v);
    static ores::dq::messaging::get_dataset_bundle_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_dataset_bundle_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_dataset_bundle_response& v);
    static ores::dq::messaging::get_dataset_bundle_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_dataset_bundle_response& v);
    static ores::dq::messaging::get_dataset_bundle_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_dataset_bundles_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_dataset_bundles_request& v);
    static ores::dq::messaging::get_many_dataset_bundles_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_many_dataset_bundles_request& v);
    static ores::dq::messaging::get_many_dataset_bundles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_dataset_bundles_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_dataset_bundles_response& v);
    static ores::dq::messaging::get_many_dataset_bundles_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_many_dataset_bundles_response& v);
    static ores::dq::messaging::get_many_dataset_bundles_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_dataset_bundle_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_dataset_bundle_request& v);
    static ores::dq::messaging::put_dataset_bundle_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_dataset_bundle_request& v);
    static ores::dq::messaging::put_dataset_bundle_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_dataset_bundle_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_dataset_bundle_response& v);
    static ores::dq::messaging::put_dataset_bundle_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_dataset_bundle_response& v);
    static ores::dq::messaging::put_dataset_bundle_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_dataset_bundles_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_dataset_bundles_request& v);
    static ores::dq::messaging::put_many_dataset_bundles_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::put_many_dataset_bundles_request& v);
    static ores::dq::messaging::put_many_dataset_bundles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_dataset_bundles_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_dataset_bundles_response& v);
    static ores::dq::messaging::put_many_dataset_bundles_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::put_many_dataset_bundles_response& v);
    static ores::dq::messaging::put_many_dataset_bundles_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_dataset_bundle_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_dataset_bundle_request& v);
    static ores::dq::messaging::delete_dataset_bundle_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_dataset_bundle_request& v);
    static ores::dq::messaging::delete_dataset_bundle_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_dataset_bundle_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_dataset_bundle_response& v);
    static ores::dq::messaging::delete_dataset_bundle_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_dataset_bundle_response& v);
    static ores::dq::messaging::delete_dataset_bundle_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_dataset_bundles_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::delete_many_dataset_bundles_request& v);
    static ores::dq::messaging::delete_many_dataset_bundles_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_dataset_bundles_request& v);
    static ores::dq::messaging::delete_many_dataset_bundles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_dataset_bundles_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::delete_many_dataset_bundles_response& v);
    static ores::dq::messaging::delete_many_dataset_bundles_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_dataset_bundles_response& v);
    static ores::dq::messaging::delete_many_dataset_bundles_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_dataset_bundle_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_dataset_bundle_versions_request& v);
    static ores::dq::messaging::list_dataset_bundle_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_dataset_bundle_versions_request& v);
    static ores::dq::messaging::list_dataset_bundle_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_dataset_bundle_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_dataset_bundle_versions_response& v);
    static ores::dq::messaging::list_dataset_bundle_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_dataset_bundle_versions_response& v);
    static ores::dq::messaging::list_dataset_bundle_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_dataset_bundle_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_dataset_bundle_version_request& v);
    static ores::dq::messaging::get_dataset_bundle_version_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_dataset_bundle_version_request& v);
    static ores::dq::messaging::get_dataset_bundle_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_dataset_bundle_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::get_dataset_bundle_version_response& v);
    static ores::dq::messaging::get_dataset_bundle_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_dataset_bundle_version_response& v);
    static ores::dq::messaging::get_dataset_bundle_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
