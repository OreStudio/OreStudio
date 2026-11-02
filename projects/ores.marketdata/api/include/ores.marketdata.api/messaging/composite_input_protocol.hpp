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
#ifndef ORES_MARKETDATA_API_MESSAGING_COMPOSITE_INPUT_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_COMPOSITE_INPUT_PROTOCOL_HPP

#include "ores.marketdata.api/domain/composite_input.hpp"
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

struct composite_input_key {
    std::string oresmd_uri;
};

struct composite_input_write {
    boost::uuids::uuid id;
    boost::uuids::uuid composite_id;
    std::string provider_code;
    std::string oresmd_uri;
    int position;
};

struct composite_input_change {
    composite_input_write write;
    ores::utility::domain::precondition precondition;
};

struct composite_input_removal {
    composite_input_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct composite_input_lookup {
    composite_input_key key;
    std::optional<ores::marketdata::domain::composite_input> composite_input;
};

struct composite_inputs_filter {
    std::optional<boost::uuids::uuid> composite_id;
    std::optional<std::string> provider_code;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> composite_id_one_of;
    std::optional<std::vector<std::string>> provider_code_one_of;
};

struct composite_input_event {
    boost::uuids::uuid event_id;
    composite_input_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct composite_input_version_key {
    composite_input_key composite_input;
    std::uint32_t version;
};

struct composite_input_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_composite_inputs_request {
    using response_type = struct list_composite_inputs_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composite_inputs.list";
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
    std::optional<composite_inputs_filter> filter;
    std::optional<std::string> as_of;
};

struct list_composite_inputs_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::composite_input> composite_inputs;
    std::uint64_t total;
};

struct get_composite_input_request {
    using response_type = struct get_composite_input_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composite_inputs.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_input_key key;
};

struct get_composite_input_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::composite_input> composite_input;
};

struct get_many_composite_inputs_request {
    using response_type = struct get_many_composite_inputs_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composite_inputs.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<composite_input_key> keys;
};

struct get_many_composite_inputs_response {
    ores::utility::domain::result result;
    std::vector<composite_input_lookup> entries;
};

struct put_composite_input_request {
    using response_type = struct put_composite_input_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composite_inputs.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_input_change change;
    ores::utility::domain::change_intent intent;
};

struct put_composite_input_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::composite_input> composite_input;
};

struct put_many_composite_inputs_request {
    using response_type = struct put_many_composite_inputs_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composite_inputs.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<composite_input_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_composite_inputs_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::composite_input> composite_inputs;
};

struct delete_composite_input_request {
    using response_type = struct delete_composite_input_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composite_inputs.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_input_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_composite_input_response {
    ores::utility::domain::result result;
};

struct delete_many_composite_inputs_request {
    using response_type = struct delete_many_composite_inputs_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composite_inputs.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<composite_input_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_composite_inputs_response {
    ores::utility::domain::result result;
};

struct list_composite_input_versions_request {
    using response_type = struct list_composite_input_versions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composite_inputs_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_input_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<composite_input_versions_filter> filter;
};

struct list_composite_input_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::composite_input> versions;
    std::uint64_t total;
};

struct get_composite_input_version_request {
    using response_type = struct get_composite_input_version_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composite_inputs_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_input_version_key key;
};

struct get_composite_input_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::composite_input> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace composite_input_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.composite_inputs_events.created";
inline constexpr std::string_view updated = "marketdata.v1.composite_inputs_events.updated";
inline constexpr std::string_view deleted = "marketdata.v1.composite_inputs_events.deleted";
}
/**
 * @brief The members of composite_input_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const composite_input_key&) {
    return std::tuple{
        utility::codec::field{"oresmd_uri", &composite_input_key::oresmd_uri},
    };
}

/**
 * @brief The members of composite_input_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const composite_input_write&) {
    return std::tuple{
        utility::codec::field{"id", &composite_input_write::id},
        utility::codec::field{"composite_id", &composite_input_write::composite_id},
        utility::codec::field{"provider_code", &composite_input_write::provider_code},
        utility::codec::field{"oresmd_uri", &composite_input_write::oresmd_uri},
        utility::codec::field{"position", &composite_input_write::position},
    };
}

/**
 * @brief The members of composite_input_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const composite_input_change&) {
    return std::tuple{
        utility::codec::field{"write", &composite_input_change::write},
        utility::codec::field{"precondition", &composite_input_change::precondition},
    };
}

/**
 * @brief The members of composite_input_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const composite_input_removal&) {
    return std::tuple{
        utility::codec::field{"key", &composite_input_removal::key},
        utility::codec::field{"precondition", &composite_input_removal::precondition},
    };
}

/**
 * @brief The members of composite_input_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const composite_input_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &composite_input_lookup::key},
        utility::codec::field{"composite_input", &composite_input_lookup::composite_input},
    };
}

/**
 * @brief The members of composite_inputs_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const composite_inputs_filter&) {
    return std::tuple{
        utility::codec::field{"composite_id", &composite_inputs_filter::composite_id},
        utility::codec::field{"provider_code", &composite_inputs_filter::provider_code},
        utility::codec::field{"id_one_of", &composite_inputs_filter::id_one_of},
        utility::codec::field{"composite_id_one_of", &composite_inputs_filter::composite_id_one_of},
        utility::codec::field{"provider_code_one_of",
                              &composite_inputs_filter::provider_code_one_of},
    };
}

/**
 * @brief The members of composite_input_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const composite_input_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &composite_input_event::event_id},
        utility::codec::field{"key", &composite_input_event::key},
        utility::codec::field{"action", &composite_input_event::action},
        utility::codec::field{"version", &composite_input_event::version},
        utility::codec::field{"occurred_at", &composite_input_event::occurred_at},
        utility::codec::field{"correlation_id", &composite_input_event::correlation_id},
    };
}

/**
 * @brief The members of composite_input_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const composite_input_version_key&) {
    return std::tuple{
        utility::codec::field{"composite_input", &composite_input_version_key::composite_input},
        utility::codec::field{"version", &composite_input_version_key::version},
    };
}

/**
 * @brief The members of composite_input_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const composite_input_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &composite_input_versions_filter::version},
        utility::codec::field{"from_version", &composite_input_versions_filter::from_version},
        utility::codec::field{"to_version", &composite_input_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_composite_inputs_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_composite_inputs_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_composite_inputs_request::offset},
        utility::codec::field{"limit", &list_composite_inputs_request::limit},
        utility::codec::field{"order", &list_composite_inputs_request::order},
        utility::codec::field{"filter", &list_composite_inputs_request::filter},
        utility::codec::field{"as_of", &list_composite_inputs_request::as_of},
    };
}

/**
 * @brief The members of list_composite_inputs_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_composite_inputs_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_composite_inputs_response::result},
        utility::codec::field{"composite_inputs",
                              &list_composite_inputs_response::composite_inputs},
        utility::codec::field{"total", &list_composite_inputs_response::total},
    };
}

/**
 * @brief The members of get_composite_input_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_composite_input_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_composite_input_request::key},
    };
}

/**
 * @brief The members of get_composite_input_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_composite_input_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_composite_input_response::result},
        utility::codec::field{"composite_input", &get_composite_input_response::composite_input},
    };
}

/**
 * @brief The members of get_many_composite_inputs_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_composite_inputs_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_composite_inputs_request::keys},
    };
}

/**
 * @brief The members of get_many_composite_inputs_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_composite_inputs_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_composite_inputs_response::result},
        utility::codec::field{"entries", &get_many_composite_inputs_response::entries},
    };
}

/**
 * @brief The members of put_composite_input_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_composite_input_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_composite_input_request::change},
        utility::codec::field{"intent", &put_composite_input_request::intent},
    };
}

/**
 * @brief The members of put_composite_input_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_composite_input_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_composite_input_response::result},
        utility::codec::field{"composite_input", &put_composite_input_response::composite_input},
    };
}

/**
 * @brief The members of put_many_composite_inputs_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_composite_inputs_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_composite_inputs_request::changes},
        utility::codec::field{"intent", &put_many_composite_inputs_request::intent},
    };
}

/**
 * @brief The members of put_many_composite_inputs_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_composite_inputs_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_composite_inputs_response::result},
        utility::codec::field{"composite_inputs",
                              &put_many_composite_inputs_response::composite_inputs},
    };
}

/**
 * @brief The members of delete_composite_input_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_composite_input_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_composite_input_request::removal},
        utility::codec::field{"intent", &delete_composite_input_request::intent},
    };
}

/**
 * @brief The members of delete_composite_input_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_composite_input_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_composite_input_response::result},
    };
}

/**
 * @brief The members of delete_many_composite_inputs_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_composite_inputs_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_composite_inputs_request::removals},
        utility::codec::field{"intent", &delete_many_composite_inputs_request::intent},
    };
}

/**
 * @brief The members of delete_many_composite_inputs_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_composite_inputs_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_composite_inputs_response::result},
    };
}

/**
 * @brief The members of list_composite_input_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_composite_input_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_composite_input_versions_request::key},
        utility::codec::field{"offset", &list_composite_input_versions_request::offset},
        utility::codec::field{"limit", &list_composite_input_versions_request::limit},
        utility::codec::field{"order", &list_composite_input_versions_request::order},
        utility::codec::field{"filter", &list_composite_input_versions_request::filter},
    };
}

/**
 * @brief The members of list_composite_input_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_composite_input_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_composite_input_versions_response::result},
        utility::codec::field{"versions", &list_composite_input_versions_response::versions},
        utility::codec::field{"total", &list_composite_input_versions_response::total},
    };
}

/**
 * @brief The members of get_composite_input_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_composite_input_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_composite_input_version_request::key},
    };
}

/**
 * @brief The members of get_composite_input_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_composite_input_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_composite_input_version_response::result},
        utility::codec::field{"version", &get_composite_input_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::composite_input_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::composite_input_key& v);
    static ores::marketdata::messaging::composite_input_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::composite_input_key& v);
    static ores::marketdata::messaging::composite_input_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::composite_input_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::composite_input_write& v);
    static ores::marketdata::messaging::composite_input_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::composite_input_write& v);
    static ores::marketdata::messaging::composite_input_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::composite_input_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::composite_input_change& v);
    static ores::marketdata::messaging::composite_input_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::composite_input_change& v);
    static ores::marketdata::messaging::composite_input_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::composite_input_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::composite_input_removal& v);
    static ores::marketdata::messaging::composite_input_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::composite_input_removal& v);
    static ores::marketdata::messaging::composite_input_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::composite_input_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::composite_input_lookup& v);
    static ores::marketdata::messaging::composite_input_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::composite_input_lookup& v);
    static ores::marketdata::messaging::composite_input_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::composite_inputs_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::composite_inputs_filter& v);
    static ores::marketdata::messaging::composite_inputs_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::composite_inputs_filter& v);
    static ores::marketdata::messaging::composite_inputs_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::composite_input_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::composite_input_event& v);
    static ores::marketdata::messaging::composite_input_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::composite_input_event& v);
    static ores::marketdata::messaging::composite_input_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::composite_input_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::composite_input_version_key& v);
    static ores::marketdata::messaging::composite_input_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::composite_input_version_key& v);
    static ores::marketdata::messaging::composite_input_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::composite_input_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::composite_input_versions_filter& v);
    static ores::marketdata::messaging::composite_input_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::composite_input_versions_filter& v);
    static ores::marketdata::messaging::composite_input_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_composite_inputs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_composite_inputs_request& v);
    static ores::marketdata::messaging::list_composite_inputs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_composite_inputs_request& v);
    static ores::marketdata::messaging::list_composite_inputs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_composite_inputs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_composite_inputs_response& v);
    static ores::marketdata::messaging::list_composite_inputs_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_composite_inputs_response& v);
    static ores::marketdata::messaging::list_composite_inputs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_composite_input_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_composite_input_request& v);
    static ores::marketdata::messaging::get_composite_input_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_composite_input_request& v);
    static ores::marketdata::messaging::get_composite_input_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_composite_input_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_composite_input_response& v);
    static ores::marketdata::messaging::get_composite_input_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_composite_input_response& v);
    static ores::marketdata::messaging::get_composite_input_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_composite_inputs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_composite_inputs_request& v);
    static ores::marketdata::messaging::get_many_composite_inputs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_composite_inputs_request& v);
    static ores::marketdata::messaging::get_many_composite_inputs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_composite_inputs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_composite_inputs_response& v);
    static ores::marketdata::messaging::get_many_composite_inputs_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_composite_inputs_response& v);
    static ores::marketdata::messaging::get_many_composite_inputs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_composite_input_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_composite_input_request& v);
    static ores::marketdata::messaging::put_composite_input_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_composite_input_request& v);
    static ores::marketdata::messaging::put_composite_input_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_composite_input_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_composite_input_response& v);
    static ores::marketdata::messaging::put_composite_input_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_composite_input_response& v);
    static ores::marketdata::messaging::put_composite_input_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_composite_inputs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_composite_inputs_request& v);
    static ores::marketdata::messaging::put_many_composite_inputs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_composite_inputs_request& v);
    static ores::marketdata::messaging::put_many_composite_inputs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_composite_inputs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_composite_inputs_response& v);
    static ores::marketdata::messaging::put_many_composite_inputs_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_composite_inputs_response& v);
    static ores::marketdata::messaging::put_many_composite_inputs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_composite_input_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_composite_input_request& v);
    static ores::marketdata::messaging::delete_composite_input_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_composite_input_request& v);
    static ores::marketdata::messaging::delete_composite_input_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_composite_input_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_composite_input_response& v);
    static ores::marketdata::messaging::delete_composite_input_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_composite_input_response& v);
    static ores::marketdata::messaging::delete_composite_input_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_composite_inputs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_composite_inputs_request& v);
    static ores::marketdata::messaging::delete_many_composite_inputs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_composite_inputs_request& v);
    static ores::marketdata::messaging::delete_many_composite_inputs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_composite_inputs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_composite_inputs_response& v);
    static ores::marketdata::messaging::delete_many_composite_inputs_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_composite_inputs_response& v);
    static ores::marketdata::messaging::delete_many_composite_inputs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_composite_input_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_composite_input_versions_request& v);
    static ores::marketdata::messaging::list_composite_input_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_composite_input_versions_request& v);
    static ores::marketdata::messaging::list_composite_input_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_composite_input_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_composite_input_versions_response& v);
    static ores::marketdata::messaging::list_composite_input_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_composite_input_versions_response& v);
    static ores::marketdata::messaging::list_composite_input_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_composite_input_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_composite_input_version_request& v);
    static ores::marketdata::messaging::get_composite_input_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_composite_input_version_request& v);
    static ores::marketdata::messaging::get_composite_input_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_composite_input_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_composite_input_version_response& v);
    static ores::marketdata::messaging::get_composite_input_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_composite_input_version_response& v);
    static ores::marketdata::messaging::get_composite_input_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
