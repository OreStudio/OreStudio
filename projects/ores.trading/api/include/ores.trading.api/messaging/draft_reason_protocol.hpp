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
#ifndef ORES_TRADING_API_MESSAGING_DRAFT_REASON_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_DRAFT_REASON_PROTOCOL_HPP

#include "ores.trading.api/domain/draft_reason.hpp"
#include "ores.trading.api/export.hpp"
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

namespace ores::trading::messaging {

struct draft_reason_key {
    std::string code;
};

struct draft_reason_write {
    std::string code;
    std::string description;
    std::string responsible_role;
};

struct draft_reason_change {
    draft_reason_write write;
    ores::utility::domain::precondition precondition;
};

struct draft_reason_removal {
    draft_reason_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct draft_reason_lookup {
    draft_reason_key key;
    std::optional<ores::trading::domain::draft_reason> draft_reason;
};

struct draft_reasons_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct draft_reason_event {
    boost::uuids::uuid event_id;
    draft_reason_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct draft_reason_version_key {
    draft_reason_key draft_reason;
    std::uint32_t version;
};

struct draft_reason_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_draft_reasons_request {
    using response_type = struct list_draft_reasons_response;
    static constexpr std::string_view nats_subject = "trading.v1.draft_reasons.list";
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
    std::optional<draft_reasons_filter> filter;
    std::optional<std::string> as_of;
};

struct list_draft_reasons_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::draft_reason> draft_reasons;
    std::uint64_t total;
};

struct get_draft_reason_request {
    using response_type = struct get_draft_reason_response;
    static constexpr std::string_view nats_subject = "trading.v1.draft_reasons.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    draft_reason_key key;
};

struct get_draft_reason_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::draft_reason> draft_reason;
};

struct get_many_draft_reasons_request {
    using response_type = struct get_many_draft_reasons_response;
    static constexpr std::string_view nats_subject = "trading.v1.draft_reasons.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<draft_reason_key> keys;
};

struct get_many_draft_reasons_response {
    ores::utility::domain::result result;
    std::vector<draft_reason_lookup> entries;
};

struct put_draft_reason_request {
    using response_type = struct put_draft_reason_response;
    static constexpr std::string_view nats_subject = "trading.v1.draft_reasons.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    draft_reason_change change;
    ores::utility::domain::change_intent intent;
};

struct put_draft_reason_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::draft_reason> draft_reason;
};

struct put_many_draft_reasons_request {
    using response_type = struct put_many_draft_reasons_response;
    static constexpr std::string_view nats_subject = "trading.v1.draft_reasons.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<draft_reason_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_draft_reasons_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::draft_reason> draft_reasons;
};

struct delete_draft_reason_request {
    using response_type = struct delete_draft_reason_response;
    static constexpr std::string_view nats_subject = "trading.v1.draft_reasons.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    draft_reason_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_draft_reason_response {
    ores::utility::domain::result result;
};

struct delete_many_draft_reasons_request {
    using response_type = struct delete_many_draft_reasons_response;
    static constexpr std::string_view nats_subject = "trading.v1.draft_reasons.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<draft_reason_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_draft_reasons_response {
    ores::utility::domain::result result;
};

struct list_draft_reason_versions_request {
    using response_type = struct list_draft_reason_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.draft_reasons_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    draft_reason_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<draft_reason_versions_filter> filter;
};

struct list_draft_reason_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::draft_reason> versions;
    std::uint64_t total;
};

struct get_draft_reason_version_request {
    using response_type = struct get_draft_reason_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.draft_reasons_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    draft_reason_version_key key;
};

struct get_draft_reason_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::draft_reason> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace draft_reason_event_subjects {
inline constexpr std::string_view created = "trading.v1.draft_reasons_events.created";
inline constexpr std::string_view updated = "trading.v1.draft_reasons_events.updated";
inline constexpr std::string_view deleted = "trading.v1.draft_reasons_events.deleted";
}
/**
 * @brief The members of draft_reason_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const draft_reason_key&) {
    return std::tuple{
        utility::codec::field{"code", &draft_reason_key::code},
    };
}

/**
 * @brief The members of draft_reason_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const draft_reason_write&) {
    return std::tuple{
        utility::codec::field{"code", &draft_reason_write::code},
        utility::codec::field{"description", &draft_reason_write::description},
        utility::codec::field{"responsible_role", &draft_reason_write::responsible_role},
    };
}

/**
 * @brief The members of draft_reason_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const draft_reason_change&) {
    return std::tuple{
        utility::codec::field{"write", &draft_reason_change::write},
        utility::codec::field{"precondition", &draft_reason_change::precondition},
    };
}

/**
 * @brief The members of draft_reason_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const draft_reason_removal&) {
    return std::tuple{
        utility::codec::field{"key", &draft_reason_removal::key},
        utility::codec::field{"precondition", &draft_reason_removal::precondition},
    };
}

/**
 * @brief The members of draft_reason_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const draft_reason_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &draft_reason_lookup::key},
        utility::codec::field{"draft_reason", &draft_reason_lookup::draft_reason},
    };
}

/**
 * @brief The members of draft_reasons_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const draft_reasons_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &draft_reasons_filter::code_one_of},
    };
}

/**
 * @brief The members of draft_reason_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const draft_reason_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &draft_reason_event::event_id},
        utility::codec::field{"key", &draft_reason_event::key},
        utility::codec::field{"action", &draft_reason_event::action},
        utility::codec::field{"version", &draft_reason_event::version},
        utility::codec::field{"occurred_at", &draft_reason_event::occurred_at},
        utility::codec::field{"correlation_id", &draft_reason_event::correlation_id},
    };
}

/**
 * @brief The members of draft_reason_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const draft_reason_version_key&) {
    return std::tuple{
        utility::codec::field{"draft_reason", &draft_reason_version_key::draft_reason},
        utility::codec::field{"version", &draft_reason_version_key::version},
    };
}

/**
 * @brief The members of draft_reason_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const draft_reason_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &draft_reason_versions_filter::version},
        utility::codec::field{"from_version", &draft_reason_versions_filter::from_version},
        utility::codec::field{"to_version", &draft_reason_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_draft_reasons_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_draft_reasons_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_draft_reasons_request::offset},
        utility::codec::field{"limit", &list_draft_reasons_request::limit},
        utility::codec::field{"order", &list_draft_reasons_request::order},
        utility::codec::field{"filter", &list_draft_reasons_request::filter},
        utility::codec::field{"as_of", &list_draft_reasons_request::as_of},
    };
}

/**
 * @brief The members of list_draft_reasons_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_draft_reasons_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_draft_reasons_response::result},
        utility::codec::field{"draft_reasons", &list_draft_reasons_response::draft_reasons},
        utility::codec::field{"total", &list_draft_reasons_response::total},
    };
}

/**
 * @brief The members of get_draft_reason_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_draft_reason_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_draft_reason_request::key},
    };
}

/**
 * @brief The members of get_draft_reason_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_draft_reason_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_draft_reason_response::result},
        utility::codec::field{"draft_reason", &get_draft_reason_response::draft_reason},
    };
}

/**
 * @brief The members of get_many_draft_reasons_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_draft_reasons_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_draft_reasons_request::keys},
    };
}

/**
 * @brief The members of get_many_draft_reasons_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_draft_reasons_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_draft_reasons_response::result},
        utility::codec::field{"entries", &get_many_draft_reasons_response::entries},
    };
}

/**
 * @brief The members of put_draft_reason_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_draft_reason_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_draft_reason_request::change},
        utility::codec::field{"intent", &put_draft_reason_request::intent},
    };
}

/**
 * @brief The members of put_draft_reason_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_draft_reason_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_draft_reason_response::result},
        utility::codec::field{"draft_reason", &put_draft_reason_response::draft_reason},
    };
}

/**
 * @brief The members of put_many_draft_reasons_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_draft_reasons_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_draft_reasons_request::changes},
        utility::codec::field{"intent", &put_many_draft_reasons_request::intent},
    };
}

/**
 * @brief The members of put_many_draft_reasons_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_draft_reasons_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_draft_reasons_response::result},
        utility::codec::field{"draft_reasons", &put_many_draft_reasons_response::draft_reasons},
    };
}

/**
 * @brief The members of delete_draft_reason_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_draft_reason_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_draft_reason_request::removal},
        utility::codec::field{"intent", &delete_draft_reason_request::intent},
    };
}

/**
 * @brief The members of delete_draft_reason_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_draft_reason_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_draft_reason_response::result},
    };
}

/**
 * @brief The members of delete_many_draft_reasons_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_draft_reasons_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_draft_reasons_request::removals},
        utility::codec::field{"intent", &delete_many_draft_reasons_request::intent},
    };
}

/**
 * @brief The members of delete_many_draft_reasons_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_draft_reasons_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_draft_reasons_response::result},
    };
}

/**
 * @brief The members of list_draft_reason_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_draft_reason_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_draft_reason_versions_request::key},
        utility::codec::field{"offset", &list_draft_reason_versions_request::offset},
        utility::codec::field{"limit", &list_draft_reason_versions_request::limit},
        utility::codec::field{"order", &list_draft_reason_versions_request::order},
        utility::codec::field{"filter", &list_draft_reason_versions_request::filter},
    };
}

/**
 * @brief The members of list_draft_reason_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_draft_reason_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_draft_reason_versions_response::result},
        utility::codec::field{"versions", &list_draft_reason_versions_response::versions},
        utility::codec::field{"total", &list_draft_reason_versions_response::total},
    };
}

/**
 * @brief The members of get_draft_reason_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_draft_reason_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_draft_reason_version_request::key},
    };
}

/**
 * @brief The members of get_draft_reason_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_draft_reason_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_draft_reason_version_response::result},
        utility::codec::field{"version", &get_draft_reason_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::draft_reason_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::draft_reason_key& v);
    static ores::trading::messaging::draft_reason_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::draft_reason_key& v);
    static ores::trading::messaging::draft_reason_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::draft_reason_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::draft_reason_write& v);
    static ores::trading::messaging::draft_reason_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::draft_reason_write& v);
    static ores::trading::messaging::draft_reason_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::draft_reason_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::draft_reason_change& v);
    static ores::trading::messaging::draft_reason_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::draft_reason_change& v);
    static ores::trading::messaging::draft_reason_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::draft_reason_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::draft_reason_removal& v);
    static ores::trading::messaging::draft_reason_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::draft_reason_removal& v);
    static ores::trading::messaging::draft_reason_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::draft_reason_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::draft_reason_lookup& v);
    static ores::trading::messaging::draft_reason_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::draft_reason_lookup& v);
    static ores::trading::messaging::draft_reason_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::draft_reasons_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::draft_reasons_filter& v);
    static ores::trading::messaging::draft_reasons_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::draft_reasons_filter& v);
    static ores::trading::messaging::draft_reasons_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::draft_reason_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::draft_reason_event& v);
    static ores::trading::messaging::draft_reason_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::draft_reason_event& v);
    static ores::trading::messaging::draft_reason_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::draft_reason_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::draft_reason_version_key& v);
    static ores::trading::messaging::draft_reason_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::draft_reason_version_key& v);
    static ores::trading::messaging::draft_reason_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::draft_reason_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::draft_reason_versions_filter& v);
    static ores::trading::messaging::draft_reason_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::draft_reason_versions_filter& v);
    static ores::trading::messaging::draft_reason_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_draft_reasons_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_draft_reasons_request& v);
    static ores::trading::messaging::list_draft_reasons_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::list_draft_reasons_request& v);
    static ores::trading::messaging::list_draft_reasons_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_draft_reasons_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_draft_reasons_response& v);
    static ores::trading::messaging::list_draft_reasons_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_draft_reasons_response& v);
    static ores::trading::messaging::list_draft_reasons_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_draft_reason_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_draft_reason_request& v);
    static ores::trading::messaging::get_draft_reason_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_draft_reason_request& v);
    static ores::trading::messaging::get_draft_reason_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_draft_reason_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_draft_reason_response& v);
    static ores::trading::messaging::get_draft_reason_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_draft_reason_response& v);
    static ores::trading::messaging::get_draft_reason_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_draft_reasons_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_draft_reasons_request& v);
    static ores::trading::messaging::get_many_draft_reasons_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_draft_reasons_request& v);
    static ores::trading::messaging::get_many_draft_reasons_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_draft_reasons_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_draft_reasons_response& v);
    static ores::trading::messaging::get_many_draft_reasons_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_draft_reasons_response& v);
    static ores::trading::messaging::get_many_draft_reasons_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_draft_reason_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_draft_reason_request& v);
    static ores::trading::messaging::put_draft_reason_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_draft_reason_request& v);
    static ores::trading::messaging::put_draft_reason_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_draft_reason_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_draft_reason_response& v);
    static ores::trading::messaging::put_draft_reason_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_draft_reason_response& v);
    static ores::trading::messaging::put_draft_reason_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_draft_reasons_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_draft_reasons_request& v);
    static ores::trading::messaging::put_many_draft_reasons_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_draft_reasons_request& v);
    static ores::trading::messaging::put_many_draft_reasons_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_draft_reasons_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_draft_reasons_response& v);
    static ores::trading::messaging::put_many_draft_reasons_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_draft_reasons_response& v);
    static ores::trading::messaging::put_many_draft_reasons_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_draft_reason_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_draft_reason_request& v);
    static ores::trading::messaging::delete_draft_reason_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_draft_reason_request& v);
    static ores::trading::messaging::delete_draft_reason_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_draft_reason_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_draft_reason_response& v);
    static ores::trading::messaging::delete_draft_reason_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_draft_reason_response& v);
    static ores::trading::messaging::delete_draft_reason_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_draft_reasons_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_draft_reasons_request& v);
    static ores::trading::messaging::delete_many_draft_reasons_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_draft_reasons_request& v);
    static ores::trading::messaging::delete_many_draft_reasons_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_draft_reasons_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_draft_reasons_response& v);
    static ores::trading::messaging::delete_many_draft_reasons_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_draft_reasons_response& v);
    static ores::trading::messaging::delete_many_draft_reasons_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_draft_reason_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_draft_reason_versions_request& v);
    static ores::trading::messaging::list_draft_reason_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_draft_reason_versions_request& v);
    static ores::trading::messaging::list_draft_reason_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_draft_reason_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_draft_reason_versions_response& v);
    static ores::trading::messaging::list_draft_reason_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_draft_reason_versions_response& v);
    static ores::trading::messaging::list_draft_reason_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_draft_reason_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_draft_reason_version_request& v);
    static ores::trading::messaging::get_draft_reason_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_draft_reason_version_request& v);
    static ores::trading::messaging::get_draft_reason_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_draft_reason_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_draft_reason_version_response& v);
    static ores::trading::messaging::get_draft_reason_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_draft_reason_version_response& v);
    static ores::trading::messaging::get_draft_reason_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
