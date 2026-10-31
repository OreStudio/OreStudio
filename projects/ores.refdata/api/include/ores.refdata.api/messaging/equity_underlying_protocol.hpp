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
#ifndef ORES_REFDATA_API_MESSAGING_EQUITY_UNDERLYING_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_EQUITY_UNDERLYING_PROTOCOL_HPP

#include "ores.refdata.api/domain/equity_underlying.hpp"
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

struct equity_underlying_key {
    std::string code;
};

struct equity_underlying_write {
    std::string code;
    std::string name;
    std::string description;
    std::string instrument_type;
    std::string resolves_to;
    std::string currency_code;
    int display_order;
};

struct equity_underlying_change {
    equity_underlying_write write;
    ores::utility::domain::precondition precondition;
};

struct equity_underlying_removal {
    equity_underlying_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct equity_underlying_lookup {
    equity_underlying_key key;
    std::optional<ores::refdata::domain::equity_underlying> equity_underlying;
};

struct equity_underlyings_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct equity_underlying_event {
    boost::uuids::uuid event_id;
    equity_underlying_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct equity_underlying_version_key {
    equity_underlying_key equity_underlying;
    std::uint32_t version;
};

struct equity_underlying_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_equity_underlyings_request {
    using response_type = struct list_equity_underlyings_response;
    static constexpr std::string_view nats_subject = "refdata.v1.equity_underlyings.list";
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
    std::optional<equity_underlyings_filter> filter;
    std::optional<std::string> as_of;
};

struct list_equity_underlyings_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::equity_underlying> equity_underlyings;
    std::uint64_t total;
};

struct get_equity_underlying_request {
    using response_type = struct get_equity_underlying_response;
    static constexpr std::string_view nats_subject = "refdata.v1.equity_underlyings.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    equity_underlying_key key;
};

struct get_equity_underlying_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::equity_underlying> equity_underlying;
};

struct get_many_equity_underlyings_request {
    using response_type = struct get_many_equity_underlyings_response;
    static constexpr std::string_view nats_subject = "refdata.v1.equity_underlyings.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<equity_underlying_key> keys;
};

struct get_many_equity_underlyings_response {
    ores::utility::domain::result result;
    std::vector<equity_underlying_lookup> entries;
};

struct put_equity_underlying_request {
    using response_type = struct put_equity_underlying_response;
    static constexpr std::string_view nats_subject = "refdata.v1.equity_underlyings.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    equity_underlying_change change;
    ores::utility::domain::change_intent intent;
};

struct put_equity_underlying_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::equity_underlying> equity_underlying;
};

struct put_many_equity_underlyings_request {
    using response_type = struct put_many_equity_underlyings_response;
    static constexpr std::string_view nats_subject = "refdata.v1.equity_underlyings.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<equity_underlying_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_equity_underlyings_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::equity_underlying> equity_underlyings;
};

struct delete_equity_underlying_request {
    using response_type = struct delete_equity_underlying_response;
    static constexpr std::string_view nats_subject = "refdata.v1.equity_underlyings.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    equity_underlying_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_equity_underlying_response {
    ores::utility::domain::result result;
};

struct delete_many_equity_underlyings_request {
    using response_type = struct delete_many_equity_underlyings_response;
    static constexpr std::string_view nats_subject = "refdata.v1.equity_underlyings.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<equity_underlying_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_equity_underlyings_response {
    ores::utility::domain::result result;
};

struct list_equity_underlying_versions_request {
    using response_type = struct list_equity_underlying_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.equity_underlyings_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    equity_underlying_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<equity_underlying_versions_filter> filter;
};

struct list_equity_underlying_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::equity_underlying> versions;
    std::uint64_t total;
};

struct get_equity_underlying_version_request {
    using response_type = struct get_equity_underlying_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.equity_underlyings_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    equity_underlying_version_key key;
};

struct get_equity_underlying_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::equity_underlying> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace equity_underlying_event_subjects {
inline constexpr std::string_view created = "refdata.v1.equity_underlyings_events.created";
inline constexpr std::string_view updated = "refdata.v1.equity_underlyings_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.equity_underlyings_events.deleted";
}
/**
 * @brief The members of equity_underlying_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const equity_underlying_key&) {
    return std::tuple{
        utility::codec::field{"code", &equity_underlying_key::code},
    };
}

/**
 * @brief The members of equity_underlying_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const equity_underlying_write&) {
    return std::tuple{
        utility::codec::field{"code", &equity_underlying_write::code},
        utility::codec::field{"name", &equity_underlying_write::name},
        utility::codec::field{"description", &equity_underlying_write::description},
        utility::codec::field{"instrument_type", &equity_underlying_write::instrument_type},
        utility::codec::field{"resolves_to", &equity_underlying_write::resolves_to},
        utility::codec::field{"currency_code", &equity_underlying_write::currency_code},
        utility::codec::field{"display_order", &equity_underlying_write::display_order},
    };
}

/**
 * @brief The members of equity_underlying_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const equity_underlying_change&) {
    return std::tuple{
        utility::codec::field{"write", &equity_underlying_change::write},
        utility::codec::field{"precondition", &equity_underlying_change::precondition},
    };
}

/**
 * @brief The members of equity_underlying_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const equity_underlying_removal&) {
    return std::tuple{
        utility::codec::field{"key", &equity_underlying_removal::key},
        utility::codec::field{"precondition", &equity_underlying_removal::precondition},
    };
}

/**
 * @brief The members of equity_underlying_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const equity_underlying_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &equity_underlying_lookup::key},
        utility::codec::field{"equity_underlying", &equity_underlying_lookup::equity_underlying},
    };
}

/**
 * @brief The members of equity_underlyings_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const equity_underlyings_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &equity_underlyings_filter::code_one_of},
    };
}

/**
 * @brief The members of equity_underlying_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const equity_underlying_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &equity_underlying_event::event_id},
        utility::codec::field{"key", &equity_underlying_event::key},
        utility::codec::field{"action", &equity_underlying_event::action},
        utility::codec::field{"version", &equity_underlying_event::version},
        utility::codec::field{"occurred_at", &equity_underlying_event::occurred_at},
        utility::codec::field{"correlation_id", &equity_underlying_event::correlation_id},
    };
}

/**
 * @brief The members of equity_underlying_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const equity_underlying_version_key&) {
    return std::tuple{
        utility::codec::field{"equity_underlying",
                              &equity_underlying_version_key::equity_underlying},
        utility::codec::field{"version", &equity_underlying_version_key::version},
    };
}

/**
 * @brief The members of equity_underlying_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const equity_underlying_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &equity_underlying_versions_filter::version},
        utility::codec::field{"from_version", &equity_underlying_versions_filter::from_version},
        utility::codec::field{"to_version", &equity_underlying_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_equity_underlyings_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_equity_underlyings_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_equity_underlyings_request::offset},
        utility::codec::field{"limit", &list_equity_underlyings_request::limit},
        utility::codec::field{"order", &list_equity_underlyings_request::order},
        utility::codec::field{"filter", &list_equity_underlyings_request::filter},
        utility::codec::field{"as_of", &list_equity_underlyings_request::as_of},
    };
}

/**
 * @brief The members of list_equity_underlyings_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_equity_underlyings_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_equity_underlyings_response::result},
        utility::codec::field{"equity_underlyings",
                              &list_equity_underlyings_response::equity_underlyings},
        utility::codec::field{"total", &list_equity_underlyings_response::total},
    };
}

/**
 * @brief The members of get_equity_underlying_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_equity_underlying_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_equity_underlying_request::key},
    };
}

/**
 * @brief The members of get_equity_underlying_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_equity_underlying_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_equity_underlying_response::result},
        utility::codec::field{"equity_underlying",
                              &get_equity_underlying_response::equity_underlying},
    };
}

/**
 * @brief The members of get_many_equity_underlyings_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_equity_underlyings_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_equity_underlyings_request::keys},
    };
}

/**
 * @brief The members of get_many_equity_underlyings_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_equity_underlyings_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_equity_underlyings_response::result},
        utility::codec::field{"entries", &get_many_equity_underlyings_response::entries},
    };
}

/**
 * @brief The members of put_equity_underlying_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_equity_underlying_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_equity_underlying_request::change},
        utility::codec::field{"intent", &put_equity_underlying_request::intent},
    };
}

/**
 * @brief The members of put_equity_underlying_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_equity_underlying_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_equity_underlying_response::result},
        utility::codec::field{"equity_underlying",
                              &put_equity_underlying_response::equity_underlying},
    };
}

/**
 * @brief The members of put_many_equity_underlyings_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_equity_underlyings_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_equity_underlyings_request::changes},
        utility::codec::field{"intent", &put_many_equity_underlyings_request::intent},
    };
}

/**
 * @brief The members of put_many_equity_underlyings_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_equity_underlyings_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_equity_underlyings_response::result},
        utility::codec::field{"equity_underlyings",
                              &put_many_equity_underlyings_response::equity_underlyings},
    };
}

/**
 * @brief The members of delete_equity_underlying_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_equity_underlying_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_equity_underlying_request::removal},
        utility::codec::field{"intent", &delete_equity_underlying_request::intent},
    };
}

/**
 * @brief The members of delete_equity_underlying_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_equity_underlying_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_equity_underlying_response::result},
    };
}

/**
 * @brief The members of delete_many_equity_underlyings_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_equity_underlyings_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_equity_underlyings_request::removals},
        utility::codec::field{"intent", &delete_many_equity_underlyings_request::intent},
    };
}

/**
 * @brief The members of delete_many_equity_underlyings_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_equity_underlyings_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_equity_underlyings_response::result},
    };
}

/**
 * @brief The members of list_equity_underlying_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_equity_underlying_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_equity_underlying_versions_request::key},
        utility::codec::field{"offset", &list_equity_underlying_versions_request::offset},
        utility::codec::field{"limit", &list_equity_underlying_versions_request::limit},
        utility::codec::field{"order", &list_equity_underlying_versions_request::order},
        utility::codec::field{"filter", &list_equity_underlying_versions_request::filter},
    };
}

/**
 * @brief The members of list_equity_underlying_versions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_equity_underlying_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_equity_underlying_versions_response::result},
        utility::codec::field{"versions", &list_equity_underlying_versions_response::versions},
        utility::codec::field{"total", &list_equity_underlying_versions_response::total},
    };
}

/**
 * @brief The members of get_equity_underlying_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_equity_underlying_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_equity_underlying_version_request::key},
    };
}

/**
 * @brief The members of get_equity_underlying_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_equity_underlying_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_equity_underlying_version_response::result},
        utility::codec::field{"version", &get_equity_underlying_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::equity_underlying_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::equity_underlying_key& v);
    static ores::refdata::messaging::equity_underlying_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::equity_underlying_key& v);
    static ores::refdata::messaging::equity_underlying_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::equity_underlying_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::equity_underlying_write& v);
    static ores::refdata::messaging::equity_underlying_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::equity_underlying_write& v);
    static ores::refdata::messaging::equity_underlying_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::equity_underlying_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::equity_underlying_change& v);
    static ores::refdata::messaging::equity_underlying_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::equity_underlying_change& v);
    static ores::refdata::messaging::equity_underlying_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::equity_underlying_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::equity_underlying_removal& v);
    static ores::refdata::messaging::equity_underlying_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::equity_underlying_removal& v);
    static ores::refdata::messaging::equity_underlying_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::equity_underlying_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::equity_underlying_lookup& v);
    static ores::refdata::messaging::equity_underlying_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::equity_underlying_lookup& v);
    static ores::refdata::messaging::equity_underlying_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::equity_underlyings_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::equity_underlyings_filter& v);
    static ores::refdata::messaging::equity_underlyings_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::equity_underlyings_filter& v);
    static ores::refdata::messaging::equity_underlyings_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::equity_underlying_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::equity_underlying_event& v);
    static ores::refdata::messaging::equity_underlying_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::equity_underlying_event& v);
    static ores::refdata::messaging::equity_underlying_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::equity_underlying_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::equity_underlying_version_key& v);
    static ores::refdata::messaging::equity_underlying_version_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::equity_underlying_version_key& v);
    static ores::refdata::messaging::equity_underlying_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::equity_underlying_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::equity_underlying_versions_filter& v);
    static ores::refdata::messaging::equity_underlying_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::equity_underlying_versions_filter& v);
    static ores::refdata::messaging::equity_underlying_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_equity_underlyings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_equity_underlyings_request& v);
    static ores::refdata::messaging::list_equity_underlyings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_equity_underlyings_request& v);
    static ores::refdata::messaging::list_equity_underlyings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_equity_underlyings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_equity_underlyings_response& v);
    static ores::refdata::messaging::list_equity_underlyings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_equity_underlyings_response& v);
    static ores::refdata::messaging::list_equity_underlyings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_equity_underlying_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_equity_underlying_request& v);
    static ores::refdata::messaging::get_equity_underlying_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_equity_underlying_request& v);
    static ores::refdata::messaging::get_equity_underlying_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_equity_underlying_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_equity_underlying_response& v);
    static ores::refdata::messaging::get_equity_underlying_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_equity_underlying_response& v);
    static ores::refdata::messaging::get_equity_underlying_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_equity_underlyings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_equity_underlyings_request& v);
    static ores::refdata::messaging::get_many_equity_underlyings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_equity_underlyings_request& v);
    static ores::refdata::messaging::get_many_equity_underlyings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_equity_underlyings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_equity_underlyings_response& v);
    static ores::refdata::messaging::get_many_equity_underlyings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_equity_underlyings_response& v);
    static ores::refdata::messaging::get_many_equity_underlyings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_equity_underlying_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_equity_underlying_request& v);
    static ores::refdata::messaging::put_equity_underlying_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_equity_underlying_request& v);
    static ores::refdata::messaging::put_equity_underlying_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_equity_underlying_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_equity_underlying_response& v);
    static ores::refdata::messaging::put_equity_underlying_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_equity_underlying_response& v);
    static ores::refdata::messaging::put_equity_underlying_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_equity_underlyings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_equity_underlyings_request& v);
    static ores::refdata::messaging::put_many_equity_underlyings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_equity_underlyings_request& v);
    static ores::refdata::messaging::put_many_equity_underlyings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_equity_underlyings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_equity_underlyings_response& v);
    static ores::refdata::messaging::put_many_equity_underlyings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_equity_underlyings_response& v);
    static ores::refdata::messaging::put_many_equity_underlyings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_equity_underlying_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_equity_underlying_request& v);
    static ores::refdata::messaging::delete_equity_underlying_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_equity_underlying_request& v);
    static ores::refdata::messaging::delete_equity_underlying_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_equity_underlying_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_equity_underlying_response& v);
    static ores::refdata::messaging::delete_equity_underlying_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_equity_underlying_response& v);
    static ores::refdata::messaging::delete_equity_underlying_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_equity_underlyings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_equity_underlyings_request& v);
    static ores::refdata::messaging::delete_many_equity_underlyings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_equity_underlyings_request& v);
    static ores::refdata::messaging::delete_many_equity_underlyings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_equity_underlyings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_equity_underlyings_response& v);
    static ores::refdata::messaging::delete_many_equity_underlyings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_equity_underlyings_response& v);
    static ores::refdata::messaging::delete_many_equity_underlyings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_equity_underlying_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_equity_underlying_versions_request& v);
    static ores::refdata::messaging::list_equity_underlying_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_equity_underlying_versions_request& v);
    static ores::refdata::messaging::list_equity_underlying_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_equity_underlying_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_equity_underlying_versions_response& v);
    static ores::refdata::messaging::list_equity_underlying_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_equity_underlying_versions_response& v);
    static ores::refdata::messaging::list_equity_underlying_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_equity_underlying_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_equity_underlying_version_request& v);
    static ores::refdata::messaging::get_equity_underlying_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_equity_underlying_version_request& v);
    static ores::refdata::messaging::get_equity_underlying_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_equity_underlying_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_equity_underlying_version_response& v);
    static ores::refdata::messaging::get_equity_underlying_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_equity_underlying_version_response& v);
    static ores::refdata::messaging::get_equity_underlying_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
