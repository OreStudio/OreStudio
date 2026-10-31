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
#ifndef ORES_REFDATA_API_MESSAGING_TENOR_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_TENOR_PROTOCOL_HPP

#include "ores.refdata.api/domain/tenor.hpp"
#include "ores.refdata.api/domain/tenor_convention.hpp"
#include "ores.refdata.api/domain/tenor_convention_resolution.hpp"
#include "ores.refdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::refdata::messaging {

struct tenor_key {
    std::string code;
};

struct tenor_write {
    std::string code;
    std::string display_name;
    std::string description;
    int sort_order;
    std::string kind;
    std::string unit;
    std::optional<int> multiplier;
};

struct tenor_change {
    tenor_write write;
    ores::utility::domain::precondition precondition;
};

struct tenor_removal {
    tenor_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct tenor_lookup {
    tenor_key key;
    std::optional<ores::refdata::domain::tenor> tenor;
};

struct tenors_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct tenor_event {
    boost::uuids::uuid event_id;
    tenor_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct tenor_version_key {
    tenor_key tenor;
    std::uint32_t version;
};

struct tenor_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_tenors_request {
    using response_type = struct list_tenors_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenors.list";
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
    std::optional<tenors_filter> filter;
    std::optional<std::string> as_of;
};

struct list_tenors_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::tenor> tenors;
    std::uint64_t total;
};

struct get_tenor_request {
    using response_type = struct get_tenor_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenors.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_key key;
};

struct get_tenor_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::tenor> tenor;
};

struct get_many_tenors_request {
    using response_type = struct get_many_tenors_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenors.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tenor_key> keys;
};

struct get_many_tenors_response {
    ores::utility::domain::result result;
    std::vector<tenor_lookup> entries;
};

struct put_tenor_request {
    using response_type = struct put_tenor_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenors.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_change change;
    ores::utility::domain::change_intent intent;
};

struct put_tenor_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::tenor> tenor;
};

struct put_many_tenors_request {
    using response_type = struct put_many_tenors_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenors.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tenor_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_tenors_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::tenor> tenors;
};

struct delete_tenor_request {
    using response_type = struct delete_tenor_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenors.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_tenor_response {
    ores::utility::domain::result result;
};

struct delete_many_tenors_request {
    using response_type = struct delete_many_tenors_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenors.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tenor_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_tenors_response {
    ores::utility::domain::result result;
};

struct list_tenor_versions_request {
    using response_type = struct list_tenor_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenors_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<tenor_versions_filter> filter;
};

struct list_tenor_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::tenor> versions;
    std::uint64_t total;
};

struct get_tenor_version_request {
    using response_type = struct get_tenor_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenors_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_version_key key;
};

struct get_tenor_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::tenor> version;
};

struct resolve_tenor_request {
    using response_type = struct resolve_tenor_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.resolve_tenor";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string tenor_code;
    std::string convention_code;
    std::string calendar_code;
    std::chrono::year_month_day horizon;
    std::optional<std::chrono::year_month_day> spot;
};

struct resolve_tenor_response {
    ores::utility::domain::result result;
    std::chrono::year_month_day anchor_date;
    std::chrono::year_month_day date_before_roll;
    std::chrono::year_month_day adjusted_date;
    std::string roll_rule;
    std::string calendar;
};

struct preview_tenor_resolution_request {
    using response_type = struct resolve_tenor_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.preview_tenor_resolution";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ores::refdata::domain::tenor tenor;
    ores::refdata::domain::tenor_convention convention;
    ores::refdata::domain::tenor_convention_resolution resolution;
    std::string calendar_code;
    std::chrono::year_month_day horizon;
    std::optional<std::chrono::year_month_day> spot;
};

/**
 * @brief What one tenor of a convention resolved to.
 *
 * The same answer @c resolve_tenor_response gives for one tenor, named by the
 * tenor it is about. A refusal is in @c result and the dates counted before
 * it stay on the entry, so one tenor that cannot resolve costs only its own
 * row.
 */
struct convention_tenor_resolution {
    std::string tenor_code;
    ores::utility::domain::result result;
    std::chrono::year_month_day anchor_date;
    std::chrono::year_month_day date_before_roll;
    std::chrono::year_month_day adjusted_date;
};

struct resolve_convention_request {
    using response_type = struct resolve_convention_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.resolve_convention";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string convention_code;
    std::string calendar_code;
    std::chrono::year_month_day horizon;
    std::optional<std::chrono::year_month_day> spot;
};

struct resolve_convention_response {
    ores::utility::domain::result result;
    std::string roll_rule;
    std::string calendar;
    std::vector<convention_tenor_resolution> tenors;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace tenor_event_subjects {
inline constexpr std::string_view created = "refdata.v1.tenors_events.created";
inline constexpr std::string_view updated = "refdata.v1.tenors_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.tenors_events.deleted";
}
/**
 * @brief The members of tenor_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_key&) {
    return std::tuple{
        utility::codec::field{"code", &tenor_key::code},
    };
}

/**
 * @brief The members of tenor_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_write&) {
    return std::tuple{
        utility::codec::field{"code", &tenor_write::code},
        utility::codec::field{"display_name", &tenor_write::display_name},
        utility::codec::field{"description", &tenor_write::description},
        utility::codec::field{"sort_order", &tenor_write::sort_order},
        utility::codec::field{"kind", &tenor_write::kind},
        utility::codec::field{"unit", &tenor_write::unit},
        utility::codec::field{"multiplier", &tenor_write::multiplier},
    };
}

/**
 * @brief The members of tenor_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_change&) {
    return std::tuple{
        utility::codec::field{"write", &tenor_change::write},
        utility::codec::field{"precondition", &tenor_change::precondition},
    };
}

/**
 * @brief The members of tenor_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_removal&) {
    return std::tuple{
        utility::codec::field{"key", &tenor_removal::key},
        utility::codec::field{"precondition", &tenor_removal::precondition},
    };
}

/**
 * @brief The members of tenor_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &tenor_lookup::key},
        utility::codec::field{"tenor", &tenor_lookup::tenor},
    };
}

/**
 * @brief The members of tenors_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenors_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &tenors_filter::code_one_of},
    };
}

/**
 * @brief The members of tenor_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &tenor_event::event_id},
        utility::codec::field{"key", &tenor_event::key},
        utility::codec::field{"action", &tenor_event::action},
        utility::codec::field{"version", &tenor_event::version},
        utility::codec::field{"occurred_at", &tenor_event::occurred_at},
        utility::codec::field{"correlation_id", &tenor_event::correlation_id},
    };
}

/**
 * @brief The members of tenor_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_version_key&) {
    return std::tuple{
        utility::codec::field{"tenor", &tenor_version_key::tenor},
        utility::codec::field{"version", &tenor_version_key::version},
    };
}

/**
 * @brief The members of tenor_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &tenor_versions_filter::version},
        utility::codec::field{"from_version", &tenor_versions_filter::from_version},
        utility::codec::field{"to_version", &tenor_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_tenors_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_tenors_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_tenors_request::offset},
        utility::codec::field{"limit", &list_tenors_request::limit},
        utility::codec::field{"order", &list_tenors_request::order},
        utility::codec::field{"filter", &list_tenors_request::filter},
        utility::codec::field{"as_of", &list_tenors_request::as_of},
    };
}

/**
 * @brief The members of list_tenors_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_tenors_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_tenors_response::result},
        utility::codec::field{"tenors", &list_tenors_response::tenors},
        utility::codec::field{"total", &list_tenors_response::total},
    };
}

/**
 * @brief The members of get_tenor_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_tenor_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_tenor_request::key},
    };
}

/**
 * @brief The members of get_tenor_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_tenor_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_tenor_response::result},
        utility::codec::field{"tenor", &get_tenor_response::tenor},
    };
}

/**
 * @brief The members of get_many_tenors_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_tenors_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_tenors_request::keys},
    };
}

/**
 * @brief The members of get_many_tenors_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_tenors_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_tenors_response::result},
        utility::codec::field{"entries", &get_many_tenors_response::entries},
    };
}

/**
 * @brief The members of put_tenor_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_tenor_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_tenor_request::change},
        utility::codec::field{"intent", &put_tenor_request::intent},
    };
}

/**
 * @brief The members of put_tenor_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_tenor_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_tenor_response::result},
        utility::codec::field{"tenor", &put_tenor_response::tenor},
    };
}

/**
 * @brief The members of put_many_tenors_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_tenors_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_tenors_request::changes},
        utility::codec::field{"intent", &put_many_tenors_request::intent},
    };
}

/**
 * @brief The members of put_many_tenors_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_tenors_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_tenors_response::result},
        utility::codec::field{"tenors", &put_many_tenors_response::tenors},
    };
}

/**
 * @brief The members of delete_tenor_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_tenor_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_tenor_request::removal},
        utility::codec::field{"intent", &delete_tenor_request::intent},
    };
}

/**
 * @brief The members of delete_tenor_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_tenor_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_tenor_response::result},
    };
}

/**
 * @brief The members of delete_many_tenors_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_tenors_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_tenors_request::removals},
        utility::codec::field{"intent", &delete_many_tenors_request::intent},
    };
}

/**
 * @brief The members of delete_many_tenors_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_tenors_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_tenors_response::result},
    };
}

/**
 * @brief The members of list_tenor_versions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_tenor_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_tenor_versions_request::key},
        utility::codec::field{"offset", &list_tenor_versions_request::offset},
        utility::codec::field{"limit", &list_tenor_versions_request::limit},
        utility::codec::field{"order", &list_tenor_versions_request::order},
        utility::codec::field{"filter", &list_tenor_versions_request::filter},
    };
}

/**
 * @brief The members of list_tenor_versions_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_tenor_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_tenor_versions_response::result},
        utility::codec::field{"versions", &list_tenor_versions_response::versions},
        utility::codec::field{"total", &list_tenor_versions_response::total},
    };
}

/**
 * @brief The members of get_tenor_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_tenor_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_tenor_version_request::key},
    };
}

/**
 * @brief The members of get_tenor_version_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_tenor_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_tenor_version_response::result},
        utility::codec::field{"version", &get_tenor_version_response::version},
    };
}

/**
 * @brief The members of resolve_tenor_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const resolve_tenor_request&) {
    return std::tuple{
        utility::codec::field{"tenor_code", &resolve_tenor_request::tenor_code},
        utility::codec::field{"convention_code", &resolve_tenor_request::convention_code},
        utility::codec::field{"calendar_code", &resolve_tenor_request::calendar_code},
        utility::codec::field{"horizon", &resolve_tenor_request::horizon},
        utility::codec::field{"spot", &resolve_tenor_request::spot},
    };
}

/**
 * @brief The members of resolve_tenor_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const resolve_tenor_response&) {
    return std::tuple{
        utility::codec::field{"result", &resolve_tenor_response::result},
        utility::codec::field{"anchor_date", &resolve_tenor_response::anchor_date},
        utility::codec::field{"date_before_roll", &resolve_tenor_response::date_before_roll},
        utility::codec::field{"adjusted_date", &resolve_tenor_response::adjusted_date},
        utility::codec::field{"roll_rule", &resolve_tenor_response::roll_rule},
        utility::codec::field{"calendar", &resolve_tenor_response::calendar},
    };
}

/**
 * @brief The members of preview_tenor_resolution_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const preview_tenor_resolution_request&) {
    return std::tuple{
        utility::codec::field{"tenor", &preview_tenor_resolution_request::tenor},
        utility::codec::field{"convention", &preview_tenor_resolution_request::convention},
        utility::codec::field{"resolution", &preview_tenor_resolution_request::resolution},
        utility::codec::field{"calendar_code", &preview_tenor_resolution_request::calendar_code},
        utility::codec::field{"horizon", &preview_tenor_resolution_request::horizon},
        utility::codec::field{"spot", &preview_tenor_resolution_request::spot},
    };
}

/**
 * @brief The members of convention_tenor_resolution, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const convention_tenor_resolution&) {
    return std::tuple{
        utility::codec::field{"tenor_code", &convention_tenor_resolution::tenor_code},
        utility::codec::field{"result", &convention_tenor_resolution::result},
        utility::codec::field{"anchor_date", &convention_tenor_resolution::anchor_date},
        utility::codec::field{"date_before_roll", &convention_tenor_resolution::date_before_roll},
        utility::codec::field{"adjusted_date", &convention_tenor_resolution::adjusted_date},
    };
}

/**
 * @brief The members of resolve_convention_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const resolve_convention_request&) {
    return std::tuple{
        utility::codec::field{"convention_code", &resolve_convention_request::convention_code},
        utility::codec::field{"calendar_code", &resolve_convention_request::calendar_code},
        utility::codec::field{"horizon", &resolve_convention_request::horizon},
        utility::codec::field{"spot", &resolve_convention_request::spot},
    };
}

/**
 * @brief The members of resolve_convention_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const resolve_convention_response&) {
    return std::tuple{
        utility::codec::field{"result", &resolve_convention_response::result},
        utility::codec::field{"roll_rule", &resolve_convention_response::roll_rule},
        utility::codec::field{"calendar", &resolve_convention_response::calendar},
        utility::codec::field{"tenors", &resolve_convention_response::tenors},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::tenor_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::tenor_key& v);
    static ores::refdata::messaging::tenor_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::tenor_key& v);
    static ores::refdata::messaging::tenor_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::tenor_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::tenor_write& v);
    static ores::refdata::messaging::tenor_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::tenor_write& v);
    static ores::refdata::messaging::tenor_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::tenor_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::tenor_change& v);
    static ores::refdata::messaging::tenor_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::tenor_change& v);
    static ores::refdata::messaging::tenor_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::tenor_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::tenor_removal& v);
    static ores::refdata::messaging::tenor_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::tenor_removal& v);
    static ores::refdata::messaging::tenor_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::tenor_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::tenor_lookup& v);
    static ores::refdata::messaging::tenor_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::tenor_lookup& v);
    static ores::refdata::messaging::tenor_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::tenors_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::tenors_filter& v);
    static ores::refdata::messaging::tenors_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::tenors_filter& v);
    static ores::refdata::messaging::tenors_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::tenor_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::tenor_event& v);
    static ores::refdata::messaging::tenor_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::tenor_event& v);
    static ores::refdata::messaging::tenor_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::tenor_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::tenor_version_key& v);
    static ores::refdata::messaging::tenor_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::tenor_version_key& v);
    static ores::refdata::messaging::tenor_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::tenor_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::tenor_versions_filter& v);
    static ores::refdata::messaging::tenor_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::tenor_versions_filter& v);
    static ores::refdata::messaging::tenor_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_tenors_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_tenors_request& v);
    static ores::refdata::messaging::list_tenors_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::list_tenors_request& v);
    static ores::refdata::messaging::list_tenors_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_tenors_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_tenors_response& v);
    static ores::refdata::messaging::list_tenors_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::list_tenors_response& v);
    static ores::refdata::messaging::list_tenors_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_tenor_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_tenor_request& v);
    static ores::refdata::messaging::get_tenor_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_tenor_request& v);
    static ores::refdata::messaging::get_tenor_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_tenor_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_tenor_response& v);
    static ores::refdata::messaging::get_tenor_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_tenor_response& v);
    static ores::refdata::messaging::get_tenor_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_many_tenors_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_many_tenors_request& v);
    static ores::refdata::messaging::get_many_tenors_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_many_tenors_request& v);
    static ores::refdata::messaging::get_many_tenors_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_many_tenors_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_many_tenors_response& v);
    static ores::refdata::messaging::get_many_tenors_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_many_tenors_response& v);
    static ores::refdata::messaging::get_many_tenors_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_tenor_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_tenor_request& v);
    static ores::refdata::messaging::put_tenor_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_tenor_request& v);
    static ores::refdata::messaging::put_tenor_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_tenor_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_tenor_response& v);
    static ores::refdata::messaging::put_tenor_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_tenor_response& v);
    static ores::refdata::messaging::put_tenor_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_many_tenors_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_many_tenors_request& v);
    static ores::refdata::messaging::put_many_tenors_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_many_tenors_request& v);
    static ores::refdata::messaging::put_many_tenors_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_many_tenors_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_many_tenors_response& v);
    static ores::refdata::messaging::put_many_tenors_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_many_tenors_response& v);
    static ores::refdata::messaging::put_many_tenors_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_tenor_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_tenor_request& v);
    static ores::refdata::messaging::delete_tenor_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::delete_tenor_request& v);
    static ores::refdata::messaging::delete_tenor_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_tenor_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_tenor_response& v);
    static ores::refdata::messaging::delete_tenor_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::delete_tenor_response& v);
    static ores::refdata::messaging::delete_tenor_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_many_tenors_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_many_tenors_request& v);
    static ores::refdata::messaging::delete_many_tenors_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::delete_many_tenors_request& v);
    static ores::refdata::messaging::delete_many_tenors_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_many_tenors_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_many_tenors_response& v);
    static ores::refdata::messaging::delete_many_tenors_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_tenors_response& v);
    static ores::refdata::messaging::delete_many_tenors_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_tenor_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_tenor_versions_request& v);
    static ores::refdata::messaging::list_tenor_versions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_tenor_versions_request& v);
    static ores::refdata::messaging::list_tenor_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_tenor_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_tenor_versions_response& v);
    static ores::refdata::messaging::list_tenor_versions_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_tenor_versions_response& v);
    static ores::refdata::messaging::list_tenor_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_tenor_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_tenor_version_request& v);
    static ores::refdata::messaging::get_tenor_version_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_tenor_version_request& v);
    static ores::refdata::messaging::get_tenor_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_tenor_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_tenor_version_response& v);
    static ores::refdata::messaging::get_tenor_version_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_tenor_version_response& v);
    static ores::refdata::messaging::get_tenor_version_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::resolve_tenor_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::resolve_tenor_request& v);
    static ores::refdata::messaging::resolve_tenor_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::resolve_tenor_request& v);
    static ores::refdata::messaging::resolve_tenor_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::resolve_tenor_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::resolve_tenor_response& v);
    static ores::refdata::messaging::resolve_tenor_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::resolve_tenor_response& v);
    static ores::refdata::messaging::resolve_tenor_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::preview_tenor_resolution_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::preview_tenor_resolution_request& v);
    static ores::refdata::messaging::preview_tenor_resolution_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::preview_tenor_resolution_request& v);
    static ores::refdata::messaging::preview_tenor_resolution_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::convention_tenor_resolution> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::convention_tenor_resolution& v);
    static ores::refdata::messaging::convention_tenor_resolution read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::convention_tenor_resolution& v);
    static ores::refdata::messaging::convention_tenor_resolution
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::resolve_convention_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::resolve_convention_request& v);
    static ores::refdata::messaging::resolve_convention_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::resolve_convention_request& v);
    static ores::refdata::messaging::resolve_convention_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::resolve_convention_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::resolve_convention_response& v);
    static ores::refdata::messaging::resolve_convention_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::resolve_convention_response& v);
    static ores::refdata::messaging::resolve_convention_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
