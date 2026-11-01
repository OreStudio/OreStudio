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
#ifndef ORES_TRADING_API_MESSAGING_INSTRUMENT_SCHEDULE_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_INSTRUMENT_SCHEDULE_PROTOCOL_HPP

#include "ores.trading.api/domain/instrument_schedule.hpp"
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

struct instrument_schedule_key {
    boost::uuids::uuid trade_id;
    std::string owner_role;
    int owner_number;
    std::string schedule_role;
    int sequence_number;
};

struct instrument_schedule_write {
    boost::uuids::uuid trade_id;
    std::string owner_role;
    int owner_number;
    std::string schedule_role;
    int sequence_number;
    boost::uuids::uuid trade_activity_id;
    std::string schedule_kind;
    std::optional<std::chrono::year_month_day> start_date;
    std::optional<std::chrono::year_month_day> end_date;
    std::optional<std::string> adjust_end_date_to_previous_month_end;
    std::optional<std::string> tenor;
    std::optional<std::string> calendar;
    std::optional<std::string> convention;
    std::optional<std::string> term_convention;
    std::optional<std::string> rule;
    std::optional<std::string> end_of_month;
    std::optional<std::string> end_of_month_convention;
    std::optional<std::chrono::year_month_day> first_date;
    std::optional<std::chrono::year_month_day> last_date;
    std::optional<bool> remove_first_date;
    std::optional<bool> remove_last_date;
    std::optional<std::string> include_duplicate_dates;
};

struct instrument_schedule_change {
    instrument_schedule_write write;
    ores::utility::domain::precondition precondition;
};

struct instrument_schedule_removal {
    instrument_schedule_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct instrument_schedule_lookup {
    instrument_schedule_key key;
    std::optional<ores::trading::domain::instrument_schedule> instrument_schedule;
};

struct instrument_schedules_filter {
    std::optional<boost::uuids::uuid> trade_id;
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;
};

struct instrument_schedule_event {
    boost::uuids::uuid event_id;
    instrument_schedule_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct instrument_schedule_version_key {
    instrument_schedule_key instrument_schedule;
    std::uint32_t version;
};

struct instrument_schedule_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_instrument_schedules_request {
    using response_type = struct list_instrument_schedules_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_schedules.list";
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
    std::optional<instrument_schedules_filter> filter;
    std::optional<std::string> as_of;
};

struct list_instrument_schedules_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_schedule> instrument_schedules;
    std::uint64_t total;
};

struct get_instrument_schedule_request {
    using response_type = struct get_instrument_schedule_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_schedules.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_schedule_key key;
};

struct get_instrument_schedule_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::instrument_schedule> instrument_schedule;
};

struct get_many_instrument_schedules_request {
    using response_type = struct get_many_instrument_schedules_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_schedules.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<instrument_schedule_key> keys;
};

struct get_many_instrument_schedules_response {
    ores::utility::domain::result result;
    std::vector<instrument_schedule_lookup> entries;
};

struct put_instrument_schedule_request {
    using response_type = struct put_instrument_schedule_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_schedules.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_schedule_change change;
    ores::utility::domain::change_intent intent;
};

struct put_instrument_schedule_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::instrument_schedule> instrument_schedule;
};

struct put_many_instrument_schedules_request {
    using response_type = struct put_many_instrument_schedules_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_schedules.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<instrument_schedule_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_instrument_schedules_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_schedule> instrument_schedules;
};

struct delete_instrument_schedule_request {
    using response_type = struct delete_instrument_schedule_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_schedules.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_schedule_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_instrument_schedule_response {
    ores::utility::domain::result result;
};

struct delete_many_instrument_schedules_request {
    using response_type = struct delete_many_instrument_schedules_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_schedules.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<instrument_schedule_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_instrument_schedules_response {
    ores::utility::domain::result result;
};

struct list_by_trade_id_instrument_schedules_request {
    using response_type = struct list_by_trade_id_instrument_schedules_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.instrument_schedules.list_by_trade_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid trade_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<instrument_schedules_filter> filter;
};

struct list_by_trade_id_instrument_schedules_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_schedule> instrument_schedules;
    std::uint64_t total;
};

struct list_instrument_schedule_versions_request {
    using response_type = struct list_instrument_schedule_versions_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.instrument_schedules_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_schedule_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<instrument_schedule_versions_filter> filter;
};

struct list_instrument_schedule_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_schedule> versions;
    std::uint64_t total;
};

struct get_instrument_schedule_version_request {
    using response_type = struct get_instrument_schedule_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_schedules_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_schedule_version_key key;
};

struct get_instrument_schedule_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::instrument_schedule> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace instrument_schedule_event_subjects {
inline constexpr std::string_view created = "trading.v1.instrument_schedules_events.created";
inline constexpr std::string_view updated = "trading.v1.instrument_schedules_events.updated";
inline constexpr std::string_view deleted = "trading.v1.instrument_schedules_events.deleted";
}
/**
 * @brief The members of instrument_schedule_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_schedule_key&) {
    return std::tuple{
        utility::codec::field{"trade_id", &instrument_schedule_key::trade_id},
        utility::codec::field{"owner_role", &instrument_schedule_key::owner_role},
        utility::codec::field{"owner_number", &instrument_schedule_key::owner_number},
        utility::codec::field{"schedule_role", &instrument_schedule_key::schedule_role},
        utility::codec::field{"sequence_number", &instrument_schedule_key::sequence_number},
    };
}

/**
 * @brief The members of instrument_schedule_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_schedule_write&) {
    return std::tuple{
        utility::codec::field{"trade_id", &instrument_schedule_write::trade_id},
        utility::codec::field{"owner_role", &instrument_schedule_write::owner_role},
        utility::codec::field{"owner_number", &instrument_schedule_write::owner_number},
        utility::codec::field{"schedule_role", &instrument_schedule_write::schedule_role},
        utility::codec::field{"sequence_number", &instrument_schedule_write::sequence_number},
        utility::codec::field{"trade_activity_id", &instrument_schedule_write::trade_activity_id},
        utility::codec::field{"schedule_kind", &instrument_schedule_write::schedule_kind},
        utility::codec::field{"start_date", &instrument_schedule_write::start_date},
        utility::codec::field{"end_date", &instrument_schedule_write::end_date},
        utility::codec::field{"adjust_end_date_to_previous_month_end",
                              &instrument_schedule_write::adjust_end_date_to_previous_month_end},
        utility::codec::field{"tenor", &instrument_schedule_write::tenor},
        utility::codec::field{"calendar", &instrument_schedule_write::calendar},
        utility::codec::field{"convention", &instrument_schedule_write::convention},
        utility::codec::field{"term_convention", &instrument_schedule_write::term_convention},
        utility::codec::field{"rule", &instrument_schedule_write::rule},
        utility::codec::field{"end_of_month", &instrument_schedule_write::end_of_month},
        utility::codec::field{"end_of_month_convention",
                              &instrument_schedule_write::end_of_month_convention},
        utility::codec::field{"first_date", &instrument_schedule_write::first_date},
        utility::codec::field{"last_date", &instrument_schedule_write::last_date},
        utility::codec::field{"remove_first_date", &instrument_schedule_write::remove_first_date},
        utility::codec::field{"remove_last_date", &instrument_schedule_write::remove_last_date},
        utility::codec::field{"include_duplicate_dates",
                              &instrument_schedule_write::include_duplicate_dates},
    };
}

/**
 * @brief The members of instrument_schedule_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_schedule_change&) {
    return std::tuple{
        utility::codec::field{"write", &instrument_schedule_change::write},
        utility::codec::field{"precondition", &instrument_schedule_change::precondition},
    };
}

/**
 * @brief The members of instrument_schedule_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_schedule_removal&) {
    return std::tuple{
        utility::codec::field{"key", &instrument_schedule_removal::key},
        utility::codec::field{"precondition", &instrument_schedule_removal::precondition},
    };
}

/**
 * @brief The members of instrument_schedule_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_schedule_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &instrument_schedule_lookup::key},
        utility::codec::field{"instrument_schedule",
                              &instrument_schedule_lookup::instrument_schedule},
    };
}

/**
 * @brief The members of instrument_schedules_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_schedules_filter&) {
    return std::tuple{
        utility::codec::field{"trade_id", &instrument_schedules_filter::trade_id},
        utility::codec::field{"trade_id_one_of", &instrument_schedules_filter::trade_id_one_of},
    };
}

/**
 * @brief The members of instrument_schedule_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_schedule_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &instrument_schedule_event::event_id},
        utility::codec::field{"key", &instrument_schedule_event::key},
        utility::codec::field{"action", &instrument_schedule_event::action},
        utility::codec::field{"version", &instrument_schedule_event::version},
        utility::codec::field{"occurred_at", &instrument_schedule_event::occurred_at},
        utility::codec::field{"correlation_id", &instrument_schedule_event::correlation_id},
    };
}

/**
 * @brief The members of instrument_schedule_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_schedule_version_key&) {
    return std::tuple{
        utility::codec::field{"instrument_schedule",
                              &instrument_schedule_version_key::instrument_schedule},
        utility::codec::field{"version", &instrument_schedule_version_key::version},
    };
}

/**
 * @brief The members of instrument_schedule_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_schedule_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &instrument_schedule_versions_filter::version},
        utility::codec::field{"from_version", &instrument_schedule_versions_filter::from_version},
        utility::codec::field{"to_version", &instrument_schedule_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_instrument_schedules_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_instrument_schedules_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_instrument_schedules_request::offset},
        utility::codec::field{"limit", &list_instrument_schedules_request::limit},
        utility::codec::field{"order", &list_instrument_schedules_request::order},
        utility::codec::field{"filter", &list_instrument_schedules_request::filter},
        utility::codec::field{"as_of", &list_instrument_schedules_request::as_of},
    };
}

/**
 * @brief The members of list_instrument_schedules_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_instrument_schedules_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_instrument_schedules_response::result},
        utility::codec::field{"instrument_schedules",
                              &list_instrument_schedules_response::instrument_schedules},
        utility::codec::field{"total", &list_instrument_schedules_response::total},
    };
}

/**
 * @brief The members of get_instrument_schedule_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_instrument_schedule_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_instrument_schedule_request::key},
    };
}

/**
 * @brief The members of get_instrument_schedule_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_instrument_schedule_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_instrument_schedule_response::result},
        utility::codec::field{"instrument_schedule",
                              &get_instrument_schedule_response::instrument_schedule},
    };
}

/**
 * @brief The members of get_many_instrument_schedules_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_instrument_schedules_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_instrument_schedules_request::keys},
    };
}

/**
 * @brief The members of get_many_instrument_schedules_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_instrument_schedules_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_instrument_schedules_response::result},
        utility::codec::field{"entries", &get_many_instrument_schedules_response::entries},
    };
}

/**
 * @brief The members of put_instrument_schedule_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_instrument_schedule_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_instrument_schedule_request::change},
        utility::codec::field{"intent", &put_instrument_schedule_request::intent},
    };
}

/**
 * @brief The members of put_instrument_schedule_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_instrument_schedule_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_instrument_schedule_response::result},
        utility::codec::field{"instrument_schedule",
                              &put_instrument_schedule_response::instrument_schedule},
    };
}

/**
 * @brief The members of put_many_instrument_schedules_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_instrument_schedules_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_instrument_schedules_request::changes},
        utility::codec::field{"intent", &put_many_instrument_schedules_request::intent},
    };
}

/**
 * @brief The members of put_many_instrument_schedules_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_instrument_schedules_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_instrument_schedules_response::result},
        utility::codec::field{"instrument_schedules",
                              &put_many_instrument_schedules_response::instrument_schedules},
    };
}

/**
 * @brief The members of delete_instrument_schedule_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_instrument_schedule_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_instrument_schedule_request::removal},
        utility::codec::field{"intent", &delete_instrument_schedule_request::intent},
    };
}

/**
 * @brief The members of delete_instrument_schedule_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_instrument_schedule_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_instrument_schedule_response::result},
    };
}

/**
 * @brief The members of delete_many_instrument_schedules_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_instrument_schedules_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_instrument_schedules_request::removals},
        utility::codec::field{"intent", &delete_many_instrument_schedules_request::intent},
    };
}

/**
 * @brief The members of delete_many_instrument_schedules_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_instrument_schedules_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_instrument_schedules_response::result},
    };
}

/**
 * @brief The members of list_by_trade_id_instrument_schedules_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_trade_id_instrument_schedules_request&) {
    return std::tuple{
        utility::codec::field{"trade_id", &list_by_trade_id_instrument_schedules_request::trade_id},
        utility::codec::field{"scope", &list_by_trade_id_instrument_schedules_request::scope},
        utility::codec::field{"offset", &list_by_trade_id_instrument_schedules_request::offset},
        utility::codec::field{"limit", &list_by_trade_id_instrument_schedules_request::limit},
        utility::codec::field{"order", &list_by_trade_id_instrument_schedules_request::order},
        utility::codec::field{"filter", &list_by_trade_id_instrument_schedules_request::filter},
    };
}

/**
 * @brief The members of list_by_trade_id_instrument_schedules_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_trade_id_instrument_schedules_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_by_trade_id_instrument_schedules_response::result},
        utility::codec::field{
            "instrument_schedules",
            &list_by_trade_id_instrument_schedules_response::instrument_schedules},
        utility::codec::field{"total", &list_by_trade_id_instrument_schedules_response::total},
    };
}

/**
 * @brief The members of list_instrument_schedule_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_instrument_schedule_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_instrument_schedule_versions_request::key},
        utility::codec::field{"offset", &list_instrument_schedule_versions_request::offset},
        utility::codec::field{"limit", &list_instrument_schedule_versions_request::limit},
        utility::codec::field{"order", &list_instrument_schedule_versions_request::order},
        utility::codec::field{"filter", &list_instrument_schedule_versions_request::filter},
    };
}

/**
 * @brief The members of list_instrument_schedule_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_instrument_schedule_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_instrument_schedule_versions_response::result},
        utility::codec::field{"versions", &list_instrument_schedule_versions_response::versions},
        utility::codec::field{"total", &list_instrument_schedule_versions_response::total},
    };
}

/**
 * @brief The members of get_instrument_schedule_version_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_instrument_schedule_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_instrument_schedule_version_request::key},
    };
}

/**
 * @brief The members of get_instrument_schedule_version_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_instrument_schedule_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_instrument_schedule_version_response::result},
        utility::codec::field{"version", &get_instrument_schedule_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_schedule_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_schedule_key& v);
    static ores::trading::messaging::instrument_schedule_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::instrument_schedule_key& v);
    static ores::trading::messaging::instrument_schedule_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_schedule_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_schedule_write& v);
    static ores::trading::messaging::instrument_schedule_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::instrument_schedule_write& v);
    static ores::trading::messaging::instrument_schedule_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_schedule_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_schedule_change& v);
    static ores::trading::messaging::instrument_schedule_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::instrument_schedule_change& v);
    static ores::trading::messaging::instrument_schedule_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_schedule_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_schedule_removal& v);
    static ores::trading::messaging::instrument_schedule_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_schedule_removal& v);
    static ores::trading::messaging::instrument_schedule_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_schedule_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_schedule_lookup& v);
    static ores::trading::messaging::instrument_schedule_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::instrument_schedule_lookup& v);
    static ores::trading::messaging::instrument_schedule_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_schedules_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_schedules_filter& v);
    static ores::trading::messaging::instrument_schedules_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_schedules_filter& v);
    static ores::trading::messaging::instrument_schedules_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_schedule_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_schedule_event& v);
    static ores::trading::messaging::instrument_schedule_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::instrument_schedule_event& v);
    static ores::trading::messaging::instrument_schedule_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_schedule_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::instrument_schedule_version_key& v);
    static ores::trading::messaging::instrument_schedule_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_schedule_version_key& v);
    static ores::trading::messaging::instrument_schedule_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_schedule_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::instrument_schedule_versions_filter& v);
    static ores::trading::messaging::instrument_schedule_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_schedule_versions_filter& v);
    static ores::trading::messaging::instrument_schedule_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_instrument_schedules_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_instrument_schedules_request& v);
    static ores::trading::messaging::list_instrument_schedules_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_instrument_schedules_request& v);
    static ores::trading::messaging::list_instrument_schedules_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_instrument_schedules_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_instrument_schedules_response& v);
    static ores::trading::messaging::list_instrument_schedules_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_instrument_schedules_response& v);
    static ores::trading::messaging::list_instrument_schedules_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_instrument_schedule_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_instrument_schedule_request& v);
    static ores::trading::messaging::get_instrument_schedule_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_instrument_schedule_request& v);
    static ores::trading::messaging::get_instrument_schedule_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_instrument_schedule_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_instrument_schedule_response& v);
    static ores::trading::messaging::get_instrument_schedule_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_instrument_schedule_response& v);
    static ores::trading::messaging::get_instrument_schedule_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_instrument_schedules_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_instrument_schedules_request& v);
    static ores::trading::messaging::get_many_instrument_schedules_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_instrument_schedules_request& v);
    static ores::trading::messaging::get_many_instrument_schedules_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_instrument_schedules_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_instrument_schedules_response& v);
    static ores::trading::messaging::get_many_instrument_schedules_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_instrument_schedules_response& v);
    static ores::trading::messaging::get_many_instrument_schedules_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_instrument_schedule_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_instrument_schedule_request& v);
    static ores::trading::messaging::put_instrument_schedule_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_instrument_schedule_request& v);
    static ores::trading::messaging::put_instrument_schedule_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_instrument_schedule_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_instrument_schedule_response& v);
    static ores::trading::messaging::put_instrument_schedule_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_instrument_schedule_response& v);
    static ores::trading::messaging::put_instrument_schedule_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_instrument_schedules_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_instrument_schedules_request& v);
    static ores::trading::messaging::put_many_instrument_schedules_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_instrument_schedules_request& v);
    static ores::trading::messaging::put_many_instrument_schedules_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_instrument_schedules_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_instrument_schedules_response& v);
    static ores::trading::messaging::put_many_instrument_schedules_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_instrument_schedules_response& v);
    static ores::trading::messaging::put_many_instrument_schedules_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_instrument_schedule_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_instrument_schedule_request& v);
    static ores::trading::messaging::delete_instrument_schedule_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_instrument_schedule_request& v);
    static ores::trading::messaging::delete_instrument_schedule_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_instrument_schedule_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_instrument_schedule_response& v);
    static ores::trading::messaging::delete_instrument_schedule_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_instrument_schedule_response& v);
    static ores::trading::messaging::delete_instrument_schedule_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_instrument_schedules_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_instrument_schedules_request& v);
    static ores::trading::messaging::delete_many_instrument_schedules_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_instrument_schedules_request& v);
    static ores::trading::messaging::delete_many_instrument_schedules_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_instrument_schedules_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_instrument_schedules_response& v);
    static ores::trading::messaging::delete_many_instrument_schedules_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_instrument_schedules_response& v);
    static ores::trading::messaging::delete_many_instrument_schedules_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_instrument_schedules_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_by_trade_id_instrument_schedules_request& v);
    static ores::trading::messaging::list_by_trade_id_instrument_schedules_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_by_trade_id_instrument_schedules_request& v);
    static ores::trading::messaging::list_by_trade_id_instrument_schedules_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_instrument_schedules_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_by_trade_id_instrument_schedules_response& v);
    static ores::trading::messaging::list_by_trade_id_instrument_schedules_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_by_trade_id_instrument_schedules_response& v);
    static ores::trading::messaging::list_by_trade_id_instrument_schedules_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_instrument_schedule_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_instrument_schedule_versions_request& v);
    static ores::trading::messaging::list_instrument_schedule_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_instrument_schedule_versions_request& v);
    static ores::trading::messaging::list_instrument_schedule_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_instrument_schedule_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_instrument_schedule_versions_response& v);
    static ores::trading::messaging::list_instrument_schedule_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_instrument_schedule_versions_response& v);
    static ores::trading::messaging::list_instrument_schedule_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_instrument_schedule_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_instrument_schedule_version_request& v);
    static ores::trading::messaging::get_instrument_schedule_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_instrument_schedule_version_request& v);
    static ores::trading::messaging::get_instrument_schedule_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_instrument_schedule_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_instrument_schedule_version_response& v);
    static ores::trading::messaging::get_instrument_schedule_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_instrument_schedule_version_response& v);
    static ores::trading::messaging::get_instrument_schedule_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
