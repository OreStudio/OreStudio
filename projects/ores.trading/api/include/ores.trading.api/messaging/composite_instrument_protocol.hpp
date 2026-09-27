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
#ifndef ORES_TRADING_API_MESSAGING_COMPOSITE_INSTRUMENT_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_COMPOSITE_INSTRUMENT_PROTOCOL_HPP

#include "ores.trading.api/domain/composite_instrument.hpp"
#include "ores.trading.api/domain/composite_leg.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct composite_instrument_key {
    boost::uuids::uuid instrument_id;
};

struct composite_instrument_write {
    boost::uuids::uuid instrument_id;
    std::string trade_type_code;
    std::optional<boost::uuids::uuid> trade_id;
    std::string description;
};

struct composite_instrument_change {
    composite_instrument_write write;
    ores::utility::domain::precondition precondition;
};

struct composite_instrument_removal {
    composite_instrument_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct composite_instrument_lookup {
    composite_instrument_key key;
    std::optional<ores::trading::domain::composite_instrument> composite_instrument;
};

struct composite_instrument_event {
    boost::uuids::uuid event_id;
    composite_instrument_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct composite_instrument_version_key {
    composite_instrument_key composite_instrument;
    std::uint32_t version;
};

struct composite_instrument_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_composite_instruments_request {
    using response_type = struct list_composite_instruments_response;
    static constexpr std::string_view nats_subject = "trading.v1.composite_instruments.list";
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
};

struct list_composite_instruments_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::composite_instrument> composite_instruments;
    std::uint64_t total;
};

struct get_composite_instrument_request {
    using response_type = struct get_composite_instrument_response;
    static constexpr std::string_view nats_subject = "trading.v1.composite_instruments.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_instrument_key key;
};

struct get_composite_instrument_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::composite_instrument> composite_instrument;
};

struct get_many_composite_instruments_request {
    using response_type = struct get_many_composite_instruments_response;
    static constexpr std::string_view nats_subject = "trading.v1.composite_instruments.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<composite_instrument_key> keys;
};

struct get_many_composite_instruments_response {
    ores::utility::domain::result result;
    std::vector<composite_instrument_lookup> entries;
};

struct put_composite_instrument_request {
    using response_type = struct put_composite_instrument_response;
    static constexpr std::string_view nats_subject = "trading.v1.composite_instruments.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_instrument_change change;
    ores::utility::domain::change_intent intent;
};

struct put_composite_instrument_response {
    ores::utility::domain::result result;
    ores::trading::domain::composite_instrument composite_instrument;
};

struct put_many_composite_instruments_request {
    using response_type = struct put_many_composite_instruments_response;
    static constexpr std::string_view nats_subject = "trading.v1.composite_instruments.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<composite_instrument_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_composite_instruments_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::composite_instrument> composite_instruments;
};

struct delete_composite_instrument_request {
    using response_type = struct delete_composite_instrument_response;
    static constexpr std::string_view nats_subject = "trading.v1.composite_instruments.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_instrument_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_composite_instrument_response {
    ores::utility::domain::result result;
};

struct delete_many_composite_instruments_request {
    using response_type = struct delete_many_composite_instruments_response;
    static constexpr std::string_view nats_subject = "trading.v1.composite_instruments.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<composite_instrument_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_composite_instruments_response {
    ores::utility::domain::result result;
};

struct list_composite_instrument_versions_request {
    using response_type = struct list_composite_instrument_versions_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.composite_instruments_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_instrument_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<composite_instrument_versions_filter> filter;
};

struct list_composite_instrument_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::composite_instrument> versions;
    std::uint64_t total;
};

struct get_composite_instrument_version_request {
    using response_type = struct get_composite_instrument_version_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.composite_instruments_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_instrument_version_key key;
};

struct get_composite_instrument_version_response {
    ores::utility::domain::result result;
    ores::trading::domain::composite_instrument version;
};

/**
 * @brief Writes a composite instrument and replaces its whole leg set.
 *
 * One request rather than two, because the store refuses a leg whose
 * parent is not current: the instrument row and its legs have to reach
 * one handler in order. The leg set the request states replaces whatever
 * the instrument carried before.
 */
struct put_composite_instrument_with_legs_request {
    using response_type = struct put_composite_instrument_with_legs_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.composite_instruments.put_with_legs";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ores::trading::domain::composite_instrument instrument;
    std::vector<ores::trading::domain::composite_leg> legs;
};

struct put_composite_instrument_with_legs_response {
    ores::utility::domain::result result;
    ores::trading::domain::composite_instrument instrument;
};

struct get_composite_instrument_legs_request {
    using response_type = struct get_composite_instrument_legs_response;
    static constexpr std::string_view nats_subject = "trading.v1.composite_instruments.legs";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string instrument_id;
};

struct get_composite_instrument_legs_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::composite_leg> legs;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace composite_instrument_event_subjects {
inline constexpr std::string_view created = "trading.v1.composite_instruments_events.created";
inline constexpr std::string_view updated = "trading.v1.composite_instruments_events.updated";
inline constexpr std::string_view deleted = "trading.v1.composite_instruments_events.deleted";
}

}

#endif
