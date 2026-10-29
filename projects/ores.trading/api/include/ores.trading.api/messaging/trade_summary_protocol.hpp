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
#ifndef ORES_TRADING_API_MESSAGING_TRADE_SUMMARY_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_TRADE_SUMMARY_PROTOCOL_HPP

#include "ores.trading.api/domain/trade_summary.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct trade_summary_key {
    boost::uuids::uuid id;
};

struct trade_summary_lookup {
    trade_summary_key key;
    std::optional<ores::trading::domain::trade_summary> trade_summary;
};

struct trade_summaries_filter {
    std::optional<std::string> row_kind;
    std::optional<boost::uuids::uuid> party_id;
    std::optional<std::optional<boost::uuids::uuid>> counterparty_id;
    std::optional<std::optional<boost::uuids::uuid>> book_id;
    std::optional<std::string> instrument_type;
    std::optional<std::optional<std::string>> status;
    std::optional<std::string> authorisation_status;
    std::optional<std::optional<std::chrono::year_month_day>> trade_date;
    std::optional<std::optional<boost::uuids::uuid>> structure_id;
    std::optional<bool> is_leg;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<std::string>> row_kind_one_of;
    std::optional<std::vector<boost::uuids::uuid>> party_id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> counterparty_id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> book_id_one_of;
    std::optional<std::vector<std::string>> instrument_type_one_of;
    std::optional<std::vector<std::string>> status_one_of;
    std::optional<std::vector<std::string>> authorisation_status_one_of;
    std::optional<std::vector<std::chrono::year_month_day>> trade_date_one_of;
    std::optional<std::vector<boost::uuids::uuid>> structure_id_one_of;
    std::optional<std::vector<bool>> is_leg_one_of;
    std::optional<std::string> trade_date_from;
    std::optional<std::string> trade_date_to;
    std::optional<std::string> search;
};

struct trade_summary_event {
    boost::uuids::uuid event_id;
    trade_summary_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_trade_summaries_request {
    using response_type = struct list_trade_summaries_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_summaries.list";
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
    std::optional<trade_summaries_filter> filter;
};

struct list_trade_summaries_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_summary> trade_summaries;
    std::uint64_t total;
};

struct get_trade_summary_request {
    using response_type = struct get_trade_summary_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_summaries.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_summary_key key;
};

struct get_trade_summary_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade_summary> trade_summary;
};

struct get_many_trade_summaries_request {
    using response_type = struct get_many_trade_summaries_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_summaries.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_summary_key> keys;
};

struct get_many_trade_summaries_response {
    ores::utility::domain::result result;
    std::vector<trade_summary_lookup> entries;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace trade_summary_event_subjects {
inline constexpr std::string_view created = "trading.v1.trade_summaries_events.created";
inline constexpr std::string_view updated = "trading.v1.trade_summaries_events.updated";
inline constexpr std::string_view deleted = "trading.v1.trade_summaries_events.deleted";
}

}

#endif
