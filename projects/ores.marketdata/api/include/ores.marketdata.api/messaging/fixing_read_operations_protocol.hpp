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
#ifndef ORES_MARKETDATA_API_MESSAGING_FIXING_READ_OPERATIONS_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_FIXING_READ_OPERATIONS_PROTOCOL_HPP

#include "ores.utility/domain/protocol.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::messaging {

/**
 * @brief Asks for the latest fixing of an item on or before a date.
 */
struct get_fixing_request {
    using response_type = struct get_fixing_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.get_fixing";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The fixing's oresmd URI, which names the ORE index.
     */
    std::string item;
    /**
     * @brief The provider to read, or empty for the one provider that carries the item.
     *
     * Two providers can deliver one ORE index, the ECB rate from the ECB itself and from
     * a vendor. An item that more than one provider carries is refused as ambiguous
     * unless the request names one.
     */
    std::string provider_code;
    /**
     * @brief Whose fixing to read, or empty for the parties the caller can see.
     *
     * A fixing is entitled by party, so a read that names none reads across every
     * party the session sees, and a session that sees none is refused. A service
     * holds no party of its own and names the party whose data it needs, which is
     * the party of the record it works for; a session that sees parties narrows to
     * the one it names and is refused when it cannot see it.
     */
    std::string party_id;
    /**
     * @brief The last date the fixing may fall on.
     */
    std::chrono::year_month_day on_or_before;
};

struct get_fixing_response {
    /**
     * @brief How the call ended: a session with no party and an item that names the
     * series of more than one party are refused here.
     */
    ores::utility::domain::result result;
    /**
     * @brief The provider that published the item.
     */
    std::string provider_code;
    /**
     * @brief The scheme of the vendor id, or empty when no mapping names the item.
     */
    std::string vendor_scheme;
    /**
     * @brief The provider's own id of the item, or empty when no mapping names it.
     */
    std::string vendor_id;
    /**
     * @brief The date of the fixing, which is the publisher's own time for a daily
     * fixing; absent when the item has no fixing on or before the date.
     */
    std::optional<std::chrono::year_month_day> fixing_date;
    /**
     * @brief The value as stored, in decimal text.
     */
    std::string value;
    /**
     * @brief The source the fixing came through.
     */
    std::string source;
    /**
     * @brief When the service stored the fixing.
     */
    std::optional<std::chrono::system_clock::time_point> received_at;
};

/**
 * @brief Asks for every fixing of an item in the days up to a date.
 */
struct get_fixing_window_request {
    using response_type = struct get_fixing_window_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.get_fixing_window";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The fixing's oresmd URI, which names the ORE index.
     */
    std::string item;
    /**
     * @brief The provider to read, or empty for the one provider that carries the item.
     *
     * Two providers can deliver one ORE index, the ECB rate from the ECB itself and from
     * a vendor. An item that more than one provider carries is refused as ambiguous
     * unless the request names one.
     */
    std::string provider_code;
    /**
     * @brief Whose fixings to read, or empty for the parties the caller can see.
     *
     * A fixing is entitled by party, so a read that names none reads across every
     * party the session sees, and a session that sees none is refused. A service
     * holds no party of its own and names the party whose data it needs, which is
     * the party of the record it works for; a session that sees parties narrows to
     * the one it names and is refused when it cannot see it.
     */
    std::string party_id;
    /**
     * @brief The last date of the window, included.
     */
    std::chrono::year_month_day to;
    /**
     * @brief How many calendar days the window spans, ending on the last date.
     */
    int days = 0;
};

struct get_fixing_window_response {
    /**
     * @brief How the call ended: a session with no party, an ambiguous item, and a
     * window that is empty or over the ceiling are refused here.
     */
    ores::utility::domain::result result;
    /**
     * @brief The provider that published the item.
     */
    std::string provider_code;
    /**
     * @brief The scheme of the vendor id, or empty when no mapping names the item.
     */
    std::string vendor_scheme;
    /**
     * @brief The provider's own id of the item, or empty when no mapping names it.
     */
    std::string vendor_id;
    /**
     * @brief How many calendar days the window spans.
     */
    int days = 0;
    /**
     * @brief How many of those days hold a fixing; the days without one are skipped.
     */
    int used = 0;
    /**
     * @brief The date of each fixing, oldest first.
     */
    std::vector<std::chrono::year_month_day> dates;
    /**
     * @brief The value of each fixing, in decimal text.
     */
    std::vector<std::string> values;
    /**
     * @brief The source of each fixing.
     */
    std::vector<std::string> sources;
    /**
     * @brief When the service stored each fixing.
     */
    std::vector<std::chrono::system_clock::time_point> received_at;
};

}

#endif
