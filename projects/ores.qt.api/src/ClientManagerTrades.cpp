/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
// Isolated in its own TU: rfl::json::read<get_trades_response> instantiates
// reflection for vector<trade> (25 fields) and must not share a TU with other
// heavy rfl types to avoid MSVC C1202 constexpr-depth limit.
#include "ores.qt/ClientManager.hpp"

#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

std::optional<TradeListResult> ClientManager::listTrades(
    std::optional<boost::uuids::uuid> node_id,
    std::uint32_t offset,
    std::uint32_t limit) {

    try {
        trading::messaging::get_trades_request request;
        request.offset = static_cast<int>(offset);
        request.limit = static_cast<int>(limit);
        if (node_id)
            request.node_id = boost::uuids::to_string(*node_id);

        auto result = process_authenticated_request(std::move(request));
        if (!result) {
            BOOST_LOG_SEV(lg(), error)
                << "listTrades failed: " << result.error();
            return std::nullopt;
        }
        if (!result->success) {
            BOOST_LOG_SEV(lg(), error)
                << "listTrades server error: " << result->message;
            return std::nullopt;
        }

        return TradeListResult{
            .trades = std::move(result->trades),
            .total_count = static_cast<std::uint32_t>(
                result->total_available_count)
        };
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "listTrades failed: " << e.what();
        return std::nullopt;
    }
}

}
