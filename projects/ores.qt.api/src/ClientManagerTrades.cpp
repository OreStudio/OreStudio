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
#include "ores.qt/ClientManager.hpp"

#include <boost/uuid/uuid_io.hpp>
#include "ores.nats/service/nats_connect_error.hpp"
#include "ores.nats/service/session_expired_error.hpp"

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

std::optional<trading::messaging::trade_export_item>
ClientManager::getTradeDetail(const std::string& trade_id) {
    try {
        trading::messaging::get_trade_detail_request request;
        request.trade_id = trade_id;
        const auto raw = send_authenticated_request(
            trading::messaging::get_trade_detail_request::nats_subject,
            rfl::json::write(request),
            std::chrono::seconds(30));

        auto base = rfl::json::read<
            trading::messaging::get_trade_detail_response_base>(raw);
        if (!base) {
            BOOST_LOG_SEV(lg(), error)
                << "getTradeDetail deserialize error: " << base.error().what();
            return std::nullopt;
        }
        if (!base->success) {
            BOOST_LOG_SEV(lg(), error)
                << "getTradeDetail server error: " << base->message;
            return std::nullopt;
        }
        // rfl::internal::no_duplicate_field_names does O(N^2) constexpr
        // comparisons over all variant alternatives.  trade_instrument has 9
        // top-level alternatives (two of which are nested variants), which
        // pushes MSVC past its default 100 000-step constexpr budget (C1202).
        // Raise the limit for this translation unit only.
#ifdef _MSC_VER
#  pragma constexpr_depth(1024)
#  pragma constexpr_steps(1000000)
#endif
        auto inst = rfl::json::read<
            trading::messaging::get_trade_detail_instrument_wrapper,
            rfl::AddTagsToVariants>(raw);
        if (!inst) {
            BOOST_LOG_SEV(lg(), error)
                << "getTradeDetail instrument deserialize error: "
                << inst.error().what();
            return std::nullopt;
        }

        trading::messaging::trade_export_item item;
        item.trade = std::move(base->trade);
        item.instrument = std::move(inst->instrument);
        return item;
    } catch (const ores::nats::service::nats_connect_error&) {
        throw;
    } catch (const ores::nats::service::session_expired_error& e) {
        BOOST_LOG_SEV(lg(), warn) << "Session expired: " << e.what();
        QMetaObject::invokeMethod(this, [this] { emit sessionExpired(); }, Qt::QueuedConnection);
        return std::nullopt;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "getTradeDetail failed: " << e.what();
        return std::nullopt;
    }
}

}
