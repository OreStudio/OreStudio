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
// getTradeDetail() is isolated here so that the two-phase rfl deserialization
// (trade base, 22 fields) + (instrument variant, 8 alternatives) does not
// share a TU with listTrades (vector<trade>, 25 fields) and push MSVC over the
// C1202 constexpr-depth limit.
#include "ores.qt/ClientManager.hpp"

#include "ores.nats/service/nats_connect_error.hpp"
#include "ores.nats/service/session_expired_error.hpp"

namespace ores::qt {

using namespace ores::logging;

std::optional<trading::messaging::trade_export_item>
ClientManager::getTradeDetail(const std::string& trade_id) {
    try {
        trading::messaging::get_trade_detail_request request;
        request.trade_id = trade_id;
        const auto raw = send_authenticated_request(
            trading::messaging::get_trade_detail_request::nats_subject,
            rfl::json::write(request),
            std::chrono::seconds(30));

        // Two-phase parse: avoid combining trade (22 fields) with the
        // trade_instrument variant (8 alternatives) in a single rfl call,
        // which exceeds MSVC's constexpr depth budget (C1202).
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
        using namespace ores::logging;
        BOOST_LOG_SEV(lg(), warn) << "Session expired: " << boost::diagnostic_information(e);
        QMetaObject::invokeMethod(this, [this] { emit sessionExpired(); }, Qt::QueuedConnection);
        return std::nullopt;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "getTradeDetail failed: " << e.what();
        return std::nullopt;
    }
}

}
