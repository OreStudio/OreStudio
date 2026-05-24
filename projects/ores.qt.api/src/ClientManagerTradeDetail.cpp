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

namespace ores::qt {

using namespace ores::logging;

// Two-phase parse: phase 1 reflects only trade (a flat struct); phase 2
// reflects only trade_instrument (the variant) with AddTagsToVariants.
// Keeping them in separate TUs prevents the compiler from having to
// instantiate both reflection passes simultaneously — the combination that
// triggers MSVC C1202 and Clang bracket-depth limits.
std::optional<trading::messaging::trade_export_item>
ClientManager::getTradeDetail(const std::string& trade_id) {
    try {
        trading::messaging::get_trade_detail_request request;
        request.trade_id = trade_id;
        const auto raw = send_authenticated_request(
            trading::messaging::get_trade_detail_request::nats_subject,
            rfl::json::write(request),
            std::chrono::seconds(30));

        // Phase 1: parse success/message/trade — no variant, no AddTagsToVariants.
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

        // Phase 2: parse instrument variant — isolated from trade reflection.
        trading::domain::trade_instrument instrument;
        auto wrapper = rfl::json::read<
            trading::messaging::get_trade_detail_instrument_wrapper,
            rfl::AddTagsToVariants>(raw);
        if (wrapper)
            instrument = std::move(wrapper->instrument);

        return trading::messaging::trade_export_item{
            .trade = std::move(base->trade),
            .instrument = std::move(instrument)
        };
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
