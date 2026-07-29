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
#include "ores.qt.headless/IInstrumentFormPopulator.hpp"
#include "ores.qt.headless/parse_trade_instrument.hpp"
#include "ores.qt/ClientManager.hpp"

namespace ores::qt {

using namespace ores::logging;

std::optional<trading::domain::trade>
ClientManager::getTradeInstrument(const std::string& trade_id,
                                  IInstrumentFormPopulator& populator) {
    try {
        trading::messaging::get_trade_instrument_request request;
        request.trade_id = trade_id;
        const auto raw = send_authenticated_request(
            trading::messaging::get_trade_instrument_request::nats_subject,
            encode_request(request),
            std::chrono::seconds(30));

        // parse_trade_instrument() is a JSON-specific two-phase parser (peeks
        // product_type, then re-parses the same raw bytes as different
        // wrapper structs) living in ores.qt.headless, outside this task's
        // file scope -- see the task's Notes for why it stays rfl::json-only
        // for now. Under wire_format::msgpack this decode will fail (request
        // and response would be format-mismatched); harmless under today's
        // json default, which is all this task's acceptance criteria cover.
        return parse_trade_instrument(raw, populator);

    } catch (const ores::nats::service::nats_connect_error&) {
        throw;
    } catch (const ores::nats::service::session_expired_error& e) {
        BOOST_LOG_SEV(lg(), warn) << "Session expired: " << e.what();
        QMetaObject::invokeMethod(this, [this] { emit sessionExpired(); }, Qt::QueuedConnection);
        return std::nullopt;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "getTradeInstrument failed: " << e.what();
        return std::nullopt;
    }
}

} // namespace ores::qt
