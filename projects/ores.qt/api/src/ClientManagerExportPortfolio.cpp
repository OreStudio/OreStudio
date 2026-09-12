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

// Isolated TU for ClientManager::exportPortfolio().
//
// export_portfolio_response carries vector<trade_export_item>, where each item
// holds an instrument_payload: the leaf type name plus the leaf as JSON text.
// No std::variant reaches the wire, so no alternative is lost to declaration
// order, and rfl::internal::no_duplicate_field_names never sees the whole
// variant at once.
//
// This TU also prevents the rfl instantiation from leaking into caller TUs via
// the process_authenticated_request<T> header template.

#include "ores.qt/ClientManager.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.

namespace ores::qt {

std::expected<trading::messaging::export_portfolio_response, std::string>
ClientManager::exportPortfolio(trading::messaging::export_portfolio_request request,
                               std::chrono::milliseconds timeout) {
    using ResponseType = trading::messaging::export_portfolio_response;
    using namespace trading::messaging;
    try {
        const auto raw = send_authenticated_request(
            export_portfolio_request::nats_subject, encode_request(request), timeout);
        return decode_response<ResponseType>(raw);
    } catch (const ores::nats::service::nats_connect_error&) {
        throw;
    } catch (const ores::nats::service::session_expired_error& e) {
        QMetaObject::invokeMethod(this, &ClientManager::sessionExpired, Qt::QueuedConnection);
        return std::unexpected(std::string(e.what()));
    } catch (const std::exception& e) {
        return std::unexpected(std::string(e.what()));
    }
}

} // namespace ores::qt
