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
// export_portfolio_response carries vector<trade_export_item> where each item
// contains trade_instrument — a std::variant with 9 alternatives including
// with_legs<..., swap_leg> types. rfl::json::read<export_portfolio_response>
// instantiates rfl::StringLiteral<N> types for swap_leg's 19 fields.
//
// Because process_authenticated_request<T> is a header template, calling it
// with export_portfolio_request in a large TU (BookMdiWindow.cpp, step 4651/4936)
// injects those Literals into an already-saturated MSVC dependency graph, triggering
// C1202. This concrete method keeps the instantiation in a dedicated, fresh TU.

#include "ores.qt/ClientManager.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.

namespace ores::qt {

std::expected<trading::messaging::export_portfolio_response, std::string>
ClientManager::exportPortfolio(trading::messaging::export_portfolio_request request,
    std::chrono::milliseconds timeout)
{
    using ResponseType = trading::messaging::export_portfolio_response;
    using namespace trading::messaging;
    try {
        const auto raw = send_authenticated_request(
            export_portfolio_request::nats_subject,
            rfl::json::write(request),
            timeout);
        auto result = rfl::json::read<ResponseType>(raw);
        if (!result) {
            return std::unexpected(
                std::string("Failed to deserialize response: ") +
                result.error().what());
        }
        return std::move(*result);
    } catch (const ores::nats::service::nats_connect_error&) {
        throw;
    } catch (const ores::nats::service::session_expired_error& e) {
        QMetaObject::invokeMethod(this, "sessionExpired", Qt::QueuedConnection);
        return std::unexpected(std::string(e.what()));
    } catch (const std::exception& e) {
        return std::unexpected(std::string(e.what()));
    }
}

} // namespace ores::qt
