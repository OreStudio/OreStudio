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
#include "ores.trading.core/service/operational_authorisation_service.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.inbox.core/service/approval_announcer.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.trading.api/domain/trade_authorisation_request.hpp"
#include "ores.trading.core/repository/trade_authorisation_request_repository.hpp"
#include "ores.trading.core/repository/trade_repository.hpp"
#include "ores.trading.core/repository/trade_state_repository.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <stdexcept>
#include <string>

namespace ores::trading::service {

using namespace ores::logging;

operational_authorisation_service::operational_authorisation_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::optional<raised_authorisation>
operational_authorisation_service::raise_if_awaiting(const boost::uuids::uuid& trade_id) {
    // The reads go through the caller's transaction, which holds the trade
    // before it commits. A query on a connection of its own would not see it.
    const auto id = boost::uuids::to_string(trade_id);
    const auto trades = repository::trade_repository().read_latest(ctx_, id);
    const auto states = repository::trade_state_repository().read_latest(ctx_, id);
    if (trades.empty() || states.empty())
        return std::nullopt;
    const auto& trade = trades.front();
    if (trade.booking_nature != domain::booking_nature::actual)
        return std::nullopt;

    // The status machine is reference data and already committed.
    const auto live = ores::database::repository::execute_parameterized_string_query(
        ctx_,
        "select ores_trading_status_is_live_fn($1::uuid)::text",
        {boost::uuids::to_string(states.front().status_id)},
        lg(),
        "Checking whether a trade state is live");
    if (live.empty() || live.front() != "true")
        return std::nullopt;

    ores::inbox::service::approval_lifecycle lifecycle(ctx_);
    auto kind = lifecycle.kind(kind_code);
    if (!kind)
        throw std::runtime_error(std::string("No such kind of request: ") + kind_code);
    const auto me = lifecycle.actor_account_id();
    if (!me)
        throw std::runtime_error("The signed-in account was not found.");

    auto request = lifecycle.raise(*kind, "A live trade awaits operational authorisation.", *me);

    domain::trade_authorisation_request detail;
    detail.tenant_id = ctx_.tenant_id();
    detail.request_id = request.id;
    detail.trade_id = trade_id;
    detail.trade_version = trade.version;
    detail.modified_by = ctx_.actor();
    detail.change_reason_code = "system.new_record";
    repository::trade_authorisation_request_repository{}.write(
        ctx_, detail, ores::utility::domain::precondition{});

    BOOST_LOG_SEV(lg(), info) << "Raised request " << boost::uuids::to_string(request.id)
                              << " for trade " << id;
    return raised_authorisation{std::move(*kind), std::move(request)};
}

void operational_authorisation_service::announce(const raised_authorisation& raised) {
    ores::inbox::service::approval_lifecycle lifecycle(ctx_);
    ores::inbox::service::approval_announcer(ctx_).tell_open_parts(
        lifecycle, raised.kind, raised.request);
}

}
