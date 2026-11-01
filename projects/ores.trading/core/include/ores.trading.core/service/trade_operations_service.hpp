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
#ifndef ORES_TRADING_CORE_SERVICE_TRADE_OPERATIONS_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_TRADE_OPERATIONS_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/service/book_status_port.hpp"
#include "ores.trading.core/service/operational_authorisation_service.hpp"
#include <string_view>

namespace ores::trading::service {

/**
 * @brief The operations that write a trade across its anchor and components.
 *
 * Hand-written beside the generated entity services, because an operation
 * writes several tables at once and no entity verb can.
 */
class ORES_TRADING_CORE_EXPORT trade_operations_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.trade_operations_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief The service, with a way to ask a book's status.
     *
     * The port is required rather than defaulted: a booking that cannot ask
     * whether its book is open would apply the rule to some callers and not
     * others, and the caller that forgot would be the one nobody notices.
     */
    trade_operations_service(context ctx, const book_status_port& books);

    /**
     * @brief Books a trade: its anchor, its booking and its first state, in
     * one transaction.
     *
     * The tenant, the party and the audit fields come from the caller's
     * context. A trade id that is already booked is answered with a conflict
     * and writes nothing.
     */
    /**
     * @brief Books a trade, asking the book's status as @p actor_token.
     *
     * The token is the caller's own, forwarded so the status read answers for
     * the person booking rather than for this service.
     */
    messaging::book_trade_response book_trade(const messaging::book_trade_request& request,
                                              std::string_view actor_token);

    /**
     * @brief Takes a draft trade live, and raises the request Operations
     * answers, in one transaction.
     *
     * The activity and the state join the request, so an actual trade is never
     * live without one. A trade that is not a draft, or that moved since the
     * caller read it, is answered with a conflict and writes nothing.
     */
    messaging::execute_trade_response execute_trade(const messaging::execute_trade_request& request);

    /**
     * @brief Completes a structure: its anchor, its legs, and each leg executed,
     * in one transaction.
     *
     * Every leg must be a draft in a real book when the request arrives. A
     * completion that is refused leaves every leg a draft and writes no member,
     * because the structure, the members and the executions are one unit of
     * work. A structure that already exists answers a conflict coded
     * already_exists.
     */
    messaging::complete_structure_response
    complete_structure(const messaging::complete_structure_request& request);

    /**
     * @brief Creates a bond issue with its legs and their child rows, in one
     * transaction.
     *
     * The issue is create-only and whole or absent. An issue id that is
     * stored with equal terms is answered with a conflict coded
     * already_exists. One stored with other terms is answered with a conflict
     * coded content_differs. Neither writes anything.
     */
    messaging::put_bond_issue_composite_response
    put_bond_issue_composite(const messaging::put_bond_issue_composite_request& request);

private:
    /**
     * @brief Takes one draft live inside the caller's transaction.
     *
     * The activity, the state and the authorisation request, without a
     * transaction of its own: an execution that a booking or a completion needs
     * must join that write rather than commit beside it.
     *
     * @return The request raised, when the trade started waiting on Operations.
     */
    std::optional<raised_authorisation>
    execute_within(context ctx,
                   const boost::uuids::uuid& trade_id,
                   const std::string& change_reason_code,
                   const std::string& change_commentary,
                   boost::uuids::uuid* activity_id = nullptr);

    context ctx_;
    const book_status_port* books_;
};

}

#endif
