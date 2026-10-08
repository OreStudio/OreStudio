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
#ifndef ORES_TRADING_CORE_SERVICE_TRADE_EXPORT_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_TRADE_EXPORT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Reads trades as an export writes them: each trade's anchor, its ORE
 * identifier, its instrument and its envelope.
 *
 * A trade belongs to the books its current booking names, so a node's trades
 * are the trades booked in the node's books. The trades come back in trade id
 * order, so successive pages do not overlap.
 */
class ORES_TRADING_CORE_EXPORT trade_export_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.trade_export_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit trade_export_service(context ctx);

    /**
     * @brief The trades booked under a book, a portfolio or a business unit,
     * or every live trade of the tenant when the node is empty.
     *
     * The trades' instruments are appended to @p instruments, whose arrays
     * cover the page's trades only.
     */
    std::vector<messaging::trade_export_item>
    export_node(const std::string& node_id,
                std::uint32_t offset,
                std::uint32_t limit,
                domain::instrument_batch& instruments) const;

    /**
     * @brief The trades booked in a set of books.
     *
     * The trades' instruments are appended to @p instruments, whose arrays
     * cover the page's trades only.
     */
    std::vector<messaging::trade_export_item>
    export_books(const std::vector<std::string>& book_ids,
                 std::uint32_t offset,
                 std::uint32_t limit,
                 domain::instrument_batch& instruments) const;

private:
    std::vector<messaging::trade_export_item>
    export_trades(const std::vector<std::string>& trade_ids,
                  domain::instrument_batch& instruments) const;

    context ctx_;
};

}

#endif
