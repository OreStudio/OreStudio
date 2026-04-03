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
#ifndef ORES_ORE_SERVICE_SERVICE_ORE_IMPORT_WORKFLOW_HPP
#define ORES_ORE_SERVICE_SERVICE_ORE_IMPORT_WORKFLOW_HPP

#include <string>
#include <vector>
#include <filesystem>
#include <boost/uuid/uuid.hpp>
#include "ores.workflow/service/workflow_executor.hpp"
#include "ores.ore.api/messaging/ore_import_protocol.hpp"

namespace ores::ore::service::service {

/**
 * @brief Saga executor for the ore_import workflow.
 *
 * Steps executed in order:
 *   0. storage_transfer::fetch_and_unpack — download + extract ORE tarball
 *   1. ore_directory_scanner::scan       — find currency and portfolio files
 *   2. refdata.v1.currencies.list        — fetch existing ISO codes for filter
 *   3. ore_import_planner::plan          — produce currencies/portfolios/books/trades
 *   4. refdata.v1.currencies.save        — one NATS call per currency
 *   5. refdata.v1.portfolios.save        — one NATS call per portfolio (parents first)
 *   6. refdata.v1.books.save             — one NATS call per book
 *   7. trading.v1.trades.save            — one NATS call per trade
 *      Trade failures are collected in item_errors; the saga continues.
 *
 * Compensation (reverse order on infrastructure failure):
 *   Delete all successfully saved trades, books, portfolios, and currencies.
 *   Steps 0-3 produce no server-side state and need no compensation.
 *
 * Market data import is deferred pending scanner support.
 */
class ore_import_workflow : public ores::workflow::service::workflow_executor {
public:
    ore_import_workflow(boost::uuids::uuid workflow_id,
        ores::ore::messaging::ore_import_request request,
        std::string correlation_id,
        std::string http_base_url,
        std::string work_dir);

    bool execute(ores::database::context ctx,
        ores::nats::service::nats_client& nats) override;

    void compensate(ores::database::context ctx,
        ores::nats::service::nats_client& nats) override;

    [[nodiscard]] const std::string& failure_reason() const override {
        return error_;
    }

    /**
     * @brief Returns the response to send back to the caller.
     *
     * Valid after execute() returns (true or false).
     */
    [[nodiscard]] const ores::ore::messaging::ore_import_response& result() const {
        return result_;
    }

private:
    boost::uuids::uuid workflow_id_;
    ores::ore::messaging::ore_import_request request_;
    std::string correlation_id_;
    std::string http_base_url_;
    std::filesystem::path work_dir_;

    // Saga progress — used for compensation
    std::vector<std::string> saved_currency_iso_codes_;
    std::vector<std::string> saved_portfolio_ids_;
    std::vector<std::string> saved_book_ids_;
    std::vector<std::string> saved_trade_ids_;

    std::string error_;
    ores::ore::messaging::ore_import_response result_;
};

}

#endif
