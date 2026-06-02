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
#ifndef ORES_ORE_SERVICE_MESSAGING_ORE_IMPORT_EXECUTE_HANDLER_HPP
#define ORES_ORE_SERVICE_MESSAGING_ORE_IMPORT_EXECUTE_HANDLER_HPP

#include <string>
#include <filesystem>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"

namespace ores::ore::service::messaging {

/**
 * @brief Workflow step handlers for the ore_import_workflow.
 *
 * Handles two fire-and-forget subjects dispatched by the workflow engine:
 *
 *  ore.v1.ore.import.execute — runs the full import (fetch, scan, plan, save)
 *                              and calls publish_step_completion.
 *
 *  ore.v1.ore.import.rollback — deletes all entities saved in a prior execute
 *                               step and calls publish_step_completion.
 *
 * The bearer_token field in the command payload is used to delegate the
 * original caller's identity to downstream domain services (save_currency,
 * save_trade, etc.).
 */
class ore_import_execute_handler {
private:
    inline static std::string_view logger_name =
        "ores.ore.service.messaging.ore_import_execute_handler";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    ore_import_execute_handler(ores::nats::service::client& nats,
        ores::nats::service::nats_client outbound_nats,
        std::string http_base_url,
        std::string work_dir);

    /**
     * @brief Handles ore.v1.ore.import.execute.
     *
     * Fetches and unpacks the ORE tarball, scans the directory, builds the
     * import plan, saves all currencies/portfolios/books/trades, and calls
     * publish_step_completion with an ore_import_execute_result payload.
     */
    void execute(ores::nats::message msg);

    /**
     * @brief Handles ore.v1.ore.import.rollback.
     *
     * Deletes all entities saved by a prior execute step (trades → books →
     * portfolios → currencies) and calls publish_step_completion.
     */
    void rollback(ores::nats::message msg);

private:
    ores::nats::service::client& nats_;
    ores::nats::service::nats_client outbound_nats_;
    std::string http_base_url_;
    std::filesystem::path work_dir_;
};

}

#endif
