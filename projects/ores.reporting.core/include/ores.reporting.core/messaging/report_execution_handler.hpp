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
#ifndef ORES_REPORTING_MESSAGING_REPORT_EXECUTION_HANDLER_HPP
#define ORES_REPORTING_MESSAGING_REPORT_EXECUTION_HANDLER_HPP

#include <string>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.workflow/service/fsm_state_map.hpp"

namespace ores::reporting::messaging {

/**
 * @brief Workflow step handlers for report execution.
 *
 * Handles fire-and-forget commands dispatched by the workflow engine:
 *
 *  reporting.v1.report.gather-trades  — loads config, resolves scope,
 *                                       fetches trades from trading service
 *  reporting.v1.report.assemble-bundle — persists gathered data (stub)
 *  reporting.v1.report.finalise       — marks instance as completed
 *  reporting.v1.report.fail           — marks instance as failed (compensation)
 */
class report_execution_handler {
private:
    inline static std::string_view logger_name =
        "ores.reporting.messaging.report_execution_handler";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    report_execution_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        ores::nats::service::nats_client svc_nats,
        ores::workflow::service::fsm_state_map instance_states,
        std::string http_base_url);

    void gather_trades(ores::nats::message msg);
    void assemble_bundle(ores::nats::message msg);
    void finalise(ores::nats::message msg);
    void fail(ores::nats::message msg);

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::nats::service::nats_client svc_nats_;
    ores::workflow::service::fsm_state_map instance_states_;
    std::string http_base_url_;
};

}

#endif
