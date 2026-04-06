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
#ifndef ORES_COMPUTE_MESSAGING_REPORT_SUBMIT_HANDLER_HPP
#define ORES_COMPUTE_MESSAGING_REPORT_SUBMIT_HANDLER_HPP

#include <string>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"

namespace ores::compute::messaging {

/**
 * @brief Workflow step handler for submit_compute.
 *
 * Receives a submit_compute_request dispatched by the workflow engine,
 * creates a compute batch and one workunit per tarball URI, publishes
 * work_assignment_event messages, and completes the workflow step.
 *
 * Phase 3.9: calls wf->complete() immediately so the report workflow
 * proceeds to finalise for end-to-end validation.  Phase 3.10 will
 * change this to an async bridge where the assimilator fires
 * step_completed when the batch terminates.
 */
class report_submit_handler {
private:
    inline static std::string_view logger_name =
        "ores.compute.messaging.report_submit_handler";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    report_submit_handler(ores::nats::service::client& nats,
        ores::database::context ctx);

    void submit(ores::nats::message msg);

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
};

}

#endif
