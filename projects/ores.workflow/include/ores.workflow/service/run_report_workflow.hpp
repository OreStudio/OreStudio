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
#ifndef ORES_WORKFLOW_SERVICE_RUN_REPORT_WORKFLOW_HPP
#define ORES_WORKFLOW_SERVICE_RUN_REPORT_WORKFLOW_HPP

#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.workflow/messaging/workflow_protocol.hpp"
#include "ores.workflow/service/workflow_executor.hpp"

namespace ores::workflow::service {

/**
 * @brief Saga executor for the run_report workflow.
 *
 * Drives a single report instance through its execution lifecycle:
 *   0. reporting.v1.report-instances.mark-running  — records start time
 *   1. ORE execution stub                          — placeholder for ORE call
 *   2. reporting.v1.report-instances.mark-completed — records completion time
 *
 * On failure, compensation calls:
 *   - reporting.v1.report-instances.mark-failed
 *
 * The workflow_id passed at construction is used as the parent FK when
 * writing workflow_step records to the database.
 */
class run_report_workflow : public workflow_executor {
public:
    /**
     * @brief Constructs the executor.
     *
     * @param workflow_id       UUID of the parent workflow instance.
     * @param report_instance_id String UUID of the report_instance to run.
     * @param tenant_id         String UUID of the owning tenant.
     * @param definition_id     String UUID of the report definition (for logging).
     */
    run_report_workflow(boost::uuids::uuid workflow_id,
        std::string report_instance_id,
        std::string tenant_id,
        std::string definition_id);

    bool execute(ores::database::context ctx,
        ores::nats::service::nats_client& nats) override;

    void compensate(ores::database::context ctx,
        ores::nats::service::nats_client& nats) override;

    [[nodiscard]] const std::string& failure_reason() const override {
        return error_;
    }

private:
    boost::uuids::uuid workflow_id_;
    std::string report_instance_id_;
    std::string tenant_id_;
    std::string definition_id_;
    std::string error_;

    /**
     * @brief Tracks how many steps completed:
     *   0 = none
     *   1 = mark-running sent
     *   2 = ORE execution completed
     *   3 = mark-completed sent
     */
    int completed_steps_{0};
};

}

#endif
