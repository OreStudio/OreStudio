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
#ifndef ORES_WORKFLOW_SERVICE_PROVISION_PARTIES_WORKFLOW_HPP
#define ORES_WORKFLOW_SERVICE_PROVISION_PARTIES_WORKFLOW_HPP

#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.workflow/domain/workflow_instance.hpp"
#include "ores.workflow/messaging/workflow_protocol.hpp"
#include "ores.workflow/service/workflow_executor.hpp"

namespace ores::workflow::service {

/**
 * @brief Saga executor for the provision_parties workflow.
 *
 * For each party in the request, executes three steps in order:
 *   0. refdata.v1.parties.save    — creates the party record
 *   1. iam.v1.accounts.save       — creates the IAM account
 *   2. iam.v1.account-parties.save — links the account to the party
 *
 * On failure, compensates completed steps in reverse order:
 *   - Deletes account-party links (iam.v1.account-parties.delete)
 *   - Deletes accounts             (iam.v1.accounts.delete)
 *   - Deletes parties              (refdata.v1.parties.delete)
 *
 * The workflow_id passed at construction is used as the parent FK when
 * writing workflow_step records to the database.
 */
class provision_parties_workflow : public workflow_executor {
public:
    /**
     * @brief Constructs the executor.
     *
     * @param workflow_id  UUID of the parent workflow instance (already created
     *                     by the handler before calling execute()).
     * @param request      Decoded inbound provisioning request.
     */
    provision_parties_workflow(boost::uuids::uuid workflow_id,
        messaging::provision_parties_request request);

    bool execute(ores::database::context ctx,
        ores::nats::service::nats_client& nats) override;

    void compensate(ores::database::context ctx,
        ores::nats::service::nats_client& nats) override;

    [[nodiscard]] const std::string& failure_reason() const override { return error_; }

    /**
     * @brief Returns the provisioning result.
     *
     * Valid only after execute() returns true.
     */
    [[nodiscard]] const messaging::provision_parties_response& result() const {
        return result_;
    }

private:
    /**
     * @brief Per-party execution state.
     *
     * completed_steps tracks how many of the 3 saga steps succeeded:
     *   0 = none, 1 = party saved, 2 = account saved, 3 = link saved.
     */
    struct party_state {
        boost::uuids::uuid party_id;
        std::string account_id;
        int completed_steps = 0;
    };

    boost::uuids::uuid workflow_id_;
    messaging::provision_parties_request request_;
    std::string error_;
    std::vector<party_state> party_states_;
    messaging::provision_parties_response result_;
};

}

#endif
