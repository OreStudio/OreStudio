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
#ifndef ORES_WORKFLOW_MESSAGING_WORKFLOW_PROTOCOL_HPP
#define ORES_WORKFLOW_MESSAGING_WORKFLOW_PROTOCOL_HPP

#include <string>
#include <vector>
#include <optional>
#include <string_view>

namespace ores::workflow::messaging {

/**
 * @brief Per-party provisioning input for provision_parties_workflow.
 *
 * Bundles the party attributes and the initial account credentials
 * for one party to be provisioned together as a saga.
 */
struct provision_party_input {
    // --- Party fields ---
    std::string full_name;
    std::string short_code;
    std::string party_category;
    std::string party_type;
    std::string business_center_code;
    std::optional<std::string> parent_party_id;

    // --- Account fields ---
    std::string principal;
    std::string password;
    std::string totp_secret;
    std::string email;
    std::string account_type;
};

/**
 * @brief Request to provision one or more parties together with their
 * initial IAM accounts.
 *
 * Each element of @p parties triggers a 3-step saga:
 *   1. refdata.v1.parties.save
 *   2. iam.v1.accounts.save
 *   3. iam.v1.account-parties.save
 *
 * On failure the executor compensates completed steps in reverse order.
 */
struct provision_parties_request {
    using response_type = struct provision_parties_response;
    static constexpr std::string_view nats_subject = "workflow.v1.parties.provision";

    std::vector<provision_party_input> parties;
};

/**
 * @brief Response for a provision_parties_request.
 *
 * On success, @p party_ids and @p account_ids are populated in the same order
 * as the input @p parties vector.
 */
struct provision_parties_response {
    bool success = false;
    std::string message;
    std::vector<std::string> party_ids;
    std::vector<std::string> account_ids;
    /**
     * @brief Correlation ID echoed back for client-side logging and support.
     *
     * Set to the Nats-Correlation-Id header value from the inbound request
     * (or a freshly generated UUID if the caller did not provide one).
     */
    std::string correlation_id;
};

/**
 * @brief Fire-and-forget message published by the reporting service when a
 *        report instance has been created and needs to be executed.
 *
 * Published to workflow.v1.reports.run after report_instance_handler::trigger()
 * saves the pending instance. The workflow service subscribes, creates a
 * workflow_instance record, and runs run_report_workflow to drive the
 * ORE execution lifecycle.
 */
struct run_report_message {
    static constexpr std::string_view nats_subject = "workflow.v1.reports.run";
    std::string report_instance_id;
    std::string tenant_id;
    std::string definition_id;
};

}

#endif
