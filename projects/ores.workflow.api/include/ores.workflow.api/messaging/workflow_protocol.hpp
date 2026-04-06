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
#ifndef ORES_WORKFLOW_API_MESSAGING_WORKFLOW_PROTOCOL_HPP
#define ORES_WORKFLOW_API_MESSAGING_WORKFLOW_PROTOCOL_HPP

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
    /**
     * @brief Pre-generated party UUIDs, in input order.
     *
     * Available immediately — UUIDs are generated before workflow dispatch.
     */
    std::vector<std::string> party_ids;
    /**
     * @brief Workflow instance UUIDs, one per party.
     *
     * Each workflow runs asynchronously; use these IDs to query status.
     */
    std::vector<std::string> workflow_instance_ids;
    /**
     * @brief Correlation ID echoed back for client-side logging and support.
     */
    std::string correlation_id;
};

/**
 * @brief Internal per-party workflow request used as workflow_instance.request_json.
 *
 * The provision_parties handler pre-generates the party UUID and bundles all
 * party and account fields into one struct. The step command builders in
 * provision_parties_definitions.hpp deserialize this to construct each step's
 * NATS command payload.
 */
struct provision_party_workflow_request {
    std::string party_id;            ///< Pre-generated UUID for the party.
    std::string full_name;
    std::string short_code;
    std::string party_category;
    std::string party_type;
    std::string business_center_code;
    std::optional<std::string> parent_party_id;
    std::string status;              ///< Initial party status (e.g. "Inactive").
    std::string principal;           ///< IAM account login name (username@hostname).
    std::string password;
    std::string totp_secret;
    std::string email;
    std::string account_type;
};

}

#endif
