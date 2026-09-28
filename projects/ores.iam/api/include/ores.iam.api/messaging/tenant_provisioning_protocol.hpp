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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_IAM_API_MESSAGING_TENANT_PROVISIONING_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_TENANT_PROVISIONING_PROTOCOL_HPP

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

namespace ores::iam::messaging {

struct complete_tenant_provisioning_command {
    using response_type = struct complete_tenant_provisioning_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants.complete-provisioning";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

struct complete_tenant_provisioning_response {
    bool success = false;
    std::string message;
};

/**
 * @brief Provisions a tenant from a seed profile.
 *
 * The tenant and its administrator are created before the answer; the steps
 * the profile orders run afterwards as a workflow instance, whose id the
 * answer carries. This is the one verb for every tenant, the first one
 * included, and it replaces iam.v1.bootstrap.provision-tenant and
 * iam.v1.tenants.provision-acme.
 */
struct provision_tenant_command {
    using response_type = struct provision_tenant_command_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants.provision";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief Code of the seed profile to provision from.
     *
     * The profile states the tenant's type, the steps to run and the parameters
     * this request must supply. An unknown code is refused.
     */
    std::string profile_code;
    /**
     * @brief Unique code of the tenant to create.
     */
    std::string tenant_code;
    /**
     * @brief Display name of the tenant.
     */
    std::string tenant_name;
    /**
     * @brief Hostname of the tenant.
     *
     * A principal of the form username@hostname resolves its tenant through this
     * value.
     */
    std::string tenant_hostname;
    /**
     * @brief Optional description for the tenant record.
     */
    std::string tenant_description;
    /**
     * @brief Username of the tenant's first administrator, without a hostname
     * part.
     */
    std::string admin_username;
    /**
     * @brief Email address of that administrator.
     */
    std::string admin_email;
    /**
     * @brief Initial password of that administrator.
     *
     * A client that reuses the caller's own password, as a demonstration
     * profile's form does, sends it here: a server cannot read the password its
     * caller signed in with.
     */
    std::string admin_password;
    /**
     * @brief Values for the profile's declared parameters, one "name=value" entry
     * each.
     *
     * A parameter the profile does not declare is refused, and so is a value the
     * parameter's data type or its choices refuse. A parameter this list omits
     * takes the profile's default; one that is required, omitted and declares no
     * default is refused, and so is a required parameter whose value is empty.
     */
    std::vector<std::string> parameters;
};

/**
 * @brief The answer to a provision request.
 *
 * The name carries the verb because @c iam.v1.bootstrap.provision-tenant still
 * declares a @c provision_tenant_response of its own, and both verbs answer
 * until the shell's provisioning moves to this one.
 */
struct provision_tenant_command_response {
    bool success = false;
    std::string message;
    /**
     * @brief Id of the workflow instance that runs the profile's steps.
     *
     * The caller follows the run, its progress and its retries by this id.
     */
    std::string instance_id;
    /**
     * @brief Id of the tenant the request created.
     */
    std::string tenant_id;
    /**
     * @brief Id of the administrator account it created.
     */
    std::string account_id;
};

// --- Acme one-click tenant provisioning (--source acme) ---
//
// A single server-side orchestrated request: imports the four-party Acme
// Bank LEI hierarchy, publishes real GLEIF counterparties (small), then
// for each operating company publishes its business units, portfolios,
// books, accounts, and account contact informations. No repeated
// per-party logins, no orchestration logic client-side -- driven by
// internal actor impersonation through the real handler pipeline, see
// ores.iam.core/messaging/tenant_provisioning_handler.hpp's provision_acme.
//
// The answer takes minutes, not the seconds the transport allows by
// default: the handler waits up to 1500 seconds on the base bundle it
// publishes. The budget this message declares is 1800 seconds --
// deliberately larger than that wait, so the caller keeps waiting while
// the handler is still working. At the transport default the caller gives
// up first and reports a timeout for a run that had not finished.
struct provision_acme_tenant_command {
    using response_type = struct provision_acme_tenant_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants.provision-acme";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

struct provision_acme_tenant_step {
    std::string step;
    std::string action;
    std::uint64_t record_count = 0;
};

struct provision_acme_tenant_response {
    bool success = false;
    std::string message;
    std::vector<provision_acme_tenant_step> steps;
};

}

#endif
