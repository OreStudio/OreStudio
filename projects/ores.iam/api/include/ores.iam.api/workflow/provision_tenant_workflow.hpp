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
#ifndef ORES_IAM_API_WORKFLOW_PROVISION_TENANT_WORKFLOW_HPP
#define ORES_IAM_API_WORKFLOW_PROVISION_TENANT_WORKFLOW_HPP

#include <string>
#include <string_view>
#include <vector>

namespace ores::iam::workflow {

/// The workflow type =iam.v1.tenants.provision= starts. The definition that
/// registers itself under this name builds one engine step per declared kind.
inline constexpr std::string_view provision_tenant_workflow_type = "provision_tenant_workflow";

/**
 * @brief One step the run takes, as the seed profile declared it.
 */
struct provision_tenant_step {
    /// The step kind, from the catalogue in code.
    std::string kind;
    /// The kind's own arguments, as the profile's row states them.
    std::string arguments_json;
};

/**
 * @brief One parameter value the run's steps read, by declared name.
 */
struct provision_tenant_parameter {
    std::string name;
    std::string value;
};

/**
 * @brief What a provision tenant run works from.
 *
 * Serialised as @c request_json in a @c start_workflow_message. The request
 * handler reads the profile, its declared parameters and its ordered steps
 * before it starts the run, so the definition that builds the engine steps
 * needs no database of its own and the run follows the profile as it stood
 * when the run started rather than as it stands when a step is dispatched.
 */
struct provision_tenant_workflow_request {
    std::string profile_code;
    std::string tenant_code;
    std::string tenant_hostname;
    /// The profile's declared parameters, each with the value it took.
    std::vector<provision_tenant_parameter> parameters;
    /// The profile's ordered steps.
    std::vector<provision_tenant_step> steps;
};

}

#endif
