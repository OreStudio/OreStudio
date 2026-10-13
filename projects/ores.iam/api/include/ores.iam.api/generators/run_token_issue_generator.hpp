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
 * Template: cpp_domain_type_generator.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_IAM_API_GENERATORS_RUN_TOKEN_ISSUE_GENERATOR_HPP
#define ORES_IAM_API_GENERATORS_RUN_TOKEN_ISSUE_GENERATOR_HPP

#include "ores.iam.api/domain/run_token_issue.hpp"
#include "ores.iam.api/export.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include <vector>

namespace ores::iam::generators {

/**
 * @brief Generates a synthetic run_token_issue.
 */
ORES_IAM_API_EXPORT domain::run_token_issue
generate_synthetic_run_token_issue(utility::generation::generation_context& ctx);

/**
 * @brief Generates N synthetic run_token_issues.
 */
ORES_IAM_API_EXPORT std::vector<domain::run_token_issue>
generate_synthetic_run_token_issues(std::size_t n, utility::generation::generation_context& ctx);

}

#endif
