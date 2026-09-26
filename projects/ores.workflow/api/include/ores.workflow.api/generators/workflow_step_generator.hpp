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
#ifndef ORES_WORKFLOW_API_GENERATORS_WORKFLOW_STEP_GENERATOR_HPP
#define ORES_WORKFLOW_API_GENERATORS_WORKFLOW_STEP_GENERATOR_HPP

#include "ores.utility/generation/generation_context.hpp"
#include "ores.workflow.api/domain/workflow_step.hpp"
#include "ores.workflow.api/export.hpp"
#include <vector>

namespace ores::workflow::generators {

/**
 * @brief Generates a synthetic workflow_step.
 */
ORES_WORKFLOW_API_EXPORT domain::workflow_step
generate_synthetic_workflow_step(utility::generation::generation_context& ctx);

/**
 * @brief Generates N synthetic workflow_steps.
 */
ORES_WORKFLOW_API_EXPORT std::vector<domain::workflow_step>
generate_synthetic_workflow_steps(std::size_t n, utility::generation::generation_context& ctx);

}

#endif
