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
#ifndef ORES_REPORTING_CORE_SERVICE_RUN_REQUIREMENTS_HPP
#define ORES_REPORTING_CORE_SERVICE_RUN_REQUIREMENTS_HPP

#include "ores.reporting.core/export.hpp"
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief The configuration types a report type requires that a definition
 * does not bind.
 *
 * A run reads every configuration its report type requires, so a definition
 * that lacks one fails part-way through the workflow, after an instance exists.
 * Asking before the instance is created refuses the run while the reason is
 * still the definition's. The answer is sorted and holds each code once, so a
 * message built from it reads the same whatever order the rows came in.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT std::vector<std::string>
missing_configuration_types(const std::vector<std::string>& required,
                            const std::vector<std::string>& bound);

}

#endif
