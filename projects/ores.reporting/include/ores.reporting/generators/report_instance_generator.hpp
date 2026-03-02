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
#ifndef ORES_REPORTING_GENERATORS_REPORT_INSTANCE_GENERATOR_HPP
#define ORES_REPORTING_GENERATORS_REPORT_INSTANCE_GENERATOR_HPP

#include <vector>
#include "ores.reporting/domain/report_instance.hpp"
#include "ores.utility/generation/generation_context.hpp"

namespace ores::reporting::generators {

/**
 * @brief Generates a synthetic report_instance.
 */
domain::report_instance generate_synthetic_report_instance(
    utility::generation::generation_context& ctx);

/**
 * @brief Generates N synthetic report_instances.
 */
std::vector<domain::report_instance>
generate_synthetic_report_instances(std::size_t n,
    utility::generation::generation_context& ctx);

}

#endif
