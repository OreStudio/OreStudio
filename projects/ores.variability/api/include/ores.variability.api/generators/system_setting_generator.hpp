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
#ifndef ORES_VARIABILITY_API_GENERATORS_SYSTEM_SETTING_GENERATOR_HPP
#define ORES_VARIABILITY_API_GENERATORS_SYSTEM_SETTING_GENERATOR_HPP

#include "ores.utility/generation/generation_context.hpp"
#include "ores.variability.api/domain/system_setting.hpp"
#include "ores.variability.api/export.hpp"
#include <vector>

namespace ores::variability::generators {

/**
 * @brief Generates a synthetic system_setting.
 */
ORES_VARIABILITY_API_EXPORT domain::system_setting
generate_synthetic_system_setting(utility::generation::generation_context& ctx);

/**
 * @brief Generates N synthetic system_settings.
 */
ORES_VARIABILITY_API_EXPORT std::vector<domain::system_setting>
generate_synthetic_system_settings(std::size_t n, utility::generation::generation_context& ctx);

}

#endif
