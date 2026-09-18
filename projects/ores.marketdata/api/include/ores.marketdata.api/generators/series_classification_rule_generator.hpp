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
#ifndef ORES_MARKETDATA_API_GENERATORS_SERIES_CLASSIFICATION_RULE_GENERATOR_HPP
#define ORES_MARKETDATA_API_GENERATORS_SERIES_CLASSIFICATION_RULE_GENERATOR_HPP

#include "ores.marketdata.api/domain/series_classification_rule.hpp"
#include "ores.marketdata.api/export.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include <vector>

namespace ores::marketdata::generators {

/**
 * @brief Generates a synthetic series_classification_rule.
 */
ORES_MARKETDATA_API_EXPORT domain::series_classification_rule
generate_synthetic_series_classification_rule(utility::generation::generation_context& ctx);

/**
 * @brief Generates N synthetic series_classification_rules.
 */
ORES_MARKETDATA_API_EXPORT std::vector<domain::series_classification_rule>
generate_synthetic_series_classification_rules(std::size_t n,
                                               utility::generation::generation_context& ctx);

}

#endif
