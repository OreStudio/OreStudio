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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_CCY_PAIR_INPUT_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_CCY_PAIR_INPUT_HPP

#include <string>

namespace ores::analytics::quant::domain {

/**
 * @brief Raw, caller-supplied input to @c topology_builder::build.
 *
 * Currency codes are plain strings here -- the caller adapts whatever
 * refdata/OpenSourceRisk currency representation it has at this boundary.
 * @c topology_builder assigns each unique code a compact @c currency_id
 * once the topology is built.
 */
struct ccy_pair_input {
    std::string base_code;
    std::string quote_code;
    bool is_driver;
};

} // namespace ores::analytics::quant::domain

#endif
