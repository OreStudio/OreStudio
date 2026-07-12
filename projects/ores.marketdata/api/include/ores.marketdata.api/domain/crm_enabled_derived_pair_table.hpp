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
#ifndef ORES_MARKETDATA_API_DOMAIN_CRM_ENABLED_DERIVED_PAIR_TABLE_HPP
#define ORES_MARKETDATA_API_DOMAIN_CRM_ENABLED_DERIVED_PAIR_TABLE_HPP

#include "ores.marketdata.api/domain/crm_enabled_derived_pair.hpp"
#include "ores.marketdata.api/export.hpp"
#include <string>
#include <vector>

namespace ores::marketdata::domain {

/**
 * @brief Converts crm_enabled_derived_pairs to the table format.
 */
ORES_MARKETDATA_API_EXPORT std::string
convert_to_table(const std::vector<crm_enabled_derived_pair>& v);

}

#endif
