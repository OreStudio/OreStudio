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
#ifndef ORES_REPORTING_CORE_SERVICE_EXECUTION_STORAGE_PLAN_HPP
#define ORES_REPORTING_CORE_SERVICE_EXECUTION_STORAGE_PLAN_HPP

#include "ores.reporting.core/export.hpp"
#include <string>

namespace ores::reporting::service {

/**
 * @brief Where one execution's gathered trades land.
 *
 * The key is the one the platform's bucket and key protocol declares, under
 * this component's own service segment, so the reader that fetches it back
 * needs no out-of-band index and no second name for the bucket.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT std::string
trades_storage_key(const std::string& report_instance_id);

/**
 * @brief Where one execution's gathered market data lands.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT std::string
market_data_storage_key(const std::string& report_instance_id);

/**
 * @brief Where one execution's gathered fixings land.
 *
 * Apart from the market data because the engine reads them from a file of
 * their own.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT std::string
fixings_storage_key(const std::string& report_instance_id);

/**
 * @brief Where one execution's retrieved compute output lands.
 *
 * The engine's output is read from compute's own object and kept here, so the
 * instance points at an object this component owns rather than at another
 * component's, and a later reader needs no second name for it.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT std::string
output_storage_key(const std::string& report_instance_id);

}

#endif
