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
#include <string_view>

namespace ores::reporting::service {

/**
 * @brief The bucket every report execution writes its gathered data to.
 *
 * The storage server refuses a bucket it does not know with a 404, so this name
 * is a contract with that server rather than a local choice. It is stated once
 * here; the ore service names the same bucket for the packaging step, and the
 * two spellings have to agree.
 */
inline constexpr std::string_view report_data_bucket = "report-data";

/**
 * @brief Where one execution's gathered trades land.
 *
 * The key is namespaced by the instance, so two executions never collide and a
 * key names the run it belongs to.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT std::string
trades_storage_key(const std::string& report_instance_id);

/**
 * @brief Where one execution's gathered market data lands.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT std::string
market_data_storage_key(const std::string& report_instance_id);

}

#endif
