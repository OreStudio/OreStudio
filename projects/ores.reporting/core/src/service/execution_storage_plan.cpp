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
#include "ores.reporting.core/service/execution_storage_plan.hpp"
#include "ores.storage.api/net/object_keys.hpp"

namespace ores::reporting::service {

namespace {

/// The service segment: this component owns every key under it.
constexpr std::string_view service_segment = "reporting";

/// The purpose segment: one run's gathered inputs.
constexpr std::string_view runs_segment = "runs";

}

std::string trades_storage_key(const std::string& report_instance_id) {
    return ores::storage::api::object_keys::make(
        service_segment, runs_segment, report_instance_id, "trades.msgpack");
}

std::string market_data_storage_key(const std::string& report_instance_id) {
    return ores::storage::api::object_keys::make(
        service_segment, runs_segment, report_instance_id, "market_data.txt");
}

std::string fixings_storage_key(const std::string& report_instance_id) {
    return ores::storage::api::object_keys::make(
        service_segment, runs_segment, report_instance_id, "fixings.txt");
}

}
