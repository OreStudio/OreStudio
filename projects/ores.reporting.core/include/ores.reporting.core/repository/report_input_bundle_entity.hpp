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
#ifndef ORES_REPORTING_REPOSITORY_REPORT_INPUT_BUNDLE_ENTITY_HPP
#define ORES_REPORTING_REPOSITORY_REPORT_INPUT_BUNDLE_ENTITY_HPP

#include <optional>
#include <string>
#include "sqlgen/Timestamp.hpp"
#include "sqlgen/PrimaryKey.hpp"

namespace ores::reporting::repository {

/**
 * @brief Persists the object-storage references assembled during a report run.
 *
 * Created by the assemble_bundle workflow step once gather_trades and
 * gather_market_data have both succeeded.  Immutable after creation.
 */
struct report_input_bundle_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename =
        "ores_reporting_report_input_bundles_tbl";

    sqlgen::PrimaryKey<std::string> id;
    std::string tenant_id;
    std::string report_instance_id;
    std::string definition_id;
    std::string trades_storage_key;
    std::string market_data_storage_key;
    int trade_count = 0;
    int series_count = 0;
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> created_at;
};

}

#endif
