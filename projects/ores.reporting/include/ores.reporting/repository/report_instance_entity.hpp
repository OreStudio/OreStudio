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
#ifndef ORES_REPORTING_REPOSITORY_REPORT_INSTANCE_ENTITY_HPP
#define ORES_REPORTING_REPOSITORY_REPORT_INSTANCE_ENTITY_HPP

#include <cstdint>
#include <string>
#include <optional>
#include <ostream>
#include "sqlgen/Timestamp.hpp"
#include "sqlgen/PrimaryKey.hpp"

namespace ores::reporting::repository {

/**
 * @brief Represents a report instance in the database.
 */
struct report_instance_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_reporting_report_instances_tbl";

    sqlgen::PrimaryKey<std::string> id;
    std::string tenant_id;
    int version = 0;
    std::string party_id;
    std::string definition_id;
    std::optional<std::string> fsm_state_id;
    std::int64_t trigger_run_id;
    std::string output_message;
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> started_at;
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> completed_at;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> valid_from = "9999-12-31 23:59:59";
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const report_instance_entity& v);

}

#endif
