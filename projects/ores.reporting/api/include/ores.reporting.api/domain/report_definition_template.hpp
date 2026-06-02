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
#ifndef ORES_REPORTING_DOMAIN_REPORT_DEFINITION_TEMPLATE_HPP
#define ORES_REPORTING_DOMAIN_REPORT_DEFINITION_TEMPLATE_HPP

#include <string>

namespace ores::reporting::domain {

/**
 * @brief A candidate report definition sourced from the DQ artefact table.
 *
 * Represents a row from ores_dq_report_definitions_artefact_tbl — a
 * pre-seeded template that the provisioning wizard offers to the user.
 * Unlike report_definition, this type carries no party, tenant, FSM state,
 * or scheduling infrastructure; it is read-only seed data.
 */
struct report_definition_template final {
    /**
     * @brief Human-readable name for the report definition.
     */
    std::string name;

    /**
     * @brief Optional description of the report's purpose.
     */
    std::string description;

    /**
     * @brief Report type code (e.g. "risk").
     */
    std::string report_type;

    /**
     * @brief Cron expression for report scheduling (e.g. "0 6 * * 1-5").
     */
    std::string schedule_expression;

    /**
     * @brief Concurrency policy code (e.g. "skip").
     */
    std::string concurrency_policy;

    /**
     * @brief Display ordering hint for UI presentation.
     */
    int display_order = 0;
};

}

#endif
