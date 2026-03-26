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
#ifndef ORES_REPORTING_REPOSITORY_REPORT_DEFINITION_REPOSITORY_HPP
#define ORES_REPORTING_REPOSITORY_REPORT_DEFINITION_REPOSITORY_HPP

#include <string>
#include <vector>
#include <sqlgen/postgres.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.reporting.api/domain/report_definition.hpp"

namespace ores::reporting::repository {

/**
 * @brief Reads and writes report definitions to data storage.
 */
class report_definition_repository {
private:
    inline static std::string_view logger_name =
        "ores.reporting.repository.report_definition_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    std::string sql();

    void write(context ctx, const domain::report_definition& v);
    void write(context ctx, const std::vector<domain::report_definition>& v);

    std::vector<domain::report_definition> read_latest(context ctx);
    std::vector<domain::report_definition>
    read_latest(context ctx, const std::string& id);
    std::vector<domain::report_definition>
    read_all(context ctx, const std::string& id);

    /**
     * @brief Returns all current report definitions that have no scheduler_job_id,
     *        across all tenants. Used during startup reconciliation.
     */
    std::vector<domain::report_definition> read_all_unscheduled(context ctx);

    void remove(context ctx, const std::string& id);
};

}

#endif
