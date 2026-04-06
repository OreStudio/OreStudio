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
#ifndef ORES_REPORTING_REPOSITORY_RISK_REPORT_CONFIG_REPOSITORY_HPP
#define ORES_REPORTING_REPOSITORY_RISK_REPORT_CONFIG_REPOSITORY_HPP

#include <optional>
#include <string>
#include <vector>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.reporting.api/domain/risk_report_config.hpp"

namespace ores::reporting::repository {

/**
 * @brief Reads risk_report_config records from the database.
 *
 * Provides read-only access to the risk_report_config table and the
 * associated book/portfolio scope junction tables.
 */
class risk_report_config_repository {
private:
    inline static std::string_view logger_name =
        "ores.reporting.repository.risk_report_config_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Finds the current risk_report_config for a report definition.
     *
     * @return The config if found, nullopt otherwise.
     */
    std::optional<domain::risk_report_config>
    find_by_definition_id(context ctx, const std::string& definition_id);

    /**
     * @brief Resolves the full set of book UUIDs in scope for a config.
     *
     * Calls ores_reporting_resolve_book_ids_for_config_fn which checks
     * explicit book scope, then portfolio scope (with subtree expansion),
     * then falls back to all tenant books.
     */
    std::vector<std::string>
    resolve_book_ids(context ctx, const std::string& config_id);

    /**
     * @brief Returns the active book UUIDs scoped to a risk_report_config.
     *
     * Empty vector means "all books within the selected portfolios".
     */
    std::vector<std::string>
    get_book_scope(context ctx, const std::string& config_id);

    /**
     * @brief Returns the active portfolio UUIDs scoped to a risk_report_config.
     *
     * Empty vector means "all portfolios visible to the tenant".
     */
    std::vector<std::string>
    get_portfolio_scope(context ctx, const std::string& config_id);
};

}

#endif
