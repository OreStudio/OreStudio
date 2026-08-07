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
#ifndef ORES_REPORTING_CORE_SERVICE_REPORT_TYPE_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_REPORT_TYPE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/report_type.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/report_type_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing report types.
 *
 * Provides a higher-level interface for report type operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT report_type_service {
private:
    inline static std::string_view logger_name = "ores.reporting.service.report_type_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a report_type_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit report_type_service(context ctx);

    /**
     * @brief Lists report types with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of report types for the requested page.
     */
    std::vector<domain::report_type> list_types(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active report types.
     *
     * @return Total number of active report types.
     */
    std::uint32_t count_types();


    /**
     * @brief Retrieves a single report type as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The report type at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::report_type> get_type_at_version(const std::string& code,
                                                           std::uint32_t version);

    /**
     * @brief Retrieves a single report type by its primary key.
     *
     * @return The report type if found, std::nullopt otherwise.
     */
    std::optional<domain::report_type> get_type(const std::string& code);

    /**
     * @brief Saves a report type (creates or updates).
     *
     * @param type The report type to save.
     * @throws std::exception on failure.
     */
    void save_type(const domain::report_type& type);

    /**
     * @brief Saves a batch of report types.
     *
     * @param types The report types to save.
     * @throws std::exception on failure.
     */
    void save_types(const std::vector<domain::report_type>& types);

    /**
     * @brief Deletes a report type by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_type(const std::string& code);

    /**
     * @brief Deletes report types by their primary keys.
     */
    void delete_types(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a report type.
     */
    std::vector<domain::report_type> get_type_history(const std::string& code);

private:
    context ctx_;
    repository::report_type_repository repo_;
};

}

#endif
