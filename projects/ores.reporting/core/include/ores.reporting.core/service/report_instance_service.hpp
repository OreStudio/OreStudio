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
#ifndef ORES_REPORTING_CORE_SERVICE_REPORT_INSTANCE_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_REPORT_INSTANCE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/report_instance.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/report_instance_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing report instances.
 *
 * Provides a higher-level interface for report instance operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT report_instance_service {
private:
    inline static std::string_view logger_name = "ores.reporting.service.report_instance_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a report_instance_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit report_instance_service(context ctx);

    /**
     * @brief Lists report instances with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of report instances for the requested page.
     */
    std::vector<domain::report_instance> list_instances(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active report instances.
     *
     * @return Total number of active report instances.
     */
    std::uint32_t count_instances();


    /**
     * @brief Retrieves a single report instance as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The report instance at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::report_instance> get_instance_at_version(const std::string& id,
                                                                   std::uint32_t version);

    /**
     * @brief Retrieves a single report instance by its primary key.
     *
     * @return The report instance if found, std::nullopt otherwise.
     */
    std::optional<domain::report_instance> get_instance(const std::string& id);

    /**
     * @brief Saves a report instance (creates or updates).
     *
     * @param instance The report instance to save.
     * @throws std::exception on failure.
     */
    void save_instance(const domain::report_instance& instance);

    /**
     * @brief Saves a batch of report instances.
     *
     * @param instances The report instances to save.
     * @throws std::exception on failure.
     */
    void save_instances(const std::vector<domain::report_instance>& instances);

    /**
     * @brief Deletes a report instance by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_instance(const std::string& id);

    /**
     * @brief Deletes report instances by their primary keys.
     */
    void delete_instances(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a report instance.
     */
    std::vector<domain::report_instance> get_instance_history(const std::string& id);

private:
    context ctx_;
    repository::report_instance_repository repo_;
};

}

#endif
