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
#ifndef ORES_DQ_CORE_SERVICE_BADGE_SEVERITY_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_BADGE_SEVERITY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/badge_severity.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/badge_severity_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing badge severities.
 *
 * Provides a higher-level interface for badge severity operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT badge_severity_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.badge_severity_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a badge_severity_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit badge_severity_service(context ctx);

    /**
     * @brief Lists badge severities with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of badge severities for the requested page.
     */
    std::vector<domain::badge_severity> list_severities(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active badge severities.
     *
     * @return Total number of active badge severities.
     */
    std::uint32_t count_severities();

    /**
     * @brief Retrieves a single badge severity as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param code The code of the badge severity.
     * @param version The version to fetch.
     * @return The badge severity at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::badge_severity> get_severity_at_version(const std::string& code,
                                                                  std::uint32_t version);

    /**
     * @brief Retrieves a single badge severity by its code.
     *
     * @param code The code of the badge severity.
     * @return The badge severity if found, std::nullopt otherwise.
     */
    std::optional<domain::badge_severity> get_severity(const std::string& code);

    /**
     * @brief Saves a badge severity (creates or updates).
     *
     * @param severity The badge severity to save.
     * @throws std::exception on failure.
     */
    void save_severity(const domain::badge_severity& severity);

    /**
     * @brief Saves a batch of badge severities.
     *
     * @param severities The badge severities to save.
     * @throws std::exception on failure.
     */
    void save_severities(const std::vector<domain::badge_severity>& severities);

    /**
     * @brief Deletes a badge severity by its code.
     *
     * @param code The code of the badge severity to delete.
     * @throws std::exception on failure.
     */
    void delete_severity(const std::string& code);

    /**
     * @brief Deletes badge severities by their codes.
     */
    void delete_severities(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a badge severity.
     */
    std::vector<domain::badge_severity> get_severity_history(const std::string& code);

private:
    context ctx_;
    repository::badge_severity_repository repo_;
};

}

#endif
