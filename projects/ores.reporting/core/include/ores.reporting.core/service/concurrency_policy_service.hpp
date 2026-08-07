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
#ifndef ORES_REPORTING_CORE_SERVICE_CONCURRENCY_POLICY_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_CONCURRENCY_POLICY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/concurrency_policy.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/concurrency_policy_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing concurrency policies.
 *
 * Provides a higher-level interface for concurrency policy operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT concurrency_policy_service {
private:
    inline static std::string_view logger_name =
        "ores.reporting.service.concurrency_policy_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a concurrency_policy_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit concurrency_policy_service(context ctx);

    /**
     * @brief Lists concurrency policies with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of concurrency policies for the requested page.
     */
    std::vector<domain::concurrency_policy> list_policies(std::uint32_t offset,
                                                          std::uint32_t limit);

    /**
     * @brief Gets the total count of active concurrency policies.
     *
     * @return Total number of active concurrency policies.
     */
    std::uint32_t count_policies();


    /**
     * @brief Retrieves a single concurrency policy as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The concurrency policy at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::concurrency_policy> get_policy_at_version(const std::string& code,
                                                                    std::uint32_t version);

    /**
     * @brief Retrieves a single concurrency policy by its primary key.
     *
     * @return The concurrency policy if found, std::nullopt otherwise.
     */
    std::optional<domain::concurrency_policy> get_policy(const std::string& code);

    /**
     * @brief Saves a concurrency policy (creates or updates).
     *
     * @param policy The concurrency policy to save.
     * @throws std::exception on failure.
     */
    void save_policy(const domain::concurrency_policy& policy);

    /**
     * @brief Saves a batch of concurrency policies.
     *
     * @param policies The concurrency policies to save.
     * @throws std::exception on failure.
     */
    void save_policies(const std::vector<domain::concurrency_policy>& policies);

    /**
     * @brief Deletes a concurrency policy by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_policy(const std::string& code);

    /**
     * @brief Deletes concurrency policies by their primary keys.
     */
    void delete_policies(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a concurrency policy.
     */
    std::vector<domain::concurrency_policy> get_policy_history(const std::string& code);

private:
    context ctx_;
    repository::concurrency_policy_repository repo_;
};

}

#endif
