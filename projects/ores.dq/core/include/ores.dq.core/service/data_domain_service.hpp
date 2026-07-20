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
#ifndef ORES_DQ_CORE_SERVICE_DATA_DOMAIN_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_DATA_DOMAIN_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/data_domain.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/data_domain_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing data domains.
 *
 * Provides a higher-level interface for data domain operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT data_domain_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.data_domain_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a data_domain_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit data_domain_service(context ctx);

    /**
     * @brief Lists data domains with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of data domains for the requested page.
     */
    std::vector<domain::data_domain> list_domains(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active data domains.
     *
     * @return Total number of active data domains.
     */
    std::uint32_t count_domains();

    /**
     * @brief Retrieves a single data domain as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param name The name of the data domain.
     * @param version The version to fetch.
     * @return The data domain at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::data_domain> get_domain_at_version(const std::string& name,
                                                             std::uint32_t version);

    /**
     * @brief Retrieves a single data domain by its name.
     *
     * @param name The name of the data domain.
     * @return The data domain if found, std::nullopt otherwise.
     */
    std::optional<domain::data_domain> get_domain(const std::string& name);

    /**
     * @brief Saves a data domain (creates or updates).
     *
     * @param domain The data domain to save.
     * @throws std::exception on failure.
     */
    void save_domain(const domain::data_domain& domain);

    /**
     * @brief Saves a batch of data domains.
     *
     * @param domains The data domains to save.
     * @throws std::exception on failure.
     */
    void save_domains(const std::vector<domain::data_domain>& domains);

    /**
     * @brief Deletes a data domain by its name.
     *
     * @param name The name of the data domain to delete.
     * @throws std::exception on failure.
     */
    void delete_domain(const std::string& name);

    /**
     * @brief Deletes data domains by their names.
     */
    void delete_domains(const std::vector<std::string>& names);

    /**
     * @brief Retrieves all historical versions of a data domain.
     */
    std::vector<domain::data_domain> get_domain_history(const std::string& name);

private:
    context ctx_;
    repository::data_domain_repository repo_;
};

}

#endif
