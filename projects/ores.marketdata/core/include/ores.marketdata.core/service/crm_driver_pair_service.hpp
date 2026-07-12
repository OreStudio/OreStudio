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
#ifndef ORES_MARKETDATA_CORE_SERVICE_CRM_DRIVER_PAIR_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_CRM_DRIVER_PAIR_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/crm_driver_pair.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/crm_driver_pair_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Service for managing CRM driver pairs.
 *
 * Provides a higher-level interface for CRM driver pair operations,
 * wrapping the underlying repository.
 */
class ORES_MARKETDATA_CORE_EXPORT crm_driver_pair_service {
private:
    inline static std::string_view logger_name = "ores.marketdata.service.crm_driver_pair_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a crm_driver_pair_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit crm_driver_pair_service(context ctx);

    /**
     * @brief Lists CRM driver pairs with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of CRM driver pairs for the requested page.
     */
    std::vector<domain::crm_driver_pair> list_crm_driver_pairs(std::uint32_t offset,
                                                               std::uint32_t limit);

    /**
     * @brief Gets the total count of active CRM driver pairs.
     *
     * @return Total number of active CRM driver pairs.
     */
    std::uint32_t count_crm_driver_pairs();

    /**
     * @brief Retrieves a single CRM driver pair as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param id The id of the CRM driver pair.
     * @param version The version to fetch.
     * @return The CRM driver pair at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::crm_driver_pair> get_crm_driver_pair_at_version(const std::string& id,
                                                                          std::uint32_t version);

    /**
     * @brief Retrieves a single CRM driver pair by its id.
     *
     * @param id The id of the CRM driver pair.
     * @return The CRM driver pair if found, std::nullopt otherwise.
     */
    std::optional<domain::crm_driver_pair> get_crm_driver_pair(const std::string& id);

    /**
     * @brief Saves a CRM driver pair (creates or updates).
     *
     * @param crm_driver_pair The CRM driver pair to save.
     * @throws std::exception on failure.
     */
    void save_crm_driver_pair(const domain::crm_driver_pair& crm_driver_pair);

    /**
     * @brief Saves a batch of CRM driver pairs.
     *
     * @param crm_driver_pairs The CRM driver pairs to save.
     * @throws std::exception on failure.
     */
    void save_crm_driver_pairs(const std::vector<domain::crm_driver_pair>& crm_driver_pairs);

    /**
     * @brief Deletes a CRM driver pair by its id.
     *
     * @param id The id of the CRM driver pair to delete.
     * @throws std::exception on failure.
     */
    void delete_crm_driver_pair(const std::string& id);

    /**
     * @brief Deletes CRM driver pairs by their ids.
     */
    void delete_crm_driver_pairs(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a CRM driver pair.
     */
    std::vector<domain::crm_driver_pair> get_crm_driver_pair_history(const std::string& id);

private:
    context ctx_;
    repository::crm_driver_pair_repository repo_;
};

}

#endif
