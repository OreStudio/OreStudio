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
#ifndef ORES_SYNTHETIC_CORE_SERVICE_GMM_COMPONENT_SERVICE_HPP
#define ORES_SYNTHETIC_CORE_SERVICE_GMM_COMPONENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.synthetic.api/domain/gmm_component.hpp"
#include "ores.synthetic.core/export.hpp"
#include "ores.synthetic.core/repository/gmm_component_repository.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Service for managing GMM components.
 *
 * Provides a higher-level interface for GMM component operations,
 * wrapping the underlying repository.
 */
class ORES_SYNTHETIC_CORE_EXPORT gmm_component_service {
private:
    inline static std::string_view logger_name = "ores.synthetic.service.gmm_component_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a gmm_component_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit gmm_component_service(context ctx);

    /**
     * @brief Lists GMM components with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of GMM components for the requested page.
     */
    std::vector<domain::gmm_component> list_gmm_components(std::uint32_t offset,
                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active GMM components.
     *
     * @return Total number of active GMM components.
     */
    std::uint32_t count_gmm_components();

    /**
     * @brief Retrieves a single GMM component by its id.
     *
     * @param id The id of the GMM component.
     * @return The GMM component if found, std::nullopt otherwise.
     */
    std::optional<domain::gmm_component> get_gmm_component(const std::string& id);

    /**
     * @brief Saves a GMM component (creates or updates).
     *
     * @param gmm_component The GMM component to save.
     * @throws std::exception on failure.
     */
    void save_gmm_component(const domain::gmm_component& gmm_component);

    /**
     * @brief Saves a batch of GMM components.
     *
     * @param gmm_components The GMM components to save.
     * @throws std::exception on failure.
     */
    void save_gmm_components(const std::vector<domain::gmm_component>& gmm_components);

    /**
     * @brief Deletes a GMM component by its id.
     *
     * @param id The id of the GMM component to delete.
     * @throws std::exception on failure.
     */
    void delete_gmm_component(const std::string& id);

    /**
     * @brief Deletes GMM components by their ids.
     */
    void delete_gmm_components(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a GMM component.
     */
    std::vector<domain::gmm_component> get_gmm_component_history(const std::string& id);

private:
    context ctx_;
    repository::gmm_component_repository repo_;
};

}

#endif
