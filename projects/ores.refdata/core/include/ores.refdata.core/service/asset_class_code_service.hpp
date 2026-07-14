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
#ifndef ORES_REFDATA_CORE_SERVICE_ASSET_CLASS_CODE_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_ASSET_CLASS_CODE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/asset_class_code.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/asset_class_code_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing asset class codes.
 *
 * Provides a higher-level interface for asset class code operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT asset_class_code_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.asset_class_code_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a asset_class_code_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit asset_class_code_service(context ctx);

    /**
     * @brief Lists asset class codes with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of asset class codes for the requested page.
     */
    std::vector<domain::asset_class_code> list_asset_classes(std::uint32_t offset,
                                                             std::uint32_t limit);

    /**
     * @brief Gets the total count of active asset class codes.
     *
     * @return Total number of active asset class codes.
     */
    std::uint32_t count_asset_classes();

    /**
     * @brief Retrieves a single asset class code as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param code The code of the asset class code.
     * @param version The version to fetch.
     * @return The asset class code at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::asset_class_code> get_asset_class_at_version(const std::string& code,
                                                                       std::uint32_t version);

    /**
     * @brief Retrieves a single asset class code by its code.
     *
     * @param code The code of the asset class code.
     * @return The asset class code if found, std::nullopt otherwise.
     */
    std::optional<domain::asset_class_code> get_asset_class(const std::string& code);

    /**
     * @brief Saves a asset class code (creates or updates).
     *
     * @param asset_class The asset class code to save.
     * @throws std::exception on failure.
     */
    void save_asset_class(const domain::asset_class_code& asset_class);

    /**
     * @brief Saves a batch of asset class codes.
     *
     * @param asset_classes The asset class codes to save.
     * @throws std::exception on failure.
     */
    void save_asset_classes(const std::vector<domain::asset_class_code>& asset_classes);

    /**
     * @brief Deletes a asset class code by its code.
     *
     * @param code The code of the asset class code to delete.
     * @throws std::exception on failure.
     */
    void delete_asset_class(const std::string& code);

    /**
     * @brief Deletes asset class codes by their codes.
     */
    void delete_asset_classes(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a asset class code.
     */
    std::vector<domain::asset_class_code> get_asset_class_history(const std::string& code);

private:
    context ctx_;
    repository::asset_class_code_repository repo_;
};

}

#endif
