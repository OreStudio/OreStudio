/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_VARIABILITY_SERVICE_FEATURE_FLAGS_SERVICE_HPP
#define ORES_VARIABILITY_SERVICE_FEATURE_FLAGS_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.variability/domain/feature_flags.hpp"
#include "ores.variability/repository/feature_flags_repository.hpp"

namespace ores::variability::service {

/**
 * @brief Service for managing feature flags.
 *
 * Provides high-level operations for retrieving and modifying feature flags.
 * Encapsulates the underlying repository and handles the bitemporal update logic.
 */
class feature_flags_service {
private:
    inline static std::string_view logger_name =
        "ores.variability.service.feature_flags_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Constructs a feature_flags_service.
     *
     * @param ctx The database context to be used by the repository.
     */
    explicit feature_flags_service(database::context ctx);

    /**
     * @brief Retrieves a single feature flag by name.
     *
     * @param name The name of the feature flag to retrieve.
     * @return An optional containing the feature flag if found, or std::nullopt.
     */
    [[nodiscard]] std::optional<domain::feature_flags> get_feature_flag(
        const std::string& name);

    /**
     * @brief Retrieves all currently active feature flags.
     *
     * @return A vector of all active feature flags.
     */
    [[nodiscard]] std::vector<domain::feature_flags> get_all_feature_flags();

    /**
     * @brief Saves a feature flag.
     *
     * Handles the bitemporal update logic: if the flag already exists,
     * it is logically removed (closed) before the new version is written.
     * This ensures a clean update of the feature flag.
     *
     * @param flag The feature flag to save.
     */
    void save_feature_flag(const domain::feature_flags& flag);

    /**
     * @brief Logically removes a feature flag.
     *
     * Closes the validity period of the feature flag, effectively deleting it
     * from the current view while preserving history.
     *
     * @param name The name of the feature flag to delete.
     */
    void delete_feature_flag(const std::string& name);

    /**
     * @brief Retrieves all historical versions of a feature flag.
     *
     * @param name The name of the feature flag.
     * @return A vector of all versions, ordered newest-first.
     */
    [[nodiscard]] std::vector<domain::feature_flags> get_feature_flag_history(
        const std::string& name);

private:
    repository::feature_flags_repository repo_;
};

}

#endif
