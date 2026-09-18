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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_service.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_MARKETDATA_CORE_SERVICE_SERIES_CLASSIFICATION_RULE_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_SERIES_CLASSIFICATION_RULE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/series_classification_rule.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/series_classification_rule_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Service for managing series classification rules.
 *
 * Provides a higher-level interface for series classification rule operations,
 * wrapping the underlying repository.
 */
class ORES_MARKETDATA_CORE_EXPORT series_classification_rule_service {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.service.series_classification_rule_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a series_classification_rule_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit series_classification_rule_service(context ctx);

    /**
     * @brief Lists series classification rules with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of series classification rules for the requested page.
     */
    std::vector<domain::series_classification_rule> list_rules(std::uint32_t offset,
                                                               std::uint32_t limit);

    /**
     * @brief Gets the total count of active series classification rules.
     *
     * @return Total number of active series classification rules.
     */
    std::uint32_t count_rules();


    /**
     * @brief Retrieves a single series classification rule as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The series classification rule at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::series_classification_rule> get_rule_at_version(
        const std::string& series_type, const std::string& metric, std::uint32_t version);

    /**
     * @brief Retrieves a single series classification rule by its primary key.
     *
     * @return The series classification rule if found, std::nullopt otherwise.
     */
    std::optional<domain::series_classification_rule> get_rule(const std::string& series_type,
                                                               const std::string& metric);

    /**
     * @brief Saves a series classification rule (creates or updates).
     *
     * @param rule The series classification rule to save.
     * @throws std::exception on failure.
     */
    void save_rule(const domain::series_classification_rule& rule);

    /**
     * @brief Saves a batch of series classification rules.
     *
     * @param rules The series classification rules to save.
     * @throws std::exception on failure.
     */
    void save_rules(const std::vector<domain::series_classification_rule>& rules);

    /**
     * @brief Deletes a series classification rule by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_rule(const std::string& series_type, const std::string& metric);

    /**
     * @brief Deletes series classification rules by their primary keys.
     */
    void delete_rules(const std::vector<std::string>& series_types,
                      const std::vector<std::string>& metrics);

    /**
     * @brief Retrieves all historical versions of a series classification rule.
     */
    std::vector<domain::series_classification_rule> get_rule_history(const std::string& series_type,
                                                                     const std::string& metric);

private:
    context ctx_;
    repository::series_classification_rule_repository repo_;
};

}

#endif
