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
 * Template: cpp_domain_type_repository.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_SERIES_CLASSIFICATION_RULE_REPOSITORY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_SERIES_CLASSIFICATION_RULE_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/series_classification_rule.hpp"
#include "ores.marketdata.core/export.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::marketdata::repository {

/**
 * @brief Reads and writes series classification rules to data storage.
 */
class ORES_MARKETDATA_CORE_EXPORT series_classification_rule_repository {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.repository.series_classification_rule_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes series classification rules to database.
     */
    /**@{*/
    void write(context ctx, const domain::series_classification_rule& v);
    void write(context ctx, const std::vector<domain::series_classification_rule>& v);
    /**@}*/

    /**
     * @brief Reads latest series classification rules, possibly filtered by primary key.
     */
    /**@{*/
    std::vector<domain::series_classification_rule> read_latest(context ctx);
    std::vector<domain::series_classification_rule>
    read_latest(context ctx, const std::string& series_type, const std::string& metric);
    /**@}*/

    /**
     * @brief Reads all series classification rules, possibly filtered by primary key.
     */
    std::vector<domain::series_classification_rule>
    read_all(context ctx, const std::string& series_type, const std::string& metric);

    /**
     * @brief Reads a single series classification rule as it stood at a specific
     * version — the version's own [valid_from, valid_to) window is returned
     * verbatim, so the caller can compose child entities "as of" the same
     * window. See the "Temporal composite entity versioning" architecture
     * doc.
     * @param ctx Repository context with database connection
     * @param version The version to fetch
     */
    std::optional<domain::series_classification_rule>
    read_at_version(context ctx,
                    const std::string& series_type,
                    const std::string& metric,
                    std::uint32_t version);

    /**
     * @brief Reads latest series classification rules with pagination support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::series_classification_rule>
    read_latest(context ctx, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active series classification rules.
     * @param ctx Repository context with database connection
     * @return Total number of active series classification rules
     */
    std::uint32_t get_total_rule_count(context ctx);

    /**
     * @brief Deletes a series classification rule by closing its temporal validity.
     */
    void remove(context ctx, const std::string& series_type, const std::string& metric);

    /**
     * @brief Deletes series classification rules by closing their temporal validity.
     */
    void remove(context ctx,
                const std::vector<std::string>& series_types,
                const std::vector<std::string>& metrics);
};

}

#endif
