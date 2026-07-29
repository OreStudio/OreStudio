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
#ifndef ORES_REFDATA_CORE_SERVICE_CDS_CONVENTION_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CDS_CONVENTION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/cds_convention.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/cds_convention_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing CDS conventions.
 *
 * Provides a higher-level interface for CDS convention operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT cds_convention_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.cds_convention_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a cds_convention_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit cds_convention_service(context ctx);

    /**
     * @brief Lists CDS conventions with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of CDS conventions for the requested page.
     */
    std::vector<domain::cds_convention> list_cds_conventions(std::uint32_t offset,
                                                             std::uint32_t limit);

    /**
     * @brief Gets the total count of active CDS conventions.
     *
     * @return Total number of active CDS conventions.
     */
    std::uint32_t count_cds_conventions();


    /**
     * @brief Retrieves a single CDS convention as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The CDS convention at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::cds_convention> get_cds_convention_at_version(const std::string& id,
                                                                        std::uint32_t version);

    /**
     * @brief Retrieves a single CDS convention by its primary key.
     *
     * @return The CDS convention if found, std::nullopt otherwise.
     */
    std::optional<domain::cds_convention> get_cds_convention(const std::string& id);

    /**
     * @brief Saves a CDS convention (creates or updates).
     *
     * @param cds_convention The CDS convention to save.
     * @throws std::exception on failure.
     */
    void save_cds_convention(const domain::cds_convention& cds_convention);

    /**
     * @brief Saves a batch of CDS conventions.
     *
     * @param cds_conventions The CDS conventions to save.
     * @throws std::exception on failure.
     */
    void save_cds_conventions(const std::vector<domain::cds_convention>& cds_conventions);

    /**
     * @brief Deletes a CDS convention by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_cds_convention(const std::string& id);

    /**
     * @brief Deletes CDS conventions by their primary keys.
     */
    void delete_cds_conventions(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a CDS convention.
     */
    std::vector<domain::cds_convention> get_cds_convention_history(const std::string& id);

private:
    context ctx_;
    repository::cds_convention_repository repo_;
};

}

#endif
