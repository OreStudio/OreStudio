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
#ifndef ORES_REFDATA_CORE_SERVICE_IR_CURVE_BOOTSTRAP_PILLAR_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_IR_CURVE_BOOTSTRAP_PILLAR_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/ir_curve_bootstrap_pillar.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/ir_curve_bootstrap_pillar_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing IR curve bootstrap pillars.
 *
 * Provides a higher-level interface for IR curve bootstrap pillar operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT ir_curve_bootstrap_pillar_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.ir_curve_bootstrap_pillar_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a ir_curve_bootstrap_pillar_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit ir_curve_bootstrap_pillar_service(context ctx);

    /**
     * @brief Lists IR curve bootstrap pillars with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of IR curve bootstrap pillars for the requested page.
     */
    std::vector<domain::ir_curve_bootstrap_pillar> list_pillars(std::uint32_t offset,
                                                                std::uint32_t limit);

    /**
     * @brief Gets the total count of active IR curve bootstrap pillars.
     *
     * @return Total number of active IR curve bootstrap pillars.
     */
    std::uint32_t count_pillars();


    /**
     * @brief Retrieves a single IR curve bootstrap pillar as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The IR curve bootstrap pillar at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::ir_curve_bootstrap_pillar> get_pillar_at_version(const std::string& id,
                                                                           std::uint32_t version);

    /**
     * @brief Retrieves a single IR curve bootstrap pillar by its primary key.
     *
     * @return The IR curve bootstrap pillar if found, std::nullopt otherwise.
     */
    std::optional<domain::ir_curve_bootstrap_pillar> get_pillar(const std::string& id);

    /**
     * @brief Saves a IR curve bootstrap pillar (creates or updates).
     *
     * @param pillar The IR curve bootstrap pillar to save.
     * @throws std::exception on failure.
     */
    void save_pillar(const domain::ir_curve_bootstrap_pillar& pillar);

    /**
     * @brief Saves a batch of IR curve bootstrap pillars.
     *
     * @param pillars The IR curve bootstrap pillars to save.
     * @throws std::exception on failure.
     */
    void save_pillars(const std::vector<domain::ir_curve_bootstrap_pillar>& pillars);

    /**
     * @brief Deletes a IR curve bootstrap pillar by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_pillar(const std::string& id);

    /**
     * @brief Deletes IR curve bootstrap pillars by their primary keys.
     */
    void delete_pillars(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a IR curve bootstrap pillar.
     */
    std::vector<domain::ir_curve_bootstrap_pillar> get_pillar_history(const std::string& id);

private:
    context ctx_;
    repository::ir_curve_bootstrap_pillar_repository repo_;
};

}

#endif
