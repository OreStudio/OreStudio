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
#ifndef ORES_REFDATA_CORE_SERVICE_PURPOSE_TYPE_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_PURPOSE_TYPE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/purpose_type.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/purpose_type_repository.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing purpose types.
 *
 * Provides a higher-level interface for purpose type operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT purpose_type_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.purpose_type_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a purpose_type_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit purpose_type_service(context ctx);

    /**
     * @brief Lists purpose types with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of purpose types for the requested page.
     */
    std::vector<domain::purpose_type> list_types(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active purpose types.
     *
     * @return Total number of active purpose types.
     */
    std::uint32_t count_types();


    /**
     * @brief Retrieves a single purpose type by its code.
     *
     * @param code The code of the purpose type.
     * @return The purpose type if found, std::nullopt otherwise.
     */
    std::optional<domain::purpose_type> get_type(const std::string& code);

    /**
     * @brief Saves a purpose type (creates or updates).
     *
     * @param type The purpose type to save.
     * @throws std::exception on failure.
     */
    void save_type(const domain::purpose_type& type);

    /**
     * @brief Saves a batch of purpose types.
     *
     * @param types The purpose types to save.
     * @throws std::exception on failure.
     */
    void save_types(const std::vector<domain::purpose_type>& types);

    /**
     * @brief Deletes a purpose type by its code.
     *
     * @param code The code of the purpose type to delete.
     * @throws std::exception on failure.
     */
    void delete_type(const std::string& code);

    /**
     * @brief Deletes purpose types by their codes.
     */
    void delete_types(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a purpose type.
     */
    std::vector<domain::purpose_type> get_type_history(const std::string& code);

private:
    context ctx_;
    repository::purpose_type_repository repo_;
};

}

#endif
