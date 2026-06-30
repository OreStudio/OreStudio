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
#ifndef ORES_REFDATA_CORE_SERVICE_PARTY_TYPE_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_PARTY_TYPE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/party_type.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/party_type_repository.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing party types.
 *
 * Provides a higher-level interface for party type operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT party_type_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.party_type_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a party_type_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit party_type_service(context ctx);

    /**
     * @brief Lists party types with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of party types for the requested page.
     */
    std::vector<domain::party_type> list_types(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active party types.
     *
     * @return Total number of active party types.
     */
    std::uint32_t count_types();

    /**
     * @brief Retrieves a single party type by its code.
     *
     * @param code The code of the party type.
     * @return The party type if found, std::nullopt otherwise.
     */
    std::optional<domain::party_type> get_type(const std::string& code);

    /**
     * @brief Saves a party type (creates or updates).
     *
     * @param type The party type to save.
     * @throws std::exception on failure.
     */
    void save_type(const domain::party_type& type);

    /**
     * @brief Saves a batch of party types.
     *
     * @param types The party types to save.
     * @throws std::exception on failure.
     */
    void save_types(const std::vector<domain::party_type>& types);

    /**
     * @brief Deletes a party type by its code.
     *
     * @param code The code of the party type to delete.
     * @throws std::exception on failure.
     */
    void delete_type(const std::string& code);

    /**
     * @brief Deletes party types by their codes.
     */
    void delete_types(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a party type.
     */
    std::vector<domain::party_type> get_type_history(const std::string& code);

private:
    context ctx_;
    repository::party_type_repository repo_;
};

}

#endif
