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
#ifndef ORES_REFDATA_CORE_SERVICE_CURRENCY_PAIR_CONVENTION_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CURRENCY_PAIR_CONVENTION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/currency_pair_convention.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/currency_pair_convention_repository.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing currency pair conventions.
 *
 * Provides a higher-level interface for currency pair convention operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT currency_pair_convention_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.currency_pair_convention_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a currency_pair_convention_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit currency_pair_convention_service(context ctx);

    /**
     * @brief Lists currency pair conventions with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of currency pair conventions for the requested page.
     */
    std::vector<domain::currency_pair_convention> list_conventions(std::uint32_t offset,
                                                                   std::uint32_t limit);

    /**
     * @brief Gets the total count of active currency pair conventions.
     *
     * @return Total number of active currency pair conventions.
     */
    std::uint32_t count_conventions();


    /**
     * @brief Retrieves a single currency pair convention by its pair_code.
     *
     * @param pair_code The pair_code of the currency pair convention.
     * @return The currency pair convention if found, std::nullopt otherwise.
     */
    std::optional<domain::currency_pair_convention> get_convention(const std::string& pair_code);

    /**
     * @brief Saves a currency pair convention (creates or updates).
     *
     * @param convention The currency pair convention to save.
     * @throws std::exception on failure.
     */
    void save_convention(const domain::currency_pair_convention& convention);

    /**
     * @brief Saves a batch of currency pair conventions.
     *
     * @param conventions The currency pair conventions to save.
     * @throws std::exception on failure.
     */
    void save_conventions(const std::vector<domain::currency_pair_convention>& conventions);

    /**
     * @brief Deletes a currency pair convention by its pair_code.
     *
     * @param pair_code The pair_code of the currency pair convention to delete.
     * @throws std::exception on failure.
     */
    void delete_convention(const std::string& pair_code);

    /**
     * @brief Deletes currency pair conventions by their pair_codes.
     */
    void delete_conventions(const std::vector<std::string>& pair_codes);

    /**
     * @brief Retrieves all historical versions of a currency pair convention.
     */
    std::vector<domain::currency_pair_convention>
    get_convention_history(const std::string& pair_code);

private:
    context ctx_;
    repository::currency_pair_convention_repository repo_;
};

}

#endif
