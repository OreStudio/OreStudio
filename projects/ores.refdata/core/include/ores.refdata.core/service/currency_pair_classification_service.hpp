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
#ifndef ORES_REFDATA_CORE_SERVICE_CURRENCY_PAIR_CLASSIFICATION_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CURRENCY_PAIR_CLASSIFICATION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/currency_pair_classification.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/currency_pair_classification_repository.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing currency pair classifications.
 *
 * Provides a higher-level interface for currency pair classification operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT currency_pair_classification_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.currency_pair_classification_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a currency_pair_classification_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit currency_pair_classification_service(context ctx);

    /**
     * @brief Lists currency pair classifications with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of currency pair classifications for the requested page.
     */
    std::vector<domain::currency_pair_classification> list_classifications(std::uint32_t offset,
                                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active currency pair classifications.
     *
     * @return Total number of active currency pair classifications.
     */
    std::uint32_t count_classifications();


    /**
     * @brief Retrieves a single currency pair classification by its code.
     *
     * @param code The code of the currency pair classification.
     * @return The currency pair classification if found, std::nullopt otherwise.
     */
    std::optional<domain::currency_pair_classification> get_classification(const std::string& code);

    /**
     * @brief Saves a currency pair classification (creates or updates).
     *
     * @param classification The currency pair classification to save.
     * @throws std::exception on failure.
     */
    void save_classification(const domain::currency_pair_classification& classification);

    /**
     * @brief Saves a batch of currency pair classifications.
     *
     * @param classifications The currency pair classifications to save.
     * @throws std::exception on failure.
     */
    void
    save_classifications(const std::vector<domain::currency_pair_classification>& classifications);

    /**
     * @brief Deletes a currency pair classification by its code.
     *
     * @param code The code of the currency pair classification to delete.
     * @throws std::exception on failure.
     */
    void delete_classification(const std::string& code);

    /**
     * @brief Deletes currency pair classifications by their codes.
     */
    void delete_classifications(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a currency pair classification.
     */
    std::vector<domain::currency_pair_classification>
    get_classification_history(const std::string& code);

private:
    context ctx_;
    repository::currency_pair_classification_repository repo_;
};

}

#endif
