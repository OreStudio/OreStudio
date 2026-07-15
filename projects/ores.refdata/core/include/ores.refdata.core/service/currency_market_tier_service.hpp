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
#ifndef ORES_REFDATA_CORE_SERVICE_CURRENCY_MARKET_TIER_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CURRENCY_MARKET_TIER_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/currency_market_tier.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/currency_market_tier_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing currency market tiers.
 *
 * Provides a higher-level interface for currency market tier operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT currency_market_tier_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.currency_market_tier_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a currency_market_tier_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit currency_market_tier_service(context ctx);

    /**
     * @brief Lists currency market tiers with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of currency market tiers for the requested page.
     */
    std::vector<domain::currency_market_tier> list_types(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active currency market tiers.
     *
     * @return Total number of active currency market tiers.
     */
    std::uint32_t count_types();

    /**
     * @brief Retrieves a single currency market tier as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param code The code of the currency market tier.
     * @param version The version to fetch.
     * @return The currency market tier at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::currency_market_tier> get_type_at_version(const std::string& code,
                                                                    std::uint32_t version);

    /**
     * @brief Retrieves a single currency market tier by its code.
     *
     * @param code The code of the currency market tier.
     * @return The currency market tier if found, std::nullopt otherwise.
     */
    std::optional<domain::currency_market_tier> get_type(const std::string& code);

    /**
     * @brief Saves a currency market tier (creates or updates).
     *
     * @param type The currency market tier to save.
     * @throws std::exception on failure.
     */
    void save_type(const domain::currency_market_tier& type);

    /**
     * @brief Saves a batch of currency market tiers.
     *
     * @param types The currency market tiers to save.
     * @throws std::exception on failure.
     */
    void save_types(const std::vector<domain::currency_market_tier>& types);

    /**
     * @brief Deletes a currency market tier by its code.
     *
     * @param code The code of the currency market tier to delete.
     * @throws std::exception on failure.
     */
    void delete_type(const std::string& code);

    /**
     * @brief Deletes currency market tiers by their codes.
     */
    void delete_types(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a currency market tier.
     */
    std::vector<domain::currency_market_tier> get_type_history(const std::string& code);

private:
    context ctx_;
    repository::currency_market_tier_repository repo_;
};

}

#endif
