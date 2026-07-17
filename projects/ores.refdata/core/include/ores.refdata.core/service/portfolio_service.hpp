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
#ifndef ORES_REFDATA_CORE_SERVICE_PORTFOLIO_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_PORTFOLIO_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/portfolio.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/portfolio_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing portfolios.
 *
 * Provides a higher-level interface for portfolio operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT portfolio_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.portfolio_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a portfolio_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit portfolio_service(context ctx);

    /**
     * @brief Lists portfolios with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of portfolios for the requested page.
     */
    std::vector<domain::portfolio> list_portfolios(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active portfolios.
     *
     * @return Total number of active portfolios.
     */
    std::uint32_t count_portfolios();


    /**
     * @brief Retrieves a single portfolio as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param id The id of the portfolio.
     * @param version The version to fetch.
     * @return The portfolio at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::portfolio> get_portfolio_at_version(const std::string& id,
                                                              std::uint32_t version);

    /**
     * @brief Retrieves a single portfolio by its id.
     *
     * @param id The id of the portfolio.
     * @return The portfolio if found, std::nullopt otherwise.
     */
    std::optional<domain::portfolio> get_portfolio(const std::string& id);

    /**
     * @brief Saves a portfolio (creates or updates).
     *
     * @param portfolio The portfolio to save.
     * @throws std::exception on failure.
     */
    void save_portfolio(const domain::portfolio& portfolio);

    /**
     * @brief Saves a batch of portfolios.
     *
     * @param portfolios The portfolios to save.
     * @throws std::exception on failure.
     */
    void save_portfolios(const std::vector<domain::portfolio>& portfolios);

    /**
     * @brief Deletes a portfolio by its id.
     *
     * @param id The id of the portfolio to delete.
     * @throws std::exception on failure.
     */
    void delete_portfolio(const std::string& id);

    /**
     * @brief Deletes portfolios by their ids.
     */
    void delete_portfolios(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a portfolio.
     */
    std::vector<domain::portfolio> get_portfolio_history(const std::string& id);

private:
    context ctx_;
    repository::portfolio_repository repo_;
};

}

#endif
