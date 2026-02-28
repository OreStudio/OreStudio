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
#ifndef ORES_REFDATA_SERVICE_PORTFOLIO_SERVICE_HPP
#define ORES_REFDATA_SERVICE_PORTFOLIO_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.refdata/domain/portfolio.hpp"
#include "ores.refdata/repository/portfolio_repository.hpp"

namespace ores::refdata::service {

/**
 * @brief Service for managing portfolios.
 *
 * This service provides functionality for:
 * - Managing portfolios (CRUD operations)
 */
class portfolio_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.portfolio_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a portfolio_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit portfolio_service(context ctx);

    /**
     * @brief Lists all portfolios.
     */
    std::vector<domain::portfolio> list_portfolios();

    /**
     * @brief Finds a portfolio by its ID.
     */
    std::optional<domain::portfolio>
    find_portfolio(const boost::uuids::uuid& id);

    /**
     * @brief Finds a portfolio by its code.
     */
    std::optional<domain::portfolio>
    find_portfolio_by_code(const std::string& code);

    /**
     * @brief Saves a portfolio (creates or updates).
     *
     * @param portfolio The portfolio to save
     */
    void save_portfolio(const domain::portfolio& portfolio);

    /**
     * @brief Saves multiple portfolios (creates or updates).
     *
     * @param portfolios The portfolios to save
     */
    void save_portfolios(const std::vector<domain::portfolio>& portfolios);

    /**
     * @brief Removes a portfolio.
     *
     * @param id The ID of the portfolio to remove
     */
    void remove_portfolio(const boost::uuids::uuid& id);

    /**
     * @brief Gets the version history for a portfolio.
     *
     * @param id The portfolio ID
     * @return Vector of all versions, newest first
     */
    std::vector<domain::portfolio>
    get_portfolio_history(const boost::uuids::uuid& id);

private:
    repository::portfolio_repository repo_;
};

}

#endif
