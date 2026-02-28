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
#ifndef ORES_REFDATA_SERVICE_BUSINESS_CENTRE_SERVICE_HPP
#define ORES_REFDATA_SERVICE_BUSINESS_CENTRE_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include "ores.database/domain/context.hpp"
#include "ores.refdata/domain/business_centre.hpp"
#include "ores.refdata/repository/business_centre_repository.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::refdata::service {

/**
 * @brief Service for managing business centres.
 *
 * Provides a higher-level interface for business centre operations, wrapping
 * the underlying repository.
 */
class business_centre_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.business_centre_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a business_centre_service with a database context.
     */
    explicit business_centre_service(context ctx);

    /**
     * @brief Lists business centres with pagination support.
     */
    std::vector<domain::business_centre> list_business_centres(
        std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active business centres.
     */
    std::uint32_t count_business_centres();

    /**
     * @brief Saves a business centre (creates or updates).
     */
    void save_business_centre(const domain::business_centre& bc);

    /**
     * @brief Saves a batch of business centres.
     */
    void save_business_centres(
        const std::vector<domain::business_centre>& business_centres);

    /**
     * @brief Deletes a business centre by its code.
     */
    void delete_business_centre(const std::string& code);

    /**
     * @brief Retrieves a single business centre by its code.
     */
    std::optional<domain::business_centre>
    get_business_centre(const std::string& code);

    /**
     * @brief Retrieves all historical versions of a business centre.
     */
    std::vector<domain::business_centre>
    get_business_centre_history(const std::string& code);

private:
    context ctx_;
    repository::business_centre_repository repo_;
};

}

#endif
