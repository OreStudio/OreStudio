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
#ifndef ORES_DQ_CORE_SERVICE_CODE_DOMAIN_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_CODE_DOMAIN_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/code_domain.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/code_domain_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing code domains.
 *
 * Provides a higher-level interface for code domain operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT code_domain_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.code_domain_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a code_domain_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit code_domain_service(context ctx);

    /**
     * @brief Lists code domains with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of code domains for the requested page.
     */
    std::vector<domain::code_domain> list_domains(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active code domains.
     *
     * @return Total number of active code domains.
     */
    std::uint32_t count_domains();

    /**
     * @brief Retrieves a single code domain as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param code The code of the code domain.
     * @param version The version to fetch.
     * @return The code domain at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::code_domain> get_domain_at_version(const std::string& code,
                                                             std::uint32_t version);

    /**
     * @brief Retrieves a single code domain by its code.
     *
     * @param code The code of the code domain.
     * @return The code domain if found, std::nullopt otherwise.
     */
    std::optional<domain::code_domain> get_domain(const std::string& code);

    /**
     * @brief Saves a code domain (creates or updates).
     *
     * @param domain The code domain to save.
     * @throws std::exception on failure.
     */
    void save_domain(const domain::code_domain& domain);

    /**
     * @brief Saves a batch of code domains.
     *
     * @param domains The code domains to save.
     * @throws std::exception on failure.
     */
    void save_domains(const std::vector<domain::code_domain>& domains);

    /**
     * @brief Deletes a code domain by its code.
     *
     * @param code The code of the code domain to delete.
     * @throws std::exception on failure.
     */
    void delete_domain(const std::string& code);

    /**
     * @brief Deletes code domains by their codes.
     */
    void delete_domains(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a code domain.
     */
    std::vector<domain::code_domain> get_domain_history(const std::string& code);

private:
    context ctx_;
    repository::code_domain_repository repo_;
};

}

#endif
