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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_service.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_DQ_CORE_SERVICE_CSA_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_CSA_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/csa.hpp"
#include "ores.dq.api/messaging/csa_protocol.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/csa_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing csas.
 *
 * Provides a higher-level interface for csa operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT csa_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.csa_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a csa_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit csa_service(context ctx);

    /**
     * @brief The protocol operations, one method per subject.
     *
     * A method takes the canonical request and answers its response, so the
     * handler that serves the subject decodes, calls and replies without
     * deciding anything. The result a caller reads -- missing, conflicting,
     * denied -- is filled here, where the storage call that decided it is
     * made, rather than being inferred from an exception.
     */
    /**@{*/
    messaging::list_csas_response list_csas(const messaging::list_csas_request& request);
    messaging::get_csa_response get_csa(const messaging::get_csa_request& request);
    messaging::get_many_csas_response
    get_many_csas(const messaging::get_many_csas_request& request);
    /**@}*/

    /**
     * @brief Lists csas with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of csas for the requested page.
     */
    std::vector<domain::csa> list_csas(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active csas.
     *
     * @return Total number of active csas.
     */
    std::uint32_t count_csas();


    /**
     * @brief Retrieves a single csa by its primary key.
     *
     * @return The csa if found, std::nullopt otherwise.
     */
    std::optional<domain::csa> get_csa(const std::string& netting_set_code);

    /**
     * @brief Retrieves a batch of csas by primary key.
     */
    std::vector<domain::csa> get_csas(const std::vector<std::string>& netting_set_codes);

    /**
     * @brief Saves a csa (creates or updates).
     *
     * @param csa The csa to save.
     * @throws std::exception on failure.
     */
    void save_csa(const domain::csa& csa);

    /**
     * @brief Saves a batch of csas.
     *
     * @param csas The csas to save.
     * @throws std::exception on failure.
     */
    void save_csas(const std::vector<domain::csa>& csas);

    /**
     * @brief Deletes a csa by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_csa(const std::string& netting_set_code);

    /**
     * @brief Deletes csas by their primary keys.
     */
    void delete_csas(const std::vector<std::string>& netting_set_codes);


private:
    context ctx_;
    repository::csa_repository repo_;
};

}

#endif
