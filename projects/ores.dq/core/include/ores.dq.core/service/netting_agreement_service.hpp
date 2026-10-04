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
#ifndef ORES_DQ_CORE_SERVICE_NETTING_AGREEMENT_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_NETTING_AGREEMENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/netting_agreement.hpp"
#include "ores.dq.api/messaging/netting_agreement_protocol.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/netting_agreement_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing netting agreements.
 *
 * Provides a higher-level interface for netting agreement operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT netting_agreement_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.netting_agreement_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a netting_agreement_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit netting_agreement_service(context ctx);

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
    messaging::list_netting_agreements_response
    list_netting_agreements(const messaging::list_netting_agreements_request& request);
    messaging::get_netting_agreement_response
    get_netting_agreement(const messaging::get_netting_agreement_request& request);
    messaging::get_many_netting_agreements_response
    get_many_netting_agreements(const messaging::get_many_netting_agreements_request& request);
    /**@}*/

    /**
     * @brief Lists netting agreements with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of netting agreements for the requested page.
     */
    std::vector<domain::netting_agreement> list_netting_agreements(std::uint32_t offset,
                                                                   std::uint32_t limit);

    /**
     * @brief Gets the total count of active netting agreements.
     *
     * @return Total number of active netting agreements.
     */
    std::uint32_t count_netting_agreements();


    /**
     * @brief Retrieves a single netting agreement by its primary key.
     *
     * @return The netting agreement if found, std::nullopt otherwise.
     */
    std::optional<domain::netting_agreement>
    get_netting_agreement(const std::string& agreement_number);

    /**
     * @brief Retrieves a batch of netting agreements by primary key.
     */
    std::vector<domain::netting_agreement>
    get_netting_agreements(const std::vector<std::string>& agreement_numbers);

    /**
     * @brief Saves a netting agreement (creates or updates).
     *
     * @param netting_agreement The netting agreement to save.
     * @throws std::exception on failure.
     */
    void save_netting_agreement(const domain::netting_agreement& netting_agreement);

    /**
     * @brief Saves a batch of netting agreements.
     *
     * @param netting_agreements The netting agreements to save.
     * @throws std::exception on failure.
     */
    void save_netting_agreements(const std::vector<domain::netting_agreement>& netting_agreements);

    /**
     * @brief Deletes a netting agreement by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_netting_agreement(const std::string& agreement_number);

    /**
     * @brief Deletes netting agreements by their primary keys.
     */
    void delete_netting_agreements(const std::vector<std::string>& agreement_numbers);


private:
    context ctx_;
    repository::netting_agreement_repository repo_;
};

}

#endif
