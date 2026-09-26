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
#ifndef ORES_DQ_CORE_SERVICE_DATA_DOMAIN_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_DATA_DOMAIN_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/data_domain.hpp"
#include "ores.dq.api/messaging/data_domain_protocol.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/data_domain_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing data domains.
 *
 * Provides a higher-level interface for data domain operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT data_domain_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.data_domain_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a data_domain_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit data_domain_service(context ctx);

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
    messaging::list_data_domains_response
    list_data_domains(const messaging::list_data_domains_request& request);
    messaging::get_data_domain_response
    get_data_domain(const messaging::get_data_domain_request& request);
    messaging::get_many_data_domains_response
    get_many_data_domains(const messaging::get_many_data_domains_request& request);
    messaging::put_data_domain_response
    put_data_domain(const messaging::put_data_domain_request& request);
    messaging::put_many_data_domains_response
    put_many_data_domains(const messaging::put_many_data_domains_request& request);
    messaging::delete_data_domain_response
    delete_data_domain(const messaging::delete_data_domain_request& request);
    messaging::delete_many_data_domains_response
    delete_many_data_domains(const messaging::delete_many_data_domains_request& request);
    messaging::list_data_domain_versions_response
    list_data_domain_versions(const messaging::list_data_domain_versions_request& request);
    messaging::get_data_domain_version_response
    get_data_domain_version(const messaging::get_data_domain_version_request& request);
    /**@}*/

    /**
     * @brief Lists data domains with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of data domains for the requested page.
     */
    std::vector<domain::data_domain> list_domains(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active data domains.
     *
     * @return Total number of active data domains.
     */
    std::uint32_t count_domains();


    /**
     * @brief Retrieves a single data domain as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The data domain at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::data_domain> get_domain_at_version(const std::string& name,
                                                             std::uint32_t version);

    /**
     * @brief Retrieves a single data domain by its primary key.
     *
     * @return The data domain if found, std::nullopt otherwise.
     */
    std::optional<domain::data_domain> get_domain(const std::string& name);

    /**
     * @brief Retrieves a batch of data domains by primary key.
     */
    std::vector<domain::data_domain> get_domains(const std::vector<std::string>& names);

    /**
     * @brief Saves a data domain (creates or updates).
     *
     * @param domain The data domain to save.
     * @throws std::exception on failure.
     */
    void save_domain(const domain::data_domain& domain);

    /**
     * @brief Saves a batch of data domains.
     *
     * @param domains The data domains to save.
     * @throws std::exception on failure.
     */
    void save_domains(const std::vector<domain::data_domain>& domains);

    /**
     * @brief Deletes a data domain by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_domain(const std::string& name);

    /**
     * @brief Deletes data domains by their primary keys.
     */
    void delete_domains(const std::vector<std::string>& names);

    /**
     * @brief Retrieves all historical versions of a data domain.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::data_domain> get_domain_history(const std::string& name);

private:
    context ctx_;
    repository::data_domain_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result prepare_change(const messaging::data_domain_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::data_domain& out);
};

}

#endif
