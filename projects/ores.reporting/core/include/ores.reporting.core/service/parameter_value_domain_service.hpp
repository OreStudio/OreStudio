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
#ifndef ORES_REPORTING_CORE_SERVICE_PARAMETER_VALUE_DOMAIN_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_PARAMETER_VALUE_DOMAIN_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/parameter_value_domain.hpp"
#include "ores.reporting.api/messaging/parameter_value_domain_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/parameter_value_domain_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing parameter value domains.
 *
 * Provides a higher-level interface for parameter value domain operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT parameter_value_domain_service {
private:
    inline static std::string_view logger_name =
        "ores.reporting.service.parameter_value_domain_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a parameter_value_domain_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit parameter_value_domain_service(context ctx);

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
    messaging::list_parameter_value_domains_response
    list_parameter_value_domains(const messaging::list_parameter_value_domains_request& request);
    messaging::get_parameter_value_domain_response
    get_parameter_value_domain(const messaging::get_parameter_value_domain_request& request);
    messaging::get_many_parameter_value_domains_response get_many_parameter_value_domains(
        const messaging::get_many_parameter_value_domains_request& request);
    messaging::put_parameter_value_domain_response
    put_parameter_value_domain(const messaging::put_parameter_value_domain_request& request);
    messaging::put_many_parameter_value_domains_response put_many_parameter_value_domains(
        const messaging::put_many_parameter_value_domains_request& request);
    messaging::delete_parameter_value_domain_response
    delete_parameter_value_domain(const messaging::delete_parameter_value_domain_request& request);
    messaging::delete_many_parameter_value_domains_response delete_many_parameter_value_domains(
        const messaging::delete_many_parameter_value_domains_request& request);
    messaging::list_parameter_value_domain_versions_response list_parameter_value_domain_versions(
        const messaging::list_parameter_value_domain_versions_request& request);
    messaging::get_parameter_value_domain_version_response get_parameter_value_domain_version(
        const messaging::get_parameter_value_domain_version_request& request);
    /**@}*/

    /**
     * @brief Lists parameter value domains with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of parameter value domains for the requested page.
     */
    std::vector<domain::parameter_value_domain> list_domains(std::uint32_t offset,
                                                             std::uint32_t limit);

    /**
     * @brief Gets the total count of active parameter value domains.
     *
     * @return Total number of active parameter value domains.
     */
    std::uint32_t count_domains();


    /**
     * @brief Retrieves a single parameter value domain as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The parameter value domain at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::parameter_value_domain> get_domain_at_version(const std::string& code,
                                                                        std::uint32_t version);

    /**
     * @brief Retrieves a single parameter value domain by its primary key.
     *
     * @return The parameter value domain if found, std::nullopt otherwise.
     */
    std::optional<domain::parameter_value_domain> get_domain(const std::string& code);

    /**
     * @brief Retrieves a batch of parameter value domains by primary key.
     */
    std::vector<domain::parameter_value_domain> get_domains(const std::vector<std::string>& codes);

    /**
     * @brief Saves a parameter value domain (creates or updates).
     *
     * @param domain The parameter value domain to save.
     * @throws std::exception on failure.
     */
    void save_domain(const domain::parameter_value_domain& domain);

    /**
     * @brief Saves a batch of parameter value domains.
     *
     * @param domains The parameter value domains to save.
     * @throws std::exception on failure.
     */
    void save_domains(const std::vector<domain::parameter_value_domain>& domains);

    /**
     * @brief Deletes a parameter value domain by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_domain(const std::string& code);

    /**
     * @brief Deletes parameter value domains by their primary keys.
     */
    void delete_domains(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a parameter value domain.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::parameter_value_domain> get_domain_history(const std::string& code);

private:
    context ctx_;
    repository::parameter_value_domain_repository repo_;

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
    ores::utility::domain::result
    prepare_change(const messaging::parameter_value_domain_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::parameter_value_domain& out);
};

}

#endif
