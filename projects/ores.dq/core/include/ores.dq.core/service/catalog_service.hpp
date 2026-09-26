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
#ifndef ORES_DQ_CORE_SERVICE_CATALOG_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_CATALOG_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/catalog.hpp"
#include "ores.dq.api/messaging/catalog_protocol.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/catalog_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing catalogs.
 *
 * Provides a higher-level interface for catalog operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT catalog_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.catalog_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a catalog_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit catalog_service(context ctx);

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
    messaging::list_catalogs_response
    list_catalogs(const messaging::list_catalogs_request& request);
    messaging::get_catalog_response get_catalog(const messaging::get_catalog_request& request);
    messaging::get_many_catalogs_response
    get_many_catalogs(const messaging::get_many_catalogs_request& request);
    messaging::put_catalog_response put_catalog(const messaging::put_catalog_request& request);
    messaging::put_many_catalogs_response
    put_many_catalogs(const messaging::put_many_catalogs_request& request);
    messaging::delete_catalog_response
    delete_catalog(const messaging::delete_catalog_request& request);
    messaging::delete_many_catalogs_response
    delete_many_catalogs(const messaging::delete_many_catalogs_request& request);
    messaging::list_catalog_versions_response
    list_catalog_versions(const messaging::list_catalog_versions_request& request);
    messaging::get_catalog_version_response
    get_catalog_version(const messaging::get_catalog_version_request& request);
    /**@}*/

    /**
     * @brief Lists catalogs with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of catalogs for the requested page.
     */
    std::vector<domain::catalog> list_catalogs(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active catalogs.
     *
     * @return Total number of active catalogs.
     */
    std::uint32_t count_catalogs();


    /**
     * @brief Retrieves a single catalog as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The catalog at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::catalog> get_catalog_at_version(const std::string& name,
                                                          std::uint32_t version);

    /**
     * @brief Retrieves a single catalog by its primary key.
     *
     * @return The catalog if found, std::nullopt otherwise.
     */
    std::optional<domain::catalog> get_catalog(const std::string& name);

    /**
     * @brief Retrieves a batch of catalogs by primary key.
     */
    std::vector<domain::catalog> get_catalogs(const std::vector<std::string>& names);

    /**
     * @brief Saves a catalog (creates or updates).
     *
     * @param catalog The catalog to save.
     * @throws std::exception on failure.
     */
    void save_catalog(const domain::catalog& catalog);

    /**
     * @brief Saves a batch of catalogs.
     *
     * @param catalogs The catalogs to save.
     * @throws std::exception on failure.
     */
    void save_catalogs(const std::vector<domain::catalog>& catalogs);

    /**
     * @brief Deletes a catalog by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_catalog(const std::string& name);

    /**
     * @brief Deletes catalogs by their primary keys.
     */
    void delete_catalogs(const std::vector<std::string>& names);

    /**
     * @brief Retrieves all historical versions of a catalog.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::catalog> get_catalog_history(const std::string& name);

private:
    context ctx_;
    repository::catalog_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::catalog_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::catalog& out);
};

}

#endif
