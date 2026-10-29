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
#ifndef ORES_MARKETDATA_CORE_SERVICE_SUPPLIER_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_SUPPLIER_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/supplier.hpp"
#include "ores.marketdata.api/messaging/supplier_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/supplier_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Service for managing suppliers.
 *
 * Provides a higher-level interface for supplier operations,
 * wrapping the underlying repository.
 */
class ORES_MARKETDATA_CORE_EXPORT supplier_service {
private:
    inline static std::string_view logger_name = "ores.marketdata.service.supplier_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a supplier_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit supplier_service(context ctx);

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
    ::ores::marketdata::messaging::list_suppliers_response
    list_suppliers(const ::ores::marketdata::messaging::list_suppliers_request& request);
    ::ores::marketdata::messaging::get_supplier_response
    get_supplier(const ::ores::marketdata::messaging::get_supplier_request& request);
    ::ores::marketdata::messaging::get_many_suppliers_response
    get_many_suppliers(const ::ores::marketdata::messaging::get_many_suppliers_request& request);
    ::ores::marketdata::messaging::list_supplier_versions_response list_supplier_versions(
        const ::ores::marketdata::messaging::list_supplier_versions_request& request);
    ::ores::marketdata::messaging::get_supplier_version_response get_supplier_version(
        const ::ores::marketdata::messaging::get_supplier_version_request& request);
    /**@}*/

    /**
     * @brief Lists suppliers with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of suppliers for the requested page.
     */
    std::vector<domain::supplier> list_suppliers(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active suppliers.
     *
     * @return Total number of active suppliers.
     */
    std::uint32_t count_suppliers();


    /**
     * @brief Retrieves a single supplier as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The supplier at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::supplier> get_supplier_at_version(const std::string& code,
                                                            std::uint32_t version);

    /**
     * @brief Retrieves a single supplier by its primary key.
     *
     * @return The supplier if found, std::nullopt otherwise.
     */
    std::optional<domain::supplier> get_supplier(const std::string& code);

    /**
     * @brief Retrieves a batch of suppliers by primary key.
     */
    std::vector<domain::supplier> get_suppliers(const std::vector<std::string>& codes);

    /**
     * @brief Saves a supplier (creates or updates).
     *
     * @param supplier The supplier to save.
     * @throws std::exception on failure.
     */
    void save_supplier(const domain::supplier& supplier);

    /**
     * @brief Saves a batch of suppliers.
     *
     * @param suppliers The suppliers to save.
     * @throws std::exception on failure.
     */
    void save_suppliers(const std::vector<domain::supplier>& suppliers);

    /**
     * @brief Deletes a supplier by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_supplier(const std::string& code);

    /**
     * @brief Deletes suppliers by their primary keys.
     */
    void delete_suppliers(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a supplier.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::supplier> get_supplier_history(const std::string& code);

private:
    context ctx_;
    repository::supplier_repository repo_;
};

}

#endif
