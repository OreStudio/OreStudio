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
#ifndef ORES_REFDATA_CORE_SERVICE_PRODUCT_FAMILY_CODE_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_PRODUCT_FAMILY_CODE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/product_family_code.hpp"
#include "ores.refdata.api/messaging/product_family_code_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/product_family_code_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing product families.
 *
 * Provides a higher-level interface for product family operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT product_family_code_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.product_family_code_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a product_family_code_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit product_family_code_service(context ctx);

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
    ::ores::refdata::messaging::list_product_family_codes_response list_product_family_codes(
        const ::ores::refdata::messaging::list_product_family_codes_request& request);
    ::ores::refdata::messaging::get_product_family_code_response get_product_family_code(
        const ::ores::refdata::messaging::get_product_family_code_request& request);
    ::ores::refdata::messaging::get_many_product_family_codes_response
    get_many_product_family_codes(
        const ::ores::refdata::messaging::get_many_product_family_codes_request& request);
    ::ores::refdata::messaging::put_product_family_code_response put_product_family_code(
        const ::ores::refdata::messaging::put_product_family_code_request& request);
    ::ores::refdata::messaging::put_many_product_family_codes_response
    put_many_product_family_codes(
        const ::ores::refdata::messaging::put_many_product_family_codes_request& request);
    ::ores::refdata::messaging::delete_product_family_code_response delete_product_family_code(
        const ::ores::refdata::messaging::delete_product_family_code_request& request);
    ::ores::refdata::messaging::delete_many_product_family_codes_response
    delete_many_product_family_codes(
        const ::ores::refdata::messaging::delete_many_product_family_codes_request& request);
    ::ores::refdata::messaging::list_product_family_code_versions_response
    list_product_family_code_versions(
        const ::ores::refdata::messaging::list_product_family_code_versions_request& request);
    ::ores::refdata::messaging::get_product_family_code_version_response
    get_product_family_code_version(
        const ::ores::refdata::messaging::get_product_family_code_version_request& request);
    /**@}*/

    /**
     * @brief Lists product families with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of product families for the requested page.
     */
    std::vector<domain::product_family_code> list_families(std::uint32_t offset,
                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active product families.
     *
     * @return Total number of active product families.
     */
    std::uint32_t count_families();


    /**
     * @brief Retrieves a single product family as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The product family at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::product_family_code> get_family_at_version(const std::string& code,
                                                                     std::uint32_t version);

    /**
     * @brief Retrieves a single product family by its primary key.
     *
     * @return The product family if found, std::nullopt otherwise.
     */
    std::optional<domain::product_family_code> get_family(const std::string& code);

    /**
     * @brief Retrieves a batch of product families by primary key.
     */
    std::vector<domain::product_family_code> get_families(const std::vector<std::string>& codes);

    /**
     * @brief Saves a product family (creates or updates).
     *
     * @param family The product family to save.
     * @throws std::exception on failure.
     */
    void save_family(const domain::product_family_code& family);

    /**
     * @brief Saves a batch of product families.
     *
     * @param families The product families to save.
     * @throws std::exception on failure.
     */
    void save_families(const std::vector<domain::product_family_code>& families);

    /**
     * @brief Deletes a product family by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_family(const std::string& code);

    /**
     * @brief Deletes product families by their primary keys.
     */
    void delete_families(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a product family.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::product_family_code> get_family_history(const std::string& code);

private:
    context ctx_;
    repository::product_family_code_repository repo_;

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
    prepare_change(const ::ores::refdata::messaging::product_family_code_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::product_family_code& out);
};

}

#endif
