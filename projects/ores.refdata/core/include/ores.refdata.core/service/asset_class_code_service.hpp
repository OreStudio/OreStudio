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
#ifndef ORES_REFDATA_CORE_SERVICE_ASSET_CLASS_CODE_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_ASSET_CLASS_CODE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/asset_class_code.hpp"
#include "ores.refdata.api/messaging/asset_class_code_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/asset_class_code_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing asset class codes.
 *
 * Provides a higher-level interface for asset class code operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT asset_class_code_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.asset_class_code_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a asset_class_code_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit asset_class_code_service(context ctx);

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
    messaging::list_asset_class_codes_response
    list_asset_class_codes(const messaging::list_asset_class_codes_request& request);
    messaging::get_asset_class_code_response
    get_asset_class_code(const messaging::get_asset_class_code_request& request);
    messaging::get_many_asset_class_codes_response
    get_many_asset_class_codes(const messaging::get_many_asset_class_codes_request& request);
    messaging::put_asset_class_code_response
    put_asset_class_code(const messaging::put_asset_class_code_request& request);
    messaging::put_many_asset_class_codes_response
    put_many_asset_class_codes(const messaging::put_many_asset_class_codes_request& request);
    messaging::delete_asset_class_code_response
    delete_asset_class_code(const messaging::delete_asset_class_code_request& request);
    messaging::delete_many_asset_class_codes_response
    delete_many_asset_class_codes(const messaging::delete_many_asset_class_codes_request& request);
    messaging::list_asset_class_code_versions_response list_asset_class_code_versions(
        const messaging::list_asset_class_code_versions_request& request);
    messaging::get_asset_class_code_version_response
    get_asset_class_code_version(const messaging::get_asset_class_code_version_request& request);
    /**@}*/

    /**
     * @brief Lists asset class codes with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of asset class codes for the requested page.
     */
    std::vector<domain::asset_class_code> list_asset_classes(std::uint32_t offset,
                                                             std::uint32_t limit);

    /**
     * @brief Gets the total count of active asset class codes.
     *
     * @return Total number of active asset class codes.
     */
    std::uint32_t count_asset_classes();


    /**
     * @brief Retrieves a single asset class code as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The asset class code at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::asset_class_code> get_asset_class_at_version(const std::string& code,
                                                                       std::uint32_t version);

    /**
     * @brief Retrieves a single asset class code by its primary key.
     *
     * @return The asset class code if found, std::nullopt otherwise.
     */
    std::optional<domain::asset_class_code> get_asset_class(const std::string& code);

    /**
     * @brief Retrieves a batch of asset class codes by primary key.
     */
    std::vector<domain::asset_class_code> get_asset_classes(const std::vector<std::string>& codes);

    /**
     * @brief Saves a asset class code (creates or updates).
     *
     * @param asset_class The asset class code to save.
     * @throws std::exception on failure.
     */
    void save_asset_class(const domain::asset_class_code& asset_class);

    /**
     * @brief Saves a batch of asset class codes.
     *
     * @param asset_classes The asset class codes to save.
     * @throws std::exception on failure.
     */
    void save_asset_classes(const std::vector<domain::asset_class_code>& asset_classes);

    /**
     * @brief Deletes a asset class code by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_asset_class(const std::string& code);

    /**
     * @brief Deletes asset class codes by their primary keys.
     */
    void delete_asset_classes(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a asset class code.
     */
    std::vector<domain::asset_class_code> get_asset_class_history(const std::string& code);

private:
    context ctx_;
    repository::asset_class_code_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::asset_class_code_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::asset_class_code& out);
};

}

#endif
