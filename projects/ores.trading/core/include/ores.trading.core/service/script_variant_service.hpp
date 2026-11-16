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
#ifndef ORES_TRADING_CORE_SERVICE_SCRIPT_VARIANT_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_SCRIPT_VARIANT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/script_variant.hpp"
#include "ores.trading.api/messaging/script_variant_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/script_variant_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing script variants.
 *
 * Provides a higher-level interface for script variant operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT script_variant_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.script_variant_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a script_variant_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit script_variant_service(const context& ctx);

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
    ::ores::trading::messaging::list_script_variants_response
    list_script_variants(const ::ores::trading::messaging::list_script_variants_request& request);
    ::ores::trading::messaging::get_script_variant_response
    get_script_variant(const ::ores::trading::messaging::get_script_variant_request& request);
    ::ores::trading::messaging::get_many_script_variants_response get_many_script_variants(
        const ::ores::trading::messaging::get_many_script_variants_request& request);
    ::ores::trading::messaging::put_script_variant_response
    put_script_variant(const ::ores::trading::messaging::put_script_variant_request& request);
    ::ores::trading::messaging::put_many_script_variants_response put_many_script_variants(
        const ::ores::trading::messaging::put_many_script_variants_request& request);
    ::ores::trading::messaging::delete_script_variant_response
    delete_script_variant(const ::ores::trading::messaging::delete_script_variant_request& request);
    ::ores::trading::messaging::delete_many_script_variants_response delete_many_script_variants(
        const ::ores::trading::messaging::delete_many_script_variants_request& request);
    ::ores::trading::messaging::list_script_variant_versions_response list_script_variant_versions(
        const ::ores::trading::messaging::list_script_variant_versions_request& request);
    ::ores::trading::messaging::get_script_variant_version_response get_script_variant_version(
        const ::ores::trading::messaging::get_script_variant_version_request& request);
    /**@}*/

    /**
     * @brief Lists script variants with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of script variants for the requested page.
     */
    std::vector<domain::script_variant> list_variants(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active script variants.
     *
     * @return Total number of active script variants.
     */
    std::uint32_t count_variants();


    /**
     * @brief Retrieves a single script variant as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The script variant at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::script_variant> get_variant_at_version(const std::string& script_name,
                                                                 const std::string& purpose,
                                                                 std::uint32_t version);

    /**
     * @brief Retrieves a single script variant by its primary key.
     *
     * @return The script variant if found, std::nullopt otherwise.
     */
    std::optional<domain::script_variant> get_variant(const std::string& script_name,
                                                      const std::string& purpose);

    /**
     * @brief Retrieves a batch of script variants by primary key.
     */
    std::vector<domain::script_variant> get_variants(const std::vector<std::string>& script_names,
                                                     const std::vector<std::string>& purposes);

    /**
     * @brief Saves a script variant (creates or updates).
     *
     * @param variant The script variant to save.
     * @throws std::exception on failure.
     */
    void save_variant(const domain::script_variant& variant);

    /**
     * @brief Saves a batch of script variants.
     *
     * @param variants The script variants to save.
     * @throws std::exception on failure.
     */
    void save_variants(const std::vector<domain::script_variant>& variants);

    /**
     * @brief Deletes a script variant by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_variant(const std::string& script_name, const std::string& purpose);

    /**
     * @brief Deletes script variants by their primary keys.
     */
    void delete_variants(const std::vector<std::string>& script_names,
                         const std::vector<std::string>& purposes);

    /**
     * @brief Retrieves all historical versions of a script variant.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::script_variant> get_variant_history(const std::string& script_name,
                                                            const std::string& purpose);

private:
    context ctx_;
    repository::script_variant_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * The caller reads the current row, so a batch reads every row it names
     * in one query rather than one query per change.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param current The row the change names as it is now, or empty.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result
    prepare_change(const ::ores::trading::messaging::script_variant_change& change,
                   const ores::utility::domain::change_intent& intent,
                   const std::vector<domain::script_variant>& current,
                   domain::script_variant& out);
};

}

#endif
