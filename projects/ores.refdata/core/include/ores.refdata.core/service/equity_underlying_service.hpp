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
#ifndef ORES_REFDATA_CORE_SERVICE_EQUITY_UNDERLYING_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_EQUITY_UNDERLYING_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/equity_underlying.hpp"
#include "ores.refdata.api/messaging/equity_underlying_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/equity_underlying_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing equity underlyings.
 *
 * Provides a higher-level interface for equity underlying operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT equity_underlying_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.equity_underlying_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a equity_underlying_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit equity_underlying_service(context ctx);

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
    messaging::list_equity_underlyings_response
    list_equity_underlyings(const messaging::list_equity_underlyings_request& request);
    messaging::get_equity_underlying_response
    get_equity_underlying(const messaging::get_equity_underlying_request& request);
    messaging::get_many_equity_underlyings_response
    get_many_equity_underlyings(const messaging::get_many_equity_underlyings_request& request);
    messaging::put_equity_underlying_response
    put_equity_underlying(const messaging::put_equity_underlying_request& request);
    messaging::put_many_equity_underlyings_response
    put_many_equity_underlyings(const messaging::put_many_equity_underlyings_request& request);
    messaging::delete_equity_underlying_response
    delete_equity_underlying(const messaging::delete_equity_underlying_request& request);
    messaging::delete_many_equity_underlyings_response delete_many_equity_underlyings(
        const messaging::delete_many_equity_underlyings_request& request);
    messaging::list_equity_underlying_versions_response list_equity_underlying_versions(
        const messaging::list_equity_underlying_versions_request& request);
    messaging::get_equity_underlying_version_response
    get_equity_underlying_version(const messaging::get_equity_underlying_version_request& request);
    /**@}*/

    /**
     * @brief Lists equity underlyings with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of equity underlyings for the requested page.
     */
    std::vector<domain::equity_underlying> list_equity_underlyings(std::uint32_t offset,
                                                                   std::uint32_t limit);

    /**
     * @brief Gets the total count of active equity underlyings.
     *
     * @return Total number of active equity underlyings.
     */
    std::uint32_t count_equity_underlyings();


    /**
     * @brief Retrieves a single equity underlying as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The equity underlying at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::equity_underlying>
    get_equity_underlying_at_version(const std::string& code, std::uint32_t version);

    /**
     * @brief Retrieves a single equity underlying by its primary key.
     *
     * @return The equity underlying if found, std::nullopt otherwise.
     */
    std::optional<domain::equity_underlying> get_equity_underlying(const std::string& code);

    /**
     * @brief Retrieves a batch of equity underlyings by primary key.
     */
    std::vector<domain::equity_underlying>
    get_equity_underlyings(const std::vector<std::string>& codes);

    /**
     * @brief Saves a equity underlying (creates or updates).
     *
     * @param equity_underlying The equity underlying to save.
     * @throws std::exception on failure.
     */
    void save_equity_underlying(const domain::equity_underlying& equity_underlying);

    /**
     * @brief Saves a batch of equity underlyings.
     *
     * @param equity_underlyings The equity underlyings to save.
     * @throws std::exception on failure.
     */
    void save_equity_underlyings(const std::vector<domain::equity_underlying>& equity_underlyings);

    /**
     * @brief Deletes a equity underlying by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_equity_underlying(const std::string& code);

    /**
     * @brief Deletes equity underlyings by their primary keys.
     */
    void delete_equity_underlyings(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a equity underlying.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::equity_underlying> get_equity_underlying_history(const std::string& code);

private:
    context ctx_;
    repository::equity_underlying_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::equity_underlying_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::equity_underlying& out);
};

}

#endif
