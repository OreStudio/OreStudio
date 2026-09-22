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
#ifndef ORES_REFDATA_CORE_SERVICE_INSTRUMENT_CODE_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_INSTRUMENT_CODE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/instrument_code.hpp"
#include "ores.refdata.api/messaging/instrument_code_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/instrument_code_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing instrument codes.
 *
 * Provides a higher-level interface for instrument code operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT instrument_code_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.instrument_code_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a instrument_code_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit instrument_code_service(context ctx);

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
    messaging::list_instrument_codes_response
    list_instrument_codes(const messaging::list_instrument_codes_request& request);
    messaging::get_instrument_code_response
    get_instrument_code(const messaging::get_instrument_code_request& request);
    messaging::get_many_instrument_codes_response
    get_many_instrument_codes(const messaging::get_many_instrument_codes_request& request);
    messaging::put_instrument_code_response
    put_instrument_code(const messaging::put_instrument_code_request& request);
    messaging::put_many_instrument_codes_response
    put_many_instrument_codes(const messaging::put_many_instrument_codes_request& request);
    messaging::delete_instrument_code_response
    delete_instrument_code(const messaging::delete_instrument_code_request& request);
    messaging::delete_many_instrument_codes_response
    delete_many_instrument_codes(const messaging::delete_many_instrument_codes_request& request);
    messaging::list_instrument_code_versions_response
    list_instrument_code_versions(const messaging::list_instrument_code_versions_request& request);
    messaging::get_instrument_code_version_response
    get_instrument_code_version(const messaging::get_instrument_code_version_request& request);
    /**@}*/

    /**
     * @brief Lists instrument codes with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of instrument codes for the requested page.
     */
    std::vector<domain::instrument_code> list_instruments(std::uint32_t offset,
                                                          std::uint32_t limit);

    /**
     * @brief Gets the total count of active instrument codes.
     *
     * @return Total number of active instrument codes.
     */
    std::uint32_t count_instruments();


    /**
     * @brief Retrieves a single instrument code as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The instrument code at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::instrument_code> get_instrument_at_version(const std::string& code,
                                                                     std::uint32_t version);

    /**
     * @brief Retrieves a single instrument code by its primary key.
     *
     * @return The instrument code if found, std::nullopt otherwise.
     */
    std::optional<domain::instrument_code> get_instrument(const std::string& code);

    /**
     * @brief Retrieves a batch of instrument codes by primary key.
     */
    std::vector<domain::instrument_code> get_instruments(const std::vector<std::string>& codes);

    /**
     * @brief Saves a instrument code (creates or updates).
     *
     * @param instrument The instrument code to save.
     * @throws std::exception on failure.
     */
    void save_instrument(const domain::instrument_code& instrument);

    /**
     * @brief Saves a batch of instrument codes.
     *
     * @param instruments The instrument codes to save.
     * @throws std::exception on failure.
     */
    void save_instruments(const std::vector<domain::instrument_code>& instruments);

    /**
     * @brief Deletes a instrument code by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_instrument(const std::string& code);

    /**
     * @brief Deletes instrument codes by their primary keys.
     */
    void delete_instruments(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a instrument code.
     */
    std::vector<domain::instrument_code> get_instrument_history(const std::string& code);

private:
    context ctx_;
    repository::instrument_code_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::instrument_code_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::instrument_code& out);
};

}

#endif
