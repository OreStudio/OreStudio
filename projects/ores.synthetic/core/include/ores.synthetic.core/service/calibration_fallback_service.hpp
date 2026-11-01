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
#ifndef ORES_SYNTHETIC_CORE_SERVICE_CALIBRATION_FALLBACK_SERVICE_HPP
#define ORES_SYNTHETIC_CORE_SERVICE_CALIBRATION_FALLBACK_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.synthetic.api/domain/calibration_fallback.hpp"
#include "ores.synthetic.api/messaging/calibration_fallback_protocol.hpp"
#include "ores.synthetic.core/export.hpp"
#include "ores.synthetic.core/repository/calibration_fallback_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Service for managing calibration fallbacks.
 *
 * Provides a higher-level interface for calibration fallback operations,
 * wrapping the underlying repository.
 */
class ORES_SYNTHETIC_CORE_EXPORT calibration_fallback_service {
private:
    inline static std::string_view logger_name =
        "ores.synthetic.service.calibration_fallback_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a calibration_fallback_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit calibration_fallback_service(context ctx);

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
    ::ores::synthetic::messaging::list_calibration_fallbacks_response list_calibration_fallbacks(
        const ::ores::synthetic::messaging::list_calibration_fallbacks_request& request);
    ::ores::synthetic::messaging::get_calibration_fallback_response get_calibration_fallback(
        const ::ores::synthetic::messaging::get_calibration_fallback_request& request);
    ::ores::synthetic::messaging::get_many_calibration_fallbacks_response
    get_many_calibration_fallbacks(
        const ::ores::synthetic::messaging::get_many_calibration_fallbacks_request& request);
    ::ores::synthetic::messaging::put_calibration_fallback_response put_calibration_fallback(
        const ::ores::synthetic::messaging::put_calibration_fallback_request& request);
    ::ores::synthetic::messaging::put_many_calibration_fallbacks_response
    put_many_calibration_fallbacks(
        const ::ores::synthetic::messaging::put_many_calibration_fallbacks_request& request);
    ::ores::synthetic::messaging::delete_calibration_fallback_response delete_calibration_fallback(
        const ::ores::synthetic::messaging::delete_calibration_fallback_request& request);
    ::ores::synthetic::messaging::delete_many_calibration_fallbacks_response
    delete_many_calibration_fallbacks(
        const ::ores::synthetic::messaging::delete_many_calibration_fallbacks_request& request);
    ::ores::synthetic::messaging::list_calibration_fallback_versions_response
    list_calibration_fallback_versions(
        const ::ores::synthetic::messaging::list_calibration_fallback_versions_request& request);
    ::ores::synthetic::messaging::get_calibration_fallback_version_response
    get_calibration_fallback_version(
        const ::ores::synthetic::messaging::get_calibration_fallback_version_request& request);
    /**@}*/

    /**
     * @brief Lists calibration fallbacks with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of calibration fallbacks for the requested page.
     */
    std::vector<domain::calibration_fallback> list_fallbacks(std::uint32_t offset,
                                                             std::uint32_t limit);

    /**
     * @brief Gets the total count of active calibration fallbacks.
     *
     * @return Total number of active calibration fallbacks.
     */
    std::uint32_t count_fallbacks();


    /**
     * @brief Retrieves a single calibration fallback as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The calibration fallback at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::calibration_fallback> get_fallback_at_version(const std::string& code,
                                                                        std::uint32_t version);

    /**
     * @brief Retrieves a single calibration fallback by its primary key.
     *
     * @return The calibration fallback if found, std::nullopt otherwise.
     */
    std::optional<domain::calibration_fallback> get_fallback(const std::string& code);

    /**
     * @brief Retrieves a batch of calibration fallbacks by primary key.
     */
    std::vector<domain::calibration_fallback> get_fallbacks(const std::vector<std::string>& codes);

    /**
     * @brief Saves a calibration fallback (creates or updates).
     *
     * @param fallback The calibration fallback to save.
     * @throws std::exception on failure.
     */
    void save_fallback(const domain::calibration_fallback& fallback);

    /**
     * @brief Saves a batch of calibration fallbacks.
     *
     * @param fallbacks The calibration fallbacks to save.
     * @throws std::exception on failure.
     */
    void save_fallbacks(const std::vector<domain::calibration_fallback>& fallbacks);

    /**
     * @brief Deletes a calibration fallback by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_fallback(const std::string& code);

    /**
     * @brief Deletes calibration fallbacks by their primary keys.
     */
    void delete_fallbacks(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a calibration fallback.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::calibration_fallback> get_fallback_history(const std::string& code);

private:
    context ctx_;
    repository::calibration_fallback_repository repo_;

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
    prepare_change(const ::ores::synthetic::messaging::calibration_fallback_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::calibration_fallback& out);
};

}

#endif
