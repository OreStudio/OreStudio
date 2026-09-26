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
#ifndef ORES_TRADING_CORE_SERVICE_ASCOT_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_ASCOT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/ascot.hpp"
#include "ores.trading.api/messaging/ascot_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/ascot_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing ascots.
 *
 * Provides a higher-level interface for ascot operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT ascot_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.ascot_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a ascot_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit ascot_service(context ctx);

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
    messaging::list_ascots_response list_ascots(const messaging::list_ascots_request& request);
    messaging::get_ascot_response get_ascot(const messaging::get_ascot_request& request);
    messaging::get_many_ascots_response
    get_many_ascots(const messaging::get_many_ascots_request& request);
    messaging::put_ascot_response put_ascot(const messaging::put_ascot_request& request);
    messaging::put_many_ascots_response
    put_many_ascots(const messaging::put_many_ascots_request& request);
    messaging::delete_ascot_response delete_ascot(const messaging::delete_ascot_request& request);
    messaging::delete_many_ascots_response
    delete_many_ascots(const messaging::delete_many_ascots_request& request);
    messaging::list_ascot_versions_response
    list_ascot_versions(const messaging::list_ascot_versions_request& request);
    messaging::get_ascot_version_response
    get_ascot_version(const messaging::get_ascot_version_request& request);
    /**@}*/

    /**
     * @brief Lists ascots with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of ascots for the requested page.
     */
    std::vector<domain::ascot> list_ascots(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active ascots.
     *
     * @return Total number of active ascots.
     */
    std::uint32_t count_ascots();


    /**
     * @brief Retrieves a single ascot as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The ascot at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::ascot> get_ascot_at_version(const boost::uuids::uuid& instrument_id,
                                                      std::uint32_t version);

    /**
     * @brief Retrieves a single ascot by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The ascot if found, std::nullopt otherwise.
     */
    std::optional<domain::ascot> get_ascot(const boost::uuids::uuid& instrument_id);

    /**
     * @brief Retrieves a batch of ascots by primary key.
     */
    std::vector<domain::ascot> get_ascots(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Saves a ascot (creates or updates).
     *
     * @param ascot The ascot to save.
     * @throws std::exception on failure.
     */
    void save_ascot(const domain::ascot& ascot);

    /**
     * @brief Saves a batch of ascots.
     *
     * @param ascots The ascots to save.
     * @throws std::exception on failure.
     */
    void save_ascots(const std::vector<domain::ascot>& ascots);

    /**
     * @brief Deletes a ascot by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_ascot(const boost::uuids::uuid& instrument_id);

    /**
     * @brief Deletes ascots by their primary keys.
     */
    void delete_ascots(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Retrieves all historical versions of a ascot.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::ascot> get_ascot_history(const std::string& instrument_id);

private:
    context ctx_;
    repository::ascot_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::ascot_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::ascot& out);
};

}

#endif
