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
#ifndef ORES_TRADING_CORE_SERVICE_CBO_TRANCHE_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_CBO_TRANCHE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.trading.api/domain/cbo_tranche.hpp"
#include "ores.trading.api/messaging/cbo_tranche_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/cbo_tranche_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing cbo tranches.
 *
 * Provides a higher-level interface for cbo tranche operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT cbo_tranche_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.cbo_tranche_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a cbo_tranche_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit cbo_tranche_service(const context& ctx);

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
    ::ores::trading::messaging::list_cbo_tranches_response
    list_cbo_tranches(const ::ores::trading::messaging::list_cbo_tranches_request& request);
    ::ores::trading::messaging::get_cbo_tranche_response
    get_cbo_tranche(const ::ores::trading::messaging::get_cbo_tranche_request& request);
    ::ores::trading::messaging::get_many_cbo_tranches_response
    get_many_cbo_tranches(const ::ores::trading::messaging::get_many_cbo_tranches_request& request);
    ::ores::trading::messaging::put_cbo_tranche_response
    put_cbo_tranche(const ::ores::trading::messaging::put_cbo_tranche_request& request);
    ::ores::trading::messaging::put_many_cbo_tranches_response
    put_many_cbo_tranches(const ::ores::trading::messaging::put_many_cbo_tranches_request& request);
    ::ores::trading::messaging::delete_cbo_tranche_response
    delete_cbo_tranche(const ::ores::trading::messaging::delete_cbo_tranche_request& request);
    ::ores::trading::messaging::delete_many_cbo_tranches_response delete_many_cbo_tranches(
        const ::ores::trading::messaging::delete_many_cbo_tranches_request& request);
    ::ores::trading::messaging::list_by_trade_id_cbo_tranches_response
    list_by_trade_id_cbo_tranches(
        const ::ores::trading::messaging::list_by_trade_id_cbo_tranches_request& request);
    ::ores::trading::messaging::list_cbo_tranche_versions_response list_cbo_tranche_versions(
        const ::ores::trading::messaging::list_cbo_tranche_versions_request& request);
    ::ores::trading::messaging::get_cbo_tranche_version_response get_cbo_tranche_version(
        const ::ores::trading::messaging::get_cbo_tranche_version_request& request);
    /**@}*/

    /**
     * @brief Lists cbo tranches with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of cbo tranches for the requested page.
     */
    std::vector<domain::cbo_tranche> list_cbo_tranches(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active cbo tranches.
     *
     * @return Total number of active cbo tranches.
     */
    std::uint32_t count_cbo_tranches();


    /**
     * @brief Lists cbo tranches filtered by trade_id, with pagination.
     *
     * @param trade_id The trade_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching cbo tranches for the requested page.
     */
    std::vector<domain::cbo_tranche> list_cbo_tranches_by_trade_id(const std::string& trade_id,
                                                                   std::uint32_t offset,
                                                                   std::uint32_t limit);

    /**
     * @brief Gets the total count of active cbo tranches filtered by trade_id.
     *
     * @param trade_id The trade_id to filter by.
     * @return Total number of matching cbo tranches.
     */
    std::uint32_t count_cbo_tranches_by_trade_id(const std::string& trade_id);


    /**
     * @brief Retrieves a single cbo tranche as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The cbo tranche at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::cbo_tranche> get_cbo_tranche_at_version(
        const std::string& trade_id, const std::string& sequence_number, std::uint32_t version);

    /**
     * @brief Retrieves a single cbo tranche by its primary key.
     *
     * @return The cbo tranche if found, std::nullopt otherwise.
     */
    std::optional<domain::cbo_tranche> get_cbo_tranche(const std::string& trade_id,
                                                       const std::string& sequence_number);

    /**
     * @brief Retrieves a batch of cbo tranches by primary key.
     */
    std::vector<domain::cbo_tranche>
    get_cbo_tranches(const std::vector<std::string>& trade_ids,
                     const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Saves a cbo tranche (creates or updates).
     *
     * @param cbo_tranche The cbo tranche to save.
     * @throws std::exception on failure.
     */
    void save_cbo_tranche(const domain::cbo_tranche& cbo_tranche);

    /**
     * @brief Saves a batch of cbo tranches.
     *
     * @param cbo_tranches The cbo tranches to save.
     * @throws std::exception on failure.
     */
    void save_cbo_tranches(const std::vector<domain::cbo_tranche>& cbo_tranches);

    /**
     * @brief Deletes a cbo tranche by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_cbo_tranche(const std::string& trade_id, const std::string& sequence_number);

    /**
     * @brief Deletes cbo tranches by their primary keys.
     */
    void delete_cbo_tranches(const std::vector<std::string>& trade_ids,
                             const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a cbo tranche.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::cbo_tranche> get_cbo_tranche_history(const std::string& trade_id,
                                                             const std::string& sequence_number);

private:
    context ctx_;
    repository::cbo_tranche_repository repo_;

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
    prepare_change(const ::ores::trading::messaging::cbo_tranche_change& change,
                   const ores::utility::domain::change_intent& intent,
                   const std::vector<domain::cbo_tranche>& current,
                   domain::cbo_tranche& out);
};

}

#endif
