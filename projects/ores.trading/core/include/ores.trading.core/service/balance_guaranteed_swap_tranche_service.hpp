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
#ifndef ORES_TRADING_CORE_SERVICE_BALANCE_GUARANTEED_SWAP_TRANCHE_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_BALANCE_GUARANTEED_SWAP_TRANCHE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/balance_guaranteed_swap_tranche.hpp"
#include "ores.trading.api/messaging/balance_guaranteed_swap_tranche_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/balance_guaranteed_swap_tranche_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing balance guaranteed swap tranches.
 *
 * Provides a higher-level interface for balance guaranteed swap tranche operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT balance_guaranteed_swap_tranche_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.balance_guaranteed_swap_tranche_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a balance_guaranteed_swap_tranche_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit balance_guaranteed_swap_tranche_service(context ctx);

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
    messaging::list_balance_guaranteed_swap_tranches_response list_balance_guaranteed_swap_tranches(
        const messaging::list_balance_guaranteed_swap_tranches_request& request);
    messaging::get_balance_guaranteed_swap_tranche_response get_balance_guaranteed_swap_tranche(
        const messaging::get_balance_guaranteed_swap_tranche_request& request);
    messaging::get_many_balance_guaranteed_swap_tranches_response
    get_many_balance_guaranteed_swap_tranches(
        const messaging::get_many_balance_guaranteed_swap_tranches_request& request);
    messaging::put_balance_guaranteed_swap_tranche_response put_balance_guaranteed_swap_tranche(
        const messaging::put_balance_guaranteed_swap_tranche_request& request);
    messaging::put_many_balance_guaranteed_swap_tranches_response
    put_many_balance_guaranteed_swap_tranches(
        const messaging::put_many_balance_guaranteed_swap_tranches_request& request);
    messaging::delete_balance_guaranteed_swap_tranche_response
    delete_balance_guaranteed_swap_tranche(
        const messaging::delete_balance_guaranteed_swap_tranche_request& request);
    messaging::delete_many_balance_guaranteed_swap_tranches_response
    delete_many_balance_guaranteed_swap_tranches(
        const messaging::delete_many_balance_guaranteed_swap_tranches_request& request);
    messaging::list_balance_guaranteed_swap_tranche_versions_response
    list_balance_guaranteed_swap_tranche_versions(
        const messaging::list_balance_guaranteed_swap_tranche_versions_request& request);
    messaging::get_balance_guaranteed_swap_tranche_version_response
    get_balance_guaranteed_swap_tranche_version(
        const messaging::get_balance_guaranteed_swap_tranche_version_request& request);
    /**@}*/

    /**
     * @brief Lists balance guaranteed swap tranches with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of balance guaranteed swap tranches for the requested page.
     */
    std::vector<domain::balance_guaranteed_swap_tranche>
    list_balance_guaranteed_swap_tranches(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active balance guaranteed swap tranches.
     *
     * @return Total number of active balance guaranteed swap tranches.
     */
    std::uint32_t count_balance_guaranteed_swap_tranches();


    /**
     * @brief Retrieves a single balance guaranteed swap tranche as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The balance guaranteed swap tranche at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::balance_guaranteed_swap_tranche>
    get_balance_guaranteed_swap_tranche_at_version(const std::string& trade_id,
                                                   const std::string& sequence_number,
                                                   std::uint32_t version);

    /**
     * @brief Retrieves a single balance guaranteed swap tranche by its primary key.
     *
     * @return The balance guaranteed swap tranche if found, std::nullopt otherwise.
     */
    std::optional<domain::balance_guaranteed_swap_tranche>
    get_balance_guaranteed_swap_tranche(const std::string& trade_id,
                                        const std::string& sequence_number);

    /**
     * @brief Retrieves a batch of balance guaranteed swap tranches by primary key.
     */
    std::vector<domain::balance_guaranteed_swap_tranche>
    get_balance_guaranteed_swap_tranches(const std::vector<std::string>& trade_ids,
                                         const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Saves a balance guaranteed swap tranche (creates or updates).
     *
     * @param balance_guaranteed_swap_tranche The balance guaranteed swap tranche to save.
     * @throws std::exception on failure.
     */
    void save_balance_guaranteed_swap_tranche(
        const domain::balance_guaranteed_swap_tranche& balance_guaranteed_swap_tranche);

    /**
     * @brief Saves a batch of balance guaranteed swap tranches.
     *
     * @param balance_guaranteed_swap_tranches The balance guaranteed swap tranches to save.
     * @throws std::exception on failure.
     */
    void save_balance_guaranteed_swap_tranches(
        const std::vector<domain::balance_guaranteed_swap_tranche>&
            balance_guaranteed_swap_tranches);

    /**
     * @brief Deletes a balance guaranteed swap tranche by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_balance_guaranteed_swap_tranche(const std::string& trade_id,
                                                const std::string& sequence_number);

    /**
     * @brief Deletes balance guaranteed swap tranches by their primary keys.
     */
    void delete_balance_guaranteed_swap_tranches(const std::vector<std::string>& trade_ids,
                                                 const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a balance guaranteed swap tranche.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::balance_guaranteed_swap_tranche>
    get_balance_guaranteed_swap_tranche_history(const std::string& trade_id,
                                                const std::string& sequence_number);

private:
    context ctx_;
    repository::balance_guaranteed_swap_tranche_repository repo_;

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
    prepare_change(const messaging::balance_guaranteed_swap_tranche_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::balance_guaranteed_swap_tranche& out);
};

}

#endif
