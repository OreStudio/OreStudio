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
#ifndef ORES_TRADING_CORE_SERVICE_EQUITY_BASKET_CONSTITUENT_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_EQUITY_BASKET_CONSTITUENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/equity_basket_constituent.hpp"
#include "ores.trading.api/messaging/equity_basket_constituent_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/equity_basket_constituent_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing equity basket constituents.
 *
 * Provides a higher-level interface for equity basket constituent operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT equity_basket_constituent_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.equity_basket_constituent_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a equity_basket_constituent_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit equity_basket_constituent_service(context ctx);

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
    ::ores::trading::messaging::list_equity_basket_constituents_response
    list_equity_basket_constituents(
        const ::ores::trading::messaging::list_equity_basket_constituents_request& request);
    ::ores::trading::messaging::get_equity_basket_constituent_response
    get_equity_basket_constituent(
        const ::ores::trading::messaging::get_equity_basket_constituent_request& request);
    ::ores::trading::messaging::get_many_equity_basket_constituents_response
    get_many_equity_basket_constituents(
        const ::ores::trading::messaging::get_many_equity_basket_constituents_request& request);
    ::ores::trading::messaging::put_equity_basket_constituent_response
    put_equity_basket_constituent(
        const ::ores::trading::messaging::put_equity_basket_constituent_request& request);
    ::ores::trading::messaging::put_many_equity_basket_constituents_response
    put_many_equity_basket_constituents(
        const ::ores::trading::messaging::put_many_equity_basket_constituents_request& request);
    ::ores::trading::messaging::delete_equity_basket_constituent_response
    delete_equity_basket_constituent(
        const ::ores::trading::messaging::delete_equity_basket_constituent_request& request);
    ::ores::trading::messaging::delete_many_equity_basket_constituents_response
    delete_many_equity_basket_constituents(
        const ::ores::trading::messaging::delete_many_equity_basket_constituents_request& request);
    ::ores::trading::messaging::list_equity_basket_constituent_versions_response
    list_equity_basket_constituent_versions(
        const ::ores::trading::messaging::list_equity_basket_constituent_versions_request& request);
    ::ores::trading::messaging::get_equity_basket_constituent_version_response
    get_equity_basket_constituent_version(
        const ::ores::trading::messaging::get_equity_basket_constituent_version_request& request);
    /**@}*/

    /**
     * @brief Lists equity basket constituents with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of equity basket constituents for the requested page.
     */
    std::vector<domain::equity_basket_constituent>
    list_equity_basket_constituents(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active equity basket constituents.
     *
     * @return Total number of active equity basket constituents.
     */
    std::uint32_t count_equity_basket_constituents();


    /**
     * @brief Retrieves a single equity basket constituent as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The equity basket constituent at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::equity_basket_constituent> get_equity_basket_constituent_at_version(
        const std::string& trade_id, const std::string& sequence_number, std::uint32_t version);

    /**
     * @brief Retrieves a single equity basket constituent by its primary key.
     *
     * @return The equity basket constituent if found, std::nullopt otherwise.
     */
    std::optional<domain::equity_basket_constituent>
    get_equity_basket_constituent(const std::string& trade_id, const std::string& sequence_number);

    /**
     * @brief Retrieves a batch of equity basket constituents by primary key.
     */
    std::vector<domain::equity_basket_constituent>
    get_equity_basket_constituents(const std::vector<std::string>& trade_ids,
                                   const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Saves a equity basket constituent (creates or updates).
     *
     * @param equity_basket_constituent The equity basket constituent to save.
     * @throws std::exception on failure.
     */
    void save_equity_basket_constituent(
        const domain::equity_basket_constituent& equity_basket_constituent);

    /**
     * @brief Saves a batch of equity basket constituents.
     *
     * @param equity_basket_constituents The equity basket constituents to save.
     * @throws std::exception on failure.
     */
    void save_equity_basket_constituents(
        const std::vector<domain::equity_basket_constituent>& equity_basket_constituents);

    /**
     * @brief Deletes a equity basket constituent by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_equity_basket_constituent(const std::string& trade_id,
                                          const std::string& sequence_number);

    /**
     * @brief Deletes equity basket constituents by their primary keys.
     */
    void delete_equity_basket_constituents(const std::vector<std::string>& trade_ids,
                                           const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a equity basket constituent.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::equity_basket_constituent>
    get_equity_basket_constituent_history(const std::string& trade_id,
                                          const std::string& sequence_number);

private:
    context ctx_;
    repository::equity_basket_constituent_repository repo_;

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
    prepare_change(const ::ores::trading::messaging::equity_basket_constituent_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::equity_basket_constituent& out);
};

}

#endif
