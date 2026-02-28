/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_REFDATA_SERVICE_CURRENCY_SERVICE_HPP
#define ORES_REFDATA_SERVICE_CURRENCY_SERVICE_HPP

#include <string>
#include <vector>
#include <cstdint>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include "ores.database/domain/context.hpp"
#include "ores.refdata/domain/currency.hpp"
#include "ores.refdata/domain/currency_version_history.hpp"
#include "ores.refdata/repository/currency_repository.hpp"
#include "ores.refdata/repository/party_currency_repository.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::refdata::service {

/**
 * @brief Service for managing currencies.
 *
 * Provides a higher-level interface for currency operations, wrapping
 * the underlying repository.
 */
class currency_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.currency_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a currency_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit currency_service(context ctx);

    /**
     * @brief Lists currencies with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of currencies for the requested page.
     */
    std::vector<domain::currency> list_currencies(std::uint32_t offset,
                                                   std::uint32_t limit);

    /**
     * @brief Gets the total count of active currencies.
     *
     * @return Total number of active currencies.
     */
    std::uint32_t count_currencies();

    /**
     * @brief Saves a currency (creates or updates).
     *
     * @param currency The currency to save.
     * @throws std::exception on failure.
     */
    void save_currency(const domain::currency& currency);

    /**
     * @brief Saves a batch of currencies atomically (all or nothing).
     *
     * @param currencies The currencies to save.
     * @throws std::exception on failure.
     */
    void save_currencies(const std::vector<domain::currency>& currencies);

    /**
     * @brief Deletes a currency by its ISO code.
     *
     * @param iso_code The ISO code of the currency to delete.
     * @throws std::exception on failure.
     */
    void delete_currency(const std::string& iso_code);

    /**
     * @brief Retrieves a single currency by its ISO code.
     *
     * @param iso_code The ISO code of the currency.
     * @return The currency if found, std::nullopt otherwise.
     */
    std::optional<domain::currency> get_currency(const std::string& iso_code);

    /**
     * @brief Retrieves all historical versions of a currency.
     *
     * @param iso_code The ISO code of the currency.
     * @return Vector of all historical versions of the currency.
     */
    std::vector<domain::currency> get_currency_history(const std::string& iso_code);

    /**
     * @brief Retrieves currency version history with version metadata.
     *
     * Converts raw currency records to currency_version objects with
     * version numbers, modified_by, and recorded_at metadata. Versions
     * are ordered from newest to oldest.
     *
     * @param iso_code The ISO code of the currency.
     * @return The version history, or std::nullopt if currency not found.
     */
    std::optional<domain::currency_version_history>
    get_currency_version_history(const std::string& iso_code);

    /**
     * @brief Lists currencies visible to a specific party, with pagination.
     *
     * Uses the party_currencies junction to filter the full currency list
     * to only those the given party is permitted to see.
     *
     * @param party_id The UUID of the party.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of visible currencies for the requested page.
     */
    std::vector<domain::currency> list_currencies_for_party(
        const boost::uuids::uuid& party_id,
        std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of currencies visible to a specific party.
     *
     * @param party_id The UUID of the party.
     * @return Number of currencies the party is permitted to see.
     */
    std::uint32_t count_currencies_for_party(const boost::uuids::uuid& party_id);

private:
    context ctx_;
    repository::currency_repository repo_;
    repository::party_currency_repository junction_repo_;
};

}

#endif
