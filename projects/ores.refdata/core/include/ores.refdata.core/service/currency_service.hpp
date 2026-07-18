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
#ifndef ORES_REFDATA_CORE_SERVICE_CURRENCY_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CURRENCY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/currency.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/currency_calendar_repository.hpp"
#include "ores.refdata.core/repository/currency_repository.hpp"
#include "ores.refdata.core/repository/party_currency_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing currencies.
 *
 * Provides a higher-level interface for currency operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT currency_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.currency_service";

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
    std::vector<domain::currency> list_currencies(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active currencies.
     *
     * @return Total number of active currencies.
     */
    std::uint32_t count_currencies();

    /**
     * @brief Retrieves a single currency as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param iso_code The iso_code of the currency.
     * @param version The version to fetch.
     * @return The currency at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::currency> get_currency_at_version(const std::string& iso_code,
                                                            std::uint32_t version);

    /**
     * @brief Retrieves a single currency by its iso_code.
     *
     * @param iso_code The iso_code of the currency.
     * @return The currency if found, std::nullopt otherwise.
     */
    std::optional<domain::currency> get_currency(const std::string& iso_code);

    /**
     * @brief Saves a currency (creates or updates).
     *
     * @param currency The currency to save.
     * @throws std::exception on failure.
     */
    void save_currency(const domain::currency& currency);

    /**
     * @brief Saves a batch of currencies.
     *
     * @param currencies The currencies to save.
     * @throws std::exception on failure.
     */
    void save_currencies(const std::vector<domain::currency>& currencies);

    /**
     * @brief Deletes a currency by its iso_code.
     *
     * @param iso_code The iso_code of the currency to delete.
     * @throws std::exception on failure.
     */
    void delete_currency(const std::string& iso_code);

    /**
     * @brief Deletes currencies by their iso_codes.
     */
    void delete_currencies(const std::vector<std::string>& iso_codes);

    /**
     * @brief Retrieves all historical versions of a currency.
     */
    std::vector<domain::currency> get_currency_history(const std::string& iso_code);

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
    std::vector<domain::currency> list_currencies_for_party(const boost::uuids::uuid& party_id,
                                                            std::uint32_t offset,
                                                            std::uint32_t limit);

    /**
     * @brief Gets the total count of currencies visible to a specific party.
     *
     * @param party_id The UUID of the party.
     * @return Number of currencies the party is permitted to see.
     */
    std::uint32_t count_currencies_for_party(const boost::uuids::uuid& party_id);

    /**
     * @brief Lists the calendars assigned to a currency for spot/settlement
     * date computation, via the currency_calendars junction.
     *
     * @param currency_iso_code The ISO 4217 code of the currency.
     * @return Vector of calendar assignment rows for the currency.
     */
    std::vector<ores::refdata::domain::currency_calendar>
    list_calendars_for_currency(const std::string& currency_iso_code);

    /**
     * @brief Assigns a calendar to a currency (creates the junction row).
     *
     * @param row The currency_calendar row to write; caller is responsible
     * for stamping tenant_id/modified_by/performed_by/change_reason_code
     * before calling (see ores::service::messaging::stamp()).
     */
    void assign_calendar_to_currency(const ores::refdata::domain::currency_calendar& row);

    /**
     * @brief Revokes a calendar from a currency (removes the junction row).
     *
     * @param currency_iso_code The ISO 4217 code of the currency.
     * @param calendar_code The QuantLib/ORE calendar token to revoke.
     */
    void revoke_calendar_from_currency(const std::string& currency_iso_code,
                                       const std::string& calendar_code);

private:
    context ctx_;
    repository::currency_repository repo_;
    repository::party_currency_repository junction_repo_;

    repository::currency_calendar_repository calendar_repo_;
};

}

#endif
